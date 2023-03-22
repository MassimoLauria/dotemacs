;;; emacsql-sqlite-module.el --- EmacSQL back-end for SQLite using a module  -*- lexical-binding:t -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Homepage: https://github.com/magit/emacsql

;; Package-Version: 20221127.2146
;; Package-X-Original-Version: 3.1.1.50-git
;; Package-Requires: ((emacs "25") (emacsql "3.1.1") (sqlite3 "0.16"))
;; Package-Commit: 6b2e65bdf785364cf7c34c31fea5812e1e58c657
;; SPDX-License-Identifier: Unlicense

;;; Commentary:

;; This package provides an EmacSQL back-end for SQLite, which uses
;; the Emacs module provided by the `sqlite3' package.

;;; Code:

(require 'sqlite3)
(require 'emacsql)

(emacsql-register-reserved emacsql-sqlite-reserved)

(defclass emacsql-sqlite-module-connection (emacsql-connection)
  ((file :initarg :file
         :type (or null string)
         :documentation "Database file name.")
   (types :allocation :class
          :reader emacsql-types
          :initform '((integer "INTEGER")
                      (float "REAL")
                      (object "TEXT")
                      (nil nil))))
  (:documentation "A connection to a SQLite database using a module."))

(cl-defmethod initialize-instance :after
  ((connection emacsql-sqlite-module-connection) &rest _)
  (setf (emacsql-process connection)
        (sqlite3-open (or (slot-value connection 'file) ":memory:")
                      sqlite-open-readwrite
                      sqlite-open-create))
  (when emacsql-global-timeout
    (emacsql connection [:pragma (= busy-timeout $s1)]
             (/ (* emacsql-global-timeout 1000) 2)))
  (emacsql connection [:pragma (= foreign-keys on)])
  (emacsql-register connection))

(cl-defun emacsql-sqlite-module (file &key debug)
  "Open a connected to database stored in FILE.
If FILE is nil use an in-memory database.

:debug LOG -- When non-nil, log all SQLite commands to a log
buffer. This is for debugging purposes."
  (let ((connection (make-instance #'emacsql-sqlite-module-connection
                                   :file file)))
    (when debug
      (emacsql-enable-debugging connection))
    connection))

(cl-defmethod emacsql-live-p ((connection emacsql-sqlite-module-connection))
  (and (emacsql-process connection) t))

(cl-defmethod emacsql-close ((connection emacsql-sqlite-module-connection))
  (sqlite3-close (emacsql-process connection))
  (setf (emacsql-process connection) nil))

(cl-defmethod emacsql-send-message
  ((connection emacsql-sqlite-module-connection) message)
  (condition-case err
      (let (rows)
        (sqlite3-exec (emacsql-process connection)
                      message
                      (lambda (_ row __)
                        (push (mapcar (lambda (col)
                                        (cond ((null col) nil)
                                              ((equal col "") "")
                                              (t (read col))))
                                      row)
                              rows)))
        (nreverse rows))
    ((db-error sql-error)
     (pcase-let* ((`(,_ ,errmsg ,errcode) err)
                  (`(,_ ,_ ,signal ,errstr)
                   (assq errcode emacsql-sqlite-error-codes)))
       (signal (or signal 'emacsql-error)
               (list errmsg errcode nil errstr))))
    (error
     (signal 'emacsql-error (cdr err)))))

(cl-defmethod emacsql ((connection emacsql-sqlite-module-connection) sql &rest args)
  (emacsql-send-message connection (apply #'emacsql-compile connection sql args)))

(provide 'emacsql-sqlite-module)

;;; emacsql-sqlite-module.el ends here
