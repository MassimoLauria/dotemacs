(defun flymake-clang-c++-init ()
 (let* ((temp-file (flymake-init-create-temp-buffer-copy
 'flymake-create-temp-inplace))
 (local-file (file-relative-name
 temp-file
 (file-name-directory buffer-file-name))))
 (list "clang++" (list "--std=c++11" "-fsyntax-only" "-fno-color-diagnostics" local-file))))

(defun flymake-clang-c++-load ()
  (interactive)
  (unless (eq buffer-file-name nil)
    (add-to-list 'flymake-allowed-file-name-masks
 '("\\.cpp\\'" flymake-clang-c++-init))
    (add-to-list 'flymake-allowed-file-name-masks
 '("\\.cc\\'" flymake-clang-c++-init))
    (add-to-list 'flymake-allowed-file-name-masks
 '("\\.h\\'" flymake-clang-c++-init))
   (add-to-list 'flymake-allowed-file-name-masks
 '("\\.hh\\'" flymake-clang-c++-init))
   (add-to-list 'flymake-allowed-file-name-masks
 '("\\.hpp\\'" flymake-clang-c++-init))
     (flymake-mode t)))

(provide 'flymake-clang-c++)
