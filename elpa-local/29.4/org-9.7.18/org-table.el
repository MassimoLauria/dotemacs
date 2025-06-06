;;; org-table.el --- The Table Editor for Org        -*- lexical-binding: t; -*-

;; Copyright (C) 2004-2024 Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten.dominik@gmail.com>
;; Keywords: outlines, hypermedia, calendar, text
;; URL: https://orgmode.org
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This file contains the table editor and spreadsheet for Org mode.

;; Watch out:  Here we are talking about two different kind of tables.
;; Most of the code is for the tables created with the Org mode table editor.
;; Sometimes, we talk about tables created and edited with the table.el
;; Emacs package.  We call the former org-type tables, and the latter
;; table.el-type tables.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'cl-lib)
(require 'org-macs)
(require 'org-compat)
(require 'org-keys)
(require 'org-fold-core)

(declare-function calc-eval "calc" (str &optional separator &rest args))
(declare-function face-remap-remove-relative "face-remap" (cookie))
(declare-function face-remap-add-relative "face-remap" (face &rest specs))
(declare-function org-at-timestamp-p "org" (&optional extended))
(declare-function org-delete-backward-char "org" (N))
(declare-function org-mode "org" ())
(declare-function org-duration-p "org-duration" (duration &optional canonical))
(declare-function org-duration-to-minutes "org-duration" (duration &optional canonical))
(declare-function org-element-at-point "org-element" (&optional pom cached-only))
(declare-function org-element-contents "org-element-ast" (node))
(declare-function org-element-extract "org-element-ast" (node))
(declare-function org-element-interpret-data "org-element" (data))
(declare-function org-element-lineage "org-element-ast" (blob &optional types with-self))
(declare-function org-element-map "org-element" (data types fun &optional info first-match no-recursion with-affiliated))
(declare-function org-element-parse-buffer "org-element" (&optional granularity visible-only keep-deferred))
(declare-function org-element-property "org-element-ast" (property node))
(declare-function org-element-end "org-element" (node))
(declare-function org-element-post-affiliated "org-element" (node))
(declare-function org-element-type-p "org-element-ast" (node types))
(declare-function org-element-cache-reset "org-element" (&optional all no-persistence))
(declare-function org-entry-get "org" (pom property &optional inherit literal-nil))
(declare-function org-export-create-backend "ox" (&rest rest) t)
(declare-function org-export-data-with-backend "ox" (data backend info))
(declare-function org-export-filter-apply-functions "ox" (filters value info))
(declare-function org-export-first-sibling-p "ox" (blob info))
(declare-function org-export-get-backend "ox" (name))
(declare-function org-export-get-environment "ox" (&optional backend subtreep ext-plist))
(declare-function org-export-install-filters "ox" (info))
(declare-function org-export-table-has-special-column-p "ox" (table))
(declare-function org-export-table-row-is-special-p "ox" (table-row info))
(declare-function org-forward-paragraph "org" (&optional arg))
(declare-function org-id-find "org-id" (id &optional markerp))
(declare-function org-indent-line "org" ())
(declare-function org-load-modules-maybe "org" (&optional force))
(declare-function org-restart-font-lock "org" ())
(declare-function org-sort-remove-invisible "org" (s))
(declare-function org-time-stamp-format "org" (&optional long inactive))
(declare-function org-time-string-to-absolute "org" (s &optional daynr prefer buffer pos))
(declare-function org-time-string-to-time "org" (s))
(declare-function org-timestamp-up-day "org" (&optional arg))

(defvar constants-unit-system)
(defvar org-M-RET-may-split-line)
(defvar org-element-use-cache)
(defvar org-export-filters-alist)
(defvar org-finish-function)
(defvar org-inhibit-highlight-removal)
(defvar org-inhibit-startup)
(defvar org-selected-window)
(defvar org-self-insert-cluster-for-undo)
(defvar org-self-insert-command-undo-counter)
(defvar org-ts-regexp)
(defvar org-ts-regexp-both)
(defvar org-ts-regexp-inactive)
(defvar org-ts-regexp3)
(defvar org-window-configuration)
(defvar sort-fold-case)


;;; Customizables

(defgroup org-table nil
  "Options concerning tables in Org mode."
  :tag "Org Table"
  :group 'org)

(defcustom orgtbl-optimized t
  "Non-nil means use the optimized table editor version for `orgtbl-mode'.

In the optimized version, the table editor takes over all simple keys that
normally just insert a character.  In tables, the characters are inserted
in a way to minimize disturbing the table structure (i.e. in overwrite mode
for empty fields).  Outside tables, the correct binding of the keys is
restored.

Changing this variable requires a restart of Emacs to become
effective."
  :group 'org-table
  :type 'boolean)

(defcustom orgtbl-radio-table-templates
  '((latex-mode "% BEGIN RECEIVE ORGTBL %n
% END RECEIVE ORGTBL %n
\\begin{comment}
#+ORGTBL: SEND %n orgtbl-to-latex :splice nil :skip 0
| | |
\\end{comment}\n")
    (texinfo-mode "@c BEGIN RECEIVE ORGTBL %n
@c END RECEIVE ORGTBL %n
@ignore
#+ORGTBL: SEND %n orgtbl-to-html :splice nil :skip 0
| | |
@end ignore\n")
    (html-mode "<!-- BEGIN RECEIVE ORGTBL %n -->
<!-- END RECEIVE ORGTBL %n -->
<!--
#+ORGTBL: SEND %n orgtbl-to-html :splice nil :skip 0
| | |
-->\n")
    (org-mode "#+ BEGIN RECEIVE ORGTBL %n
#+ END RECEIVE ORGTBL %n

#+ORGTBL: SEND %n orgtbl-to-orgtbl :splice nil :skip 0
| | |
"))
  "Templates for radio tables in different major modes.
Each template must define lines that will be treated as a comment and that
must contain the \"BEGIN RECEIVE ORGTBL %n\" and \"END RECEIVE ORGTBL\"
lines where \"%n\" will be replaced with the name of the table during
insertion of the template.  The transformed table will later be inserted
between these lines.

The template should also contain a minimal table in a multiline comment.
If multiline comments are not possible in the buffer language,
you can pack it into a string that will not be used when the code
is compiled or executed.  Above the table will you need a line with
the fixed string \"#+ORGTBL: SEND\", followed by instruction on how to
convert the table into a data structure useful in the
language of the buffer.  Check the manual for the section on
\"Translator functions\", and more generally check out
the Info node `(org)Tables in arbitrary syntax'.

All occurrences of %n in a template will be replaced with the name of the
table, obtained by prompting the user."
  :group 'org-table
  :type '(repeat
	  (list (symbol :tag "Major mode")
		(string :tag "Format"))))

(defgroup org-table-settings nil
  "Settings for tables in Org mode."
  :tag "Org Table Settings"
  :group 'org-table)

(defcustom org-table-header-line-p nil
  "Activate `org-table-header-line-mode' by default?"
  :type 'boolean
  :package-version '(Org . "9.4")
  :group 'org-table)

(defcustom org-table-default-size "5x2"
  "The default size for newly created tables, Columns x Rows."
  :group 'org-table-settings
  :type 'string)

(defcustom org-table-number-regexp
  "^\\([<>]?[-+^.0-9]*[0-9][-+^.0-9eEdDx()%:]*\\|[<>]?[-+]?0[xX][[:xdigit:].]+\\|[<>]?[-+]?[0-9]+#[0-9a-zA-Z.]+\\|nan\\|[-+u]?inf\\)$"
  "Regular expression for recognizing numbers in table columns.
If a table column contains mostly numbers, it will be aligned to the
right.  If not, it will be aligned to the left.

The default value of this option is a regular expression which allows
anything which looks remotely like a number as used in scientific
context.  For example, all of the following will be considered a
number:
    12    12.2    2.4e-08    2x10^12    4.034+-0.02    2.7(10)  >3.5

Other options offered by the customize interface are more restrictive."
  :group 'org-table-settings
  :type '(choice
	  (const :tag "Positive Integers"
		 "^[0-9]+$")
	  (const :tag "Integers"
		 "^[-+]?[0-9]+$")
	  (const :tag "Floating Point Numbers"
		 "^[-+]?\\([0-9]*\\.[0-9]+\\|[0-9]+\\.[0-9]*\\)$")
	  (const :tag "Floating Point Number or Integer"
		 "^[-+]?\\([0-9]*\\.[0-9]+\\|[0-9]+\\.?[0-9]*\\)$")
	  (const :tag "Exponential, Floating point, Integer"
		 "^[-+]?[0-9.]+\\([eEdD][-+0-9]+\\)?$")
	  (const :tag "Very General Number-Like, including hex and Calc radix"
		 "^\\([<>]?[-+^.0-9]*[0-9][-+^.0-9eEdDx()%]*\\|[<>]?[-+]?0[xX][[:xdigit:].]+\\|[<>]?[-+]?[0-9]+#[0-9a-zA-Z.]+\\|nan\\|[-+u]?inf\\)$")
	  (const :tag "Very General Number-Like, including hex and Calc radix, allows comma as decimal mark"
		 "^\\([<>]?[-+^.,0-9]*[0-9][-+^.0-9eEdDx()%]*\\|[<>]?[-+]?0[xX][[:xdigit:].]+\\|[<>]?[-+]?[0-9]+#[0-9a-zA-Z.]+\\|nan\\|[-+u]?inf\\)$")
	  (regexp :tag "Regexp:")))

(defcustom org-table-number-fraction 0.5
  "Fraction of numbers in a column required to make the column align right.
In a column all non-white fields are considered.  If at least
this fraction of fields is matched by `org-table-number-regexp',
alignment to the right border applies."
  :group 'org-table-settings
  :type 'number)

(defcustom org-table-formula-field-format "%s"
  "Format for fields which contain the result of a formula.
For example, using \"~%s~\" will display the result within tilde
characters.  Beware that modifying the display can prevent the
field from being used in another formula."
  :group 'org-table-settings
  :version "24.1"
  :type 'string)

(defgroup org-table-editing nil
  "Behavior of tables during editing in Org mode."
  :tag "Org Table Editing"
  :group 'org-table)

(defcustom org-table-automatic-realign t
  "Non-nil means automatically re-align table when pressing TAB or RETURN.
When nil, aligning is only done with `\\[org-table-align]', or after column
removal/insertion."
  :group 'org-table-editing
  :type 'boolean)

(defcustom org-table-auto-blank-field t
  "Non-nil means automatically blank table field when starting to type into it.
This only happens when typing immediately after a field motion
command (TAB, S-TAB or RET)."
  :group 'org-table-editing
  :type 'boolean)

(defcustom org-table-exit-follow-field-mode-when-leaving-table t
  "Non-nil means automatically exit the follow mode.
When nil, the follow mode will stay on and be active in any table
the cursor enters.  Since the table follow filed mode messes with the
window configuration, it is not recommended to set this variable to nil,
except maybe locally in a special file that has mostly tables with long
fields."
  :group 'org-table
  :version "24.1"
  :type 'boolean)

(defcustom org-table-fix-formulas-confirm nil
  "Whether the user should confirm when Org fixes formulas."
  :group 'org-table-editing
  :version "24.1"
  :type '(choice
	  (const :tag "with yes-or-no" yes-or-no-p)
	  (const :tag "with y-or-n" y-or-n-p)
	  (const :tag "no confirmation" nil)))

(defcustom org-table-tab-jumps-over-hlines t
  "Non-nil means tab in the last column of a table with jump over a hline.
If a horizontal separator line is following the current line,
`org-table-next-field' can either create a new row before that line, or jump
over the line.  When this option is nil, a new line will be created before
this line."
  :group 'org-table-editing
  :type 'boolean)

(defcustom org-table-shrunk-column-indicator "…"
  "String to be displayed in a shrunk column."
  :group 'org-table-editing
  :type 'string
  :package-version '(Org . "9.2")
  :safe (lambda (v) (and (stringp v) (not (equal v "")))))

(defgroup org-table-calculation nil
  "Options concerning tables in Org mode."
  :tag "Org Table Calculation"
  :group 'org-table)

(defcustom org-table-use-standard-references 'from
  "Non-nil means using table references like B3 instead of @3$2.
Possible values are:
nil     never use them
from    accept as input, do not present for editing
t       accept as input and present for editing"
  :group 'org-table-calculation
  :type '(choice
	  (const :tag "Never, don't even check user input for them" nil)
	  (const :tag "Always, both as user input, and when editing" t)
	  (const :tag "Convert user input, don't offer during editing" from)))

(defcustom org-table-copy-increment t
  "Non-nil means increment when copying current field with \
`\\[org-table-copy-down]'."
  :group 'org-table-calculation
  :version "26.1"
  :package-version '(Org . "8.3")
  :type '(choice
	  (const :tag "Use the difference between the current and the above fields" t)
	  (integer :tag "Use a number" 1)
	  (const :tag "Don't increment the value when copying a field" nil)))

(defcustom org-calc-default-modes
  '(calc-internal-prec 12
		       calc-float-format  (float 8)
		       calc-angle-mode    deg
		       calc-prefer-frac   nil
		       calc-symbolic-mode nil
		       calc-date-format (YYYY "-" MM "-" DD " " Www (" " hh ":" mm))
		       calc-display-working-message t)
  "List with Calc mode settings for use in `calc-eval' for table formulas.
The list must contain alternating symbols (Calc modes variables and values).
Don't remove any of the default settings, just change the values.  Org mode
relies on the variables to be present in the list."
  :group 'org-table-calculation
  :type 'plist)

(defcustom org-table-duration-custom-format 'hours
  "Format for the output of calc computations like $1+$2;t.
The default value is `hours', and will output the results as a
number of hours.  Other allowed values are `seconds', `minutes' and
`days', and the output will be a fraction of seconds, minutes or
days.  `hh:mm' selects to use hours and minutes, ignoring seconds.
The `U' flag in a table formula will select this specific format for
a single formula."
  :group 'org-table-calculation
  :version "24.1"
  :type '(choice (symbol :tag "Seconds" 'seconds)
		 (symbol :tag "Minutes" 'minutes)
		 (symbol :tag "Hours  " 'hours)
		 (symbol :tag "Days   " 'days)
		 (symbol :tag "HH:MM  " 'hh:mm)))

(defcustom org-table-duration-hour-zero-padding t
  "Non-nil means hours in table duration computations should be zero-padded.
So this is about 08:32:34 versus 8:33:34."
  :group 'org-table-calculation
  :version "26.1"
  :package-version '(Org . "9.1")
  :type 'boolean
  :safe #'booleanp)

(defcustom org-table-formula-evaluate-inline t
  "Non-nil means TAB and RET evaluate a formula in current table field.
If the current field starts with an equal sign, it is assumed to be a formula
which should be evaluated as described in the manual and in the documentation
string of the command `org-table-eval-formula'.  This feature requires the
Emacs calc package.
When this variable is nil, formula calculation is only available through
the command `\\[org-table-eval-formula]'."
  :group 'org-table-calculation
  :type 'boolean)

(defcustom org-table-formula-use-constants t
  "Non-nil means interpret constants in formulas in tables.
A constant looks like `$c' or `$Grav' and will be replaced before evaluation
by the value given in `org-table-formula-constants', or by a value obtained
from the `constants.el' package."
  :group 'org-table-calculation
  :type 'boolean)

(defcustom org-table-formula-constants nil
  "Alist with constant names and values, for use in table formulas.
The car of each element is a name of a constant, without the `$' before it.
The cdr is the value as a string.  For example, if you'd like to use the
speed of light in a formula, you would configure

  (setq org-table-formula-constants \\='((\"c\" . \"299792458.\")))

and then use it in an equation like `$1*$c'.

Constants can also be defined on a per-file basis using a line like

#+CONSTANTS: c=299792458. pi=3.14 eps=2.4e-6"
  :group 'org-table-calculation
  :type '(repeat
	  (cons (string :tag "name")
		(string :tag "value"))))

(defcustom org-table-allow-automatic-line-recalculation t
  "Non-nil means lines marked with |#| or |*| will be recomputed automatically.
\\<org-mode-map>\
Automatically means when `TAB' or `RET' or `\\[org-ctrl-c-ctrl-c]' \
are pressed in the line."
  :group 'org-table-calculation
  :type 'boolean)

(defcustom org-table-relative-ref-may-cross-hline t
  "Non-nil means relative formula references may cross hlines.
Here are the allowed values:

nil    Relative references may not cross hlines.  They will reference the
       field next to the hline instead.  Coming from below, the reference
       will be to the field below the hline.  Coming from above, it will be
       to the field above.
t      Relative references may cross hlines.
error  An attempt to cross a hline will throw an error.

It is probably good to never set this variable to nil, for the sake of
portability of tables."
  :group 'org-table-calculation
  :type '(choice
          (const :tag "Allow crossing hline" t)
	  (const :tag "Stick to hline" nil)
	  (const :tag "Error on attempt to cross" error)))

(defcustom org-table-formula-create-columns nil
  "Non-nil means evaluation of formula can add new columns.
When non-nil, evaluating an out-of-bounds field can insert as
many columns as needed.  When set to `warn', issue a warning when
doing so.  When set to `prompt', ask user before creating a new
column.  Otherwise, throw an error."
  :group 'org-table-calculation
  :package-version '(Org . "8.3")
  :type '(choice
	  (const :tag "Out-of-bounds field generates an error (default)" nil)
	  (const :tag "Out-of-bounds field silently adds columns as needed" t)
	  (const :tag "Out-of-bounds field adds columns, but issues a warning" warn)
	  (const :tag "Prompt user when setting an out-of-bounds field" prompt)))

(defgroup org-table-import-export nil
  "Options concerning table import and export in Org mode."
  :tag "Org Table Import Export"
  :group 'org-table)

(defcustom org-table-export-default-format "orgtbl-to-tsv"
  "Default export parameters for `org-table-export'.
These can be overridden for a specific table by setting the
TABLE_EXPORT_FORMAT property.  See the manual section on orgtbl
radio tables for the different export transformations and
available parameters."
  :group 'org-table-import-export
  :type 'string)

(defcustom org-table-convert-region-max-lines 999
  "Max lines that `org-table-convert-region' will attempt to process.

The function can be slow on larger regions; this safety feature
prevents it from hanging Emacs."
  :group 'org-table-import-export
  :type 'integer
  :package-version '(Org . "8.3"))


;;; Org table header minor mode
(defun org-table-row-get-visible-string (&optional pos)
  "Get the visible string of a table row.
This may be useful when columns have been shrunk."
  (save-excursion
    (when pos (goto-char pos))
    (let* ((beg (line-beginning-position))
           (end (line-end-position))
           (str (buffer-substring beg end)))
      ;; FIXME: This does not handle intersecting overlays.
      (dolist (ov (overlays-in beg end))
        (when (overlay-get ov 'display)
          (put-text-property
           (- (overlay-start ov) beg) (- (overlay-end ov) beg)
           'display (overlay-get ov 'display)
           str)))
      str)))

(defvar-local org-table-header-overlay nil)
(put 'org-table-header-overlay 'permanent-local t)
(defun org-table-header-set-header ()
  "Display the header of the table at point."
  (let ((gcol temporary-goal-column))
    (unwind-protect
        (progn
          (when (overlayp org-table-header-overlay)
            (delete-overlay org-table-header-overlay))
          ;; We might be called after scrolling but before display is
          ;; updated. Make sure that any queued redisplay is executed
          ;; before we look into `window-start'.
          (redisplay)
          (let* ((ws (window-start))
                 (beg (save-excursion
                        ;; Check table at window start, not at point.
                        ;; Point might be after the table, or at
                        ;; another table located below the one visible
                        ;; on top.
                        (goto-char ws)
                        (goto-char (org-table-begin))
                        (while (or (org-at-table-hline-p)
                                   (looking-at-p ".*|\\s-+<[rcl]?\\([0-9]+\\)?>"))
                          (move-beginning-of-line 2))
                        (line-beginning-position))))
            (if (pos-visible-in-window-p beg)
                (when (overlayp org-table-header-overlay)
                  (delete-overlay org-table-header-overlay))
              (setq org-table-header-overlay
                    (make-overlay
                     (save-excursion (goto-char ws) (line-beginning-position))
                     (save-excursion (goto-char ws) (line-end-position))))
              (org-overlay-display
               org-table-header-overlay
               (org-table-row-get-visible-string beg)
               'org-table-header))))
      (setq temporary-goal-column gcol))))

;;;###autoload
(define-minor-mode org-table-header-line-mode
  "Display the first row of the table at point in the header line."
  :lighter " TblHeader"
  (unless (eq major-mode 'org-mode)
    (user-error "Cannot turn org table header mode outside org-mode buffers"))
  (if org-table-header-line-mode
      (add-hook 'post-command-hook #'org-table-header-set-header nil t)
    (when (overlayp org-table-header-overlay)
      (delete-overlay org-table-header-overlay)
      (setq org-table-header-overlay nil))
    (remove-hook 'post-command-hook #'org-table-header-set-header t)))


;;; Regexps Constants

(defconst org-table-any-line-regexp "^[ \t]*\\(|\\|\\+-[-+]\\)"
  "Detect an org-type or table-type table.")

(defconst org-table-line-regexp "^[ \t]*|"
  "Detect an org-type table line.")

(defconst org-table-dataline-regexp "^[ \t]*|[^-]"
  "Detect an org-type table line.")

(defconst org-table-hline-regexp "^[ \t]*|-"
  "Detect an org-type table hline.")

(defconst org-table1-hline-regexp "^[ \t]*\\+-[-+]"
  "Detect a table-type table hline.")

(defconst org-table-any-border-regexp "^[ \t]*[^|+ \t]"
  "Detect the first line outside a table when searching from within it.
This works for both table types.")

(defconst org-TBLFM-regexp "^[ \t]*#\\+TBLFM: "
  "Detect a #+TBLFM line.")

(defvar org-table-TBLFM-begin-regexp "^[ \t]*|.*\n[ \t]*#\\+TBLFM: ")

(defconst org-table-auto-recalculate-regexp "^[ \t]*| *# *\\(|\\|$\\)"
  "Regexp matching a line marked for automatic recalculation.")

(defconst org-table-recalculate-regexp "^[ \t]*| *[#*] *\\(|\\|$\\)"
  "Regexp matching a line marked for recalculation.")

(defconst org-table-calculate-mark-regexp "^[ \t]*| *[!$^_#*] *\\(|\\|$\\)"
  "Regexp matching a line marked for calculation.")

(defconst org-table-border-regexp "^[ \t]*[^| \t]"
  "Regexp matching any line outside an Org table.")

(defconst org-table-range-regexp
  "@\\([-+]?I*[-+]?[0-9]*\\)\\(\\$[-+]?[0-9]+\\)?\\(\\.\\.@?\\([-+]?I*[-+]?[0-9]*\\)\\(\\$[-+]?[0-9]+\\)?\\)?"
  ;;   1                        2                    3          4                        5
  "Regular expression for matching ranges in formulas.")

(defconst org-table-range-regexp2
  (concat
   "\\(" "@[-0-9I$&]+" "\\|" "[a-zA-Z]\\{1,2\\}\\([0-9]+\\|&\\)" "\\|" "\\$[a-zA-Z0-9]+" "\\)"
   "\\.\\."
   "\\(" "@?[-0-9I$&]+" "\\|" "[a-zA-Z]\\{1,2\\}\\([0-9]+\\|&\\)" "\\|" "\\$[a-zA-Z0-9]+" "\\)")
  "Match a range for reference display.")

(defconst org-table-translate-regexp
  (concat "\\(" "@[-0-9I$]+" "\\|" "[a-zA-Z]\\{1,2\\}\\([0-9]+\\|&\\)" "\\)")
  "Match a reference that needs translation, for reference display.")

(defconst org-table--separator-space-pre
  (propertize " " 'display '(space :relative-width 1) 'rear-nonsticky t)
  "Space used in front of fields when aligning the table.
This space serves as a segment separator for the purposes of the
bidirectional reordering.
Note that `org-table--separator-space-pre' is not `eq' to
`org-table--separator-space-post'.  This is done to prevent Emacs from
visually merging spaces in an empty table cell.  See bug#45915.")

(defconst org-table--separator-space-post
  (propertize " " 'display '(space :relative-width 1.001))
  "Space used after fields when aligning the table.
This space serves as a segment separator for the purposes of the
bidirectional reordering.
Note that `org-table--separator-space-pre' is not `eq' to
`org-table--separator-space-post'.  This is done to prevent Emacs from
visually merging spaces in an empty table cell.  See bug#45915.")


;;; Internal Variables

(defvar org-table-last-highlighted-reference nil)

(defvar org-table-formula-history nil)

(defvar org-field-marker nil)
(defvar org-table-buffer-is-an nil)

(defvar-local org-table-formula-constants-local nil
  "Local version of `org-table-formula-constants'.")

(defvar org-table-may-need-update t
  "Indicates that a table might need an update.
This variable is set by `org-before-change-function'.
`org-table-align' sets it back to nil.")

(defvar orgtbl-after-send-table-hook nil
  "Hook for functions attaching to \\`C-c C-c', if the table is sent.
This can be used to add additional functionality after the table is sent
to the receiver position, otherwise, if table is not sent, the functions
are not run.")

(defvar org-table-column-names nil
  "Alist with column names, derived from the `!' line.
This variable is initialized with `org-table-analyze'.")

(defvar org-table-column-name-regexp nil
  "Regular expression matching the current column names.
This variable is initialized with `org-table-analyze'.")

(defvar org-table-local-parameters nil
  "Alist with parameter names, derived from the `$' line.
This variable is initialized with `org-table-analyze'.")

(defvar org-table-named-field-locations nil
  "Alist with locations of named fields.
Associations follow the pattern (NAME LINE COLUMN) where
  NAME is the name of the field as a string,
  LINE is the number of lines from the beginning of the table,
  COLUMN is the column of the field, as an integer.
This variable is initialized with `org-table-analyze'.")

(defvar org-table-current-line-types nil
  "Table row types in current table.
This variable is initialized with `org-table-analyze'.")

(defvar org-table-current-begin-pos nil
  "Current table begin position, as a marker.
This variable is initialized with `org-table-analyze'.")

(defvar org-table-current-ncol nil
  "Number of columns in current table.
This variable is initialized with `org-table-analyze'.")

(defvar org-table-dlines nil
  "Vector of data line line numbers in the current table.
Line numbers are counted from the beginning of the table.  This
variable is initialized with `org-table-analyze'.")

(defvar org-table-hlines nil
  "Vector of hline line numbers in the current table.
Line numbers are counted from the beginning of the table.  This
variable is initialized with `org-table-analyze'.")

(defvar org-table-aligned-begin-marker (make-marker)
  "Marker at the beginning of the table last aligned.
Used to check if cursor still is in that table, to minimize realignment.")

(defvar org-table-aligned-end-marker (make-marker)
  "Marker at the end of the table last aligned.
Used to check if cursor still is in that table, to minimize realignment.")

(defvar org-table-last-alignment nil
  "List of flags for flushright alignment, from the last re-alignment.
This is being used to correctly align a single field after TAB or RET.")

(defvar org-table-last-column-widths nil
  "List of max width of fields in each column.
This is being used to correctly align a single field after TAB or RET.")

(defvar-local org-table-formula-debug nil
  "Non-nil means debug table formulas.
When nil, simply write \"#ERROR\" in corrupted fields.")

(defvar-local org-table-overlay-coordinates nil
  "Overlay coordinates after each align of a table.")

(defvar org-last-recalc-line nil)

(defvar org-show-positions nil)

(defvar org-table-rectangle-overlays nil)

(defvar org-table-clip nil
  "Clipboard for table regions.")

(defvar org-timecnt nil)

(defvar org-recalc-commands nil
  "List of commands triggering the recalculation of a line.
Will be filled automatically during use.")

(defvar org-recalc-marks
  '((" " . "Unmarked: no special line, no automatic recalculation")
    ("#" . "Automatically recalculate this line upon TAB, RET, and C-c C-c in the line")
    ("*" . "Recalculate only when entire table is recalculated with `C-u C-c *'")
    ("!" . "Column name definition line.  Reference in formula as $name.")
    ("$" . "Parameter definition line name=value.  Reference in formula as $name.")
    ("_" . "Names for values in row below this one.")
    ("^" . "Names for values in row above this one.")))

(defvar org-pos nil)


;;; Macros and Inlined Functions

(defmacro org-table-with-shrunk-columns (&rest body)
  "Expand all columns before executing BODY, then shrink them again."
  (declare (debug (body)))
  (org-with-gensyms (shrunk-columns begin end)
    `(let ((,begin (copy-marker (org-table-begin)))
	   (,end (copy-marker (org-table-end) t))
	   (,shrunk-columns (org-table--list-shrunk-columns)))
       (org-with-point-at ,begin (org-table-expand ,begin ,end))
       (unwind-protect
	   (progn ,@body)
	 (org-table--shrink-columns ,shrunk-columns ,begin ,end)
	 (set-marker ,begin nil)
	 (set-marker ,end nil)))))

(defmacro org-table-with-shrunk-field (&rest body)
  "Save field shrunk state, execute BODY and restore state."
  (declare (debug (body)))
  (org-with-gensyms (end shrunk size)
    `(let* ((,shrunk (save-match-data (org-table--shrunk-field)))
	    (,end (and ,shrunk (copy-marker (overlay-end ,shrunk) t)))
	    (,size (and ,shrunk (- ,end (overlay-start ,shrunk)))))
       (when ,shrunk (delete-overlay ,shrunk))
       (unwind-protect (progn ,@body)
	 (when ,shrunk (move-overlay ,shrunk (- ,end ,size) ,end))))))

(defmacro org-table-save-field (&rest body)
  "Save current field; execute BODY; restore field.
Field is restored even in case of abnormal exit."
  (declare (debug (body)))
  (org-with-gensyms (line column)
    `(let ((,line (copy-marker (line-beginning-position)))
	   (,column (org-table-current-column)))
       (unwind-protect
	   (progn ,@body)
	 (goto-char ,line)
	 (org-table-goto-column ,column)
	 (set-marker ,line nil)))))


;;; Predicates

(defun org-at-TBLFM-p (&optional pos)
  "Non-nil when point (or POS) is in #+TBLFM line."
  (save-excursion
    (goto-char (or pos (point)))
    (forward-line 0)
    (and (let ((case-fold-search t)) (looking-at org-TBLFM-regexp))
	 (org-element-type-p (org-element-at-point) 'table))))

(defun org-at-table-p (&optional table-type)
  "Non-nil if the cursor is inside an Org table.
If TABLE-TYPE is non-nil, also check for table.el-type tables."
  (and (org-match-line (if table-type "[ \t]*[|+]" "[ \t]*|"))
       (or (not (derived-mode-p 'org-mode))
	   (let ((e (org-element-lineage (org-element-at-point) 'table t)))
	     (and e (or table-type
			(eq 'org (org-element-property :type e))))))))

(defun org-at-table.el-p ()
  "Non-nil when point is at a table.el table."
  (and (org-match-line "[ \t]*[|+]")
       (let ((element (org-element-at-point)))
	 (and (org-element-type-p element 'table)
	      (eq (org-element-property :type element) 'table.el)))))

(defun org-at-table-hline-p ()
  "Non-nil when point is inside a hline in a table.
Assume point is already in a table."
  (org-match-line org-table-hline-regexp))

(defun org-table-check-inside-data-field (&optional noerror assume-table)
  "Non-nil when point is inside a table data field.
Raise an error otherwise, unless NOERROR is non-nil.  In that
case, return nil if point is not inside a data field.  When
optional argument ASSUME-TABLE is non-nil, assume point is within
a table."
  (cond ((and (or assume-table (org-at-table-p))
	      (not (save-excursion (skip-chars-backward " \t") (bolp)))
	      (not (org-at-table-hline-p))
	      (not (looking-at-p "[ \t]*$"))))
	(noerror nil)
	(t (user-error "Not in table data field"))))


;;; Create, Import, and Convert Tables

;;;###autoload
(defun org-table-create-with-table.el ()
  "Use the table.el package to insert a new table.
If there is already a table at point, convert between Org tables
and table.el tables."
  (interactive)
  (require 'table)
  (cond
   ((and (org-at-table.el-p)
	 (y-or-n-p "Convert table to Org table? "))
    (org-table-convert))
   ((and (org-at-table-p)
	 (y-or-n-p "Convert table to table.el table? "))
    (org-table-align)
    (org-table-convert))
   (t (call-interactively 'table-insert))))

;;;###autoload
(defun org-table-create-or-convert-from-region (arg)
  "Convert region to table, or create an empty table.
If there is an active region, convert it to a table, using the function
`org-table-convert-region'.  See the documentation of that function
to learn how the prefix argument is interpreted to determine the field
separator.
If there is no such region, create an empty table with `org-table-create'."
  (interactive "P")
  (if (org-region-active-p)
      (org-table-convert-region (region-beginning) (region-end) arg)
    (org-table-create arg)))

;;;###autoload
(defun org-table-create (&optional size)
  "Query for a size and insert a table skeleton.
SIZE is a string Columns x Rows like for example \"3x2\"."
  (interactive "P")
  (unless size
    (setq size (read-string
		(concat "Table size Columns x Rows [e.g. "
			org-table-default-size "]: ")
		"" nil org-table-default-size)))

  (let* ((pos (point))
	 (indent (make-string (current-column) ?\ ))
	 (split (org-split-string size " *x *"))
	 (rows (string-to-number (nth 1 split)))
	 (columns (string-to-number (car split)))
	 (line (concat (apply 'concat indent "|" (make-list columns "  |"))
		       "\n")))
    (if (string-match "^[ \t]*$" (buffer-substring-no-properties
                                  (line-beginning-position) (point)))
	(forward-line 0)
      (newline))
    ;; (mapcar (lambda (x) (insert line)) (make-list rows t))
    (dotimes (_ rows) (insert line))
    (goto-char pos)
    (when (> rows 1)
      ;; Insert a hline after the first row.
      (end-of-line 1)
      (insert "\n|-")
      (goto-char pos))
    (org-table-align)))

;;;###autoload
(defun org-table-convert-region (beg0 end0 &optional separator)
  "Convert region to a table.

The region goes from BEG0 to END0, but these borders will be moved
slightly, to make sure a beginning of line in the first line is
included.

Throw an error when the region has more than
`org-table-convert-region-max-lines' lines.

SEPARATOR specifies the field separator in the lines.  It can have the
following values:

(4)     Use the comma as a field separator
(16)    Use a TAB as field separator
(64)    Prompt for a regular expression as field separator
integer  When a number, use that many spaces, or a TAB, as field separator
regexp   When a regular expression, use it to match the separator
nil      When nil, the command tries to be smart and figure out the
         separator in the following way:
         - when each line contains a TAB, assume TAB-separated material
         - when each line contains a comma, assume CSV material
         - else, assume one or more SPACE characters as separator.
`babel-auto'
       Use the same rules as nil, but do not try any separator when
       the region contains a single line and has no commas or tabs."
  (interactive "r\nP")
  (let* ((beg (min beg0 end0))
	 (end (max beg0 end0))
	 re)
    (when (> (count-lines beg end) org-table-convert-region-max-lines)
      (user-error "Region is longer than `org-table-convert-region-max-lines' (%s) lines; not converting"
		  org-table-convert-region-max-lines))
    (when (equal separator '(64))
      (setq separator (read-regexp "Regexp for field separator")))
    (goto-char beg)
    (forward-line 0)
    (setq beg (point-marker))
    (goto-char end)
    (if (bolp) (backward-char 1) (end-of-line 1))
    (setq end (point-marker))
    ;; Get the right field separator
    (when (or (not separator) (eq separator 'babel-auto))
      (goto-char beg)
      (setq separator
	    (cond
	     ((not (save-excursion (re-search-forward "^[^\n\t]+$" end t))) '(16))
	     ((not (save-excursion (re-search-forward "^[^\n,]+$" end t))) '(4))
             ((and (eq separator 'babel-auto)
                   (= 1 (count-lines beg end)))
              (rx unmatchable))
	     (t 1))))
    (goto-char beg)
    (if (equal separator '(4))
	(while (< (point) end)
	  ;; parse the csv stuff
	  (cond
	   ((looking-at "^") (insert "| "))
	   ((looking-at "[ \t]*$") (replace-match " |") (forward-line 1))
	   ((looking-at "[ \t]*\"\\([^\"\n]*\\)\"")
	    (replace-match "\\1")
	    (if (looking-at "\"") (insert "\"")))
	   ((looking-at "[^,\n]+") (goto-char (match-end 0)))
	   ((looking-at "[ \t]*,") (replace-match " | "))
	   (t (forward-line 1))))
      (setq re (cond
		((equal separator '(4)) "^\\|\"?[ \t]*,[ \t]*\"?")
		((equal separator '(16)) "^\\|\t")
		((integerp separator)
		 (if (< separator 1)
		     (user-error "Number of spaces in separator must be >= 1")
		   (format "^ *\\| *\t *\\| \\{%d,\\}" separator)))
		((stringp separator)
		 (format "^ *\\|%s" separator))
		(t (error "This should not happen"))))
      (while (re-search-forward re end t)
	(replace-match "| " t t)))
    (goto-char beg)
    (org-table-align)))

;;;###autoload
(defun org-table-import (file separator)
  "Import FILE as a table.

The command tries to be smart and figure out the separator in the
following way:

- when each line contains a TAB, assume TAB-separated material;
- when each line contains a comma, assume CSV material;
- else, assume one or more SPACE characters as separator.

When non-nil, SEPARATOR specifies the field separator in the
lines.  It can have the following values:

- (4)     Use the comma as a field separator.
- (16)    Use a TAB as field separator.
- (64)    Prompt for a regular expression as field separator.
- integer When a number, use that many spaces, or a TAB, as field separator.
- regexp  When a regular expression, use it to match the separator."
  (interactive "f\nP")
  (when (and (called-interactively-p 'any)
	     (not (string-match-p (rx "." (or "txt" "tsv" "csv") eos) file))
             (not (yes-or-no-p "The file's extension is not .txt, .tsv or .csv.  Import? ")))
    (user-error "Cannot import such file"))
  (unless (bolp) (insert "\n"))
  (let ((beg (point))
	(pm (point-max)))
    (insert-file-contents file)
    (org-table-convert-region beg (+ (point) (- (point-max) pm)) separator)))

(defun org-table-convert ()
  "Convert from Org table to table.el and back.
Obviously, this only works within limits.  When an Org table is converted
to table.el, all horizontal separator lines get lost, because table.el uses
these as cell boundaries and has no notion of horizontal lines.  A table.el
table can be converted to an Org table only if it does not do row or column
spanning.  Multiline cells will become multiple cells.  Beware, Org mode
does not test if the table can be successfully converted - it blindly
applies a recipe that works for simple tables."
  (interactive)
  (require 'table)
  (if (org-at-table.el-p)
      ;; convert to Org table
      (let ((beg (copy-marker (org-table-begin t)))
	    (end (copy-marker (org-table-end t))))
	(table-unrecognize-region beg end)
	(goto-char beg)
	(while (re-search-forward "^\\([ \t]*\\)\\+-.*\n" end t)
	  (replace-match ""))
	(goto-char beg))
    (if (org-at-table-p)
	;; convert to table.el table
	(let ((beg (copy-marker (org-table-begin)))
	      (end (copy-marker (org-table-end))))
	  ;; first, get rid of all horizontal lines
	  (goto-char beg)
	  (while (re-search-forward "^\\([ \t]*\\)|-.*\n" end t)
	    (replace-match ""))
	  ;; insert a hline before first
	  (goto-char beg)
	  (org-table-insert-hline 'above)
	  (forward-line -2)
	  ;; insert a hline after each line
	  (while (progn (forward-line 2) (< (point) end))
	    (org-table-insert-hline))
	  (goto-char beg)
	  (setq end (move-marker end (org-table-end)))
	  ;; replace "+" at beginning and ending of hlines
	  (while (re-search-forward "^\\([ \t]*\\)|-" end t)
	    (replace-match "\\1+-"))
	  (goto-char beg)
	  (while (re-search-forward "-|[ \t]*$" end t)
	    (replace-match "-+"))
	  (goto-char beg)))))


;;; Navigation and Structure Editing

;;;###autoload
(defun org-table-begin (&optional table-type)
  "Find the beginning of the table and return its position.
With a non-nil optional argument TABLE-TYPE, return the beginning
of a table.el-type table.  This function assumes point is on
a table."
  (cond (table-type
	 (org-element-post-affiliated (org-element-at-point)))
	((save-excursion
	   (and (re-search-backward org-table-border-regexp nil t)
		(line-beginning-position 2))))
	(t (point-min))))

;;;###autoload
(defun org-table-end (&optional table-type)
  "Find the end of the table and return its position.
With a non-nil optional argument TABLE-TYPE, return the end of
a table.el-type table.  This function assumes point is on
a table."
  (save-excursion
    (cond (table-type
	   (goto-char (org-element-end (org-element-at-point)))
	   (skip-chars-backward " \t\n")
	   (line-beginning-position 2))
	  ((re-search-forward org-table-border-regexp nil t)
	   (match-beginning 0))
	  ;; When the line right after the table is the last line in
	  ;; the buffer with trailing spaces but no final newline
	  ;; character, be sure to catch the correct ending at its
	  ;; beginning.  In any other case, ending is expected to be
	  ;; at point max.
	  (t (goto-char (point-max))
	     (skip-chars-backward " \t")
	     (if (bolp) (point) (line-end-position))))))

;;;###autoload
(defun org-table-next-field ()
  "Go to the next field in the current table, creating new lines as needed.
Before doing so, re-align the table if necessary."
  (interactive)
  (org-table-maybe-eval-formula)
  (org-table-maybe-recalculate-line)
  (when (and org-table-automatic-realign
	     org-table-may-need-update)
    (org-table-align))
  (let ((end (org-table-end)))
    (if (org-at-table-hline-p)
	(end-of-line 1))
    (condition-case nil
	(progn
	  (re-search-forward "|" end)
	  (if (looking-at "[ \t]*$")
	      (re-search-forward "|" end))
	  (if (and (looking-at "-")
		   org-table-tab-jumps-over-hlines
		   (re-search-forward "^[ \t]*|\\([^-]\\)" end t))
	      (goto-char (match-beginning 1)))
	  (if (looking-at "-")
	      (progn
		(forward-line -1)
		(org-table-insert-row 'below))
	    (if (looking-at " ") (forward-char 1))))
      (error
       (org-table-insert-row 'below)))))

;;;###autoload
(defun org-table-previous-field ()
  "Go to the previous field in the table.
Before doing so, re-align the table if necessary."
  (interactive)
  (org-table-justify-field-maybe)
  (org-table-maybe-recalculate-line)
  (when (and org-table-automatic-realign
	     org-table-may-need-update)
    (org-table-align))
  (when (org-at-table-hline-p)
    (end-of-line))
  (let ((start (org-table-begin))
	(origin (point)))
    (condition-case nil
	(progn
	  (search-backward "|" start nil 2)
	  (while (looking-at-p "|\\(?:-\\|[ \t]*$\\)")
	    (search-backward "|" start)))
      (error
       (goto-char origin)
       (user-error "Cannot move to previous table field"))))
  (when (looking-at "| ?")
    (goto-char (match-end 0))))

(defun org-table-beginning-of-field (&optional n)
  "Move to the beginning of the current table field.
If already at or before the beginning, move to the beginning of the
previous field.
With numeric argument N, move N-1 fields backward first."
  (interactive "p")
  (let ((pos (point)))
    (while (> n 1)
      (setq n (1- n))
      (org-table-previous-field))
    (if (not (re-search-backward "|" (line-beginning-position 0) t))
	(user-error "No more table fields before the current")
      (goto-char (match-end 0))
      (and (looking-at " ") (forward-char 1)))
    (when (>= (point) pos) (org-table-beginning-of-field 2))))

(defun org-table-end-of-field (&optional n)
  "Move to the end of the current table field.
If already at or after the end, move to the end of the next table field.
With numeric argument N, move N-1 fields forward first."
  (interactive "p")
  (let ((pos (point)))
    (while (> n 1)
      (setq n (1- n))
      (org-table-next-field))
    (when (re-search-forward "|" (line-end-position 1) t)
      (backward-char 1)
      (skip-chars-backward " ")
      (when (and (equal (char-before (point)) ?|) (equal (char-after (point)) ?\s))
	(forward-char 1)))
    (when (<= (point) pos) (org-table-end-of-field 2))))

;;;###autoload
(defun org-table-next-row ()
  "Go to the next row (same column) in the current table.
When next row is an hline or outside the table, create a new empty
row.  Before doing so, re-align the table if necessary."
  (interactive)
  (org-table-maybe-eval-formula)
  (org-table-maybe-recalculate-line)
  (if (and org-table-automatic-realign
	   org-table-may-need-update)
      (org-table-align))
  (let ((col (org-table-current-column)))
    (forward-line 1)
    (unless (bolp) (insert "\n"))	;missing newline at eob
    (when (or (not (org-at-table-p))
	      (org-at-table-hline-p))
      (forward-line -1)
      (org-table-insert-row 'below))
    (org-table-goto-column col)
    (skip-chars-backward "^|\n\r")
    (when (looking-at " ") (forward-char))))

(defun org-table-get (line column)
  "Get the field in table line LINE, column COLUMN.
If LINE is larger than the number of data lines in the table, the function
returns nil.  However, if COLUMN is too large, we will simply return an
empty string.
If LINE is nil, use the current line.
If COLUMN is nil, use the current column."
  (setq column (or column (org-table-current-column)))
  (save-excursion
    (and (or (not line) (org-table-goto-line line))
	 (org-trim (org-table-get-field column)))))

(defun org-table-put (line column value &optional align)
  "Put VALUE into line LINE, column COLUMN.
When ALIGN is set, also realign the table."
  (setq column (or column (org-table-current-column)))
  (prog1 (save-excursion
	   (and (or (not line) (org-table-goto-line line))
		(progn (org-table-goto-column column nil 'force) t)
		(org-table-get-field column value)))
    (and align (org-table-align))))

(defun org-table-current-line ()
  "Return the index of the current data line."
  (let ((pos (point)) (end (org-table-end)) (cnt 0))
    (save-excursion
      (goto-char (org-table-begin))
      (while (and (re-search-forward org-table-dataline-regexp end t)
		  (setq cnt (1+ cnt))
                  (< (line-end-position) pos))))
    cnt))

(defun org-table-current-column ()
  "Return current column number."
  (interactive)
  (save-excursion
    (let ((pos (point)))
      (forward-line 0)
      (if (not (search-forward "|" pos t)) 0
	(let ((column 1)
	      (separator (if (org-at-table-hline-p) "[+|]" "|")))
	  (while (re-search-forward separator pos t) (cl-incf column))
	  column)))))

(defun org-table-current-dline ()
  "Find out what table data line we are in.
Only data lines count for this."
  (save-excursion
    (let ((c 0)
	  (pos (line-beginning-position)))
      (goto-char (org-table-begin))
      (while (<= (point) pos)
	(when (looking-at org-table-dataline-regexp) (cl-incf c))
	(forward-line))
      c)))

(defun org-table-goto-line (N)
  "Go to the Nth data line in the current table.
Return t when the line exists, nil if it does not exist."
  (goto-char (org-table-begin))
  (let ((end (org-table-end)) (cnt 0))
    (while (and (re-search-forward org-table-dataline-regexp end t)
		(< (setq cnt (1+ cnt)) N)))
    (= cnt N)))

;;;###autoload
(defun org-table-blank-field ()
  "Blank the current table field or active region."
  (interactive)
  (org-table-check-inside-data-field)
  (if (and (called-interactively-p 'any) (org-region-active-p))
      (let (org-table-clip)
	(org-table-cut-region (region-beginning) (region-end)))
    (skip-chars-backward "^|")
    (backward-char 1)
    (if (looking-at "|[^|\n]+")
	(let* ((pos (match-beginning 0))
	       (match (match-string 0))
	       (len (save-match-data (org-string-width match nil 'org-table))))
	  (replace-match (concat "|" (make-string (1- len) ?\ )))
	  (goto-char (+ 2 pos))
	  (substring match 1)))))

(defun org-table-get-field (&optional n replace)
  "Return the value of the field in column N of current row.
N defaults to current column.  If REPLACE is a string, replace
field with this value.  The return value is always the old
value."
  (when n (org-table-goto-column n))
  (skip-chars-backward "^|\n")
  (if (or (bolp) (looking-at-p "[ \t]*$"))
      ;; Before first column or after last one.
      ""
    (looking-at "[^|\r\n]*")
    (let* ((pos (match-beginning 0))
	   (val (buffer-substring pos (match-end 0))))
      (when replace
	(org-table-with-shrunk-field
	 (replace-match (if (equal replace "") " " replace) t t)))
      (goto-char (min (line-end-position) (1+ pos)))
      val)))

;;;###autoload
(defun org-table-field-info (_arg)
  "Show info about the current field, and highlight any reference at point."
  (interactive "P")
  (unless (org-at-table-p) (user-error "Not at a table"))
  (org-table-analyze)
  (save-excursion
    (let* ((pos (point))
	   (col (org-table-current-column))
	   (cname (car (rassoc (number-to-string col) org-table-column-names)))
	   (name (car (rassoc (list (count-lines org-table-current-begin-pos
						 (line-beginning-position))
				    col)
			      org-table-named-field-locations)))
	   (eql (org-table-expand-lhs-ranges
		 (mapcar
		  (lambda (e)
		    (cons (org-table-formula-handle-first/last-rc (car e))
			  (cdr e)))
		  (org-table-get-stored-formulas))))
	   (dline (org-table-current-dline))
	   (ref (format "@%d$%d" dline col))
	   (ref1 (org-table-convert-refs-to-an ref))
	   ;; Prioritize field formulas over column formulas.
	   (fequation (or (assoc name eql) (assoc ref eql)))
	   (cequation (assoc (format "$%d" col) eql))
	   (eqn (or fequation cequation)))
      (let ((p (and eqn (get-text-property 0 :orig-eqn (car eqn)))))
	(when p (setq eqn p)))
      (goto-char pos)
      (ignore-errors (org-table-show-reference 'local))
      (message "line @%d, col $%s%s, ref @%d$%d or %s%s%s"
	       dline col
	       (if cname (concat " or $" cname) "")
	       dline col ref1
	       (if name (concat " or $" name) "")
	       ;; FIXME: formula info not correct if special table line
	       (if eqn
		   (concat ", formula: "
			   (org-table-formula-to-user
			    (concat
			     (if (or (string-prefix-p "$" (car eqn))
				     (string-prefix-p "@" (car eqn)))
				 ""
			       "$")
			     (car eqn) "=" (cdr eqn))))
		 "")))))

(defun org-table-goto-field (ref &optional create-column-p)
  "Move point to a specific field in the current table.

REF is either the name of a field its absolute reference, as
a string.  No column is created unless CREATE-COLUMN-P is
non-nil.  If it is a function, it is called with the column
number as its argument as is used as a predicate to know if the
column can be created.

This function assumes the table is already analyzed (i.e., using
`org-table-analyze')."
  (let* ((coordinates
	  (cond
	   ((cdr (assoc ref org-table-named-field-locations)))
	   ((string-match "\\`@\\([1-9][0-9]*\\)\\$\\([1-9][0-9]*\\)\\'" ref)
	    (list (condition-case nil
		      (aref org-table-dlines
			    (string-to-number (match-string 1 ref)))
		    (error (user-error "Invalid row number in %s" ref)))
		  (string-to-number (match-string 2 ref))))
	   (t (user-error "Unknown field: %s" ref))))
	 (line (car coordinates))
	 (column (nth 1 coordinates))
	 (create-new-column (if (functionp create-column-p)
				(funcall create-column-p column)
			      create-column-p)))
    (when coordinates
      (goto-char org-table-current-begin-pos)
      (forward-line line)
      (org-table-goto-column column nil create-new-column))))

;;;###autoload
(defun org-table-goto-column (n &optional on-delim force)
  "Move the cursor to the Nth column in the current table line.
With optional argument ON-DELIM, stop with point before the left delimiter
of the field.
If there are less than N fields, just go to after the last delimiter.
However, when FORCE is non-nil, create new columns if necessary."
  (interactive "p")
  (forward-line 0)
  (when (> n 0)
    (while (and (> (setq n (1- n)) -1)
                (or (search-forward "|" (line-end-position) t)
		    (and force
			 (progn (end-of-line 1)
				(skip-chars-backward "^|")
				(insert " | ")
				t)))))
    (when (and force (not (looking-at ".*|")))
      (save-excursion (end-of-line 1) (insert " | ")))
    (if on-delim
	(backward-char 1)
      (if (looking-at " ") (forward-char 1)))))

;;;###autoload
(defun org-table-insert-column ()
  "Insert a new column into the table."
  (interactive)
  (unless (org-at-table-p) (user-error "Not at a table"))
  (when (eobp) (save-excursion (insert "\n")))
  (unless (string-match-p "|[ \t]*$" (org-current-line-string))
    (org-table-align))
  (org-table-find-dataline)
  (let ((col (max 1 (org-table-current-column)))
	(beg (org-table-begin))
	(end (copy-marker (org-table-end)))
	(shrunk-columns (org-table--list-shrunk-columns)))
    (org-table-expand beg end)
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
	(unless (org-at-table-hline-p)
	  (org-table-goto-column col t)
	  (insert "|"))
	(forward-line)))
    (org-table-goto-column col)
    (org-table-align)
    ;; Shift appropriately stored shrunk column numbers, then hide the
    ;; columns again.
    (org-table--shrink-columns (mapcar (lambda (c) (if (< c col) c (1+ c)))
				       shrunk-columns)
			       beg end)
    (set-marker end nil)
    ;; Fix TBLFM formulas, if desirable.
    (when (or (not org-table-fix-formulas-confirm)
	      (funcall org-table-fix-formulas-confirm "Fix formulas? "))
      (org-table-fix-formulas "$" nil (1- col) 1))))

(defun org-table-find-dataline ()
  "Find a data line in the current table, which is needed for column commands.
This function assumes point is in a table.  Raise an error when
there is no data row below."
  (or (not (org-at-table-hline-p))
      (let ((col (current-column))
	    (end (org-table-end)))
	(forward-line)
	(while (and (< (point) end) (org-at-table-hline-p))
	  (forward-line))
	(when (>= (point) end)
	  (user-error "Cannot find data row for column operation"))
	(org-move-to-column col)
	t)))

(defun org-table-line-to-dline (line &optional above)
  "Turn a buffer line number into a data line number.

If there is no data line in this line, return nil.

If there is no matching dline (most likely the reference was
a hline), the first dline below it is used.  When ABOVE is
non-nil, the one above is used."
  (let ((min 1)
	(max (1- (length org-table-dlines))))
    (cond ((or (> (aref org-table-dlines min) line)
	       (< (aref org-table-dlines max) line))
	   nil)
	  ((= line (aref org-table-dlines max)) max)
	  (t (catch 'exit
	       (while (> (- max min) 1)
		 (let* ((mean (/ (+ max min) 2))
			(v (aref org-table-dlines mean)))
		   (cond ((= v line) (throw 'exit mean))
			 ((> v line) (setq max mean))
			 (t (setq min mean)))))
	       (cond ((= line (aref org-table-dlines max)) max)
		     ((= line (aref org-table-dlines min)) min)
		     (above min)
		     (t max)))))))

(defun org-table--swap-cells (row1 col1 row2 col2)
  "Swap two cells indicated by the coordinates provided.
ROW1, COL1, ROW2, COL2 are integers indicating the row/column
position of the two cells that will be swapped in the table."
  (let ((content1 (org-table-get row1 col1))
	(content2 (org-table-get row2 col2)))
    (org-table-put row1 col1 content2)
    (org-table-put row2 col2 content1)))

(defun org-table--move-cell (direction)
  "Move the current cell in a cardinal direction.
DIRECTION is a symbol among `up', `down', `left', and `right'.
The contents the current cell are swapped with cell in the
indicated direction.  Raise an error if the move cannot be done."
  (let ((row-shift (pcase direction (`up -1) (`down 1) (_ 0)))
	(column-shift (pcase direction (`left -1) (`right 1) (_ 0))))
    (when (and (= 0 row-shift) (= 0 column-shift))
      (error "Invalid direction: %S" direction))
    ;; Initialize `org-table-current-ncol' and `org-table-dlines'.
    (org-table-analyze)
    (let* ((row (org-table-current-line))
	   (column (org-table-current-column))
	   (target-row (+ row row-shift))
	   (target-column (+ column column-shift))
	   (org-table-current-nrow (1- (length org-table-dlines))))
      (when (or (< target-column 1)
		(< target-row 1)
		(> target-column org-table-current-ncol)
		(> target-row org-table-current-nrow))
	(user-error "Cannot move cell further"))
      (org-table--swap-cells row column target-row target-column)
      (org-table-goto-line target-row)
      (org-table-goto-column target-column))))

;;;###autoload
(defun org-table-move-cell-up ()
  "Move a single cell up in a table.
Swap with anything in target cell."
  (interactive)
  (unless (org-table-check-inside-data-field)
    (error "No table at point"))
  (org-table--move-cell 'up)
  (org-table-align))

;;;###autoload
(defun org-table-move-cell-down ()
  "Move a single cell down in a table.
Swap with anything in target cell."
  (interactive)
  (unless (org-table-check-inside-data-field)
    (error "No table at point"))
  (org-table--move-cell 'down)
  (org-table-align))

;;;###autoload
(defun org-table-move-cell-left ()
  "Move a single cell left in a table.
Swap with anything in target cell."
  (interactive)
  (unless (org-table-check-inside-data-field)
    (error "No table at point"))
  (org-table--move-cell 'left)
  (org-table-align))

;;;###autoload
(defun org-table-move-cell-right ()
  "Move a single cell right in a table.
Swap with anything in target cell."
  (interactive)
  (unless (org-table-check-inside-data-field)
    (error "No table at point"))
  (org-table--move-cell 'right)
  (org-table-align))

;;;###autoload
(defun org-table-delete-column ()
  "Delete a column from the table."
  (interactive)
  (unless (org-at-table-p) (user-error "Not at a table"))
  (org-table-find-dataline)
  (when (save-excursion (skip-chars-forward " \t") (eolp))
    (search-backward "|"))		;snap into last column
  (org-table-check-inside-data-field nil t)
  (let* ((col (org-table-current-column))
	 (beg (org-table-begin))
	 (end (copy-marker (org-table-end)))
	 (shrunk-columns (remq col (org-table--list-shrunk-columns))))
    (org-table-expand beg end)
    (org-table-save-field
     (goto-char beg)
     (while (< (point) end)
       (if (org-at-table-hline-p)
	   nil
	 (org-table-goto-column col t)
	 (and (looking-at "|[^|\n]+|")
	      (replace-match "|")))
       (forward-line)))
    (org-table-align)
    ;; Shift appropriately stored shrunk column numbers, then hide the
    ;; columns again.
    (org-table--shrink-columns (mapcar (lambda (c) (if (< c col) c (1- c)))
				       shrunk-columns)
			       beg end)
    (set-marker end nil)
    ;; Fix TBLFM formulas, if desirable.
    (when (or (not org-table-fix-formulas-confirm)
	      (funcall org-table-fix-formulas-confirm "Fix formulas? "))
      (org-table-fix-formulas
       "$" (list (cons (number-to-string col) "INVALID")) col -1 col))))

;;;###autoload
(defun org-table-move-column-right ()
  "Move column to the right."
  (interactive)
  (org-table-move-column nil))

;;;###autoload
(defun org-table-move-column-left ()
  "Move column to the left."
  (interactive)
  (org-table-move-column 'left))

;;;###autoload
(defun org-table-move-column (&optional left)
  "Move the current column to the right.  With arg LEFT, move to the left."
  (interactive "P")
  (unless (org-at-table-p) (user-error "Not at a table"))
  (org-table-find-dataline)
  (org-table-check-inside-data-field nil t)
  (let* ((col (org-table-current-column))
	 (col1 (if left (1- col) col))
	 (colpos (if left (1- col) (1+ col)))
	 (beg (org-table-begin))
	 (end (copy-marker (org-table-end))))
    (when (and left (= col 1))
      (user-error "Cannot move column further left"))
    (when (and (not left) (looking-at "[^|\n]*|[^|\n]*$"))
      (user-error "Cannot move column further right"))
    (let ((shrunk-columns (org-table--list-shrunk-columns)))
      (org-table-expand beg end)
      (org-table-save-field
       (goto-char beg)
       (while (< (point) end)
	 (unless (org-at-table-hline-p)
	   (org-table-goto-column col1 t)
	   (when (looking-at "|\\([^|\n]+\\)|\\([^|\n]+\\)|")
	     (transpose-regions
	      (match-beginning 1) (match-end 1)
	      (match-beginning 2) (match-end 2))))
	 (forward-line)))
      (org-table-goto-column colpos)
      (org-table-align)
      ;; Shift appropriately stored shrunk column numbers, then shrink
      ;; the columns again.
      (org-table--shrink-columns
       (mapcar (lambda (c)
		 (cond ((and (= col c) left) (1- c))
		       ((= col c) (1+ c))
		       ((and (= col (1+ c)) left) (1+ c))
		       ((and (= col (1- c)) (not left) (1- c)))
		       (t c)))
	       shrunk-columns)
       beg end)
      (set-marker end nil)
      ;; Fix TBLFM formulas, if desirable.
      (when (or (not org-table-fix-formulas-confirm)
		(funcall org-table-fix-formulas-confirm "Fix formulas? "))
	(org-table-fix-formulas
	 "$" (list (cons (number-to-string col) (number-to-string colpos))
		   (cons (number-to-string colpos) (number-to-string col))))))))

;;;###autoload
(defun org-table-move-row-down ()
  "Move table row down."
  (interactive)
  (org-table-move-row nil))

;;;###autoload
(defun org-table-move-row-up ()
  "Move table row up."
  (interactive)
  (org-table-move-row 'up))

;;;###autoload
(defun org-table-move-row (&optional up)
  "Move the current table line down.  With arg UP, move it up."
  (interactive "P")
  (let* ((col (current-column))
	 (pos (point))
	 (hline1p (save-excursion (forward-line 0)
				  (looking-at org-table-hline-regexp)))
	 (dline1 (org-table-current-dline))
	 (dline2 (+ dline1 (if up -1 1)))
	 (tonew (if up -1 1))
	 hline2p)
    (when (and up (= (point-min) (line-beginning-position)))
      (user-error "Cannot move row further"))
    (forward-line tonew)
    (when (or (and (not up) (eobp)) (not (org-at-table-p)))
      (goto-char pos)
      (user-error "Cannot move row further"))
    (org-table-with-shrunk-columns
     (setq hline2p (looking-at org-table-hline-regexp))
     (goto-char pos)
     (let ((row (delete-and-extract-region (line-beginning-position)
					   (line-beginning-position 2))))
       (forward-line tonew)
       (unless (bolp) (insert "\n"))	;at eob without a newline
       (insert row)
       (unless (bolp) (insert "\n"))	;missing final newline in ROW
       (forward-line -1)
       (org-move-to-column col)
       (unless (or hline1p hline2p
		   (not (or (not org-table-fix-formulas-confirm)
			  (funcall org-table-fix-formulas-confirm
				   "Fix formulas? "))))
	 (org-table-fix-formulas
	  "@" (list
	       (cons (number-to-string dline1) (number-to-string dline2))
	       (cons (number-to-string dline2) (number-to-string dline1)))))))))

;;;###autoload
(defun org-table-insert-row (&optional arg)
  "Insert a new row above the current line into the table.
With prefix ARG, insert below the current line."
  (interactive "P")
  (unless (org-at-table-p) (user-error "Not at a table"))
  (when (eobp) (save-excursion (insert "\n")))
  (unless (string-match-p "|[ \t]*$" (org-current-line-string))
    (org-table-align))
  (org-table-with-shrunk-columns
   (let* ((line (buffer-substring (line-beginning-position) (line-end-position)))
	  (new (org-table-clean-line line)))
     ;; Fix the first field if necessary
     (when (string-match "^[ \t]*| *[#*$] *|" line)
       (setq new (replace-match (match-string 0 line) t t new)))
     (forward-line (if arg 1 0))
     ;; Buffer may not end of a newline character, so ensure
     ;; (forward-line 1) moves point to a new line.
     (unless (bolp) (insert "\n"))
     (let (org-table-may-need-update) (insert-before-markers new "\n"))
     (forward-line -1)
     (re-search-forward "| ?" (line-end-position) t)
     (when (or org-table-may-need-update org-table-overlay-coordinates)
       (org-table-align))
     (when (or (not org-table-fix-formulas-confirm)
	       (funcall org-table-fix-formulas-confirm "Fix formulas? "))
       (org-table-fix-formulas "@" nil (1- (org-table-current-dline)) 1)))))

;;;###autoload
(defun org-table-insert-hline (&optional above)
  "Insert a horizontal-line below the current line into the table.
With prefix ABOVE, insert above the current line."
  (interactive "P")
  (unless (org-at-table-p) (user-error "Not at a table"))
  (when (eobp) (save-excursion (insert "\n")))
  (unless (string-match-p "|[ \t]*$" (org-current-line-string))
    (org-table-align))
  (org-table-with-shrunk-columns
   (let ((line (org-table-clean-line
                (buffer-substring (line-beginning-position) (line-end-position))))
	 (col (current-column)))
     (while (string-match "|\\( +\\)|" line)
       (setq line (replace-match
		   (concat "+" (make-string (- (match-end 1) (match-beginning 1))
					    ?-) "|") t t line)))
     (and (string-match "\\+" line) (setq line (replace-match "|" t t line)))
     (forward-line (if above 0 1))
     (insert line "\n")
     (forward-line (if above 0 -2))
     (org-move-to-column col)
     (when org-table-overlay-coordinates (org-table-align)))))

;;;###autoload
(defun org-table-hline-and-move (&optional same-column)
  "Insert a hline and move to the row below that line."
  (interactive "P")
  (let ((col (org-table-current-column)))
    (org-table-maybe-eval-formula)
    (org-table-maybe-recalculate-line)
    (org-table-insert-hline)
    (end-of-line 2)
    (if (looking-at "\n[ \t]*|-")
	(progn (insert "\n|") (org-table-align))
      (org-table-next-field))
    (if same-column (org-table-goto-column col))))

(defun org-table-clean-line (s)
  "Convert a table line S into a string with only \"|\" and space.
In particular, this does handle wide and invisible characters."
  (if (string-match "^[ \t]*|-" s)
      ;; It's a hline, just map the characters
      (setq s (mapconcat (lambda (x) (if (member x '(?| ?+)) "|" " ")) s ""))
    (while (string-match "|\\([ \t]*?[^ \t\r\n|][^\r\n|]*\\)|" s)
      (setq s (replace-match
	       (concat "|"
                       (make-string
                        (save-match-data
                          (org-string-width (match-string 1 s) nil 'org-table))
			?\ )
                       "|")
	       t t s)))
    s))

;;;###autoload
(defun org-table-kill-row ()
  "Delete the current row or horizontal line from the table."
  (interactive)
  (unless (org-at-table-p) (user-error "Not at a table"))
  (let ((col (current-column))
	(dline (and (not (org-match-line org-table-hline-regexp))
		    (org-table-current-dline))))
    (org-table-with-shrunk-columns
     (kill-region (line-beginning-position)
                  (min (1+ (line-end-position)) (point-max)))
     (if (not (org-at-table-p)) (forward-line -1))
     (org-move-to-column col)
     (when (and dline
		(or (not org-table-fix-formulas-confirm)
		    (funcall org-table-fix-formulas-confirm "Fix formulas? ")))
       (org-table-fix-formulas
	"@" (list (cons (number-to-string dline) "INVALID")) dline -1 dline)))))

;;;###autoload
(defun org-table-cut-region (beg end)
  "Copy region in table to the clipboard and blank all relevant fields.
If there is no active region, use just the field at point."
  (interactive (list
		(if (org-region-active-p) (region-beginning) (point))
		(if (org-region-active-p) (region-end) (point))))
  (org-table-copy-region beg end 'cut))

(defun org-table--increment-field (field previous)
  "Increment string FIELD according to PREVIOUS field.

Increment FIELD only if it is a string representing a number, per
Emacs Lisp syntax, a timestamp, or is either prefixed or suffixed
with a number.  In any other case, return FIELD as-is.

If PREVIOUS has the same structure as FIELD, e.g.,
a number-prefixed string with the same pattern, the increment
step is the difference between numbers (or timestamps, measured
in days) in PREVIOUS and FIELD.  Otherwise, it uses
`org-table-copy-increment', if the variable contains a number, or
default to 1.

The function assumes `org-table-copy-increment' is non-nil."
  (let* ((default-step (if (numberp org-table-copy-increment)
			   org-table-copy-increment
			 1))
	 (number-regexp			;Lisp read syntax for numbers
	  (rx (and string-start
		   (opt (any "+-"))
		   (or (and (one-or-more digit) (opt "."))
		       (and (zero-or-more digit) "." (one-or-more digit)))
		   (opt (any "eE") (opt (opt (any "+-")) (one-or-more digit)))
		   string-end)))
	 (number-prefix-regexp (rx (and string-start (one-or-more digit))))
	 (number-suffix-regexp (rx (and (one-or-more digit) string-end)))
	 (analyze
	  (lambda (field)
	    ;; Analyze string FIELD and return information related to
	    ;; increment or nil.  When non-nil, return value has the
	    ;; following scheme: (TYPE VALUE PATTERN) where
	    ;; - TYPE is a symbol among `number', `prefix', `suffix'
	    ;;   and `timestamp',
	    ;; - VALUE is a timestamp if TYPE is `timestamp', or
	    ;;   a number otherwise,
	    ;; - PATTERN is the field without its prefix, or suffix if
	    ;;   TYPE is either `prefix' or `suffix' , or nil
	    ;;   otherwise.
	    (cond ((not (org-string-nw-p field)) nil)
		  ((string-match-p number-regexp field)
		   (list 'number
			 (string-to-number field)
			 nil))
		  ((string-match number-prefix-regexp field)
		   (list 'prefix
			 (string-to-number (match-string 0 field))
			 (substring field (match-end 0))))
		  ((string-match number-suffix-regexp field)
		   (list 'suffix
			 (string-to-number (match-string 0 field))
			 (substring field 0 (match-beginning 0))))
		  ((string-match-p org-ts-regexp3 field)
		   (list 'timestamp field nil))
		  (t nil))))
	 (next-number-string
	  (lambda (n1 &optional n2)
	    ;; Increment number N1 and return it as a string.  If N2
	    ;; is also a number, deduce increment step from the
	    ;; difference between N1 and N2.  Otherwise, increment
	    ;; step is `default-step'.
	    (number-to-string (if n2 (+ n1 (- n1 n2)) (+ n1 default-step)))))
	 (shift-timestamp
	  (lambda (t1 &optional t2)
	    ;; Increment timestamp T1 and return it.  If T2 is also
	    ;; a timestamp, deduce increment step from the difference,
	    ;; in days, between T1 and T2.  Otherwise, increment by
	    ;; `default-step' days.
	    (with-temp-buffer
	      (insert t1)
	      (org-timestamp-up-day (if (not t2) default-step
				      (- (org-time-string-to-absolute t1)
					 (org-time-string-to-absolute t2))))
	      (buffer-string)))))
    ;; Check if both PREVIOUS and FIELD have the same type.  Also, if
    ;; the case of prefixed or suffixed numbers, make sure their
    ;; pattern, i.e., the part of the string without the prefix or the
    ;; suffix, is the same.
    (pcase (cons (funcall analyze field) (funcall analyze previous))
      (`((number ,n1 ,_) . (number ,n2 ,_))
       (funcall next-number-string n1 n2))
      (`((number ,n ,_) . ,_)
       (funcall next-number-string n))
      (`((prefix ,n1 ,p1) . (prefix ,n2 ,p2))
       (concat (funcall next-number-string n1 (and (equal p1 p2) n2)) p1))
      (`((prefix ,n ,p) . ,_)
       (concat (funcall next-number-string n) p))
      (`((suffix ,n1 ,p1) . (suffix ,n2 ,p2))
       (concat p1 (funcall next-number-string n1 (and (equal p1 p2) n2))))
      (`((suffix ,n ,p) . ,_)
       (concat p (funcall next-number-string n)))
      (`((timestamp ,t1 ,_) . (timestamp ,t2 ,_))
       (funcall shift-timestamp t1 t2))
      (`((timestamp ,t1 ,_) . ,_)
       (funcall shift-timestamp t1))
      (_ field))))

;;;###autoload
(defun org-table-copy-down (n)
  "Copy the value of the current field one row below.

If the field at the cursor is empty, copy the content of the
nearest non-empty field above.  With argument N, use the Nth
non-empty field.

If the current field is not empty, it is copied down to the next
row, and the cursor is moved with it.  Therefore, repeating this
command causes the column to be filled row-by-row.

If the variable `org-table-copy-increment' is non-nil and the
field is a number, a timestamp, or is either prefixed or suffixed
with a number, it will be incremented while copying.  By default,
increment by the difference between the value in the current
field and the one in the field above, if any.  To increment using
a fixed integer, set `org-table-copy-increment' to a number.  In
the case of a timestamp, increment by days.

However, when N is 0, do not increment the field at all."
  (interactive "p")
  (org-table-check-inside-data-field)
  (let* ((beg (org-table-begin))
	 (column (org-table-current-column))
	 (initial-field (save-excursion
			  (let ((f (org-string-nw-p (org-table-get-field))))
			    (and f (org-trim f)))))
	 field field-above next-field)
    (save-excursion
      ;; Get reference field.
      (if initial-field (setq field initial-field)
	(forward-line 0)
	(setq field
	      (catch :exit
		(while (re-search-backward org-table-dataline-regexp beg t)
		  (let ((f (org-string-nw-p (org-table-get-field column))))
		    (cond ((and (> n 1) f) (cl-decf n))
			  (f (throw :exit (org-trim f)))
			  (t nil))
		    (forward-line 0)))
		(user-error "No non-empty field found"))))
      ;; Check if increment is appropriate, and how it should be done.
      (when (and org-table-copy-increment (/= n 0))
	;; If increment step is not explicit, get non-empty field just
	;; above the field being incremented to guess it.
	(unless (numberp org-table-copy-increment)
	  (setq field-above
		(let ((f (unless (= beg (line-beginning-position))
			   (forward-line -1)
			   (not (org-at-table-hline-p))
			   (org-table-get-field column))))
		  (and (org-string-nw-p f)
		       (org-trim f)))))
	;; Compute next field.
	(setq next-field (org-table--increment-field field field-above))))
    ;; Since initial field in not empty, we modify row below instead.
    ;; Skip alignment since we do it at the end of the process anyway.
    (when initial-field
      (let ((org-table-may-need-update nil)) (org-table-next-row))
      (org-table-blank-field))
    ;; Insert the new field.  NEW-FIELD may be nil if
    ;; `org-table-copy-increment' is nil, or N = 0.  In that case,
    ;; copy FIELD.
    (insert (or next-field field))
    (org-table-maybe-recalculate-line)
    (org-table-align)))

;;;###autoload
(defun org-table-copy-region (beg end &optional cut)
  "Copy rectangular region in table to clipboard.
A special clipboard is used which can only be accessed with
`org-table-paste-rectangle'.  Return the region copied, as a list
of lists of fields."
  (interactive (list
		(if (org-region-active-p) (region-beginning) (point))
		(if (org-region-active-p) (region-end) (point))
		current-prefix-arg))
  (goto-char (min beg end))
  (org-table-check-inside-data-field)
  (let ((beg (line-beginning-position))
	(c01 (org-table-current-column))
	region)
    (goto-char (max beg end))
    (org-table-check-inside-data-field nil t)
    (let* ((end (copy-marker (line-end-position)))
	   (c02 (org-table-current-column))
	   (column-start (min c01 c02))
	   (column-end (max c01 c02))
	   (column-number (1+ (- column-end column-start)))
	   (rpl (and cut "  ")))
      (goto-char beg)
      (while (< (point) end)
	(unless (org-at-table-hline-p)
	  ;; Collect every cell between COLUMN-START and COLUMN-END.
	  (let (cols)
	    (dotimes (c column-number)
	      (push (org-table-get-field (+ c column-start) rpl) cols))
	    (push (nreverse cols) region)))
	(forward-line))
      (set-marker end nil))
    (when cut (org-table-align))
    (when (called-interactively-p 'any)
      (message (substitute-command-keys "Cells in the region copied, use \
\\[org-table-paste-rectangle] to paste them in a table.")))
    (setq org-table-clip (nreverse region))))

;;;###autoload
(defun org-table-paste-rectangle ()
  "Paste a rectangular region into a table.
The upper right corner ends up in the current field.  All involved fields
will be overwritten.  If the rectangle does not fit into the present table,
the table is enlarged as needed.  The process ignores horizontal separator
lines."
  (interactive)
  (unless (consp org-table-clip)
    (user-error "First cut/copy a region to paste!"))
  (org-table-check-inside-data-field)
  (let* ((column (org-table-current-column))
	 (org-table-automatic-realign nil))
    (org-table-save-field
     (dolist (row org-table-clip)
       (while (org-at-table-hline-p) (forward-line))
       ;; If we left the table, create a new row.
       (when (and (bolp) (not (looking-at "[ \t]*|")))
	 (end-of-line 0)
	 (org-table-next-field))
       (let ((c column))
	 (dolist (field row)
	   (org-table-goto-column c nil 'force)
	   (org-table-get-field nil field)
	   (cl-incf c)))
       (forward-line)))
    (org-table-align)))


;;; Follow Field minor mode

(define-minor-mode org-table-follow-field-mode
  "Minor mode to make the table field editor window follow the cursor.
When this mode is active, the field editor window will always show the
current field.  The mode exits automatically when the cursor leaves the
table (but see `org-table-exit-follow-field-mode-when-leaving-table')."
  :lighter " TblFollow"
  (if org-table-follow-field-mode
      (add-hook 'post-command-hook 'org-table-follow-fields-with-editor
		'append 'local)
    (remove-hook 'post-command-hook 'org-table-follow-fields-with-editor 'local)
    (let* ((buf (get-buffer "*Org Table Edit Field*"))
	   (win (and buf (get-buffer-window buf))))
      (when win (delete-window win))
      (when buf
	(with-current-buffer buf
	  (move-marker org-field-marker nil))
	(kill-buffer buf)))))

;;;###autoload
(defun org-table-edit-field (arg)
  "Edit table field in a different window.
This is mainly useful for fields that contain hidden parts.

When called with a `\\[universal-argument]' prefix, just make the full field
visible so that it can be edited in place.

When called with a `\\[universal-argument] \\[universal-argument]' prefix, \
toggle `org-table-follow-field-mode'."
  (interactive "P")
  (unless (org-at-table-p) (user-error "Not at a table"))
  (cond
   ((equal arg '(16))
    (org-table-follow-field-mode (if org-table-follow-field-mode -1 1)))
   (arg
    (let ((b (save-excursion (skip-chars-backward "^|") (point)))
	  (e (save-excursion (skip-chars-forward "^|\r\n") (point))))
      (remove-text-properties b e '(invisible t intangible t))
      (if font-lock-mode
	  (font-lock-fontify-block))))
   (t
    (let ((pos (point-marker))
	  (coord
	   (if (eq org-table-use-standard-references t)
	       (concat (org-number-to-letters (org-table-current-column))
		       (number-to-string (org-table-current-dline)))
	     (concat "@" (number-to-string (org-table-current-dline))
		     "$" (number-to-string (org-table-current-column)))))
	  (field (org-table-get-field))
	  (cw (current-window-configuration))
	  p)
      (goto-char pos)
      (switch-to-buffer-other-window "*Org Table Edit Field*")
      (when (and (local-variable-p 'org-field-marker)
		 (markerp org-field-marker))
	(move-marker org-field-marker nil))
      (erase-buffer)
      (insert "#\n# Edit field " coord " and finish with C-c C-c\n#\n")
      (let ((org-inhibit-startup t)) (org-mode))
      (auto-fill-mode -1)
      (setq truncate-lines nil)
      (setq word-wrap t)
      (goto-char (setq p (point-max)))
      (insert (org-trim field))
      (remove-text-properties p (point-max) '(invisible t intangible t))
      (goto-char p)
      (setq-local org-finish-function 'org-table-finish-edit-field)
      (setq-local org-window-configuration cw)
      (setq-local org-field-marker pos)
      (message "Edit and finish with C-c C-c")))))

(defun org-table-follow-fields-with-editor ()
  (if (and org-table-exit-follow-field-mode-when-leaving-table
	   (not (org-at-table-p)))
      ;; We have left the table, exit the follow mode
      (org-table-follow-field-mode -1)
    (when (org-table-check-inside-data-field 'noerror)
      (let ((win (selected-window)))
	(org-table-edit-field nil)
	(org-fit-window-to-buffer)
	(select-window win)))))

(defun org-table-finish-edit-field ()
  "Finish editing a table data field.
Remove all newline characters, insert the result into the table, realign
the table and kill the editing buffer."
  (let ((pos org-field-marker)
	(cw org-window-configuration)
	(cb (current-buffer))
	text)
    (goto-char (point-min))
    (while (re-search-forward "^#.*\n?" nil t) (replace-match ""))
    (while (re-search-forward "[ \t]*\n[ \t\n]*" nil t)
      (replace-match " "))
    (setq text (org-trim (buffer-string)))
    (set-window-configuration cw)
    (kill-buffer cb)
    (select-window (get-buffer-window (marker-buffer pos)))
    (goto-char pos)
    (move-marker pos nil)
    (org-table-check-inside-data-field)
    (org-table-get-field nil text)
    (org-table-align)
    (message "New field value inserted")))


;;; Formulas

(defun org-table-current-field-formula (&optional key noerror)
  "Return the formula active for the current field.

Assumes that table is already analyzed.  If KEY is given, return
the key to this formula.  Otherwise return the formula preceded
with \"=\" or \":=\"."
  (let* ((line (count-lines org-table-current-begin-pos
			    (line-beginning-position)))
	 (row (org-table-line-to-dline line)))
    (cond
     (row
      (let* ((col (org-table-current-column))
	     (name (car (rassoc (list line col)
				org-table-named-field-locations)))
	     (scol (format "$%d" col))
	     (ref (format "@%d$%d" (org-table-current-dline) col))
	     (stored-list (org-table-get-stored-formulas noerror))
	     (ass (or (assoc name stored-list)
		      (assoc ref stored-list)
		      (assoc scol stored-list))))
	(cond (key (car ass))
	      (ass (concat (if (string-match-p "^[0-9]+$" (car ass)) "=" ":=")
			   (cdr ass))))))
     (noerror nil)
     (t (error "No formula active for the current field")))))

(defun org-table-get-formula (&optional equation named)
  "Read a formula from the minibuffer, offer stored formula as default.
When NAMED is non-nil, look for a named equation."
  (let* ((stored-list (org-table-get-stored-formulas))
	 (name (car (rassoc (list (count-lines org-table-current-begin-pos
					       (line-beginning-position))
				  (org-table-current-column))
			    org-table-named-field-locations)))
	 (ref (format "@%d$%d"
		      (org-table-current-dline)
		      (org-table-current-column)))
	 (scol (cond
		((not named) (format "$%d" (org-table-current-column)))
		(name)
		(t ref)))
	 (name (or name ref))
	 (org-table-may-need-update nil)
	 (stored (cdr (assoc scol stored-list)))
	 (eq (cond
	      ((and stored equation (string-match-p "^ *=? *$" equation))
	       stored)
	      ((stringp equation) equation)
	      (t
	       (org-table-formula-from-user
		(read-string
		 (org-table-formula-to-user
		  (format "%s formula %s=" (if named "Field" "Column") scol))
		 (if stored (org-table-formula-to-user stored) "")
		 'org-table-formula-history)))))
	 mustsave)
    (unless (org-string-nw-p eq)
      ;; Remove formula.
      (setq stored-list (delq (assoc scol stored-list) stored-list))
      (org-table-store-formulas stored-list)
      (user-error "Formula removed"))
    (when (string-match "^ *=?" eq) (setq eq (replace-match "" t t eq)))
    (when (string-match " *$" eq) (setq eq (replace-match "" t t eq)))
    (when (and name (not named))
      ;; We set the column equation, delete the named one.
      (setq stored-list (delq (assoc name stored-list) stored-list)
	    mustsave t))
    (if stored
	(setcdr (assoc scol stored-list) eq)
      (setq stored-list (cons (cons scol eq) stored-list)))
    (when (or mustsave (not (equal stored eq)))
      (org-table-store-formulas stored-list))
    eq))

(defun org-table-store-formulas (alist &optional location)
  "Store the list of formulas below the current table.
If optional argument LOCATION is a buffer position, insert it at
LOCATION instead."
  (save-excursion
    (if location
	(progn (goto-char location) (forward-line 0))
      (goto-char (org-table-end)))
    (let ((case-fold-search t))
      (if (looking-at "\\([ \t]*\n\\)*[ \t]*\\(#\\+TBLFM:\\)\\(.*\n?\\)")
	  (progn
	    ;; Don't overwrite TBLFM, we might use text properties to
	    ;; store stuff.
	    (goto-char (match-beginning 3))
	    (delete-region (match-beginning 3) (match-end 0)))
	(org-indent-line)
	(insert "#+TBLFM:"))
      (insert " "
	      (mapconcat (lambda (x) (concat (car x) "=" (cdr x)))
			 (sort alist #'org-table-formula-less-p)
			 "::")
	      "\n"))))

(defsubst org-table-formula-make-cmp-string (a)
  (when (string-match "\\`\\$[<>]" a)
    (let ((arrow (string-to-char (substring a 1))))
      ;; Fake a high number to make sure this is sorted at the end.
      (setq a (org-table-formula-handle-first/last-rc a))
      (setq a (format "$%d" (+ 10000
			       (if (= arrow ?<) -1000 0)
			       (string-to-number (substring a 1)))))))
  (when (string-match
	 "^\\(@\\([0-9]+\\)\\)?\\(\\$?\\([0-9]+\\)\\)?\\(\\$?[a-zA-Z0-9]+\\)?"
	 a)
    (concat
     (if (match-end 2)
	 (format "@%05d" (string-to-number (match-string 2 a))) "")
     (if (match-end 4)
	 (format "$%05d" (string-to-number (match-string 4 a))) "")
     (if (match-end 5)
	 (concat "@@" (match-string 5 a))))))

(defun org-table-formula-less-p (a b)
  "Compare two formulas for sorting."
  (let ((as (org-table-formula-make-cmp-string (car a)))
	(bs (org-table-formula-make-cmp-string (car b))))
    (and as bs (string< as bs))))

;;;###autoload
(defun org-table-get-stored-formulas (&optional noerror location)
  "Return an alist with the stored formulas directly after current table.
By default, only return active formulas, i.e., formulas located
on the first line after the table.  However, if optional argument
LOCATION is a buffer position, consider the formulas there."
  (save-excursion
    (if location
	(progn (goto-char location) (forward-line 0))
      (goto-char (org-table-end)))
    (let ((case-fold-search t))
      (when (looking-at "\\([ \t]*\n\\)*[ \t]*#\\+TBLFM: *\\(.*\\)")
	(let ((strings (org-split-string (match-string-no-properties 2)
					 " *:: *"))
	      eq-alist seen)
	  (dolist (string strings (nreverse eq-alist))
	    (when (string-match "\\`\\(@[-+I<>0-9.$@]+\\|\\$\\([_a-zA-Z0-9]+\\|\
\[<>]+\\)\\) *= *\\(.*[^ \t]\\)"
				string)
	      (let ((lhs
		     (let ((m (match-string 1 string)))
		       (cond
			((not (match-end 2)) m)
			;; Is it a column reference?
			((string-match-p "\\`\\$\\([0-9]+\\|[<>]+\\)\\'" m) m)
			;; Since named columns are not possible in
			;; LHS, assume this is a named field.
			(t (match-string 2 string)))))
		    (rhs (match-string 3 string)))
		(push (cons lhs rhs) eq-alist)
		(cond
		 ((not (member lhs seen)) (push lhs seen))
		 (noerror
		  (message
		   "Double definition `%s=' in TBLFM line, please fix by hand"
		   lhs)
		  (ding)
		  (sit-for 2))
		 (t
		  (user-error
		   "Double definition `%s=' in TBLFM line, please fix by hand"
		   lhs)))))))))))

(defun org-table-fix-formulas (key replace &optional limit delta remove)
  "Modify the equations after the table structure has been edited.
KEY is \"@\" or \"$\".  REPLACE is an alist of numbers to replace.
For all numbers larger than LIMIT, shift them by DELTA."
  (save-excursion
    (goto-char (org-table-end))
    (while (let ((case-fold-search t)) (looking-at "[ \t]*#\\+tblfm:"))
      (let ((re (concat key "\\([0-9]+\\)"))
	    (re2
	     (when remove
	       (if (equal key "$")
		   (format "\\(@[0-9]+\\)?%s%d=.*?\\(::\\|$\\)"
			   (regexp-quote key) remove)
		 (format "@%d\\$[0-9]+=.*?\\(::\\|$\\)" remove))))
	    s n a)
	(when remove
          (save-excursion
            (while (re-search-forward re2 (line-end-position) t)
	      (unless (save-match-data (org-in-regexp "remote([^)]+?)"))
	        (if (equal (char-before (match-beginning 0)) ?.)
		    (user-error
		     "Change makes TBLFM term %s invalid, use undo to recover"
		     (match-string 0))
		  (replace-match ""))))))
        (while (re-search-forward re (line-end-position) t)
	  (unless (save-match-data (org-in-regexp "remote([^)]+?)"))
	    (setq s (match-string 1) n (string-to-number s))
	    (cond
	     ((setq a (assoc s replace))
	      (replace-match (concat key (cdr a)) t t))
	     ((and limit (> n limit))
	      (replace-match (concat key (number-to-string (+ n delta))) t t)))))
	(message "The formulas in #+TBLFM have been updated"))
      (forward-line))))

;;;###autoload
(defun org-table-maybe-eval-formula ()
  "Check if the current field starts with \"=\" or \":=\".
If yes, store the formula and apply it."
  ;; We already know we are in a table.  Get field will only return a formula
  ;; when appropriate.  It might return a separator line, but no problem.
  (when org-table-formula-evaluate-inline
    (let* ((field (org-trim (or (org-table-get-field) "")))
	   named eq)
      (when (string-match "^:?=\\(.*[^=]\\)$" field)
	(setq named (equal (string-to-char field) ?:)
	      eq (match-string 1 field))
	(org-table-eval-formula (and named '(4))
				(org-table-formula-from-user eq))))))

;;;###autoload
(defun org-table-rotate-recalc-marks (&optional newchar)
  "Rotate the recalculation mark in the first column.
If in any row, the first field is not consistent with a mark,
insert a new column for the markers.
When there is an active region, change all the lines in the region,
after prompting for the marking character.
After each change, a message will be displayed indicating the meaning
of the new mark."
  (interactive)
  (unless (org-at-table-p) (user-error "Not at a table"))
  (let* ((region (org-region-active-p))
	 (l1 (and region
		  (save-excursion (goto-char (region-beginning))
				  (copy-marker (line-beginning-position)))))
	 (l2 (and region
		  (save-excursion (goto-char (region-end))
				  (copy-marker (line-beginning-position)))))
	 (l (copy-marker (line-beginning-position)))
	 (col (org-table-current-column))
	 (newchar (if region
		      (char-to-string
		       (read-char-exclusive
			"Change region to what mark?  Type # * ! $ or SPC: "))
		    newchar))
	 (no-special-column
	  (save-excursion
	    (goto-char (org-table-begin))
	    (re-search-forward
	     "^[ \t]*|[^-|][^|]*[^#!$*_^| \t][^|]*|" (org-table-end) t))))
    (when (and newchar (not (assoc newchar org-recalc-marks)))
      (user-error "Invalid character `%s' in `org-table-rotate-recalc-marks'"
		  newchar))
    (when l1 (goto-char l1))
    (save-excursion
      (forward-line 0)
      (unless (looking-at org-table-dataline-regexp)
	(user-error "Not at a table data line")))
    (when no-special-column
      (org-table-goto-column 1)
      (org-table-insert-column))
    (let ((previous-line-end (line-end-position))
	  (newchar
	   (save-excursion
	     (forward-line 0)
	     (cond ((not (looking-at "^[ \t]*| *\\([#!$*^_ ]\\) *|")) "#")
		   (newchar)
		   (t (cadr (member (match-string 1)
				    (append (mapcar #'car org-recalc-marks)
					    '(" ")))))))))
      ;; Rotate mark in first row.
      (org-table-get-field 1 (format " %s " newchar))
      ;; Rotate marks in additional rows if a region is active.
      (when region
	(save-excursion
	  (forward-line)
	  (while (<= (point) l2)
	    (when (looking-at org-table-dataline-regexp)
	      (org-table-get-field 1 (format " %s " newchar)))
	    (forward-line))))
      ;; Only align if rotation actually changed lines' length.
      (when (/= previous-line-end (line-end-position)) (org-table-align)))
    (goto-char l)
    (org-table-goto-column (if no-special-column (1+ col) col))
    (when l1 (set-marker l1 nil))
    (when l2 (set-marker l2 nil))
    (set-marker l nil)
    (when (called-interactively-p 'interactive)
      (message "%s" (cdr (assoc newchar org-recalc-marks))))))

;;;###autoload
(defun org-table-maybe-recalculate-line ()
  "Recompute the current line if marked for it, and if we haven't just done it."
  (interactive)
  (and org-table-allow-automatic-line-recalculation
       (not (and (memq last-command org-recalc-commands)
	       (eq org-last-recalc-line (line-beginning-position))))
       (save-excursion (forward-line 0)
		       (looking-at org-table-auto-recalculate-regexp))
       (org-table-recalculate) t))

;;;###autoload
(defun org-table-eval-formula (&optional arg equation
					 suppress-align suppress-const
					 suppress-store suppress-analysis)
  "Replace the table field value at the cursor by the result of a calculation.

In a table, this command replaces the value in the current field with the
result of a formula.  It also installs the formula as the \"current\" column
formula, by storing it in a special line below the table.  When called
with a `\\[universal-argument]' prefix the formula is installed as a \
field formula.

When called with a `\\[universal-argument] \\[universal-argument]' prefix, \
insert the active equation for the field
back into the current field, so that it can be edited there.  This is \
useful
in order to use \\<org-table-fedit-map>`\\[org-table-show-reference]' to \
check the referenced fields.

When called, the command first prompts for a formula, which is read in
the minibuffer.  Previously entered formulas are available through the
history list, and the last used formula is offered as a default.
These stored formulas are adapted correctly when moving, inserting, or
deleting columns with the corresponding commands.

The formula can be any algebraic expression understood by the Calc package.
For details, see the Org mode manual.

This function can also be called from Lisp programs and offers
additional arguments: EQUATION can be the formula to apply.  If this
argument is given, the user will not be prompted.

SUPPRESS-ALIGN is used to speed-up recursive calls by by-passing
unnecessary aligns.

SUPPRESS-CONST suppresses the interpretation of constants in the
formula, assuming that this has been done already outside the
function.

SUPPRESS-STORE means the formula should not be stored, either
because it is already stored, or because it is a modified
equation that should not overwrite the stored one.

SUPPRESS-ANALYSIS prevents analyzing the table and checking
location of point."
  (interactive "P")
  (unless suppress-analysis
    (org-table-check-inside-data-field nil t)
    (org-table-analyze))
  (if (equal arg '(16))
      (let ((eq (org-table-current-field-formula)))
	(org-table-get-field nil eq)
	(org-table-align)
	(setq org-table-may-need-update t))
    (let* (fields
	   (ndown (if (integerp arg) arg 1))
	   (org-table-automatic-realign nil)
	   (case-fold-search nil)
	   (down (> ndown 1))
	   (formula (if (and equation suppress-store)
			equation
		      (org-table-get-formula equation (equal arg '(4)))))
	   (n0 (org-table-current-column))
	   (calc-modes (copy-sequence org-calc-default-modes))
	   (numbers nil)	   ; was a variable, now fixed default
	   (keep-empty nil)
	   form form0 formrpl formrg bw fmt ev orig lispp literal
	   duration duration-output-format)
      ;; Parse the format string.  Since we have a lot of modes, this is
      ;; a lot of work.  However, I think calc still uses most of the time.
      (if (string-match "\\(.*\\);\\(.*\\)" formula)
	  (progn
	    (setq fmt (concat (cdr (assoc "%" org-table-local-parameters))
			      (match-string-no-properties 2 formula)))
	    (setq formula (match-string-no-properties 1 formula))
	    (while (string-match "\\([pnfse]\\)\\(-?[0-9]+\\)" fmt)
	      (let ((c (string-to-char (match-string 1 fmt)))
		    (n (string-to-number (match-string 2 fmt))))
		(cl-case c
		  (?p (setf (cl-getf calc-modes 'calc-internal-prec) n))
		  (?n (setf (cl-getf calc-modes 'calc-float-format) (list 'float n)))
		  (?f (setf (cl-getf calc-modes 'calc-float-format) (list 'fix n)))
		  (?s (setf (cl-getf calc-modes 'calc-float-format) (list 'sci n)))
		  (?e (setf (cl-getf calc-modes 'calc-float-format) (list 'eng n)))))
	      ;; Remove matched flags from the mode string.
	      (setq fmt (replace-match "" t t fmt)))
	    (while (string-match "\\([tTUNLEDRFSu]\\)" fmt)
	      (let ((c (string-to-char (match-string 1 fmt))))
		(cl-case c
		  (?t (setq duration t numbers t
                            duration-output-format org-table-duration-custom-format))
		  (?T (setq duration t numbers t duration-output-format nil))
		  (?U (setq duration t numbers t duration-output-format 'hh:mm))
		  (?N (setq numbers t))
		  (?L (setq literal t))
		  (?E (setq keep-empty t))
		  (?D (setf (cl-getf calc-modes 'calc-angle-mode) 'deg))
		  (?R (setf (cl-getf calc-modes 'calc-angle-mode) 'rad))
		  (?F (setf (cl-getf calc-modes 'calc-prefer-frac) t))
		  (?S (setf (cl-getf calc-modes 'calc-symbolic-mode) t))
		  (?u (setf (cl-getf calc-modes 'calc-simplify-mode) 'units))))
	      ;; Remove matched flags from the mode string.
	      (setq fmt (replace-match "" t t fmt)))
	    (unless (string-match "\\S-" fmt)
	      (setq fmt nil))))
      (when (and (not suppress-const) org-table-formula-use-constants)
	(setq formula (org-table-formula-substitute-names formula)))
      (setq orig (or (get-text-property 1 :orig-formula formula) "?"))
      (setq formula (org-table-formula-handle-first/last-rc formula))
      (while (> ndown 0)
	(setq fields (org-split-string
		      (org-trim
		       (buffer-substring-no-properties
			(line-beginning-position) (line-end-position)))
		      " *| *"))
	;; replace fields with duration values if relevant
	(if duration
	    (setq fields
		  (mapcar (lambda (x) (org-table-time-string-to-seconds x))
			  fields)))
	(if (eq numbers t)
	    (setq fields (mapcar
			  (lambda (x)
			    (if (string-match "\\S-" x)
				(number-to-string (string-to-number x))
			      x))
			  fields)))
	(setq ndown (1- ndown))
	(setq form (copy-sequence formula)
	      lispp (and (> (length form) 2) (equal (substring form 0 2) "'(")))
	(if (and lispp literal) (setq lispp 'literal))

	;; Insert row and column number of formula result field
	(while (string-match "[@$]#" form)
	  (setq form
		(replace-match
		 (format "%d"
			 (save-match-data
			   (if (equal (substring form (match-beginning 0)
						 (1+ (match-beginning 0)))
				      "@")
			       (org-table-current-dline)
			     (org-table-current-column))))
		 t t form)))

	;; Check for old vertical references
	(org-table--error-on-old-row-references form)
	;; Insert remote references
	(setq form (org-table-remote-reference-indirection form))
	(while (string-match "\\<remote([ \t]*\\([^,)]+\\)[ \t]*,[ \t]*\\([^\n)]+\\))" form)
	  (setq form
		(replace-match
		 (save-match-data
		   (org-table-make-reference
		    (let ((rmtrng (org-table-get-remote-range
				   (match-string 1 form) (match-string 2 form))))
		      (if duration
			  (if (listp rmtrng)
			      (mapcar (lambda(x) (org-table-time-string-to-seconds x)) rmtrng)
			    (org-table-time-string-to-seconds rmtrng))
			rmtrng))
		    keep-empty numbers lispp))
		 t t form)))
	;; Insert complex ranges
	(while (and (string-match org-table-range-regexp form)
		    (> (length (match-string 0 form)) 1))
	  (setq formrg
		(save-match-data
		  (org-table-get-range
		   (match-string 0 form) org-table-current-begin-pos n0)))
	  (setq formrpl
		(save-match-data
		  (org-table-make-reference
		   ;; possibly handle durations
		   (if duration
		       (if (listp formrg)
			   (mapcar (lambda(x) (org-table-time-string-to-seconds x)) formrg)
			 (org-table-time-string-to-seconds formrg))
		     formrg)
		   keep-empty numbers lispp)))
	  (if (not (save-match-data
		     (string-match (regexp-quote form) formrpl)))
	      (setq form (replace-match formrpl t t form))
	    (user-error "Spreadsheet error: invalid reference \"%s\"" form)))
	;; Insert simple ranges, i.e. included in the current row.
	(while (string-match
		"\\$\\(\\([-+]\\)?[0-9]+\\)\\.\\.\\$\\(\\([-+]\\)?[0-9]+\\)"
		form)
	  (setq form
		(replace-match
		 (save-match-data
		   (org-table-make-reference
		    (cl-subseq fields
			       (+ (if (match-end 2) n0 0)
				  (string-to-number (match-string 1 form))
				  -1)
			       (+ (if (match-end 4) n0 0)
				  (string-to-number (match-string 3 form))))
		    keep-empty numbers lispp))
		 t t form)))
	(setq form0 form)
	;; Insert the references to fields in same row
	(while (string-match "\\$\\(\\([-+]\\)?[0-9]+\\)" form)
	  (let* ((n (+ (string-to-number (match-string 1 form))
		       (if (match-end 2) n0 0)))
		 (x (nth (1- (if (= n 0) n0 (max n 1))) fields)))
	    (setq formrpl (save-match-data
			    (org-table-make-reference
			     x keep-empty numbers lispp)))
	    (when (or (not x)
		      (save-match-data
			(string-match (regexp-quote formula) formrpl)))
	      (user-error "Invalid field specifier \"%s\""
			  (match-string 0 form))))
	  (setq form (replace-match formrpl t t form)))

	(if lispp
	    (setq ev (condition-case nil
                         ;; FIXME: Arbitrary code evaluation.
			 (eval (eval (read form)))
		       (error "#ERROR"))
		  ev (if (numberp ev) (number-to-string ev) ev)
		  ev (if duration (org-table-time-seconds-to-string
				   (string-to-number ev)
				   duration-output-format)
		       ev))

	  ;; Use <...> timestamps so that Calc can handle them.
	  (setq form
		(replace-regexp-in-string org-ts-regexp-inactive "<\\1>" form))
	  ;; Internationalize local timestamps by setting locale to
	  ;; "C".
	  (setq form
		(replace-regexp-in-string
		 org-ts-regexp
		 (lambda (ts)
		   (let ((system-time-locale "C"))
		     (format-time-string
		      (org-time-stamp-format
		       (string-match-p "[0-9]\\{1,2\\}:[0-9]\\{2\\}" ts))
		      (save-match-data (org-time-string-to-time ts)))))
		 form t t))

	  (setq ev (if (and duration (string-match "^[0-9]+:[0-9]+\\(?::[0-9]+\\)?$" form))
		       form
		     (calc-eval (cons form calc-modes)
				(when (and (not keep-empty) numbers) 'num)))
		ev (if (and duration
                            ;; When the result is an empty string,
                            ;; keep it empty.
                            ;; See https://list.orgmode.org/orgmode/CAF_DUeEFpNU5UXjE80yB1MB9xj5oVLqG=XadnkqCdzWtakWdPg@mail.gmail.com/
                            (not (string-empty-p ev)))
                       (org-table-time-seconds-to-string
			(if (string-match "^[0-9]+:[0-9]+\\(?::[0-9]+\\)?$" ev)
			    (string-to-number (org-table-time-string-to-seconds ev))
			  (string-to-number ev))
			duration-output-format)
		     ev)))

	(when org-table-formula-debug
	  (let ((wcf (current-window-configuration)))
	    (with-output-to-temp-buffer "*Substitution History*"
	      (princ (format "Substitution history of formula
Orig:   %s
$xyz->  %s
@r$c->  %s
$1->    %s\n" orig formula form0 form))
	      (if (consp ev)
		  (princ (format "        %s^\nError:  %s"
				 (make-string (car ev) ?\-) (nth 1 ev)))
		(princ (format "Result: %s\nFormat: %s\nFinal:  %s"
			       ev (or fmt "NONE")
			       (if fmt (format fmt (string-to-number ev)) ev)))))
	    (setq bw (get-buffer-window "*Substitution History*"))
	    (org-fit-window-to-buffer bw)
	    (unless (and (called-interactively-p 'any) (not ndown))
	      (unless (let (inhibit-redisplay)
			(y-or-n-p "Debugging Formula.  Continue to next? "))
		(org-table-align)
		(user-error "Abort"))
	      (delete-window bw)
	      (message "")
	      (set-window-configuration wcf))))
	(when (consp ev) (setq fmt nil ev "#ERROR"))
	(org-table-justify-field-maybe
	 (format org-table-formula-field-format
		 (cond
		  ((not (stringp ev)) ev)
		  (fmt (format fmt (string-to-number ev)))
		  ;; Replace any active time stamp in the result with
		  ;; an inactive one.  Dates in tables are likely
		  ;; piece of regular data, not meant to appear in the
		  ;; agenda.
		  (t (replace-regexp-in-string org-ts-regexp "[\\1]" ev)))))
	(if (and down (> ndown 0) (looking-at ".*\n[ \t]*|[^-]"))
	    (call-interactively 'org-return)
	  (setq ndown 0)))
      (and down (org-table-maybe-recalculate-line))
      (or suppress-align (and org-table-may-need-update
			      (org-table-align))))))

(defun org-table-put-field-property (prop value)
  (save-excursion
    (put-text-property (progn (skip-chars-backward "^|") (point))
		       (progn (skip-chars-forward "^|") (point))
		       prop value)))

(defun org-table-get-range (desc &optional tbeg col highlight corners-only)
  "Get a calc vector from a column, according to descriptor DESC.

Optional arguments TBEG and COL can give the beginning of the table and
the current column, to avoid unnecessary parsing.

HIGHLIGHT means just highlight the range.

When CORNERS-ONLY is set, only return the corners of the range as
a list (line1 column1 line2 column2) where line1 and line2 are
line numbers relative to beginning of table, or TBEG, and column1
and column2 are table column numbers."
  (let* ((desc (if (string-match-p "\\`\\$[0-9]+\\.\\.\\$[0-9]+\\'" desc)
		   (replace-regexp-in-string "\\$" "@0$" desc)
		 desc))
	 (col (or col (org-table-current-column)))
	 (tbeg (or tbeg (org-table-begin)))
	 (thisline (count-lines tbeg (line-beginning-position))))
    (unless (string-match org-table-range-regexp desc)
      (user-error "Invalid table range specifier `%s'" desc))
    (let ((rangep (match-end 3))
	  (r1 (let ((r (and (match-end 1) (match-string 1 desc))))
		(or (save-match-data
		      (and (org-string-nw-p r)
			   (org-table--descriptor-line r thisline)))
		    thisline)))
	  (r2 (let ((r (and (match-end 4) (match-string 4 desc))))
		(or (save-match-data
		      (and (org-string-nw-p r)
			   (org-table--descriptor-line r thisline)))
		    thisline)))
	  (c1 (let ((c (and (match-end 2) (substring (match-string 2 desc) 1))))
		(if (or (not c) (= (string-to-number c) 0)) col
		  (+ (string-to-number c)
		     (if (memq (string-to-char c) '(?- ?+)) col 0)))))
	  (c2 (let ((c (and (match-end 5) (substring (match-string 5 desc) 1))))
		(if (or (not c) (= (string-to-number c) 0)) col
		  (+ (string-to-number c)
		     (if (memq (string-to-char c) '(?- ?+)) col 0))))))
      (save-excursion
	(if (and (not corners-only)
		 (or (not rangep) (and (= r1 r2) (= c1 c2))))
	    ;; Just one field.
	    (progn
	      (forward-line (- r1 thisline))
	      (while (not (looking-at org-table-dataline-regexp))
		(forward-line))
	      (prog1 (org-trim (org-table-get-field c1))
		(when highlight (org-table-highlight-rectangle))))
	  ;; A range, return a vector.  First sort the numbers to get
	  ;; a regular rectangle.
	  (let ((first-row (min r1 r2))
		(last-row (max r1 r2))
		(first-column (min c1 c2))
		(last-column (max c1 c2)))
	    (if corners-only (list first-row first-column last-row last-column)
	      ;; Copy the range values into a list.
	      (forward-line (- first-row thisline))
	      (while (not (looking-at org-table-dataline-regexp))
		(forward-line)
		(cl-incf first-row))
	      (org-table-goto-column first-column)
	      (let ((beg (point)))
		(forward-line (- last-row first-row))
		(while (not (looking-at org-table-dataline-regexp))
		  (forward-line -1))
		(org-table-goto-column last-column)
		(let ((end (point)))
		  (when highlight
		    (org-table-highlight-rectangle
		     beg (progn (skip-chars-forward "^|\n") (point))))
		  ;; Return string representation of calc vector.
		  (mapcar #'org-trim
			  (apply #'append
				 (org-table-copy-region beg end))))))))))))

(defun org-table--descriptor-line (desc cline)
  "Return relative line number corresponding to descriptor DESC.
The cursor is currently in relative line number CLINE."
  (if (string-match "\\`[0-9]+\\'" desc)
      (aref org-table-dlines (string-to-number desc))
    (when (or (not (string-match
		    "^\\(\\([-+]\\)?\\(I+\\)\\)?\\(\\([-+]\\)?\\([0-9]+\\)\\)?"
		    ;;  1  2          3           4  5          6
		    desc))
	      (and (not (match-end 3)) (not (match-end 6)))
	      (and (match-end 3) (match-end 6) (not (match-end 5))))
      (user-error "Invalid row descriptor `%s'" desc))
    (let* ((hn (and (match-end 3) (- (match-end 3) (match-beginning 3))))
	   (hdir (match-string 2 desc))
	   (odir (match-string 5 desc))
	   (on (and (match-end 6) (string-to-number (match-string 6 desc))))
	   (rel (and (match-end 6)
		     (or (and (match-end 1) (not (match-end 3)))
			 (match-end 5)))))
      (when (and hn (not hdir))
	(setq cline 0)
	(setq hdir "+")
	(when (eq (aref org-table-current-line-types 0) 'hline) (cl-decf hn)))
      (when (and (not hn) on (not odir)) (user-error "Should never happen"))
      (when hn
	(setq cline
	      (org-table--row-type 'hline hn cline (equal hdir "-") nil desc)))
      (when on
	(setq cline
	      (org-table--row-type 'dline on cline (equal odir "-") rel desc)))
      cline)))

(defun org-table--row-type (type n i backwards relative desc)
  "Return relative line of Nth row with type TYPE.
Search starts from relative line I.  When BACKWARDS in non-nil,
look before I.  When RELATIVE is non-nil, the reference is
relative.  DESC is the original descriptor that started the
search, as a string."
  (let ((l (length org-table-current-line-types)))
    (catch :exit
      (dotimes (_ n)
	(while (and (cl-incf i (if backwards -1 1))
		    (>= i 0)
		    (< i l)
		    (not (eq (aref org-table-current-line-types i) type))
		    ;; We are going to cross a hline.  Check if this is
		    ;; an authorized move.
		    (cond
		     ((not relative))
		     ((not (eq (aref org-table-current-line-types i) 'hline)))
		     ((eq org-table-relative-ref-may-cross-hline t))
		     ((eq org-table-relative-ref-may-cross-hline 'error)
		      (user-error "Row descriptor %s crosses hline" desc))
		     (t (cl-decf i (if backwards -1 1)) ; Step back.
			(throw :exit nil)))))))
    (cond ((or (< i 0) (>= i l))
	   (user-error "Row descriptor %s leads outside table" desc))
	  ;; The last hline doesn't exist.  Instead, point to last row
	  ;; in table.
	  ((= i (1- l)) (1- i))
	  (t i))))

(defun org-table--error-on-old-row-references (s)
  (when (string-match "&[-+0-9I]" s)
    (user-error "Formula contains old &row reference, please rewrite using @-syntax")))

(defun org-table-make-reference (elements keep-empty numbers lispp)
  "Convert list ELEMENTS to something appropriate to insert into formula.
KEEP-EMPTY indicated to keep empty fields, default is to skip them.
NUMBERS indicates that everything should be converted to numbers.
LISPP non-nil means to return something appropriate for a Lisp
list, `literal' is for the format specifier L."
  ;; Calc nan (not a number) is used for the conversion of the empty
  ;; field to a reference for several reasons: (i) It is accepted in a
  ;; Calc formula (e. g. "" or "()" would result in a Calc error).
  ;; (ii) In a single field (not in range) it can be distinguished
  ;; from "(nan)" which is the reference made from a single field
  ;; containing "nan".
  (if (stringp elements)
      ;; field reference
      (if lispp
	  (if (eq lispp 'literal)
	      elements
            ;; Ignore KEEP-EMPTY here.
            ;; When ELEMENTS="" and NUMBERS=t, (string-to-number "")
            ;; returns 0 - consistent with (0) for Calc branch.
            ;; When ELEMENTS="" and NUMBERS=nil, `prin1-to-string' will
            ;; return "\"\"" - historical behavior that also does not
            ;; leave missing arguments in formulas like (string< $1 $2)
            ;; when $2 cell is empty.
            (prin1-to-string
	     (if numbers (string-to-number elements) elements)))
	(if (string-match "\\S-" elements)
	    (progn
	      (when numbers (setq elements (number-to-string
					    (string-to-number elements))))
	      (concat "(" elements ")"))
	  (if (or (not keep-empty) numbers) "(0)" "nan")))
    ;; range reference
    (unless keep-empty
      (setq elements
	    (delq nil
		  (mapcar (lambda (x) (if (string-match "\\S-" x) x nil))
			  elements))))
    (setq elements (or elements '()))  ; if delq returns nil then we need '()
    (if lispp
	(mapconcat
	 (lambda (x)
	   (if (eq lispp 'literal)
	       x
	     (prin1-to-string (if numbers (string-to-number x) x))))
	 elements " ")
      (concat "[" (mapconcat
		   (lambda (x)
		     (if (string-match "\\S-" x)
			 (if numbers
			     (number-to-string (string-to-number x))
			   x)
		       (if (or (not keep-empty) numbers) "0" "nan")))
		   elements
		   ",") "]"))))

(defun org-table-message-once-per-second (t1 &rest args)
  "If there has been more than one second since T1, display message.
ARGS are passed as arguments to the `message' function.  Returns
current time if a message is printed, otherwise returns T1.  If
T1 is nil, always messages."
  (let ((curtime (current-time)))
    (if (or (not t1) (time-less-p 1 (time-subtract curtime t1)))
	(progn (apply 'message args)
	       curtime)
      t1)))

;;;###autoload
(defun org-table-recalculate (&optional all noalign)
  "Recalculate the current table line by applying all stored formulas.

With prefix arg ALL, do this for all lines in the table.

When called with a `\\[universal-argument] \\[universal-argument]' prefix, or \
if ALL is the symbol `iterate',
recompute the table until it no longer changes.

If NOALIGN is not nil, do not re-align the table after the computations
are done.  This is typically used internally to save time, if it is
known that the table will be realigned a little later anyway."
  (interactive "P")
  (unless (memq this-command org-recalc-commands)
    (push this-command org-recalc-commands))
  (unless (org-at-table-p) (user-error "Not at a table"))
  (if (or (eq all 'iterate) (equal all '(16)))
      (org-table-iterate)
    (org-table-analyze)
    (let* ((eqlist (sort (org-table-get-stored-formulas)
			 (lambda (a b) (string< (car a) (car b)))))
	   (inhibit-redisplay (not debug-on-error))
	   (line-re org-table-dataline-regexp)
	   (log-first-time (current-time))
	   (log-last-time log-first-time)
	   (cnt 0)
	   beg end eqlcol eqlfield)
      ;; Insert constants in all formulas.
      (when eqlist
	(org-table-with-shrunk-columns
	 (org-table-save-field
	  ;; Expand equations, then split the equation list between
	  ;; column formulas and field formulas.
	  (dolist (eq eqlist)
	    (let* ((rhs (org-table-formula-substitute-names
			 (org-table-formula-handle-first/last-rc (cdr eq))))
		   (old-lhs (car eq))
		   (lhs
		    (org-table-formula-handle-first/last-rc
		     (cond
		      ((string-match "\\`@-?I+" old-lhs)
		       (user-error "Can't assign to hline relative reference"))
		      ((string-match "\\`\\$[<>]" old-lhs)
		       (let ((new (org-table-formula-handle-first/last-rc
				   old-lhs)))
			 (when (assoc new eqlist)
			   (user-error "\"%s=\" formula tries to overwrite \
existing formula for column %s"
				       old-lhs
				       new))
			 new))
		      (t old-lhs)))))
	      (if (string-match-p "\\`\\$[0-9]+\\'" lhs)
		  (push (cons lhs rhs) eqlcol)
		(push (cons lhs rhs) eqlfield))))
	  (setq eqlcol (nreverse eqlcol))
	  ;; Expand ranges in lhs of formulas
	  (setq eqlfield (org-table-expand-lhs-ranges (nreverse eqlfield)))
	  ;; Get the correct line range to process.
	  (if all
	      (progn
		(setq end (copy-marker (org-table-end)))
		(goto-char (setq beg org-table-current-begin-pos))
		(cond
		 ((re-search-forward org-table-calculate-mark-regexp end t)
		  ;; This is a table with marked lines, compute selected
		  ;; lines.
		  (setq line-re org-table-recalculate-regexp))
		 ;; Move forward to the first non-header line.
		 ((and (re-search-forward org-table-dataline-regexp end t)
		       (re-search-forward org-table-hline-regexp end t)
		       (re-search-forward org-table-dataline-regexp end t))
		  (setq beg (match-beginning 0)))
		 ;; Just leave BEG at the start of the table.
		 (t nil)))
	    (setq beg (line-beginning-position)
		  end (copy-marker (line-beginning-position 2))))
	  (goto-char beg)
	  ;; Mark named fields untouchable.  Also check if several
	  ;; field/range formulas try to set the same field.
	  (remove-text-properties beg end '(:org-untouchable t))
	  (let ((current-line (count-lines org-table-current-begin-pos
					   (line-beginning-position)))
		seen-fields)
	    (dolist (eq eqlfield)
	      (let* ((name (car eq))
		     (location (assoc name org-table-named-field-locations))
		     (eq-line (or (nth 1 location)
				  (and (string-match "\\`@\\([0-9]+\\)" name)
				       (aref org-table-dlines
					     (string-to-number
					      (match-string 1 name))))))
		     (reference
		      (if location
			  ;; Turn field coordinates associated to NAME
			  ;; into an absolute reference.
			  (format "@%d$%d"
				  (org-table-line-to-dline eq-line)
				  (nth 2 location))
			name)))
		(when (member reference seen-fields)
		  (user-error "Several field/range formulas try to set %s"
			      reference))
		(push reference seen-fields)
		(when (or all (eq eq-line current-line))
		  (org-table-goto-field name)
		  (org-table-put-field-property :org-untouchable t)))))
	  ;; Evaluate the column formulas, but skip fields covered by
	  ;; field formulas.
	  (goto-char beg)
	  (while (re-search-forward line-re end t)
	    (unless (string-match "\\` *[_^!$/] *\\'" (org-table-get-field 1))
	      ;; Unprotected line, recalculate.
	      (cl-incf cnt)
	      (when all
		(setq log-last-time
		      (org-table-message-once-per-second
		       log-last-time
		       "Re-applying formulas to full table...(line %d)" cnt)))
	      (if (markerp org-last-recalc-line)
		  (move-marker org-last-recalc-line (line-beginning-position))
		(setq org-last-recalc-line
		      (copy-marker (line-beginning-position))))
	      (dolist (entry eqlcol)
		(goto-char org-last-recalc-line)
		(org-table-goto-column
		 (string-to-number (substring (car entry) 1)) nil 'force)
		(unless (get-text-property (point) :org-untouchable)
		  (org-table-eval-formula
		   nil (cdr entry) 'noalign 'nocst 'nostore 'noanalysis)))))
	  ;; Evaluate the field formulas.
	  (dolist (eq eqlfield)
	    (let ((reference (car eq))
		  (formula (cdr eq)))
	      (setq log-last-time
		    (org-table-message-once-per-second
		     (and all log-last-time)
		     "Re-applying formula to field: %s" (car eq)))
	      (org-table-goto-field
	       reference
	       ;; Possibly create a new column, as long as
	       ;; `org-table-formula-create-columns' allows it.
	       (let ((column-count (progn (end-of-line)
					  (1- (org-table-current-column)))))
		 (lambda (column)
		   (when (> column 1000)
		     (user-error "Formula column target too large"))
		   (and (> column column-count)
			(or (eq org-table-formula-create-columns t)
			    (and (eq org-table-formula-create-columns 'warn)
				 (progn
				   (org-display-warning
				    "Out-of-bounds formula added columns")
				   t))
			    (and (eq org-table-formula-create-columns 'prompt)
				 (yes-or-no-p
				  "Out-of-bounds formula.  Add columns? "))
			    (user-error
			     "Missing columns in the table.  Aborting"))))))
	      (org-table-eval-formula nil formula t t t t)))
	  ;; Clean up marker.
	  (set-marker end nil)))
	(unless noalign
	  (when org-table-may-need-update (org-table-align))
	  (when all
	    (org-table-message-once-per-second
	     log-first-time "Re-applying formulas to %d lines... done" cnt)))
	(org-table-message-once-per-second
	 (and all log-first-time) "Re-applying formulas... done")))))

;;;###autoload
(defun org-table-iterate (&optional arg)
  "Recalculate the table until it does not change anymore.
The maximum number of iterations is 10, but you can choose a different value
with the prefix ARG."
  (interactive "P")
  (let ((imax (if arg (prefix-numeric-value arg) 10))
	(i 0)
	(lasttbl (buffer-substring (org-table-begin) (org-table-end)))
	thistbl)
    (catch 'exit
      (while (< i imax)
	(setq i (1+ i))
	(org-table-recalculate 'all)
	(setq thistbl (buffer-substring (org-table-begin) (org-table-end)))
	(if (not (string= lasttbl thistbl))
	    (setq lasttbl thistbl)
	  (if (> i 1)
	      (message "Convergence after %d iterations" i)
	    (message "Table was already stable"))
	  (throw 'exit t)))
      (user-error "No convergence after %d iterations" i))))

;;;###autoload
(defun org-table-recalculate-buffer-tables ()
  "Recalculate all tables in the current buffer."
  (interactive)
  (org-with-wide-buffer
   (org-table-map-tables
    (lambda ()
      ;; Reason for separate `org-table-align': When repeating
      ;; (org-table-recalculate t) `org-table-may-need-update' gets in
      ;; the way.
      (org-table-recalculate t t)
      (org-table-align))
    t)))

;;;###autoload
(defun org-table-iterate-buffer-tables ()
  "Iterate all tables in the buffer, to converge inter-table dependencies."
  (interactive)
  (let* ((imax 10)
	 (i imax)
	 (checksum (md5 (buffer-string)))
	 c1)
    (org-with-wide-buffer
     (catch 'exit
       (while (> i 0)
	 (setq i (1- i))
	 (org-table-map-tables (lambda () (org-table-recalculate t t)) t)
	 (if (equal checksum (setq c1 (md5 (buffer-string))))
	     (progn
	       (org-table-map-tables #'org-table-align t)
	       (message "Convergence after %d iterations" (- imax i))
	       (throw 'exit t))
	   (setq checksum c1)))
       (org-table-map-tables #'org-table-align t)
       (user-error "No convergence after %d iterations" imax)))))

(defun org-table-calc-current-TBLFM (&optional arg)
  "Apply the #+TBLFM in the line at point to the table."
  (interactive "P")
  (unless (org-at-TBLFM-p) (user-error "Not at a #+TBLFM line"))
  (let ((formula (buffer-substring
		  (line-beginning-position)
		  (line-end-position))))
    (save-excursion
      ;; Insert a temporary formula at right after the table
      (goto-char (org-table-TBLFM-begin))
      (let ((s (point-marker)))
	(insert formula "\n")
	(let ((e (point-marker)))
	  ;; Recalculate the table.
	  (forward-line -1)		; move to the inserted line
	  (skip-chars-backward " \r\n\t")
	  (unwind-protect
	      (org-call-with-arg #'org-table-recalculate (or arg t))
	    ;; Delete the formula inserted temporarily.
	    (delete-region s e)
	    (set-marker s nil)
	    (set-marker e nil)))))))

(defun org-table-TBLFM-begin ()
  "Find the beginning of the TBLFM lines and return its position.
Return nil when the beginning of TBLFM line was not found."
  (save-excursion
    (when (progn (forward-line 1)
		 (re-search-backward org-table-TBLFM-begin-regexp nil t))
      (line-beginning-position 2))))

(defun org-table-expand-lhs-ranges (equations)
  "Expand list of formulas.
If some of the RHS in the formulas are ranges or a row reference,
expand them to individual field equations for each field.  This
function assumes the table is already analyzed (i.e., using
`org-table-analyze')."
  (let (res)
    (dolist (e equations (nreverse res))
      (let ((lhs (car e))
	    (rhs (cdr e)))
	(cond
	 ((string-match-p "\\`@[-+0-9]+\\$-?[0-9]+\\'" lhs)
	  ;; This just refers to one fixed field.
	  (push e res))
	 ((string-match-p "\\`[a-zA-Z][_a-zA-Z0-9]*\\'" lhs)
	  ;; This just refers to one fixed named field.
	  (push e res))
	 ((string-match-p "\\`\\$[0-9]+\\'" lhs)
	  ;; Column formulas are treated specially and are not
	  ;; expanded.
	  (push e res))
	 ((string-match "\\`@[0-9]+\\'" lhs)
	  (dotimes (ic org-table-current-ncol)
	    (push (cons (propertize (format "%s$%d" lhs (1+ ic)) :orig-eqn e)
			rhs)
		  res)))
	 (t
	  (let* ((range (org-table-get-range
			 lhs org-table-current-begin-pos 1 nil 'corners))
		 (r1 (org-table-line-to-dline (nth 0 range)))
		 (c1 (nth 1 range))
		 (r2 (org-table-line-to-dline (nth 2 range) 'above))
		 (c2 (nth 3 range)))
	    (cl-loop for ir from r1 to r2 do
		     (cl-loop for ic from c1 to c2 do
			      (push (cons (propertize
					   (format "@%d$%d" ir ic) :orig-eqn e)
					  rhs)
				    res))))))))))

(defun org-table-formula-handle-first/last-rc (s)
  "Replace @<, @>, $<, $> with first/last row/column of the table.
So @< and $< will always be replaced with @1 and $1, respectively.
The advantage of these special markers are that structure editing of
the table will not change them, while @1 and $1 will be modified
when a line/row is swapped out of that privileged position.  So for
formulas that use a range of rows or columns, it may often be better
to anchor the formula with \"I\" row markers, or to offset from the
borders of the table using the @< @> $< $> makers."
  (let (n nmax len char (start 0))
    (while (string-match "\\([@$]\\)\\(<+\\|>+\\)\\|\\(remote([^)]+)\\)"
			 s start)
      (if (match-end 3)
	  (setq start (match-end 3))
	(setq nmax (if (equal (match-string 1 s) "@")
		       (1- (length org-table-dlines))
		     org-table-current-ncol)
	      len (- (match-end 2) (match-beginning 2))
	      char (string-to-char (match-string 2 s))
	      n (if (= char ?<)
		    len
		  (- nmax len -1)))
	(if (or (< n 1) (> n nmax))
	    (user-error "Reference \"%s\" in expression \"%s\" points outside table"
			(match-string 0 s) s))
	(setq start (match-beginning 0))
	(setq s (replace-match (format "%s%d" (match-string 1 s) n) t t s)))))
  s)

(defun org-table-formula-substitute-names (f)
  "Replace $const with values in string F."
  (let ((start 0)
	(pp (/= (string-to-char f) ?'))
	(duration (string-match-p ";.*[Tt].*\\'" f))
	(new (replace-regexp-in-string	; Check for column names.
	      org-table-column-name-regexp
	      (lambda (m)
		(concat "$" (cdr (assoc (match-string 1 m)
					org-table-column-names))))
	      f t t)))
    ;; Parameters and constants.
    (while (setq start
		 (string-match
		  "\\$\\([a-zA-Z][_a-zA-Z0-9]*\\)\\|\\(\\<remote([^)]*)\\)"
		  new start))
      (if (match-end 2) (setq start (match-end 2))
	(cl-incf start)
	;; When a duration is expected, convert value on the fly.
	(let ((value
	       (save-match-data
		 (let ((v (org-table-get-constant (match-string 1 new))))
		   (if (and (org-string-nw-p v) duration)
		       (org-table-time-string-to-seconds v)
		     v)))))
	  (when value
	    (setq new (replace-match
		       (concat (and pp "(") value (and pp ")")) t t new))))))
    (if org-table-formula-debug (propertize new :orig-formula f) new)))

(defun org-table-get-constant (const)
  "Find the value for a parameter or constant in a formula.
Parameters get priority."
  (or (cdr (assoc const org-table-local-parameters))
      (cdr (assoc const org-table-formula-constants-local))
      (cdr (assoc const org-table-formula-constants))
      (and (fboundp 'constants-get) (constants-get const))
      (and (string= (substring const 0 (min 5 (length const))) "PROP_")
	   (org-entry-get nil (substring const 5) 'inherit))
      "#UNDEFINED_NAME"))

(defvar org-table-fedit-map
  (let ((map (make-sparse-keymap)))
    (org-defkey map "\C-x\C-s"      'org-table-fedit-finish)
    (org-defkey map "\C-c\C-s"      'org-table-fedit-finish)
    (org-defkey map "\C-c\C-c"      'org-table-fedit-finish)
    (org-defkey map "\C-c'"         'org-table-fedit-finish)
    (org-defkey map "\C-c\C-q"      'org-table-fedit-abort)
    (org-defkey map "\C-c?"	    'org-table-show-reference)
    (org-defkey map [(meta shift up)]    'org-table-fedit-line-up)
    (org-defkey map [(meta shift down)]  'org-table-fedit-line-down)
    (org-defkey map [(shift up)]    'org-table-fedit-ref-up)
    (org-defkey map [(shift down)]  'org-table-fedit-ref-down)
    (org-defkey map [(shift left)]  'org-table-fedit-ref-left)
    (org-defkey map [(shift right)] 'org-table-fedit-ref-right)
    (org-defkey map [(meta up)]     'org-table-fedit-scroll-down)
    (org-defkey map [(meta down)]   'org-table-fedit-scroll)
    (org-defkey map [(meta tab)]    'lisp-complete-symbol)
    (org-defkey map "\M-\C-i"       'lisp-complete-symbol)
    (org-defkey map [(tab)]	    'org-table-fedit-lisp-indent)
    (org-defkey map "\C-i"	    'org-table-fedit-lisp-indent)
    (org-defkey map "\C-c\C-r" 'org-table-fedit-toggle-ref-type)
    (org-defkey map "\C-c}"    'org-table-fedit-toggle-coordinates)
    map))

(easy-menu-define org-table-fedit-menu org-table-fedit-map "Org Edit Formulas Menu."
  '("Edit-Formulas"
    ["Finish and Install" org-table-fedit-finish t]
    ["Finish, Install, and Apply" (org-table-fedit-finish t) :keys "C-u C-c C-c"]
    ["Abort" org-table-fedit-abort t]
    "--"
    ["Pretty-Print Lisp Formula" org-table-fedit-lisp-indent t]
    ["Complete Lisp Symbol" lisp-complete-symbol t]
    "--"
    "Shift Reference at Point"
    ["Up" org-table-fedit-ref-up t]
    ["Down" org-table-fedit-ref-down t]
    ["Left" org-table-fedit-ref-left t]
    ["Right" org-table-fedit-ref-right t]
    "-"
    "Change Test Row for Column Formulas"
    ["Up" org-table-fedit-line-up t]
    ["Down" org-table-fedit-line-down t]
    "--"
    ["Scroll Table Window" org-table-fedit-scroll t]
    ["Scroll Table Window down" org-table-fedit-scroll-down t]
    ["Show Table Grid" org-table-fedit-toggle-coordinates
     :style toggle :selected (with-current-buffer (marker-buffer org-pos)
			       org-table-overlay-coordinates)]
    "--"
    ["Standard Refs (B3 instead of @3$2)" org-table-fedit-toggle-ref-type
     :style toggle :selected org-table-buffer-is-an]))

(defvar org-table--fedit-source nil
  "Position of the TBLFM line being edited.")

;;;###autoload
(defun org-table-edit-formulas ()
  "Edit the formulas of the current table in a separate buffer."
  (interactive)
  (let ((at-tblfm (org-at-TBLFM-p)))
    (unless (or at-tblfm (org-at-table-p))
      (user-error "Not at a table"))
    (save-excursion
      ;; Move point within the table before analyzing it.
      (when at-tblfm (re-search-backward "^[ \t]*|"))
      (org-table-analyze))
    (let ((key (org-table-current-field-formula 'key 'noerror))
	  (eql (sort (org-table-get-stored-formulas t (and at-tblfm (point)))
		     #'org-table-formula-less-p))
	  (pos (point-marker))
	  (source (copy-marker (line-beginning-position)))
	  (startline 1)
	  (wc (current-window-configuration))
	  (sel-win (selected-window))
	  (titles '((column . "# Column Formulas\n")
		    (field . "# Field and Range Formulas\n")
		    (named . "# Named Field Formulas\n"))))
      (let ((pop-up-frames nil))
        ;; We explicitly prohibit creating edit buffer in a new frame
        ;; - such configuration is not supported.
        (switch-to-buffer-other-window "*Edit Formulas*"))
      (erase-buffer)
      ;; Keep global-font-lock-mode from turning on font-lock-mode
      (let ((font-lock-global-modes '(not fundamental-mode)))
	(fundamental-mode))
      (setq-local font-lock-global-modes (list 'not major-mode))
      (setq-local org-pos pos)
      (setq-local org-table--fedit-source source)
      (setq-local org-window-configuration wc)
      (setq-local org-selected-window sel-win)
      (use-local-map org-table-fedit-map)
      (add-hook 'post-command-hook #'org-table-fedit-post-command t t)
      (setq startline (org-current-line))
      (dolist (entry eql)
	(let* ((type (cond
		      ((string-match "\\`\\$\\([0-9]+\\|[<>]+\\)\\'"
				     (car entry))
		       'column)
		      ((equal (string-to-char (car entry)) ?@) 'field)
		      (t 'named)))
	       (title (assq type titles)))
	  (when title
	    (unless (bobp) (insert "\n"))
	    (insert
	     (org-add-props (cdr title) nil 'face font-lock-comment-face))
	    (setq titles (remove title titles)))
	  (when (equal key (car entry)) (setq startline (org-current-line)))
	  (let ((s (concat
		    (if (memq (string-to-char (car entry)) '(?@ ?$)) "" "$")
		    (car entry) " = " (cdr entry) "\n")))
	    (remove-text-properties 0 (length s) '(face nil) s)
	    (insert s))))
      (when (eq org-table-use-standard-references t)
	(org-table-fedit-toggle-ref-type))
      (org-goto-line startline)
      (message "%s" (substitute-command-keys "\\<org-mode-map>\
Edit formulas, finish with `\\[org-ctrl-c-ctrl-c]' or `\\[org-edit-special]'.  \
See menu for more commands.")))))

(defun org-table-fedit-post-command ()
  (when (not (memq this-command '(lisp-complete-symbol)))
    (let ((win (selected-window)))
      (save-excursion
	(ignore-errors (org-table-show-reference))
	(select-window win)))))

(defun org-table-formula-to-user (s)
  "Convert a formula from internal to user representation."
  (if (eq org-table-use-standard-references t)
      (org-table-convert-refs-to-an s)
    s))

(defun org-table-formula-from-user (s)
  "Convert a formula from user to internal representation."
  (if org-table-use-standard-references
      (org-table-convert-refs-to-rc s)
    s))

(defun org-table-convert-refs-to-rc (s)
  "Convert spreadsheet references from A7 to @7$28.
Works for single references, but also for entire formulas and even the
full TBLFM line."
  (let ((start 0))
    (while (string-match "\\<\\([a-zA-Z]+\\)\\([0-9]+\\>\\|&\\)\\|\\(;[^\r\n:]+\\|\\<remote([^,)]*)\\)" s start)
      (cond
       ((match-end 3)
	;; format match, just advance
	(setq start (match-end 0)))
       ((and (> (match-beginning 0) 0)
	     (equal ?. (aref s (max (1- (match-beginning 0)) 0)))
	     (not (equal ?. (aref s (max (- (match-beginning 0) 2) 0)))))
	;; 3.e5 or something like this.
	(setq start (match-end 0)))
       ((or (> (- (match-end 1) (match-beginning 1)) 2)
	    ;; (member (match-string 1 s)
	    ;;	    '("arctan" "exp" "expm" "lnp" "log" "stir"))
	    )
	;; function name, just advance
	(setq start (match-end 0)))
       (t
	(setq start (match-beginning 0)
	      s (replace-match
		 (if (equal (match-string 2 s) "&")
		     (format "$%d" (org-letters-to-number (match-string 1 s)))
		   (format "@%d$%d"
			   (string-to-number (match-string 2 s))
			   (org-letters-to-number (match-string 1 s))))
		 t t s)))))
    s))

(defun org-table-convert-refs-to-an (s)
  "Convert spreadsheet references from to @7$28 to AB7.
Works for single references, but also for entire formulas and even the
full TBLFM line.

Leave the relative references unchanged."
  (while (string-match "@\\([0-9]+\\)\\$\\([0-9]+\\)" s)
    (setq s (replace-match
	     (format "%s%d"
		     (org-number-to-letters
		      (string-to-number (match-string 2 s)))
		     (string-to-number (match-string 1 s)))
	     t t s)))
  (while (string-match "\\(^\\|[^0-9a-zA-Z]\\)\\$\\([1-9][0-9]*\\)" s)
    (setq s (replace-match (concat "\\1"
				   (org-number-to-letters
				    (string-to-number (match-string 2 s))) "&")
			   t nil s)))
  s)

(defun org-letters-to-number (s)
  "Convert a base 26 number represented by letters into an integer.
For example:  AB -> 28."
  (let ((n 0))
    (setq s (upcase s))
    (while (> (length s) 0)
      (setq n (+ (* n 26) (string-to-char s) (- ?A) 1)
	    s (substring s 1)))
    n))

(defun org-number-to-letters (n)
  "Convert an integer into a base 26 number represented by letters.
For example:  28 -> AB."
  (let ((s ""))
    (while (> n 0)
      (setq s (concat (char-to-string (+ (mod (1- n) 26) ?A)) s)
	    n (/ (1- n) 26)))
    s))

(defun org-table-time-string-to-seconds (s)
  "Convert a time string into numerical duration in seconds.
S can be a string matching either -?HH:MM:SS or -?HH:MM.
If S is a string representing a number, keep this number."
  (if (equal s "")
      s
    (let (hour minus min sec res)
      (cond
       ((and (string-match "\\(-?\\)\\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\)" s))
	(setq minus (< 0 (length (match-string 1 s)))
	      hour (string-to-number (match-string 2 s))
	      min (string-to-number (match-string 3 s))
	      sec (string-to-number (match-string 4 s)))
	(if minus
	    (setq res (- (+ (* hour 3600) (* min 60) sec)))
	  (setq res (+ (* hour 3600) (* min 60) sec))))
       ((and (not (string-match org-ts-regexp-both s))
	     (string-match "\\(-?\\)\\([0-9]+\\):\\([0-9]+\\)" s))
	(setq minus (< 0 (length (match-string 1 s)))
	      hour (string-to-number (match-string 2 s))
	      min (string-to-number (match-string 3 s)))
	(if minus
	    (setq res (- (+ (* hour 3600) (* min 60))))
	  (setq res (+ (* hour 3600) (* min 60)))))
       (t (setq res (string-to-number s))))
      (number-to-string res))))

(defun org-table-time-seconds-to-string (secs &optional output-format)
  "Convert a number of seconds to a time string.
If OUTPUT-FORMAT is non-nil, return a number of days, hours,
minutes or seconds."
  (let* ((secs0 (abs secs))
	 (res
	  (cond ((eq output-format 'days)
		 (format "%.3f" (/ (float secs0) 86400)))
		((eq output-format 'hours)
		 (format "%.2f" (/ (float secs0) 3600)))
		((eq output-format 'minutes)
		 (format "%.1f" (/ (float secs0) 60)))
		((eq output-format 'seconds)
		 (format "%d" secs0))
		((eq output-format 'hh:mm)
		 ;; Ignore seconds
		 (substring (format-seconds
			     (if org-table-duration-hour-zero-padding
				 "%.2h:%.2m:%.2s" "%h:%.2m:%.2s")
			     secs0)
			    0 -3))
		(t (format-seconds
		    (if org-table-duration-hour-zero-padding
			"%.2h:%.2m:%.2s" "%h:%.2m:%.2s")
		    secs0)))))
    (if (< secs 0) (concat "-" res) res)))

(defun org-table-fedit-convert-buffer (function)
  "Convert all references in this buffer, using FUNCTION."
  (let ((origin (copy-marker (line-beginning-position))))
    (goto-char (point-min))
    (while (not (eobp))
      (insert (funcall function (buffer-substring (point) (line-end-position))))
      (delete-region (point) (line-end-position))
      (forward-line))
    (goto-char origin)
    (set-marker origin nil)))

(defun org-table-fedit-toggle-ref-type ()
  "Convert all references in the buffer from B3 to @3$2 and back."
  (interactive)
  (setq-local org-table-buffer-is-an (not org-table-buffer-is-an))
  (org-table-fedit-convert-buffer
   (if org-table-buffer-is-an
       'org-table-convert-refs-to-an 'org-table-convert-refs-to-rc))
  (message "Reference type switched to %s"
	   (if org-table-buffer-is-an "A1 etc" "@row$column")))

(defun org-table-fedit-ref-up ()
  "Shift the reference at point one row/hline up."
  (interactive)
  (org-table-fedit-shift-reference 'up))

(defun org-table-fedit-ref-down ()
  "Shift the reference at point one row/hline down."
  (interactive)
  (org-table-fedit-shift-reference 'down))

(defun org-table-fedit-ref-left ()
  "Shift the reference at point one field to the left."
  (interactive)
  (org-table-fedit-shift-reference 'left))

(defun org-table-fedit-ref-right ()
  "Shift the reference at point one field to the right."
  (interactive)
  (org-table-fedit-shift-reference 'right))

(defun org-table--rematch-and-replace (n &optional decr hline)
  "Re-match the group N, and replace it with the shifted reference."
  (or (match-end n) (user-error "Cannot shift reference in this direction"))
  (goto-char (match-beginning n))
  (and (looking-at (regexp-quote (match-string n)))
       (replace-match (org-table-shift-refpart (match-string 0) decr hline)
		      t t)))

(defun org-table-fedit-shift-reference (dir)
  (cond
   ((org-in-regexp "\\(\\<[a-zA-Z]\\)&")
    (if (memq dir '(left right))
	(org-table--rematch-and-replace 1 (eq dir 'left))
      (user-error "Cannot shift reference in this direction")))
   ((org-in-regexp "\\(\\<[a-zA-Z]\\{1,2\\}\\)\\([0-9]+\\)")
    ;; A B3-like reference
    (if (memq dir '(up down))
	(org-table--rematch-and-replace 2 (eq dir 'up))
      (org-table--rematch-and-replace 1 (eq dir 'left))))
   ((org-in-regexp
     "\\(@\\|\\.\\.\\)\\([-+]?\\(I+\\>\\|[0-9]+\\)\\)\\(\\$\\([-+]?[0-9]+\\)\\)?")
    ;; An internal reference
    (if (memq dir '(up down))
	(org-table--rematch-and-replace 2 (eq dir 'up) (match-end 3))
      (org-table--rematch-and-replace 5 (eq dir 'left))))))

(defun org-table-shift-refpart (ref &optional decr hline)
  "Shift a reference part REF.
If DECR is set, decrease the references row/column, else increase.
If HLINE is set, this may be a hline reference, it certainly is not
a translation reference."
  (save-match-data
    (let* ((sign (string-match "^[-+]" ref)) n)

      (if sign (setq sign (substring ref 0 1) ref (substring ref 1)))
      (cond
       ((and hline (string-match "^I+" ref))
	(setq n (string-to-number (concat sign (number-to-string (length ref)))))
	(setq n (+ n (if decr -1 1)))
	(if (= n 0) (setq n (+ n (if decr -1 1))))
	(if sign
	    (setq sign (if (< n 0) "-" "+") n (abs n))
	  (setq n (max 1 n)))
	(concat sign (make-string n ?I)))

       ((string-match "^[0-9]+" ref)
	(setq n (string-to-number (concat sign ref)))
	(setq n (+ n (if decr -1 1)))
	(if sign
	    (concat (if (< n 0) "-" "+") (number-to-string (abs n)))
	  (number-to-string (max 1 n))))

       ((string-match "^[a-zA-Z]+" ref)
	(org-number-to-letters
	 (max 1 (+ (org-letters-to-number ref) (if decr -1 1)))))

       (t (user-error "Cannot shift reference"))))))

(defun org-table-fedit-toggle-coordinates ()
  "Toggle the display of coordinates in the referenced table."
  (interactive)
  (let ((pos (marker-position org-pos)))
    (with-current-buffer (marker-buffer org-pos)
      (save-excursion
	(goto-char pos)
	(org-table-toggle-coordinate-overlays)))))

(defun org-table-fedit-finish (&optional arg)
  "Parse the buffer for formula definitions and install them.
With prefix ARG, apply the new formulas to the table."
  (interactive "P")
  (org-table-remove-rectangle-highlight)
  (when org-table-use-standard-references
    (org-table-fedit-convert-buffer 'org-table-convert-refs-to-rc)
    (setq org-table-buffer-is-an nil))
  (let ((pos org-pos)
	(sel-win org-selected-window)
	(source org-table--fedit-source)
	eql)
    (goto-char (point-min))
    (while (re-search-forward
	    "^\\(@[-+I<>0-9.$@]+\\|@?[0-9]+\\|\\$\\([a-zA-Z0-9]+\\|[<>]+\\)\\) *= *\\(.*\\(\n[ \t]+.*$\\)*\\)"
	    nil t)
      (let ((var (match-string 1))
	    (form (org-trim (match-string 3))))
	(unless (equal form "")
	  (while (string-match "[ \t]*\n[ \t]*" form)
	    (setq form (replace-match " " t t form)))
	  (when (assoc var eql)
	    (user-error "Double formulas for %s" var))
	  (push (cons var form) eql))))
    (set-window-configuration org-window-configuration)
    (select-window sel-win)
    (goto-char source)
    (org-table-store-formulas eql)
    (set-marker pos nil)
    (set-marker source nil)
    (when-let* ((window (get-buffer-window "*Edit Formulas*" t)))
      (quit-window 'kill window))
    (when (get-buffer "*Edit Formulas*") (kill-buffer "*Edit Formulas*"))
    (if arg
	(org-table-recalculate 'all)
      (message "New formulas installed - press C-u C-c C-c to apply."))))

(defun org-table-fedit-abort ()
  "Abort editing formulas, without installing the changes."
  (interactive)
  (org-table-remove-rectangle-highlight)
  (let ((pos org-pos) (sel-win org-selected-window))
    (set-window-configuration org-window-configuration)
    (select-window sel-win)
    (goto-char pos)
    (move-marker pos nil)
    (message "Formula editing aborted without installing changes")))

(defun org-table-fedit-lisp-indent ()
  "Pretty-print and re-indent Lisp expressions in the Formula Editor."
  (interactive)
  (let ((pos (point)) beg end ind)
    (forward-line 0)
    (cond
     ((looking-at "[ \t]")
      (goto-char pos)
      (call-interactively 'lisp-indent-line))
     ((looking-at "[$&@0-9a-zA-Z]+ *= *[^ \t\n']") (goto-char pos))
     ((looking-at "[$&@0-9a-zA-Z]+ *= *'(")
      (goto-char (- (match-end 0) 2))
      (setq beg (point))
      (setq ind (make-string (current-column) ?\ ))
      (condition-case nil (forward-sexp 1)
	(error
	 (user-error "Cannot pretty-print Lisp expression: Unbalanced parenthesis")))
      (setq end (point))
      (save-restriction
	(narrow-to-region beg end)
	(if (eq last-command this-command)
	    (progn
	      (goto-char (point-min))
	      (setq this-command nil)
	      (while (re-search-forward "[ \t]*\n[ \t]*" nil t)
		(replace-match " ")))
	  (pp-buffer)
	  (untabify (point-min) (point-max))
	  (goto-char (1+ (point-min)))
	  (while (re-search-forward "^." nil t)
	    (forward-line 0)
	    (insert ind))
	  (goto-char (point-max))
	  (org-delete-backward-char 1)))
      (goto-char beg))
     (t nil))))

(defun org-table-fedit-line-up ()
  "Move cursor one line up in the window showing the table."
  (interactive)
  (org-table-fedit-move 'previous-line))

(defun org-table-fedit-line-down ()
  "Move cursor one line down in the window showing the table."
  (interactive)
  (org-table-fedit-move 'next-line))

(defun org-table-fedit-move (command)
  "Move the cursor in the window showing the table.
Use COMMAND to do the motion, repeat if necessary to end up in a data line."
  (let ((org-table-allow-automatic-line-recalculation nil)
	(pos org-pos) (win (selected-window)) p)
    (select-window (get-buffer-window (marker-buffer org-pos)))
    (setq p (point))
    (call-interactively command)
    (while (and (org-at-table-p)
		(org-at-table-hline-p))
      (call-interactively command))
    (or (org-at-table-p) (goto-char p))
    (move-marker pos (point))
    (select-window win)))

(defun org-table-fedit-scroll (N)
  (interactive "p")
  (let ((other-window-scroll-buffer (marker-buffer org-pos)))
    (scroll-other-window N)))

(defun org-table-fedit-scroll-down (N)
  (interactive "p")
  (org-table-fedit-scroll (- N)))

(defun org-table-add-rectangle-overlay (beg end &optional face)
  "Add a new overlay."
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'face (or face 'secondary-selection))
    (push ov org-table-rectangle-overlays)))

(defun org-table-highlight-rectangle (&optional beg end face)
  "Highlight rectangular region in a table.
When buffer positions BEG and END are provided, use them to
delimit the region to highlight.  Otherwise, refer to point.  Use
FACE, when non-nil, for the highlight."
  (let* ((beg (or beg (point)))
	 (end (or end (point)))
	 (b (min beg end))
	 (e (max beg end))
	 (start-coordinates
	  (save-excursion
	    (goto-char b)
	    (cons (line-beginning-position) (org-table-current-column))))
	 (end-coordinates
	  (save-excursion
	    (goto-char e)
	    (cons (line-beginning-position) (org-table-current-column)))))
    (when (boundp 'org-show-positions)
      (setq org-show-positions (cons b (cons e org-show-positions))))
    (goto-char (car start-coordinates))
    (let ((column-start (min (cdr start-coordinates) (cdr end-coordinates)))
	  (column-end (max (cdr start-coordinates) (cdr end-coordinates)))
	  (last-row (car end-coordinates)))
      (while (<= (point) last-row)
	(when (looking-at org-table-dataline-regexp)
	  (org-table-goto-column column-start)
	  (skip-chars-backward "^|\n")
	  (let ((p (point)))
	    (org-table-goto-column column-end)
	    (skip-chars-forward "^|\n")
	    (org-table-add-rectangle-overlay p (point) face)))
	(forward-line)))
    (goto-char (car start-coordinates)))
  (add-hook 'before-change-functions #'org-table-remove-rectangle-highlight))

(defun org-table-remove-rectangle-highlight (&rest _ignore)
  "Remove the rectangle overlays."
  (unless org-inhibit-highlight-removal
    (remove-hook 'before-change-functions 'org-table-remove-rectangle-highlight)
    (mapc 'delete-overlay org-table-rectangle-overlays)
    (setq org-table-rectangle-overlays nil)))

(defvar-local org-table-coordinate-overlays nil
  "Collects the coordinate grid overlays, so that they can be removed.")
(put 'org-table-coordinate-overlays 'permanent-local t)

(defun org-table-overlay-coordinates ()
  "Add overlays to the table at point, to show row/column coordinates."
  (interactive)
  (mapc 'delete-overlay org-table-coordinate-overlays)
  (setq org-table-coordinate-overlays nil)
  (save-excursion
    (let ((id 0) (ih 0) hline eol str ov)
      (goto-char (org-table-begin))
      (while (org-at-table-p)
        (setq eol (line-end-position))
        (setq ov (make-overlay (line-beginning-position)
                               (1+ (line-beginning-position))))
	(push ov org-table-coordinate-overlays)
	(setq hline (looking-at org-table-hline-regexp))
	(setq str (if hline (format "I*%-2d" (setq ih (1+ ih)))
		    (format "%4d" (setq id (1+ id)))))
	(org-overlay-before-string ov str 'org-special-keyword 'evaporate)
	(when hline
	  (let ((ic 0))
	    (while (re-search-forward "[+|]\\(-+\\)" eol t)
	      (cl-incf ic)
	      (let* ((beg (1+ (match-beginning 0)))
		     (s1 (format "$%d" ic))
		     (s2 (org-number-to-letters ic))
		     (str (if (eq t org-table-use-standard-references) s2 s1))
		     (ov (make-overlay beg (+ beg (length str)))))
		(push ov org-table-coordinate-overlays)
		(org-overlay-display ov str 'org-special-keyword 'evaporate)))))
	(forward-line)))))

;;;###autoload
(defun org-table-toggle-coordinate-overlays ()
  "Toggle the display of Row/Column numbers in tables."
  (interactive)
  (if (not (org-at-table-p))
      (user-error "Not on a table")
    (setq org-table-overlay-coordinates (not org-table-overlay-coordinates))
    (when (and (org-at-table-p) org-table-overlay-coordinates)
      (org-table-align))
    (unless org-table-overlay-coordinates
      (mapc 'delete-overlay org-table-coordinate-overlays)
      (setq org-table-coordinate-overlays nil))
    (message "Tables Row/Column numbers display turned %s"
	     (if org-table-overlay-coordinates "on" "off"))))

;;;###autoload
(defun org-table-toggle-formula-debugger ()
  "Toggle the formula debugger in tables."
  (interactive)
  (setq org-table-formula-debug (not org-table-formula-debug))
  (message "Formula debugging has been turned %s"
	   (if org-table-formula-debug "on" "off")))


;;; Columns Shrinking

(defun org-table--shrunk-field ()
  "Non-nil if current field is narrowed.
When non-nil, return the overlay narrowing the field."
  (cl-some (lambda (o)
	     (and (eq 'table-column-hide (overlay-get o 'org-overlay-type))
		  o))
	   (overlays-at (save-excursion
			  (skip-chars-forward (if (org-at-table-hline-p) "^+|"
						"^|")
					      (line-end-position))
			  (1- (point))))))

(defun org-table--list-shrunk-columns ()
  "List currently shrunk columns in table at point."
  (save-excursion
    ;; We really check shrunk columns in current row only.  It could
    ;; be wrong if all rows do not contain the same number of columns
    ;; (i.e. the table is not properly aligned).  As a consequence,
    ;; some columns may not be shrunk again upon aligning the table.
    ;;
    ;; For example, in the following table, cursor is on first row and
    ;; "<>" indicates a shrunk column.
    ;;
    ;; | |
    ;; | | <> |
    ;;
    ;; Aligning table from the first row will not shrink again the
    ;; second row, which was not visible initially.
    ;;
    ;; However, fixing it requires checking every row, which may be
    ;; slow on large tables.  Moreover, the hindrance of this
    ;; pathological case is very limited.
    (forward-line 0)
    (search-forward "|")
    (let ((separator (if (org-at-table-hline-p) "+" "|"))
	  (column 1)
	  (shrunk (and (org-table--shrunk-field) (list 1)))
	  (end (line-end-position)))
      (while (search-forward separator end t)
	(cl-incf column)
	(when (org-table--shrunk-field) (push column shrunk)))
      (nreverse shrunk))))

(defun org-table--make-shrinking-overlay (start end display field &optional pre)
  "Create an overlay to shrink text between START and END.

Use string DISPLAY instead of the real text between the two
buffer positions.  FIELD is the real contents of the field, as
a string, or nil.  It is meant to be displayed upon moving the
mouse onto the overlay.

When optional argument PRE is non-nil, assume the overlay is
located at the beginning of the field, and prepend
`org-table--separator-space-pre' to it.  Otherwise, concatenate
`org-table-shrunk-column-indicator' at its end.

Return the overlay."
  (let ((show-before-edit
	 (lambda (o &rest _)
	   ;; Removing one overlay removes all other overlays in the
	   ;; same column.
	   (mapc #'delete-overlay
		 (cdr (overlay-get o 'org-table-column-overlays)))))
	(o (make-overlay start end)))
    (overlay-put o 'insert-behind-hooks (list show-before-edit))
    (overlay-put o 'insert-in-front-hooks (list show-before-edit))
    (overlay-put o 'modification-hooks (list show-before-edit))
    (overlay-put o 'org-overlay-type 'table-column-hide)
    (when (stringp field) (overlay-put o 'help-echo field))
    ;; Make sure overlays stays on top of table coordinates overlays.
    ;; See `org-table-overlay-coordinates'.
    (overlay-put o 'priority 1)
    (let ((d (if pre (concat org-table--separator-space-pre display)
	       (concat display org-table-shrunk-column-indicator))))
      (org-overlay-display o d 'org-table t))
    o))

(defun org-table--shrink-field (width align start end contents)
  "Shrink a table field to a specified width.

WIDTH is an integer representing the number of characters to
display, in addition to `org-table-shrunk-column-indicator'.
ALIGN is the alignment of the current column, as either \"l\",
\"c\" or \"r\".  START and END are, respectively, the beginning
and ending positions of the field.  CONTENTS is its trimmed
contents, as a string, or `hline' for table rules.

Real field is hidden under one or two overlays.  They have the
following properties:

  `org-overlay-type'

    Set to `table-column-hide'.  Used to identify overlays
    responsible for shrinking columns in a table.

  `org-table-column-overlays'

    It is a list with the pattern (siblings . COLUMN-OVERLAYS)
    where COLUMN-OVERLAYS is the list of all overlays hiding the
    same column.

Whenever the text behind or next to the overlay is modified, all
the overlays in the column are deleted, effectively displaying
the column again.

Return a list of overlays hiding the field, or nil if field is
already hidden."
  (cond
   ((= start end) nil)			;no field to narrow
   ((org-table--shrunk-field) nil)	;already shrunk
   ((= 0 width)				;shrink to one character
    (list (org-table--make-shrinking-overlay
	   start end "" (if (eq 'hline contents) "" contents))))
   ((eq contents 'hline)
    (list (org-table--make-shrinking-overlay
	   start end (make-string (1+ width) ?-) "")))
   ((equal contents "")			;no contents to hide
    (list
     (let ((w (org-string-width (buffer-substring start end) nil 'org-table))
	   ;; We really want WIDTH + 2 whitespace, to include blanks
	   ;; around fields.
	   (full (+ 2 width)))
       (if (<= w full)
	   (org-table--make-shrinking-overlay
	    (1- end) end (make-string (- full w) ?\s) "")
	 (org-table--make-shrinking-overlay (- end (- w full) 1) end "" "")))))
   (t
    ;; If the field is not empty, display exactly WIDTH characters.
    ;; It can mean to partly hide the field, or extend it with virtual
    ;; blanks.  To that effect, we use one or two overlays.  The
    ;; first, optional, one may add or hide white spaces before the
    ;; contents of the field.  The other, mandatory, one cuts the
    ;; field or displays white spaces at the end of the field.  It
    ;; also always displays `org-table-shrunk-column-indicator'.
    (let* ((lead (org-with-point-at start (skip-chars-forward " ")))
	   (trail (org-with-point-at end (abs (skip-chars-backward " "))))
	   (contents-width (org-string-width
			    (buffer-substring (+ start lead) (- end trail))
                            nil 'org-table)))
      (cond
       ;; Contents are too large to fit in WIDTH character.  Limit, if
       ;; possible, blanks at the beginning of the field to a single
       ;; white space, and cut the field at an appropriate location.
       ((<= width contents-width)
	(let ((pre
	       (and (> lead 0)
		    (org-table--make-shrinking-overlay
		     start (+ start lead) "" contents t)))
	      (post
	       (org-table--make-shrinking-overlay
		;; Find cut location so that WIDTH characters are
		;; visible using dichotomy.
		(let* ((begin (+ start lead))
		       (lower begin)
		       (upper (1- end))
		       ;; Compensate the absence of leading space,
		       ;; thus preserving alignment.
		       (width (if (= lead 0) (1+ width) width)))
		  (catch :exit
		    (while (> (- upper lower) 1)
		      (let ((mean (+ (ash lower -1)
				     (ash upper -1)
				     (logand lower upper 1))))
			(pcase (org-string-width (buffer-substring begin mean) nil 'org-table)
			  ((pred (= width)) (throw :exit mean))
			  ((pred (< width)) (setq upper mean))
			  (_ (setq lower mean)))))
		    upper))
		end "" contents)))
	  (if pre (list pre post) (list post))))
       ;; Contents fit it WIDTH characters.  First compute number of
       ;; white spaces needed on each side of contents, then expand or
       ;; compact blanks on each side of the field in order to
       ;; preserve width and obey to alignment constraints.
       (t
	(let* ((required (- width contents-width))
	       (before
		(pcase align
		  ;; Compensate the absence of leading space, thus
		  ;; preserving alignment.
		  ((guard (= lead 0)) -1)
		  ("l" 0)
		  ("r" required)
		  ("c" (/ required 2))))
	       (after (- required before))
	       (pre
		(pcase (1- lead)
		  ((or (guard (= lead 0)) (pred (= before))) nil)
		  ((pred (< before))
		   (org-table--make-shrinking-overlay
		    start (+ start (- lead before)) "" contents t))
		  (_
		   (org-table--make-shrinking-overlay
		    start (1+ start)
		    (make-string (- before (1- lead)) ?\s)
		    contents t))))
	       (post
		(pcase (1- trail)
		  ((pred (= after))
		   (org-table--make-shrinking-overlay (1- end) end "" contents))
		  ((pred (< after))
		   (org-table--make-shrinking-overlay
		    (+ after (- end trail)) end "" contents))
		  (_
		   (org-table--make-shrinking-overlay
		    (1- end) end
		    (make-string (- after (1- trail)) ?\s)
		    contents)))))
	  (if pre (list pre post) (list post)))))))))

(defun org-table--read-column-selection (select max)
  "Read column selection select as a list of numbers.

SELECT is a string containing column ranges, separated by white
space characters, see `org-table-toggle-column-width' for details.
MAX is the maximum column number.

Return value is a sorted list of numbers.  Ignore any number
outside of the [1;MAX] range."
  (catch :all
    (sort
     (delete-dups
      (cl-mapcan
       (lambda (s)
	 (cond
	  ((member s '("-" "1-")) (throw :all (number-sequence 1 max)))
	  ((string-match-p "\\`[0-9]+\\'" s)
	   (let ((n (string-to-number s)))
	     (and (> n 0) (<= n max) (list n))))
	  ((string-match "\\`\\([0-9]+\\)?-\\([0-9]+\\)?\\'" s)
	   (let ((n (match-string 1 s))
		 (m (match-string 2 s)))
	     (number-sequence (if n (max 1 (string-to-number n))
				1)
			      (if m (min max (string-to-number m))
				max))))
	  (t nil)))			;invalid specification
       (split-string select)))
     #'<)))

(defun org-table--shrink-columns (columns beg end)
  "Shrink COLUMNS in a table.
COLUMNS is a sorted list of column numbers.  BEG and END are,
respectively, the beginning position and the end position of the
table."
  (org-with-wide-buffer
   (font-lock-ensure beg end)
   (dolist (c columns)
     (goto-char beg)
     (let ((align nil)
	   (width nil)
	   (fields nil))
       (while (< (point) end)
	 (catch :continue
	   (let* ((hline? (org-at-table-hline-p))
		  (separator (if hline? "+" "|")))
	     ;; Move to COLUMN.
	     (search-forward "|")
	     (or (= c 1)		;already there
		 (search-forward separator (line-end-position) t (1- c))
		 (throw :continue nil)) ;skip invalid columns
	     ;; Extract boundaries and contents from current field.
	     ;; Also set the column's width if we encounter a width
	     ;; cookie for the first time.
	     (let* ((start (point))
		    (end (progn
			   (skip-chars-forward (concat "^|" separator)
					       (line-end-position))
			   (point)))
		    (contents (if hline? 'hline
				(org-trim (buffer-substring start end)))))
	       (push (list start end contents) fields)
	       (when (and (not hline?)
			  (string-match "\\`<\\([lrc]\\)?\\([0-9]+\\)>\\'"
					contents))
		 (unless align (setq align (match-string 1 contents)))
		 (unless width
		   (setq width (string-to-number (match-string 2 contents))))))))
	 (forward-line))
       ;; Link overlays for current field to the other overlays in the
       ;; same column.
       (let ((chain (list 'siblings)))
	 (dolist (field fields)
	   (dolist (new (apply #'org-table--shrink-field
			       (or width 0) (or align "l") field))
	     (push new (cdr chain))
	     (overlay-put new 'org-table-column-overlays chain))))))))

;;;###autoload
(defun org-table-toggle-column-width (&optional arg)
  "Shrink or expand current column in an Org table.

If a width cookie specifies a width W for the column, the first
W visible characters are displayed.  Otherwise, the column is
shrunk to a single character.

When point is before the first column or after the last one, ask
for the columns to shrink or expand, as a list of ranges.
A column range can be one of the following patterns:

  N    column N only
  N-M  every column between N and M (both inclusive)
  N-   every column between N (inclusive) and the last column
  -M   every column between the first one and M (inclusive)
  -    every column

When optional argument ARG is a string, use it as white space
separated list of column ranges.

When called with `\\[universal-argument]' prefix, call \
`org-table-shrink', i.e.,
shrink columns with a width cookie and expand the others.

When called with `\\[universal-argument] \\[universal-argument]' \
prefix, expand all columns."
  (interactive "P")
  (unless (org-at-table-p) (user-error "Not in a table"))
  (let* ((begin (org-table-begin))
	 (end (org-table-end))
	 ;; Compute an upper bound for the number of columns.
	 ;; Nonexistent columns are ignored anyway.
	 (max-columns (/ (- (line-end-position) (line-beginning-position)) 2))
	 (shrunk (org-table--list-shrunk-columns))
	 (columns
	  (pcase arg
	    (`nil
	     (if (save-excursion
		   (skip-chars-backward "^|" (line-beginning-position))
		   (or (bolp) (looking-at-p "[ \t]*$")))
		 ;; Point is either before first column or past last
		 ;; one.  Ask for columns to operate on.
		 (org-table--read-column-selection
		  (read-string "Column ranges (e.g. 2-4 6-): ")
		  max-columns)
	       (list (org-table-current-column))))
	    ((pred stringp) (org-table--read-column-selection arg max-columns))
	    ((or `(4) `(16)) nil)
	    (_ (user-error "Invalid argument: %S" arg)))))
    (pcase arg
      (`(4) (org-table-shrink begin end))
      (`(16) (org-table-expand begin end))
      (_
       (org-table-expand begin end)
       (org-table--shrink-columns
	(cl-set-exclusive-or columns shrunk) begin end)))))

;;;###autoload
(defun org-table-shrink (&optional begin end)
  "Shrink all columns with a width cookie in the table at point.

Columns without a width cookie are expanded.

Optional arguments BEGIN and END, when non-nil, specify the
beginning and end position of the current table."
  (interactive)
  (unless (or begin (org-at-table-p)) (user-error "Not at a table"))
  (org-with-wide-buffer
   (let ((begin (or begin (org-table-begin)))
	 (end (or end (org-table-end)))
	 (regexp "|[ \t]*<[lrc]?[0-9]+>[ \t]*\\(|\\|$\\)")
	 (columns))
     (goto-char begin)
     (while (re-search-forward regexp end t)
       (goto-char (match-beginning 1))
       (cl-pushnew (org-table-current-column) columns))
     (org-table-expand begin end)
     ;; Make sure invisible characters in the table are at the right
     ;; place since column widths take them into account.
     (font-lock-ensure begin end)
     (org-table--shrink-columns (sort columns #'<) begin end))))

;;;###autoload
(defun org-table-expand (&optional begin end)
  "Expand all columns in the table at point.
Optional arguments BEGIN and END, when non-nil, specify the
beginning and end position of the current table."
  (interactive)
  (unless (or begin (org-at-table-p)) (user-error "Not at a table"))
  (org-with-wide-buffer
   (let ((begin (or begin (org-table-begin)))
	 (end (or end (org-table-end))))
     (remove-overlays begin end 'org-overlay-type 'table-column-hide))))


;;; Generic Tools

;;;###autoload
(defun org-table-map-tables (f &optional quietly)
  "Apply function F to the start of all tables in the buffer."
  (org-with-point-at 1
    (while (re-search-forward org-table-line-regexp nil t)
      (let ((table (org-element-lineage (org-element-at-point) 'table t)))
	(when table
	  (unless quietly
	    (message "Mapping tables: %d%%"
		     (floor (* 100.0 (point)) (buffer-size))))
	  (goto-char (org-element-post-affiliated table))
	  (let ((end (copy-marker (org-element-end table))))
	    (unwind-protect
		(progn (funcall f) (goto-char end))
	      (set-marker end nil)))))))
  (unless quietly (message "Mapping tables: done")))

;;;###autoload
(defun org-table-export (&optional file format)
  "Export table to a file, with configurable format.
Such a file can be imported into usual spreadsheet programs.

FILE can be the output file name.  If not given, it will be taken
from a TABLE_EXPORT_FILE property in the current entry or higher
up in the hierarchy, or the user will be prompted for a file
name.  FORMAT can be an export format, of the same kind as it
used when `-mode' sends a table in a different format.

The command suggests a format depending on TABLE_EXPORT_FORMAT,
whether it is set locally or up in the hierarchy, then on the
extension of the given file name, and finally on the variable
`org-table-export-default-format'."
  (interactive)
  (unless (org-at-table-p) (user-error "No table at point"))
  (org-table-align)	       ; Make sure we have everything we need.
  (let ((file (or file (org-entry-get (point) "TABLE_EXPORT_FILE" t))))
    (unless file
      (setq file (read-file-name "Export table to: "))
      (unless (or (not (file-exists-p file))
		  (y-or-n-p (format "Overwrite file %s? " file)))
	(user-error "File not written")))
    (when (file-directory-p file)
      (user-error "This is a directory path, not a file"))
    (when (and (buffer-file-name (buffer-base-buffer))
	       (file-equal-p
		(file-truename file)
		(file-truename (buffer-file-name (buffer-base-buffer)))))
      (user-error "Please specify a file name that is different from current"))
    (let ((fileext (concat (file-name-extension file) "$"))
	  (format (or format (org-entry-get (point) "TABLE_EXPORT_FORMAT" t))))
      (unless format
	(let* ((formats '("orgtbl-to-tsv" "orgtbl-to-csv" "orgtbl-to-latex"
			  "orgtbl-to-html" "orgtbl-to-generic"
			  "orgtbl-to-texinfo" "orgtbl-to-orgtbl"
			  "orgtbl-to-unicode"))
	       (deffmt-readable
		 (replace-regexp-in-string
		  "\t" "\\t"
		  (replace-regexp-in-string
		   "\n" "\\n"
		   (or (car (delq nil
				  (mapcar
				   (lambda (f)
				     (and (string-match-p fileext f) f))
				   formats)))
		       org-table-export-default-format)
		   t t)
		  t t)))
	  (setq format
		(org-completing-read
		 "Format: " formats nil nil deffmt-readable))))
      (if (string-match "\\([^ \t\r\n]+\\)\\( +.*\\)?" format)
	  (let ((transform (intern (match-string 1 format)))
		(params (and (match-end 2)
			     (read (concat "(" (match-string 2 format) ")"))))
		(table (org-table-to-lisp)))
	    (unless (fboundp transform)
	      (user-error "No such transformation function %s" transform))
            (with-temp-file file
              (insert (funcall transform table params) "\n"))
	    (message "Export done."))
	(user-error "TABLE_EXPORT_FORMAT invalid")))))

;;;###autoload
(defun org-table--align-field (field width align)
  "Format FIELD according to column WIDTH and alignment ALIGN.
FIELD is a string.  WIDTH is a number.  ALIGN is either \"c\",
\"l\" or\"r\"."
  (let* ((spaces (- width (org-string-width field nil 'org-table)))
	 (prefix (pcase align
		   ("l" "")
		   ("r" (make-string spaces ?\s))
		   ("c" (make-string (/ spaces 2) ?\s))))
	 (suffix (make-string (- spaces (length prefix)) ?\s)))
    (concat org-table--separator-space-pre
	    prefix
	    field
	    suffix
	    org-table--separator-space-post)))

(defun org-table-align ()
  "Align the table at point by aligning all vertical bars."
  (interactive)
  (let ((beg (org-table-begin))
	(end (copy-marker (org-table-end))))
    (org-table-save-field
     ;; Make sure invisible characters in the table are at the right
     ;; place since column widths take them into account.
     (font-lock-ensure beg end)
     (move-marker org-table-aligned-begin-marker beg)
     (move-marker org-table-aligned-end-marker end)
     (goto-char beg)
     (org-table-with-shrunk-columns
      (let* ((table (org-table-to-lisp))
             (rows (remq 'hline table))
	     (widths nil)
	     (alignments nil)
	     (columns-number 1))
	(if (null rows)
	    ;; Table contains only horizontal rules.  Compute the
	    ;; number of columns anyway, and choose an arbitrary width
	    ;; and alignment.
	    (let ((end (line-end-position)))
	      (save-excursion
		(while (search-forward "+" end t)
		  (cl-incf columns-number)))
	      (setq widths (make-list columns-number 1))
	      (setq alignments (make-list columns-number "l")))
	  ;; Compute alignment and width for each column.
	  (setq columns-number (apply #'max (mapcar #'length rows)))
	  (dotimes (i columns-number)
	    (let ((max-width 1)
		  (fixed-align? nil)
		  (numbers 0)
		  (non-empty 0))
	      (dolist (row rows)
		(let ((cell (or (nth i row) "")))
		  (setq max-width (max max-width (org-string-width cell nil 'org-table)))
		  (cond (fixed-align? nil)
			((equal cell "") nil)
			((string-match "\\`<\\([lrc]\\)[0-9]*>\\'" cell)
			 (setq fixed-align? (match-string 1 cell)))
			(t
			 (cl-incf non-empty)
			 (when (string-match-p org-table-number-regexp cell)
			   (cl-incf numbers))))))
	      (push max-width widths)
	      (push (cond
		     (fixed-align?)
		     ((>= numbers (* org-table-number-fraction non-empty)) "r")
		     (t "l"))
		    alignments)))
	  (setq widths (nreverse widths))
	  (setq alignments (nreverse alignments)))
	;; Store alignment of this table, for later editing of single
	;; fields.
	(setq org-table-last-alignment alignments)
	(setq org-table-last-column-widths widths)
	;; Build new table rows.  Only replace rows that actually
	;; changed.
	(let ((rule (and (memq 'hline table)
			 (mapconcat (lambda (w) (make-string (+ 2 w) ?-))
				    widths
				    "+")))
              (indent (progn (looking-at "[ \t]*|") (match-string 0))))
	  (dolist (row table)
	    (let ((previous (buffer-substring (point) (line-end-position)))
		  (new
                   (concat indent
		           (if (eq row 'hline) rule
		             (let* ((offset (- columns-number (length row)))
			            (fields (if (= 0 offset) row
                                              ;; Add missing fields.
				              (append row
						      (make-list offset "")))))
			       (mapconcat #'identity
				          (cl-mapcar #'org-table--align-field
					             fields
					             widths
					             alignments)
				          "|")))
		           "|")))
	      (if (equal new previous)
		  (forward-line)
		(insert new "\n")
		(delete-region (point) (line-beginning-position 2))))))
	(set-marker end nil)
	(when org-table-overlay-coordinates (org-table-overlay-coordinates))
	(setq org-table-may-need-update nil))))))

;;;###autoload
(defun org-table-justify-field-maybe (&optional new)
  "Justify the current field, text to left, number to right.
Optional argument NEW may specify text to replace the current field content."
  ;; FIXME: Prevent newlines inside field.  They are currently not
  ;; supported.
  (when (and (stringp new) (string-match-p "\n" new))
    (message "Removing newlines from formula result: %S" new)
    (setq new (replace-regexp-in-string
               "\n" " "
               (replace-regexp-in-string "\\(^\n+\\)\\|\\(\n+$\\)" "" new))))
  (cond
   ((and (not new) org-table-may-need-update)) ; Realignment will happen anyway
   ((org-at-table-hline-p))
   (t
    (when (or (not (eq (marker-buffer org-table-aligned-begin-marker)
		     (current-buffer)))
	      (< (point) org-table-aligned-begin-marker)
	      (>= (point) org-table-aligned-end-marker))
      ;; This is not the same table, force a full re-align.
      (setq org-table-may-need-update t
            org-table-last-alignment nil
            org-table-last-column-widths nil))
    (when new
      ;; Realign the current field, based on previous full realign.
      (let ((pos (point))
	    (col (org-table-current-column)))
        (when (> col 0)
	  (skip-chars-backward "^|")
	  (if (not (looking-at " *\\(?:\\([^|\n]*?\\) *\\(|\\)\\|\\([^|\n]+?\\) *\\($\\)\\)"))
	      (setq org-table-may-need-update t)
	    (let* ((align (nth (1- col) org-table-last-alignment))
		   (width (nth (1- col) org-table-last-column-widths))
		   (cell (match-string 0))
		   (field (match-string 1))
		   (properly-closed? (/= (match-beginning 2) (match-end 2)))
		   (new-cell
		    (save-match-data
		      (cond (org-table-may-need-update
			     (format " %s |" (or new field)))
			    ((not properly-closed?)
			     (setq org-table-may-need-update t)
			     (format " %s |" (or new field)))
			    ((not new)
			     (concat (org-table--align-field field width align)
				     "|"))
			    ((and width (<= (org-string-width new nil 'org-table) width))
			     (concat (org-table--align-field new width align)
				     "|"))
			    (t
			     (setq org-table-may-need-update t)
			     (format " %s |" new))))))
	      (unless (equal new-cell cell)
	        (let (org-table-may-need-update)
		  (replace-match new-cell t t)))
	      (goto-char pos)))))))))

;;;###autoload
(defun org-table-sort-lines
    (&optional with-case sorting-type getkey-func compare-func interactive?)
  "Sort table lines according to the column at point.

The position of point indicates the column to be used for
sorting, and the range of lines is the range between the nearest
horizontal separator lines, or the entire table of no such lines
exist.  If point is before the first column, you will be prompted
for the sorting column.  If there is an active region, the mark
specifies the first line and the sorting column, while point
should be in the last line to be included into the sorting.

The command then prompts for the sorting type which can be
alphabetically, numerically, or by time (as given in a time stamp
in the field, or as a HH:MM value).  Sorting in reverse order is
also possible.

With prefix argument WITH-CASE, alphabetic sorting will be case-sensitive
if the locale allows for it.

If SORTING-TYPE is specified when this function is called from a Lisp
program, no prompting will take place.  SORTING-TYPE must be a character,
any of (?a ?A ?n ?N ?t ?T ?f ?F) where the capital letters indicate that
sorting should be done in reverse order.

If the SORTING-TYPE is ?f or ?F, then GETKEY-FUNC specifies
a function to be called to extract the key.  It must return a value
that is compatible with COMPARE-FUNC, the function used to compare
entries.

A non-nil value for INTERACTIVE? is used to signal that this
function is being called interactively."
  (interactive (list current-prefix-arg nil nil nil t))
  (when (org-region-active-p) (goto-char (region-beginning)))
  ;; Point must be either within a field or before a data line.
  (save-excursion
    (skip-chars-backward " \t")
    (when (bolp) (search-forward "|" (line-end-position) t))
    (org-table-check-inside-data-field))
  ;; Set appropriate case sensitivity and column used for sorting.
  (let ((column (let ((c (org-table-current-column)))
		  (cond ((> c 0) c)
			(interactive?
			 (read-number "Use column N for sorting: "))
			(t 1))))
	(sorting-type
	 (or sorting-type
	     (read-char-exclusive "Sort Table: [a]lphabetic, [n]umeric, \
\[t]ime, [f]unc.  A/N/T/F means reversed: ")))
	(start (org-table-begin))
	(end (org-table-end)))
    (save-restriction
      ;; Narrow buffer to appropriate sorting area.
      (if (org-region-active-p)
	  (progn (goto-char (region-beginning))
		 (narrow-to-region
		  (point)
		  (save-excursion (goto-char (region-end))
				  (line-beginning-position 2))))
	(narrow-to-region
	 (save-excursion
	   (if (re-search-backward org-table-hline-regexp start t)
	       (line-beginning-position 2)
	     start))
	 (if (save-excursion (re-search-forward org-table-hline-regexp end t))
	     (match-beginning 0)
	   end)))
      ;; Determine arguments for `sort-subr'.  Also record original
      ;; position.  `org-table-save-field' cannot help here since
      ;; sorting is too much destructive.
      (let* ((coordinates
	      (cons (count-lines (point-min) (line-beginning-position))
		    (current-column)))
	     (extract-key-from-field
	      ;; Function to be called on the contents of the field
	      ;; used for sorting in the current row.
	      (cl-case sorting-type
		((?n ?N) #'string-to-number)
		((?a ?A) #'org-sort-remove-invisible)
		((?t ?T)
		 (lambda (f)
		   (cond ((string-match org-ts-regexp-both f)
			  (float-time
			   (org-time-string-to-time (match-string 0 f))))
			 ((org-duration-p f) (org-duration-to-minutes f))
			 ((string-match "\\<[0-9]+:[0-9]\\{2\\}\\>" f)
			  (org-duration-to-minutes (match-string 0 f)))
			 (t 0))))
		((?f ?F)
		 (or getkey-func
		     (and interactive?
			  (org-read-function "Function for extracting keys: "))
		     (error "Missing key extractor to sort rows")))
		(t (user-error "Invalid sorting type `%c'" sorting-type))))
	     (predicate
	      (cl-case sorting-type
		((?n ?N ?t ?T) #'<)
		((?a ?A) (if with-case #'org-string<
			   (lambda (s1 s2) (org-string< s1 s2 nil t))))
		((?f ?F)
		 (or compare-func
		     (and interactive?
			  (org-read-function
			   "Function for comparing keys (empty for default \
`sort-subr' predicate): "
			   'allow-empty))))))
	     (shrunk-columns (remq column (org-table--list-shrunk-columns))))
	(goto-char (point-min))
	(sort-subr (memq sorting-type '(?A ?N ?T ?F))
		   (lambda ()
		     (forward-line)
		     (while (and (not (eobp))
				 (not (looking-at org-table-dataline-regexp)))
		       (forward-line)))
		   #'end-of-line
		   (lambda ()
		     (funcall extract-key-from-field
			      (org-trim (org-table-get-field column))))
		   nil
		   predicate)
	;; Hide all columns but the one being sorted.
	(org-table--shrink-columns shrunk-columns start end)
	;; Move back to initial field.
	(forward-line (car coordinates))
	(move-to-column (cdr coordinates))))))

(defun org-table-transpose-table-at-point ()
  "Transpose Org table at point and eliminate hlines.
So a table like

| 1 | 2 | 4 | 5 |
|---+---+---+---|
| a | b | c | d |
| e | f | g | h |

will be transposed as

| 1 | a | e |
| 2 | b | f |
| 4 | c | g |
| 5 | d | h |

Note that horizontal lines disappear."
  (interactive)
  (let* ((table (delete 'hline (org-table-to-lisp)))
	 (dline_old (org-table-current-line))
	 (col_old (org-table-current-column))
	 (contents (mapcar (lambda (_)
			     (let ((tp table))
			       (mapcar
				(lambda (_)
				  (prog1
				      (pop (car tp))
				    (setq tp (cdr tp))))
				table)))
			   (car table))))
    (goto-char (org-table-begin))
    (re-search-forward "|")
    (backward-char)
    (delete-region (point) (org-table-end))
    (insert (mapconcat
	     (lambda(x)
	       (concat "| " (mapconcat 'identity x " | " ) "  |\n" ))
	     contents ""))
    (org-table-goto-line col_old)
    (org-table-goto-column dline_old))
  (org-table-align))

;;;###autoload
(defun org-table-wrap-region (arg)
  "Wrap several fields in a column like a paragraph.
This is useful if you'd like to spread the contents of a field over several
lines, in order to keep the table compact.

If there is an active region, and both point and mark are in the same column,
the text in the column is wrapped to minimum width for the given number of
lines.  Generally, this makes the table more compact.  A prefix ARG may be
used to change the number of desired lines.  For example, \
`C-2 \\[org-table-wrap-region]'
formats the selected text to two lines.  If the region was longer than two
lines, the remaining lines remain empty.  A negative prefix argument reduces
the current number of lines by that amount.  The wrapped text is pasted back
into the table.  If you formatted it to more lines than it was before, fields
further down in the table get overwritten - so you might need to make space in
the table first.

If there is no region, the current field is split at the cursor position and
the text fragment to the right of the cursor is prepended to the field one
line down.

If there is no region, but you specify a prefix ARG, the current field gets
blank, and the content is appended to the field above."
  (interactive "P")
  (org-table-check-inside-data-field)
  (if (org-region-active-p)
      ;; There is a region: fill as a paragraph.
      (let ((start (region-beginning)))
        (save-restriction
          (narrow-to-region
           (save-excursion (goto-char start) (move-beginning-of-line 1))
           (save-excursion (org-forward-paragraph) (point)))
          (org-table-cut-region (region-beginning) (region-end))
	  (when (> (length (car org-table-clip)) 1)
	    (user-error "Region must be limited to single column"))
	  (let ((nlines (cond ((not arg) (length org-table-clip))
			      ((< arg 1) (+ (length org-table-clip) arg))
			      (t arg))))
	    (setq org-table-clip
		  (mapcar #'list
			  (org-wrap (mapconcat #'car org-table-clip " ")
				    nil
				    nlines))))
	  (goto-char start)
	  (org-table-paste-rectangle))
        (org-table-align))
    ;; No region, split the current field at point.
    (unless (org-get-alist-option org-M-RET-may-split-line 'table)
      (skip-chars-forward "^\r\n|"))
    (cond
     (arg				; Combine with field above.
      (let ((s (org-table-blank-field))
	    (col (org-table-current-column)))
	(forward-line -1)
	(while (org-at-table-hline-p) (forward-line -1))
	(org-table-goto-column col)
	(skip-chars-forward "^|")
	(skip-chars-backward " ")
	(insert " " (org-trim s))
	(org-table-align)))
     ((looking-at "\\([^|]+\\)|")	; Split field.
      (let ((s (match-string 1)))
	(replace-match " |")
	(goto-char (match-beginning 0))
	(org-table-next-row)
	(insert (org-trim s) " ")
	(org-table-align)))
     (t (org-table-next-row)))))

(defun org-table--number-for-summing (s)
  (let (n)
    (if (string-match "^ *|? *" s)
	(setq s (replace-match "" nil nil s)))
    (if (string-match " *|? *$" s)
	(setq s (replace-match "" nil nil s)))
    (setq n (string-to-number s))
    (cond
     ((and (string-match "0" s)
	   (string-match "\\`[-+ \t0.edED]+\\'" s)) 0)
     ((string-match "\\`[ \t]+\\'" s) nil)
     ((string-match "\\`\\([0-9]+\\):\\([0-9]+\\)\\(:\\([0-9]+\\)\\)?\\'" s)
      (let ((h (string-to-number (or (match-string 1 s) "0")))
	    (m (string-to-number (or (match-string 2 s) "0")))
	    (s (string-to-number (or (match-string 4 s) "0"))))
	(if (boundp 'org-timecnt) (setq org-timecnt (1+ org-timecnt)))
	(* 1.0 (+ h (/ m 60.0) (/ s 3600.0)))))
     ((equal n 0) nil)
     (t n))))

;;;###autoload
(defun org-table-sum (&optional beg end nlast)
  "Sum numbers in region of current table column.
The result will be displayed in the echo area, and will be available
as kill to be inserted with \\[yank].

If there is an active region, it is interpreted as a rectangle and all
numbers in that rectangle will be summed.  If there is no active
region and point is located in a table column, sum all numbers in that
column.

If at least one number looks like a time HH:MM or HH:MM:SS, all other
numbers are assumed to be times as well (in decimal hours) and the
numbers are added as such.

If NLAST is a number, only the NLAST fields will actually be summed."
  (interactive)
  (save-excursion
    (let (col (org-timecnt 0) diff h m s org-table-clip)
      (cond
       ((and beg end))			; beg and end given explicitly
       ((org-region-active-p)
	(setq beg (region-beginning) end (region-end)))
       (t
	(setq col (org-table-current-column))
	(goto-char (org-table-begin))
	(unless (re-search-forward "^[ \t]*|[^-]" nil t)
	  (user-error "No table data"))
	(org-table-goto-column col)
	(setq beg (point))
	(goto-char (org-table-end))
	(unless (re-search-backward "^[ \t]*|[^-]" nil t)
	  (user-error "No table data"))
	(org-table-goto-column col)
	(setq end (point))))
      (let* ((items (apply 'append (org-table-copy-region beg end)))
	     (items1 (cond ((not nlast) items)
			   ((>= nlast (length items)) items)
			   (t (setq items (reverse items))
			      (setcdr (nthcdr (1- nlast) items) nil)
			      (nreverse items))))
	     (numbers (delq nil (mapcar #'org-table--number-for-summing
					items1)))
	     (res (apply '+ numbers))
	     (sres (if (= org-timecnt 0)
		       (number-to-string res)
		     (setq diff (* 3600 res)
			   h (floor diff 3600) diff (mod diff 3600)
			   m (floor diff 60) diff (mod diff 60)
			   s diff)
		     (format "%.0f:%02.0f:%02.0f" h m s))))
	(kill-new sres)
	(when (called-interactively-p 'interactive)
	  (message (substitute-command-keys
		    (format "Sum of %d items: %-20s     \
\(\\[yank] will insert result into buffer)"
			    (length numbers)
			    sres))))
	sres))))

;;;###autoload
(defun org-table-analyze ()
  "Analyze table at point and store results.

This function sets up the following dynamically scoped variables:

 `org-table-column-name-regexp',
 `org-table-column-names',
 `org-table-current-begin-pos',
 `org-table-current-line-types',
 `org-table-current-ncol',
 `org-table-dlines',
 `org-table-hlines',
 `org-table-local-parameters',
 `org-table-named-field-locations'."
  (let ((beg (org-table-begin))
	(end (org-table-end)))
    (save-excursion
      (goto-char beg)
      ;; Extract column names.
      (setq org-table-column-names nil)
      (when (save-excursion
	      (re-search-forward "^[ \t]*| *! *\\(|.*\\)" end t))
	(let ((c 1))
	  (dolist (name (org-split-string (match-string 1) " *| *"))
	    (cl-incf c)
	    (when (string-match "\\`[a-zA-Z][_a-zA-Z0-9]*\\'" name)
	      (push (cons name (number-to-string c)) org-table-column-names)))))
      (setq org-table-column-names (nreverse org-table-column-names))
      (setq org-table-column-name-regexp
	    (format "\\$\\(%s\\)\\>"
		    (regexp-opt (mapcar #'car org-table-column-names) t)))
      ;; Extract local parameters.
      (setq org-table-local-parameters nil)
      (save-excursion
	(while (re-search-forward "^[ \t]*| *\\$ *\\(|.*\\)" end t)
	  (dolist (field (org-split-string (match-string 1) " *| *"))
	    (when (string-match
		   "\\`\\([a-zA-Z][_a-zA-Z0-9]*\\|%\\) *= *\\(.*\\)" field)
	      (push (cons (match-string 1 field) (match-string 2 field))
		    org-table-local-parameters)))))
      ;; Update named fields locations.  We minimize `count-lines'
      ;; processing by storing last known number of lines in LAST.
      (setq org-table-named-field-locations nil)
      (save-excursion
	(let ((last (cons (point) 0)))
	  (while (re-search-forward "^[ \t]*| *\\([_^]\\) *\\(|.*\\)" end t)
	    (let ((c (match-string 1))
		  (fields (org-split-string (match-string 2) " *| *")))
	      (save-excursion
		(forward-line (if (equal c "_") 1 -1))
		(let ((fields1
		       (and (looking-at "^[ \t]*|[^|]*\\(|.*\\)")
			    (org-split-string (match-string 1) " *| *")))
		      (line (cl-incf (cdr last) (count-lines (car last) (point))))
		      (col 1))
		  (setcar last (point))	; Update last known position.
		  (while (and fields fields1)
		    (let ((field (pop fields))
			  (v (pop fields1)))
		      (cl-incf col)
		      (when (and (stringp field)
				 (stringp v)
				 (string-match "\\`[a-zA-Z][_a-zA-Z0-9]*\\'"
					       field))
			(push (cons field v) org-table-local-parameters)
			(push (list field line col)
			      org-table-named-field-locations))))))))))
      ;; Reuse existing markers when possible.
      (if (markerp org-table-current-begin-pos)
	  (move-marker org-table-current-begin-pos (point))
	(setq org-table-current-begin-pos (point-marker)))
      ;; Analyze the line types.
      (let ((l 0) hlines dlines types)
	(while (looking-at "[ \t]*|\\(-\\)?")
	  (push (if (match-end 1) 'hline 'dline) types)
	  (if (match-end 1) (push l hlines) (push l dlines))
	  (forward-line)
	  (cl-incf l))
	(push 'hline types) ; Add an imaginary extra hline to the end.
	(setq org-table-current-line-types (apply #'vector (nreverse types)))
	(setq org-table-dlines (apply #'vector (cons nil (nreverse dlines))))
	(setq org-table-hlines (apply #'vector (cons nil (nreverse hlines)))))
      ;; Get the number of columns from the first data line in table.
      (goto-char beg)
      (forward-line (aref org-table-dlines 1))
      (setq org-table-current-ncol
	    (length (org-split-string
		     (buffer-substring (line-beginning-position) (line-end-position))
		     "[ \t]*|[ \t]*"))))))

(defun org-table--force-dataline ()
  "Move point to the closest data line in a table.
Raise an error if the table contains no data line.  Preserve
column when moving point."
  (unless (org-match-line org-table-dataline-regexp)
    (let* ((re org-table-dataline-regexp)
	   (column (current-column))
	   (p1 (save-excursion (re-search-forward re (org-table-end) t)))
	   (p2 (save-excursion (re-search-backward re (org-table-begin) t))))
      (cond ((and p1 p2)
	     (goto-char (if (< (abs (- p1 (point))) (abs (- p2 (point))))
			    p1
			  p2)))
	    ((or p1 p2) (goto-char (or p1 p2)))
	    (t (user-error "No table data line around here")))
      (org-move-to-column column))))

(defun org-table-show-reference (&optional local)
  "Show the location/value of the $ expression at point.
When LOCAL is non-nil, show references for the table at point."
  (interactive)
  (org-table-remove-rectangle-highlight)
  (when local (org-table-analyze))
  (catch 'exit
    (let ((pos (if local (point) org-pos))
	  (face2 'highlight)
	  (org-inhibit-highlight-removal t)
	  (win (selected-window))
	  (org-show-positions nil)
	  var name e what match dest)
      (setq what (cond
		  ((org-in-regexp "^@[0-9]+[ \t=]")
		   (setq match (concat (substring (match-string 0) 0 -1)
				       "$1.."
				       (substring (match-string 0) 0 -1)
				       "$100"))
		   'range)
		  ((or (org-in-regexp org-table-range-regexp2)
		       (org-in-regexp org-table-translate-regexp)
		       (org-in-regexp org-table-range-regexp))
		   (setq match
			 (save-match-data
			   (org-table-convert-refs-to-rc (match-string 0))))
		   'range)
		  ((org-in-regexp "\\$[a-zA-Z][a-zA-Z0-9]*") 'name)
		  ((org-in-regexp "\\$[0-9]+") 'column)
		  ((not local) nil)
		  (t (user-error "No reference at point")))
	    match (and what (or match (match-string 0))))
      (when (and  match (not (equal (match-beginning 0) (line-beginning-position))))
	(org-table-add-rectangle-overlay (match-beginning 0) (match-end 0)
					 'secondary-selection))
      (add-hook 'before-change-functions
		#'org-table-remove-rectangle-highlight)
      (when (eq what 'name) (setq var (substring match 1)))
      (when (eq what 'range)
	(unless (eq (string-to-char match) ?@) (setq match (concat "@" match)))
	(setq match (org-table-formula-substitute-names match)))
      (unless local
	(save-excursion
	  (end-of-line)
	  (re-search-backward "^\\S-" nil t)
	  (forward-line 0)
	  (when (looking-at "\\(\\$[0-9a-zA-Z]+\\|@[0-9]+\\$[0-9]+\\|[a-zA-Z]+\
\\([0-9]+\\|&\\)\\) *=")
	    (setq dest
		  (save-match-data
		    (org-table-convert-refs-to-rc (match-string 1))))
	    (org-table-add-rectangle-overlay
	     (match-beginning 1) (match-end 1) face2))))
      (if (and (markerp pos) (marker-buffer pos))
	  (if (get-buffer-window (marker-buffer pos))
	      (select-window (get-buffer-window (marker-buffer pos)))
	    (switch-to-buffer-other-window (get-buffer-window
					    (marker-buffer pos)))))
      (goto-char pos)
      (org-table--force-dataline)
      (let ((table-start
	     (if local org-table-current-begin-pos (org-table-begin))))
	(when dest
	  (setq name (substring dest 1))
	  (cond
	   ((string-match-p "\\`\\$[a-zA-Z][a-zA-Z0-9]*" dest)
	    (org-table-goto-field dest))
	   ((string-match-p "\\`@\\([1-9][0-9]*\\)\\$\\([1-9][0-9]*\\)\\'"
			    dest)
	    (org-table-goto-field dest))
	   (t (org-table-goto-column (string-to-number name))))
	  (move-marker pos (point))
	  (org-table-highlight-rectangle nil nil face2))
	(cond
	 ((equal dest match))
	 ((not match))
	 ((eq what 'range)
	  (ignore-errors (org-table-get-range match table-start nil 'highlight)))
	 ((setq e (assoc var org-table-named-field-locations))
	  (org-table-goto-field var)
	  (org-table-highlight-rectangle)
	  (message "Named field, column %d of line %d" (nth 2 e) (nth 1 e)))
	 ((setq e (assoc var org-table-column-names))
	  (org-table-goto-column (string-to-number (cdr e)))
	  (org-table-highlight-rectangle)
	  (goto-char table-start)
	  (if (re-search-forward (concat "^[ \t]*| *! *.*?| *\\(" var "\\) *|")
				 (org-table-end) t)
	      (progn
		(goto-char (match-beginning 1))
		(org-table-highlight-rectangle)
		(message "Named column (column %s)" (cdr e)))
	    (user-error "Column name not found")))
	 ((eq what 'column)
	  ;; Column number.
	  (org-table-goto-column (string-to-number (substring match 1)))
	  (org-table-highlight-rectangle)
	  (message "Column %s" (substring match 1)))
	 ((setq e (assoc var org-table-local-parameters))
	  (goto-char table-start)
	  (if (re-search-forward (concat "^[ \t]*| *\\$ *.*?| *\\(" var "=\\)") nil t)
	      (progn
		(goto-char (match-beginning 1))
		(org-table-highlight-rectangle)
		(message "Local parameter."))
	    (user-error "Parameter not found")))
	 ((not var) (user-error "No reference at point"))
	 ((setq e (assoc var org-table-formula-constants-local))
	  (message "Local Constant: $%s=%s in #+CONSTANTS line."
		   var (cdr e)))
	 ((setq e (assoc var org-table-formula-constants))
	  (message "Constant: $%s=%s in `org-table-formula-constants'."
		   var (cdr e)))
	 ((setq e (and (fboundp 'constants-get) (constants-get var)))
	  (message "Constant: $%s=%s, from `constants.el'%s."
		   var e (format " (%s units)" constants-unit-system)))
	 (t (user-error "Undefined name $%s" var)))
	(goto-char pos)
	(when (and org-show-positions
		   (not (memq this-command '(org-table-fedit-scroll
					     org-table-fedit-scroll-down))))
	  (push pos org-show-positions)
	  (push table-start org-show-positions)
	  (let ((min (apply 'min org-show-positions))
		(max (apply 'max org-show-positions)))
	    (set-window-start (selected-window) min)
	    (goto-char max)
	    (or (pos-visible-in-window-p max)
		(set-window-start (selected-window) max)))))
      (select-window win))))


;;; The Orgtbl minor mode

;; Define a minor mode which can be used in other modes in order to
;; integrate the Org table editor.

;; This is really a hack, because the Org table editor uses several
;; keys which normally belong to the major mode, for example the TAB
;; and RET keys.  Here is how it works: The minor mode defines all the
;; keys necessary to operate the table editor, but wraps the commands
;; into a function which tests if the cursor is currently inside
;; a table.  If that is the case, the table editor command is
;; executed.  However, when any of those keys is used outside a table,
;; the function uses `key-binding' to look up if the key has an
;; associated command in another currently active keymap (minor modes,
;; major mode, global), and executes that command.  There might be
;; problems if any of the keys used by the table editor is otherwise
;; used as a prefix key.

;; Another challenge is that the key binding for TAB can be tab or \C-i,
;; likewise the binding for RET can be return or \C-m.  Orgtbl-mode
;; addresses this by checking explicitly for both bindings.

;; The optimized version (see variable `orgtbl-optimized') takes over
;; all keys which are bound to `self-insert-command' in the *global map*.
;; Some modes bind other commands to simple characters, for example
;; AUCTeX binds the double quote to `Tex-insert-quote'.  With orgtbl-mode
;; active, this binding is ignored inside tables and replaced with a
;; modified self-insert.

(defvar orgtbl-mode-map (make-keymap)
  "Keymap for `orgtbl-mode'.")

(defvar org-old-auto-fill-inhibit-regexp nil
  "Local variable used by `orgtbl-mode'.")

(defconst orgtbl-line-start-regexp
  "[ \t]*\\(|\\|#\\+\\(tblfm\\|orgtbl\\|tblname\\):\\)"
  "Matches a line belonging to an orgtbl.")

(defconst orgtbl-extra-font-lock-keywords
  (list (list (concat "^" orgtbl-line-start-regexp ".*")
	      0 (quote 'org-table) 'prepend))
  "Extra `font-lock-keywords' to be added when `orgtbl-mode' is active.")

;;;###autoload
(defun turn-on-orgtbl ()
  "Unconditionally turn on `orgtbl-mode'."
  (require 'org-table)
  (orgtbl-mode 1))

;; Install it as a minor mode.
(put 'orgtbl-mode :included t)
(put 'orgtbl-mode :menu-tag "Org Table Mode")

(easy-menu-define orgtbl-mode-menu orgtbl-mode-map "OrgTbl menu."
  '("OrgTbl"
    ["Create or convert" org-table-create-or-convert-from-region
     :active (not (org-at-table-p)) :keys "C-c |" ]
    "--"
    ["Align" org-ctrl-c-ctrl-c :active (org-at-table-p) :keys "C-c C-c"]
    ["Next Field" org-cycle :active (org-at-table-p) :keys "TAB"]
    ["Previous Field" org-shifttab :active (org-at-table-p) :keys "S-TAB"]
    ["Next Row" org-return :active (org-at-table-p) :keys "RET"]
    "--"
    ["Blank Field" org-table-blank-field :active (org-at-table-p) :keys "C-c SPC"]
    ["Edit Field" org-table-edit-field :active (org-at-table-p) :keys "C-c ` "]
    ["Copy Field from Above"
     org-table-copy-down :active (org-at-table-p) :keys "S-RET"]
    "--"
    ("Column"
     ["Move Column Left" org-metaleft :active (org-at-table-p) :keys "M-<left>"]
     ["Move Column Right" org-metaright :active (org-at-table-p) :keys "M-<right>"]
     ["Delete Column" org-shiftmetaleft :active (org-at-table-p) :keys "M-S-<left>"]
     ["Insert Column" org-shiftmetaright :active (org-at-table-p) :keys "M-S-<right>"])
    ("Row"
     ["Move Row Up" org-metaup :active (org-at-table-p) :keys "M-<up>"]
     ["Move Row Down" org-metadown :active (org-at-table-p) :keys "M-<down>"]
     ["Delete Row" org-shiftmetaup :active (org-at-table-p) :keys "M-S-<up>"]
     ["Insert Row" org-shiftmetadown :active (org-at-table-p) :keys "M-S-<down>"]
     ["Sort lines in region" org-table-sort-lines :active (org-at-table-p) :keys "C-c ^"]
     "--"
     ["Insert Hline" org-table-insert-hline :active (org-at-table-p) :keys "C-c -"])
    ("Rectangle"
     ["Copy Rectangle" org-copy-special :active (org-at-table-p)]
     ["Cut Rectangle" org-cut-special :active (org-at-table-p)]
     ["Paste Rectangle" org-paste-special :active (org-at-table-p)]
     ["Fill Rectangle" org-table-wrap-region :active (org-at-table-p)])
    "--"
    ("Radio tables"
     ["Insert table template" orgtbl-insert-radio-table
      (cl-assoc-if #'derived-mode-p orgtbl-radio-table-templates)]
     ["Comment/uncomment table" orgtbl-toggle-comment t])
    "--"
    ["Set Column Formula" org-table-eval-formula :active (org-at-table-p) :keys "C-c ="]
    ["Set Field Formula" (org-table-eval-formula '(4)) :active (org-at-table-p) :keys "C-u C-c ="]
    ["Edit Formulas" org-table-edit-formulas :active (org-at-table-p) :keys "C-c '"]
    ["Recalculate line" org-table-recalculate :active (org-at-table-p) :keys "C-c *"]
    ["Recalculate all" (org-table-recalculate '(4)) :active (org-at-table-p) :keys "C-u C-c *"]
    ["Iterate all" (org-table-recalculate '(16)) :active (org-at-table-p) :keys "C-u C-u C-c *"]
    ["Toggle Recalculate Mark" org-table-rotate-recalc-marks :active (org-at-table-p) :keys "C-c #"]
    ["Sum Column/Rectangle" org-table-sum
     :active (or (org-at-table-p) (org-region-active-p)) :keys "C-c +"]
    ["Which Column?" org-table-current-column :active (org-at-table-p) :keys "C-c ?"]
    ["Debug Formulas"
     org-table-toggle-formula-debugger :active (org-at-table-p)
     :keys "C-c {"
     :style toggle :selected org-table-formula-debug]
    ["Show Col/Row Numbers"
     org-table-toggle-coordinate-overlays :active (org-at-table-p)
     :keys "C-c }"
     :style toggle :selected org-table-overlay-coordinates]
    "--"
    ("Plot"
     ["Ascii plot" orgtbl-ascii-plot :active (org-at-table-p) :keys "C-c \" a"]
     ["Gnuplot" org-plot/gnuplot :active (org-at-table-p) :keys "C-c \" g"])))

;;;###autoload
(define-minor-mode orgtbl-mode
  "The Org mode table editor as a minor mode for use in other modes."
  :lighter " OrgTbl"
  (org-load-modules-maybe)
  (cond
   ((derived-mode-p 'org-mode)
    ;; Exit without error, in case some hook functions calls this by
    ;; accident in Org mode.
    (message "Orgtbl mode is not useful in Org mode, command ignored"))
   (orgtbl-mode
    (orgtbl-setup)
    ;; Make sure we are first in minor-mode-map-alist
    (let ((c (assq 'orgtbl-mode minor-mode-map-alist)))
      ;; FIXME: maybe it should use emulation-mode-map-alists?
      (and c (setq minor-mode-map-alist
                   (cons c (delq c minor-mode-map-alist)))))
    (setq-local org-table-may-need-update t)
    (add-hook 'before-change-functions 'org-before-change-function
	      nil 'local)
    (setq-local org-old-auto-fill-inhibit-regexp
		auto-fill-inhibit-regexp)
    (setq-local auto-fill-inhibit-regexp
		(if auto-fill-inhibit-regexp
		    (concat orgtbl-line-start-regexp "\\|"
			    auto-fill-inhibit-regexp)
		  orgtbl-line-start-regexp))
    (font-lock-add-keywords nil orgtbl-extra-font-lock-keywords)
    (org-restart-font-lock))
   (t
    (setq auto-fill-inhibit-regexp org-old-auto-fill-inhibit-regexp)
    (remove-hook 'before-change-functions 'org-before-change-function t)
    (font-lock-remove-keywords nil orgtbl-extra-font-lock-keywords)
    (org-restart-font-lock)
    (force-mode-line-update 'all))))

(defun orgtbl-make-binding (fun n &rest keys)
  "Create a function for binding in the table minor mode.
FUN is the command to call inside a table.  N is used to create a unique
command name.  KEYS are keys that should be checked in for a command
to execute outside of tables."
  (eval
   (list 'defun
	 (intern (concat "orgtbl-hijacker-command-" (number-to-string n)))
	 '(arg)
	 (concat "In tables, run `" (symbol-name fun) "'.\n"
		 "Outside of tables, run the binding of `"
		 (mapconcat #'key-description keys "' or `")
		 "'.")
	 '(interactive "p")
	 (list 'if
	       '(org-at-table-p)
	       (list 'call-interactively (list 'quote fun))
	       (list 'let '(orgtbl-mode)
		     (list 'call-interactively
			   (append '(or)
				   (mapcar (lambda (k)
					     (list 'key-binding k))
					   keys)
				   '('orgtbl-error))))))))

(defun orgtbl-error ()
  "Error when there is no default binding for a table key."
  (interactive)
  (user-error "This key has no function outside tables"))

;; Fill in orgtbl keymap.
(let ((nfunc 0)
      (bindings
       '(([(meta shift left)]  org-table-delete-column)
	 ([(meta left)]	 org-table-move-column-left)
	 ([(meta right)]       org-table-move-column-right)
	 ([(meta shift right)] org-table-insert-column)
	 ([(meta shift up)]    org-table-kill-row)
	 ([(meta shift down)]  org-table-insert-row)
	 ([(meta up)]		 org-table-move-row-up)
	 ([(meta down)]	 org-table-move-row-down)
	 ("\C-c\C-w"		 org-table-cut-region)
	 ("\C-c\M-w"		 org-table-copy-region)
	 ("\C-c\C-y"		 org-table-paste-rectangle)
	 ("\C-c\C-w"           org-table-wrap-region)
	 ("\C-c-"		 org-table-insert-hline)
	 ("\C-c}"		 org-table-toggle-coordinate-overlays)
	 ("\C-c{"		 org-table-toggle-formula-debugger)
	 ("\C-m"		 org-table-next-row)
	 ([(shift return)]	 org-table-copy-down)
	 ("\C-c?"		 org-table-field-info)
	 ("\C-c "		 org-table-blank-field)
	 ("\C-c+"		 org-table-sum)
	 ("\C-c="		 org-table-eval-formula)
	 ("\C-c'"		 org-table-edit-formulas)
	 ("\C-c`"		 org-table-edit-field)
	 ("\C-c*"		 org-table-recalculate)
	 ("\C-c^"		 org-table-sort-lines)
	 ("\M-a"		 org-table-beginning-of-field)
	 ("\M-e"		 org-table-end-of-field)
	 ([(control ?#)]       org-table-rotate-recalc-marks)))
      elt key fun cmd)
  (while (setq elt (pop bindings))
    (setq nfunc (1+ nfunc))
    (setq key (org-key (car elt))
	  fun (nth 1 elt)
	  cmd (orgtbl-make-binding fun nfunc key))
    (org-defkey orgtbl-mode-map key cmd))

  ;; Special treatment needed for TAB, RET and DEL
  (org-defkey orgtbl-mode-map [(return)]
	      (orgtbl-make-binding 'orgtbl-ret 100 [(return)] "\C-m"))
  (org-defkey orgtbl-mode-map "\C-m"
	      (orgtbl-make-binding 'orgtbl-ret 101 "\C-m" [(return)]))
  (org-defkey orgtbl-mode-map [(tab)]
	      (orgtbl-make-binding 'orgtbl-tab 102 [(tab)] "\C-i"))
  (org-defkey orgtbl-mode-map "\C-i"
	      (orgtbl-make-binding 'orgtbl-tab 103 "\C-i" [(tab)]))
  (org-defkey orgtbl-mode-map [(shift tab)]
	      (orgtbl-make-binding 'org-table-previous-field 104
				   [(shift tab)] [(tab)] "\C-i"))
  (org-defkey orgtbl-mode-map [backspace]
	      (orgtbl-make-binding 'org-delete-backward-char 109
				   [backspace] (kbd "DEL")))

  (org-defkey orgtbl-mode-map [S-iso-lefttab]
	      (orgtbl-make-binding 'org-table-previous-field 107
				   [S-iso-lefttab] [backtab] [(shift tab)]
				   [(tab)] "\C-i"))

  (org-defkey orgtbl-mode-map [backtab]
	      (orgtbl-make-binding 'org-table-previous-field 108
				   [backtab] [S-iso-lefttab] [(shift tab)]
				   [(tab)] "\C-i"))

  (org-defkey orgtbl-mode-map "\M-\C-m"
	      (orgtbl-make-binding 'org-table-wrap-region 105
				   "\M-\C-m" [(meta return)]))
  (org-defkey orgtbl-mode-map [(meta return)]
	      (orgtbl-make-binding 'org-table-wrap-region 106
				   [(meta return)] "\M-\C-m"))

  (org-defkey orgtbl-mode-map "\C-c\C-c" 'orgtbl-ctrl-c-ctrl-c)
  (org-defkey orgtbl-mode-map "\C-c|" 'orgtbl-create-or-convert-from-region))

(defun orgtbl-setup ()
  "Setup orgtbl keymaps."
  ;; If the user wants maximum table support, we need to hijack
  ;; some standard editing functions
  (org-remap orgtbl-mode-map
	     'self-insert-command (and orgtbl-optimized 'orgtbl-self-insert-command)
	     'delete-char (and orgtbl-optimized 'org-delete-char)
             'delete-forward-char (and orgtbl-optimized 'org-delete-char)
	     'delete-backward-char (and orgtbl-optimized 'org-delete-backward-char))
  (org-defkey orgtbl-mode-map "|" (and orgtbl-optimized 'org-force-self-insert)))

(defun orgtbl-ctrl-c-ctrl-c (arg)
  "If the cursor is inside a table, realign the table.
If it is a table to be sent away to a receiver, do it.
With prefix arg, also recompute table."
  (interactive "P")
  (let ((case-fold-search t) (pos (point)) action)
    (save-excursion
      (forward-line 0)
      (setq action (cond
		    ((looking-at "[ \t]*#\\+ORGTBL:.*\n[ \t]*|") (match-end 0))
		    ((looking-at "[ \t]*|") pos)
		    ((looking-at "[ \t]*#\\+tblfm:") 'recalc))))
    (cond
     ((integerp action)
      (goto-char action)
      (org-table-maybe-eval-formula)
      (if arg
	  (call-interactively 'org-table-recalculate)
	(org-table-maybe-recalculate-line))
      (call-interactively 'org-table-align)
      (when (orgtbl-send-table 'maybe)
	(run-hooks 'orgtbl-after-send-table-hook)))
     ((eq action 'recalc)
      (save-excursion
	(forward-line 0)
	(skip-chars-backward " \r\n\t")
	(if (org-at-table-p)
	    (org-call-with-arg 'org-table-recalculate t))))
     (t (let (orgtbl-mode)
	  (call-interactively (key-binding "\C-c\C-c")))))))

(defun orgtbl-create-or-convert-from-region (_arg)
  "Create table or convert region to table, if no conflicting binding.
This installs the table binding `C-c |', but only if there is no
conflicting binding to this key outside `orgtbl-mode'."
  (interactive "P")
  (let* (orgtbl-mode (cmd (key-binding "\C-c|")))
    (if cmd
	(call-interactively cmd)
      (call-interactively 'org-table-create-or-convert-from-region))))

(defun orgtbl-tab (arg)
  "Justification and field motion for `orgtbl-mode'."
  (interactive "P")
  (if arg (org-table-edit-field t)
    (org-table-justify-field-maybe)
    (org-table-next-field)))

(defun orgtbl-ret ()
  "Justification and field motion for `orgtbl-mode'."
  (interactive)
  (if (bobp)
      (newline)
    (org-table-justify-field-maybe)
    (org-table-next-row)))

(defun orgtbl-self-insert-command (N)
  "Like `self-insert-command', use `overwrite-mode' for whitespace in tables.
If the cursor is in a table looking at whitespace, the whitespace is
overwritten, and the table is not marked as requiring realignment."
  (interactive "p")
  (if (and (org-at-table-p)
	   (or
	    (and org-table-auto-blank-field
		 (member last-command
			 '(orgtbl-hijacker-command-100
			   orgtbl-hijacker-command-101
			   orgtbl-hijacker-command-102
			   orgtbl-hijacker-command-103
			   orgtbl-hijacker-command-104
			   orgtbl-hijacker-command-105
			   yas/expand))
		 (org-table-blank-field))
	    t)
	   (eq N 1)
	   (looking-at "[^|\n]* \\( \\)|"))
      (let (org-table-may-need-update)
	(delete-region (match-beginning 1) (match-end 1))
	(self-insert-command N))
    (setq org-table-may-need-update t)
    (let* (orgtbl-mode
	   (cmd (or (key-binding
		     (or (and (listp function-key-map)
			      (cdr (assoc last-command-event function-key-map)))
			 (vector last-command-event)))
		    'self-insert-command)))
      (call-interactively cmd)
      (if (and org-self-insert-cluster-for-undo
	       (eq cmd 'self-insert-command))
	  (if (not (eq last-command 'orgtbl-self-insert-command))
	      (setq org-self-insert-command-undo-counter 1)
	    (if (>= org-self-insert-command-undo-counter 20)
		(setq org-self-insert-command-undo-counter 1)
	      (and (> org-self-insert-command-undo-counter 0)
		   buffer-undo-list
		   (not (cadr buffer-undo-list)) ; remove nil entry
		   (setcdr buffer-undo-list (cddr buffer-undo-list)))
	      (setq org-self-insert-command-undo-counter
		    (1+ org-self-insert-command-undo-counter))))))))

;;;###autoload
(defvar orgtbl-exp-regexp "^\\([-+]?[0-9][0-9.]*\\)[eE]\\([-+]?[0-9]+\\)$"
  "Regular expression matching exponentials as produced by calc.")

(defun orgtbl-gather-send-defs ()
  "Gather a plist of :name, :transform, :params for each destination before
a radio table."
  (save-excursion
    (goto-char (org-table-begin))
    (let (rtn)
      (forward-line -1)
      (catch :bob
        (while (looking-at "[ \t]*#\\+ORGTBL[: \t][ \t]*SEND[ \t]+\\([^ \t\r\n]+\\)[ \t]+\\([^ \t\r\n]+\\)\\([ \t]+.*\\)?")
	  (let ((name (org-no-properties (match-string 1)))
	        (transform (intern (match-string 2)))
	        (params (if (match-end 3)
			    (read (concat "(" (match-string 3) ")")))))
	    (push (list :name name :transform transform :params params)
		  rtn)
            (when (bobp) (throw :bob nil))
	    (forward-line -1))))
      rtn)))

(defun orgtbl-send-replace-tbl (name text)
  "Find and replace table NAME with TEXT."
  (save-excursion
    (goto-char (point-min))
    (let* ((location-flag nil)
	   (name (regexp-quote name))
	   (begin-re (format "BEGIN +RECEIVE +ORGTBL +%s\\([ \t]\\|$\\)" name))
	   (end-re (format "END +RECEIVE +ORGTBL +%s\\([ \t]\\|$\\)" name)))
      (while (re-search-forward begin-re nil t)
	(unless location-flag (setq location-flag t))
	(let ((beg (line-beginning-position 2)))
	  (unless (re-search-forward end-re nil t)
	    (user-error "Cannot find end of receiver location at %d" beg))
	  (forward-line 0)
	  (delete-region beg (point))
	  (insert text "\n")))
      (unless location-flag
	(user-error "No valid receiver location found in the buffer")))))

;;;###autoload
(defun org-table-to-lisp (&optional txt)
  "Convert the table at point to a Lisp structure.

The structure will be a list.  Each item is either the symbol `hline'
for a horizontal separator line, or a list of field values as strings.
The table is taken from the parameter TXT, or from the buffer at point."
  (if txt
      (with-temp-buffer
	(buffer-disable-undo)
        (insert txt)
        (goto-char (point-min))
        (org-table-to-lisp))
    (save-excursion
      (goto-char (org-table-begin))
      (let (table)
        (while (progn (skip-chars-forward " \t")
                      (eq (following-char) ?|))
	  (forward-char)
	  (push
	   (if (eq (following-char) ?-)
	       'hline
	     (let (row)
	       (while (progn
                        (skip-chars-forward " \t")
                        (not (eolp)))
                 (let ((q (point)))
                   (skip-chars-forward "^|\n")
                   (goto-char
                    (prog1
                        (let ((p (point)))
                          (unless (eolp) (setq p (1+ p)))
                          p)
	              (skip-chars-backward " \t" q)
                      ;; Preserve text properties.  They are used when
                      ;; calculating cell width.
	              (push (buffer-substring q (point)) row)))))
	       (nreverse row)))
	   table)
	  (forward-line))
	(nreverse table)))))

(defun org-table-collapse-header (table &optional separator max-header-lines)
  "Collapse the lines before `hline' into a single header.

The given TABLE is a list of lists as returned by `org-table-to-lisp'.
The leading lines before the first `hline' symbol are considered
forming the table header.  This function collapses all leading header
lines into a single header line, followed by the `hline' symbol, and
the rest of the TABLE.  Header cells are glued together with a space,
or the given SEPARATOR."
  (while (eq (car table) 'hline) (pop table))
  (let* ((separator (or separator " "))
	 (max-header-lines (or max-header-lines 4))
	 (trailer table)
	 (header-lines (cl-loop for line in table
				until (eq 'hline line)
				collect (pop trailer))))
    (if (and trailer (<= (length header-lines) max-header-lines))
	(cons (apply #'cl-mapcar
		     (lambda (&rest x)
		       (org-trim
			(mapconcat #'identity x separator)))
		     header-lines)
	      trailer)
      table)))

(defun orgtbl-send-table (&optional maybe)
  "Send a transformed version of table at point to the receiver position.
With argument MAYBE, fail quietly if no transformation is defined
for this table."
  (interactive)
  (catch 'exit
    (unless (org-at-table-p) (user-error "Not at a table"))
    ;; when non-interactive, we assume align has just happened.
    (when (called-interactively-p 'any) (org-table-align))
    (let ((dests (orgtbl-gather-send-defs))
	  (table (org-table-to-lisp))
	  (ntbl 0))
      (unless dests
	(if maybe (throw 'exit nil)
	  (user-error "Don't know how to transform this table")))
      (dolist (dest dests)
	(let ((name (plist-get dest :name))
	      (transform (plist-get dest :transform))
	      (params (plist-get dest :params)))
	  (unless (fboundp transform)
	    (user-error "No such transformation function %s" transform))
	  (orgtbl-send-replace-tbl name (funcall transform table params)))
	(cl-incf ntbl))
      (message "Table converted and installed at %d receiver location%s"
	       ntbl (if (> ntbl 1) "s" ""))
      (and (> ntbl 0) ntbl))))

(defun org-remove-by-index (list indices &optional i0)
  "Remove the elements in LIST with indices in INDICES.
First element has index 0, or I0 if given."
  (if (not indices)
      list
    (if (integerp indices) (setq indices (list indices)))
    (setq i0 (1- (or i0 0)))
    (delq :rm (mapcar (lambda (x)
			(setq i0 (1+ i0))
			(if (memq i0 indices) :rm x))
		      list))))

(defun orgtbl-toggle-comment ()
  "Comment or uncomment the orgtbl at point."
  (interactive)
  (let* ((case-fold-search t)
	 (re1 (concat "^" (regexp-quote comment-start) orgtbl-line-start-regexp))
	 (re2 (concat "^" orgtbl-line-start-regexp))
	 (commented (save-excursion (forward-line 0)
				    (cond ((looking-at re1) t)
					  ((looking-at re2) nil)
					  (t (user-error "Not at an org table")))))
	 (re (if commented re1 re2))
	 beg end)
    (save-excursion
      (forward-line 0)
      (while (and (not (eq (point) (point-min)))
                  (looking-at re))
        (forward-line -1))
      (unless (eq (point) (point-min)) (forward-line 1))
      (setq beg (point))
      (while (and (not (eq (point) (point-max)))
                  (looking-at re))
        (forward-line 1))
      (setq end (point)))
    (comment-region beg end (if commented '(4) nil))))

(defun orgtbl-insert-radio-table ()
  "Insert a radio table template appropriate for this major mode."
  (interactive)
  (let* ((e (cl-assoc-if #'derived-mode-p orgtbl-radio-table-templates))
	 (txt (nth 1 e))
	 name pos)
    (unless e (user-error "No radio table setup defined for %s" major-mode))
    (setq name (read-string "Table name: "))
    (while (string-match "%n" txt)
      (setq txt (replace-match name t t txt)))
    (or (bolp) (insert "\n"))
    (setq pos (point))
    (insert txt)
    (goto-char pos)))

;;;###autoload
(defun orgtbl-to-generic (table params)
  "Convert the `orgtbl-mode' TABLE to some other format.

This generic routine can be used for many standard cases.

TABLE is a list, each entry either the symbol `hline' for
a horizontal separator line, or a list of fields for that
line.  PARAMS is a property list of parameters that can
influence the conversion.

Valid parameters are:

:backend, :raw

  Export backend used as a basis to transcode elements of the
  table, when no specific parameter applies to it.  It is also
  used to translate cells contents.  You can prevent this by
  setting :raw property to a non-nil value.

:splice

  When non-nil, only convert rows, not the table itself.  This is
  equivalent to setting to the empty string both :tstart
  and :tend, which see.

:skip

  When set to an integer N, skip the first N lines of the table.
  Horizontal separation lines do count for this parameter!

:skipcols

  List of columns that should be skipped.  If the table has
  a column with calculation marks, that column is automatically
  discarded beforehand.

:hline

  String to be inserted on horizontal separation lines.  May be
  nil to ignore these lines altogether.

:sep

  Separator between two fields, as a string.

Each in the following group may be either a string or a function
of no arguments returning a string:

:tstart, :tend

  Strings to start and end the table.  Ignored when :splice is t.

:lstart, :lend

  Strings to start and end a new table line.

:llstart, :llend

  Strings to start and end the last table line.  Default,
  respectively, to :lstart and :lend.

Each in the following group may be a string or a function of one
argument (either the cells in the current row, as a list of
strings, or the current cell) returning a string:

:lfmt

  Format string for an entire row, with enough %s to capture all
  fields.  When non-nil, :lstart, :lend, and :sep are ignored.

:llfmt

  Format for the entire last line, defaults to :lfmt.

:fmt

  A format to be used to wrap the field, should contain %s for
  the original field value.  For example, to wrap everything in
  dollars, you could use :fmt \"$%s$\".  This may also be
  a property list with column numbers and format strings, or
  functions, e.g.,

    (:fmt (2 \"$%s$\" 4 (lambda (c) (format \"$%s$\" c))))

  The format is ignored for empty fields.  Use :raw t with non-nil
  :backend option to force formatting empty fields.

:hlstart :hllstart :hlend :hllend :hsep :hlfmt :hllfmt :hfmt

 Same as above, specific for the header lines in the table.
 All lines before the first hline are treated as header.  If
 any of these is not present, the data line value is used.

This may be either a string or a function of two arguments:

:efmt

  Use this format to print numbers with exponential.  The format
  should have %s twice for inserting mantissa and exponent, for
  example \"%s\\\\times10^{%s}\".  This may also be a property
  list with column numbers and format strings or functions.
  :fmt will still be applied after :efmt."
  ;; Make sure `org-export-create-backend' is available.
  (require 'ox)
  (let* ((backend (plist-get params :backend))
	 (custom-backend
	  ;; Build a custom backend according to PARAMS.  Before
	  ;; defining a translator, check if there is anything to do.
	  ;; When there isn't, let BACKEND handle the element.
	  (org-export-create-backend
	   :parent (or backend 'org)
	   :transcoders
	   `((table . ,(org-table--to-generic-table params))
	     (table-row . ,(org-table--to-generic-row params))
	     (table-cell . ,(org-table--to-generic-cell params))
	     ;; Macros are not going to be expanded.  However, no
	     ;; regular backend has a transcoder for them.  We
	     ;; provide one so they are not ignored, but displayed
	     ;; as-is instead.
	     (macro . (lambda (m c i) (org-element-macro-interpreter m nil))))))
	 data info)
    ;; Store TABLE as Org syntax in DATA.  Tolerate non-string cells.
    ;; Initialize communication channel in INFO.
    (with-temp-buffer
      (let ((org-inhibit-startup t)) (org-mode))
      (org-fold-core-ignore-modifications
        (let ((standard-output (current-buffer))
	      (org-element-use-cache nil))
	  (dolist (e table)
	    (cond ((eq e 'hline) (princ "|--\n"))
		  ((consp e)
		   (princ "| ") (dolist (c e) (princ c) (princ " |"))
		   (princ "\n")))))
        (org-element-cache-reset)
        ;; Add backend specific filters, but not user-defined ones.  In
        ;; particular, make sure to call parse-tree filters on the
        ;; table.
        (setq info
	      (let ((org-export-filters-alist nil))
	        (org-export-install-filters
	         (org-combine-plists
		  (org-export-get-environment backend nil params)
		  `(:back-end ,(org-export-get-backend backend))))))
        (setq data
	      (org-export-filter-apply-functions
	       (plist-get info :filter-parse-tree)
	       (org-element-map (org-element-parse-buffer) 'table
	         #'identity nil t)
	       info))
        (when (and backend (symbolp backend) (not (org-export-get-backend backend)))
          (user-error "Unknown :backend value"))))
    (when (or (not backend) (plist-get info :raw)) (require 'ox-org))
    ;; Handle :skip parameter.
    (let ((skip (plist-get info :skip)))
      (when skip
	(unless (wholenump skip) (user-error "Wrong :skip value"))
	(let ((n 0))
	  (org-element-map data 'table-row
	    (lambda (row)
	      (if (>= n skip) t
		(org-element-extract row)
		(cl-incf n)
		nil))
	    nil t))))
    ;; Handle :skipcols parameter.
    (let ((skipcols (plist-get info :skipcols)))
      (when skipcols
	(unless (consp skipcols) (user-error "Wrong :skipcols value"))
	(org-element-map data 'table
	  (lambda (table)
	    (let ((specialp (org-export-table-has-special-column-p table)))
	      (dolist (row (org-element-contents table))
		(when (eq (org-element-property :type row) 'standard)
		  (let ((c 1))
		    (dolist (cell (nthcdr (if specialp 1 0)
					  (org-element-contents row)))
		      (when (memq c skipcols)
			(org-element-extract cell))
		      (cl-incf c))))))))))
    ;; Since we are going to export using a low-level mechanism,
    ;; ignore special column and special rows manually.
    (let ((special? (org-export-table-has-special-column-p data))
	  ignore)
      (org-element-map data (if special? '(table-cell table-row) 'table-row)
	(lambda (datum)
	  (when (if (org-element-type-p datum 'table-row)
		    (org-export-table-row-is-special-p datum nil)
		  (org-export-first-sibling-p datum nil))
	    (push datum ignore))))
      (setq info (plist-put info :ignore-list ignore)))
    ;; We use a low-level mechanism to export DATA so as to skip all
    ;; usual pre-processing and post-processing, i.e., hooks, Babel
    ;; code evaluation, include keywords and macro expansion.  Only
    ;; backend specific filters are retained.
    (let ((output (org-export-data-with-backend data custom-backend info)))
      ;; Remove final newline.
      (if (org-string-nw-p output) (substring-no-properties output 0 -1) ""))))

(defun org-table--generic-apply (value name &optional with-cons &rest args)
  (cond ((null value) nil)
        ((functionp value) `(funcall ',value ,@args))
        ((stringp value)
	 (cond ((consp (car args)) `(apply #'format ,value ,@args))
	       (args `(format ,value ,@args))
	       (t value)))
	((and with-cons (consp value))
	 `(let ((val (cadr (memq column ',value))))
	    (cond ((null val) contents)
		  ((stringp val) (format val ,@args))
		  ((functionp val) (funcall val ,@args))
		  (t (user-error "Wrong %s value" ,name)))))
        (t (user-error "Wrong %s value" name))))

(defun org-table--to-generic-table (params)
  "Return custom table transcoder according to PARAMS.
PARAMS is a plist.  See `orgtbl-to-generic' for more
information."
  (let ((backend (plist-get params :backend))
	(splice (plist-get params :splice))
	(tstart (plist-get params :tstart))
	(tend (plist-get params :tend)))
    `(lambda (table contents info)
       (concat
	,(and tstart (not splice)
	      `(concat ,(org-table--generic-apply tstart ":tstart") "\n"))
	,(if (or (not backend) tstart tend splice) 'contents
	   `(org-export-with-backend ',backend table contents info))
	,(org-table--generic-apply (and (not splice) tend) ":tend")))))

(defun org-table--to-generic-row (params)
  "Return custom table row transcoder according to PARAMS.
PARAMS is a plist.  See `orgtbl-to-generic' for more
information."
  (let* ((backend (plist-get params :backend))
	 (lstart (plist-get params :lstart))
	 (llstart (plist-get params :llstart))
	 (hlstart (plist-get params :hlstart))
	 (hllstart (plist-get params :hllstart))
	 (lend (plist-get params :lend))
	 (llend (plist-get params :llend))
	 (hlend (plist-get params :hlend))
	 (hllend (plist-get params :hllend))
	 (lfmt (plist-get params :lfmt))
	 (llfmt (plist-get params :llfmt))
	 (hlfmt (plist-get params :hlfmt))
	 (hllfmt (plist-get params :hllfmt)))
    `(lambda (row contents info)
       (if (eq (org-element-property :type row) 'rule)
	   ,(cond
	     ((plist-member params :hline)
	      (org-table--generic-apply (plist-get params :hline) ":hline"))
	     (backend `(org-export-with-backend ',backend row nil info)))
	 (let ((headerp ,(and (or hlfmt hlstart hlend)
			      '(org-export-table-row-in-header-p row info)))
	       (last-header-p
		,(and (or hllfmt hllstart hllend)
		      '(org-export-table-row-ends-header-p row info)))
	       (lastp (not (org-export-get-next-element row info))))
	   (when contents
	     ;; Check if we can apply `:lfmt', `:llfmt', `:hlfmt', or
	     ;; `:hllfmt' to CONTENTS.  Otherwise, fallback on
	     ;; `:lstart', `:lend' and their relatives.
	     ,(let ((cells
		     '(org-element-map row 'table-cell
			(lambda (cell)
			  ;; Export all cells, without separators.
			  ;;
			  ;; Use `org-export-data-with-backend'
			  ;; instead of `org-export-data' to eschew
			  ;; cached values, which
			  ;; ignore :orgtbl-ignore-sep parameter.
			  (org-export-data-with-backend
			   cell
			   (plist-get info :back-end)
			   (org-combine-plists info '(:orgtbl-ignore-sep t))))
			info)))
		`(cond
		  ,(and hllfmt
			`(last-header-p ,(org-table--generic-apply
					  hllfmt ":hllfmt" nil cells)))
		  ,(and hlfmt
			`(headerp ,(org-table--generic-apply
				    hlfmt ":hlfmt" nil cells)))
		  ,(and llfmt
			`(lastp ,(org-table--generic-apply
				  llfmt ":llfmt" nil cells)))
		  (t
		   ,(if lfmt (org-table--generic-apply lfmt ":lfmt" nil cells)
		      `(concat
			(cond
			 ,(and
			   (or hllstart hllend)
			   `(last-header-p
			     (concat
			      ,(org-table--generic-apply hllstart ":hllstart")
			      contents
			      ,(org-table--generic-apply hllend ":hllend"))))
			 ,(and
			   (or hlstart hlend)
			   `(headerp
			     (concat
			      ,(org-table--generic-apply hlstart ":hlstart")
			      contents
			      ,(org-table--generic-apply hlend ":hlend"))))
			 ,(and
			   (or llstart llend)
			   `(lastp
			     (concat
			      ,(org-table--generic-apply llstart ":llstart")
			      contents
			      ,(org-table--generic-apply llend ":llend"))))
			 (t
			  ,(cond
			    ((or lstart lend)
			     `(concat
			       ,(org-table--generic-apply lstart ":lstart")
			       contents
			       ,(org-table--generic-apply lend ":lend")))
			    (backend
			     `(org-export-with-backend
			       ',backend row contents info))
			    (t 'contents)))))))))))))))

(defun org-table--to-generic-cell (params)
  "Return custom table cell transcoder according to PARAMS.
PARAMS is a plist.  See `orgtbl-to-generic' for more
information."
  (let* ((backend (plist-get params :backend))
	 (efmt (plist-get params :efmt))
	 (fmt (plist-get params :fmt))
	 (hfmt (plist-get params :hfmt))
	 (sep (plist-get params :sep))
	 (hsep (plist-get params :hsep)))
    `(lambda (cell contents info)
       ;; Make sure that contents are exported as Org data when :raw
       ;; parameter is non-nil.
       ,(when (and backend (plist-get params :raw))
	  `(setq contents
		 ;; Since we don't know what are the pseudo object
		 ;; types defined in backend, we cannot pass them to
		 ;; `org-element-interpret-data'.  As a consequence,
		 ;; they will be treated as pseudo elements, and will
		 ;; have newlines appended instead of spaces.
		 ;; Therefore, we must make sure :post-blank value is
		 ;; really turned into spaces.
		 (replace-regexp-in-string
		  "\n" " "
		  (org-trim
		   (org-element-interpret-data
		    (org-element-contents cell))))))

       (let ((headerp ,(and (or hfmt hsep)
			    '(org-export-table-row-in-header-p
			      (org-element-parent-element cell) info)))
	     (column
	      ;; Call costly `org-export-table-cell-address' only if
	      ;; absolutely necessary, i.e., if one
	      ;; of :fmt :efmt :hfmt has a "plist type" value.
	      ,(and (cl-some (lambda (v) (integerp (car-safe v)))
			     (list efmt hfmt fmt))
		    '(1+ (cdr (org-export-table-cell-address cell info))))))
	 (when contents
	   ;; Check if we can apply `:efmt' on CONTENTS.
	   ,(when efmt
	      `(when (string-match orgtbl-exp-regexp contents)
		 (let ((mantissa (match-string 1 contents))
		       (exponent (match-string 2 contents)))
		   (setq contents ,(org-table--generic-apply
				    efmt ":efmt" t 'mantissa 'exponent)))))
	   ;; Check if we can apply FMT (or HFMT) on CONTENTS.
	   (cond
	    ,(and hfmt `(headerp (setq contents ,(org-table--generic-apply
						  hfmt ":hfmt" t 'contents))))
	    ,(and fmt `(t (setq contents ,(org-table--generic-apply
					   fmt ":fmt" t 'contents))))))
	 ;; If a separator is provided, use it instead of BACKEND's.
	 ;; Separators are ignored when LFMT (or equivalent) is
	 ;; provided.
	 ,(cond
	   ((or hsep sep)
	    `(if (or ,(and (not sep) '(not headerp))
		     (plist-get info :orgtbl-ignore-sep)
		     (not (org-export-get-next-element cell info)))
		 ,(if (not backend) 'contents
		    `(org-export-with-backend ',backend cell contents info))
	       (concat contents
		       ,(if (and sep hsep) `(if headerp ,hsep ,sep)
			  (or hsep sep)))))
	   (backend `(org-export-with-backend ',backend cell contents info))
	   (t 'contents))))))

;;;###autoload
(defun orgtbl-to-tsv (table params)
  "Convert the `orgtbl-mode' TABLE to TAB separated material."
  (orgtbl-to-generic table (org-combine-plists '(:sep "\t") params)))

;;;###autoload
(defun orgtbl-to-csv (table params)
  "Convert the `orgtbl-mode' TABLE to CSV material.
This does take care of the proper quoting of fields with comma or quotes."
  (orgtbl-to-generic table
		     (org-combine-plists '(:sep "," :fmt org-quote-csv-field)
					 params)))

;;;###autoload
(defun orgtbl-to-latex (table params)
  "Convert the `orgtbl-mode' TABLE to LaTeX.

TABLE is a list, each entry either the symbol `hline' for
a horizontal separator line, or a list of fields for that line.
PARAMS is a property list of parameters that can influence the
conversion.  All parameters from `orgtbl-to-generic' are
supported.  It is also possible to use the following ones:

:booktabs

  When non-nil, use formal \"booktabs\" style.

:environment

  Specify environment to use, as a string.  If you use
  \"longtable\", you may also want to specify :language property,
  as a string, to get proper continuation strings."
  (require 'ox-latex)
  (orgtbl-to-generic
   table
   (org-combine-plists
    ;; Provide sane default values.
    (list :backend 'latex
	  :latex-default-table-mode 'table
	  :latex-tables-centered nil
	  :latex-tables-booktabs (plist-get params :booktabs)
	  :latex-table-scientific-notation nil
	  :latex-default-table-environment
	  (or (plist-get params :environment) "tabular"))
    params)))

;;;###autoload
(defun orgtbl-to-html (table params)
  "Convert the `orgtbl-mode' TABLE to HTML.

TABLE is a list, each entry either the symbol `hline' for
a horizontal separator line, or a list of fields for that line.
PARAMS is a property list of parameters that can influence the
conversion.  All parameters from `orgtbl-to-generic' are
supported.  It is also possible to use the following one:

:attributes

  Attributes and values, as a plist, which will be used in
  <table> tag."
  (require 'ox-html)
  (orgtbl-to-generic
   table
   (org-combine-plists
    ;; Provide sane default values.
    (list :backend 'html
	  :html-table-data-tags '("<td%s>" . "</td>")
	  :html-table-use-header-tags-for-first-column nil
	  :html-table-align-individual-fields t
	  :html-table-row-tags '("<tr>" . "</tr>")
	  :html-table-attributes
	  (if (plist-member params :attributes)
	      (plist-get params :attributes)
	    '(:border "2" :cellspacing "0" :cellpadding "6" :rules "groups"
		      :frame "hsides")))
    params)))

;;;###autoload
(defun orgtbl-to-texinfo (table params)
  "Convert the `orgtbl-mode' TABLE to Texinfo.

TABLE is a list, each entry either the symbol `hline' for
a horizontal separator line, or a list of fields for that line.
PARAMS is a property list of parameters that can influence the
conversion.  All parameters from `orgtbl-to-generic' are
supported.  It is also possible to use the following one:

:columns

  Column widths, as a string.  When providing column fractions,
  \"@columnfractions\" command can be omitted."
  (require 'ox-texinfo)
  (let ((output
	 (orgtbl-to-generic
	  table
	  (org-combine-plists
	   (list :backend 'texinfo
		 :texinfo-tables-verbatim nil
		 :texinfo-table-scientific-notation nil)
	   params)))
	(columns (let ((w (plist-get params :columns)))
		   (cond ((not w) nil)
			 ((string-match-p "{\\|@columnfractions " w) w)
			 (t (concat "@columnfractions " w))))))
    (if (not columns) output
      (replace-regexp-in-string
       "@multitable \\(.*\\)" columns output t nil 1))))

;;;###autoload
(defun orgtbl-to-orgtbl (table params)
  "Convert the `orgtbl-mode' TABLE into another `orgtbl-mode' table.

TABLE is a list, each entry either the symbol `hline' for
a horizontal separator line, or a list of fields for that line.
PARAMS is a property list of parameters that can influence the
conversion.  All parameters from `orgtbl-to-generic' are
supported.

Useful when slicing one table into many.  The :hline, :sep,
:lstart, and :lend provide orgtbl framing.  :tstart and :tend can
be set to provide ORGTBL directives for the generated table."
  (require 'ox-org)
  (orgtbl-to-generic table (org-combine-plists params (list :backend 'org))))

(defun orgtbl-to-table.el (table params)
  "Convert the `orgtbl-mode' TABLE into a table.el table.
TABLE is a list, each entry either the symbol `hline' for
a horizontal separator line, or a list of fields for that line.
PARAMS is a property list of parameters that can influence the
conversion.  All parameters from `orgtbl-to-generic' are
supported."
  (with-temp-buffer
    (insert (orgtbl-to-orgtbl table params))
    (org-table-align)
    (goto-char (point-min))
    (while (search-forward "-|" nil t)
      (replace-match "-+"))
    (goto-char (point-min))
    (while (search-forward "|-" nil t)
      (replace-match "+-"))
    (buffer-string)))

(defun orgtbl-to-unicode (table params)
  "Convert the `orgtbl-mode' TABLE into a table with unicode characters.

TABLE is a list, each entry either the symbol `hline' for
a horizontal separator line, or a list of fields for that line.
PARAMS is a property list of parameters that can influence the
conversion.  All parameters from `orgtbl-to-generic' are
supported.  It is also possible to use the following ones:

:ascii-art

  When non-nil, use \"ascii-art-to-unicode\" package to translate
  the table.  You can download it here:
  https://gnuvola.org/software/j/aa2u/ascii-art-to-unicode.el.

:narrow

  When non-nil, narrow columns width than provided width cookie,
  using \"=>\" as an ellipsis, just like in an Org mode buffer."
  (require 'ox-ascii)
  (orgtbl-to-generic
   table
   (org-combine-plists
    (list :backend 'ascii
	  :ascii-charset 'utf-8
	  :ascii-table-widen-columns (not (plist-get params :narrow))
	  :ascii-table-use-ascii-art (plist-get params :ascii-art))
    params)))

;; Put the cursor in a column containing numerical values
;; of an Org table,
;; type C-c " a
;; A new column is added with a bar plot.
;; When the table is refreshed (C-u C-c *),
;; the plot is updated to reflect the new values.

(defun orgtbl-ascii-draw (value min max &optional width characters)
  "Draw an ascii bar in a table.
VALUE is the value to plot, it determines the width of the bar to draw.
MIN is the value that will be displayed as empty (zero width bar).
MAX is the value that will draw a bar filling all the WIDTH.
WIDTH is the span in characters from MIN to MAX.
CHARACTERS is a string that will compose the bar, with shades of grey
from pure white to pure black.  It defaults to a 10 characters string
of regular ascii characters."
  (let* ((width      (ceiling (or width 12)))
	 (characters (or characters " .:;c!lhVHW"))
	 (len        (1- (length characters)))
	 (value      (float (if (numberp value)
				value (string-to-number value))))
	 (relative   (/ (- value min) (- max min)))
	 (steps      (round (* relative width len))))
    (cond ((< steps             0) "too small")
	  ((> steps (* width len)) "too large")
	  (t (let* ((int-division (/ steps len))
		    (remainder    (- steps (* int-division len))))
	       (concat (make-string int-division (elt characters len))
		       (string (elt characters remainder))))))))

;;;###autoload
(defun orgtbl-ascii-plot (&optional ask)
  "Draw an ASCII bar plot in a column.

With cursor in a column containing numerical values, this function
will draw a plot in a new column.

ASK, if given, is a numeric prefix to override the default 12
characters width of the plot.  ASK may also be the `\\[universal-argument]' \
prefix,
which will prompt for the width."
  (interactive "P")
  (let ((col (org-table-current-column))
	(min  1e999)		 ; 1e999 will be converted to infinity
	(max -1e999)		 ; which is the desired result
	(table (org-table-to-lisp))
	(length
	 (cond ((consp ask)
		(read-number "Length of column " 12))
	       ((numberp ask) ask)
	       (t 12))))
    ;; Skip any hline a the top of table.
    (while (eq (car table) 'hline) (pop table))
    ;; Skip table header if any.
    (dolist (x (or (cdr (memq 'hline table)) table))
      (when (consp x)
	(setq x (nth (1- col) x))
	(when (string-match
	       "^[-+]?\\([0-9]*[.]\\)?[0-9]*\\([eE][+-]?[0-9]+\\)?$"
	       x)
	  (setq x (string-to-number x))
	  (when (> min x) (setq min x))
	  (when (< max x) (setq max x)))))
    (org-table-insert-column)
    (org-table-move-column-right)
    (org-table-store-formulas
     (cons
      (cons
       (concat "$" (number-to-string (1+ col)))
       (format "'(%s $%s %s %s %s)"
	       "orgtbl-ascii-draw" col min max length))
      (org-table-get-stored-formulas)))
    (org-table-recalculate t)))

;; Example of extension: unicode characters
;; Here are two examples of different styles.

;; Unicode block characters are used to give a smooth effect.
;; See https://en.wikipedia.org/wiki/Block_Elements
;; Use one of those drawing functions
;; - orgtbl-ascii-draw   (the default ascii)
;; - orgtbl-uc-draw-grid (unicode with a grid effect)
;; - orgtbl-uc-draw-cont (smooth unicode)

;; This is best viewed with the "DejaVu Sans Mono" font
;; (use M-x set-frame-font).

(defun orgtbl-uc-draw-grid (value min max &optional width)
  "Draw a bar in a table using block unicode characters.
It is a variant of `orgtbl-ascii-draw' with Unicode block
characters, for a smooth display.  Bars appear as grids (to the
extent the font allows)."
  ;; https://en.wikipedia.org/wiki/Block_Elements
  ;; best viewed with the "DejaVu Sans Mono" font.
  (orgtbl-ascii-draw value min max width
		     " \u258F\u258E\u258D\u258C\u258B\u258A\u2589"))

(defun orgtbl-uc-draw-cont (value min max &optional width)
  "Draw a bar in a table using block unicode characters.
It is a variant of `orgtbl-ascii-draw' with Unicode block
characters, for a smooth display.  Bars are solid (to the extent
the font allows)."
  (orgtbl-ascii-draw value min max width
		     " \u258F\u258E\u258D\u258C\u258B\u258A\u2589\u2588"))

(defun org-table-get-remote-range (name-or-id form)
  "Get a field value or a list of values in a range from table at ID.

NAME-OR-ID may be the name of a table in the current file as set
by a \"#+NAME:\" directive.  The first table following this line
will then be used.  Alternatively, it may be an ID referring to
any entry, also in a different file.  In this case, the first
table in that entry will be referenced.
FORM is a field or range descriptor like \"@2$3\" or \"B3\" or
\"@I$2..@II$2\".  All the references must be absolute, not relative.

The return value is either a single string for a single field, or a
list of the fields in the rectangle."
  (save-match-data
    (let ((case-fold-search t) (id-loc nil)
	  ;; Protect a bunch of variables from being overwritten by
	  ;; the context of the remote table.
	  org-table-column-names org-table-column-name-regexp
	  org-table-local-parameters org-table-named-field-locations
	  org-table-current-line-types
	  org-table-current-begin-pos org-table-dlines
	  org-table-current-ncol
	  org-table-hlines
	  org-table-last-column-widths
	  org-table-last-alignment
	  buffer loc)
      (setq form (org-table-convert-refs-to-rc form))
      (org-with-wide-buffer
       (goto-char (point-min))
       (if (re-search-forward
	    (concat "^[ \t]*#\\+\\(tbl\\)?name:[ \t]*"
		    (regexp-quote name-or-id) "[ \t]*$")
	    nil t)
	   (setq buffer (current-buffer) loc (match-beginning 0))
	 (setq id-loc (org-id-find name-or-id 'marker))
	 (unless (and id-loc (markerp id-loc))
	   (user-error "Can't find remote table \"%s\"" name-or-id))
	 (setq buffer (marker-buffer id-loc)
	       loc (marker-position id-loc))
	 (move-marker id-loc nil))
       (with-current-buffer buffer
	 (org-with-wide-buffer
	  (goto-char loc)
	  (forward-char 1)
	  (unless (and (re-search-forward "^\\(\\*+ \\)\\|^[ \t]*|" nil t)
		       (not (match-beginning 1)))
	    (user-error "Cannot find a table at NAME or ID %s" name-or-id))
	  (org-table-analyze)
	  (setq form (org-table-formula-substitute-names
		      (org-table-formula-handle-first/last-rc form)))
	  (if (and (string-match org-table-range-regexp form)
		   (> (length (match-string 0 form)) 1))
	      (org-table-get-range
	       (match-string 0 form) org-table-current-begin-pos 1)
	    form)))))))

(defun org-table-remote-reference-indirection (form)
  "Return formula with table remote references substituted by indirection.
For example \"remote($1, @>$2)\" => \"remote(year_2013, @>$1)\".
This indirection works only with the format @ROW$COLUMN.  The
format \"B3\" is not supported because it can not be
distinguished from a plain table name or ID."
  (let ((regexp
	 ;; Same as in `org-table-eval-formula'.
	 (concat "\\<remote([ \t]*\\("
		 ;; Allow "$1", "@<", "$-1", "@<<$1" etc.
		 "[@$][^ \t,]+"
		 "\\)[ \t]*,[ \t]*\\([^\n)]+\\))")))
    (replace-regexp-in-string
     regexp
     (lambda (m)
       (save-match-data
	 (let ((eq (org-table-formula-handle-first/last-rc (match-string 1 m))))
	   (org-table-get-range
	    (if (string-match-p "\\`\\$[0-9]+\\'" eq)
		(concat "@0" eq)
	      eq)))))
     form t t 1)))

(defmacro org-define-lookup-function (mode)
  (let ((mode-str (symbol-name mode))
	(first-p (eq mode 'first))
	(all-p (eq mode 'all)))
    (let ((plural-str (if all-p "s" "")))
      `(defun ,(intern (format "org-lookup-%s" mode-str)) (val s-list r-list &optional predicate)
	 ,(format "Find %s occurrence%s of VAL in S-LIST; return corresponding element%s of R-LIST.
If R-LIST is nil, return matching element%s of S-LIST.
If PREDICATE is not nil, use it instead of `equal' to match VAL.
Matching is done by (PREDICATE VAL S), where S is an element of S-LIST.
This function is generated by a call to the macro `org-define-lookup-function'."
		  mode-str plural-str plural-str plural-str)
	 (let ,(let ((lvars '((p (or predicate 'equal))
			      (sl s-list)
			      (rl (or r-list s-list))
			      (ret nil))))
		 (if first-p (cons '(match-p nil) lvars) lvars))
	   (while ,(if first-p '(and (not match-p) sl) 'sl)
	     (when (funcall p val (car sl))
	       ,(when first-p '(setq match-p t))
	       (let ((rval (car rl)))
		 (setq ret ,(if all-p '(append ret (list rval)) 'rval))))
	     (setq sl (cdr sl) rl (cdr rl)))
	   ret)))))

(org-define-lookup-function first)
(org-define-lookup-function last)
(org-define-lookup-function all)

(provide 'org-table)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; org-table.el ends here
