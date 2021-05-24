;;; skeletor-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "skeletor" "skeletor.el" (0 0 0 0))
;;; Generated autoloads from skeletor.el

(autoload 'skeletor-define-template "skeletor" "\
Declare a new project type.

* NAME is a string naming the project type. A corresponding
  skeleton should exist in `skeletor--directory' or
  `skeletor-user-directory'.

* TITLE is the name to use when referring to this project type in
  the UI.

* SUBSTITUTIONS is an alist of (string . substitution) specifying
  substitutions to be used, in addition to the global
  substitutions defined in `skeletor-global-substitutions'. These
  are evaluated when creating an instance of the template.

* When NO-LICENSE? is t, the project will not be initialised with
  a license file.

* DEFAULT-LICENSE is a regexp matching the name of a license to
  be used as the default. This default is used to pre-populate
  the license prompt when creating an insance of the template.

* LICENSE-FILE-NAME is the filename to use for the generated
  license file.

* AFTER-CREATION is a unary function to be run once the project
  is created. It should take a single argument--the path to the
  newly-created project.

* REQUIRES-EXECUTABLES is an alist of `(PROGRAM . URL)'
  expressing programs needed to expand this skeleton. See
  `skeletor-require-executables'.

\(fn NAME &key TITLE SUBSTITUTIONS BEFORE-GIT AFTER-CREATION NO-LICENSE? DEFAULT-LICENSE LICENSE-FILE-NAME REQUIRES-EXECUTABLES)" nil t)

(function-put 'skeletor-define-template 'lisp-indent-function '1)

(autoload 'skeletor-define-constructor "skeletor" "\
Define a new project type with a custom way of constructing a skeleton.
This can be used to add bindings for command-line tools.

* TITLE is a string naming the project type in the UI.

* INITIALISE is a unary function that creates the project
  structure. It will be passed an alist containing a
  specification for the skeleton, including the following keys:

  - `project-name': The name of the project read from the user
  - `project-dir': The directory at which the project should be created
  - `dest': The project-directory joined with the project name.

  Consider using `let-alist' to conveniently bind these variables
  to `.project-name', `.project-dir' and `.dest' in the scope of
  your initialisation function.

  INITIALISE is expected to initialise the new project at `dest'.
  The command should signal an error if this fails for any
  reason.

  Make sure to switch to a shell buffer if INITIALISE is a shell
  command that requires user interaction.

* SUBSTITUTIONS is an alist of (string . substitution) specifying
  substitutions to be used, in addition to the global
  substitutions defined in `skeletor-global-substitutions'. These
  are evaluated when creating an instance of the template.

* BEFORE-GIT is a unary function to be run once the project is
  created, but before git is initialised. It should take a single
  argument--the path to the newly-created project.

* AFTER-CREATION is a unary function to be run once the project
  is created. It should take a single argument--the path to the
  newly-created project.

* When NO-GIT? is t, the project will not be initialised with a
  git repo, regardless of the value of `skeletor-init-with-git'.

* When NO-LICENSE? is t, the project will not be initialised with
  a license file.

* DEFAULT-LICENSE is a regexp matching the name of a license to
  be used as the default. This default is used to pre-populate
  the license prompt when creating an insance of the template.

* LICENSE-FILE-NAME is the filename to use for the generated
  license file.

* REQUIRES-EXECUTABLES is an alist of `(PROGRAM . URL)'
  expressing programs needed to expand this skeleton. See
  `skeletor-require-executables'.

\(fn TITLE &key INITIALISE SUBSTITUTIONS BEFORE-GIT AFTER-CREATION NO-GIT? NO-LICENSE? DEFAULT-LICENSE LICENSE-FILE-NAME REQUIRES-EXECUTABLES)" nil t)

(function-put 'skeletor-define-constructor 'lisp-indent-function '1)

(autoload 'skeletor-create-project "skeletor" "\
Interactively create a new project with Skeletor.

SKELETON is a SkeletorProjectType.

\(fn SKELETON)" t nil)

(autoload 'skeletor-create-project-at "skeletor" "\
Interactively create a new project with Skeletor.

DIR is destination directory, which must exist.

SKELETON is a SkeletorProjectType.

\(fn DIR SKELETON)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "skeletor" '("skeletor-")))

;;;***

;;;### (autoloads nil nil ("skeletor-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; skeletor-autoloads.el ends here
