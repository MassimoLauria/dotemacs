(define-package "magit" "20240811.1419" "A Git porcelain inside Emacs."
  '((emacs "26.1")
    (compat "30.0.0.0")
    (dash "20240510")
    (git-commit "20240808")
    (magit-section "20240808")
    (seq "2.24")
    (transient "20240805")
    (with-editor "20240806"))
  :commit "a2739d7db1fdf19b95f36f6ddd15b0c1f523bd26" :authors
  '(("Marius Vollmer" . "marius.vollmer@gmail.com")
    ("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev"))
  :maintainer
  '("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev")
  :keywords
  '("git" "tools" "vc")
  :url "https://github.com/magit/magit")
;; Local Variables:
;; no-byte-compile: t
;; End:
