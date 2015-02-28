(TeX-add-style-hook
 "__PROJECT-NAME__"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "11pt" "twoside" "a4paper" "notitlepage")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("geometry" "a4paper" "margin=2.5cm") ("babel" "english") ("subfigure" "sf" "SF")))
   (TeX-run-style-hooks
    "latex2e"
    "preamble"
    "notationProofComplexity"
    "notationClauseSpacePebbling"
    "notationLocal"
    "editingmacros"
    "abstract"
    "introduction"
    "infoaboutmacros"
    "concludingremarks"
    "acknowledgements"
    "article"
    "art11"
    "ifthen"
    "geometry"
    "times"
    "babel"
    "fancyhdr"
    "subfigure"
    "bussproofs")
   (TeX-add-symbols
    "loadhyperrefpackage"
    "DONOTINSERTCOMMENTS")
   (LaTeX-add-bibliographies
    "refArticles"
    "refBooks"
    "refOther")))

