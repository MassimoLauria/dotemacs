;;; auto-complete-latex-lib.el --- keywords database for auto-complete-latex.el

;; Copyright (C) 2010 tequilasunset

;; Author:   tequilasunset <tequilasunset.mac@gmail.com>
;; Keywords: LaTeX
;; Version:  0.1.0

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;

;;; Requirements:

;;   auto-complete-latex.el:
;;
;;     http://bitbucket.org/tequilasunset/auto-complete-latex/src/
;;

;;; Commentary:

;;; Code:

(require 'auto-complete-latex)

(setq ac-l-sources
      '(ac-l-source-user-commands
        ac-l-source-basic-commands
        ac-l-source-user-variables
        ac-l-source-basic-variables))

(setq ac-l-package-sources
      '( ;; --command--
        ("amsmath" . ac-l-source-AMS-commands)
        ("latexsym" . ac-l-source-latexsym-commands)
        ("hyperref" . ac-l-source-hyperref-commands)
        ("graphicx" . ac-l-source-graphicx-commands)
        ("color\\|colortbl" . ac-l-source-color-commands)
        ("colortbl" . ac-l-source-colortbl-commands)
        ("array\\|tabularx\\|colortbl" . ac-l-source-array-commands)
        ("tabularx" . ac-l-source-tabularx-commands)
        ("longtable" . ac-l-source-longtable-commands)
        ("textcomp" . ac-l-source-textcomp-commands)
        ("txfonts" . ac-l-source-txfonts-commands)
        ("otf" . ac-l-source-otf-commands)
        ;; --variable--
        ("amsmath" . ac-l-source-AMS-variables)
        ("hyperref" . ac-l-source-hyperref-variables)
        ("graphicx" . ac-l-source-graphicx-variables)
        ("color\\|colortbl" . ac-l-source-color-variables)
        ("longtable" . ac-l-source-longtable-variables)))

(defmacro ac-l-define-source (name list mark length)
  "Define source of commands. NAME is a name of source. The end
of it has to be `-commands'. LIST is a list of candidates.
MARK is a symbol as string. LENGTH is integer. If LENGTH
above `ac-prefix', included in candidates. More info, see
`ac-sources'."
  `(defvar ,name
     '((candidates . (lambda () (all-completions ac-prefix ,list)))
       (symbol . ,mark)
       (prefix . "\\\\\\([a-zA-Z@]+\\)")
       (requires . ,length))))

(defmacro ac-l-define-source* (name list mark length)
  "Define source of variables. NAME is a name of source. The end
of it has to be `-variables'. LIST is a list of candidates.
MARK is a symbol as string. LENGTH is integer. If LENGTH
above `ac-prefix', included in candidates. More info, see
`ac-sources'."
  `(defvar ,name
     '((candidates . (lambda () (all-completions ac-prefix ,list)))
       (symbol . ,mark)
       (requires . ,length))))


;;;; dictionary sources

(ac-l-define-source ac-l-source-user-commands ac-l-user-commands "U" 1)

(ac-l-define-source* ac-l-source-user-variables ac-l-user-variables "U" 1)

(ac-l-define-source*
 ac-l-source-basic-variables
 '("a4paper" "a5paper" "abstract" "array" "article" "b4paper" "b5paper" "book"
   "bp" "center" "cm" "dd" "description" "displaymath" "document" "draft" "em"
   "ex" "enumerate" "eqnarray" "equation" "evensidemargin" "executivepaper"
   "figure" "final" "fleqn" "flushleft" "flushright" "in" "itemize" "landscape"
   "legalpaper" "leqno" "letter" "letterpaper" "list" "minipage" "mm"
   "notitlepage" "oneside" "openany" "openbib" "picture" "pc" "pt" "quotation"
   "quote" "report" "tabbing" "table" "tabular" "thebibliography" "theindex"
   "titlepage" "twoside" "verb" "verbatim" "verse"

   ;; pLaTeX
   "disablejfam" "jarticle" "jbook" "jreport" "jsarticle" "jsbook" "mentuke"
   "tarticle" "tbook" "tombo" "tombow" "treport" "zh" "zw"

   ;; package name
   "amsmath" "array" "color" "colortbl" "hyperref" "graphicx" "latexsym"
   "longtable" "otf" "tabularx" "textcomp" "txfonts"
   ) "l" 1)

(ac-l-define-source*
 ac-l-source-AMS-variables
 '("align" "alignat" "aligned" "alignedat" "amsmath" "amssymb" "bmatrix" "cases"
   "centertags" "flalign" "fleqn" "gather" "gathered" "intlimits" "leqno"
   "matrix" "multline" "namelimits" "nointlimits" "nonamelimits" "nosumlimits"
   "pmatrix" "reqno" "smallmatrix" "split" "subarray" "subequations" "sumlimits"
   "tbtags" "vmatrix"
   "Bmatrix" "MaxMatrixCols" "Vmatrix"
   ) "a" 1)

(ac-l-define-source*
 ac-l-source-hyperref-variables
 '("anchorcolor" "backref" "baseurl" "bookmarks" "bookmarksdepth"
   "bookmarksnumbered" "bookmarksopen" "bookmarksopenlevel" "bookmarkstype"
   "breaklinks" "citebordercolor" "citecolor" "colorlinks" "debug" "dvipdf"
   "dvipdfm" "dvips" "dvipsone" "dviwindo" "extension" "filebordercolor"
   "filecolor" "frenchlinks" "hyperfigures" "hyperindex" "hypertex"
   "hypertexnames" "implicit" "latex2html" "legalpaper" "linkbordercolor"
   "linkcolor" "linktocpage" "menubordercolor" "menucolor" "naturalnames"
   "nesting" "pageanchor" "pagebackref" "pagebordercolor" "pagecolor"
   "pdfauthor" "pdfborder" "pdfborderstyle" "pdfcenterwindow" "pdfcreator"
   "pdfescapeform" "pdffitwindow" "pdfhighlight" "pdfkeywords" "pdfmenubar"
   "pdfnewwindow" "pdfpagelabels" "pdfpagelayout" "pdfpagemode" "pdfpagescrop"
   "pdfpagetransition" "pdfproducer" "pdfstartpage" "pdfstartview" "pdfsubject"
   "pdftex" "pdftitle" "pdftoolbar" "pdfview" "pdfwindowui" "plainpages"
   "ps2pdf" "raiselinks" "runbordercolor" "setpagesize" "tex4ht" "textures"
   "unicode" "urlbordercolor" "urlcolor" "verbose" "vtex"
   ) "h" 1)

(ac-l-define-source*
 ac-l-source-graphicx-variables
 '("angle" "bb" "bbllx" "bblly" "bburx" "bbury" "clip" "command" "demo" "draft"
   "ext" "final" "graphicx" "height" "hiderotate" "hidesbb" "hidescale"
   "hiresbb" "keepaspectratio" "natheight" "natwidth" "origin" "read" "scale"
   "totalheight" "trim" "type" "viewport" "width"
   ) "g" 1)

(ac-l-define-source*
 ac-l-source-color-variables
 '("black" "blue" "green" "cyan" "magenta" "red" "white" "yellow"
   "Apricot" "Aquamarine" "Bittersweet" "Black" "Blue" "BlueGreen" "BlueViolet"
   "BrickRed" "Brown" "BurntOrange" "CadetBlue" "CarnationPink" "Cerulean"
   "CornflowerBlue" "Cyan" "Dandelion" "DarkOrchid" "Emerald" "ForestGreen"
   "Fuchsia" "Goldenrod" "Gray" "Green" "GreenYellow" "JungleGreen" "Lavender"
   "LimeGreen" "Magenta" "Mahogany" "Maroon" "Melon" "MidnightBlue" "Mulberry"
   "NavyBlue" "OliveGreen" "Orange" "OrangeRed" "Orchid" "Peach" "Periwinkle"
   "PineGreen" "Plum" "ProcessBlue" "Purple" "RawSienna" "Red" "RedOrange"
   "RedViolet" "Rhodamine" "RoyalBlue" "RoyalPurple" "RubineRed" "Salmon"
   "SeaGreen" "Sepia" "SkyBlue" "SpringGreen" "Tan" "TealBlue" "Thistle"
   "Turquoise" "Violet" "VioletRed" "WildStrawberry" "Yellow" "YellowGreen"
   "YellowOrange"
   ) "c" 1)

(ac-l-define-source*
 ac-l-source-longtable-variables
 '("errorshow" "final" "pausing" "set"
   "LTchunksize"
   ) "t" 1)

(ac-l-define-source ac-l-source-basic-commands ac-l-basic-commands "l" 1)

(setq ac-l-basic-commands
      '("acute" "address" "addtocounter" "addtolength" "aleph" "alpha" "amalg"
        "angle" "appendix" "approx" "arabic" "arccos" "arcsin" "arctan" "arg"
        "arraycolsep" "arrayrulewidth" "arraystretch" "arrowvert" "ast" "asymp"
        "atop" "author" "backslash" "bar" "baselineskip" "because" "begin"
        "beta" "bf" "bfseries" "bibitem" "bibliography" "bibliographystyle"
        "big" "bigcap" "bigcirc" "bigcup" "bigg" "biggl" "biggm" "biggr" "bigl"
        "bigm" "bigodot" "bigoplus" "bigotimes" "bigr" "bigskip" "bigsqcup"
        "bigtriangledown" "bigtriangleup" "biguplus" "bigvee" "bigwedge" "bmod"
        "boldmath" "bot" "bowtie" "bracevert" "breve" "buildrel" "bullet" "cap"
        "caption" "cbezier" "cdot" "cdots" "centering" "chapter" "check" "chi"
        "circ" "cite" "cleardoublepage" "clearpage" "cline" "clubsuit" "colon"
        "columnsep" "columnseprule" "cong" "coprod" "cos" "cosh" "cot" "crcr"
        "csc" "cup" "dagger" "dashbox" "dashv" "date" "ddagger" "ddot" "ddots"
        "def" "deg" "delta" "det" "diamond" "diamondsuit" "dim" "displaystyle"
        "div" "documentclass" "dot" "doteq" "doublerulesep" "downarrow" "ell"
        "emph" "emptyset" "end" "enlargethispage" "enskip" "enspace"
        "ensuremath" "epsilon" "equiv" "eta" "exists" "exp" "extracolsep" "fbox"
        "footnote" "footnotemark" "footnotesize" "footnotetext" "footskip"
        "forall" "frac" "framebox" "frown" "gamma" "gcd" "ge" "geq" "gets" "gg"
        "glossary" "grave" "hat" "hbar" "hbox" "headheight" "headsep"
        "heartsuit" "hfil" "hfill" "hline" "hom" "hookleftarrow"
        "hookrightarrow" "hphantom" "hskip" "hspace" "hss" "huge" "ialign" "iff"
        "imath" "in" "include" "includeonly" "indent" "index" "inf" "infty"
        "input" "int" "iota" "it" "item" "itemindent" "itemsep" "itshape"
        "jmath" "join" "kappa" "ker" "kern" "kill" "label" "labelsep"
        "labelwidth" "lambda" "land" "langle" "lbrace" "lceil" "ldots" "le"
        "left" "leftarrow" "leftharpoondown" "leftharpoonup" "leftmargin"
        "leftrightarrow" "leq" "lfloor" "lg" "lgroup" "lim" "liminf" "limsup"
        "line" "linebreak" "linethickness" "linewidth" "listoffigures"
        "listoftables" "listparindent" "ll" "llap" "lmoustache" "ln" "lnot"
        "log" "longleftarrow" "longleftrightarrow" "longmapsto" "longrightarrow"
        "lor" "lower" "macrocode" "makeatletter" "makeatother" "makebox"
        "makeindex" "maketitle" "mapsto" "marginpar" "marginparsep"
        "marginparwidth" "mathbin" "mathbm" "mathcal" "mathclose" "mathindent"
        "mathit" "mathnormal" "mathop" "mathopen" "mathord" "mathpunct"
        "mathrel" "mathring" "mathrm" "mathsf" "mathtt" "max" "mbox" "mdseries"
        "medskip" "mid" "min" "models" "mp" "mu" "multicolumn" "multiput"
        "nabla" "natural" "ne" "nearrow" "neg" "negthinspace" "neq" "newcommand"
        "newcounter" "newenvironment" "newlength" "newline" "newpage"
        "newsavebox" "newtheorem" "ni" "noalign" "nocite" "nofiles" "noindent"
        "nolinebreak" "nonumber" "nopagebreak" "normalsize" "not" "notin" "nu"
        "nwarrow" "oalign" "oddsidemargin" "odot" "oint" "omega" "ominus"
        "onelineskip" "ooalign" "oplus" "oslash" "otimes" "oval" "overbrace"
        "overleftarrow" "overline" "overrightarrow" "owns" "pagebreak"
        "pagenumbering" "pageref" "pagestyle" "par" "paragraph" "parallel"
        "parbox" "parindent" "parsep" "parskip" "part" "partial" "partopsep"
        "perp" "phantom" "phi" "pi" "pm" "pmod" "prec" "preceq" "prime" "prod"
        "propto" "protect" "providecommand" "psi" "put" "qbezier" "qquad" "quad"
        "raise" "raisebox" "rbrace" "rceil" "ref" "renewcommand"
        "renewenvironment" "rfloor" "rgroup" "rho" "right" "rightarrow"
        "rightharpoondown" "rightharpoonup" "rightleftharpoons" "rightmargin"
        "rlap" "rm" "rmfamily" "rmoustache" "rule" "savebox" "sbox" "sc"
        "scriptscriptstyle" "scriptsize" "scriptstyle" "scshape" "searrow"
        "sec" "section" "setbox" "setcounter" "setlength" "setminus"
        "settowidth" "sf" "sffamily" "sharp" "sigma" "sim" "simeq" "sin" "sinh"
        "skew" "sl" "sloppy" "sloppypar" "slshape" "small" "smallint"
        "smallskip" "smile" "spadesuit" "sqcap" "sqcup" "sqrt" "sqsubseteq"
        "sqsupseteq" "stackrel" "star" "stepcounter" "subparagraph" "subsection"
        "subset" "subseteq" "subsubparagraph" "subsubsection" "succ" "succeq"
        "sum" "sup" "supset" "supseteq" "surd" "swarrow" "tabcolsep"
        "tableofcontents" "tan" "tanh" "tau" "textbf" "textcircled" "textheight"
        "textit" "textmd" "textnormal" "textrm" "textsc" "textsf" "textsl"
        "textsterling" "textstyle" "textsuperscript" "texttt" "textup"
        "textwidth" "thanks" "therefore" "theta" "thicklines" "thinlines"
        "thinspace" "thispagestyle" "tilde" "times" "tiny" "title" "to" "top"
        "topmargin" "topsep" "topskip" "triangle" "triangleleft" "triangleright"
        "tt" "ttfamily" "twocolumn" "typeout" "unboldmath" "underbrace"
        "underline" "unitlength" "uparrow" "updownarrow" "uplus" "upshape"
        "upsilon" "usebox" "usepackage" "varbigtriangledown" "varbigtriangleup"
        "varepsilon" "varphi" "varpi" "varrho" "varsigma" "vartheta" "vdash"
        "vdots" "vec" "vector" "vee" "vert" "vfil" "vfill" "vphantom" "vskip"
        "vspace" "vss" "vtop" "wedge" "widehat" "widetilde" "wp" "wr" "xi"
        "zeta"
        "Arrowvert" "Big" "Bigg" "Biggl" "Biggm" "Biggr" "Bigl" "Bigm" "Bigr"
        "Delta" "Downarrow" "Gamma" "Huge" "Im" "LARGE" "LaTeX" "LaTeXe"
        "Lambda" "Large" "Leftarrow" "Leftrightarrow" "Lleftarrow"
        "Longleftarrow" "Longleftrightarrow" "Longrightarrow" "Omega" "Phi" "Pi"
        "Pr" "Psi" "Re" "Rightarrow" "Sigma" "TeX" "Theta" "Uparrow"
        "Updownarrow" "Upsilon" "Vert" "Xi"

        ;; pLaTeX
        "gtfamily" "mathgt" "mathmc" "textgt" "textmc"
        ))

(ac-l-define-source
 ac-l-source-AMS-commands
 '("abs" "allowdisplaybreaks" "approxeq" "backepsilon" "backprime" "backsim"
   "backsimeq" "barwedge" "because" "beth" "between" "bigstar" "binom"
   "blacklozenge" "blackprime" "blacksquare" "blacktriangle" "blacktriangledown"
   "blacktriangleleft" "blacktriangleright" "boldsymbol" "boxdot" "boxed"
   "boxminus" "boxplus" "boxtimes" "bumpeq" "c@MaxMatrixCols" "cdots"
   "centerdot" "cfrac" "circeq" "circlearrowleft" "circlearrowright" "circledS"
   "circledast" "circledcirc" "circleddash" "complement" "curlyeqprec"
   "curlyeqsucc" "curlyvee" "curlywedge" "curvearrowleft" "curvearrowright"
   "daleth" "dbinom" "ddddot" "dddot" "dfrac" "diagdown" "diagup" "digamma"
   "displaybreak" "divideontimes" "doteqdot" "dotplus" "dotsb" "dotsc" "dotsi"
   "dotsm" "dotso" "doublebarwedge" "doublecap" "doublecup" "downdownarrows"
   "downharpoonleft" "downharpoonright" "eqcirc" "eqref" "eqsim" "eqslantgtr"
   "eqslantless" "eth" "fallingdotseq" "genfrac" "geqq" "geqslant" "ggg" "gggtr"
   "gimel" "gnapprox" "gneq" "gneqq" "gnsim" "gtrapprox" "gtrdot" "gtreqless"
   "gtreqqless" "gtrless" "gtrsim" "gvertneqq" "hdotsfor" "hslash" "idotsint"
   "iiiint" "iiint" "iint" "injlim" "intercal" "intertext" "lVert" "ldots"
   "leftarrowtail" "leftleftarrows" "leftrightarrows" "leftrightharpoons"
   "leftrightsquigarrow" "leftroot" "leftthreetimes" "leqq" "leqslant"
   "lessapprox" "lessdot" "lesseqgtr" "lesseqqgtr" "lessgtr" "lesssim" "lll"
   "llless" "lnapprox" "lneq" "lneqq" "lnsim" "looparrowleft" "looparrowright"
   "lozenge" "ltimes" "lvert" "lvertneqq" "mathbb" "mathcal" "mathfrak"
   "measuredangle" "medspace" "mspace" "multimap" "multlinegap" "nLeftarrow"
   "nLeftrightarrow" "nRightarrow" "nVDash" "nVdash" "namelimits" "ncong"
   "negmedspace" "negthickspace" "nexists" "ngeq" "ngeqq" "ngeqslant" "ngtr"
   "nleftarrow" "nleftrightarrow" "nleq" "nleqq" "nleqslant" "nless" "nmid"
   "nobreakdash" "nointlimits" "nonamelimits" "norm" "nosumlimits" "notag"
   "nparallel" "nprec" "npreceq" "nrightarrow" "nshortmid" "nshortparallel"
   "nsim" "nsubseteq" "nsubseteqq" "nsucc" "nsucceq" "nsupseteq" "nsupseteqq"
   "ntriangleleft" "ntrianglelefteq" "ntriangleright" "ntrianglerighteq"
   "numberwithin" "nvDash" "nvdash" "operatorname" "overleftrightarrow"
   "overset" "pitchfork" "pmb" "precapprox" "preccurlyeq" "precnapprox"
   "precneqq" "precnsim" "presim" "projlim" "rVert" "raisetag" "rightarrowtail"
   "rightleftarrows" "rightrightarrows" "rightsquigarrow" "rightthreetimes"
   "risingdotseq" "rtimes" "rvert" "shortmid" "shortparallel" "sideset"
   "smallfrown" "smallsetminus" "smallsmile" "sphat" "sphericalangle" "sptilde"
   "square" "subseteqq" "subsetneq" "subsetneqq" "succapprox" "succcurlyeq"
   "succnapprox" "succneqq" "succnsim" "succsim" "supseteqq" "supsetneq"
   "supsetneqq" "tag" "tagform@" "tbinom" "tfrac" "therefore" "thickapprox"
   "thicksim" "thickspace" "triangledown" "trianglelefteq" "triangleq"
   "trianglerighteq" "twoheadleftarrow" "twoheadrightarrow" "underleftarrow"
   "underleftrightarrow" "underrightarrow" "underset" "upharpoonleft"
   "upharpoonright" "uproot" "upuparrows" "vDash" "varDelta" "varGamma"
   "varLambda" "varOmega" "varPhi" "varPi" "varPsi" "varSigma" "varTheta"
   "varUpsilon" "varXi" "varinjlim" "varkappa" "varliminf" "varlimsup"
   "varnothing" "varprojlim" "varpropto" "varsubsetneq" "varsubsetneqq"
   "varsupsetneq" "varsupsetneqq" "vartriangle" "vartriangleleft"
   "vartriangleright" "veebar" "xleftarrow" "xrightarrow"
   "Bbbk" "Bumpeq" "Cap" "Cup" "DeclareMathOperator" "Doteq" "Finv" "Game"
   "Lleftarrow" "Lsh" "Rrightarrow" "Rsh" "Subset" "Supset" "Vdash" "Vvdash"
   ) "a" 1)

(ac-l-define-source
 ac-l-source-latexsym-commands
 '("leadsto" "lhd" "mho" "rhd" "sqsubset" "sqsupset" "unlhd" "unrhd"
   "Box" "Diamond" "Join"
   ) "s" 1)

(ac-l-define-source
 ac-l-source-hyperref-commands
 '("appendixautorefname" "autoref" "chapterautorefname" "equationautorefname"
   "figureautorefname" "footnoteautorefname" "href" "hyperbaseurl" "hyperdef"
   "hyperimage" "hyperlink" "hyperref" "hypersetup" "hypertarget"
   "itemautorefname" "nolinkurl" "pageautorefname" "paragraphautorefname"
   "partautorefname" "sectionautorefname" "subparagraphautorefname"
   "subsectionautorefname" "subsubsectionautorefname" "tableautorefname"
   "texorpdfstring" "theoremautorefname" "url"
   "Acrobatmenu" "AMSautorefname" "Hfootnoteautorefname" "Itemautorefname"
   ) "h" 1)

(ac-l-define-source
 ac-l-source-graphicx-commands
 '("graphicspath" "includegraphics" "reflectbox" "resizebox" "rotatebox"
   "scalebox"
   "DeclareGraphicsExtensions" "DeclareGraphicsRule"
   ) "g" 1)

(ac-l-define-source
 ac-l-source-color-commands
 '("color" "colorbox" "definecolor" "fcolorbox" "pagecolor" "textcolor"
   ) "c" 1)

(ac-l-define-source
 ac-l-source-colortbl-commands
 '("arrayrulecolor" "cellcolor" "columncolor" "doublerulesepcolor" "hhline"
   "minrowclearance" "rowcolor"
   ) "c" 1)

(ac-l-define-source
 ac-l-source-array-commands
 '("array" "arraybackslash" "endarray" "extrarowheight" "extratabsurround"
   "firsthline" "lasthline" "newcolumn" "newcolumntype" "showcols"
   ) "t" 1)

(ac-l-define-source
 ac-l-source-tabularx-commands
 '("endtabularx" "tabularx" "tabularxcolumn" "tracingtabularx"
   ) "t" 1)

(ac-l-define-source
 ac-l-source-longtable-commands
 '("endfirsthead" "endfoot" "endhead" "endlastfoot" "endlongtable" "longtable"
   "setlongtables" "tablename" "tabularnewline"
   "LTcapwidth" "LTchunksize" "LTleft" "LTmulticolumn" "LTpost" "LTpre"
   "LTright"
   ) "t" 1)

(ac-l-define-source
 ac-l-source-textcomp-commands
 '("textacutedbl" "textasciiacute" "textasciibreve" "textasciicaron"
   "textasciidieresis" "textasciigrave" "textasciimacron" "textasteriskcentered"
   "textbaht" "textbardbl" "textbigcircle" "textblank" "textborn"
   "textbrokenbar" "textbullet" "textcelsius" "textcent" "textcentoldstyle"
   "textcircledP" "textcolonmonetary" "textcopyleft" "textcopyright"
   "textcurrency" "textdagger" "textdaggerdbl" "textdblhyphen"
   "textdblhyphenchar" "textdegree" "textdied" "textdiscount" "textdiv"
   "textdivorced" "textdollar" "textdollaroldstyle" "textdong" "textdownarrow"
   "texteightoldstyle" "textestimated" "texteuro" "textfiveoldstyle"
   "textflorin" "textfouroldstyle" "textfractionsolidus" "textgravedbl"
   "textguarani" "textinterrobang" "textinterrobangdown" "textlangle"
   "textlbrackdbl" "textleaf" "textleftarrow" "textlira" "textlnot" "textlquill"
   "textmarried" "textmho" "textminus" "textmu" "textmusicalnote" "textnaira"
   "textnineoldstyle" "textnumero" "textohm" "textonehalf" "textoneoldstyle"
   "textonequarter" "textonesuperior" "textopenbullet" "textordfeminine"
   "textordmasculine" "textparagraph" "textperiodcentered" "textpertenthousand"
   "textperthousand" "textpeso" "textpilcrow" "textpm" "textquotesingle"
   "textquotestraightbase" "textquotestraightdblbase" "textrangle"
   "textrbrackdbl" "textrecipe" "textreferencemark" "textregistered"
   "textrightarrow" "textrquill" "textsection" "textservicemark"
   "textsevenoldstyle" "textsixoldstyle" "textsterling" "textsurd"
   "textthreeoldstyle" "textthreequarters" "textthreequartersemdash"
   "textthreesuperior" "texttildelow" "texttimes" "texttrademark"
   "texttwelveudash" "texttwooldstyle" "texttwosuperior" "textuparrow" "textwon"
   "textyen" "textzerooldstyle"
   ) "f" 4)

(ac-l-define-source
 ac-l-source-txfonts-commands
 '("alphaup" "betaup" "bignplus" "bigsqcap" "bigsqcapplus" "bigsqcupplus"
   "boxLeft" "boxRight" "boxast" "boxbar" "boxbslash" "boxdotLeft" "boxdotRight"
   "boxdotleft" "boxdotright" "boxleft" "boxright" "boxslash" "chiup"
   "circledbar" "circledbslash" "circleddotleft" "circleddotright" "circledgtr"
   "circledless" "circledvee" "circledwedge" "circleleft" "circleright"
   "colonapprox" "coloneq" "coloneqq" "colonsim" "cong" "dashleftarrow"
   "dashleftrightarrow" "dashrightarrow" "deltaup" "doteq" "epsilonup" "eqcolon"
   "eqqcolon" "etaup" "fint" "gammaup" "idotsint" "iiiint" "iiint" "iint"
   "invamp" "iotaup" "kappaup" "lJoin" "lambdabar" "lambdaslash" "lambdaup"
   "lbag" "leadsto" "leadstoext" "leftsquigarrow" "lhd" "llbracket"
   "longmappedfrom" "longmmappedfrom" "longmmapsto" "lrJoin" "lrtimes"
   "mathfrak" "mappedfrom" "medbullet" "medcirc" "mmappedfrom" "mmapsto"
   "multimapboth" "multimapbothvert" "multimapdot" "multimapdotboth"
   "multimapdotbothA" "multimapdotbothAvert" "multimapdotbothB"
   "multimapdotbothBvert" "multimapdotbothvert" "multimapdotinv" "multimapinv"
   "muup" "nBumpeq" "nSubset" "nSupset" "napprox" "napproxeq" "nasymp"
   "nbacksim" "nbacksimeq" "nbumpeq" "ne" "neq" "nequiv" "ngg" "ngtrapprox"
   "ngtrless" "ngtrsim" "nlessapprox" "nlessgtr" "nlesssim" "nll" "notin"
   "notni" "notowns" "nplus" "nprecapprox" "npreccurlyeq" "npreceqq" "nprecsim"
   "nsim" "nsimeq" "nsqsubset" "nsqsubseteq" "nsqsupset" "nsqsupseteq" "nsubset"
   "nsuccapprox" "nsucccurlyeq" "nsucceqq" "nsuccsim" "nsupset" "nthickapprox"
   "ntwoheadleftarrow" "ntwoheadrightarrow" "nuup" "nvarparallel"
   "nvarparallelinv" "oiiint" "oiiintclockwise" "oiiintctrclockwise" "oiint"
   "oiintclockwise" "oiintctrclockwise" "ointclockwise" "ointctrclockwise"
   "omegaup" "openJoin" "opentimes" "phiup" "piup" "preceqq" "psiup" "rJoin"
   "rbag" "rhd" "rhoup" "rrbracket" "sigmaup" "sqcapplus" "sqcupplus"
   "sqiiintop" "sqiintop" "sqint" "strictfi" "strictif" "strictiff" "succeqq"
   "tauup" "thetaup" "unlhd" "unrhd" "upsilonup" "varBbbk" "varclubsuit"
   "vardiamondsuit" "varepsilonup" "varg" "varheartsuit" "varmathbb"
   "varoiiintclockwise" "varoiiintctrclockwise" "varoiintclockwise"
   "varoiintctrclockwise" "varointclockwise" "varointctrclockwise" "varparallel"
   "varparallelinv" "varphiup" "varpiup" "varprod" "varrhoup" "varsigmaup"
   "varspadesuit" "varthetaup" "varg" "varv" "varw" "vary" "xiup" "zetaup"
   "Bot" "Colonapprox" "Coloneq" "Coloneqq" "Colonsim" "Diamond" "DiamondLeft"
   "DiamondRight" "Diamondblack" "Diamonddot" "DiamonddotLeft" "DiamonddotRight"
   "Diamonddotleft" "Diamonddotright" "Diamondleft" "Diamondright" "Eqcolon"
   "Eqqcolon" "Join" "Longmappedfrom" "Longmapsto" "Longmmappedfrom"
   "Longmmapsto" "Mappedfrom" "Mapsto" "Mmappedfrom" "Mmapsto" "Nearrow"
   "Nwarrow" "Perp" "Searrow" "Swarrow" "Top" "VDash" "VvDash" "Wr"
   ) "f" 1)

(ac-l-define-source
 ac-l-source-otf-commands
 '("ajBall" "ajBlackFlorette" "ajBlackSesame" "ajCheckmark" "ajCloud" "ajClub"
   "ajCommandKey" "ajDiamond" "ajDownBArrow" "ajDownHand" "ajDownScissors"
   "ajDownWArrow" "ajGoteMark" "ajHashigoTaka" "ajHeart" "ajHotSpring" "ajJIS"
   "ajKakko" "ajKakko*" "ajKakkoAlph" "ajKakkoHira" "ajKakkoKata" "ajKakkoRoman"
   "ajKakkoYobi" "ajKakkoalph" "ajKakkoroman" "ajKaku" "ajKaku*" "ajKakuAlph"
   "ajKakuHira" "ajKakuKata" "ajKakuYobi" "ajKakualph" "ajKoto" "ajKuroKaku"
   "ajKuroKaku*" "ajKuroKakuAlph" "ajKuroKakuHira" "ajKuroKakuKata"
   "ajKuroKakuYobi" "ajKuroKakualph" "ajKuroMaru" "ajKuroMaru*" "ajKuroMaruAlph"
   "ajKuroMaruHira" "ajKuroMaruKaku" "ajKuroMaruKaku*" "ajKuroMaruKakuAlph"
   "ajKuroMaruKakuHira" "ajKuroMaruKakuKata" "ajKuroMaruKakuYobi"
   "ajKuroMaruKakualph" "ajKuroMaruKata" "ajKuroMaruYobi" "ajKuroMarualph"
   "ajLeftBArrow" "ajLeftDownArrow" "ajLeftHand" "ajLeftScissors"
   "ajLeftUpArrow" "ajLeftWArrow" "ajLig{!!*}" "ajLig{!!}" "ajLig{!*}"
   "ajLig{!?*}" "ajLig{!?}" "ajLig{()}" "ajLig{?!}" "ajLig{??}" "ajLig{AM}"
   "ajLig{FAX}" "ajLig{F}" "ajLig{GB}" "ajLig{HP}" "ajLig{Hz}" "ajLig{K.K.}"
   "ajLig{KB}" "ajLig{KK.}" "ajLig{KK}" "ajLig{MB}" "ajLig{No.}" "ajLig{No}"
   "ajLig{PH}" "ajLig{PM}" "ajLig{PR}" "ajLig{TB}" "ajLig{TEL}" "ajLig{Tel}"
   "ajLig{VS}" "ajLig{a.m.}" "ajLig{a/c}" "ajLig{c.c.}" "ajLig{c/c}"
   "ajLig{c/o}" "ajLig{cal}" "ajLig{cc}" "ajLig{cm2}" "ajLig{cm3}" "ajLig{cm}"
   "ajLig{dB}" "ajLig{dl*}" "ajLig{dl}" "ajLig{g}" "ajLig{hPa}" "ajLig{in}"
   "ajLig{kcal}" "ajLig{kg}" "ajLig{kl*}" "ajLig{kl}" "ajLig{km2}" "ajLig{km3}"
   "ajLig{km}" "ajLig{l*}" "ajLig{l}" "ajLig{m/m}" "ajLig{m2}" "ajLig{m3}"
   "ajLig{mb}" "ajLig{mg}" "ajLig{mho}" "ajLig{microg}" "ajLig{microm}"
   "ajLig{micros}" "ajLig{min}" "ajLig{ml*}" "ajLig{ml}" "ajLig{mm2}"
   "ajLig{mm3}" "ajLig{mm}" "ajLig{ms}" "ajLig{m}" "ajLig{n/m}" "ajLig{ns}"
   "ajLig{ohm}" "ajLig{p.m.}" "ajLig{pH}" "ajLig{ps}" "ajLig{sec}" "ajLig{tel}"
   "ajLig{tm}" "ajLig{○}" "ajLig{●}" "ajLig{◇}" "ajLig{◆}" "ajLig{□}"
   "ajLig{■}" "ajLig{さじ}" "ajLig{アール*}" "ajLig{アール}" "ajLig{アト}"
   "ajLig{アパート}" "ajLig{アルファ}" "ajLig{アンぺア}" "ajLig{イニング}"
   "ajLig{インチ*}" "ajLig{インチ}" "ajLig{ウォン}" "ajLig{ウルシ}"
   "ajLig{エーカー}" "ajLig{エクサ}" "ajLig{エスクード}" "ajLig{オーム}"
   "ajLig{オングストローム*}" "ajLig{オングストローム}" "ajLig{オンス}"
   "ajLig{オントロ}" "ajLig{カイリ}" "ajLig{カップ}" "ajLig{カラット}"
   "ajLig{カロリー}" "ajLig{ガロン}" "ajLig{ガンマ}" "ajLig{キュリー}" "ajLig{キロ}"
   "ajLig{キログラム}" "ajLig{キロメートル}" "ajLig{キロリットル}" "ajLig{キロワット}"
   "ajLig{ギガ}" "ajLig{ギニー}" "ajLig{ギルダー}" "ajLig{クルサード}"
   "ajLig{クルゼイロ}" "ajLig{クローネ}" "ajLig{グスーム}" "ajLig{グラム*}"
   "ajLig{グラム}" "ajLig{グラムトン}" "ajLig{ケース}" "ajLig{コーポ*}"
   "ajLig{コーポ}" "ajLig{コルナ}" "ajLig{サイクル}" "ajLig{サンチーム}"
   "ajLig{シリング}" "ajLig{センチ*}" "ajLig{センチ}" "ajLig{セント*}"
   "ajLig{セント}" "ajLig{ダース}" "ajLig{テラ}" "ajLig{デカ}" "ajLig{デシ}"
   "ajLig{トン}" "ajLig{ドラクマ}" "ajLig{ドル}" "ajLig{ナノ}" "ajLig{ノット}"
   "ajLig{ハイツ*}" "ajLig{ハイツ}" "ajLig{バーツ}" "ajLig{バーレル}"
   "ajLig{バレル}" "ajLig{パーセント}" "ajLig{パスカル}" "ajLig{ビル}"
   "ajLig{ピアストル}" "ajLig{ピクル}" "ajLig{ピコ}" "ajLig{ファラッド}"
   "ajLig{ファラド}" "ajLig{フィート}" "ajLig{フェムト}" "ajLig{フラン}"
   "ajLig{ブッシェル}" "ajLig{ヘクタール}" "ajLig{ヘクト}" "ajLig{ヘクトパスカル}"
   "ajLig{ヘルツ*}" "ajLig{ヘルツ}" "ajLig{ベータ}" "ajLig{ページ*}" "ajLig{ページ}"
   "ajLig{ペセタ}" "ajLig{ペソ}" "ajLig{ペタ}" "ajLig{ペニヒ}" "ajLig{ペンス}"
   "ajLig{ホール}" "ajLig{ホーン*}" "ajLig{ホーン}" "ajLig{ホン}" "ajLig{ボルト}"
   "ajLig{ポイント}" "ajLig{ポンド}" "ajLig{マイクロ}" "ajLig{マイル}"
   "ajLig{マッハ}" "ajLig{マルク}" "ajLig{マンション}" "ajLig{ミクロン}"
   "ajLig{ミリ}" "ajLig{ミリバール}" "ajLig{メートル}" "ajLig{メガ}"
   "ajLig{メガトン}" "ajLig{ヤード*}" "ajLig{ヤード}" "ajLig{ヤール}"
   "ajLig{ユーロ}" "ajLig{ユアン}" "ajLig{ラド}" "ajLig{リットル}" "ajLig{リラ}"
   "ajLig{ルーブル}" "ajLig{ルクス}" "ajLig{ルピー}" "ajLig{ルピア}" "ajLig{レム}"
   "ajLig{レントゲン}" "ajLig{ワット*}" "ajLig{ワット}" "ajLig{医療法人}"
   "ajLig{学校法人}" "ajLig{株式会社}" "ajLig{共同組合}" "ajLig{協同組合}"
   "ajLig{合資会社}" "ajLig{合名会社}" "ajLig{財団法人}" "ajLig{社団法人}"
   "ajLig{宗教法人}" "ajLig{小}" "ajLig{昭和}" "ajLig{大正}" "ajLig{平成}"
   "ajLig{明治}" "ajLig{有限会社}" "ajLig{郵便番号}" "ajLig{゙}" "ajLig{゚}"
   "ajMaru" "ajMaru*" "ajMaruAlph" "ajMaruHira" "ajMaruKaku" "ajMaruKaku*"
   "ajMaruKakuAlph" "ajMaruKakuHira" "ajMaruKakuKata" "ajMaruKakuYobi"
   "ajMaruKakualph" "ajMaruKansuji" "ajMaruKata" "ajMaruYobi" "ajMarualph"
   "ajMasu" "ajMayuHama" "ajNijuMaru" "ajPeriod" "ajPhone" "ajPostal"
   "ajRecycle" "ajReturnKey" "ajRightBArrow" "ajRightDownArrow" "ajRightHand"
   "ajRightScissors" "ajRightUpArrow" "ajRightWArrow" "ajRoman" "ajSenteMark"
   "ajSnowman" "ajSpade" "ajSun" "ajTatsuSaki" "ajTsuchiYoshi" "ajTsumesuji1"
   "ajTsumesuji2" "ajTsumesuji3" "ajTsumesuji4" "ajUmbrella" "ajUpBArrow"
   "ajUpHand" "ajUpScissors" "ajUpWArrow" "ajUta" "ajVisibleSpace"
   "ajWhiteFlorette" "ajWhiteSesame" "ajYori" "ajroman" "ajvarClub"
   "ajvarDiamond" "ajvarHeart" "ajvarPostal" "ajvarSpade" "aj半角" "deluxe"
   ) "f" 2)

(ac-l-define-source
 ac-l-source-primitives
 '("above" "abovedisplayshortskip" "abovedisplayskip" "abovewithdelims" "accent"
   "adjdemerits" "advance" "afterassignment" "aftergroup" "atop"
   "atopwithdelims" "badness" "baselineskip" "batchmode" "begingroup"
   "belowdisplayshortskip" "belowdisplayskip" "binoppenalty" "botmark" "box"
   "boxmaxdepth" "brokenpenalty" "catcode" "char" "chardef" "cleaders" "closein"
   "closeout" "clubpenalty" "copy" "count" "countdef" "cr" "crcr" "csname" "day"
   "deadcycles" "def" "defaulthyphenchar" "defaultskewchar" "delcode"
   "delimiter" "delimiterfactor" "delimitershortfall" "dimen" "dimendef"
   "discretionary" "displayindent" "displaylimits" "displaystyle"
   "displaywidowpenalty" "displaywidth" "divide" "doublehyphendemerits" "dp"
   "dump" "edef" "else" "emergencystretch" "end" "endcsname" "endgroup"
   "endinput" "endlinechar" "eqno" "errhelp" "errmessage" "errorcontextlines"
   "errorstopmode" "escapechar" "everycr" "everydisplay" "everyhbox" "everyjob"
   "everymath" "everypar" "everyvbox" "exhyphenpenalty" "expandafter" "fam" "fi"
   "finalhyphendemerits" "firstmark" "floatingpenalty" "font" "fontdimen"
   "fontname" "futurelet" "gdef" "global" "globaldefs" "halign" "hangafter"
   "hangindent" "hbadness" "hbox" "hfil" "hfill" "hfilneg" "hfuzz" "hoffset"
   "holdinginserts" "hrule" "hsize" "hskip" "hss" "ht" "hyphenation"
   "hyphenchar" "hyphenpenalty" "if" "ifcase" "ifcat" "ifdim" "ifeof" "iffalse"
   "ifhbox" "ifhmode" "ifinner" "ifmmode" "ifnum" "ifodd" "iftrue" "ifvbox"
   "ifvmode" "ifvoid" "ifx" "ignorespaces" "immediate" "indent" "input"
   "inputlineno" "insert" "insertpenalties" "interlinepenalty" "jobname" "kern"
   "language" "lastbox" "lastkern" "lastpenalty" "lastskip" "lccode" "leaders"
   "left" "lefthyphenmin" "leftskip" "leqno" "let" "limits" "linepenalty"
   "lineskip" "lineskiplimit" "long" "looseness" "lower" "lowercase" "mag"
   "mark" "mathaccent" "mathbin" "mathchar" "mathchardef" "mathchoice"
   "mathclose" "mathcode" "mathinner" "mathop" "mathopen" "mathord" "mathpunct"
   "mathrel" "mathsurround" "maxdeadcycles" "maxdepth" "meaning" "medmuskip"
   "message" "mkern" "month" "moveleft" "moveright" "mskip" "multiply" "muskip"
   "muskipdef" "newlinechar" "noalign" "noboundary" "noexpand" "noindent"
   "nolimits" "nonscript" "nonstopmode" "nulldelimiterspace" "nullfont" "number"
   "omit" "openin" "openout" "or" "outer" "output" "outputpenalty" "over"
   "overfullrule" "overline" "overwithdelims" "pagedepth" "pagefilllstretch"
   "pagefillstretch" "pagefilstretch" "pagegoal" "pageshrink" "pagestretch"
   "pagetotal" "par" "parfillskip" "parindent" "parshape" "parskip" "patterns"
   "pausing" "penalty" "postdisplaypenalty" "predisplaypenalty" "predisplaysize"
   "pretolerance" "prevdepth" "prevgraf" "radical" "raise" "read" "relax"
   "relpenalty" "right" "righthyphenmin" "rightskip" "romannumeral" "scriptfont"
   "scriptscriptfont" "scriptscriptstyle" "scriptspace" "scriptstyle"
   "scrollmode" "setbox" "setlanguage" "sfcode" "shipout" "show" "showbox"
   "showboxbreadth" "showboxdepth" "showlists" "showthe" "skewchar" "skip"
   "skipdef" "spacefactor" "spaceskip" "span" "special" "splitbotmark"
   "splitfirstmark" "splitmaxdepth" "splittopskip" "string" "tabskip" "textfont"
   "textstyle" "the" "thickmuskip" "thinmuskip" "time" "toks" "toksdef"
   "tolerance" "topmark" "topskip" "tracingcommands" "tracinglostchars"
   "tracingmacros" "tracingonline" "tracingoutput" "tracingpages"
   "tracingparagraphs" "tracingrestores" "tracingstats" "uccode" "uchyph"
   "underline" "unhbox" "unhcopy" "unkern" "unpenalty" "unskip" "unvbox"
   "unvcopy" "uppercase" "vadjust" "valign" "vbadness" "vbox" "vcenter" "vfil"
   "vfill" "vfilneg" "vfuzz" "voffset" "vrule" "vsize" "vskip" "vsplit" "vss"
   "vtop" "wd" "widowpenalty" "write" "xdef" "xleaders" "xspaceskip" "year"

   ;; pTeX
   "autospacing" "autoxspacing" "euc" "inhibitxspcode" "jcharwindowpenalty"
   "jfam" "jfont" "jis" "kanjiskip" "kansuji" "kansujichar" "kuten"
   "noautospacing" "noautoxspacing" "postbreakpenalty" "prebreakpenalty"
   "showmode" "sjis" "tate" "tbaselineshift" "tfont" "xkanjiskip" "xspcode"
   "ybaselineshift" "yoko"
   ) "l" 1)

(ac-l-define-source
 ac-l-source-macro-commands
 '("@@end" "@@ifdefinable" "@@input" "@@par" "@Alph" "@M" "@MM" "@Roman"
   "@TeXversion" "@addtofilelist" "@addtoreset" "@afterheading"
   "@afterindentfalse" "@afterindenttrue" "@alph" "@arabic" "@author"
   "@badmath" "@beginparpenalty" "@biblabel" "@captype" "@centercr" "@chapapp"
   "@cite" "@clubpenalty" "@date" "@dblarg" "@dblfloat" "@dblfpbot" "@dblfpsep"
   "@dblfptop" "@dotsep" "@dottedtocline" "@empty" "@endparpenalty" "@endpart"
   "@eqnnum" "@evenfoot" "@evenhead" "@firstoftwo" "@float" "@fnsymbol"
   "@fontswitch" "@for" "@fpbot" "@fpsep" "@fptop" "@gobble" "@gobbletwo"
   "@gtempa" "@hangfrom" "@highpenalty" "@idxitem" "@ifclassloaded"
   "@ifdefinable" "@ifnextchar" "@ifpackageloaded" "@ifstar" "@ifundefined"
   "@ignoretrue" "@input" "@inputcheck" "@inputcheck0" "@itempenalty" "@ixpt"
   "@latex@error" "@listI" "@listi" "@listii" "@listiii" "@listiv" "@listv"
   "@listvi" "@lowpenalty" "@m" "@mainmatterfalse" "@mainmattertrue"
   "@makecaption" "@makechapterhead" "@makefnmark" "@makefntext" "@makeother"
   "@maketitle" "@maxdepth" "@medpenalty" "@minipagefalse" "@minipagerestore"
   "@minipagetrue" "@minus" "@mkboth" "@mparswitchfalse" "@mparswitchtrue"
   "@mpfn" "@mpfootins" "@ne" "@nil" "@nobreaktrue" "@nocounterr" "@nomath"
   "@normalsize" "@oddfoot" "@oddhead" "@openbib@code" "@part" "@plus"
   "@pnumwidth" "@providesfile" "@ptsize" "@restonecolfalse" "@restonecoltrue"
   "@roman" "@schapter" "@secondoftwo" "@sect" "@set@topoint" "@setfontsize"
   "@setpar" "@settopoint" "@site" "@spart" "@ssect" "@startsection"
   "@starttoc" "@svsec" "@svsechd" "@tabacckludge" "@tempboxa" "@tempcnta"
   "@tempdima" "@tempdimb" "@tempswa" "@tempswafalse" "@tempswatrue" "@testopt"
   "@textsuperscript" "@tfor" "@thanks" "@thefnmark" "@title" "@titlepagefalse"
   "@titlepagetrue" "@tocrmarg" "@topnum" "@twosidefalse" "@twosidetrue"
   "@undefined" "@viiipt" "@viipt" "@vipt" "@vpt" "@warning" "@whiledim"
   "@whileenum" "@whilesw" "@xdblarg" "@xiipt" "@xipt" "@xivpt" "@xpt"
   "@xsect" "@xviipt" "@xxpt" "@xxvpt"
   "aa" "abovecaptionskip" "addcontentsline" "ae" "alloc@" "and" "appendixname"
   "backmatter" "belowcaptionskip" "bgroup" "bibindent" "bibname"
   "bigskipamount" "botfigrule" "bottomfraction" "brokenpenalty"
   "c@bottomnumber" "c@chapter" "c@dbltopnumber" "c@enumi" "c@enumii"
   "c@enumiii" "c@enumiv" "c@figure" "c@footnote" "c@mpfootnote" "c@paragraph"
   "c@part" "c@secnumdepth" "c@section" "c@subparagraph" "c@subsection"
   "c@subsubsection" "c@tocdepth" "c@topnumber" "c@totalnumber" "changes"
   "chaptermark" "chaptername" "chardef" "check@mathfonts" "clubpenalty"
   "col@number" "contentsline" "contentsname" "copyright" "count@" "countdef"
   "day" "dblfloatpagefraction" "dblfloatsep" "dbltextfloatsep" "dbltopfraction"
   "dbltopnumber" "define@key" "displaywidowpenalty" "egroup" "end@dblfloat"
   "end@float" "endfigure" "endinupt" "endlinechar" "endlist" "endtrivlist"
   "errmessage" "escapechar" "ext@figure" "ext@table" "fboxrule" "fboxsep"
   "figurename" "file" "filename@parse" "firstmark" "floatpagefraction"
   "floatsep" "flushbottom" "fnsymbol" "fnum@figure" "fnum@table" "fontsize"
   "footnoterule" "fps@figure" "fps@table" "frontmatter" "ftype@figure"
   "ftype@table" "hb@xt@" "hss" "if@compatibility" "if@mainmatter"
   "if@mparswitch" "if@noskipsec" "if@openright" "if@restonecol" "if@tempswa"
   "if@titlepage" "if@twocolumn" "if@twoside" "ifcase" "ij" "imath" "indexname"
   "interlinepenalty" "intextsep" "iterate" "jmath" "jot" "l@chapter"
   "l@paragraph" "l@part" "l@section" "l@subparagraph" "l@subsection"
   "l@subsubsection" "labelenumi" "labelenumii" "labelenumiii" "labelenumiv"
   "labelitemi" "labelitemii" "labelitemiii" "labelitemiv" "lbrace" "ldots"
   "leavevmode" "leftmargin" "leftmargini" "leftmarginii" "leftmarginiii"
   "leftmarginiv" "leftmarginv" "leftmarginvi" "list" "listfigurename"
   "listtablename" "loop" "lq" "m@ne" "m@th" "mainmatter" "makeglossary"
   "marginpar" "markboth" "math@fontsfalse" "math@fontstrue" "mathdollar"
   "mathellipsis" "mathindent" "mathparagraph" "mathsection" "mathsterling"
   "maxdepth" "medskipamount" "meta" "month" "mpfootnote" "newblock" "newbox"
   "newcount" "newdimen" "newhelp" "newlanguage" "newmuskip" "newread" "newskip"
   "newtoks" "newwrite" "nfss@text" "nobreak" "nobreakspace" "nocorr"
   "noexpand" "normalbaselineskip" "normalmarginpar" "null" "num@figure"
   "number" "numberline" "oe" "onecolumn" "operator@font" "or" "overfullrule"
   "p@" "p@enumii" "p@enumiii" "p@enumiv" "paragraphmark" "parfillskip"
   "penalty" "postchaptername" "postdisplaypenalty" "postpartname" "pounds"
   "prechaptername" "predisplaypenalty" "prepartname" "protected@edef"
   "ps@headings" "pstyle" "raggedbottom" "raggedleft" "raggedright" "rbrace"
   "reDeclareMathAlphabet" "refname" "refstepcounter" "repeat" "reset@font"
   "reversemarginpar" "rq" "secdef" "sectionmark" "selectfont" "sf@size"
   "sfcode" "sixt@@n" "sloppy" "smallskipamount" "ss" "subitem"
   "subparagraphmark" "subsectionmark" "subsubitem" "subsubsectionmark"
   "tabbingsep" "tablename" "textasciicircum" "textasciitilde"
   "textasteriskcentered" "textbackslash" "textbar" "textbardbl" "textbraceleft"
   "textbraceright" "textbullet" "textcircled" "textcompwordmark"
   "textcopyright" "textdagger" "textdaggerdbl" "textdollar" "textellipsis"
   "textemdash" "textendash" "textexclamdown" "textfloatsep" "textfraction"
   "textgreater" "textless" "textordfeminine" "textordmasculine" "textparagraph"
   "textperiodcentered" "textquestiondown" "textquotedblleft"
   "textquotedblright" "textquoteleft" "textquoteright" "textregistered"
   "textsection" "textsterling" "texttrademark" "textunderscore"
   "textvisiblespace" "thechapter" "theenumi" "theenumii" "theenumiii"
   "theenumiv" "theequation" "thefigure" "thefootnote" "thempfootnote"
   "theparagraph" "thepart" "thesection" "thesubparagraph" "thesubsection"
   "thesubsubsection" "tocdepth" "today" "topfigrule" "topfraction" "trivlist"
   "tw@" "two@digits" "typeout" "viiipt" "viipt" "vipt" "widowpenalty" "wlog"
   "z@"
   "AA" "AE" "AtBeginDocument" "AtBeginDvi" "AtEndDocument" "AtEndOfPackage"
   "CheckSum" "CodelineIndex" "DeclareFontShape" "DeclareMathAlphabet"
   "DeclareOption" "DeclareRobustCommand" "DeclareSymbolFont"
   "DeclareSymbolFontAlphabet" "DeclareTextAccentDefault" "DeclareTextCommand"
   "DeclareTextCommandDefault" "DeclareTextFontCommand"
   "DeclareTextSymbolDefault" "DisableCrossrefs" "DoNotIndex" "DocInclude"
   "DocInput" "EnableCrossrefs" "ExecuteOptions" "Finale" "GetFileInfo" "IJ"
   "IfFileExists" "IndexColumns" "Lcount" "Lopt" "MacroIndent" "MakeShortVerb"
   "MakeUppercase" "NeedsTeXFormat" "OE" "OldMakeindex" "OnlyDescription"
   "PackageWarning" "PrintChanges" "PrintIndex" "ProcessOptions"
   "ProvidePackage" "ProvidesClass" "ProvidesFile" "RecordChanges"
   "RequirePackage" "SS" "SetSymbolFont" "StandardModuleDepth" "StopEventually"
   ) "l" 1)

(ac-l-define-source
 ac-l-source-latex-dot-ltx
 '("@@enc@update" "@@end" "@@endpbox" "@@eqncr" "@@fileswith@pti@ns" "@@hyph"
   "@@if@newlist" "@@ifdefinable" "@@input" "@@italiccorr" "@@line"
   "@@math@egroup" "@@par" "@@protect" "@@startpbox" "@@underline"
   "@@unprocessedoptions" "@@warning" "@Alph" "@DeclareMathDelimiter"
   "@DeclareMathSizes" "@Esphack" "@Roman" "@TeXversion" "@acci" "@accii"
   "@acciii" "@acol" "@acolampacol" "@activechar@info" "@addamp" "@addfield"
   "@addmarginpar" "@addtobot" "@addtocurcol" "@addtodblcol" "@addtofilelist"
   "@addtonextcol" "@addtopreamble" "@addtoreset" "@addtotoporbot"
   "@afterheading" "@alph" "@ampacol" "@arabic" "@argarraycr" "@argdef"
   "@argtabularcr" "@array" "@arrayacol" "@arrayclassiv" "@arrayclassz"
   "@arraycr" "@arrayparboxrestore" "@arrayrule" "@arstrut" "@author" "@auxout"
   "@backslashchar" "@badend" "@badlinearg" "@badmath" "@badpoptabs"
   "@badrequireerror" "@badtab" "@begin@tempboxa" "@begindocumenthook"
   "@begindvi" "@begintheorem" "@bezier" "@bibitem" "@biblabel" "@bitor"
   "@botlist" "@boxfpsbit" "@break@tfor" "@bsphack" "@caption" "@captype"
   "@car" "@carcube" "@cdr" "@centercr" "@cflb" "@cflt" "@changed@cmd"
   "@charlb" "@charrb" "@check@c" "@check@eq" "@checkend" "@circ" "@circle"
   "@circlefnt" "@cite" "@cite@ofmt" "@citea" "@citeb" "@citex" "@classi"
   "@classii" "@classiii" "@classiv" "@classoptionslist" "@classv" "@classz"
   "@cline" "@cls@pkg" "@clsextension" "@clubpenalty" "@combinedblfloats"
   "@combinefloats" "@comdblflelt" "@comflelt" "@cons" "@contfield" "@ctrerr"
   "@curr@enc" "@currbox" "@currdir" "@current@cmd" "@currentlabel"
   "@currenvir" "@currenvline" "@currext" "@currlist" "@currname"
   "@currnamestack" "@curroptions" "@currsize" "@date" "@dbflt" "@dblarg"
   "@dbldeferlist" "@dblfloat" "@dblfloatplacement" "@dbltoplist"
   "@dec@text@cmd" "@declaredoptions" "@declareoption" "@defaultsubs"
   "@defaultunits" "@defdefault" "@deferlist" "@definecounter" "@dischyph"
   "@doclearpage" "@documentclasshook" "@doendpe" "@dofilelist" "@donoparitem"
   "@dot" "@dottedtocline" "@downline" "@downvector" "@eha" "@ehb" "@ehc"
   "@ehd" "@elt" "@empty" "@emptycol" "@end@tempboxa" "@enddocumenthook"
   "@endfloatbox" "@endparenv" "@endpbox" "@endtheorem" "@enlargepage"
   "@ensuredmath" "@enumctr" "@eqncr" "@eqnnum" "@eqnsel" "@esphack"
   "@evenfoot" "@evenhead" "@expandtwoargs" "@expast" "@failedlist"
   "@filef@und" "@filelist" "@fileswfalse" "@fileswith@pti@ns"
   "@fileswith@ptions" "@fileswithoptions" "@fileswtrue" "@finalstrut"
   "@firstofone" "@firstoftwo" "@flcheckspace" "@flfail" "@float"
   "@floatboxreset" "@floatplacement" "@flsetnum" "@flsettextmin" "@flstop"
   "@flsucceed" "@fltovf" "@flupdates" "@fnsymbol" "@font@info"
   "@font@warning" "@fontswitch" "@footnotemark" "@footnotetext" "@for"
   "@forloop" "@fornoop" "@fps" "@fpsadddefault" "@frameb@x" "@framebox"
   "@framepicbox" "@freelist" "@getcirc" "@getfpsbit" "@getlarrow"
   "@getlinechar" "@getpen" "@getrarrow" "@gnewline" "@gobble" "@gobblecr"
   "@gobblefour" "@gobbletwo" "@gtempa" "@halignto" "@hangfrom" "@height"
   "@hline" "@hspace" "@hspacer" "@hvector" "@icentercr" "@iden" "@if"
   "@if@pti@ns" "@if@ptions" "@ifatmargin" "@ifclasslater" "@ifclassloaded"
   "@ifclasswith" "@ifdefinable" "@iffileonpath" "@ifl@aded" "@ifl@t@r"
   "@ifl@ter" "@ifnch" "@ifnextchar" "@iforloop" "@ifpackagelater"
   "@ifpackageloaded" "@ifpackagewith" "@iframebox" "@iframepicbox" "@ifstar"
   "@ifundefined" "@ignorefalse" "@ignoretrue" "@iiiminipage" "@iiiparbox"
   "@iiminipage" "@iinput" "@iiparbox" "@iirsbox" "@imakebox" "@imakepicbox"
   "@iminipage" "@include" "@index" "@inmatherr" "@inmathwarn" "@input"
   "@input@" "@inputcheck" "@iparbox" "@irsbox" "@isavebox" "@isavepicbox"
   "@ishortstack" "@istackcr" "@itabcr" "@item" "@itemfudge" "@itemitem"
   "@itemlabel" "@iwhiledim" "@iwhilenum" "@iwhilesw" "@ixpt" "@ixstackcr"
   "@killglue" "@largefloatcheck" "@latex@error" "@latex@info"
   "@latex@info@no@line" "@latex@warning" "@latex@warning@no@line" "@latexbug"
   "@latexerr" "@lbibitem" "@leftmark" "@let@token" "@lign" "@linefnt"
   "@listdepth" "@listfiles" "@loadwithoptions" "@ltab" "@mainaux" "@makebox"
   "@makecol" "@makefcolumn" "@makefnmark" "@makeother" "@makepicbox"
   "@makespecialcolbox" "@marginparreset" "@markright" "@maxdepth" "@midlist"
   "@minipagefalse" "@minipagerestore" "@minipagetrue" "@minus"
   "@missingfileerror" "@mkboth" "@mklab" "@mkpream" "@mpargs" "@mpfn"
   "@mpfootnotetext" "@multiplelabels" "@multiput" "@multispan" "@namedef"
   "@nameuse" "@nbitem" "@needsf@rmat" "@needsformat" "@newcommand" "@newctr"
   "@newenv" "@newenva" "@newenvb" "@newl" "@newl@bel" "@newline" "@next"
   "@nnil" "@no@font@optfalse" "@no@lnbk" "@no@pgbk" "@nobreakfalse"
   "@nobreaktrue" "@nocnterr" "@nocounterr" "@nodocument" "@noitemerr"
   "@noligs" "@nolnerr" "@nomath" "@normalsize" "@notdefinable" "@notprerr"
   "@nthm" "@obsoletefile" "@oddfoot" "@oddhead" "@onefilewithoptions"
   "@onelevel" "@onlypreamble" "@opargbegintheorem" "@opcol" "@options" "@othm"
   "@outputdblcol" "@outputpage" "@oval" "@ovhorz" "@ovvert" "@p@pfilename"
   "@par" "@parboxrestore" "@parboxto" "@parmoderr" "@parse@version" "@partaux"
   "@partlist" "@partswfalse" "@partswtrue" "@pass@ptions" "@penup"
   "@percentchar" "@picture" "@picture@warn" "@pkgextension" "@plus"
   "@popfilename" "@pr@videpackage" "@preamble" "@preamblecmds" "@preamerr"
   "@process@pti@ns" "@process@ptions" "@protected@testopt" "@providesfile"
   "@ptionlist" "@pushfilename" "@put" "@qend" "@qrelax" "@rc@ifdefinable"
   "@reargdef" "@refundefined" "@reinserts" "@removeelement" "@reset@ptions"
   "@resetactivechars" "@resethfps" "@restorepar" "@rightmark" "@roman"
   "@rsbox" "@rtab" "@rule" "@sanitize" "@savebox" "@savemarbox" "@savepicbox"
   "@scolelt" "@sdblcolelt" "@seccntformat" "@secondoftwo" "@sect" "@seqncr"
   "@setckpt" "@setfloattypecounts" "@setfontsize" "@setfpsbit" "@setminipage"
   "@setnobreak" "@setpar" "@setref" "@setsize" "@settab" "@settodim"
   "@settopoint" "@sharp" "@shortstack" "@sline" "@slowromancap" "@spaces"
   "@specialoutput" "@specialstyle" "@sptoken" "@sqrt" "@ssect" "@stackcr"
   "@star@or@long" "@startcolumn" "@startdblcolumn" "@startfield" "@startline"
   "@startpbox" "@startsection" "@starttoc" "@stopfield" "@stopline" "@stpelt"
   "@svector" "@sverb" "@svsec" "@svsechd" "@sxverbatim" "@tabacckludge"
   "@tabacol" "@tabarray" "@tabclassiv" "@tabclassz" "@tabcr" "@tablab"
   "@tabminus" "@tabplus" "@tabrj" "@tabular" "@tabularcr" "@tempdimb"
   "@tempswa" "@tempswafalse" "@tempswatrue" "@testdef" "@testfalse" "@testfp"
   "@testopt" "@testpach" "@testtrue" "@text@composite" "@text@composite@x"
   "@textbottom" "@textsuperscript" "@texttop" "@tf@r" "@tfor" "@tforloop"
   "@thanks" "@thefnmark" "@thefoot" "@thehead" "@themargin" "@themark"
   "@thirdofthree" "@thm" "@thmcounter" "@thmcountersep" "@title" "@toodeep"
   "@toplist" "@topnewpage" "@trivlist" "@tryfcolumn" "@trylist"
   "@twoclasseserror" "@twoloadclasserror" "@typein" "@typeset"
   "@typeset@protect" "@uclclist" "@unexpandable@protect" "@unknownoptionerror"
   "@unprocessedoptions" "@unused" "@unusedoptionlist" "@upline" "@upordown"
   "@upvector" "@use@ption" "@use@text@encoding" "@verb" "@verbatim" "@viiipt"
   "@viipt" "@vipt" "@vline" "@vobeyspaces" "@vpt" "@vspace" "@vspacer"
   "@vtryfc" "@vvector" "@warning" "@wckptelt" "@whiledim" "@whilenum"
   "@whilesw" "@wrglossary" "@wrindex" "@writeckpt" "@writefile"
   "@wrong@font@char" "@wtryfc" "@x@protect" "@x@sf" "@xDeclareMathDelimiter"
   "@xaddvskip" "@xargarraycr" "@xargdef" "@xarraycr" "@xbitor" "@xcentercr"
   "@xdblarg" "@xdblfloat" "@xeqncr" "@xexnoop" "@xexpast" "@xfloat"
   "@xfootnote" "@xfootnotemark" "@xfootnotenext" "@xhline" "@xifnch" "@xiipt"
   "@xipt" "@xivpt" "@xmpar" "@xnewline" "@xnext" "@xnthm" "@xprocess@ptions"
   "@xpt" "@xsect" "@xtabcr" "@xtabularcr" "@xthm" "@xtryfc" "@xtypein"
   "@xverbatim" "@xviipt" "@xxDeclareMathDelimiter" "@xxpt" "@xxvpt" "@xympar"
   "@yargarraycr" "@yargd@f" "@yargdef" "@yeqncr" "@ympar" "@ynthm" "@ythm"
   "@ytryfc" "@ztryfc"
   "aa" "accent@spacefactor" "active@math@prime" "add@accent" "addcontentsline"
   "addpenalty" "addto@hook" "addtocontents" "addtocounter" "addtolength"
   "addtoversion" "addvspace" "alloc@" "allowbreak" "alph" "alpha@elt"
   "alpha@list" "and" "arabic" "arccos" "arcsin" "arctan" "areRobustCommand"
   "arg" "array" "arraystretch" "author" "baselinestretch" "begin" "best@size"
   "bezier" "bfseries" "bgroup" "bibcite" "bibdata" "bibitem" "bibliography"
   "bibliographystyle" "bibstyle" "bigbreak" "biggl" "biggm" "biggr" "bigl"
   "bigm" "bigr" "bigskip" "bm@b" "bm@c" "bm@l" "bm@r" "bm@s" "bm@t" "bmod"
   "boldmath" "bordermatrix" "botfigrule" "bottomfraction" "brace" "brack"
   "break" "buildrel" "c@errorcontextlines" "calculate@math@sizes" "caption"
   "cases" "cdp@elt" "cdp@list" "center" "centering" "centerline" "cf@encoding"
   "ch@ck" "chardef@text@cmd" "check@command" "check@icl" "check@icr"
   "check@mathfonts" "check@nocorr@" "check@range" "check@single" "choose"
   "circle" "citation" "cite" "cl@@ckpt" "cl@page" "cleardoublepage"
   "clearpage" "cline" "closein" "closeout" "color@begingroup" "color@endbox"
   "color@endgroup" "color@hbox" "color@setgroup" "color@vbox" "contentsline"
   "copyright" "cos" "cosh" "cot" "coth" "cr" "csc" "curr@fontshape"
   "curr@math" "curr@math@size" "dag" "dashbox" "date" "dblfigrule"
   "dblfloatpagefraction" "dbltopfraction" "ddag" "declare@robustcommand"
   "default@M" "default@T" "default@ds" "default@family" "default@mextra"
   "default@series" "default@shape" "defaulthyphenchar" "defaultscriptratio"
   "defaultscriptscriptratio" "defaultskewchar" "define@mathalphabet"
   "define@mathgroup" "define@newfont" "deg" "depth" "det" "dim" "dimen@"
   "displ@y" "displaylines" "displaymath" "do" "do@noligs"
   "do@subst@correction" "document" "document@select@group" "documentclass"
   "documentstyle" "dorestore@version" "dospecials" "dotfill" "dots" "ds@"
   "eject" "em" "empty" "empty@sfcnt" "enc@update" "end" "end@dblfloat"
   "end@float" "endarray" "endcenter" "enddisplaymath" "enddocument"
   "endenumerate" "endeqnarray" "endequation" "endfilecontents" "endflushleft"
   "endflushright" "endgraf" "enditemize" "endline" "endlinechar" "endlist"
   "endlrbox" "endmath" "endminipage" "endpicture" "endsloppypar" "endtabbing"
   "endtabular" "endtrivlist" "endverbatim" "enlargethispage" "enskip" "enspace"
   "ensuremath" "enumerate" "eqnarray" "equation" "err@rel@i" "error@fontshape"
   "every@math@size" "everydisplay" "everymath" "execute@size@function" "exp"
   "external@font" "extra@def" "extracolsep" "extract@alph@from@version"
   "extract@font" "extract@rangefontinfo" "extract@sizefn" "f@baselineskip"
   "f@encoding" "f@family" "f@linespread" "f@series" "f@shape" "f@size"
   "f@user@size" "fbox" "filbreak" "filec@ntents" "filecontents"
   "filename@area" "filename@base" "filename@dot" "filename@ext"
   "filename@parse" "filename@path" "filename@simple" "finph@nt" "finsm@sh"
   "firstmark" "fix@penalty" "fixed@sfcnt" "floatpagefraction" "flushbottom"
   "flushleft" "flushright" "fmtname" "fmtversion" "fmtversion@topatch"
   "fnsymbol" "font@info" "font@name" "font@submax" "fontencoding" "fontfamily"
   "fontseries" "fontshape" "fontsize" "fontsubfuzz" "footnote" "footnotemark"
   "footnoterule" "footnotetext" "frac" "frame" "framebox" "frenchspacing"
   "frozen@everydisplay" "frozen@everymath" "fussy" "g@addto@macro" "gcd"
   "gen@sfcnt" "genb@sfcnt" "genb@x" "genb@y" "get@cdp" "get@external@font"
   "getanddefine@fonts" "glb@currsize" "glb@settings" "glossary" "goodbreak"
   "group@elt" "group@list" "hb@xt@" "height" "hexnumber@" "hgl@" "hglue"
   "hidewidth" "hline" "hmode@bgroup" "hmode@start@before@group" "hom"
   "hphantom" "hrulefill" "hspace" "hss" "ialign" "if@filesw" "if@ignore"
   "if@minipage" "if@newlist" "if@no@font@opt" "if@nobreak" "if@noskipsec"
   "if@partsw" "if@tempswa" "if@test" "ifnot@nil" "ignorespacesafterend" "in@"
   "in@@" "include" "includeonly" "index" "inf" "init@restore@glb@settings"
   "init@restore@version" "input" "input@path" "install@mathalphabet"
   "interfootnotelinepenalty" "is@range" "item" "itemize" "iterate" "itshape"
   "ker" "kernel@ifnextchar" "kill" "l@ngrel@x" "label" "labelsep" "labelwidth"
   "last@fontshape" "lbrack" "lccode" "ldots" "leadsto" "leavevmode" "lefteqn"
   "leftline" "leftmark" "lg" "lhd" "lim" "liminf" "limsup" "line" "linebreak"
   "linespread" "linethickness" "list" "listfiles" "llap" "ln" "log"
   "loggingall" "loggingoutput" "loop" "lq" "lrbox" "ltx@sh@ft" "m@th"
   "magstep" "magstephalf" "makeatletter" "makeatother" "makebox"
   "makeglossary" "makeindex" "makelabel" "makeph@nt" "makesm@sh"
   "mandatory@arg" "marginpar" "markboth" "markright" "math" "math@bgroup"
   "math@egroup" "math@fonts" "math@version" "mathalpha" "mathchar@type"
   "mathchardef" "mathellipsis" "mathgroup" "mathhexbox" "mathpalette"
   "mathph@nt" "mathsm@sh" "mathstrut" "mathversion" "matrix" "max" "maxdepth"
   "maybe@ic" "maybe@ic@" "mb@b" "mb@l" "mb@r" "mb@t" "mbox" "mdseries"
   "medbreak" "medskip" "mho" "min" "minipage" "multicolumn" "multiput"
   "multispan" "narrower" "negthinspace" "new@command" "new@environment"
   "new@fontshape" "new@mathalphabet" "new@mathgroup" "new@mathversion"
   "new@symbolfont" "newbox" "newcommand" "newcount" "newcounter" "newdimen"
   "newenvironment" "newfam" "newfont" "newhelp" "newif" "newinsert" "newlabel"
   "newlanguage" "newlength" "newline" "newmathalphabet" "newmathalphabet@@"
   "newmathalphabet@@@" "newmuskip" "newpage" "newread" "newsavebox" "newskip"
   "newtheorem" "newtoks" "newwrite" "nfss@catcodes" "nfss@text"
   "no@alphabet@error" "noaccents@" "noboundary" "nobreak" "nobreakdashes"
   "nobreakspace" "nocite" "nocorr" "nocorrlist" "noexpand" "nofiles"
   "nointerlineskip" "nolinebreak" "non@alpherr" "nonfrenchspacing" "nonumber"
   "nopagebreak" "normalbaselines" "normalcolor" "normalfont" "normalmarginpar"
   "normalsfcodes" "normalsize" "not@base" "not@math@alphabet" "null"
   "numberline" "o@lign" "oalign" "obeycr" "obeylines" "obeyspaces"
   "offinterlineskip" "oldstylenums" "on@line" "onecolumn" "ooalign" "openout"
   "openup" "optional@arg" "outer@nobreak" "oval" "pagebreak" "pagenumbering"
   "pageref" "pagestyle" "par" "paragraphmark" "parbox" "partopsep" "ph@nt"
   "phantom" "pickup@font" "pictur@" "picture" "pmatrix" "pmod" "poptabs"
   "pounds" "pr@@@s" "pr@@@t" "pr@m@s" "preload@sizes" "prim@s"
   "process@table" "protect" "protected" "protected@edef" "protected@xdef"
   "provide@command" "providecommand" "ps@empty" "ps@plain" "pushtabs" "put"
   "qbezier" "qbeziermax" "qquad" "quad" "r@@t" "raggedbottom" "raggedleft"
   "raggedright" "raisebox" "rbrack" "ref" "refstepcounter" "remove@angles"
   "remove@star" "remove@to@nnil" "removelastskip" "renew@command"
   "renew@environment" "renewcommand" "renewenvironment" "repeat" "reserved@a"
   "reserved@b" "reserved@c" "reserved@d" "reserved@e" "reserved@f"
   "reset@font" "restglb@settings" "restore@mathversion" "restore@protect"
   "restorecr" "reversemarginpar" "rhd" "rightline" "rightmark" "rlap"
   "rmfamily" "roman" "root" "rq" "rule" "samepage" "savebox" "sb" "sbox"
   "scan@@fontshape" "scan@fontshape" "scshape" "sec" "secdef" "sectionmark"
   "select@group" "selectfont" "set@@mathdelimiter" "set@color"
   "set@display@protect" "set@fontsize" "set@mathaccent" "set@mathchar"
   "set@mathdelimiter" "set@mathsymbol" "set@simple@size@args"
   "set@size@funct@args" "set@size@funct@args@" "set@typeset@protect"
   "setcounter" "setlength" "settodepth" "settoheight" "settowidth" "sffamily"
   "sh@ft" "shortstack" "showhyphens" "showoutput" "showoverfull" "sin" "sinh"
   "size@update" "sizefn@info" "skip@" "slash" "sloppy" "sloppypar" "slshape"
   "smallbreak" "smallskip" "smash" "sp" "sp@n" "space" "split@name" "sqrt"
   "sqsubset" "sqsupset" "ssf@size" "stackrel" "stepcounter" "stop" "stretch"
   "strip@prefix" "strip@pt" "strut" "sub@sfcnt" "subf@sfcnt"
   "subparagraphmark" "subsectionmark" "subst@correction" "subst@fontshape"
   "subsubsectionmark" "sup" "suppressfloats" "sw@slant" "symbol" "t@st@ic"
   "tabbing" "tabular" "tabularnewline" "tan" "tanh" "text@command"
   "textellipsis" "textfont@name" "textfraction" "textsuperscript" "tf@size"
   "thanks" "thefootnote" "thempfn" "thempfootnote" "thepage" "thicklines"
   "thinlines" "thinspace" "thispagestyle" "title" "today" "topfigrule"
   "topfraction" "totalheight" "tracingall" "tracingfonts" "trivlist"
   "try@load@fontshape" "try@simple@size" "try@simples" "try@size@range"
   "try@size@substitution" "tryif@simple" "ttfamily" "two@digits" "twocolumn"
   "typein" "typeout" "uccode" "unboldmath" "underbar" "underline" "unhbox"
   "unlhd" "unrestored@protected@xdef" "unrhd" "upshape" "use@mathgroup"
   "usebox" "usecounter" "usefont" "usepackage" "value" "vector" "verb"
   "verb@balance@group" "verb@egroup" "verb@eol@error" "verbatim"
   "verbatim@font" "verbatim@nolig@list" "version@elt" "version@list" "vgl@"
   "vglue" "vline" "vphantom" "vspace" "warn@rel@i" "width" "wlog" "write"
   "wrong@fontshape" "x@protect" "zap@space"
   "AA" "Alph" "AtBeginDocument" "AtBeginDvi" "AtEndDocument" "AtEndOfClass"
   "AtEndOfPackage" "Biggl" "Biggm" "Biggr" "Bigl" "Bigm" "Bigr" "Box"
   "CheckCommand" "ClassError" "ClassInfo" "ClassWarning" "ClassWarningNoLine"
   "CurrentOption" "DeclareErrorFont" "DeclareFixedFont" "DeclareFontEncoding"
   "DeclareFontEncoding@" "DeclareFontEncodingDefaults" "DeclareFontFamily"
   "DeclareFontShape" "DeclareFontShape@" "DeclareFontSubstitution"
   "DeclareMathAccent" "DeclareMathAlphabet" "DeclareMathDelimiter"
   "DeclareMathRadical" "DeclareMathSizes" "DeclareMathSymbol"
   "DeclareMathVersion" "DeclareOldFontCommand" "DeclareOption"
   "DeclarePreloadSizes" "DeclareRobustCommand" "DeclareSizeFunction"
   "DeclareSymbolFont" "DeclareSymbolFontAlphabet" "DeclareSymbolFontAlphabet@"
   "DeclareTextAccent" "DeclareTextAccentDefault" "DeclareTextCommand"
   "DeclareTextCommandDefault" "DeclareTextComposite"
   "DeclareTextCompositeCommand" "DeclareTextFontCommand" "DeclareTextSymbol"
   "DeclareTextSymbolDefault" "Diamond" "ExecuteOptions" "G@refundefinedtrue"
   "GenericError" "GenericInfo" "GenericWarning" "IfFileExists"
   "InputIfFileExists" "Join" "LaTeX" "LaTeXe" "LastDeclaredEncoding"
   "LoadClass" "LoadClassWithOptions" "MakeLowercase" "MakeUppercase"
   "MessageBreak" "NeedsTeXFormat" "OptionNotUsed" "PackageError" "PackageInfo"
   "PackageWarning" "PackageWarningNoLine" "PassOptionsToClass"
   "PassOptionsToPackage" "Pr" "ProcessOptions" "ProvideTextCommand"
   "ProvideTextCommandDefault" "ProvidesClass" "ProvidesFile" "ProvidesPackage"
   "RequirePackage" "RequirePackageWithOptions" "Roman" "SetMathAlphabet"
   "SetMathAlphabet@" "SetSymbolFont" "SetSymbolFont@" "TeX"
   "TextSymbolUnavailable" "UndeclareTextCommand" "UseTextAccent"
   "UseTextSymbol"
   ) "l" 1)

(provide 'auto-complete-latex-lib)

;;; auto-complete-latex-lib.el ends here
