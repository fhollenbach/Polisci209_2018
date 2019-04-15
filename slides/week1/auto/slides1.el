(TeX-add-style-hook
 "slides1"
 (lambda ()
   (TeX-run-style-hooks
    "latex2e"
    "beamer"
    "beamer10"
    "tikz"
    "dcolumn"
    "booktabs"
    "comment"
    "color"
    "colortbl"
    "xcolor")
   (TeX-add-symbols
    "boxit"
    "sym")
   (LaTeX-add-environments
    "hypothesis"))
 :latex)

