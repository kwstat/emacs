% (>>>FILE<<<)
% Time-stamp: (>>>DIR<<<)(>>>FILE<<<)

\documentclass[12pt]{article}

\usepackage{wright-knit}

\begin{document}

\title{Some title}
\author{Kevin Wright}
\maketitle
\thispagestyle{fancy}

% Setup stuff.
<<SETUP, echo=FALSE, results="hide">>=
opts_chunk$set(fig.path="figs/(>>>FILE_SANS<<<)-", fig.align="center", 
  fig.width=6, fig.height=4, background="#fcfcfc", comment=NA)
knit_hooks$set(output = function(x, options) {
  paste('\\begin{Soutput}\n', x, '\\end{Soutput}\n', sep = '')
})
options(width=90)
if(!file.exists("figs")) dir.create("figs")
@
% ----------------------------------------------------------------------------
\section{Setup}
<<message=FALSE>>=
require("agridat")
require("asreml")
require("kw")
@
% ----------------------------------------------------------------------------
\section{}

% ----------------------------------------------------------------------------
\section*{Appendix}
This document was prepared \shorttoday\ with the following configuration:
<<FINISH, echo=FALSE, results="asis">>=
knit_hooks$set(output = function(x, options) { x })
toLatex(sessionInfo(), locale=FALSE)
@
\bibliography{c:/x/notes/kw}
\end{document}
