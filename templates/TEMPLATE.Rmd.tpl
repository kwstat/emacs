---
title: ""
author: "Kevin Wright"
date: ""
bibliography: c:/Dropbox/notes/refs.bib
latex_engine: xelatex
fontsize: 11pt
output:
  pdf_document: default
  html_document:
    theme: cerulean
    highlight: tango
  prettydoc::html_pretty:
    theme: cayman
    highlight: vignette
header-includes:
  - \usepackage[utf8]{inputenc} # For Swedish characters
  - \usepackage{lmodern} # not pixelated like cmbright
  - \usepackage{microtype} # need to disable ligatures
  - \DisableLigatures{encoding = *, family = * }
  - \usepackage{inconsolata} # load after lmodern for sanserif code
  - \usepackage[cm]{sfmath} # For Computer Modern fonts in equations
  - \usepackage[document]{ragged2e} # ragged right for whole doc
  - \usepackage[margin=1in]{geometry} # cannot set to 2cm for some reason

---
<style type="text/css">
body, td { font-size: 14pt; color: #000; padding:1em; }
code, pre, #ws, #message {border: 0px; font-size: 12pt;
  color: #000; background-color: #f5f5f5; }
code {border: 0px solid #e0e0e0; padding: 0 5px;}
pre > code { border: 0; }
</style>

# Abstract

# R setup

```{r setup, message=FALSE}
library("knitr")
knitr::opts_chunk$set(fig.align="center", fig.width=6, fig.height=6)
options(width=90)
library(pacman)
p_load(asreml, dplyr, fs, janitor, kw, lattice, readr, readxl, reshape2, tibble)
library(rmarkdown)

```
# Data preparation

```{r data}
setwd("(>>>DIR<<<)")
dat0 <- import("(>>>FILE_SANS<<<)")
dat <- dat0
dat <- nick(dat, loc=Location)
dat <- transformat(dat, loc=factor(loc))
str(dat)
describe(dat)
```

# Exploratory graphics

```{r eda}
```

	
