---
title: ""
author: "Kevin Wright"
date: "2021"
bibliography: c:/one/notes/refs.bib
geometry: "margin=2cm"
fontsize: 11pt
urlcolor: blue
output:
  pdf_document:
    number_sections: true
    includes:
      in_header: "wright_rmd.sty"
  html_document:
    theme: cerulean
    highlight: tango
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
asreml.options(colourise=FALSE)
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

	
