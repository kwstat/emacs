# (>>>FILE<<<)
# Time-stamp: <09 Feb 2021 10:03:50 c:/one/emacs/templates/TEMPLATE.R.tpl>

library(pacman)
p_load(asreml,dplyr,fs,janitor,kw,lattice,readxl,readr,reshape2,tibble)
asreml.options(colourise=FALSE)

setwd("(>>>DIR<<<)")
dat0 <- read_excel("(>>>FILE_SANS<<<).xlsx")
dat0 <- read_csv("(>>>FILE_SANS<<<).csv")
dat <- janitor::clean_names(dat0)
dat <- mutate_at(dat, vars(), factor)
head(dat)


