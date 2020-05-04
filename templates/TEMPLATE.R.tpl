# (>>>FILE<<<)
# Time-stamp: <05 Feb 2018 14:40:41 c:/Dropbox/emacs/templates/TEMPLATE.R.tpl>

libs(asreml,dplyr,fs,janitor,kw,lattice,readxl,readr,reshape2,tibble)

setwd("(>>>DIR<<<)")
dat0 <- read_excel("(>>>FILE_SANS<<<).xlsx")
dat0 <- read_csv("(>>>FILE_SANS<<<).csv")
dat <- janitor::clean_names(dat0)
head(dat)


