setwd('~/Dropbox/Projects')

library(devtools)

document('TriMatch')
install('TriMatch', build_vignettes=FALSE)
install('TriMatch')
build('TriMatch')
check('TriMatch', cran=TRUE)

library(TriMatch)
ls('package:TriMatch')

data(nmes)
data(tutoring)

tools::checkRdaFiles('TriMatch/data')
tools::resaveRdaFiles('TriMatch/data')

tools::checkRdaFiles('TriMatch/inst/doc')
tools::resaveRdaFiles('TriMatch/inst/doc')

require(cacheSweave); require(tools)
setwd('TriMatch/inst/doc/')
Sweave("TriMatch.Rnw", driver = cacheSweaveDriver)
texi2pdf('TriMatch.tex')


##### Rd2markdown ##############################################################

frontMatter <- "---
layout: mathjax	
title: TriMatch
subtitle: Propensity Score Matching for Non-Binary Treatments
published: true
status: publish
submenu: trimatch
---
"
outdir <- '~/Dropbox/Projects/jbryer.github.com/TriMatch/docs/'
pkg <- 'TriMatch'
results <- Rd2markdown(pkg, outdir=outdir, front.matter=frontMatter)
names(results)

##### nmes data prep ###########################################################
require(tools)
nmes <- read.csv("TriMatch/pscore/nmesdata.txt", header=T)
nmes <- nmes[!is.na(nmes$smoke),]
nmes$smoke <- factor(nmes$smoke, ordered=F)
nmes$RACE3 <- factor(nmes$RACE3, ordered=F)
nmes$marital <- factor(nmes$marital, ordered=F)
nmes$SREGION <- factor(nmes$SREGION, ordered=F)
nmes$educate <- factor(nmes$SREGION, ordered=F)
nmes$beltuse <- factor(nmes$beltuse, ordered=F)
nmes$beltuse <- factor(nmes$beltuse, ordered=F)
nmes$POVSTALB <- factor(nmes$POVSTALB, ordered=F)
save(nmes, file='TriMatch/data/nmes.rda')
tools::checkRdaFiles('TriMatch/data')
tools::resaveRdaFiles('TriMatch/data')
