require(devtools)
setwd('~/Dropbox/Projects')

document('TriMatch')
check_doc('TriMatch')
install('TriMatch')
build('TriMatch')
check('TriMatch')

require(TriMatch)
data(nmes)
data(students)

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
