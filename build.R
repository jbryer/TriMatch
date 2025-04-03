usethis::use_tidy_description()
devtools::document()
devtools::install(upgrade = 'never')
devtools::install()
devtools::install(build_vignettes = TRUE)
devtools::build(args = "--compact-vignettes=both")
devtools::build_vignettes()
devtools::check(cran = TRUE,
				build_args = c('--compact-vignettes="both"'))

# Build website
pkgdown::build_site()

tools::resaveRdaFiles('data/')

# need ghostscript installed 
# brew install ghostscript

# Ready for CRAN?
devtools::release(args = c('--compact-vignettes="both"'))



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
