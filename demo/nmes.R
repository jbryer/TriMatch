require(TriMatch)
data(nmes)

#http://imai.princeton.edu/research/pscore.html

head(nmes[,c('smoke','AGESMOKE','SMOKENOW','AGESTOP','packyears','yearsince')], n=10)

nmes$smoke <- factor(nmes$smoke, levels=c(0,1,2), labels=c('Never','Smoker','Former'))
table(nmes$smoke, useNA='ifany')

#Unadjusted results
describeBy(nmes$TOTALEXP, group=list(nmes$smoke), mat=TRUE, skew=FALSE)
ggplot(nmes, aes(x=factor(smoke), y=log(TOTALEXP), group=factor(smoke))) + 
	coord_flip() + geom_jitter(alpha=.1, color='grey80') + geom_boxplot(alpha=0)

#We'll create a log(TOTALEXP) varaible
nmes$LogTotalExp <- log(nmes$TOTALEXP)
nmes[nmes$LogTotalExp < 0,]$LogTotalExp <- 0

#From Imai and van Dyk (2004, pp. 857-858):
# "Our analysis includes the following subject-level covariates: age at the times 
# of the survey (19–94), age when the individual started smoking, gender 
# (male, female), race (white, black, other), marriage status (married, widowed, 
# divorced, separated, never married), education level (college graduate, some 
# college, high school graduate, other), census region (Northeast, Mid- west, 
# South, West), poverty status (poor, near poor, low income, middle income, 
# high income), and seat belt usage (rarely, some- times, always/almost always)."
formu <- ~ LASTAGE + MALE + RACE3 + beltuse + educate + marital + SREGION + POVSTALB

nmes <- na.omit(nmes[,c(all.vars(formu), 'smoke', 'TOTALEXP')])

treat <- nmes$smoke
table(treat, useNA='ifany')

tpsa <- trips(nmes, treat, formu)
head(tpsa)
#We'll plot 5% random triplets to get a sence of matches
(p <- plot(tpsa, sample=c(.05), edge.alpha=.1))

if(file.exists('tmatch.nmes.rda')) {
	load('tmatch.nmes.rda')
} else {
	tmatch <- trimatch(tpsa, M1=2, M2=1, exact=nmes[,c('MALE','RACE3')], nmatch=c(10,20))
	save(tmatch, file='tmatch.nmes.rda')
}

table(duplicated(tmatch[,c('Smoker')]))
table(duplicated(tmatch[,c('Smoker','Former')]))

#Effect size balance plot
multibalance.plot(tpsa, grid=TRUE)

loess3.plot(tmatch, nmes$TOTALEXP, plot.points=NULL, ylab='Total Expenditures')

boxdiff.plot(tmatch, nmes$TOTALEXP) #Not very useful with this many points

boxdiff.plot(tmatch, nmes$LogTotalExp)

tmatch.out <- merge(x=tmatch, y=nmes[,c('LogTotalExp')])
tmatch.out <- merge(x=tmatch, y=nmes[,c('TOTALEXP')])
outcomes <- grep(".out$", names(tmatch.out), perl=TRUE)
tmatch.out$id <- 1:nrow(tmatch.out)
out <- melt(tmatch.out[,c(outcomes, ncol(tmatch.out))],id.vars='id')
names(out) <- c('ID','Treatment','Outcome')
set.seed(2112)
friedman.test(Outcome ~ Treatment | ID, out)
(rmanova <- ezANOVA(data=out, dv=Outcome, wid=ID, within=Treatment))

#Possible approach for post-hoc test
pairwise.wilcox.test(x=out$Outcome, g=out$Treatment, paired=TRUE, p.adjust.method='bonferroni')

mean(tmatch.out$Never.out); mean(tmatch.out$Former.out); mean(tmatch.out$Smoker.out);
t.test(tmatch.out$Smoker.out, tmatch.out$Never.out, paired=TRUE)

