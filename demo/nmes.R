require(TriMatch)
data(nmes)

head(nmes[,c('smoke','AGESMOKE','SMOKENOW','AGESTOP','packyears','yearsince')], n=10)

nmes$smoke <- factor(nmes$smoke, levels=c(0,1,2), labels=c('Never','Smoker','Former'))
table(nmes$smoke, useNA='ifany')

describeBy(nmes$TOTALEXP, group=list(nmes$smoke), mat=TRUE)

ggplot(nmes, aes(x=factor(smoke), y=log(TOTALEXP), group=factor(smoke))) + 
	coord_flip() + geom_jitter(alpha=.1, color='grey80') + geom_boxplot(alpha=0)

cols.model <- c('LASTAGE','MALE','RACE3','beltuse','educate',
				'marital','SREGION','POVSTALB')
head(nmes[,c(cols.model, 'smoke', 'TOTALEXP')])
nmes <- na.omit(nmes[,c(cols.model, 'smoke', 'TOTALEXP')])

treat <- nmes$smoke
table(treat, useNA='ifany')

tpsa <- triangle.psa(nmes[,cols.model], treat, ids=1:nrow(nmes))
head(tpsa)
(p <- plot(tpsa[sample(1:nrow(tpsa), 500),], edge.alpha=.1))

if(file.exists('tmatch.nmes.rda')) {
	load('tmatch.nmes.rda')
} else {
	tmatch <- triangle.match(tpsa, M=1, nmatch=c(10,10))
	save(tmatch, file='tmatch.nmes.rda')
}

head(tmatch); nrow(tmatch)
nmes$LogTotalExp <- log(nmes$TOTALEXP)
tmatch.out <- merge(x=tmatch, y=nmes[,c('LogTotalExp')])

plot.parallel(tmatch.out) #Not very useful with this many points

outcomes <- grep(".out$", names(tmatch.out), perl=TRUE)
tmatch.out$id <- 1:nrow(tmatch.out)
out <- melt(tmatch.out[,c(outcomes, ncol(tmatch.out))],id.vars='id')
names(out) <- c('ID','Treatment','Outcome')
out[out$Outcome < 0,]$Outcome <- 0
head(out)
set.seed(2112)
friedman.test(Outcome ~ Treatment | ID, out)


