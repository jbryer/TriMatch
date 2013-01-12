require(TriMatch)

data(students)

describeBy(students$CreditsAttempted, group=list(students$TreatBy), mat=TRUE, skew=FALSE)

ggplot(students, aes(x=TreatBy, y=CreditsAttempted, colour=TreatBy)) + geom_boxplot() + 
	coord_flip() + geom_jitter()

cols.model <- c('Military','Income', 'Employment', 'NativeEnglish','EdLevelMother',
				'EdLevelFather','HasAssocAtEnrollment','Ethnicity','Gender','Age')

treat <- students$TreatBy
table(treat, useNA='ifany')

tpsa <- trips(students[,cols.model], treat, ids=1:nrow(students))
head(tpsa)
(p <- plot(tpsa))

#Correlation of fitted values between different models
cor(tpsa$ps1, tpsa$ps2, use='pairwise.complete.obs')
cor(tpsa$ps1, tpsa$ps3, use='pairwise.complete.obs')
cor(tpsa$ps3, tpsa$ps2, use='pairwise.complete.obs')

#Match triplets
tmatch <- trimatch(tpsa)
#tmatch <- trimatch(tpsa, match.order=c("Treatment2", "Treatment1", "Control"))
head(tmatch); tail(tmatch); nrow(tmatch)
names(attributes(tmatch))

#Look at the subjects that could not be matched
unmatched <- attr(tmatch, 'unmatched')
nrow(unmatched) / nrow(tpsa) * 100
#Percentage of each group not matched
table(unmatched$treat) / table(tpsa$treat) * 100 
unmatched[unmatched$treat != 'Control',]

#Triangle plot of only the unmatched students
plot(tpsa[tpsa$id %in% unmatched$id,])

plot(tmatch, rows=c(2), line.alpha=1, draw.segments=TRUE)

plot.distances(tmatch)
plot.distances(tmatch, caliper=c(.15, .2, .25))

tmatch[tmatch$Dtotal > .11,]

tmatch[838,]
plot(tmatch, rows=c(838), line.alpha=1, draw.segments=TRUE)

#Add in the outcome variable
#Can add one variable...
tmatch.out <- merge(x=tmatch, y=students[,c('CreditsAttempted')])
#Or more (here we also look at log(credits))
#students$LogCredits <- log(students$ECHOURS_ATTEMPTED_COURSES_CALC)
#tmatch.out <- merge(x=tmatch, y=students[,c('LogCredits', 'ECHOURS_ATTEMPTED_COURSES_CALC')])

names(tmatch.out)

plot.parallel(tmatch.out)

outcomes <- grep(".out$", names(tmatch.out), perl=TRUE)
tmatch.out$id <- 1:nrow(tmatch.out)
out <- melt(tmatch.out[,c(outcomes, which(names(tmatch.out) == 'id'))],id.vars='id')
names(out) <- c('ID','Treatment','Outcome')
head(out)
set.seed(2112)
friedman.test(Outcome ~ Treatment | ID, out)

#Repeated measures ANOVA
rmanova <- ezANOVA(data=out, dv=Outcome, wid=ID, within=Treatment)
ls(rmanova)
print(rmanova)

#Individual t-tests
t1 <- t.test(x=tmatch.out$Treatment1.out, y=tmatch.out$Control.out, paired=TRUE)
t2 <- t.test(x=tmatch.out$Treatment2.out, y=tmatch.out$Control.out, paired=TRUE)
t3 <- t.test(x=tmatch.out$Treatment2.out, y=tmatch.out$Treatment1.out, paired=TRUE)

ci <- as.data.frame(rbind(t1$conf.int, t2$conf.int, t3$conf.int))
ci$Treatment <- names(tmatch.out)[12:14]
ci$estimate <- c(t1$estimate, t2$estimate, t3$estimate)

#Boxplot of differences
tmatch.out$Treat1_Control <- tmatch.out$Treatment1.out - tmatch.out$Control.out
tmatch.out$Treat2_Control <- tmatch.out$Treatment2.out - tmatch.out$Control.out
tmatch.out$Treat2_Treat1 <- tmatch.out$Treatment2.out - tmatch.out$Treatment1.out
out.box <- melt(tmatch.out[,c('id','Treat1_Control','Treat2_Control','Treat2_Treat1')], id.vars='id')
names(out.box) <- c('Student','Treatment','Difference')
ggplot(out.box, aes(x=Treatment, y=Difference)) + geom_boxplot() + geom_hline(yintercept=0) +
	geom_crossbar(data=ci, aes(x=Treatment, ymin=V1, ymax=V2, y=estimate), 
				  color='green', fill='green', width=.72, alpha=.6) +
	scale_x_discrete(NULL, labels=c(Treat1_Control='Treat1 - Control', 
									Treat2_Control='Treat2 - Control',
									Treat2_Treat1='Treat2 - Treat1')) +
	xlab(NULL)

#Possible approach for post-hoc test
pairwise.wilcox.test(x=out$Outcome, g=out$Treatment, paired=TRUE, p.adjust.method='bonferroni')

#Loess plots

tpsa$credits <- students$CreditsAttempted
#tpsa$credits <- log(tpsa$credits)
#tpsa[tpsa$credits < 0, ]$credits <- 0
pscores <- melt(tpsa[,c('treat','ps1','ps2','ps3','credits')], id.vars=c('treat','credits'))
pscores$variable <- factor(pscores$variable, levels=c('ps1','ps2','ps3'), 
						   labels=c('Model 1', 'Model 2', 'Model 3'))
ggplot(pscores, aes(x=value, y=credits, color=treat)) + 
	geom_point(alpha=.3) + geom_smooth() +
	facet_wrap(~ variable, ncol=1)

ggplot(pscores[pscores$credits > 0,], aes(x=value, y=credits, color=treat)) + 
	geom_point(alpha=.3) + geom_smooth() +
	facet_wrap(~ variable, ncol=1)
