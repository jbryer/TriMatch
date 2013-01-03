require(TriMatch)
require(ez)

data(students)

describeBy(students$CreditsAttempted, group=list(students$TreatBy), mat=TRUE, skew=FALSE)

ggplot(students, aes(x=TreatBy, y=CreditsAttempted, colour=TreatBy)) + geom_boxplot() + 
	coord_flip() + geom_jitter()

cols.model <- c('Military','Income', 'Employment', 'NativeEnglish','EdLevelMother',
				'EdLevelFather','HasAssocAtEnrollment','Ethnicity','Gender','Age')

treat <- students$TreatBy
table(treat, useNA='ifany')

tpsa <- triangle.psa(students[,cols.model], treat, ids=1:nrow(students))
head(tpsa)
(p <- plot(tpsa))

#Correlation of fitted values between different models
cor(tpsa$ps1, tpsa$ps2, use='pairwise.complete.obs')
cor(tpsa$ps1, tpsa$ps3, use='pairwise.complete.obs')
cor(tpsa$ps3, tpsa$ps2, use='pairwise.complete.obs')

#Match triplets
tmatch <- triangle.match(tpsa)
#tmatch <- triangle.match(tpsa, match.order=c("Treatment2", "Treatment1", "Control"))
head(tmatch); tail(tmatch); nrow(tmatch)
names(attributes(tmatch))

#Look at the subjects that could not be matched
unmatched <- attr(tmatch, 'unmatched')
nrow(unmatched) / nrow(tpsa) * 100
#Percentage of each group not matched
table(unmatched$treat) / table(tpsa$treat) * 100 
unmatched[unmatched$treat != 'Control',]

plot(tmatch, rows=c(2), line.alpha=1, draw.segments=TRUE)

plot.distances(tmatch)
plot.distances(tmatch, caliper=c(.15, .2, .25))

tmatch[tmatch$Dtotal > .11,]

tmatch[128,]
plot(tmatch, rows=c(128), line.alpha=1, draw.segments=TRUE)

#Add in the outcome variable
#Can add one variable...
tmatch.out <- merge(x=tmatch, y=students[,c('CreditsAttempted')])
#Or more (here we also look at log(credits))
#students$LogCredits <- log(students$ECHOURS_ATTEMPTED_COURSES_CALC)
#tmatch.out <- merge(x=tmatch, y=students[,c('LogCredits', 'ECHOURS_ATTEMPTED_COURSES_CALC')])

names(tmatch.out)

plot.parallel(tmatch.out)

outcomes <- grep(".out$", names(tmatch.out), perl=TRUE)
#tmatch.out <- tmatch.out[tmatch.out$Dtotal < .2 * total.sd,]
tmatch.out$id <- 1:nrow(tmatch.out)
out <- melt(tmatch.out[,c(outcomes, ncol(tmatch.out))],id.vars='id')
names(out) <- c('ID','Treatment','Outcome')
head(out)
set.seed(2112)
friedman.test.with.post.hoc(Outcome ~ Treatment | ID, out)
friedman.test(Outcom ~ Treatment | ID, out)

#Repeated measures ANOVA
rmanova <- ezANOVA(data=out, dv=Outcome, wid=ID, within=Treatment)
ls(rmanova)
print(rmanova)

#Boxplot of differences
tmatch$Control_Treat1 <- tmatch$Control - tmatch$Treat1
tmatch$Control_Treat2 <- tmatch$Control - tmatch$Treat2
tmatch$Treat2_Treat1 <- tmatch$Treat2 - tmatch$Treat1
out.box <- melt(tmatch[,c('id','Control_Treat1','Control_Treat2','Treat2_Treat1')], id.vars='id')
names(out.box) <- c('Student','Treatment','Difference')
ggplot(out.box, aes(x=Treatment, y=Difference)) + geom_boxplot() + geom_hline(yintercept=0)


#Possible approach for post-hoc test
?pairwise.wilcox.test
pairwise.wilcox.test()

#Loess plots
tpsa$credits <- students$ECHOURS_ATTEMPTED_COURSES_CALC
pscores <- melt(tpsa[,c('treat','ps1','ps2','ps3','credits')], id.vars=c('treat','credits'))
pscores$variable <- factor(pscores$variable, levels=c('ps1','ps2','ps3'), 
						   labels=c('Model 1', 'Model 2', 'Model 3'))
ggplot(pscores, aes(x=value, y=credits, color=treat)) + geom_point(alpha=.3) + geom_smooth() +
	facet_wrap(~ variable, ncol=1)

