require(TriMatch)
data(students)

#Unadjusted results
describeBy(students$CreditsAttempted, group=list(students$TreatBy), mat=TRUE, skew=FALSE)
ggplot(students, aes(x=TreatBy, y=CreditsAttempted, colour=TreatBy)) + geom_boxplot() + 
	coord_flip() + geom_jitter()

#Recode ordered factors to integers
students$Income <- as.integer(students$Income)
students$Employment <- as.integer(students$Employment)
students$EdLevelMother <- as.integer(students$EdLevelMother)
students$EdLevelFather <- as.integer(students$EdLevelFather)

#Logistic regression formula used by trips (and ultimately by glm)
form <- ~ Military + Income + Employment + NativeEnglish + EdLevelMother + 
	      EdLevelFather + Ethnicity + Gender + Age

table(students$TreatBy, useNA='ifany')

#Estimate propensity scores for three models for each pair of treatments/control
tpsa <- trips(students, students$TreatBy, form)
head(tpsa)
(p <- plot(tpsa))

plot(tpsa, sample=c(50)) #Plot 50 of each group

#Look at the summary of the three logistic regression models
summary(attr(tpsa, 'model1'))
summary(attr(tpsa, 'model2'))
summary(attr(tpsa, 'model3'))

#Match triplets
tmatch <- trimatch(tpsa, M1=3, M2=1)
tmatch <- trimatch(tpsa, M1=3, M2=1, exact=students[,c('Military')])
tmatch <- trimatch(tpsa, M1=3, M2=1, exact=students[,c('Military','Gender')])
#tmatch <- trimatch(tpsa, match.order=c("Treatment2", "Treatment1", "Control"))
head(tmatch); tail(tmatch); nrow(tmatch)
names(attributes(tmatch))


#Look at the subjects that could not be matched
unmatched <- unmatched(tmatch)
summary(unmatched)

#Triangle plot of only the unmatched students
plot(unmatched)

plot(tmatch, rows=c(3), line.alpha=1, draw.segments=TRUE)

tmatch[which(tmatch$Treatment2 == '10'),]
plot(tmatch, rows=which(tmatch$Treatment2 == '10'), line.alpha=1, draw.segments=TRUE)

#This will use the caliper used by trimatch
distances.plot(tmatch) 
#We can also explore how many matches we would loose if we used a smaller caliper
distances.plot(tmatch, caliper=c(.1, .15, .2))

#List those with a total distance greater than .3 (equivelent to a caliper of .1
#within each model).
tmatch[tmatch$Dtotal > .30,]

#Plot the matched triplet of the largest distance
rn <- row.names(tmatch[tmatch$Dtotal > .30,][1,])
tmatch[rn,]
plot(tmatch, rows=c(rn), line.alpha=1, draw.segments=TRUE)

#Check balance
balance.plot(tmatch, students$Age, label='Age')
balance.plot(tmatch, students$Age, label='Age', nstrata=8)

balance.plot(tmatch, students$Military, label='Military')
balance.plot(tmatch, students$Military, label='Military', model=1)
balance.plot(tmatch, students$Military, label='Military', model=2)

balance.plot(tmatch, students$Gender, label='Gender')
balance.plot(tmatch, students$Ethnicity, label='Ethnicity')

#Effect size balance plot
multibalance.plot(tpsa, grid=TRUE)

#Analysis of outcome of interest. Credits attempted here.
loess3.plot(tmatch, students$CreditsAttempted, plot.points=geom_jitter, ylab='Credits Attempted')
loess3.plot(tmatch, students$CreditsAttempted, plot.points=geom_jitter, ylab='Credits Attempted', 
			points.alpha=.5, plot.connections=TRUE)

boxdiff.plot(tmatch, students$CreditsAttempted)

#Add in the outcome variable
tmatch.out <- merge(tmatch, students$CreditsAttempted)

outcomes <- grep(".out$", names(tmatch.out), perl=TRUE)
tmatch.out$id <- 1:nrow(tmatch.out)
out <- melt(tmatch.out[,c(outcomes, which(names(tmatch.out) == 'id'))],id.vars='id')
names(out) <- c('ID','Treatment','Outcome')
set.seed(2112)
friedman.test(Outcome ~ Treatment | ID, out)

#Repeated measures ANOVA
(rmanova <- ezANOVA(data=out, dv=Outcome, wid=ID, within=Treatment))

#Possible approach for post-hoc test
pairwise.wilcox.test(x=out$Outcome, g=out$Treatment, paired=TRUE, p.adjust.method='bonferroni')

