require(TriMatch)
data(tutoring)

names(tutoring)
table(tutoring$treat, useNA='ifany')

formu <- ~ Gender + Ethnicity + Military + ESL + EdMother + EdFather + Age +
	       Employment + Income + Transfer + GPA

tutoring.tpsa <- trips(tutoring, tutoring$treat, formu)

names(attributes(tutoring.tpsa))

summary(attr(tutoring.tpsa, 'model1'))

attr(tutoring.tpsa, 'groups')

plot(tutoring.tpsa, sample=c(200))

tutoring.matched <- trimatch(tutoring.tpsa)

plot(tutoring.matched, rows=which(tutoring.matched$Treatment2 == '10'), 
	 line.alpha=1, draw.segments=TRUE)

unmatched <- unmatched(tutoring.matched)
summary(unmatched)

#Caliper matching
tutoring.matched.all <- trimatch(tutoring.tpsa, method=NULL)
nrow(tutoring.matched.all)
unmatched.all <- unmatched(tutoring.matched.all)
table(unmatched.all$treat) / table(tutoring.tpsa$treat) * 100
boxdiff.plot(tutoring.matched.all, tutoring$Grade)
parallel.plot(tutoring.matched.all, tutoring$Grade)

# 2-2-1 matching
tutoring.matched.n <- trimatch(tutoring.tpsa, method=OneToN, M1=5, M2=3)
nrow(tutoring.matched.n)
unmatched.n <- unmatched(tutoring.matched.n)
table(unmatched.n$treat) / table(tutoring.tpsa$treat) * 100
plot(tutoring.matched.n, rows=which(tutoring.matched.n$Treatment2 == '10'), 
	 line.alpha=1, draw.segments=TRUE)
boxdiff.plot(tutoring.matched.n, tutoring$Grade)
parallel.plot(tutoring.matched.n, tutoring$Grade)


#Check balance
multibalance.plot(tutoring.tpsa)

boxdiff.plot(tutoring.matched, tutoring$Grade)

plot(tutoring.matched, rows=c(3), line.alpha=1, draw.segments=TRUE)

distances.plot(tutoring.matched) 

balance.plot(tutoring.matched, tutoring$Age, label='Age')

multibalance.plot(tutoring.tpsa, grid=TRUE)

boxdiff.plot(tutoring.matched, tutoring$Grade)

