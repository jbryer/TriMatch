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

unmatched <- unmatched(tutoring.matched)
table(unmatched$treat) / table(tutoring.tpsa$treat) * 100

#Check balance
multibalance.plot(tutoring.tpsa)

boxdiff.plot(tutoring.matched, tutoring$Grade)
