\name{trips}
\alias{trips}
\title{Estimates propensity scores for three groups}
\usage{
  trips(thedata, treat, formu = ~., groups = unique(treat),
    nstrata = 5, ...)
}
\arguments{
  \item{thedata}{the data frame.}

  \item{treat}{vector or factor indicating the
  treatment/control assignment for \code{thedata}. Length
  must be equal to \code{nrow(thedata)}.}

  \item{formu}{the logistic regression formula. Note that
  the dependent variable should not be specified and will
  be modified.}

  \item{groups}{a vector of exactly length three
  corresponding the values in \code{treat} for each
  control/treatment.}

  \item{nstrata}{the number of strata marks to plot on the
  edge.}

  \item{...}{other parameters passed to \code{\link{glm}}.}
}
\description{
  The propensity score is \eqn{e(X)=P({ W }=1|X)} This
  function will estimate the propensity scores for each
  pair of groups (e.g. two treatments and one control).
}
\details{
  \deqn{{ PS }_{ 1 }=e({ X }_{ { T }_{ 1 }C })=Pr(z=1|{ X
  }_{ { T }_{ 1 }C })} \deqn{{ PS }_{ 2 }=e({ X }_{ { T }_{
  2 }C })=Pr(z=1|{ X }_{ { T }_{ 2 }C })} \deqn{{ PS }_{ 3
  }=e({ X }_{ { T }_{ 2 }{ T }_{ 1 } })=Pr(z=1|{ X }_{ { T
  }_{ 2 }{ T }_{ 1 } })}
}
\examples{
data(students)
	      students$Income <- as.integer(students$Income)
	      students$Employment <- as.integer(students$Employment)
	      students$EdLevelMother <- as.integer(students$EdLevelMother)
	      students$EdLevelFather <- as.integer(students$EdLevelFather)
	      form <- ~ Military + Income + Employment + NativeEnglish + EdLevelMother +
	      	EdLevelFather + HasAssocAtEnrollment + Ethnicity + Gender + Age
	      tpsa <- trips(students, students$TreatBy, form)
	      head(tpsa)
}

