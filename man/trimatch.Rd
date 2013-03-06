\name{trimatch}
\alias{trimatch}
\title{Creates matched triplets.}
\usage{
  trimatch(tpsa, caliper = 0.25, nmatch = c(25),
    match.order, M1 = 2, M2 = 1, exact, status = TRUE, ...)
}
\arguments{
  \item{tpsa}{the results from \code{\link{trips}}}

  \item{caliper}{a vector of length one or three indicating
  the caliper to use for matching within each step. This is
  expressed in standardized units such that .25 means that
  matches must be within .25 of one standard deviation to
  be kept, otherwise the match is dropped.}

  \item{nmatch}{number of closest matches to retain before
  moving to next edge. This can be \code{Inf} in which case
  all matches within the caliper will be retained through
  to the next step. For large datasets, evaluating all
  possible matches within the caliper could be time
  consuming.}

  \item{match.order}{character vector of length three
  indicating the order in which the matching algorithm will
  processes. The default is to use start with the group the
  middle number of subjects, followed by the smallest, and
  then the largest.}

  \item{M1}{a scaler indicating the number of unique
  subjects in group one to retain. This applies only to the
  first group in the matching order.}

  \item{M2}{a scaler indicating the number of unique
  matches to retain. This applies to the first two groups
  in the matching order.}

  \item{exact}{a vector or data frame of representing
  covariates for exact matching.  That is, matched triplets
  will first be matched exactly on these covariates before
  evalutating distances.}

  \item{...}{currently unused.}

  \item{status}{whether to print a status bar while
  executing.}
}
\description{
  Create matched triplets by minimizing the total distance
  between matched triplets within a specified caliper.
}
\details{
  The \code{\link{trips}} function will estimate the
  propensity scores for three models. This method will then
  find the best matched triplets based upon minimizing the
  summed differences between propensity scores across the
  three models. That is, the algorithm works as follows:

  \itemize{ \item The first subject from model 1 is
  selected. \item The \code{nmatch} smallest distances are
  selected using propensity scores from model 1. \item For
  each of the matches identified, the subjects propensity
  score from model 2 is retrieved. \item The \code{nmatch}
  smallest distances are selected using propensity score
  from model 3. \item For each of those matches identified,
  the subjects propensity score from model 2 is retrieved.
  \item The distances is calculated from the first and last
  subjects propensity scores from model 2. \item The three
  distances are summed. \item The triplet with the smallest
  overall distance is selected and returned. }
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
	      tmatch <- trimatch(tpsa, M1=3, M2=1, status=FALSE)
	      head(tmatch)
	      plot(tmatch, rows=c(3), line.alpha=1, draw.segments=TRUE)
}

