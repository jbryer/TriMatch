#' Estimates propensity scores for three groups
#' 
#' The propensity score is \deqn{e(X)=P({ W }=1|X)}
#' This function will estimate the propensity scores for each pair of groups
#' (e.g. two treatments and one control).
#' 
#' \deqn{{ PS }_{ 1 }=e({ X }_{ { T }_{ 1 }C })=Pr(z=1|{ X }_{ { T }_{ 1 }C })}
#' 
#' \deqn{{ PS }_{ 2 }=e({ X }_{ { T }_{ 2 }C })=Pr(z=1|{ X }_{ { T }_{ 2 }C })}
#' 
#' \deqn{{ PS }_{ 3 }=e({ X }_{ { T }_{ 2 }{ T }_{ 1 } })=Pr(z=1|{ X }_{ { T }_{ 2 }{ T }_{ 1 } })}
#' 
#' @param thedata the data frame.
#' @param treat vector or factor indicating the treatment/control assignment for
#'        \code{thedata}. Length must be equal to \code{nrow(thedata)}.
#' @param formu the logistic regression formula. Note that the dependent variable
#'        should not be specified and will be modified.
#' @param groups a vector of exactly length three corresponding the values in
#'        \code{treat} for each control/treatment.
#' @param nstrata the number of strata marks to plot on the edge.
#' @param method the method to use to estimate the propensity scores. Current options are logistic
#'        or randomForest.
#' @param ... other parameters passed to [stats::glm()].
#' @export
#' @examples
#' \dontrun{
#' data(tutoring)
#' formu <- ~ Gender + Ethnicity + Military + ESL + EdMother + EdFather + Age +
#'      Employment + Income + Transfer + GPA
#' tpsa <- trips(tutoring, tutoring$treat, formu)
#' head(tpsa)
#' }
trips <- function(thedata, treat, formu = ~ ., groups=unique(treat), nstrata=5, 
				  method = 'logistic', ...) {
	if(length(groups) != 3) stop('Sorry, exactly three groups are required.')
	if(nrow(thedata) != length(treat)) stop('length(treat) does not equal nrow(thedata)')
	
	treats <- data.frame(id=1:length(treat), treat=treat, 
						 model1=rep(as.logical(NA), length(treat)),
						 model2=rep(as.logical(NA), length(treat)),
						 model3=rep(as.logical(NA), length(treat)),
						 ps1=rep(as.numeric(NA), length(treat)),
						 ps2=rep(as.numeric(NA), length(treat)),
						 ps3=rep(as.numeric(NA), length(treat)) )
	treats[which(treats$treat == groups[1]), c('model1','model2')] <- FALSE
	treats[which(treats$treat == groups[2]), c('model1','model3')] <- TRUE
	treats[which(treats$treat == groups[3]), c('model2')] <- TRUE
	treats[which(treats$treat == groups[3]), c('model3')] <- FALSE
	
	rows1 <- which(!is.na(treats$model1))
	rows2 <- which(!is.na(treats$model2))
	rows3 <- which(!is.na(treats$model3))
	if(method == 'logistic') {
		message('Using logistic regression to estimate propensity scores...')
		
		m1 <- glm(update(formu, treat ~ .),
				  data=cbind(thedata[rows1,], treat=treats[rows1,]$model1),
				  family='binomial', ...)
		treats[rows1,]$ps1 <- fitted(m1)
		
		m2 <- glm(update(formu, treat ~ .),
				  data=cbind(thedata[rows2,], treat=treats[rows2,]$model2),
				  family='binomial', ...)
		treats[rows2,]$ps2 <- fitted(m2)

		m3 <- glm(update(formu, treat ~ .),
				  data=cbind(thedata[rows3,], treat=treats[rows3,]$model3),
				  family='binomial', ...)
		treats[rows3,]$ps3 <- fitted(m3)
	} else if(method == 'randomForest') {
		message('Using random forests to estimate propensity scores...')

		m1 <- randomForest(update(formu, treat ~ .), 
						   data=cbind(thedata[rows1,], treat=factor(treats[rows1,]$model1)), ...)
		treats[rows1,]$ps1 <- predict(m1, type='prob')[,2]

		m2 <- randomForest(update(formu, treat ~ .), 
						   data=cbind(thedata[rows2,], treat=factor(treats[rows2,]$model2)), ...)
		treats[rows2,]$ps2 <- predict(m2, type='prob')[,2]
		
		m3 <- randomForest(update(formu, treat ~ .), 
						   data=cbind(thedata[rows3,], treat=factor(treats[rows3,]$model3)), ...)
		treats[rows3,]$ps3 <- predict(m3, type='prob')[,2]
	} else {
		stop(paste0('Unsupported method: ', method))
	}
	
	breaks1 <- quantile(treats$ps1, probs=seq(0,1,1/nstrata), na.rm=TRUE)
	breaks2 <- quantile(treats$ps2, probs=seq(0,1,1/nstrata), na.rm=TRUE)
	breaks3 <- quantile(treats$ps3, probs=seq(0,1,1/nstrata), na.rm=TRUE)
	
	treats$strata1 <- cut(treats$ps1, breaks=breaks1, labels=1:nstrata, include.lowest=TRUE)
	treats$strata2 <- cut(treats$ps2, breaks=breaks2, labels=1:nstrata, include.lowest=TRUE)
	treats$strata3 <- cut(treats$ps3, breaks=breaks3, labels=1:nstrata, include.lowest=TRUE)
	
	class(treats) <- c('triangle.psa', 'data.frame')
	attr(treats, 'data') <- thedata
	attr(treats, 'nstrata') <- nstrata
	attr(treats, 'model1') <- m1
	attr(treats, 'model2') <- m2
	attr(treats, 'model3') <- m3
	attr(treats, 'breaks1') <- breaks1
	attr(treats, 'breaks2') <- breaks2
	attr(treats, 'breaks3') <- breaks3
	attr(treats, 'groups') <- groups
	
	return(treats)
}
