#' Estimates propensity scores for three groups (e.g. two treatments and one control).
#' 
#' @param thedata the data frame.
#' @param treat vector or factor indicating the treatment/control assignment for
#'        \code{thedata}. Length must be equal to \code{nrow(thedata)}.
#' @param ids a vector that uniquely identify each row. This will be
#'        passed through to the results to assist in matching. If NULL, 
#'        \code{row.names(thedata)} will be used.
#' @param groups a vector of exactly length three corresponding the values in
#'        \code{treat} for each control/treatment.
#' @param nstrata the number of strata marks to plot on the edge.
#' @param ... other parameters passed to \code{\link{glm}}.
#' @export
triangle.psa <- function(thedata, treat, ids, groups=unique(treat), nstrata=5, ...) {
	if(length(groups) != 3) stop('Sorry, exactly three groups are required.')
	if(nrow(thedata) != length(treat)) stop('length(treat) does not equal nrow(thedata)')
	if(missing(ids)) {
		ids <- row.names(thedata)
	}
	
	treats <- data.frame(id=ids, treat=treat, 
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
	m1 <- glm(treat ~ ., data=cbind(thedata[rows1,], treat=treats[rows1,]$model1), 
			  family='binomial', ...)
	treats[rows1,]$ps1 <- fitted(m1)
	
	rows2 <- which(!is.na(treats$model2))
	m2 <- glm(treat ~ ., data=cbind(thedata[rows2,], treat=treats[rows2,]$model2), 
			  family='binomial', ...)
	treats[rows2,]$ps2 <- fitted(m2)
	
	rows3 <- which(!is.na(treats$model3))
	m3 <- glm(treat ~ ., data=cbind(thedata[rows3,], treat=treats[rows3,]$model3), 
			  family='binomial', ...)
	treats[rows3,]$ps3 <- fitted(m3)
	
	breaks1 <- quantile(treats$ps1, probs=seq(0,1,1/nstrata), na.rm=TRUE)
	breaks2 <- quantile(treats$ps2, probs=seq(0,1,1/nstrata), na.rm=TRUE)
	breaks3 <- quantile(treats$ps3, probs=seq(0,1,1/nstrata), na.rm=TRUE)
	
	treats$strata1 <- cut(treats$ps1, breaks=breaks1, labels=1:nstrata)
	treats$strata2 <- cut(treats$ps2, breaks=breaks2, labels=1:nstrata)
	treats$strata3 <- cut(treats$ps3, breaks=breaks3, labels=1:nstrata)
	
	class(treats) <- c('triangle.psa', 'data.frame')
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