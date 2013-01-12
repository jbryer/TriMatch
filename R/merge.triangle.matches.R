#' Merges outcomes with the matched set.
#' 
#' @param x the result of \code{\link{trimatch}}
#' @param y another data frame or vetor to merge with.
#' @param ... unused
#' @return \code{x} with the additional column(s) added.
#' @export
#' @S3method merge triangle.matches
merge.triangle.matches <- function(x, y, ...) {
	if(is.vector(y)) {
		x <- cbind(x, y[as.integer(x[,1])])
		x <- cbind(x, y[as.integer(x[,2])])
		x <- cbind(x, y[as.integer(x[,3])])
		names(x)[(ncol(x)-2):ncol(x)] <- paste(names(x)[1:3], '.out', sep='')
	} else {
		x <- cbind(x, y[as.integer(x[,1]),])
		x <- cbind(x, y[as.integer(x[,2]),])
		x <- cbind(x, y[as.integer(x[,3]),])
		names(x)[(ncol(x)-(3*ncol(y))+1):ncol(x)] <- paste(rep(names(x)[1:3], each=ncol(y)), 
						names(y), sep='.')
	}
	return(x)
}
