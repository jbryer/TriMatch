#' Returns rows from \code{\link{trips}} that were not matched by \code{\link{trimatch}}.
#' 
#' This function returns a subset of \code{\link{trips}} that were not matched by
#' \code{\link{trimatch}}. All data frame methods work with the returned object
#' but special \code{summary} function will provided relevant information.
#' 
#' @param tmatch the results of \code{\link{trimatch}}.
#' @return a data frame of unmatched rows.
#' @export
unmatched <- function(tmatch) {
	tpsa <- attr(tmatch, 'triangle.psa')
	unmatched <- tpsa[!(tpsa$id %in% c(tmatch[,1], tmatch[,2], tmatch[,3])),]
	class(unmatched) <- c('unmatched', 'triangle.psa', 'data.frame')
	attr(unmatched, 'triangle.psa') <- tpsa
	return(unmatched)
}

#' Provides a summary of unmatched subjects.
#' 
#' Will return as a list and print the percentage of total unmatched rows and
#' percent by treatment.
#' 
#' @param object results of \code{\link{unmatched}}
#' @param digits number of digits to print.
#' @param ... currently unused.
#' @return a list of summary results.
#' @export
#' @S3method summary unmatched
summary.unmatched <- function(object, digits=3, ...) {
	tpsa <- attr(object, 'triangle.psa')
	cat(paste(prettyNum(nrow(object) / nrow(tpsa) * 100, digits=digits), 
				  '% of total data points were not matched.', sep=''))
	cat('\nUnmatched by treatment:')
	print(table(object$treat) / table(tpsa$treat) * 100, digits=digits)
	results <- list()
	results$PercentUnmatched <- nrow(object) / nrow(tpsa)
	results$PercentByTreat <- table(object$treat) / table(tpsa$treat)
	invisible(results)
}
