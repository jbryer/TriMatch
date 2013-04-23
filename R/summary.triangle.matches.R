#' Provides a summary of the matched triplets including analysis of outcome
#' measure if provided.
#' 
#' If an outcome measure is provided this function will perform a Freidman
#' Rank Sum Test and repeated measures ANOVA. If either test has a statistically
#' significant difference (as determined by the value of the \code{p} parameter),
#' a Pairwise Wilcoxon Rank Sum Test will also be provided.
#' 
#' @param object result of \code{\link{trimatch}}.
#' @param outcome vector representing the outcome measure.
#' @param p threshold of the p value to perform a 
#' @param ... parameters passed to other statistical tests.
#' @seealso \code{\link{friedman.test}}, \code{\link{ezANOVA}}, 
#'        \code{\link{pairwise.wilcox.test}}
#' @return a trimatch.summary object.
#' @S3method summary triangle.matches
#' @method summary triangle.matches
#' @export
summary.triangle.matches <- function(object, outcome, p=.05, ...) {
	results <- list()
	
	tpsa <- attr(object, 'triangle.psa')
	
	tab <- table(tpsa$treat)
	tab2 <- c(length(unique(object[,names(tab)[1]])),
		length(unique(object[,names(tab)[2]])),
		length(unique(object[,names(tab)[3]])) )
	
	results$PercentMatched <- as.numeric(tab2 / tab)
	names(results$PercentMatched) <- names(tab)
		
	if(missing(outcome)) {
		stop('Outcome measure not specified.')
	} else {
		tmatch.out <- merge(object, outcome)
		outcomes <- grep(".out$", names(tmatch.out), perl=TRUE)
		tmatch.out$id <- 1:nrow(tmatch.out)
		out <- melt(tmatch.out[,c(outcomes, which(names(tmatch.out) == 'id'))],id.vars='id')
		names(out) <- c('ID','Treatment','Outcome')
		out$ID <- as.factor(out$ID)
		results$friedman.test <- friedman.test(Outcome ~ Treatment | ID, out, ...)
		
		#Repeated measures ANOVA
		results$rmanova <- ezANOVA(data=out, dv=Outcome, wid=ID, 
								   within=Treatment, ...)
		
		if(results$rmanova$ANOVA$p <= p || results$friedman.test$p.value <= p) {
			#Possible approach for post-hoc test
			results$pairwise.wilcox.test <- pairwise.wilcox.test(
						x=out$Outcome, g=out$Treatment, paired=TRUE, 
						p.adjust.method='bonferroni', ...)
		}		
	}
	
	class(results) <- c("trimatch.summary", "list")
	return(results)
}

#' Prints the results of \code{\link{summary.triangle.matches}}.
#' 
#' This is an S3 generic function to print the results of 
#' \code{\link{summary.triangle.matches}}.
#' 
#' @param x results of \code{\link{summary.triangle.matches}}.
#' @param ... currenlty unused.
#' @S3method print trimatch.summary
#' @method print trimatch.summary
#' @export
print.trimatch.summary <- function(x, ...) {
	NextMethod(x, ...) #TODO: Provide a better print method.
}
