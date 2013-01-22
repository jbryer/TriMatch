#' Returns a \code{ggplot2} box plot of the differences.
#' 
#' A boxplot of differences between each pair of treatments.
#' 
#' @param tmatch the results from \code{\link{trimatch}}.
#' @param out a vector of the outcome measure of interest.
#' @param plot.mean logical indicating whether the means should be plotted.
#' @return a \code{ggplot2} boxplot of the differences.
#' @export
plot.boxdiff <- function(tmatch, out, plot.mean=TRUE) {
	tmatch.out <- merge(tmatch, out)
	outcomes <- grep(".out$", names(tmatch.out), perl=TRUE)
	tmatch.out$id <- 1:nrow(tmatch.out)
	
	diffcols <- c(
		paste(names(tmatch.out)[outcomes[c(1,2)]], collapse='-'),
		paste(names(tmatch.out)[outcomes[c(1,3)]], collapse='-'),
		paste(names(tmatch.out)[outcomes[c(2,3)]], collapse='-')
	)
	
	tmatch.out[, diffcols[1] ] <- tmatch.out[,outcomes[1]] - tmatch.out[,outcomes[2]]
	tmatch.out[, diffcols[2] ] <- tmatch.out[,outcomes[1]] - tmatch.out[,outcomes[3]]
	tmatch.out[, diffcols[3] ] <- tmatch.out[,outcomes[2]] - tmatch.out[,outcomes[3]]
	
	#Individual t-tests
	t1 <- t.test(x=tmatch.out[,outcomes[1]], y=tmatch.out[,outcomes[2]], paired=TRUE)
	t2 <- t.test(x=tmatch.out[,outcomes[1]], y=tmatch.out[,outcomes[3]], paired=TRUE)
	t3 <- t.test(x=tmatch.out[,outcomes[2]], y=tmatch.out[,outcomes[3]], paired=TRUE)
	
	ci <- as.data.frame(rbind(t1$conf.int, t2$conf.int, t3$conf.int))
	ci$Treatments <- diffcols
	ci$estimate <- c(t1$estimate, t2$estimate, t3$estimate)

	out.box <- melt(tmatch.out[,c('id',diffcols)], id.vars='id')
	names(out.box) <- c('ID','Treatments','Difference')
	
	#Create new labels for each box
	labels <- gsub('-', ' - ', gsub('.out', '', diffcols))
	names(labels) <- diffcols
	
	p <- ggplot(out.box, aes(x=Treatments, y=Difference)) + 
		geom_boxplot() + 
		geom_crossbar(data=ci, aes(x=Treatments, ymin=V1, ymax=V2, y=estimate), 
					  color='green', fill='green', width=.72, alpha=.6) +
		geom_hline(color='blue', yintercept=0) +
		geom_point(data=ci, aes(x=Treatments, y=estimate), color='red', size=3)
	if(plot.mean) {
		p <- p + geom_text(data=ci, aes(x=Treatments, y=estimate, 
							label=prettyNum(estimate, digits=2)), vjust=-1)
	}
	p <- p + scale_x_discrete(NULL, labels=labels) +
		xlab(NULL)
	
	return(p)
}
