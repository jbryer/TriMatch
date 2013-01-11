#' Barplot for the sum of distances.
#' 
#' @param tmatch the results of \code{\link{triangle.match}}.
#' @param caliper a vector indicating where vertical lines should be drawn as a
#'        factor of the standard deviation. Rosenbaum and Rubin (1985) suggested
#'        one quarter of one standard deviation.
#' @seealso triangle.match
#' @export
plot.distances <- function(tmatch, caliper=.25) {
	tpsa <- attr(tmatch, 'triangle.psa')
	tmatch$id <- row.names(tmatch)
	tmp <- melt(tmatch[,c('id','D.m1','D.m2','D.m3')], id.vars=1)
	names(tmp) <- c('ID','Model','Distance')
	l <- (sd(tpsa$ps1, na.rm=TRUE) + sd(tpsa$ps2, na.rm=TRUE) + sd(tpsa$ps3, na.rm=TRUE))
	cat(paste("Standard deviations of propensity scores: ",
			  paste(prettyNum(
			  	sapply(tpsa[,c('ps1','ps2','ps3')], sd, na.rm=TRUE), digits=2), 
			  	  collapse=' + '),
			  " = ", prettyNum(l, digits=2), '\n', sep='' ))
	p <- ggplot(tmp, aes(x=ID, y=Distance)) + geom_bar(aes(fill=Model)) + coord_flip() + 
		geom_hline(yintercept=l*caliper, color='black') +
		theme(axis.text.y=element_blank()) +
		xlab(NULL) + xlim(tmatch[order(tmatch$Dtotal),'id'])
	if(length(which(tmatch$Dtotal > (l * min(caliper)))) > 0) {
		p <- p + geom_text(data=tmatch[tmatch$Dtotal > (l * min(caliper)),], 
						   aes(x=id, y=0, label=id),
						   size=3, hjust=1)	
	}
	for(i in seq_along(caliper)) {
		cat(paste("Percentage of matches exceding a distance of ", 
				  prettyNum(l * caliper[i], digits=2), 
				  " (caliper = ", caliper[i], "): ", 
				  prettyNum(length(which(tmatch$Dtotal > l * caliper[i])) / 
				  		  	length(tmatch$Dtotal) * 100, digits=2), '%\n',
				  sep=''))
	}
	return(p)
}
