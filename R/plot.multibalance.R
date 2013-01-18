#' Multiple covariate blance assessment plot.
#' 
#' A graphic based upon \code{\link{cv.bal.psa}} function in the \code{PSAgraphics}
#' package. This graphic plots the effect sizes for multiple covariated before and
#' after propensity score andjustement.
#' 
#' @export
plot.multibalance <- function(tpsa, covs, grid=FALSE) {	
	tpsa2 <- cbind(tpsa, (covs))
	
	results <- data.frame(covariate=character(), model=integer(), unadjusted=numeric(),
						  adjusted=numeric(), stringsAsFactors=FALSE)
	for(i in 1:3) {
		m <- tpsa2[!is.na(tpsa2[,paste('model', i, sep='')]),]
		
		bal <- covariateBalance(m[,names(m) %in% names(covs)], 
						  m[,paste('model', i, sep='')], 
						  m[,paste('ps', i, sep='')],
						  m[,paste('strata', i, sep='')])
		results <- rbind(results, data.frame(
			covariate = row.names(bal$effect.sizes),
			model = rep(i, ncol(covs)),
			unadjusted = bal$effect.sizes[,'stES_unadj'],
			adjusted = bal$effect.sizes[,'stES_adj'],
			stringsAsFactors = FALSE
		))
	}
	
	row.names(results) <- 1:nrow(results)
	results <- melt(results, id.vars=c('covariate','model'))
	results$group <- paste(results$variable, results$model, sep='-')
	
	results <- results[rev(order(results$model, results$covariate)),]
	
	results$covariate <- factor(results$covariate, ordered=TRUE)
	
	p <- ggplot(results, 
		aes(x=value, y=covariate, color=variable, shape=factor(model), linetype=factor(model))) + 
		geom_point() + geom_path(alpha=.5, aes(group=group)) +
		ylab('Covariate') + xlab('Effect Size') +
		scale_color_hue('Adjustment') + scale_linetype('Model') + scale_shape('Model')
	if(grid) {
		p <- p + facet_grid(~ model)
	}
	
	return(p)
}
