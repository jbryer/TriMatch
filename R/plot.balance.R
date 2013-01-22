#' Balance plot for the given covariate.
#'
#' If the covariate is numeric, boxplots will be drawn with red points for the mean
#' and green error bars for the standard error. For non-numeric covariates a barplot
#' will be drawn.
#' 
#' A Friedman rank sum test will be performed for all covariate types, printed,
#' and stored as an attribute to the returned object named \code{friedman}. If
#' a continuous covariate a repeated measures ANOVA will also be performed, printed,
#' and returned as an attribute named \code{rmanova}.
#' 
#' @param tmatch results from \code{\link{trimatch}}.
#' @param covar vector of the covaraite to check balance of.
#' @param model an integer between 1 and 3 indicating from which model the propensity
#'        scores will be used.
#' @param nstrata number of strata to use.
#' @param ylab label of the y-axis.
#' @param se.ratio a multiplier for how large standard error bars will be.
#' @param label label for the legend.
#' @return a \code{ggplot2} figure.
#' @export
plot.balance <- function(tmatch, covar, model,
					     nstrata=attr(attr(tmatch, 'triangle.psa'), 'nstrata'),
					     label='Covariate',
					     ylab='',
						 se.ratio = 2) {
	if(!is.numeric(covar)) {
		covar <- as.character(covar)
	}
	#TODO: Much of this code is shared with plot.loess3
	tpsa <- attr(tmatch, 'triangle.psa')
	tmatch2 <- merge(x=tmatch, y=covar)
	groups <- names(tmatch2)[1:3]
	
	if(missing(model)) {
		for(i in 1:3) {
			if(length(which(is.na(tpsa[tpsa$treat %in% groups[1:2],paste('model', i, sep='')]))) == 0) {
				model <- i
				break;
			}
		}
		if(model == 0) {
			stop('Could not find model. There are missing propensity scores in all models.')
		}
		message(paste('Using propensity scores from model ', model, ' for evaluating balance.', sep=''))
	} else {
		#Need to determine which groups these models represent
		if(length(which(is.na(tpsa[tpsa$treat %in% groups[1:2],
								paste('model', model, sep='')]))) == 0) {
			groups <- groups[c(1,2,3)]
		} else if(length(which(is.na(tpsa[tpsa$treat %in% groups[2:3],
								paste('model', model, sep='')]))) == 0) {
			groups <- groups[c(2,3,1)]
			
		} else if(length(which(is.na(tpsa[tpsa$treat %in% groups[c(1,3)],
								paste('model', model, sep='')]))) == 0) {
			groups <- groups[c(1,3,2)]			
		} else {
			stop(paste('Could not use model ', model, 
					   '. There is are unexpected missing propensity scores', sep=''))
		}
	}

	tmatch2 <- merge(tmatch2, tpsa[which(tpsa$treat == groups[1]), c('id',paste('ps', model, sep=''))], 
					 by.x=groups[1], by.y='id', all.x=TRUE)
	names(tmatch2)[ncol(tmatch2)] <- paste(groups[1], '.ps', sep='')
	tmatch2 <- merge(tmatch2, tpsa[which(tpsa$treat == groups[2]), c('id',paste('ps', model, sep=''))], 
					 by.x=groups[2], by.y='id', all.x=TRUE)
	names(tmatch2)[ncol(tmatch2)] <- paste(groups[2], '.ps', sep='')
	tmatch2[,'mean.ps'] <- apply(tmatch2[,(ncol(tmatch2)-1):ncol(tmatch2)], 1, mean)

	breaks <- quantile(tmatch2$mean.ps, probs=seq(0,1,1/nstrata), na.rm=TRUE)
	tmatch2$strata <- cut(tmatch2$mean.ps, breaks=breaks, labels=1:nstrata, include.lowest=TRUE)
	
	tmatch2$id <- 1:nrow(tmatch2)
	
	badrows <- which(is.na(tmatch2$strata))
	if(length(badrows) > 0) {
		warning(paste('Could not determine strata for the following rows: ', 
					  paste(badrows, collapse=', '), sep=''))
		tmatch2 <- tmatch2[-badrows,] 
	}
	
	out <- melt(tmatch2[,c(paste(groups, '.out', sep=''), 'id', 'strata')], id.vars=c('id','strata'))
	names(out) <- c('ID','Strata','Treatment','Covariate')	
	out$Treatment <- as.character(out$Treatment)
	out$Treatment <- substr(out$Treatment, 1, (sapply(out$Treatment, nchar)-4)) #Strip .out from value
	
	p <- ggplot(out, aes(x=Treatment)) + facet_grid(~ Strata) 
	if(is.numeric(out$Covariate)) {
		df <- describeBy(out$Covariate, group=list(out$Treatment, out$Strata), 
						 mat=TRUE, skew=FALSE)[,c('group1','group2','mean','sd','se')]
		names(df) <- c('Treatment','Strata','Mean','SD','SE')
		df$ymin <- (df$Mean - se.ratio * df$SE)
		df$ymax <- (df$Mean + se.ratio * df$SE)
		p <- p + geom_boxplot(aes(y=Covariate)) + ylab(label) +
			geom_point(data=df, aes(y=Mean), color='red', size=3) +
			geom_line(data=df, aes(y=Mean, group=Strata)) +
			geom_errorbar(data=df, aes(ymin=ymin, ymax=ymax), color='green', width=.5)
	} else { #Categorical varaible
		require(scales)
		p <- p + geom_bar(aes(fill=factor(Covariate), y=(..count..)/sum(..count..)), 
						  position='fill')  +
			scale_fill_hue(label) + ylab('Percent') +
			scale_y_continuous(labels = percent)
	}
	
	p <- p + theme(axis.text.x=element_text(angle=-45, vjust=.5),
				   panel.background=element_rect(color='black', fill='#F9F3FD'))
	
	ft <- friedman.test(Covariate ~ Treatment | ID, out)
	print(ft)
	attr(p, 'friedman') <- ft
	
	if(is.numeric(out$Covariate)) {
		#We can also use repeated measure ANOVA for continuous covariates
		out$Treatment <- as.factor(out$Treatment)
		out$ID <- as.factor(out$ID)
		rmanova <- ezANOVA(data=out, dv=Covariate, wid=ID, within=Treatment)
		cat(' Repeated measures ANOVA\n\n')
		print(rmanova$ANOVA)	
		attr(p, 'rmanova') <- rmanova
	}
	
	return(p)
}
