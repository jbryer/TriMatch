#' Triangle plot.
#' 
#' Triangle plot showing the fitted values (propensity scores) for three different
#' models.
#' 
#' @param tpsa the results from \code{\link{trips}}.
#' @param point.alpha alpha level for points.
#' @param point.size point size.
#' @param legend.title title for the legend.
#' @param text.size text size.
#' @param draw.edges draw edges of the triangle.
#' @param draw.segments draw segments connecting points across two models.
#' @param edge.alpha alpha level for edges if drawn.
#' @param edge.color the color for edges if drawn.
#' @param edge.labels the labels to use for each edge of the triangle.
#' @param ... currently unused.
#' @return ggplot2 figure
#' @seealso triangle.psa
#' @export
plot.triangle.psa <- function(tpsa, 	
							  point.alpha = .3,
							  point.size = 5,
							  legend.title = 'Treatment',
							  text.size = 4,
							  draw.edges = FALSE,
							  draw.segments = TRUE,
							  edge.alpha = .2,
							  edge.color = 'grey',
							  edge.labels = c('Model 1', 'Model 2', 'Model 3'),
							  ...) {
	expand <- .2
	axis.offset <- .1
	
	groups <- attr(tpsa, 'groups')
	nstrata <- attr(tpsa, 'nstrata')
	m <- sqrt(.75)
	
	pts1 <- as.data.frame(matrix(unlist(lapply(tpsa$ps1, segment1)), ncol=2, byrow=TRUE))
	pts2 <- data.frame(x=tpsa$ps2, y=ifelse(is.na(tpsa$ps2), NA, 0))
	pts3 <- as.data.frame(matrix(unlist(lapply(tpsa$ps3, segment2)), ncol=2, byrow=TRUE))
	names(pts1) <- names(pts2) <- names(pts3) <- c('x','y')
	pts1$treat <- pts2$treat <- pts3$treat <- tpsa$treat
	pts1$name <- pts2$name <- pts3$name <- 1:nrow(pts1)
	pts <- rbind(pts1, pts2, pts3)
	
	cols <- c('blue', 'red', 'green')
	names(cols) <- groups
	
	#Tick marks for strata
	q1 <- as.data.frame(attr(tpsa, 'breaks1'))
	q2 <- as.data.frame(attr(tpsa, 'breaks2'))
	q3 <- as.data.frame(attr(tpsa, 'breaks3'))
	names(q1) <- names(q2) <- names(q3) <- 'ps'
	
	tmp <- as.data.frame(matrix(unlist(lapply(q1$ps, segment1)), ncol=2, byrow=TRUE))
	names(tmp) <- c('x','y')
	q1 <- cbind(q1, tmp)
	q1 <- cbind(q1, t(as.data.frame(apply(q1, 1, FUN=function(x) { 
		pt <- perpPt(x=x['x'], y=x['y'])
		pt[1] <- x['x'] - pt[1]
		pt[2] <- x['y'] + pt[2]
		return(pt)
	}))))
	names(q1)[4:5] <- c('xend', 'yend')
	
	q1.labels <- data.frame(
		strata=1:nstrata,
		x=unlist(lapply(2:nrow(q1), function(x) mean(c(q1[x-1,'xend'], q1[x,'xend'])))),
		y=unlist(lapply(2:nrow(q1), function(x) mean(c(q1[x-1,'yend'], q1[x,'yend'])))) )
	
	q2$x <- q2$ps
	q2$y <- rep(0, nrow(q2))
	q2$xend <- q2$x
	q2$yend <- rep(-.05, nrow(q2))
	
	q2.labels <- data.frame(
		strata=1:nstrata,
		x=unlist(lapply(2:nrow(q2), function(x) mean(c(q2[x-1,'xend'], q2[x,'xend'])))),
		y=unlist(lapply(2:nrow(q2), function(x) mean(c(q2[x-1,'yend'], q2[x,'yend'])))) )
	
	tmp <- as.data.frame(matrix(unlist(lapply(q3$ps, segment2)), ncol=2, byrow=TRUE))
	names(tmp) <- c('x','y')
	q3 <- cbind(q3, tmp)
	q3 <- cbind(q3, t(as.data.frame(apply(q3, 1, FUN=function(x) { 
		pt <- perpPt(x=x['x'], y=x['y'])
		pt[1] <- x['x'] + pt[1]
		pt[2] <- x['y'] + pt[2]
		return(pt)
	}))))
	names(q3)[4:5] <- c('xend', 'yend')
	
	q3.labels <- data.frame(
		strata=1:nstrata,
		x=unlist(lapply(2:nrow(q3), function(x) mean(c(q3[x-1,'xend'], q3[x,'xend'])))),
		y=unlist(lapply(2:nrow(q3), function(x) mean(c(q3[x-1,'yend'], q3[x,'yend'])))) )
	
	labels <- data.frame(label=groups, x=c(0, .5, 1), y=c(0, m, 0), angle=c(-45, 0, 45),
						 vjust=c(3,-2,3), hjust=c(.7, .5, .3))
	
	p <- ggplot(data=tpsa)
	
	if(draw.edges) {
		p <- p +
			geom_segment(x=0, y=0, xend=.5, yend=m, alpha=edge.alpha, colour=edge.color) +
			geom_segment(x=.5, y=m, xend=1, yend=0, alpha=edge.alpha, colour=edge.color) +
			geom_segment(x=0, y=0, xend=1, yend=0, alpha=edge.alpha, colour=edge.color)
	}
	
	#TODO: draw grid
	if(draw.segments) {
		p <- p + geom_line(data=pts, aes(x=x, y=y, colour=treat, group=name), alpha=edge.alpha)
	}
	
	p <- p +
		geom_text(data=labels, aes(x=x, y=y, angle=angle, label=label, vjust=vjust, hjust=hjust)) +
		geom_point(data=pts, aes(x=x, y=y, colour=treat), alpha=point.alpha, point.size=point.size) +
		scale_color_manual(legend.title, values=cols)
	
	p <- p +
		xlim(c(-expand,1 + expand)) + ylim(c(-expand, (m + expand))) + 
		xlab(NULL) + ylab(NULL) +
		coord_equal() +
		theme(axis.text=element_blank(), axis.ticks=element_blank(), panel.grid=element_blank())
	
	#Plot strata
	p <- p + 
		geom_segment(data=q1, aes(x=x, y=y, xend=xend, yend=yend)) +
		geom_segment(data=q2, aes(x=x, y=0, xend=xend, yend=-.05)) +
		geom_segment(data=q3, aes(x=x, y=y, xend=xend, yend=yend)) +
		geom_text(data=q1.labels, aes(x=x, y=y, label=strata), angle=60, size=text.size) +
		geom_text(data=q2.labels, aes(x=x, y=y, label=strata), angle=0, size=text.size) +
		geom_text(data=q3.labels, aes(x=x, y=y, label=strata), angle=-60, size=text.size)
	
	#Label each side of the triangle
	a1 <- perpPt(segment1(.5)[1], segment1(.5)[2], d=axis.offset)
	a1[1] <- segment1(.5)[1] - a1[1]
	a1[2] <- segment1(.5)[2] + a1[2]
	a2 <- c(x=.5, y=-axis.offset)
	a3 <- perpPt(segment2(.5)[1], segment2(.5)[2], d=axis.offset)
	a3[1] <- segment2(.5)[1] + a3[1]
	a3[2] <- segment2(.5)[2] + a3[2]
	
	p <- p + 
		geom_text(label=edge.labels[1], x=a1[1], y=a1[2], angle=60, size=text.size) +
		geom_text(label=edge.labels[2], x=a2[1], y=a2[2], angle=0, size=text.size) +
		geom_text(label=edge.labels[3], x=a3[1], y=a3[2], angle=-60, size=text.size)
	
	p <- p + theme(legend.position=c(1,.9), legend.justification=c(1,1))
	
	class(p) <- c('triangle.plot', class(p))
	
	return(p)
}

#' Print method for \code{\link{plot.triangle.psa}}. The primary purpose is to 
#' suppress the "Removed n rows containing missing values" warning printed
#' by \code{ggplot2}.
#' 
#' @param p a plot from \code{\link{plot.triangle.psa}}.
#' @param ... other parameters passed to ggplot2.
#' @export
print.triangle.plot <- function(p, ...) {
	suppressWarnings(NextMethod(p, ...))
}
