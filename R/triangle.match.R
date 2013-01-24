#' Euclidean distance calculation.
#' 
#' This method uses a simple Euclidean distance caluclation for determining the
#' distances between two matches. That is, |ps1 - ps2|.
#' 
#' @param x vector of propensity scores.
#' @param grouping vector or factor identifying group membership.
#' @param id vector corresponding to unique identifer for each element in 
#'        \code{x} and \code{grouping}.
#' @param groups vector of length two indicating the unique groups to calculate
#'        the distance between. The first element will be the rows, the second columns.
#' @param caliper a scaler indicating the caliper to use for matching within
#'        each step.
#' @param nmatch number of smallest distances to retain.
#' @return a list of length equal to \code{x}. Each element of the list is a
#'        named numeric vector where the values correspond to the distance and the
#'        name to the \code{id}.
distance.euclid <- function(x, grouping, id, groups, caliper, nmatch=Inf) {
	sd <- sd(x, na.rm=TRUE)
	tmp <- data.frame(ps=x, group=grouping, id=id, stringsAsFactors=FALSE)
	tmp <- tmp[!is.na(tmp$ps),]
	tmp.1 <- tmp[tmp$group == groups[1],]
	tmp.2 <- tmp[tmp$group == groups[2],]
	d.list <- list()
	for(i in 1:nrow(tmp.1)) {
		dist <- abs(tmp.1[i,]$ps - tmp.2$ps)
		names(dist) <- tmp.2$id
		dist <- dist[order(dist)]
		dist <- dist / sd #Convert to standized units
		dist <- dist[1:min(length(dist), nmatch)]
		dist <- dist[dist < caliper]
		d.list[[i]] <- dist
	}
	names(d.list) <- as.character(tmp.1$id)
	return(d.list)
}

#' Creates matched triplets.
#' 
#' The \code{\link{trips}} function will estimate the propensity scores
#' for three models. This method will then find the best matched triplets based
#' upon minimizing the summed differences between propensity scores across the
#' three models. That is, the algorithm works as follows:
#' 
#' -The first subject from model 1 is selected.
#' -The \code{nmatch} smallest distances are selected using propensity scores from
#'  model 1.
#' -For each of the matches identified, the subjects propensity score from model
#'  2 is retrieved.
#' -The \code{nmatch} smallest distances are selected using propensity score from
#'  model 3.
#' -For each of those matches identified, the subjects propensity score from model
#'  2 is retrieved.
#' -The distances is calculated from the first and last subjects propensity scores
#'  from model 2.
#' -The three distances are summed.
#' -The triplet with the smallest overall distance is selected and returned.
#' 
#' @param tpsa the results from \code{\link{trips}}
#' @param caliper a vector of length one or three indicating the caliper to use for matching within
#'        each step. This is expressed in standardized units such that .25 means
#'        that matches must be within .25 of one standard deviation to be kept,
#'        otherwise the match is dropped.
#' @param nmatch number of closest matches to retain before moving to next edge. This can
#'        be \code{Inf} in which case all matches within the caliper will be retained
#'        through to the next step. For large datasets, evaluating all possible
#'        matches within the caliper could be time consuming. 
#' @param match.order character vector of length three indicating the order in 
#'        which the matching algorithm will processes. The default is to use start
#'        with the group the middle number of subjects, followed by the smallest,
#'        and then the largest. 
#' @param M1 a scaler indicating the number of unique subjects in group one to
#'        retain. This applies only to the first group in the matching order.
#' @param M2 a scaler indicating the number of unique matches to retain. This applies
#'        to the first two groups in the matching order.
#' @param exact a vector or data frame of representing covariates for exact matching.
#'        That is, matched triplets will first be matched exactly on these covariates
#'        before evalutating distances.
#' @param ... currently unused.
#' @export
trimatch <- function(tpsa, caliper=.25, nmatch=c(25), match.order, M1=2, M2=1, exact, ...) {
	if(length(nmatch) == 1) {
		nmatch <- c(nmatch, nmatch)
	}
	if(length(caliper) == 1) {
		caliper <- rep(caliper, 3)
	}
	
	groups <- attr(tpsa, 'groups')
	if(missing(match.order)) {
		group.sizes <- table(tpsa$treat)
		ordering <- order(group.sizes, decreasing=FALSE)
		ordering <- ordering[c(2,1,3)]
		match.order <- names(group.sizes)[ordering]
	} else {
		ordering <- c(
			which(groups == match.order[1]),
			which(groups == match.order[2]),
			which(groups == match.order[3]))
	}
	message(paste("Matching order: ", paste(match.order, collapse=', '), sep=''))
	
	#Position to model:
	# 1 & 2 == Model 1, left
	# 2 & 3 == Model 3, right
	# 1 & 3 == Model 2, bottom
	getPS <- function(x, y) {
		if(x %in% c(1,2) & y %in% c(1,2)) {
			return(tpsa$ps1)
		} else if(x %in% c(2,3) & y %in% c(2,3)) {
			return(tpsa$ps3)
		} else if(x %in% c(1,3) & y %in% c(1,3)) {
			return(tpsa$ps2)
		}
	}
	getModel <- function(x, y) {
		if(x %in% c(1,2) & y %in% c(1,2)) {
			return('m1')
		} else if(x %in% c(2,3) & y %in% c(2,3)) {
			return('m3')
		} else if(x %in% c(1,3) & y %in% c(1,3)) {
			return('m2')
		}
	}	
		
	ps1 <- getPS(ordering[1], ordering[2])
	ps2 <- getPS(ordering[2], ordering[3])
	ps3 <- getPS(ordering[3], ordering[1])
	
	t <- length(which(tpsa$treat == match.order[1])) + 3
	pb <- txtProgressBar(min=0, max=t, style=3)
	
	#Calculate the distances
	d1 <- sapply(ps1[tpsa$treat == match.order[2]],
				 FUN=function(x) { abs(x - ps1[tpsa$treat == match.order[1]]) / sd(ps1, na.rm=TRUE) })		
	dimnames(d1) <- list(
		tpsa[tpsa$treat == match.order[1],'id'],
		tpsa[tpsa$treat == match.order[2],'id']	)
	setTxtProgressBar(pb, 1)
	d2 <- sapply(ps2[tpsa$treat == match.order[3]],
				 FUN=function(x) { abs(x - ps2[tpsa$treat == match.order[2]]) / sd(ps2, na.rm=TRUE) })		
	dimnames(d2) <- list(
		tpsa[tpsa$treat == match.order[2],'id'],
		tpsa[tpsa$treat == match.order[3],'id']	)
	setTxtProgressBar(pb, 2)
	d3 <- sapply(ps3[tpsa$treat == match.order[1]],
				 FUN=function(x) { abs(x - ps3[tpsa$treat == match.order[3]]) / sd(ps3, na.rm=TRUE) })		
	dimnames(d3) <- list(
		tpsa[tpsa$treat == match.order[3],'id'],
		tpsa[tpsa$treat == match.order[1],'id']	)
	setTxtProgressBar(pb, 3)
	
	if(!missing(exact)) {
		if(class(exact) %in% c('matrix','data.frame')) {
			exact.g1 <- exact[tpsa$treat == match.order[1],]
			exact.g2 <- exact[tpsa$treat == match.order[2],]
			exact.g3 <- exact[tpsa$treat == match.order[3],]
			m1 <- t(as.matrix(exact.g1))
			m2 <- as.matrix(exact.g2)
			exact1 <- apply(m2, 1, FUN=function(x) { apply(m1, 2, FUN=function(y) { all(x == y) } ) })
			setTxtProgressBar(pb, 4)
			m2 <- t(as.matrix(exact.g2))
			m3 <- as.matrix(exact.g3)
			exact2 <- apply(m3, 1, FUN=function(x) { apply(m2, 2, FUN=function(y) { all(x == y) } ) })
			setTxtProgressBar(pb, 5)
			m3 <- t(as.matrix(exact.g3))
			m1 <- as.matrix(exact.g1)
			exact3 <- apply(m1, 1, FUN=function(x) { apply(m3, 2, FUN=function(y) { all(x == y) } ) })
			setTxtProgressBar(pb, 6)
		} else {
			exact1 <- sapply(exact[tpsa$treat == match.order[2]],
						     FUN=function(x) { x == exact[tpsa$treat == match.order[1]] })
			setTxtProgressBar(pb, 4)
			exact2 <- sapply(exact[tpsa$treat == match.order[3]],
							 FUN=function(x) { x == exact[tpsa$treat == match.order[2]] })
			setTxtProgressBar(pb, 5)
			exact3 <- sapply(exact[tpsa$treat == match.order[1]],
							 FUN=function(x) { x == exact[tpsa$treat == match.order[3]] })
			setTxtProgressBar(pb, 6)
		}
	} else {
		exact1 <- matrix(TRUE, 
						 nrow=length(which(treat == match.order[1])),
						 ncol=length(which(treat == match.order[2])))
		setTxtProgressBar(pb, 4)
		exact2 <- matrix(TRUE, 
						 nrow=length(which(treat == match.order[2])),
						 ncol=length(which(treat == match.order[3])))
		setTxtProgressBar(pb, 5)
		exact3 <- matrix(TRUE, 
						 nrow=length(which(treat == match.order[3])),
						 ncol=length(which(treat == match.order[1])))
		setTxtProgressBar(pb, 6)
	}
	dimnames(exact1) <- list(
		tpsa[tpsa$treat == match.order[1],'id'],
		tpsa[tpsa$treat == match.order[2],'id']	)
	dimnames(exact2) <- list(
		tpsa[tpsa$treat == match.order[2],'id'],
		tpsa[tpsa$treat == match.order[3],'id'] )
	dimnames(exact3) <- list(
		tpsa[tpsa$treat == match.order[3],'id'],
		tpsa[tpsa$treat == match.order[1],'id'] )
	
	results <- data.frame(g1=character(), g2=character(), g3=character(), 
						  D1=numeric(), D2=numeric(), D3=numeric(),
						  stringsAsFactors=FALSE)
	
	counter.i <- 0
	for(i in dimnames(d1)[[1]]) {
		counter.i <- counter.i + 1
		row1 <- d1[i,]
		row1 <- row1[exact1[i,] & row1 < caliper[1]]
		row1 <- row1[order(row1)][min(length(row1), 1):min(length(row1), nmatch[1])]
		counter.j <- 0
		for(j in names(row1)) {
			counter.j <- counter.j + 1
			row2 <- d2[j,]
			row2 <- row2[exact2[j,] & row2 < caliper[2]]
			row2 <- row2[order(row2)][min(length(row2), 1):min(length(row2), nmatch[2])]
			for(k in names(row2)) {
				row3 <- d3[k,]
				row3 <- row3[exact3[k,] & row3 < caliper[3]]
				t3 <- as.numeric(row3[names(row3) == i])
				if(length(t3) > 0) {
					results <- rbind(results, data.frame(
						g1 = i,
						g2 = j,
						g3 = k,
						D1 = row1[j],
						D2 = row2[k],
						D3 = t3,
						stringsAsFactors=FALSE
					))
				}
			}
		}
		setTxtProgressBar(pb, 6 + counter.i)
	}
	setTxtProgressBar(pb, t)
	close(pb)

	names(results) <- c(match.order,
						paste('D.', getModel(ordering[1],ordering[2]), sep=''),
						paste('D.', getModel(ordering[2],ordering[3]), sep=''),
						paste('D.', getModel(ordering[3],ordering[1]), sep='')
	)
	
	results$Dtotal <- results$D.m1 + results$D.m2 + results$D.m3
	results <- results[order(results$Dtotal),]
	
	retain <- data.frame()
	for(i in 1:M2) {
		keep <- which(!duplicated(results[, 1:2]))
		if(length(keep) > 0) {
			retain <- rbind(retain, results[keep,])
			results <- results[-keep,]
		}
	}
	results <- retain
	row.names(results) <- 1:nrow(results)
	
	if(!missing(M1)) {
		retain <- data.frame()
		for(i in 1:M1) {
			keep <- which(!duplicated(results[,1]))
			if(length(keep) > 0) {
				retain <- rbind(retain, results[keep,])
				results <- results[-keep,]
			}
		}
		results <- retain
		row.names(results) <- 1:nrow(results)
	}
	
	unmatched <- tpsa[!(tpsa$id %in% c(results[,1], results[,2], results[,3])),]

	class(results) <- c('triangle.matches','data.frame')
	attr(results, 'triangle.psa') <- tpsa
	attr(results, 'match.order') <- match.order
	attr(results, 'unmatched') <- unmatched
	
	message(paste(prettyNum(nrow(unmatched) / nrow(tpsa) * 100, digits=2), 
				  '% of data points could not be matched.', sep=''))
	return(results)
}

require(compiler)
trimatch <- cmpfun(trimatch)
