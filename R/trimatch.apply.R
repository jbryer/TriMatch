#' Recursive function to find possible matched triplets using the apply functions.
#' 
#' Internal method.
#' 
#' @inheritParams trimatch
#' @param d1 matrix of distances between groups A and B
#' @param d2 matrix of distances between groups B and C
#' @param d3 matrix of distances between groups C and A
#' @param exact1 matrix of exact matches between groups A and B
#' @param exact2 matrix of exact matches between groups B and C
#' @param exact3 matrix of exact matches between groups C and A
trimatch.apply <- function(tpsa, caliper, d1, d2, d3, exact1, exact2, exact3, nmatch, status=TRUE) {	
	#Check the caliper
	d1[d1 > caliper[1]] <- NA
	d2[d2 > caliper[2]] <- NA
	d3[d3 > caliper[3]] <- NA
	#Check exact matches
	d1[!exact1] <- NA
	d2[!exact2] <- NA
	d3[!exact3] <- NA
	
	pb <- NULL
	if(status) { pb <- txtProgressBar(min=0, max=nrow(d1), style=3) }
	counter.i <- 0
	
	results <- lapply(dimnames(d1)[[1]], FUN=function(i1) {
		counter.i <- counter.i + 1
		row1 <- d1[i1,]
		row1 <- row1[!is.na(row1)]
		row1 <- row1[order(row1)]
		lapply(names(row1)[seq_len(min(nmatch[1], length(row1)))], FUN=function(i2) {
			row2 <- d2[i2,]
			row2 <- row2[!is.na(row2)]
			row2 <- row2[order(row2)]
			lapply(names(row2)[seq_len(min(nmatch[2], length(row2)))], FUN=function(i3) {
				val <- d3[i3,i1]
				counter.i <- counter.i + 1
				if(status) { setTxtProgressBar(pb, counter.i ) }
				if(is.na(val)) {
					return(c())
				} else {
					c(i1, i2, i3, row1[i2], row2[i3], val)
				}
			})
		})
	})
	
	if(status) {
		setTxtProgressBar(pb, nrow(d1))
		close(pb)
	}
	
	results <- as.data.frame(matrix(unlist(results), ncol=6, byrow=TRUE), stringsAsFactors=FALSE)
	for(i in 4:6) { results[,i] <- as.numeric(results[,i]) }
	invisible(results)
}
