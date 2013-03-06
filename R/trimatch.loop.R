#' Recursive function to find possible matched triplets using nested for loops.
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
trimatch.loop <- function(tpsa, d1, d2, d3, exact1, exact2, exact3, nmatch) {
	t <- length(which(tpsa$treat == match.order[1]))
	pb <- txtProgressBar(min=0, max=t, style=3)
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
		setTxtProgressBar(pb, counter.i)
	}
	setTxtProgressBar(pb, t)
	close(pb)
	
	return(results)
}
