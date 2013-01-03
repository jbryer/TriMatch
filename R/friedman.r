#' 
#' http://www.r-statistics.com/2010/02/post-hoc-analysis-for-friedmans-test-r-code/
#' @author Tal Galili
friedman.test.with.post.hoc <- function(formu, data, 
										to.print.friedman = T, 
										to.post.hoc.if.signif = T,  
										to.plot.parallel = T, 
										to.plot.boxplot = T, 
										signif.P = .05, 
										color.blocks.in.cor.plot = T, 
										jitter.Y.in.cor.plot =F) {
	# formu is a formula of the shape: 	Y ~ X | block
	# data is a long data.frame with three columns:	[[ Y (numeric), X (factor), block (factor) ]]
	
	# Note: This function doesn't handle NA's! In case of NA in Y in one of the blocks, 
	# then that entire block should be removed.
	# Loading needed packages
	require(coin)
	require(multcomp)
	require(colorspace)
	
	# get the names out of the formula
	formu.names <- all.vars(formu)
	Y.name <- formu.names[1]
	X.name <- formu.names[2]
	block.name <- formu.names[3]
	
	# In case we have a "data" data frame with more then the three columns we need. 
	# This code will clean it from them...
	if(dim(data)[2] >3) data <- data[,c(Y.name,X.name,block.name)]	
	
	# Note: the function doesn't handle NA's. In case of NA in one of the block 
	# T outcomes, that entire block should be removed.
	
	# stopping in case there is NA in the Y vector
	if(sum(is.na(data[,Y.name])) > 0) stop(paste("Function stopped: ", 
				"This function doesn't handle NA's. In case of NA in Y in one of ", 
				"the blocks, then that entire block should be removed.", sep=''))
	
	# make sure that the number of factors goes with the actual values present in the data:
	data[,X.name ] <- factor(data[,X.name ])
	data[,block.name ] <- factor(data[,block.name ])
	number.of.X.levels <- length(levels(data[,X.name ]))
	if(number.of.X.levels == 2) { 
		warning(paste("'",X.name,"'", "has only two levels. ", 
				"Consider using paired wilcox.test instead of friedman test"))
	}
	
	# making the object that will hold the friedman test and the other.
	the.sym.test <- symmetry_test(formu, data = data,	### all pairwise comparisons
						teststat = "max",
						xtrafo = function(Y.data) { 
							trafo( Y.data, factor_trafo = function(x) { 
								model.matrix(~ x - 1) %*% t(contrMat(table(x), "Tukey")) 
							} ) 
						},
						ytrafo = function(Y.data){ 
							trafo(Y.data, numeric_trafo = rank, block = data[,block.name] ) 
						}
	)
	# if(to.print.friedman) { print(the.sym.test) }
	
	
	if(to.post.hoc.if.signif) {
		if(pvalue(the.sym.test) < signif.P)	{
			# the post hoc test
			# this is the post hoc of the friedman test		
			The.post.hoc.P.values <- pvalue(the.sym.test, method = "single-step")	
			
			# plotting
			if(to.plot.parallel & to.plot.boxplot)	{
				# if we are plotting two plots, let's make sure we'll be able to see both
				par(mfrow = c(1,2)) 
			}
			
			if(to.plot.parallel) {
				X.names <- levels(data[, X.name])
				X.for.plot <- seq_along(X.names)
				# adding some spacing from both sides of the plot
				plot.xlim <- c(.7 , length(X.for.plot)+.3)	
				
				if(color.blocks.in.cor.plot) { 
					blocks.col <- rainbow_hcl(length(levels(data[,block.name])))
				} else {
					blocks.col <- 1 # black
				}
				
				data2 <- data
				if(jitter.Y.in.cor.plot) {
					data2[,Y.name] <- jitter(data2[,Y.name])
					par.cor.plot.text <- "Parallel coordinates plot (with Jitter)"
				} else {
					par.cor.plot.text <- "Parallel coordinates plot"
				}
				
				# adding a Parallel coordinates plot
				matplot(as.matrix(reshape(data2,  idvar=X.name, timevar=block.name,
										  direction="wide")[,-1])  ,
						type = "l",  lty = 1, axes = FALSE, ylab = Y.name,
						xlim = plot.xlim,
						col = blocks.col,
						main = par.cor.plot.text)
				axis(1, at = X.for.plot , labels = X.names) # plot X axis
				axis(2) # plot Y axis
				points(tapply(data[,Y.name], data[,X.name], median) ~ X.for.plot, 
					   col = "red",pch = 4, cex = 2, lwd = 5)
			}
			
			if(to.plot.boxplot) {
				# first we create a function to create a new Y, by substracting 
				# different combinations of X levels from each other.
				subtract.a.from.b <- function(a.b , the.data) {
					the.data[,a.b[2]] - the.data[,a.b[1]]
				}
				
				temp.wide <- reshape(data,  idvar=X.name, timevar=block.name,
								direction="wide")
				wide.data <- as.matrix(t(temp.wide[,-1]))
				colnames(wide.data) <- temp.wide[,1]
				
				Y.b.minus.a.combos <- apply(with(data,combn(levels(data[,X.name]), 2)), 2, 
								subtract.a.from.b, the.data =wide.data)
				names.b.minus.a.combos <- apply(with(data,combn(levels(data[,X.name]), 2)), 2, 
								function(a.b) {paste(a.b[2],a.b[1],sep=" - ")})
				
				the.ylim <- range(Y.b.minus.a.combos)
				# adding some space for the labels
				#This is now deprecated: the.ylim[2] <- the.ylim[2] + max(sd(Y.b.minus.a.combos))
				the.ylim[2] <- the.ylim[2] + max(apply(Y.b.minus.a.combos, 2, sd))
				is.signif.color <- ifelse(The.post.hoc.P.values < .05 , "green", "grey")
				
				boxplot(Y.b.minus.a.combos,
						names = names.b.minus.a.combos ,
						col = is.signif.color,
						main = "Boxplots (of the differences)",
						ylim = the.ylim
				)
				legend("topright", legend = paste(names.b.minus.a.combos, 
								rep(" ; PostHoc P.value:", number.of.X.levels),
								round(The.post.hoc.P.values,5)) , fill =  is.signif.color )
				abline(h = 0, col = "blue")
			}
			
			list.to.return <- list(Friedman.Test = the.sym.test, 
								   PostHoc.Test = The.post.hoc.P.values)
			if(to.print.friedman) { print(list.to.return) }
			return(list.to.return)
			
		} else {
			print("The results where not significant, There is no need for a post hoc test")
			return(the.sym.test)
		}
	}
	# Original credit (for linking online, to the package that performs the post 
	# hoc test) goes to "David Winsemius", see:
	# http://tolstoy.newcastle.edu.au/R/e8/help/09/10/1416.html
}
