#' Compute ANOVA
#' 
#' *Note that this adapted from the `ez` R package available here: https://github.com/mike-lawrence/ez
#' It appears the `ez` package is no longer maintained so the function was copied here.*
#' 
#' This function provides easy analysis of data from factorial experiments, including purely 
#' within-Ss designs (a.k.a. “repeated measures”), purely between-Ss designs, and mixed 
#' within-and-between-Ss designs, yielding ANOVA results, generalized effect sizes and 
#' assumption checks.
#' 
#' @details
#' 
#' ANCOVA is implemented by first regressing the DV against each covariate (after collapsing the 
#' data to the means of that covariate's levels per subject) and subtracting from the raw data the 
#' fitted values from this regression (then adding back the mean to maintain scale). These 
#' regressions are computed across Ss in the case of between-Ss covariates and computed within 
#' each Ss in the case of within-Ss covariates.
#' 
#' **Warning**
#' 
#' Prior to running (though after obtaining running ANCOVA regressions as described in the details 
#' section), dv is collapsed to a mean for each cell defined by the combination of wid and any 
#' variables supplied to within and/or between and/or diff. Users are warned that while convenient 
#' when used properly, this automatic collapsing can lead to inconsistencies if the pre-collapsed 
#' data are unbalanced (with respect to cells in the full design) and only the partial design is 
#' supplied to ezANOVA. When this is the case, use within_full to specify the full design to ensure 
#' proper automatic collapsing.
#' 
#' @param data Data frame containing the data to be analyzed.
#' @param dv Name of the column in data that contains the dependent variable. Values in this column 
#' must be numeric.
#' @param wid Name of the column in data that contains the variable specifying the case/Ss 
#' identifier. This should be a unique value per case/Ss.
#' @param within Names of columns in data that contain predictor variables that are manipulated 
#' (or observed) within-Ss. If a single value, may be specified by name alone; if multiple values, 
#' must be specified as a .() list.
#' @param within_full Same as within, but intended to specify the full within-Ss design in cases 
#' where the data have not already been collapsed to means per condition specified by within and 
#' when within only specifies a subset of the full design.
#' @param within_covariates Names of columns in data that contain predictor variables that are 
#' manipulated (or observed) within-Ss and are to serve as covariates in the analysis. If a single 
#' value, may be specified by name alone; if multiple values, must be specified as a .() list.
#' @param between Names of columns in data that contain predictor variables that are manipulated 
#' (or observed) between-Ss. If a single value, may be specified by name alone; if multiple values,
#'  must be specified as a .() list.
#' @param between_covariates Names of columns in data that contain predictor variables that are 
#' manipulated (or observed) between-Ss and are to serve as covariates in the analysis. If a single 
#' value, may be specified by name alone; if multiple values, must be specified as a .() list.
#' @param observed Names of columns in data that are already specified in either within or between 
#' that contain predictor variables that are observed variables (i.e. not manipulated). If a single 
#' value, may be specified by name alone; if multiple values, must be specified as a .() list. The 
#' presence of observed variables affects the computation of the generalized eta-squared measure of 
#' effect size reported by ezANOVA.
#' @param diff Names of any variables to collapse to a difference score. If a single value, may be 
#' specified by name alone; if multiple values, must be specified as a .() list. All supplied 
#' variables must be factors, ideally with only two levels (especially if setting the reverse_diff 
#' argument to TRUE).
#' @param reverse_diff Logical. If TRUE, triggers reversal of the difference collapse requested by 
#' diff. Take care with variables with more than 2 levels.
#' @param type Numeric value (either 1, 2 or 3) specifying the Sums of Squares "type" to employ 
#' when data are unbalanced (eg. when group sizes differ). type = 2 is the default because this will 
#' yield identical ANOVA results as type = 1 when data are balanced but type = 2 will additionally 
#' yield various assumption tests where appropriate. When data are unbalanced, users are warned that 
#' they should give special consideration to the value of type. type=3 will emulate the approach 
#' taken by popular commercial statistics packages like SAS and SPSS, but users are warned that this 
#' approach is not without criticism.
#' @param white.adjust Only affects behavior if the design contains only between-Ss predictor 
#' variables. If not FALSE, the value is passed as the white.adjust argument to Anova, which 
#' provides heteroscedasticity correction. See Anova for details on possible values.
#' @param detailed Logical. If TRUE, returns extra information (sums of squares columns, intercept 
#' row, etc.) in the ANOVA table.
#' @param return_aov Logical. If TRUE, computes and returns an aov object corresponding to the 
#' requested ANOVA (useful for computing post-hoc contrasts).
#' 
#' @return 
#' A list containing one or more of the following components:
#' 
#' \describe{
#' \item{ANOVA}{A data frame containing the ANOVA results.}
#' \item{Mauchly's Test for Sphericity}{If any within-Ss variables with >2 levels are present, a data frame containing the results of Mauchly's test for Sphericity. Only reported for effects >2 levels because sphericity necessarily holds for effects with only 2 levels.}
#' \item{Sphericity Corrections}{If any within-Ss variables are present, a data frame containing the Greenhouse-Geisser and Huynh-Feldt epsilon values, and corresponding corrected p-values.}
#' \item{Levene's Test for Homogeneity}{If the design is purely between-Ss, a data frame containing the results of Levene's test for Homogeneity of variance. Note that Huynh-Feldt corrected p-values where the Huynh-Feldt epsilon >1 will use 1 as the correction epsilon.}
#' \item{aov}{An aov object corresponding to the requested ANOVA.}
#' }
#' 
#' Some column names in the output data frames are abbreviated to conserve space:
#' 
#' \describe{
#' \item{DFn}{Degrees of Freedom in the numerator (a.k.a. DFeffect).}
#' \item{DFd}{Degrees of Freedom in the denominator (a.k.a. DFerror).}
#' \item{SSn}{Sum of Squares in the numerator (a.k.a. SSeffect).}
#' \item{SSd}{Sum of Squares in the denominator (a.k.a. SSerror).}
#' \item{F}{F-value.}
#' \item{p}{p-value (probability of the data given the null hypothesis).}
#' \item{p<.05}{Highlights p-values less than the traditional alpha level of .05.}
#' \item{ges}{Generalized Eta-Squared measure of effect size (see in references below: Bakeman, 2005).}
#' \item{GGe}{Greenhouse-Geisser epsilon.}
#' \item{p[GGe]}{p-value after correction using Greenhouse-Geisser epsilon.}
#' \item{p[GGe]<.05}{Highlights p-values (after correction using Greenhouse-Geisser epsilon) less than the traditional alpha level of .05.}
#' \item{HFe}{Huynh-Feldt epsilon.}
#' \item{p[HFe]}{p-value after correction using Huynh-Feldt epsilon.}
#' \item{p[HFe]<.05}{Highlights p-values (after correction using Huynh-Feldt epsilon) less than the traditional alpha level of .05.}
#' \item{W}{Mauchly's W statistic}
#' }
#' 
#' @author Mike Lawrence
#' @references Bakeman, R. (2005). Recommended effect size statistics for repeated measures designs. 
#' Behavior Research Methods, 37 (3), 379-384.
#' 
#' @import plyr
#' @importFrom car Anova
#' @importFrom stringr str_count str_detect
ezANOVA <-	function(
		data,
		dv,
		wid,
		within = NULL,
		within_full = NULL,
		within_covariates = NULL,
		between = NULL,
		between_covariates = NULL,
		observed = NULL,
		diff = NULL,
		reverse_diff = FALSE,
		type = 1,
		white.adjust = FALSE,
		detailed = FALSE,
		return_aov = FALSE) {
	if (inherits(data, "tbl_df")) {
		data <- as.data.frame(data)
	}
	
	args_to_check = c(
		'dv',
		'wid',
		'within',
		'between',
		'observed',
		'diff',
		'within_full',
		'within_covariates',
		'between_covariates'
	)
	
	args <- as.list(match.call()[-1])
	for(i in 1:length(args)) {
		arg_name <- names(args)[i]
		if(arg_name %in% args_to_check) {
			if(is.symbol(args[[i]])) {
				code <- paste(arg_name, '=.(', as.character(args[[i]]), ')', sep = '')
				eval(parse(text = code))
			} else {
				if(is.language(args[[i]])) {
					arg_vals <- as.character(args[[i]])
					arg_vals <- arg_vals[2:length(arg_vals)]
					arg_vals <- paste(arg_vals, collapse = ',')
					code <- paste(arg_name, '=.(', arg_vals, ')', sep = '')
					eval(parse(text = code))
				}
			}
		}
	}
	
	to_return = ezANOVA_main(
		data = data,
		dv = dv,
		wid = wid,
		within = within,
		within_full = within_full,
		within_covariates = within_covariates,
		between = between,
		between_covariates = between_covariates,
		diff = diff,
		reverse_diff = reverse_diff,
		type = type,
		return_aov = return_aov,
		white.adjust = white.adjust
	)
	
	########
	# Compute effect size
	########
	if(!white.adjust) {
		if (!is.null(observed)) {
			obs <- rep(F, nrow(to_return$ANOVA))
			for (i in as.character(observed)) {
				obs <- obs | stringr::str_detect(to_return$ANOVA$Effect, i)
			}
			obs_SSn1 <- sum(to_return$ANOVA$SSn * obs)
			obs_SSn2 <- to_return$ANOVA$SSn * obs
		} else{
			obs_SSn1 <- 0
			obs_SSn2 <- 0
		}
		to_return$ANOVA$ges <- to_return$ANOVA$SSn / (to_return$ANOVA$SSn + sum(unique(to_return$ANOVA$SSd)) +
													 	obs_SSn1 - obs_SSn2)
	}
	
	########
	# Final clean-up
	########
	
	#remove the data & type label from to_return
	to_return <- to_return[!(names(to_return) %in% c('data', 'type'))]
	
	#if necessary, remove extra columns and the Intercept row from the anova
	if (!detailed) {
		to_return$ANOVA <- to_return$ANOVA[, names(to_return$ANOVA) %in% c('Effect', 'DFn', 'DFd', 'F', 'p', 'p<.05', 'ges')]
		row_keep <- !stringr::str_detect(to_return$ANOVA$Effect, '(Intercept)')
		to_return$ANOVA <- to_return$ANOVA[row_keep, ]
	}
	
	#all done!
	return(to_return)
}




