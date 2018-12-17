#' Canned routine for AHP
#'
#' @author Frankie Cho
#'
#' @description `ahp` is a canned routine that returns a `data.frame` of individuals' priorities (based on ahp.indpref), number of missing values, consistency ratios, and top three pairs of inconsistent pairwise comparisons. An optional argument `agg` allows the user to generate a data.frame of aggregated priorities alongside individual priorities.
#'
#' @param df a dataframe, each row corresponding to one decision-maker, with columns ordered according to  `atts`.
#' @param atts a list of attributes in the correct order
#' @param negconvert logical, whether to convert all positive values to
#'  negative. In the pairwise comparison A_B, if -6 denotes A is more
#'  important than B by 6 units, set `negconvert = TRUE`.
#' @param reciprocal logical, whether to convert negative values (after `negconvert`) to its reciprocal. If the comparison A_B where B is more important than A was already entered in its reciprocal (e.g. 1/6), choose `reciprocal = FALSE`. When `reciprocal = FALSE`, do not set `negconvert = TRUE`.
#' @param method if `method = "eigen"`, the individual priority weights are computed using the Dominant Eigenvalues method described in \insertCite{Saaty2003;textual}{ahpsurvey}. Otherwise, then the priorities are computed based on the averages of normalized values. Basically it normalizes the matrices so that all of the columns add up to 1, and then computes the averages of the row as the priority weights of each attribute. Three modes of finding the averages are available:  ``arithmetic``: the arithmetic mean; ``geometric``: the geometric mean (the default); ``rootmean``: the square root of the sum of the squared value.
#' @param aggmethod how to aggregate the individual priorities. By default `aggmethod = method`. Apart from the methods offered in `method`, `aggmethod` also permits three other options: `tmean` computes the trimmed arithmetic mean, `tgmean` computes the trimmed geometric mean (both with quantiles trimmed based on `qt`), and `sd` computes the standard deviation from the arithmetic mean. If `method = "eigen"` and `aggmethod` is not specified, `aggmethod` defaults to `"geometric"`.
#' @param qt specifies the quantile which the top **and** bottom priority weights are trimmed. Used only if `aggmethod = 'tmean'` or `aggmethod = 'tgmean'`. For example, `qt = 0.25` specifies that the aggregation is the arithmetic mean of the values from the 25 to 75 percentile. By default `qt = 0`.
#' @param censorcr the threshold of censoring observations. All observations with a consistency ratio higher than the specified CR will be dropped from the routine. By default, ``censorcr = Inf``, i.e. drops no observations.
#' @param agg by default, `agg = FALSE`, which suppresses the output of Aggregated priorities. If `agg = TRUE`, the output will be a list where `$AggPref` contains the Aggregated priorities and Standard Deviations.
#' @return If ``agg = TRUE``, returns a list of the results with two elements, `indpref` and `aggpref`. `$indpref` contains individual preference weight (same as `ahp.indpref`), consistency ratios, and the top three most inconsistent pairwise comparisons (same as `ahp.pwerror`). `$aggpref` contains the Aggregated Priorities and Standard Deviations. Otherwise (i.e. default setting), outputs a `data.frame` with only `$indpref`.
#'
#' @examples
#'
#' data(city200)
#' ahp(df = city200, atts = c('cult', 'fam', 'house', 'jobs', 'trans'), negconvert = TRUE)
#'
#'@references
#'
#'\insertRef{Saaty2004}{ahpsurvey}
#'
#'@include ahp_mat.R ahp_indpref.R ahp_cr.R ahp_pwerror.R
#'
#'
#'@export
#'

ahp <- function(df, atts, negconvert = FALSE, reciprocal = TRUE, method = 'eigen', aggmethod = 'geometric', qt = 0, censorcr = Inf, agg = FALSE){
  ahpmat <- ahp.mat(df = df, atts = atts, negconvert = negconvert, reciprocal = reciprocal)
  cr <- ahp.cr(ahpmat = ahpmat, atts = atts)
  
  ## Censor observations based on CR -- generate list of conditions of whether to keep or not
  censorlist <- sapply(cr, function(x) x <= censorcr)
  
  ## make a new censored AHP mat
  cahpmat <- ahpmat[censorlist]
  
  ## Print number of censored observations
  print(paste("Number of observations censored =", length(censorlist[censorlist==FALSE])))
  
  ## Recalculate list of CR based on censored AHP mat
  ccr <- ahp.cr(ahpmat = cahpmat, atts = atts)
  
  indpref <- ahp.indpref(ahpmat = cahpmat, atts = atts, method = method)
  pwerror <- ahp.pwerror(ahpmat = cahpmat, atts = atts)
  
  output <- cbind(indpref, CR = ccr, pwerror, row.names = NULL)
  
  if (agg == TRUE){
    agg <- ahp.aggpref(ahpmat = cahpmat, atts = atts, method = method, aggmethod = aggmethod, qt = qt)
    agg.sd <- ahp.aggpref(ahpmat = cahpmat, atts = atts, method = method, aggmethod = 'sd', qt = qt)
    agg.output <- cbind(AggPref = agg, SD.AggPref = agg.sd, row.names = NULL)
    output <- list(indpref = output, aggpref = agg.output)
  }
  return(output)
}