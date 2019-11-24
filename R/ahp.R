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
#' @param ID the column name, or a vector of column names, of variables in `df`, which are used to indicate specific observations. For example, if a survey dataset comes with an identifier/ multiple identified for each observation, use this so that the output dataframe would contain this identifier.
#' @param col a list of column names which contains the columns for the pairwise comparison matrix in chronological order, as specified in `atts`. For example, if `atts` is `c("A","B","C")`, then `col` would be `c("A_B","A_C","B_C")`, with `"A_B"` being a pairwise comparison of attribute A and B. `col = NULL` by default, indicating that the dataframe specified already contained the pairwise comparisons in correct order and nothing else except of what is inside `ID`.
#' @param suppress Suppresses the supplementary indices. Can take the values of "all" (suppresses all indices), "pwerror" (suppresses pwerror), or "cr" (suppresses consistency ratio).
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

ahp <- function (df, atts, negconvert = FALSE, reciprocal = TRUE, method = "eigen", 
                 aggmethod = "geometric", qt = 0, censorcr = Inf, agg = FALSE, ID = NULL, 
                 col = NULL, suppress = "none"){
  # Save names in a separate column
  if (is.null(ID)==FALSE) {
    if (is.vector(ID) == FALSE & is.character(ID) == FALSE) {
      stop("ID must be a vector or character.")
    }
    
    # Check if ID is in the data frame
    for (n in ID){
      if (n %in% names(df) == FALSE){
        stop("ID is not in dataframe supplied. Mistyped ID column name?")
      }
    }
    
    ID_saved <- df[,ID]
    
    # Remove data frame ID column
    df[,ID] <- NULL
  }
  
  # If columns are selected, then order it based on a particular order
  if (is.null(col) == FALSE){
    
    # Check if the column names were correctly specified
    for (n in col){
      if (n %in% names(df) == FALSE){
        stop("One or more elements in col are not in dataframe supplied. Mistyped col column name?")
      }
    }
    
    # Subset dataframe based on col
    df <- df[,col]
  }
  
  # Stop if Censor CR = 0, because that is simply wrong
  if (censorcr <= 0) stop("Censor CR cannot be equal or smaller than zero, because you will be throwing away all of your observations.")
  
  # Check whether everything in the data frame is within -9 and 9
  for (i in 1:nrow(df)){
    for (j in 1:ncol(df)){
      if (is.numeric(df[i,j]) == FALSE) {
        stop(paste("Non numeric element(s) in", names(df)[i], " column. Missing or miscoded data?"))
      } else if (df[i,j] > 9 | df[i,j] < -9){
        stop("There is something larger than 9 or smaller than -9 in your dataset. Miscoded?")
      }
    }
  }
  
  # Generate a AHP matrix
  ahpmat <- ahp.mat(df = df, atts = atts, negconvert = negconvert, 
                    reciprocal = reciprocal)
  
  # Calculate consistency ratios
  cr <- ahp.cr(ahpmat = ahpmat, atts = atts)
  
  # Dummy code CR into remove (0) or not (1)
  censorlist <- sapply(cr, function(x) x <= censorcr)
  
  # Keep only the consistent ahp matrices
  cahpmat <- ahpmat[censorlist]
  
  # Make a list of only the non-censored people ID
  
    if (is.null(ID)==FALSE) {
      if (length(ID) > 1){
        cID <- ID_saved[censorlist,]
      } else {
    cID <- ID_saved[censorlist]
      }
    }
  
  
  # Print number of censored observations
  if (censorcr != Inf) print(paste("Number of observations censored =", length(censorlist[censorlist == FALSE])))
  
  # Make individual prirorities
  indpref <- ahp.indpref(ahpmat = cahpmat, atts = atts, method = method)
  
  # Make pairwise comparison errors
  pwerror <- ahp.pwerror(ahpmat = cahpmat, atts = atts)
  
  if (is.null(ID)==FALSE) {
    output <- cbind(cID, indpref, CR = cr[censorlist], pwerror, row.names = NULL)
  } else {
    output <- cbind(indpref, CR = cr[censorlist], pwerror, row.names = NULL)
  }
  
  if (suppress == "all"){
    output$CR <- NULL
    output$top1 <- output$top2 <- output$top3 <- NULL
  } else if (suppress == "cr"){
    output$CR <- NULL
  } else if (suppress == "pwerror"){
    output$top1 <- output$top2 <- output$top3 <- NULL
  }
  
  if (agg == TRUE) {
    agg <- ahp.aggpref(ahpmat = cahpmat, atts = atts, method = method, 
                       aggmethod = aggmethod, qt = qt)
    agg.sd <- ahp.aggpref(ahpmat = cahpmat, atts = atts, 
                          method = method, aggmethod = "sd", qt = qt)
    agg.output <- cbind(AggPref = agg, SD.AggPref = agg.sd, 
                        row.names = NULL)
    output <- list(indpref = output, aggpref = agg.output)
  }
  return(output)
}
