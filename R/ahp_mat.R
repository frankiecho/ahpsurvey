#' Generate AHP pairwise matrices from survey data
#'
#' @author Frankie Cho
#'
#' @description `ahp.mat` takes in paired comparisons from survey data with questions using the
#' analytic hierarchy process and converts it into pairwise comparison matrices
#' for each individual decision-maker. Examples based on \insertCite{Saaty2004;textual}{ahpsurvey}.
#'
#' @param df a dataframe, each row corresponding to one decision-maker, with columns ordered according to  `atts`.
#' @param atts a list of attributes in the correct order
#' @param negconvert logical, whether to convert all positive values to
#'  negative. In the pairwise comparison A_B, if -6 denotes A is more
#'  important than B by 6 units, set `negconvert = TRUE`.
#' @param reciprocal logical, whether to convert negative values (after `negconvert`) to its reciprocal. If the comparison A_B where B is more important than A was already entered in its reciprocal (e.g. 1/6), choose `reciprocal = FALSE`. When `reciprocal = FALSE`, do not set `negconvert = TRUE`.
#' @return A list of pairwise comparison matrices of each decision-maker.
#'
#' @examples
#'
#'
#' data(city200)
#' atts <- c('cult', 'fam', 'house', 'jobs', 'trans')
#' ahp.mat(df = city200, atts = atts, negconvert = TRUE)
#'
#'@references
#'
#'\insertRef{Saaty2004}{ahpsurvey}
#'
#'@export

ahp.mat <- function(df, atts, negconvert = FALSE, reciprocal = TRUE) {
    
  ## If reciprocal = FALSE, negconvert cannot be set to TRUE.
  
  if (reciprocal == FALSE & negconvert == TRUE){
    stop("Error: cannot set reciprocal = FALSE & negconvert = TRUE, because data was already assumed to be all positive.")
  }
  
    ## Since in the data, if the attribute in the left is the more important the data is
    ## negative, here I will invert the whole matrix so that the attribute coming first is
    ## rated positively Inversion rule set
    if (missing(negconvert) | negconvert == FALSE) {
    } else if (negconvert == TRUE) {
        df <- -df
    }
    
    ## Tri computes the number of pairwise comparisons given a particular length of the
    ## attributes
    tri <- function(n) {
        x <- (n - 1) * (n)/2
        x
    }
    
    ## Inverting all negative values to its reciprocals
    if (reciprocal == TRUE){
      for (x in 1:ncol(df)) {
          for (y in 1:nrow(df)) {
              if (is.na(df[y, x]) == TRUE) {
              } else if (df[y, x] < 0) {
                  df[y, x] <- 1/(-df[y, x])
              }
          }
      }
    }
    
    respmat <- list()
    for (i in 1:nrow(df)) {
        ## Decompose input data frame into a vector
        .ind <- as.vector(t(df[i, ]))
        .mat <- matrix(data = NA, nrow = length(atts), ncol = length(atts))
        
        ## Assign all diagonals as 1
        .mat[row(.mat) == col(.mat)] <- 1
        
        ## Fill in the attributes in the **upper** triangle of the matrix, and transpose it
        .mat[lower.tri(.mat, diag = FALSE)] <- .ind[1:tri(length(atts))]
        .mat <- t(.mat)
        
        ## Create attribute names
        colnames(.mat) <- atts
        rownames(.mat) <- atts
        
        ## Saving the resulting matrix into a list
        respmat[[i]] <- .mat
    }
    
    ## Filling in the lower triangle of the matrix as a_ij = 1/a_ji
    for (ind in 1:nrow(df)) {
        for (i in 1:nrow(respmat[[ind]])) {
            for (j in 1:ncol(respmat[[ind]])) {
                if (is.na(respmat[[ind]][i, j]) == TRUE) {
                  respmat[[ind]][i, j] <- 1/respmat[[ind]][j, i]
                }
            }
        }
    }
    respmat
}
