#' Generate random indices
#'
#' @author Frankie Cho
#'
#' @description The `ahp.ri` function calculates the mean consistency indices of a specific numbers of random number pairwise comparison matrices.
#'
#' The random index of one pairwise comparison matrix is given as below, where \eqn{lambda} is the maximum eigenvalue and \eqn{n} is the number of attributes.
#' 
#' \deqn{RI = (\lambda-n)/((n-1)}
#' 
#' `ahp.ri` creates `nsims` number of pairwise comparison matrices with number of dimensions=`dim`, and returns its average.
#'
#' @param nsims Number of random pairwise comparison matrices to be generated. Processing time increases substantially with higher `nsims`.
#' @param dim Number of dimensions of the matrix.
#' @param seed The random number generator seed for reproducibility, which is same as `set.seed`. By default, `seed = 42`.
#'
#' @return The generated random index, which is numeric.
#'
#' @examples
#'
#' ahp.ri(nsims = 10000, dim = 5, seed = 42)
#'
#'
#'@export


ahp.ri <- function(nsims, dim, seed = 42) {
  
  # Break if dim is not an integer.
  if (dim%%1!=0){
    stop("Number of dimensions must be an integer.")
  }
  
  # Break if dim is equal or less than 2
  if (dim < 3){
    stop("Number of dimensions cannot be lower than 3.")
  }
  
  # Set random seed for reproducibility
  set.seed(seed)
  
  # genri is a function where it takes a dimension and returns the 
  # consistency index of that random pairwise comparison matrix with 
  # dimensions dim
  genri <- function(dim){
    
    saatyscale <- c(1/9,1/8,1/7,1/6,1/5,1/4,1/3,1/2,1:9)
    
    # Draw from the Saaty Scale n(n-1)/2 values
    draws <- sample(saatyscale, 0.5*dim*(dim-1), replace = TRUE)
    
    # Make a PCM using ahp.mat, extracting the first matrix
    .mat <- matrix(data = NA, nrow = dim, ncol = dim)
    .mat[row(.mat) == col(.mat)] <- 1
    .mat[lower.tri(.mat, diag = FALSE)] <- draws[1:(0.5*dim*(dim-1))]
    
    for (i in 1:nrow(.mat)) {
      for (j in 1:ncol(.mat)) {
        if (is.na(.mat[i, j])) {
          .mat[i, j] <- 1/.mat[j,i]
        }
      }
    }
    
    # Return the maximum eigenvalue
    max_lambda <- max(Re(eigen(.mat)$values))
    
    # Calculating the consistency ratio using the maximum eigenvalue
    ri <- (max_lambda - dim)/(dim - 1)
    
    # Returns the CR of the random matrix
    return(ri)
  }
  
  mean(replicate(nsims, genri(dim)))
}



