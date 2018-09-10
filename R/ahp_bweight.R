ahp.bweight <- function(currentmat, atts, maxdf, nelement) {
    ## Import the current matrix
    .B <- currentmat
    .maxdf <- maxdf
    
    ## Replace the replacement matrix to zeros
    for (n in 1:nelement) {
        i <- as.numeric(.maxdf[n, 1])
        j <- as.numeric(.maxdf[n, 2])
        
        .B[i, j] <- 0
        .B[j, i] <- 0
    }
    
    ## Detect the number of zeros in each row
    .zerorows <- apply(.B == 0, 1, sum) + 1
    
    ## Replace the diagonals with 1+ number of zeros
    for (i in 1:nrow(.B)) {
        .B[i, i] <- .zerorows[i]
    }
    
    ## Calculate weights of .Blist
    .Bweights <- ahp.indpref(list(.B), atts, method = "eigen")
    .Bweights
}
