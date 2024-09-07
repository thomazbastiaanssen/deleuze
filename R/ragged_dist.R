#' Compute pairwise distances while ignoring shared zeroes/lowest values. 
#' 
#' @description
#' Works like `dist`.
#' @param x a numeric matrix
#' @param upper logical value indicating whether the upper triangle of the distance matrix should be printed by `print.dist`.
#' @param diag logical value indicating whether the diagonal of the distance matrix should be printed by `print.dist`.
#' @importFrom stats as.dist
#' @export
#' 
pw_dist <- function(x, upper = FALSE, diag = FALSE){
  #Rcpp needs a df
  if(!is.matrix(x)) {x = as.matrix(x)}
  
  ZAP <- 1e-15
  d   <- pw_dist_cpp(x)
  d[d < ZAP] <- 0
  if (any(is.na(d)))
    warning("missing values in results")

  d <- as.dist(d, upper, diag)
  ## add attribute maxdist: the maximum value of the distance function
  attr(d, "maxdist") <- max(d)
  attr(d, "method")  <- "euclidean"
  attr(d, "Labels")  <- colnames(x)
  attr(d, "call")    <- match.call()
  d
}
