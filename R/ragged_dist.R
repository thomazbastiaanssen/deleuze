#' Compute pairwise distances while ignoring shared zeores/lowest values. 
#' 
#' @description
#' Works like `dist`.
#' @importFrom stats as.dist
#' @export
#' 
ragged_dist <- function(x, upper = FALSE, diag = FALSE){
  ZAP <- 1e-15
  d   <- r_dist_cpp(x)
  d[d < ZAP] <- 0
  if (any(is.na(d)))
    warning("missing values in results")
  # d <- structure(d, class = 'dist',
  #           Size = nrow(x))
  d <- as.dist(d)
  ## add attribute maxdist: the maximum value of the distance function
  attr(d, "maxdist") <- max(d)
  attr(d, "Labels")  <- dimnames(x)[[1]]
  attr(d, "call")    <- match.call()
  d
}
