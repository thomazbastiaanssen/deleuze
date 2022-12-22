#' samples from the probability density function of a feature in count data.  
#'
#' @description See rbeta.
#' @param samples An integer. How many samples should be taken
#' @param k An integer. How many counts were observerd
#' @param n An integer. How many obserations were made
#' 
#' @export
#'
sampleBetaBinom <- function(samples, k, n){
  #wrap around rbeta for the special case of Binom()
  rbeta(n = samples, shape1 = k + 1, shape2 = n - k + 1)
}

#' samples from the log() of the geometric mean.
#'
#' @param samples An integer. How many samples should be taken
#' @param count_samples A vector of count data.
#' 
#' @export
#' 
sampleGeomMeam <- function(samples, count_sample){
  #Make a matrix with resamples for each feature.
  sample_list = sapply(count_sample, FUN = function(x){sampleBetaBinom(samples = samples, k = x, n = sum(data))}, simplify = T)
  
  #Take the average of the log-transformed values, which corresponds to the log-transformed geometric mean.
  apply(X = log(sample_list), MARGIN = 1, FUN = mean)
}
