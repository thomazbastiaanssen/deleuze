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
#' @param count_sample A vector of count data.
#' 
#' @export
#' 
sampleGeomMeam <- function(samples, count_sample, log_transformed = T){
  #Make a matrix with resamples for each feature.
  sample_mat = sampleEachBetaBinom(samples = samples, count_sample = count_sample)
  
  #Take the average of the log-transformed values, which corresponds to the log-transformed geometric mean.
  geom_mean = apply(X = log(sample_mat), MARGIN = 1, FUN = mean)
  
  if(!log_transformed){geom_mean = exp(geom_mean)}
  
  return(geom_mean)
}

#' samples from the probability density function of a feature in count data for each feature.  
#'
#' @param samples An integer. How many samples should be taken
#' @param count_sample A vector of count data.
#' 
#' @export
#' 
sampleEachBetaBinom <- function(samples, count_sample){
  return(sapply(count_sample, FUN = function(x){sampleBetaBinom(samples = samples, k = x, n = sum(count_sample))}, simplify = T))
}

#' samples from the probability density function of a CLR-transformed count sample.  
#'
#' @param samples An integer. How many samples should be taken
#' @param count_sample A vector of count data.
#' 
#' @export
#' 
sampleCLR <- function(samples, count_sample){
  log_ratios = log(sampleEachBetaBinom(samples = samples, count_sample = count_sample)) - sampleGeomMeam(samples = samples, count_sample = count_sample, log_transformed = T)
  
  return(log_ratios)
}
