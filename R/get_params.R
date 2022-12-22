#' Calculate by feature variance from a beta distributions
#'
#' @description See rbeta.
#' @param count_sample
#' 
#' @export
#'
getBetaVars <- function(count_sample, log_transformed = F){
  
  if(!log_transformed){
    return(sapply(count_sample, 
                  FUN = function(x){getBetaVar(k = x, n = sum(count_sample))}, simplify = T))
  }
  
  if(log_transformed){
    return(sapply(count_sample, 
                  FUN = function(x){getLogBetaVar(k = x, n = sum(count_sample))}, simplify = T))
  }
  
  }


#' Calculate the variance from a beta distribution. 
#'
#' @description See rbeta. Called by getBetaVars. 
#' @param k An integer. How many counts were observerd
#' @param n An integer. How many obserations were made
#' 
#' @export
#'
getBetaVar <- function(k, n){
  a = k + 1
  b = n - k + 1
  return(a*b/((a+b*b)*(a + b + 1)))
}


#' Calculate the log variance from a beta distribution. 
#'
#' @description See rbeta. Called by getBetaVars. 
#' @param k An integer. How many counts were observerd
#' @param n An integer. How many obserations were made
#' 
#' @export
#'
getLogBetaVar <- function(k, n){
  a = k + 1
  b = n - k + 1
  return(trigamma(a) - trigamma(a + b))
}


#' Calculate by feature mean from a beta distributions
#'
#' @description See rbeta.
#' @param count_sample
#' 
#' @export
#'
getBetaMeans <- function(count_sample, log_transformed = F){
  
  if(!log_transformed){
    return(sapply(count_sample, FUN = function(x){getBetaMean(k = x, n = sum(count_sample))}, simplify = T))
    }
  if(log_transformed){
    return(sapply(count_sample, FUN = function(x){getLogBetaMean(k = x, n = sum(count_sample))}, simplify = T))
  }
  
}


#' Calculate the mean from a beta distribution. 
#'
#' @description See rbeta. Called by getBetaMeans. 
#' @param k An integer. How many counts were observerd
#' @param n An integer. How many obserations were made
#' 
#' @export
#'
getBetaMean <- function(k, n){
  a = k + 1
  b = n - k + 1
  return(a/(a+b))
}



#' Calculate the log mean from a beta distribution. 
#'
#' @description See rbeta. Called by getBetaMeans. 
#' @param k An integer. How many counts were observerd
#' @param n An integer. How many obserations were made
#' 
#' @export
#'
getLogBetaMean <- function(k, n){
  a = k + 1
  b = n - k + 1
  return(digamma(a) - digamma(a + b))
}

