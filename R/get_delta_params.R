#' Calculate by feature-pair mean from a CLR transformed approximation
#'
#' @description See rbeta.
#' @param x A vector of count data.
#' @param y A vector of count data.
#' 
#' @export
#'
getDeltaCLRMeans <- function(x, y){
  
  getCLRMeans(x) - getCLRMeans(y)
  
}

#' Calculate by feature-pair variance from a CLR transformed approximation
#'
#' @description See rbeta.
#' @param x A vector of count data.
#' @param y A vector of count data.
#' 
#' @export
#'
getDeltaCLRVars <- function(x, y){
  
  getCLRVars(x) + getCLRVars(y)
  
}


#' Calculate expected sum of squared differences from a CLR transformed approximation.
#'
#' @description See rbeta.
#' @param x A vector of count data.
#' @param y A vector of count data.
#' 
#' @export
#'
getGenChiSqMean <- function(x, y){
  #Assumes DeltaCLRMeans is normal. 
  
  #Mean = k + lambda; 
  #For a squared normal distribution
  #k = 1 and lambda = DeltaCLRMeans^2. 
  
  sum(1 + getDeltaCLRVars(x, y) + getDeltaCLRMeans(x, y)^2)
  
}



#' Calculate variance of the expected sum of squared differences from a CLR transformed approximation.
#'
#' @description See rbeta.
#' @param x A vector of count data.
#' @param y A vector of count data.
#' 
#' @export
#'
getGenChiSqVar <- function(x, y){
  #Assumes DeltaCLRMeans is normal. 
  
  #Mean = k + lambda; 
  #For a squared normal distribution
  #k = 1 and lambda = DeltaCLRMeans^2. 
  
  2 * sum(1 + 2 * (getDeltaCLRVars(x, y) + (getDeltaCLRMeans(x, y)^2)))
}

#' 
#' #' Calculate expected distance between two count data vectors.
#' #'
#' #' @description See rbeta.
#' #' @param x A vector of count data.
#' #' @param y A vector of count data.
#' #' 
#' #' @export
#' #'
#' getMeanDist <- function(x, y){
#'  sqrt(getGenChiSqMean(x = x, y = y))
#' }
#' 
#' 
#' 
#' #' Calculate variance over expected distance between two count data vectors.
#' #'
#' #' @description See rbeta.
#' #' @param x A vector of count data.
#' #' @param y A vector of count data.
#' #' 
#' #' @export
#' #'
#' getMeanDistVar <- function(x, y){
#' 
#' #Expected var(sqrt(X)) = X - sqrt(X)^2 
#' 
#'   getGenChiSqMean(x = x, y = y)
#'   
#'   }
#' 
