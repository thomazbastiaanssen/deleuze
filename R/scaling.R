#' Compute the geometric mean of a vector.
#'
#' @description Simply shorthand for exp(mean(log(x))).
#' @param x a numeric vector that does not contain zeroes.
#'
#' @export
#'
g_mean <- function(x){exp(mean(log(x)))}

#' Center by the geometric mean.
#'
#' @description Simply divide each value by its corresponding geometric mean.
#' @param x a numeric table that does not contain zeroes.
#'
#' @export
#'
g_center_by <- function(x, MARGIN = MARGIN){
  return(x / apply(X = x, MARGIN = MARGIN, FUN = mean))
}

#' Undo CLR transformation with softmax
#' @description softmax function to transform vector to relative abundance/proportions.
#' @param x A clr-transformed vector
#' @return A relative abundance/proportions vector.
#'
softmax <- function(x) exp(x) / sum(exp(x))




#' Perturb vector 
#' @noRd
#'
perturb <- function(x, perturbation){
  return(x * perturbation)
}

#' Center by margin
#' @param x a table
#' @param MARGIN see apply
#' 
g_center_by <- function(x, MARGIN = MARGIN){
  return(x / apply(X = x, MARGIN = MARGIN, FUN = mean))
}


#' Double center a table.
#'
#' @description To center: Divide each value by its corresponding row and column geometric mean.
#' @param x a numeric table.
#' @param log.transform A boolean. Whether to return log-transformed values. 
#'
#' @export
#'
double_center <- function(x, log.transform = FALSE){
  x <- impute_and_scale(x)
  x <- log(x)
  x <- sweep(x, MARGIN = 1, rowMeans(x), `-`, check.margin = TRUE)
  x <- sweep(x, MARGIN = 2, colMeans(x), `-`, check.margin = TRUE)
  
  if(!log.transform) {return(exp(x))}
  
  return(x)
}


#'@noRd
#'
impute_and_scale <- function(x){
  getTableMeans(x, CLR_transformed = FALSE)
}