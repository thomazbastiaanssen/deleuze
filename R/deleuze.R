#' Estimate the approximated means from a CLR-transformed count sample using.  
#'
#' @description See rbeta.
#' @param count_table A table of count data with rows as features and columns as samples. 
#' @param log_transformed A boolean, whether to return log-transformed values.
#' @param rows_as_features A boolean. Toggles whether rows or columns are samples
#' 
#' @export
#'
getTableMeans <- function(count_table, log_transformed = T, rows_as_features = F){
  margin = 2 - rows_as_features
  table_means = apply(X = count_table, MARGIN = margin, FUN = getCLRMeans)
  
  if(rows_as_features){table_means = t(table_means)}
  
  return(table_means)
}

#' Estimate the approximated vars from a CLR-transformed count sample using.  
#'
#' @description See rbeta.
#' @param count_table A table of count data with rows as features and columns as samples. 
#' @param log_transformed A boolean, whether to return log-transformed values.
#' @param rows_as_features A boolean. Toggles whether rows or columns are samples.
#' 
#' @export
#'
getTableVars <- function(count_table, log_transformed = T, rows_as_features = F){
  margin = 2 - rows_as_features
  table_vars = apply(X = count_table, MARGIN = margin, FUN = getCLRVars)
  
  if(rows_as_features){table_vars = t(table_vars)}
  
  return(table_vars)
}