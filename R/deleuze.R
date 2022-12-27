#' Estimate the approximated means from a CLR-transformed count sample using.  
#'
#' @description See rbeta.
#' @param count_table A table of count data with rows as features and columns as samples. 
#' @param CLR_transformed A boolean, whether to return centered log-ratio (CLR) transformed values.
#' @param rows_as_features A boolean. Toggles whether rows or columns are samples
#' 
#' @export
#'
getTableMeans <- function(count_table, CLR_transformed = T, rows_as_features = F){
  margin = 2 - rows_as_features

  if(CLR_transformed){
    table_means = apply(X = count_table, 
                        MARGIN = margin, 
                        FUN = getCLRMeans)
    }
  if(!CLR_transformed){
    table_means = apply(X = count_table, 
                        MARGIN = margin, 
                        FUN = getBetaMeans,
                        log_transformed = F)
  }
  
  if(rows_as_features){table_means = t(table_means)}
  
  return(table_means)
}

#' Estimate the approximated vars from a CLR-transformed count sample using.  
#'
#' @description See rbeta.
#' @param count_table A table of count data with rows as features and columns as samples. 
#' @param CLR_transformed A boolean, whether to return centered log-ratio (CLR) values.
#' @param rows_as_features A boolean. Toggles whether rows or columns are samples.
#' 
#' @export
#'
getTableVars <- function(count_table, CLR_transformed = T, rows_as_features = F){
  margin = 2 - rows_as_features
  
  if(CLR_transformed){
    table_vars = apply(X = count_table, 
                       MARGIN = margin, 
                       FUN = getCLRVars)
  }
  if(!CLR_transformed){
    table_vars = apply(X = count_table, 
                       MARGIN = margin, 
                       FUN = getBetaVars, 
                       log_transformed = F)
  }
  
  if(rows_as_features){table_vars = t(table_vars)}
  
  return(table_vars)
}
