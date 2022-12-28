#' Estimate the approximated means from a CLR-transformed count sample.  
#'
#' @description See rbeta.
#' @param count_table A table of count data with rows as features and columns as samples. 
#' @param CLR_transformed A boolean, whether to return centered log-ratio (CLR) transformed values.
#' @param cols_as_features A boolean. Toggles whether rows or columns are samples
#' 
#' @export
#'
getTableMeans <- function(count_table, CLR_transformed = T, cols_as_features = F){
  margin = 2 - cols_as_features

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
  
  if(cols_as_features){table_means = t(table_means)}
  
  return(table_means)
}

#' Estimate the approximated vars from a CLR-transformed count sample.  
#'
#' @description See rbeta.
#' @param count_table A table of count data with rows as features and columns as samples. 
#' @param CLR_transformed A boolean, whether to return centered log-ratio (CLR) values.
#' @param cols_as_features A boolean. Toggles whether rows or columns are samples.
#' 
#' @export
#'
getTableVars <- function(count_table, CLR_transformed = T, cols_as_features = F){
  margin = 2 - cols_as_features
  
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
  
  if(cols_as_features){table_vars = t(table_vars)}
  
  return(table_vars)
}

#' compute CLR using Aitchison's method
#' @description CLR transform a table.
#' @param x A table of compositional data without zeroes.
#' @param cols_as_features A boolean. Toggles whether rows or columns are samples.
#' @return A table of CLR-transformed data
#'
clr <- function(x, cols_as_features = F){
  margin = 2 - cols_as_features
  apply(X = x, MARGIN = margin, FUN = function(y){log(y)-mean(log(y))})
}



#' Give shrunk means from a CLR-transformed count sample.  
#'
#' @description See rbeta.
#' @param count_table A table of count data with rows as features and columns as samples. 
#' @param cols_as_features A boolean. Toggles whether rows or columns are samples.
#' @export
#'
sCLR <- function(count_table, cols_as_features = F){
 
  res_clr = clr(
    apply(X = count_table, 
          MARGIN = 2 - cols_as_features, 
          FUN = getBetaMeans,
          log_transformed = F, 
          simplify = T)/
      apply(X = getTableVars(count_table, CLR_transformed = T, 
                             cols_as_features = cols_as_features),
            MARGIN = 2 - cols_as_features,
            FUN = mean, 
            simplify = T), 
    cols_as_features = F)
  

  if(cols_as_features)(res_clr = t(res_clr))
  
  return(res_clr)
}
