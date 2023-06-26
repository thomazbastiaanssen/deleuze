#' #' Comute the geometric mean of a vector.  
#' #'
#' #' @description Simply shorthand for exp(mean(log(x))).
#' #' @param x a numeric vector that does not contain zeroes. 
#' #' 
#' #' @export
#' #'
#' g_mean <- function(x){exp(mean(log(x)))}
#' 
#' rescale <- function(x){
#'   return(x / apply(x, 1, g_mean))
#' }
#' 
#' #' Undo CLR transformation with softmax
#' #' @description softmax function to transform vector to relative abundance/proportions.
#' #' @param x A clr-transformed vector
#' #' @return A relative abundance/proportions vector.
#' #'
#' softmax <- function(x) exp(x) / sum(exp(x))
#' 
#' perturb <- function(x, perturbation){
#'   return(x * perturbation)
#' }
#' 
#' library(volatility)
#' metadata = vola_metadata[vola_metadata$cohort == "Validation",]
#' data     = getTableMeans(vola_genus_table[,metadata$sample_ID], CLR_transformed = F)
#' 
#' data.exp <- clr(data)
#' 
#' 
#' perturbation = runif(n = nrow(data), min = 0, max = 10)
#' perturbed <- sample(1:76, size = 38, replace = F)
#' data[,perturbed] <- perturb(data[,perturbed], perturbation = perturbation)
#' 
#' data.perturb.exp <- clr(data)
#' metadata$batches <- "batch1"
#' metadata$batches[perturbed] <- "batch2"
#' 
#' vperturb = volatility(data, metadata = metadata$ID)
#' 
#' plot(vbase$volatility, vperturb$volatility)
#' 
#' v1 = data[,metadata$batches == "batch1"]
#' v2 = data[,metadata$batches == "batch2"]
#' 
#' v1 = rescale(v1)
#' v2 = rescale(v2)
#' v = cbind(v1, v2)[,metadata$sample_ID]
#' 
#' v.exp <- clr(v)
#' vrescaled <- volatility(v, metadata$ID)
#' 
#' plot(vbase$volatility, vrescaled$volatility)
#' 
#' plot(vperturb$volatility, vrescaled$volatility)
#' 
#' data
#' 
#' 
#' plot(v.exp[1,], data.exp[1,])
#' plot(v.exp[1,], data.perturb.exp[1,])
#' plot(data.exp[1,], data.perturb.exp[1,])
#' hist(v.exp[3,])
#' 
#' dis_ait1 = dist(t(data.exp), method = "euclidean")
#' dis_ait2 = dist(t(data.perturb.exp), method = "euclidean")
#' dis_ait3 = dist(t(v.exp), method = "euclidean")
#' 
#' hist((dis_ait1 - dis_ait2), breaks = 400)
#' hist((dis_ait2 - dis_ait3), breaks = 400)
#' 
#' hist((dis_ait1 - dis_ait3), breaks = 400)
#' 
#' 
#' vegan::adonis2(dis_ait1~metadata$treatment *metadata$timepoint, strata = metadata$ID)
#' vegan::adonis2(dis_ait2~metadata$treatment *metadata$timepoint, strata = metadata$ID)
#' vegan::adonis2(dis_ait3~metadata$treatment *metadata$timepoint, strata = metadata$ID)
#' 
#' #Use the betadisper function to assess whether the groups have a difference in variance
#' beta_disp = betadisper(dis_ait, group = metadata$Group)
#' 
#' hist(data.perturb.exp[1,])
#' hist(data.exp[1,])
#' hist(v.exp[1,])
