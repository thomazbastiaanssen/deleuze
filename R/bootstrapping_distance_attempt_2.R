set.seed(12345)
library(tidyverse)
library(deleuze)
library(patchwork)
library(Tjazi)
library(vegan)
library(LaplacesDemon)


col_diff = function(X){
  X[,1] - X[,2]
}

bayes_boot_dist <- function(X, n, dir_alpha = 4){
  stopifnot("bayes_boot_dist takes a matrix with exaclty two columns" = ncol(X) == 2)
  
  X = as.matrix(X)
  stopifnot("bayes_boot_dist takes a matrix without zeroes" = !any(X==0))
  
  #Could be sped up with Rcpp:
  X.arr = replicate(n = n, X  * t(rdirichlet(ncol(X), alpha = rep(dir_alpha, nrow(X)))), simplify = "array")
  
  #CLR each iteration
  X.arr = apply(X = X.arr, MARGIN = 3, FUN = deleuze:::clr, simplify = "list")
  
  #Create container vector
  agg_diff <- vector(mode = "numeric", length = nrow(X))
  
  #Log the differences in the vector pair per iteration
  for(i in 1:n){
    agg_diff = agg_diff + col_diff(X.arr[[i]])
  }
  
  #Go from aggregated difference to euclidean distance
  agg_diff = sqrt(sum((agg_diff/n)^2))
  
  return(agg_diff) 
}

bayes_boot_dist_2 <- function(X, n, dir_alpha = 4){
  stopifnot("bayes_boot_diff takes a matrix with exaclty two columns" = ncol(X) == 2)
  
  X = as.matrix(X)
  stopifnot("bayes_boot_diff takes a matrix without zeroes" = !any(X==0))
  
  #Could be sped up with Rcpp:
  X.arr = replicate(n = n, X  * t(rdirichlet(ncol(X), alpha = rep(dir_alpha, nrow(X)))), simplify = "array")
  
  #CLR each iteration
  
  X.arr = apply(X = X.arr, MARGIN = 3, FUN = deleuze:::clr, simplify = "list")
  
  #Sum your list of matrices
  X.arr = Reduce("+", X.arr)/n

  agg_diff = col_diff(X.arr)
  
  #Go from aggregated difference to euclidean distance
  agg_diff = sqrt(sum((agg_diff)^2))
  
  return(agg_diff) 
  
}


boot_dist3 <- function(X, n, dir_alpha, cl = NULL){
  
  X = as.matrix(X)
  stopifnot("bayes_boot_diff takes a matrix without zeroes" = !any(X==0))
  
  #Could be sped up with Rcpp:
  X.arr = replicate(n = n, X  * t(rdirichlet(ncol(X), alpha = rep(dir_alpha, nrow(X)))), simplify = "array")
  
  #CLR each iteration
  X.arr = apply(X = X.arr, MARGIN = 3, FUN = deleuze:::clr, simplify = "list")
  
  #Sum your list of matrices
  X.arr = Reduce("+", X.arr)/n
  
  #Calculate the euclidean distance over the summed CLR-transformed matrix
  out_mat = dist(t(X.arr), method = "euclidean")
  
  return(out_mat)
  
}

boot_dist2 <- function(X, n, dir_alpha = 4, cl = NULL){
  if(!is.data.frame(X)){X = as.data.frame(X)}
  
  #Prepare output matrix
  out_mat <- outer(colnames(X), colnames(X), paste, sep="_")
  
  indx          <- which(lower.tri(out_mat, diag=TRUE))
  compnames     <- out_mat[indx]
  
  out_mat = matrix(NA, nrow = nrow(out_mat), ncol = ncol(out_mat))
  
  out_mat[indx] <- if(requireNamespace("pbapply", quietly = TRUE)) {
    pbapply::pbsapply(X = compnames, FUN = function(x){
      targets = unlist(strsplit(x = x, split = "_" ))
      Xmat = X[,targets]
      bayes_boot_dist_2(X = Xmat, n = n, dir_alpha = dir_alpha)
    }, cl = cl)} else {sapply(X = compnames, FUN = function(x){
      targets = unlist(strsplit(x = x, split = "_" ))
      Xmat = X[,targets]
      bayes_boot_dist_2(X = Xmat, n = n, dir_alpha = dir_alpha)
    })}
  
  dia <- diag(out_mat)
  
  out_mat = as.dist(out_mat, diag = T)
  out_mat = as.matrix(out_mat, upper=TRUE, lower=TRUE)
  diag(out_mat) <- dia
  #out_mat = as.dist(out_mat, diag = T, upper = T)
  
  return(out_mat)
}

boot_dist <- function(X, n, dir_alpha = 4, cl = NULL){
  if(!is.data.frame(X)){X = as.data.frame(X)}
  
  #Prepare output matrix
  out_mat <- outer(colnames(X), colnames(X), paste, sep="_")
  
  indx          <- which(lower.tri(out_mat, diag=TRUE))
  compnames     <- out_mat[indx]
  
  out_mat = matrix(NA, nrow = nrow(out_mat), ncol = ncol(out_mat))
  
  out_mat[indx] <- if(requireNamespace("pbapply", quietly = TRUE)) {
    pbapply::pbsapply(X = compnames, FUN = function(x){
      targets = unlist(strsplit(x = x, split = "_" ))
      Xmat = X[,targets]
      bayes_boot_dist(X = Xmat, n = n, dir_alpha = dir_alpha)
    }, cl = cl)} else {sapply(X = compnames, FUN = function(x){
      targets = unlist(strsplit(x = x, split = "_" ))
      Xmat = X[,targets]
      bayes_boot_dist(X = Xmat, n = n, dir_alpha = dir_alpha)
    })}
  
  dia <- diag(out_mat)
  
  out_mat = as.dist(out_mat, diag = T)
  out_mat = as.matrix(out_mat, upper=TRUE, lower=TRUE)
  diag(out_mat) <- dia
  #out_mat = as.dist(out_mat, diag = T, upper = T)
  
  return(out_mat)
}





b1  <-      c(rep(1, 10), rep(5, 20), rep(15, 20), rep(40, 20), rep(80, 20), rep(100, 5), rep(250, 5) )
b1a <- b1 * c(rep(1, 10), rep(exp(1), 10), rep(1, 10),  rep(1/exp(1), 10), rep(1, 60) ) 

res_b1 = sapply(X = rep(seq(1000,20000, by = 1000), each = 10),FUN = function(x){
  table(factor(sample(paste0("feature_",1:100), 
                      prob = b1, 
                      replace = T, size = x), levels = paste0("feature_",1:100)))
  
})

res_b1a = sapply(X = rep(seq(1000,20000, by = 1000), each = 10),FUN = function(x){
  table(factor(sample(paste0("feature_",1:100), 
                      prob = b1a, 
                      replace = T, size = x), levels = paste0("feature_",1:100)))
  
})


res_b1_prob  <- getTableMeans(res_b1,  CLR_transformed = F)
res_b1a_prob <- getTableMeans(res_b1a, CLR_transformed = F)

data = as.data.frame(cbind(res_b1_prob, res_b1a_prob))

data.arr = replicate(n = 100, as.matrix(data)  * t(rdirichlet(ncol(data), alpha = rep(4, nrow(data)))), simplify = "array")
dim(data.arr)
data.exp <- apply(X = data.arr, MARGIN = 3, FUN = deleuze:::clr, simplify = F)



aa = data.frame(a)

colnames(a)
a = boot_dist(X = data, n = 1000, dir_alpha = 4)
b = boot_dist2(X = data, n = 1000, dir_alpha = 4)




dep <- rep(rep(seq(1000,20000, by = 1000), each = 10), 2)
names(dep) <- as.character(1:400)          

str(aa)
saveRDS(a, file = "/home/thomaz/Desktop/a_big_perm.rds")
b = readRDS("/home/thomaz/Desktop/a_big_perm_df.rds")
long_dist = aa %>% 
  rownames_to_column("ID") %>% 
  pivot_longer(!ID) %>% 
  
  filter(!str_detect(ID,"\\.")) %>% 
  mutate(ID = str_remove(ID, "X")) %>% 
  
  mutate(name = str_remove(name, "\\.")) %>% 
  mutate(name = str_remove(name, "X")) %>% 
  
  filter(as.numeric(ID)   <= 200) %>% 
  filter(as.numeric(name) >= 201) %>% 
  
  mutate(ID   = dep[ID]) %>% 
  mutate(name = dep[name]) %>% 
  group_by(ID,name) %>% 
  summarise(mean = round(mean(value) - sqrt(20), 1),
            var  = round(var(value), 2)) %>% 
  ungroup()


long_dist %>% 
  ggplot() +
  aes(x = ID, y = name, fill = mean, label = mean) +
  
  geom_tile() +
  geom_text(colour = "black", size = 2.5) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limits = c(-1,3), "Delta from true mean") +
  theme_bw() +
  ggtitle("New method", subtitle = "10% Rare features") +
  xlab("Sampling depth of first sample") +
  ylab("Sampling depth of second sample")


long_dist %>% 
  ggplot() +
  aes(x = ID, y = name, fill = var, label = var) +
  
  geom_tile() +
  geom_text(colour = "black", size = 2.5) +
  scale_fill_gradient(low = "white", high = "blue", limits = c(0,1.5), "Variance") +
  theme_bw() +
  ggtitle("New method", subtitle = "10% Rare features") +
  xlab("Sampling depth of first sample") +
  ylab("Sampling depth of second sample")