# library(deleuze)
# library(LaplacesDemon)
# library(tidyverse)
# 
# set.seed(12345)
# b1  <-      c(rep(1, 10), rep(5, 20), rep(15, 20), rep(40, 20), rep(80, 20), rep(100, 5), rep(250, 5) )
# b1a <- b1 * c(rep(1, 10), rep(exp(1), 10), rep(1, 10),  rep(1/exp(1), 10), rep(1, 60) ) 
# 
# res_b1 = sapply(X = rep(seq(1000,20000, by = 1000), each = 10),FUN = function(x){
#   table(factor(sample(paste0("feature_",1:100), 
#                       prob = b1, 
#                       replace = T, size = x), levels = paste0("feature_",1:100)))
#   
# })
# 
# res_b1a = sapply(X = rep(seq(1000,20000, by = 1000), each = 10),FUN = function(x){
#   table(factor(sample(paste0("feature_",1:100), 
#                       prob = b1a, 
#                       replace = T, size = x), levels = paste0("feature_",1:100)))
#   
# })
# 
# 
# res_b1_prob  <- getTableMeans(res_b1,  CLR_transformed = F)
# res_b1a_prob <- getTableMeans(res_b1a, CLR_transformed = F)
# 
# data = cbind(res_b1_prob, res_b1a_prob)
# 
# data.arr = replicate(n = 100, data  * t(rdirichlet(ncol(data), alpha = rep(4, nrow(data)))))
# 
# data.exp <- apply(X = data.arr, MARGIN = 3, FUN = deleuze:::clr, simplify = F)
# 
# 
# 
# pw_diff <- function(x){
#   x = as.data.frame(x)
#   
#   unlist(outer(1:ncol(x), 1:ncol(x), 
#                function(y,z) x[,y]-x[,z]))
# }
# 
# apply_pw_diff <- function(x){
#   res <- matrix(0, 
#                 nrow = nrow(x[[1]]), ncol = ncol(x[[1]])^2, 
#                 dimnames = list(rownames(x[[1]]), 
#                                 c(
#                                   outer(
#                                     colnames(as.data.frame(x[[1]])), 
#                                     colnames(as.data.frame(x[[1]])), paste, sep="_"))
#                                 
#                 )
#   )
#   
#   for(i in 1:length(x)){
#     res <- res + pw_diff(x[[i]])
#     
#     print(paste("Finished iteration", i))
#   }
#   return(res/length(x))
# }
# 
# # diff.ls <- lapply(data.exp[1:50], FUN = pw_diff)
# # 
# # dist.arr <- simplify2array(diff.ls, higher = T) 
# # 
# # dist.arr[,,1]
# # meandist = apply(dist.arr, MARGIN = c(1, 2), FUN = mean) %>% 
# #   as.data.frame()
# 
# meandiff <- apply_pw_diff(data.exp[1:5])
# dim(meandiff)
# meansq   <- meandiff^2
# 
# meandist <- sqrt(colSums(meansq))
# 
# dep <- rep(seq(1000,20000, by = 1000), each = 10)
# names(dep) <- as.character(1:200)          
# 
# 
# long_dist = meandist %>% 
#   as.data.frame() %>% 
#   rownames_to_column("ID") %>% 
#   separate() %>% 
#   
#   filter(!str_detect(ID,"\\.")) %>% 
#   mutate(ID = str_remove(ID, "X")) %>% 
#   mutate(name = str_remove(name, "V")) %>% 
#   
#   mutate(ID   = dep[ID]) %>% 
#   mutate(name = dep[name]) %>% 
#   group_by(ID,name) %>% 
#   summarise(mean = round(mean(value) - sqrt(20), 2),
#             var  = round(var(value), 2)) %>% 
#   ungroup()
# 
# 
# long_dist = meandist %>% 
#   as.data.frame() %>% 
#   rownames_to_column("ID") %>% 
#   separate(ID, ) %>% 
#   
#   filter(!str_detect(ID,"\\.")) %>% 
#   mutate(ID = str_remove(ID, "X")) %>% 
#   mutate(name = str_remove(name, "V")) %>% 
#   
#   mutate(ID   = dep[ID]) %>% 
#   mutate(name = dep[name]) %>% 
#   group_by(ID,name) %>% 
#   summarise(mean = round(mean(value) - sqrt(20), 2),
#             var  = round(var(value), 2)) %>% 
#   ungroup()
# 
# long_dist %>% 
#   ggplot() +
#   aes(x = ID, y = name, fill = mean, label = mean) +
#   
#   geom_tile() +
#   geom_text(colour = "black", size = 2.5) +
#   scale_fill_gradient(low = "white", high = "red", limits = c(0,8.5), "Error from true mean") +
#   theme_bw() +
#   ggtitle("Constant replacement", subtitle = "10% Rare features") +
#   xlab("Sampling depth of first sample") +
#   ylab("Sampling depth of second sample")
# 
# 
# a.pca = cbind(a.exp %>% cbind("real" = deleuze:::clr(data.frame(b1 ))[,1]), 
#               b.exp %>% cbind("real" = deleuze:::clr(data.frame(b1a))[,1])) %>% 
#   t() %>%
#   prcomp()
# 
# #Extract the amount of variance the first four components explain for plotting. 
# pc1 <- round(const_b1a.pca$sdev[1]^2/sum(const_b1a.pca$sdev^2),4) * 100
# pc2 <- round(const_b1a.pca$sdev[2]^2/sum(const_b1a.pca$sdev^2),4) * 100
# pc3 <- round(const_b1a.pca$sdev[3]^2/sum(const_b1a.pca$sdev^2),4) * 100
# pc4 <- round(const_b1a.pca$sdev[4]^2/sum(const_b1a.pca$sdev^2),4) * 100
# 
# #Extract the scores for every sample for the first four components for plotting. 
# pca  = data.frame(PC1 = const_b1a.pca$x[,1], 
#                   PC2 = const_b1a.pca$x[,2], 
#                   PC3 = const_b1a.pca$x[,3], 
#                   PC4 = const_b1a.pca$x[,4])
# 
# pca$samples = factor(x = c(rep(seq(1000,20000, by = 1000), each = 100), "real"), 
#                      levels = unique(c(seq(1000,20000, by = 1000), "real")))
# 
# pca$source = rep(c("A","B"), each = 2001)
# 
# const_b1a = ggplot(pca) +  
#   
#   aes(x = PC1, y = PC2, fill = samples, group = interaction(samples,source)) +
#   
#   #Create the points and ellipses
#   stat_ellipse(geom = "polygon", alpha = 1/4) +
#   geom_point(col = "black", shape = 21, 
#              aes(size = samples == "real")) + 
#   #Adjust appearance
#   
#   #Adjust labels
#   ggtitle("Constant replacement", subtitle = "10% Rare features") + 
#   xlab(paste("PC1: ", pc1,  "%", sep="")) + 
#   ylab(paste("PC2: ", pc2,  "%", sep="")) +
#   theme_bw() +
#   theme(legend.position = 'bottom') 
