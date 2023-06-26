library(deleuze)
library(LaplacesDemon)
library(tidyverse)

set.seed(12345)
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

data = cbind(res_b1_prob, res_b1a_prob)

data.arr = replicate(n = 100, data  * t(rdirichlet(ncol(data), alpha = rep(4, nrow(data)))))

data.exp <- apply(X = data.arr, MARGIN = 3, FUN = deleuze:::clr, simplify = F)



pw_diff <- function(x){
  x = as.data.frame(x)
  
  unlist(outer(1:ncol(x), 1:ncol(x), 
               function(y,z) x[,y]-x[,z]))
}

apply_pw_diff <- function(x){
  res <- matrix(0, 
                nrow = nrow(x[[1]]), ncol = ncol(x[[1]])^2, 
                dimnames = list(rownames(x[[1]]), 
                                c(
                                  outer(
                                    colnames(as.data.frame(x[[1]])), 
                                    colnames(as.data.frame(x[[1]])), paste, sep="_"))
                                
                )
  )
  
  for(i in 1:length(x)){
    res <- res + pw_diff(x[[i]])
    
    print(paste("Finished iteration", i))
  }
  return(res/length(x))
}



meandiff <- apply_pw_diff(data.exp[1:5])


meansq   <- meandiff^2

meandist <- sqrt(colSums(meansq))



meandistmat <- meandist %>% matrix(data = ., 
                                   nrow = nrow(outer(
                                     colnames(as.data.frame(data)), 
                                     colnames(as.data.frame(data)), 
                                     paste, sep="_"))
) %>% data.frame()

dep <- rep(rep(seq(1000,20000, by = 1000), each = 10), 2)
names(dep) <- as.character(1:400)          



long_dist = meandistmat %>% 
  rownames_to_column("ID") %>% 
  pivot_longer(!ID) %>% 
  
  filter(!str_detect(ID,"\\.")) %>% 
  mutate(ID = str_remove(ID, "X")) %>% 
  
  #filter(str_detect(name,"\\.")) %>% 
  mutate(name = str_remove(name, "\\..")) %>% 
  mutate(name = str_remove(name, "X")) %>% 
  # filter(!is.na(ID)) %>% 
  # filter(!is.na(name)) %>% 
  # 

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
  scale_fill_gradient(low = "white", high = "blue", limits = c(0,3), "Variance") +
  theme_bw() +
  ggtitle("New method", subtitle = "10% Rare features") +
  xlab("Sampling depth of first sample") +
  ylab("Sampling depth of second sample")



meandistmat %>% 
  rownames_to_column("ID") %>% 
  pivot_longer(!ID) %>% 
  
  filter(!str_detect(ID,"\\.")) %>% 
  mutate(ID = str_remove(ID, "X")) %>% 
  
  #filter(str_detect(name,"\\.")) %>% 
  mutate(name = str_remove(name, "\\..")) %>% 
  mutate(name = str_remove(name, "X")) %>% 
  # filter(!is.na(ID)) %>% 
  # filter(!is.na(name)) %>% 
  # 
  
  filter(ID != name) %>% 
  mutate(ID   = as.numeric(ID), 
         name = as.numeric(name)) %>% 
  # mutate(ID   = dep[ID]) %>% 
  # mutate(name = dep[name]) %>% 
  # 
  ggplot() +
  aes(x = ID, y = name, colour = value - sqrt(20)) +
  
  geom_point(alpha = 1/10) +
  scale_colour_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limits = c(-3,3), "Delta from true mean") +
  theme_bw()
  

  
  
