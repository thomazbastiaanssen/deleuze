``` r
set.seed(12345)
library(tidyverse)
library(deleuze)
library(patchwork)
library(Tjazi)
library(vegan)
library(LaplacesDemon)


#Estimate distance
boot_dist <- function(X, n, dir_alpha, cl = NULL){
  
  X = as.matrix(X)
  stopifnot("bayes_boot_diff takes a matrix without zeroes" = !any(X==0))
  
  #Could be sped up with Rcpp:
  X.arr = replicate(n = n, X  * t(rdirichlet(ncol(X), alpha = rep(dir_alpha, nrow(X)))), simplify = "array")
  
  #CLR each iteration
  X.arr = apply(X = X.arr, MARGIN = 3, FUN = deleuze:::clr, simplify = "list")
  
  #Sum your list of matrices
  X.arr = Reduce("+", X.arr)/n
  
  #Calculate the euclidean distance over the summed CLR-transformed matrix
  out_mat = dist(t(X.arr), method = "euclidean", diag = T, upper = T)
  
  return(out_mat)
  
}

#Perturb a clr-transformed sample by a given distance
perturb_by <- function(x, by, prop = 1){
  
  #Which features to alter
  target = sample(1:length(x), max(2, round(length(x)*prop), digits = 0))
  
  #Generate container fro perturnbation
  res_perturb = vector(mode = "numeric", length = length(target))
  
  #Decide which features are going to be positive and negative
  division <- c(T, F, sample(c(T, F), size = length(target)-2, replace = T))
  
  #Distribute half of the squared distance to the positive division
  res_perturb[ division] =  sqrt(((by^2)/2)*(rdirichlet(n = 1, alpha = rep(1, sum(division)))))
  
  #Distribute half of the squared distance to the negative division
  res_perturb[!division] = -sqrt(((by^2)/2)*(rdirichlet(n = 1, alpha = rep(1, sum(!division)))))
  
  #Perturb the input
  x[target] = x[target] + res_perturb
  
  return(x)
}
```

``` r
b1  <-      c(rep(1, 10), rep(5, 20), rep(15, 20), rep(40, 20), rep(80, 20), rep(100, 5), rep(250, 5) )
b1a <-      b1 %>% {log(.) - mean(log(.))} %>% perturb_by(., by = 10) %>% {exp(.) / sum(exp(.))}
b1b <-      b1 %>% {log(.) - mean(log(.))} %>% perturb_by(., by = 20) %>% {exp(.) / sum(exp(.))}
b1c <-      b1 %>% {log(.) - mean(log(.))} %>% perturb_by(., by = 30) %>% {exp(.) / sum(exp(.))}
b1d <-      b1 %>% {log(.) - mean(log(.))} %>% perturb_by(., by = 40) %>% {exp(.) / sum(exp(.))}
b1e <-      b1 %>% {log(.) - mean(log(.))} %>% perturb_by(., by = 50) %>% {exp(.) / sum(exp(.))}

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

res_b1b = sapply(X = rep(seq(1000,20000, by = 1000), each = 10),FUN = function(x){
  table(factor(sample(paste0("feature_",1:100), 
                      prob = b1a, 
                      replace = T, size = x), levels = paste0("feature_",1:100)))
  
})

res_b1c = sapply(X = rep(seq(1000,20000, by = 1000), each = 10),FUN = function(x){
  table(factor(sample(paste0("feature_",1:100), 
                      prob = b1a, 
                      replace = T, size = x), levels = paste0("feature_",1:100)))
  
})

res_b1d = sapply(X = rep(seq(1000,20000, by = 1000), each = 10),FUN = function(x){
  table(factor(sample(paste0("feature_",1:100), 
                      prob = b1a, 
                      replace = T, size = x), levels = paste0("feature_",1:100)))
  
})

res_b1e = sapply(X = rep(seq(1000,20000, by = 1000), each = 10),FUN = function(x){
  table(factor(sample(paste0("feature_",1:100), 
                      prob = b1a, 
                      replace = T, size = x), levels = paste0("feature_",1:100)))
  
})


res_b1_prob  <- getTableMeans(res_b1,  CLR_transformed = F)
res_b1a_prob <- getTableMeans(res_b1a, CLR_transformed = F)
res_b1b_prob <- getTableMeans(res_b1b, CLR_transformed = F)
res_b1c_prob <- getTableMeans(res_b1c, CLR_transformed = F)
res_b1d_prob <- getTableMeans(res_b1d, CLR_transformed = F)
res_b1e_prob <- getTableMeans(res_b1e, CLR_transformed = F)

data = as.data.frame(cbind(res_b1_prob, res_b1a_prob, res_b1b_prob, res_b1c_prob, res_b1d_prob, res_b1e_prob))
```

``` r
est.dist <- boot_dist(X = data, n = 1000, dir_alpha = 4)
```

``` r
dep <- rep(rep(seq(1000,20000, by = 1000), each = 10), 6)
names(dep) <- as.character(1:ncol(data))      

long_dist1 = data.frame(as.matrix(est.dist)) %>% 
  rownames_to_column("ID") %>% 
  pivot_longer(!ID) %>% 
  
  filter(!str_detect(ID,"\\.")) %>% 
  mutate(ID = str_remove(ID, "X")) %>% 
  mutate(ID = str_remove(ID, "V")) %>% 
  
  
  mutate(name = str_remove(name, "\\.")) %>% 
  mutate(name = str_remove(name, "X")) %>% 
  mutate(name = str_remove(name, "V")) %>% 
  
  filter(as.numeric(ID)   < 201) %>% 
  filter(as.numeric(name) < 201) %>% 
  filter(name != ID) %>% 
  
  mutate(ID   = dep[ID]) %>% 
  mutate(name = dep[name]) %>% 
  group_by(ID,name) %>% 
  summarise(mean = mean(value),
            var  = var(value)) %>%
  ungroup()
```

    ## `summarise()` has grouped output by 'ID'. You can override using the `.groups`
    ## argument.

``` r
plot_dist1 <- long_dist1 %>% 
  ggplot() +
  aes(x = ID, y = name, fill = mean, label = round(mean, 1)) +
  
  geom_tile() +
  geom_text(colour = "black", size = 2.5) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limits = c(0,7), "Delta from true mean") +
  theme_bw() +
  ggtitle("Within sample offset 1", subtitle = "10% Rare features") +
  xlab("Sampling depth of first sample") +
  ylab("Sampling depth of second sample")

p_geom_mean <- data.frame(as.matrix(est.dist)) %>% 
  rownames_to_column("ID") %>% 
  pivot_longer(!ID) %>% 
  
  filter(!str_detect(ID,"\\.")) %>% 
  mutate(ID = str_remove(ID, "X")) %>% 
  mutate(ID = str_remove(ID, "V")) %>% 
  
  
  mutate(name = str_remove(name, "\\.")) %>% 
  mutate(name = str_remove(name, "X")) %>% 
  mutate(name = str_remove(name, "V")) %>% 
  
  filter(as.numeric(ID)   <= 201 & as.numeric(ID)   < 401) %>% 
  filter(as.numeric(name) >  201 & as.numeric(name) < 401) %>% 
  filter(name != ID) %>% 
  
  mutate(ID   = dep[ID]) %>% 
  mutate(name = dep[name]) %>% 
  group_by(ID,name) %>% 
  summarise(mean = mean(value),
            var  = var(value)) %>% 
  ungroup() %>% 
  mutate(mean_s = long_dist1$mean) %>% 
  mutate(mean_offset = sqrt((mean *  mean_s)) ) %>% 
  mutate(mean_from_10 = mean_offset - 10) %>% 
  
  ggplot() +
  aes(x = ID, y = name, fill = mean_from_10, label = round(mean_from_10, 1)) +
  
  geom_tile() +
  geom_text(colour = "black", size = 2.5) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limits = c(-3,3), "Error from ground truth") +
  theme_bw() +
  ggtitle("Error from true distance - normalised by geom_mean", subtitle = "10% Rare features") +
  xlab("Sampling depth of first sample") +
  ylab("Sampling depth of second sample")
```

    ## `summarise()` has grouped output by 'ID'. You can override using the `.groups`
    ## argument.

``` r
p_arith_mean <- data.frame(as.matrix(est.dist)) %>% 
  rownames_to_column("ID") %>% 
  pivot_longer(!ID) %>% 
  
  filter(!str_detect(ID,"\\.")) %>% 
  mutate(ID = str_remove(ID, "X")) %>% 
  mutate(ID = str_remove(ID, "V")) %>% 
  
  
  mutate(name = str_remove(name, "\\.")) %>% 
  mutate(name = str_remove(name, "X")) %>% 
  mutate(name = str_remove(name, "V")) %>% 
  
  filter(as.numeric(ID)   <= 201 & as.numeric(ID)   < 401) %>% 
  filter(as.numeric(name) >  201 & as.numeric(name) < 401) %>% 
  
  filter(name != ID) %>% 
  
  mutate(ID   = dep[ID]) %>% 
  mutate(name = dep[name]) %>% 
  group_by(ID,name) %>% 
  summarise(mean = mean(value),
            var  = var(value)) %>% 
  ungroup() %>% 
  mutate(mean_s = long_dist1$mean) %>% 
  mutate(mean_offset = (mean +  mean_s)/2 ) %>% 
  mutate(mean_from_10 = mean_offset - 10) %>% 
  
  ggplot() +
  aes(x = ID, y = name, fill = mean_from_10, label = round(mean_from_10, 1)) +
  
  geom_tile() +
  geom_text(colour = "black", size = 2.5) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limits = c(-3,3), "Error from ground truth") +
  theme_bw() +
  ggtitle("Error from true distance - normalised by arith_mean", subtitle = "10% Rare features") +
  xlab("Sampling depth of first sample") +
  ylab("Sampling depth of second sample")
```

    ## `summarise()` has grouped output by 'ID'. You can override using the `.groups`
    ## argument.

``` r
data.frame(as.matrix(est.dist)) %>% 
  rownames_to_column("ID") %>% 
  pivot_longer(!ID) %>% 
  
  filter(!str_detect(ID,"\\.")) %>% 
  mutate(ID = str_remove(ID, "X")) %>% 
  mutate(ID = str_remove(ID, "V")) %>% 
  
  
  mutate(name = str_remove(name, "\\.")) %>% 
  mutate(name = str_remove(name, "X")) %>% 
  mutate(name = str_remove(name, "V")) %>% 
  
  filter(as.numeric(ID)   <= 201 & as.numeric(ID)   < 401) %>% 
  filter(as.numeric(name) >  201 & as.numeric(name) < 401) %>% 
  filter(name != ID) %>% 
  
  mutate(ID   = dep[ID]) %>% 
  mutate(name = dep[name]) %>% 
  group_by(ID,name) %>% 
  summarise(mean = mean(value),
            var  = var(value)) %>% 
  ungroup() %>% 
  ggplot() +
  aes(x = ID, y = name, fill = mean, label = round(mean, 1)) +
  
  geom_tile() +
  geom_text(colour = "black", size = 2.5) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limits = c(0,8), "Estimated distance") +
  theme_bw() +
  ggtitle("Estimated distance between Sample 1 and Sample 2", subtitle = "10% Rare features") +
  xlab("Sampling depth of first sample") +
  ylab("Sampling depth of second sample")
```

    ## `summarise()` has grouped output by 'ID'. You can override using the `.groups`
    ## argument.

![](bootstrapping_distance_files/figure-gfm/prepare%20plots-1.png)<!-- -->

``` r
data.frame(as.matrix(est.dist)) %>% 
  rownames_to_column("ID") %>% 
  pivot_longer(!ID) %>% 
  
  filter(!str_detect(ID,"\\.")) %>% 
  mutate(ID = str_remove(ID, "X")) %>% 
  mutate(ID = str_remove(ID, "V")) %>% 
  
  
  mutate(name = str_remove(name, "\\.")) %>% 
  mutate(name = str_remove(name, "X")) %>% 
  mutate(name = str_remove(name, "V")) %>% 
  
  filter(as.numeric(ID)   <= 201 & as.numeric(ID)   < 401) %>% 
  filter(as.numeric(name) >  201 & as.numeric(name) < 401) %>% 
  filter(name != ID) %>% 
  
  mutate(ID   = dep[ID]) %>% 
  mutate(name = dep[name]) %>% 
  group_by(ID,name) %>% 
  summarise(mean = mean(value),
            var  = var(value)) %>% 
  ungroup() %>% 
  mutate(mean_s = long_dist1$mean) %>% 
  #mutate(mean_offset = (mean +  mean_s)/2 ) %>% 
  #mutate(mean_from_10 = mean_offset - 10) %>% 
  
  ggplot() +
  aes(x = ID, y = name, fill = mean, label = round(mean, 1)) +
  
  geom_tile() +
  geom_text(colour = "black", size = 2.5) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limits = c(0,8), "Estimated distance") +
  theme_bw() +
  ggtitle("Estimated distance between Sample 1 and Sample 2", subtitle = "10% Rare features") +
  xlab("Sampling depth of first sample") +
  ylab("Sampling depth of second sample")
```

    ## `summarise()` has grouped output by 'ID'. You can override using the `.groups`
    ## argument.

![](bootstrapping_distance_files/figure-gfm/prepare%20plots-2.png)<!-- -->

``` r
data.frame(as.matrix(est.dist)) %>% 
  rownames_to_column("ID") %>% 
  pivot_longer(!ID) %>% 
  
  filter(!str_detect(ID,"\\.")) %>% 
  mutate(ID = str_remove(ID, "X")) %>% 
  mutate(ID = str_remove(ID, "V")) %>% 
  
  
  mutate(name = str_remove(name, "\\.")) %>% 
  mutate(name = str_remove(name, "X")) %>% 
  mutate(name = str_remove(name, "V")) %>% 
  
  filter(as.numeric(ID)   <= 201 & as.numeric(ID)   < 401) %>% 
  filter(as.numeric(name) >  201 & as.numeric(name) < 401) %>% 
  filter(name != ID) %>% 
  
  mutate(ID   = dep[ID]) %>% 
  mutate(name = dep[name]) %>% 
  group_by(ID,name) %>% 
  summarise(mean = mean(value),
            var  = var(value)) %>% 
  ungroup() %>% 
  mutate(mean_s = long_dist1$mean) %>% 
  #mutate(mean_offset = sqrt((mean *  mean_s)) ) %>% 
  mutate(mean_from_10 = mean - 10) %>% 
  
  ggplot() +
  aes(x = ID, y = name, fill = mean_from_10, label = round(mean_from_10, 1)) +
  
  geom_tile() +
  geom_text(colour = "black", size = 2.5) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limits = c(-3,3), "Error from ground truth") +
  theme_bw() +
  ggtitle("Error from true distance", subtitle = "10% Rare features") +
  xlab("Sampling depth of first sample") +
  ylab("Sampling depth of second sample")
```

    ## `summarise()` has grouped output by 'ID'. You can override using the `.groups`
    ## argument.

![](bootstrapping_distance_files/figure-gfm/prepare%20plots-3.png)<!-- -->

``` r
#Let's see if we can find out where this bias comes from.
plot_dist1
```

![](bootstrapping_distance_files/figure-gfm/plot-1.png)<!-- -->

``` r
p_geom_mean + p_arith_mean + plot_layout(guides = 'collect')
```

![](bootstrapping_distance_files/figure-gfm/plot-2.png)<!-- -->

``` r
cora <- data.frame(as.matrix(est.dist)) %>% 
  rownames_to_column("ID") %>% 
  pivot_longer(!ID) %>% 
  
  filter(!str_detect(ID,"\\.")) %>% 
  mutate(ID = str_remove(ID, "X")) %>% 
  mutate(ID = str_remove(ID, "V")) %>% 
  
  
  mutate(name = str_remove(name, "\\.")) %>% 
  mutate(name = str_remove(name, "X")) %>% 
  mutate(name = str_remove(name, "V")) %>% 
  
  filter(as.numeric(ID)   <= 201 & as.numeric(ID)   < 401) %>% 
  filter(as.numeric(name) >  201 & as.numeric(name) < 401) %>% 
  filter(name != ID) %>% 
  
  mutate(ID   = dep[ID]) %>% 
  mutate(name = dep[name]) %>% 
  group_by(ID,name) %>% 
  summarise(mean = mean(value),
            var  = var(value)) %>% 
  ungroup() %>% 
  mutate(mean_s = long_dist1$mean) %>% 
  mutate(mean_offset = (mean +  mean_s)/2 ) %>% 
  mutate(mean_from_10 = mean_offset - 10) %>% 
  mutate(mindepth = pmin(ID, name)) %>% 
  
  ggplot() +
  aes(x = mean_s, y = mean_from_10, colour = mindepth) +
  
  geom_point() +
  theme_bw() +
  ggtitle("Comparing errors - normalised by arith_mean", subtitle = "10% Rare features") +
  xlab("Within Group Error") +
  ylab("Between Group Error")
```

    ## `summarise()` has grouped output by 'ID'. You can override using the `.groups`
    ## argument.

``` r
corg <- data.frame(as.matrix(est.dist)) %>% 
  rownames_to_column("ID") %>% 
  pivot_longer(!ID) %>% 
  
  filter(!str_detect(ID,"\\.")) %>% 
  mutate(ID = str_remove(ID, "X")) %>% 
  mutate(ID = str_remove(ID, "V")) %>% 
  
  
  mutate(name = str_remove(name, "\\.")) %>% 
  mutate(name = str_remove(name, "X")) %>% 
  mutate(name = str_remove(name, "V")) %>% 
  
  filter(as.numeric(ID)   <= 201 & as.numeric(ID)   < 401) %>% 
  filter(as.numeric(name) >  201 & as.numeric(name) < 401) %>% 
  filter(name != ID) %>% 
  
  mutate(ID   = dep[ID]) %>% 
  mutate(name = dep[name]) %>% 
  group_by(ID,name) %>% 
  summarise(mean = mean(value),
            var  = var(value)) %>% 
  ungroup() %>% 
  mutate(mean_s = long_dist1$mean) %>% 
  mutate(mean_offset = sqrt((mean *  mean_s)) ) %>% 
  mutate(mean_from_10 = mean_offset - 10) %>% 
  mutate(mindepth = pmin(ID, name)) %>% 
  
  ggplot() +
  aes(x = mean_s, y = mean_from_10, colour = mindepth) +
  
  geom_point() +
  theme_bw() +
  ggtitle("Comparing errors - normalised by geom_mean", subtitle = "10% Rare features") +
  xlab("Within Group Error") +
  ylab("Between Group Error")
```

    ## `summarise()` has grouped output by 'ID'. You can override using the `.groups`
    ## argument.

``` r
corc <- data.frame(as.matrix(est.dist)) %>% 
  rownames_to_column("ID") %>% 
  pivot_longer(!ID) %>% 
  
  filter(!str_detect(ID,"\\.")) %>% 
  mutate(ID = str_remove(ID, "X")) %>% 
  mutate(ID = str_remove(ID, "V")) %>% 
  
  
  mutate(name = str_remove(name, "\\.")) %>% 
  mutate(name = str_remove(name, "X")) %>% 
  mutate(name = str_remove(name, "V")) %>% 
  
  filter(as.numeric(ID)   <= 201 & as.numeric(ID)   < 401) %>% 
  filter(as.numeric(name) >  201 & as.numeric(name) < 401) %>% 
  filter(name != ID) %>% 
  
  mutate(ID   = dep[ID]) %>% 
  mutate(name = dep[name]) %>% 
  group_by(ID,name) %>% 
  summarise(mean = mean(value),
            var  = var(value)) %>% 
  ungroup() %>% 
  mutate(mean_s = long_dist1$mean) %>% 

  mutate(mean_offset = ((mean - 10 ) +  mean_s)/2 ) %>% 
  mutate(mean_from_10 = mean_offset) %>% 
  mutate(mindepth = pmin(ID, name)) %>% 
  
  ggplot() +
  aes(x = mean_s, y = mean_from_10, colour = mindepth) +
  
  geom_point() +
  theme_bw() +
  ggtitle("Comparing errors - normalised by arith_mean", subtitle = "10% Rare features") +
  xlab("Within Group Error") +
  ylab("Between Group Error")
```

    ## `summarise()` has grouped output by 'ID'. You can override using the `.groups`
    ## argument.

``` r
cord <- data.frame(as.matrix(est.dist)) %>% 
  rownames_to_column("ID") %>% 
  pivot_longer(!ID) %>% 
  
  filter(!str_detect(ID,"\\.")) %>% 
  mutate(ID = str_remove(ID, "X")) %>% 
  mutate(ID = str_remove(ID, "V")) %>% 
  
  
  mutate(name = str_remove(name, "\\.")) %>% 
  mutate(name = str_remove(name, "X")) %>% 
  mutate(name = str_remove(name, "V")) %>% 
  
  filter(as.numeric(ID)   <= 201 & as.numeric(ID)   < 401) %>% 
  filter(as.numeric(name) >  201 & as.numeric(name) < 401) %>% 
  filter(name != ID) %>% 
  
  mutate(ID   = dep[ID]) %>% 
  mutate(name = dep[name]) %>% 
  group_by(ID,name) %>% 
  summarise(mean = mean(value),
            var  = var(value)) %>% 
  ungroup() %>% 
  mutate(mean_s = long_dist1$mean) %>% 

  mutate(mean_offset = sqrt(((mean - 10) *  mean_s)) ) %>% 
  mutate(mean_from_10 = mean_offset) %>% 
  mutate(mindepth = pmin(ID, name)) %>% 
  
  ggplot() +
  aes(x = mean_s, y = mean_from_10, colour = mindepth) +
  
  geom_point() +
  theme_bw() +
  ggtitle("Comparing errors - normalised by geom_mean", subtitle = "10% Rare features") +
  xlab("Within Group Error") +
  ylab("Between Group Error")
```

    ## `summarise()` has grouped output by 'ID'. You can override using the `.groups`
    ## argument.

    ## Warning in sqrt(((mean - 10) * mean_s)): NaNs produced

``` r
(cora + corg) /
  (corc  + cord)+ plot_layout(guides = 'collect')
```

    ## Warning: Removed 400 rows containing missing values (geom_point).

![](bootstrapping_distance_files/figure-gfm/pursue_offset-1.png)<!-- -->

``` r
data.frame(as.matrix(est.dist)) %>% 
  rownames_to_column("ID") %>% 
  pivot_longer(!ID) %>% 
  
  filter(!str_detect(ID,"\\.")) %>% 
  mutate(ID = str_remove(ID, "X")) %>% 
  mutate(ID = str_remove(ID, "V")) %>% 
  
  
  mutate(name = str_remove(name, "\\.")) %>% 
  mutate(name = str_remove(name, "X")) %>% 
  mutate(name = str_remove(name, "V")) %>% 
  
  filter(as.numeric(ID)   <= 201 & as.numeric(ID)   < 401) %>% 
  filter(as.numeric(name) >  201 & as.numeric(name) < 401) %>% 
  filter(name != ID) %>% 
  
  mutate(ID   = dep[ID]) %>% 
  mutate(name = dep[name]) %>% 
  group_by(ID,name) %>% 
  summarise(mean = mean(value),
            var  = var(value)) %>% 
  ungroup() %>% 
  
  mutate(mean_s = long_dist1$mean) %>% 

  mutate(mean_s = mean_s * coefficients(lm(mean~mean_s))[2]) %>% 

 # mutate(mean_offset = sqrt((mean *  mean_s)) ) %>% 
 
  mutate(mean_offset = (mean - mean_s) ) %>% 
  #mutate(of)
 #  .$mean_offset %>% mean
  #mutate(mean_offset = mean_offset - 10*0.85) %>% 

  #mutate(mean_from_10 = mean_offset - 10*0.85) %>% 
  
  mutate(mean_from_10 = mean_offset - 10) %>% 
  
  mutate(mindepth = pmin(ID, name)) %>% 
  
  ggplot() +
  aes(x = mean_s, y = mean_from_10, colour = mindepth) +
  
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  ggtitle("Comparing errors - normalised by arith_mean", subtitle = "10% Rare features") +
  xlab("Within Group Error") +
  ylab("Between Group Error")
```

    ## `summarise()` has grouped output by 'ID'. You can override using the `.groups`
    ## argument.
    ## `geom_smooth()` using formula 'y ~ x'

![](bootstrapping_distance_files/figure-gfm/We%20must%20go%20deeper-1.png)<!-- -->

``` r
data.frame(as.matrix(est.dist)) %>% 
  rownames_to_column("ID") %>% 
  pivot_longer(!ID) %>% 
  
  filter(!str_detect(ID,"\\.")) %>% 
  mutate(ID = str_remove(ID, "X")) %>% 
  mutate(ID = str_remove(ID, "V")) %>% 
  
  
  mutate(name = str_remove(name, "\\.")) %>% 
  mutate(name = str_remove(name, "X")) %>% 
  mutate(name = str_remove(name, "V")) %>% 
  
  filter(as.numeric(ID)   <= 201 & as.numeric(ID)   < 401) %>% 
  filter(as.numeric(name) >  201 & as.numeric(name) < 401) %>% 
  filter(name != ID) %>% 
  
  mutate(ID   = dep[ID]) %>% 
  mutate(name = dep[name]) %>% 
  group_by(ID,name) %>% 
  summarise(mean = mean(value),
            var  = var(value)) %>% 
  ungroup() %>% 
  
  mutate(mean_s = long_dist1$mean) %>% 

  mutate(mean_c = mean_s * coefficients(lm(mean~mean_s))[2]) %>% 

  mutate(mean_offset = (mean - mean_c) ) %>% 
  
  mutate(mean_from_10 = mean_offset - 10) %>% 
  
  mutate(mindepth = pmin(ID, name)) %>% 
  
  ggplot() +
  aes(x = ID, y = name, fill = mean_from_10, label = round(mean_from_10, 1)) +
  
  geom_tile() +
  geom_text(colour = "black", size = 2.5) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limits = c(-1.5,1.5), "Delta from true mean") +
  theme_bw() +
  ggtitle("Error from true distance after regressing out within-sample error", subtitle = "10% Rare features") +
  xlab("Sampling depth of first sample") +
  ylab("Sampling depth of second sample")
```

    ## `summarise()` has grouped output by 'ID'. You can override using the `.groups`
    ## argument.

![](bootstrapping_distance_files/figure-gfm/We%20must%20go%20deeper-2.png)<!-- -->
