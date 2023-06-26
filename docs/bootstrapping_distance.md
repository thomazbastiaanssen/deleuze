``` r
set.seed(12345)
library(tidyverse)
library(deleuze)
library(patchwork)
library(Tjazi)
library(vegan)
library(LaplacesDemon)


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
```

``` r
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
```

``` r
#This is a slow step.
meandiff <- apply_pw_diff(data.exp)
```

    ## [1] "Finished iteration 1"
    ## [1] "Finished iteration 2"
    ## [1] "Finished iteration 3"
    ## [1] "Finished iteration 4"
    ## [1] "Finished iteration 5"
    ## [1] "Finished iteration 6"
    ## [1] "Finished iteration 7"
    ## [1] "Finished iteration 8"
    ## [1] "Finished iteration 9"
    ## [1] "Finished iteration 10"
    ## [1] "Finished iteration 11"
    ## [1] "Finished iteration 12"
    ## [1] "Finished iteration 13"
    ## [1] "Finished iteration 14"
    ## [1] "Finished iteration 15"
    ## [1] "Finished iteration 16"
    ## [1] "Finished iteration 17"
    ## [1] "Finished iteration 18"
    ## [1] "Finished iteration 19"
    ## [1] "Finished iteration 20"
    ## [1] "Finished iteration 21"
    ## [1] "Finished iteration 22"
    ## [1] "Finished iteration 23"
    ## [1] "Finished iteration 24"
    ## [1] "Finished iteration 25"
    ## [1] "Finished iteration 26"
    ## [1] "Finished iteration 27"
    ## [1] "Finished iteration 28"
    ## [1] "Finished iteration 29"
    ## [1] "Finished iteration 30"
    ## [1] "Finished iteration 31"
    ## [1] "Finished iteration 32"
    ## [1] "Finished iteration 33"
    ## [1] "Finished iteration 34"
    ## [1] "Finished iteration 35"
    ## [1] "Finished iteration 36"
    ## [1] "Finished iteration 37"
    ## [1] "Finished iteration 38"
    ## [1] "Finished iteration 39"
    ## [1] "Finished iteration 40"
    ## [1] "Finished iteration 41"
    ## [1] "Finished iteration 42"
    ## [1] "Finished iteration 43"
    ## [1] "Finished iteration 44"
    ## [1] "Finished iteration 45"
    ## [1] "Finished iteration 46"
    ## [1] "Finished iteration 47"
    ## [1] "Finished iteration 48"
    ## [1] "Finished iteration 49"
    ## [1] "Finished iteration 50"
    ## [1] "Finished iteration 51"
    ## [1] "Finished iteration 52"
    ## [1] "Finished iteration 53"
    ## [1] "Finished iteration 54"
    ## [1] "Finished iteration 55"
    ## [1] "Finished iteration 56"
    ## [1] "Finished iteration 57"
    ## [1] "Finished iteration 58"
    ## [1] "Finished iteration 59"
    ## [1] "Finished iteration 60"
    ## [1] "Finished iteration 61"
    ## [1] "Finished iteration 62"
    ## [1] "Finished iteration 63"
    ## [1] "Finished iteration 64"
    ## [1] "Finished iteration 65"
    ## [1] "Finished iteration 66"
    ## [1] "Finished iteration 67"
    ## [1] "Finished iteration 68"
    ## [1] "Finished iteration 69"
    ## [1] "Finished iteration 70"
    ## [1] "Finished iteration 71"
    ## [1] "Finished iteration 72"
    ## [1] "Finished iteration 73"
    ## [1] "Finished iteration 74"
    ## [1] "Finished iteration 75"
    ## [1] "Finished iteration 76"
    ## [1] "Finished iteration 77"
    ## [1] "Finished iteration 78"
    ## [1] "Finished iteration 79"
    ## [1] "Finished iteration 80"
    ## [1] "Finished iteration 81"
    ## [1] "Finished iteration 82"
    ## [1] "Finished iteration 83"
    ## [1] "Finished iteration 84"
    ## [1] "Finished iteration 85"
    ## [1] "Finished iteration 86"
    ## [1] "Finished iteration 87"
    ## [1] "Finished iteration 88"
    ## [1] "Finished iteration 89"
    ## [1] "Finished iteration 90"
    ## [1] "Finished iteration 91"
    ## [1] "Finished iteration 92"
    ## [1] "Finished iteration 93"
    ## [1] "Finished iteration 94"
    ## [1] "Finished iteration 95"
    ## [1] "Finished iteration 96"
    ## [1] "Finished iteration 97"
    ## [1] "Finished iteration 98"
    ## [1] "Finished iteration 99"
    ## [1] "Finished iteration 100"

``` r
meansq   <- meandiff^2

meandist <- sqrt(colSums(meansq))



meandistmat <- meandist %>% matrix(data = ., 
                                   nrow = nrow(outer(
                                     colnames(as.data.frame(data)), 
                                     colnames(as.data.frame(data)), 
                                     paste, sep="_"))
) %>% data.frame()
```

``` r
dep <- rep(rep(seq(1000,20000, by = 1000), each = 10), 2)
names(dep) <- as.character(1:400)          

long_dist = meandistmat %>% 
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
```

    ## `summarise()` has grouped output by 'ID'. You can override using the `.groups`
    ## argument.

``` r
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
```

![](bootstrapping_distance_files/figure-gfm/prepare%20plottingdata-1.png)<!-- -->

``` r
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
```

![](bootstrapping_distance_files/figure-gfm/prepare%20plottingdata-2.png)<!-- -->
