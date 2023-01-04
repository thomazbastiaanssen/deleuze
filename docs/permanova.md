``` r
set.seed(12345)
library(tidyverse)
library(deleuze)
library(patchwork)
library(Tjazi)
library(vegan)
```

``` r
set.seed(12345)
b1 <- c(rep(1, 10), rep(5, 20), rep(15, 20), rep(40, 20), rep(80, 20), rep(100, 5), rep(250, 5) )
b2 <- c(rep(1, 30), rep(5, 15), rep(15, 15), rep(40, 15), rep(80, 15), rep(100, 5), rep(350, 5) )
b3 <- c(rep(1, 50), rep(5, 10), rep(15, 10), rep(40, 10), rep(80, 10), rep(100, 5), rep(450, 5) )


res_b1 = sapply(X = rep(seq(1000,20000, by = 1000), each = 100),FUN = function(x){
  table(factor(sample(paste0("feature_",1:100), 
                      prob = b1, 
                      replace = T, size = x), levels = paste0("feature_",1:100)))
  
})

res_b2 = sapply(X = rep(seq(1000,20000, by = 1000), each = 100),FUN = function(x){
  table(factor(sample(paste0("feature_",1:100), 
                      prob = b2, 
                      replace = T, size = x), levels = paste0("feature_",1:100)))
  
})

res_b3 = sapply(X = rep(seq(1000,20000, by = 1000), each = 100),FUN = function(x){
  table(factor(sample(paste0("feature_",1:100), 
                      prob = b3, 
                      replace = T, size = x), levels = paste0("feature_",1:100)))
  
})
```

``` r
clr_logunif_b1 <- res_b1 %>%
  data.frame() %>%
  Tjazi::clr_logunif() %>%
  data.frame() %>%
  mutate(real_abundance = b1) %>%
  rownames_to_column("ID") %>%
  pivot_longer(!c(ID, real_abundance))  %>%
  mutate(depth = rep(rep(seq(from = 1000, to = 20000, by = 1000), each = 100), 100),
         source = "b1",
         method = "log-uniform replacement")


clr_const_b1 <- res_b1 %>%  
  data.frame() %>%
  Tjazi::clr_c() %>% 
  data.frame() %>%
  mutate(real_abundance = b1) %>%
  rownames_to_column("ID") %>%
  pivot_longer(!c(ID, real_abundance))  %>% 
  mutate(depth = rep(rep(seq(from = 1000, to = 20000, by = 1000), each = 100), 100), 
         source = "b1", 
         method = "constant replacement") 
  

clr_unif_b1 <- res_b1 %>%
  data.frame() %>%
  Tjazi::clr_unif() %>%
  data.frame() %>%
  mutate(real_abundance = b1) %>%
  rownames_to_column("ID") %>%
  pivot_longer(!c(ID, real_abundance))  %>%
  mutate(depth = rep(rep(seq(from = 1000, to = 20000, by = 1000), each = 100), 100),
         source = "b1",
         method = "uniform replacement")

clr_shrunk_b1 <- res_b1 %>%
  data.frame() %>%
  sCLR() %>% 
  data.frame() %>%
  mutate(real_abundance = b1) %>%
  rownames_to_column("ID") %>%
  pivot_longer(!c(ID, real_abundance))  %>% 
  mutate(depth = rep(rep(seq(from = 1000, to = 20000, by = 1000), each = 100), 100), 
         source = "b1", 
         method = "arithmetic shrinkage") 

clr_new_b1 <- res_b1 %>% 
  data.frame() %>%
  getTableMeans() %>% 
  data.frame() %>%
  mutate(real_abundance = b1) %>%
  rownames_to_column("ID") %>%
  pivot_longer(!c(ID, real_abundance))  %>% 
  mutate(depth = rep(rep(seq(from = 1000, to = 20000, by = 1000), each = 100), 100), 
         source = "b1", 
         method = "parameterization") 

clr_logunif_b2 <- res_b2 %>%
  data.frame() %>%
  Tjazi::clr_logunif() %>%
  data.frame() %>%
  mutate(real_abundance = b2) %>%
  rownames_to_column("ID") %>%
  pivot_longer(!c(ID, real_abundance))  %>%
  mutate(depth = rep(rep(seq(from = 1000, to = 20000, by = 1000), each = 100), 100),
         source = "b2",
         method = "log-uniform replacement")

clr_const_b2 <- res_b2 %>%  
  data.frame() %>%
  Tjazi::clr_c() %>% 
  data.frame() %>%
  mutate(real_abundance = b2) %>%
  rownames_to_column("ID") %>%
  pivot_longer(!c(ID, real_abundance))  %>% 
  mutate(depth = rep(rep(seq(from = 1000, to = 20000, by = 1000), each = 100), 100), 
         source = "b2", 
         method = "constant replacement") 

clr_unif_b2 <- res_b2 %>%
  data.frame() %>%
  Tjazi::clr_unif() %>%
  data.frame() %>%
  mutate(real_abundance = b2) %>%
  rownames_to_column("ID") %>%
  pivot_longer(!c(ID, real_abundance))  %>%
  mutate(depth = rep(rep(seq(from = 1000, to = 20000, by = 1000), each = 100), 100),
         source = "b2",
         method = "uniform replacement")

clr_shrunk_b2 <- res_b2 %>%
  data.frame() %>%
  sCLR() %>% 
  data.frame() %>%
  mutate(real_abundance = b2) %>%
  rownames_to_column("ID") %>%
  pivot_longer(!c(ID, real_abundance))  %>% 
  mutate(depth = rep(rep(seq(from = 1000, to = 20000, by = 1000), each = 100), 100), 
         source = "b2", 
         method = "arithmetic shrinkage") 

clr_new_b2 <- res_b2 %>% 
  data.frame() %>%
  getTableMeans() %>% 
  data.frame() %>%
  mutate(real_abundance = b2) %>%
  rownames_to_column("ID") %>%
  pivot_longer(!c(ID, real_abundance))  %>% 
  mutate(depth = rep(rep(seq(from = 1000, to = 20000, by = 1000), each = 100), 100), 
         source = "b2", 
         method = "parameterization") 

clr_logunif_b3 <- res_b3 %>%
  data.frame() %>%
  Tjazi::clr_logunif()  %>%
  data.frame() %>%
  mutate(real_abundance = b3) %>%
  rownames_to_column("ID") %>%
  pivot_longer(!c(ID, real_abundance))  %>%
  mutate(depth = rep(rep(seq(from = 1000, to = 20000, by = 1000), each = 100), 100),
         source = "b3",
         method = "log-uniform replacement")


clr_const_b3 <- res_b3 %>%  
  data.frame() %>%
  Tjazi::clr_c()  %>% 
  data.frame() %>%
  mutate(real_abundance = b3) %>%
  rownames_to_column("ID") %>%
  pivot_longer(!c(ID, real_abundance))  %>% 
  mutate(depth = rep(rep(seq(from = 1000, to = 20000, by = 1000), each = 100), 100), 
         source = "b3", 
         method = "constant replacement") 

clr_unif_b3 <- res_b3 %>%
  data.frame() %>%
  Tjazi::clr_unif() %>%
  data.frame() %>%
  mutate(real_abundance = b3) %>%
  rownames_to_column("ID") %>%
  pivot_longer(!c(ID, real_abundance))  %>%
  mutate(depth = rep(rep(seq(from = 1000, to = 20000, by = 1000), each = 100), 100),
         source = "b3",
         method = "uniform replacement")

clr_shrunk_b3 <- res_b3 %>%
  data.frame() %>%
  sCLR() %>% 
  data.frame() %>%
  mutate(real_abundance = b3) %>%
  rownames_to_column("ID") %>%
  pivot_longer(!c(ID, real_abundance))  %>% 
  mutate(depth = rep(rep(seq(from = 1000, to = 20000, by = 1000), each = 100), 100), 
         source = "b3", 
         method = "arithmetic shrinkage") 

clr_new_b3 <- res_b3 %>% 
  data.frame() %>%
  getTableMeans()  %>% 
  data.frame() %>%
  mutate(real_abundance = b3) %>%
  rownames_to_column("ID") %>%
  pivot_longer(!c(ID, real_abundance))  %>% 
  mutate(depth = rep(rep(seq(from = 1000, to = 20000, by = 1000), each = 100), 100), 
         source = "b3", 
         method = "parameterization") 

do.call(rbind, list(
  clr_const_b1,   clr_const_b2,   clr_const_b3, 
  clr_shrunk_b1,  clr_shrunk_b2,  clr_shrunk_b3, 
  clr_new_b1,     clr_new_b2,     clr_new_b3, 
  clr_unif_b1,    clr_unif_b2,    clr_unif_b3, 
  clr_logunif_b1, clr_logunif_b2, clr_logunif_b3
                    )) %>% 
  mutate(source = str_replace(source, pattern = "b1", replacement = "10% Rare Features"), 
         source = str_replace(source, pattern = "b2", replacement = "30% Rare Features"), 
         source = str_replace(source, pattern = "b3", replacement = "50% Rare Features")) %>%
  mutate(real_abundance = as.character(real_abundance)) %>%
  mutate(real_abundance = str_replace(real_abundance, pattern = "250|350|450", replacement = "High" )) %>%

  mutate(real_abundance = factor(real_abundance, levels = c("1", "5", "15", "40", "80", "100", "High"))) %>%
  #filter(as.numeric(real_abundance) < 20) %>%
  ggplot() +
  aes(x = depth, y = value, colour = real_abundance) +
  geom_point(alpha = 1/100) + 
  geom_smooth(se = F) +
  scale_colour_manual(values = c(
    "1" = "#e41a1c", 
    "5" = "#377eb8", 
    "15" = "#4daf4a", 
    "40" = "#984ea3", 
    "80" = "#ff7f00", 
    "100" = "#ffff33", 
    "High" = "#a65628"), 
    "Sampling abundance") +
  guides(colour = guide_legend(override.aes = list(alpha = 1) ) ) +
  facet_grid(method~source, switch = "y") +
  
  theme_bw() +
  theme(legend.position = 'bottom') 
```

    ## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'

![](permanova_files/figure-gfm/individual%20feature%20precision-1.png)<!-- -->

``` r
do.call(rbind, list(
  clr_const_b1,   clr_const_b2,   clr_const_b3, 
  clr_shrunk_b1,  clr_shrunk_b2,  clr_shrunk_b3, 
  clr_new_b1,     clr_new_b2,     clr_new_b3, 
  clr_unif_b1,    clr_unif_b2,    clr_unif_b3, 
  clr_logunif_b1, clr_logunif_b2, clr_logunif_b3
                    )) %>% 
  mutate(source = str_replace(source, pattern = "b1", replacement = "10% Rare Features"), 
         source = str_replace(source, pattern = "b2", replacement = "30% Rare Features"), 
         source = str_replace(source, pattern = "b3", replacement = "50% Rare Features")) %>%
  mutate(real_abundance = as.character(real_abundance)) %>%
  mutate(real_abundance = str_replace(real_abundance, pattern = "250|350|450", replacement = "High" )) %>%

  group_by(source, depth, method, real_abundance) %>%
  summarise(Variance = var(value)) %>%
  ungroup() %>%
  mutate(real_abundance = factor(real_abundance, levels = c("1", "5", "15", "40", "80", "100", "High"))) %>%
  #filter(as.numeric(real_abundance) < 20) %>%
  ggplot() +
  aes(x = depth, y = Variance, colour = real_abundance, fill = real_abundance) +
  geom_smooth(method = "loess", se = F) +
  geom_point(shape = 21, colour = "black") + 
  scale_colour_manual(values = c("1" = "#e41a1c", "5" = "#377eb8", "15" = "#4daf4a", "40" = "#984ea3", "80" = "#ff7f00", "100" = "#ffff33", "High" = "#a65628"), "Sampling abundance") +
    scale_fill_manual(values = c("1" = "#e41a1c", "5" = "#377eb8", "15" = "#4daf4a", "40" = "#984ea3", "80" = "#ff7f00", "100" = "#ffff33", "High" = "#a65628"), "Sampling abundance") +
  guides(colour = guide_legend(override.aes = list(alpha = 1) ) ) +
  facet_grid(method~source, switch = "y") +
  
  theme_bw() +
  theme(legend.position = 'bottom') 
```

    ## `summarise()` has grouped output by 'source', 'depth', 'method'. You can
    ## override using the `.groups` argument.
    ## `geom_smooth()` using formula 'y ~ x'

![](permanova_files/figure-gfm/variance%20by%20feature-1.png)<!-- -->

``` r
dist_logunif_b1 <- res_b1 %>%
  data.frame() %>%
  Tjazi::clr_logunif() %>%
  t() %>%
  dist(x = ., method = "euclidean")

dist_const_b1 <- res_b1 %>%  
  data.frame() %>%
  Tjazi::clr_c() %>%
  t() %>%
  dist(x = ., method = "euclidean")  

dist_unif_b1 <- res_b1 %>%
  data.frame() %>%
  Tjazi::clr_unif() %>%
  t() %>%
  dist(x = ., method = "euclidean")

dist_shrunk_b1 <- res_b1 %>%
  data.frame() %>%
  sCLR() %>%
  t() %>%
  dist(x = .,method = "euclidean")

dist_new_b1 <- res_b1 %>% 
  data.frame() %>%
  getTableMeans() %>%
  t() %>%
  dist(x = ., method = "euclidean")


dist_logunif_b2 <- res_b2 %>%
  data.frame() %>%
  Tjazi::clr_logunif() %>%
  t() %>%
  dist(x = ., method = "euclidean")

dist_const_b2 <- res_b2 %>%  
  data.frame() %>%
  Tjazi::clr_c() %>%
  t() %>%
  dist(x = ., method = "euclidean")  

dist_unif_b2 <- res_b2 %>%
  data.frame() %>%
  Tjazi::clr_unif() %>%
  t() %>%
  dist(x = ., method = "euclidean")

dist_shrunk_b2 <- res_b2 %>%
  data.frame() %>%
  sCLR() %>%
  t() %>%
  dist(x = .,method = "euclidean")

dist_new_b2 <- res_b2 %>% 
  data.frame() %>%
  getTableMeans() %>%
  t() %>%
  dist(x = ., method = "euclidean")


dist_logunif_b3 <- res_b3 %>%
  data.frame() %>%
  Tjazi::clr_logunif() %>%
  t() %>%
  dist(x = ., method = "euclidean")

dist_const_b3 <- res_b3 %>%  
  data.frame() %>%
  Tjazi::clr_c() %>%
  t() %>%
  dist(x = ., method = "euclidean")  

dist_unif_b3 <- res_b3 %>%
  data.frame() %>%
  Tjazi::clr_unif() %>%
  t() %>%
  dist(x = ., method = "euclidean")

dist_shrunk_b3 <- res_b3 %>%
  data.frame() %>%
  sCLR() %>%
  t() %>%
  dist(x = .,method = "euclidean")

dist_new_b3 <- res_b3 %>% 
  data.frame() %>%
  getTableMeans() %>%
  t() %>%
  dist(x = ., method = "euclidean")


groups = rep(seq(1000,20000, by = 1000), each = 100)

b1_v1 <- vegan::adonis2(dist_const_b1   ~ groups, method = "euclidean", permutations = 1000)
b1_v2 <- vegan::adonis2(dist_unif_b1    ~ groups, method = "euclidean", permutations = 1000)
b1_v3 <- vegan::adonis2(dist_logunif_b1 ~ groups, method = "euclidean", permutations = 1000)
b1_v4 <- vegan::adonis2(dist_new_b1     ~ groups, method = "euclidean", permutations = 1000)
b1_v5 <- vegan::adonis2(dist_shrunk_b1  ~ groups, method = "euclidean", permutations = 1000)

adonis_b1 <- data.frame(R2 =     c(b1_v1$R2[1], 
                                   b1_v2$R2[1], 
                                   b1_v3$R2[1], 
                                   b1_v4$R2[1], 
                                   b1_v5$R2[1]), 
                        method = factor(c("constant replacement", 
                                          "uniform replacement", 
                                          "log-uniform replacement", 
                                          "parameterization", 
                                          "arithmetric shrinkage"), 
                                        levels = rev(c("constant replacement", 
                                                       "uniform replacement", 
                                                       "log-uniform replacement", 
                                                       "parameterization", 
                                                       "arithmetric shrinkage"))), 
                        distribution = "10%")


b2_v1 <- vegan::adonis2(dist_const_b2   ~ groups, method = "euclidean", permutations = 1000)
b2_v2 <- vegan::adonis2(dist_unif_b2    ~ groups, method = "euclidean", permutations = 1000)
b2_v3 <- vegan::adonis2(dist_logunif_b2 ~ groups, method = "euclidean", permutations = 1000)
b2_v4 <- vegan::adonis2(dist_new_b2     ~ groups, method = "euclidean", permutations = 1000)
b2_v5 <- vegan::adonis2(dist_shrunk_b2  ~ groups, method = "euclidean", permutations = 1000)

adonis_b2 <- data.frame(R2 =     c(b2_v1$R2[1], 
                                   b2_v2$R2[1], 
                                   b2_v3$R2[1], 
                                   b2_v4$R2[1], 
                                   b2_v5$R2[1]), 
                        method = factor(c("constant replacement", 
                                          "uniform replacement", 
                                          "log-uniform replacement", 
                                          "parameterization", 
                                          "arithmetric shrinkage"), 
                                        levels = rev(c("constant replacement", 
                                                       "uniform replacement", 
                                                       "log-uniform replacement", 
                                                       "parameterization", 
                                                       "arithmetric shrinkage"))), 
                        distribution = "30%")


b3_v1 <- vegan::adonis2(dist_const_b3   ~ groups, method = "euclidean", permutations = 1000)
b3_v2 <- vegan::adonis2(dist_unif_b3    ~ groups, method = "euclidean", permutations = 1000)
b3_v3 <- vegan::adonis2(dist_logunif_b3 ~ groups, method = "euclidean", permutations = 1000)
b3_v4 <- vegan::adonis2(dist_new_b3     ~ groups, method = "euclidean", permutations = 1000)
b3_v5 <- vegan::adonis2(dist_shrunk_b3  ~ groups, method = "euclidean", permutations = 1000)

adonis_b3 <- data.frame(R2 =     c(b3_v1$R2[1], 
                                   b3_v2$R2[1], 
                                   b3_v3$R2[1], 
                                   b3_v4$R2[1], 
                                   b3_v5$R2[1]), 
                        method = factor(c("constant replacement", 
                                          "uniform replacement", 
                                          "log-uniform replacement", 
                                          "parameterization", 
                                          "arithmetric shrinkage"), 
                                        levels = rev(c("constant replacement", 
                                                       "uniform replacement", 
                                                       "log-uniform replacement", 
                                                       "parameterization", 
                                                       "arithmetric shrinkage"))), 
                        distribution = "50%")


do.call(rbind, list(adonis_b1, adonis_b2, adonis_b3)) %>%
  mutate(distribution = factor(distribution, levels = rev(c("10%", "30%", "50%")))) %>%

ggplot() +
  
  aes(x = R2, y = method, fill = distribution, colour = distribution, groups = distribution) + 
  
  geom_bar(stat = "identity", position = position_dodge(0.5), width = 1/5)+
  geom_point(size = 4, shape = 21, colour = "black", position = position_dodge2(0.5, preserve = "total")) +
  #facet_wrap(~distribution) +
  
  theme_bw() +
  theme(legend.position = 'bottom') + 
  scale_x_continuous(limits = c(0,.1)) +
  scale_fill_manual(values = c("10%" = "#feb24c", "30%" = "#fc4e2a", "50%" = "#800026"), "Rare features",
                      guide = guide_legend(reverse = TRUE)) +
  scale_colour_manual(values = c("10%" = "#feb24c", "30%" = "#fc4e2a", "50%" = "#800026"), "Rare features",
                      guide = guide_legend(reverse = TRUE))+

  ylab("") + 
  xlab(Variance~explained~by~sampling~depth~(R^2)) +
  ggtitle("The effect of sampling depth is dependent on zero imputation strategy")
```

![](permanova_files/figure-gfm/permanovas-1.png)<!-- -->
