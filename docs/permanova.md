``` r
#devtools::install_github("thomazbastiaanssen/volatility")
library(tidyverse)
library(deleuze)
library(patchwork)
library(Tjazi)
library(vegan)
```

``` r
set.seed(12345)
library(tidyverse)
library(deleuze)
library(patchwork)
library(Tjazi)
library(vegan)

x <- 0
y <- 1
fib <- c()
while (x < 2000 & y < 2000){
  x <- x + y
  y <- x + y
  fib = c(fib, x, y)
}


res_fib = sapply(X = rep(seq(1000,10000, by = 1000), each = 100),FUN = function(x){
  table(factor(sample(paste0("size_",fib), prob = fib, replace = T,size = x ),levels = paste0("size_",fib)))
  
})

colnames(res_fib) = paste0(rep(seq(1000,10000, by = 1000), each = 100))



dist_logunif <- res_fib %>%
  data.frame() %>%
  Tjazi::clr_logunif() %>%
  t() %>%
  dist(x = ., method = "euclidean")  

dist_const <- res_fib %>%  
  data.frame() %>%
  Tjazi::clr_c() %>%
  t() %>%
  dist(x = ., method = "euclidean")  

dist_unif <- res_fib %>%
  data.frame() %>%
  Tjazi::clr_unif() %>%
  t() %>%
  dist(x = ., method = "euclidean")  

dist_shrunk <- (getTableMeans(res_fib, CLR_transformed = F)/(rowMeans(getTableVars(res_fib)))) %>%
  data.frame() %>%
  getTableMeans() %>%
  t() %>%
  dist(x = .,method = "euclidean")

dist_new <- res_fib %>% 
  data.frame() %>%
  getTableMeans() %>%
  t() %>%
  dist(x = ., method = "euclidean")


groups = rep(seq(1000,10000, by = 1000), each = 100)

vegan::adonis2(dist_const   ~ groups, method = "euclidean", permutations = 1000)
```

    ## Permutation test for adonis under reduced model
    ## Terms added sequentially (first to last)
    ## Permutation: free
    ## Number of permutations: 1000
    ## 
    ## vegan::adonis2(formula = dist_const ~ groups, permutations = 1000, method = "euclidean")
    ##           Df SumOfSqs      R2      F   Pr(>F)    
    ## groups     1   452.82 0.19247 237.86 0.000999 ***
    ## Residual 998  1899.89 0.80753                    
    ## Total    999  2352.71 1.00000                    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
vegan::adonis2(dist_unif    ~ groups, method = "euclidean", permutations = 1000)
```

    ## Permutation test for adonis under reduced model
    ## Terms added sequentially (first to last)
    ## Permutation: free
    ## Number of permutations: 1000
    ## 
    ## vegan::adonis2(formula = dist_unif ~ groups, permutations = 1000, method = "euclidean")
    ##           Df SumOfSqs      R2      F   Pr(>F)    
    ## groups     1   390.15 0.16012 190.26 0.000999 ***
    ## Residual 998  2046.47 0.83988                    
    ## Total    999  2436.61 1.00000                    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
vegan::adonis2(dist_logunif ~ groups, method = "euclidean", permutations = 1000)
```

    ## Permutation test for adonis under reduced model
    ## Terms added sequentially (first to last)
    ## Permutation: free
    ## Number of permutations: 1000
    ## 
    ## vegan::adonis2(formula = dist_logunif ~ groups, permutations = 1000, method = "euclidean")
    ##           Df SumOfSqs      R2      F   Pr(>F)    
    ## groups     1   234.18 0.07689 83.134 0.000999 ***
    ## Residual 998  2811.29 0.92311                    
    ## Total    999  3045.47 1.00000                    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
vegan::adonis2(dist_new     ~ groups, method = "euclidean", permutations = 1000)
```

    ## Permutation test for adonis under reduced model
    ## Terms added sequentially (first to last)
    ## Permutation: free
    ## Number of permutations: 1000
    ## 
    ## vegan::adonis2(formula = dist_new ~ groups, permutations = 1000, method = "euclidean")
    ##           Df SumOfSqs      R2      F   Pr(>F)    
    ## groups     1   317.23 0.12563 143.39 0.000999 ***
    ## Residual 998  2207.90 0.87437                    
    ## Total    999  2525.13 1.00000                    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
vegan::adonis2(dist_shrunk  ~ groups, method = "euclidean", permutations = 1000)
```

    ## Permutation test for adonis under reduced model
    ## Terms added sequentially (first to last)
    ## Permutation: free
    ## Number of permutations: 1000
    ## 
    ## vegan::adonis2(formula = dist_shrunk ~ groups, permutations = 1000, method = "euclidean")
    ##           Df SumOfSqs      R2      F   Pr(>F)    
    ## groups     1   0.0795 0.00354 3.5466 0.000999 ***
    ## Residual 998  22.3576 0.99646                    
    ## Total    999  22.4370 1.00000                    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
