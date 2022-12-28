For benchmarking click
[Here](https://github.com/thomazbastiaanssen/deleuze/blob/main/docs/benchmarking.md)
For PERMANOVAs click
[Here](https://github.com/thomazbastiaanssen/deleuze/blob/main/docs/permanova.md)

(put on their own page as it’s slow to knit)

``` r
#devtools::install_github("thomazbastiaanssen/volatility")
library(tidyverse)
library(deleuze)
library(patchwork)
library(Tjazi)
library(vegan)

data = volatility::vola_genus_table$Validation_Pre_Control_2

data
```

    ##   [1]    0    0    0    0    0   47  114   20  453  915    2  757    0    0    0
    ##  [16]    0    0    0  144 2030    0    0  154   17   23    0    0    0    6    0
    ##  [31]    0    0    0    0    9   41    0    0    0    0    0    0    0   17    0
    ##  [46]   54  130    0    0    0    0  487    0    0    0   86    0  808  162    0
    ##  [61]    0    0 7547    0  242    0  364    0    0    0    0    0 1403    0   55
    ##  [76]   19    0    0    0    0  114   58  149    8    0    0    0    0   12    6
    ##  [91]    0  508    0  773    0    7    0    0    0  705    0  216  212  934   41
    ## [106]  188    0   95    0    0   95   63  111  564   75    0    0  147    0    0
    ## [121]    0    0    0    0   59    0    0    0    0    0    0    8    0    0    4
    ## [136] 3642    0

``` r
#comparing the distributions
#comparing mean and SD of the resampled and approximated distributions
knitr::kable(t(data.frame("observed" = 
                            c("mean"   = mean(sampleGeomMeam(samples = 10000, count_sample = data, log_transformed = T)), 
                              "sd"     = sd(sampleGeomMeam(samples = 10000, count_sample = data, log_transformed = T))), 
                          "approximated" = 
                            c("mean" = mean(getBetaMeans(data, log_transformed = T)), 
                              "sd"   = sqrt(sum(getBetaVars(count_sample = data, log_transformed = T)) /(length(data)* length(data)))))
               )
             )
```

|              |      mean |        sd |
|:-------------|----------:|----------:|
| observed     | -8.645728 | 0.0852562 |
| approximated | -8.645706 | 0.0859221 |

``` r
#Overlaid:
plot(density(sampleGeomMeam(samples = 10000, count_sample = data, log_transformed = T)), 
     col = "red", main = "Comparing the sampled geometric mean (red)\n to the approximated distribution (black)")

lines(density(sampleGeomMeanApprox(samples = 10000, count_sample = data, log_transformed = T)))
```

![](README_files/figure-gfm/estimation%20of%20the%20geometric%20mean-1.png)<!-- -->

``` r
#real sampled data

a = sampleCLR(10000, data) %>%
  data.frame() %>%
  mutate(sample = as.character(1:10000)) %>%
  pivot_longer(!sample) %>%
  
  filter(name %in% paste0("X", 1:20)) %>%
  mutate(type = "sampled")

#sampled from approximation
b = sampleCLRApprox(samples = 10000, data) %>%
  data.frame() %>%
  mutate(sample = as.character(1:10000)) %>% 
  pivot_longer(!sample) %>%
  
  filter(name %in% paste0("X", 1:20)) %>%
  mutate(type = "approx")

rbind(a, b) %>%
  filter(name %in% paste0("X", 1:15)) %>%

  mutate(name = paste(name, "n counts =", data[1:15])) %>%
  mutate(name = factor(name, levels = paste0("X", 1:15, " n counts = ", data[1:15]))) %>%
  

  ggplot() +
  aes(x = value, fill = type) +
  
  geom_density(alpha = 2/3) +
  
  facet_wrap(~name, scales = "free", ncol = 3) +
  theme_bw() +
  theme(legend.position = 'bottom') + 
  ggtitle("Notice that the zero-count features such as X13\n have a much higher spread than high rollers like X7")
```

<img src="README_files/figure-gfm/comparing CLR to approx-1.png" width="100%" />

Dividing by the variance of the CLR transformed data before transforming
reduces dispersion.

``` r
set.seed(12345)

library(tidyverse)
library(deleuze)

#Create some dummy data with known ground truth
vec = round(runif(n = 100, min = 1, max = 100))


x = exp(c(
  seq(1,4.95,by = 0.05), 
  seq(5,1.05,by = -0.05)
  ))

y = c(x[41:160],x[1:40])
  

z = rep(1,160)

xpart = sapply(X = vec[ 1: 25], FUN = function(f){(f*x)}, simplify = T) 
ypart = sapply(X = vec[26: 50], FUN = function(f){(f*y)}, simplify = T) 
zpart = sapply(X = vec[51:100], FUN = function(f){(f*z)}, simplify = T)


dummy = do.call(cbind, list(xpart,ypart,zpart)) 

  
rownames(dummy) = paste("sample",  1:160, sep = "_")
colnames(dummy) = paste("microbe", 1:100, sep = "_") 


res_dummy = apply(dummy, 1,FUN = function(x){
  table(factor(sample(colnames(dummy), prob = x, replace = T,size = 10000 ),levels = colnames(dummy)))
  
})

par(mfrow = c(1,2))

plot(c(unlist(dummy[1:10,] %>% 
                t() %>% 
                Tjazi::clr_c() )),
     
     c(
       getTableMeans(res_dummy[,1:10]/
                       (rowMeans(getTableVars(res_dummy[,1:10]))))
          )
)


plot(c(unlist(dummy[1:10,] %>% 
                t() %>% 
                Tjazi::clr_c() )),
     c(unlist(res_dummy[,1:10] %>% 
                
                Tjazi::clr_c() )))
```

<img src="README_files/figure-gfm/reduce overdispersion-1.png" width="100%" />

``` r
set.seed(12345)



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
#View(res_fib)

data.a.pca = res_fib %>% 
  cbind("real" = fib) %>%
  data.frame() %>%
  clr_c() %>%
  t() %>%
  prcomp()

#Extract the amount of variance the first four components explain for plotting. 
pc1 <- round(data.a.pca$sdev[1]^2/sum(data.a.pca$sdev^2),4) * 100
pc2 <- round(data.a.pca$sdev[2]^2/sum(data.a.pca$sdev^2),4) * 100
pc3 <- round(data.a.pca$sdev[3]^2/sum(data.a.pca$sdev^2),4) * 100
pc4 <- round(data.a.pca$sdev[4]^2/sum(data.a.pca$sdev^2),4) * 100

#Extract the scores for every sample for the first four components for plotting. 
pca  = data.frame(PC1 = data.a.pca$x[,1], 
                  PC2 = data.a.pca$x[,2], 
                  PC3 = data.a.pca$x[,3], 
                  PC4 = data.a.pca$x[,4])

pca$samples = str_remove(row.names(pca), pattern = "X") %>%
  str_remove("\\..*") %>% factor(levels = c(seq(1000,10000, by = 1000), "real"))



#First, the main plot. Plot the first two components of the PCA
old = ggplot(pca, aes(x       = PC1,
                y       = PC2,
                fill    = samples)) +  
  
  #Create the points and ellipses
  stat_ellipse(geom = "polygon", alpha = 1/4) +
  geom_point(col = "black", shape = 21, 
             aes(size = samples == "real")) + 
  #Adjust appearance
  
  #Adjust labels
  ggtitle("old method") + 
  xlab(paste("PC1: ", pc1,  "%", sep="")) + 
  ylab(paste("PC2: ", pc2,  "%", sep="")) +
  theme_bw() +
  theme(legend.position = 'bottom') 


data.a.pca = (res_fib %>% cbind("real" = fib)) %>%
  data.frame() %>%
  getTableMeans() %>%
  t() %>%
  prcomp()

#Extract the amount of variance the first four components explain for plotting. 
pc1 <- round(data.a.pca$sdev[1]^2/sum(data.a.pca$sdev^2),4) * 100
pc2 <- round(data.a.pca$sdev[2]^2/sum(data.a.pca$sdev^2),4) * 100
pc3 <- round(data.a.pca$sdev[3]^2/sum(data.a.pca$sdev^2),4) * 100
pc4 <- round(data.a.pca$sdev[4]^2/sum(data.a.pca$sdev^2),4) * 100

#Extract the scores for every sample for the first four components for plotting. 
pca  = data.frame(PC1 = data.a.pca$x[,1], 
                  PC2 = data.a.pca$x[,2], 
                  PC3 = data.a.pca$x[,3], 
                  PC4 = data.a.pca$x[,4])

pca$samples = str_remove(row.names(pca), pattern = "X") %>%
  str_remove("\\..*") %>% factor(levels = c(seq(1000,10000, by = 1000), "real"))



#First, the main plot. Plot the first two components of the PCA
new = ggplot(pca, aes(x       = PC1,
                      y       = PC2,
                      fill    = samples)) +  
  
  #Create the points and ellipses
  stat_ellipse(geom = "polygon", alpha = 1/4) +
  geom_point(col = "black", shape = 21, 
             aes(size = samples == "real")) + 
  #Adjust appearance
  
  #Adjust labels
  ggtitle("new method") + 
  xlab(paste("PC1: ", pc1,  "%", sep="")) + 
  ylab(paste("PC2: ", pc2,  "%", sep="")) +
  theme_bw() +
  theme(legend.position = 'bottom') 



data.a.pca =  sCLR(res_fib %>% cbind("real" = fib)) %>%
  data.frame() %>%
  t() %>%
  prcomp()

#Extract the amount of variance the first four components explain for plotting. 
pc1 <- round(data.a.pca$sdev[1]^2/sum(data.a.pca$sdev^2),4) * 100
pc2 <- round(data.a.pca$sdev[2]^2/sum(data.a.pca$sdev^2),4) * 100
pc3 <- round(data.a.pca$sdev[3]^2/sum(data.a.pca$sdev^2),4) * 100
pc4 <- round(data.a.pca$sdev[4]^2/sum(data.a.pca$sdev^2),4) * 100

#Extract the scores for every sample for the first four components for plotting. 
pca  = data.frame(PC1 = data.a.pca$x[,1], 
                  PC2 = data.a.pca$x[,2], 
                  PC3 = data.a.pca$x[,3], 
                  PC4 = data.a.pca$x[,4])

pca$samples = str_remove(row.names(pca), pattern = "X") %>%
  str_remove("\\..*") %>% factor(levels = c(seq(1000,10000, by = 1000), "real"))



#First, the main plot. Plot the first two components of the PCA
new_shrunk = ggplot(pca, 
                    aes(x       = PC1,
                        y       = PC2,
                        fill    = samples)) +  
  
  #Create the points and ellipses
  stat_ellipse(geom = "polygon", alpha = 1/4) +
  geom_point(col = "black", shape = 21, 
             aes(size = samples == "real")) + 
  #Adjust appearance
  
  #Adjust labels
  ggtitle("new shrunk method") + 
  xlab(paste("PC1: ", pc1,  "%", sep="")) + 
  ylab(paste("PC2: ", pc2,  "%", sep="")) +
  theme_bw() +
  theme(legend.position = 'bottom') 


old + new + new_shrunk + plot_layout(guides = 'collect') &  theme(legend.position = 'bottom') 
```

    ## Warning: Using size for a discrete variable is not advised.

    ## Too few points to calculate an ellipse

    ## Warning: Using size for a discrete variable is not advised.

    ## Too few points to calculate an ellipse

    ## Warning: Using size for a discrete variable is not advised.

    ## Too few points to calculate an ellipse

<img src="README_files/figure-gfm/fib comparison-1.png" width="100%" />

``` r
df_shr <- sCLR(res_fib %>% cbind("real" = fib)) %>%
  t() %>%
dist(., diag = T, upper = T, method = "euclidean")  %>%
  as.matrix %>%
  data.frame()

df_new <- (res_fib %>% cbind("real" = fib)) %>%
  data.frame() %>%
  getTableMeans() %>%
  t() %>%
  dist(., diag = T, upper = T, method = "euclidean")  %>%
  as.matrix %>%
  data.frame() 

df_c <- (res_fib %>% cbind("real" = fib)) %>%
  data.frame() %>%
  Tjazi::clr_c() %>%
  t() %>%
  dist(., diag = T, upper = T, method = "euclidean")  %>%
  as.matrix %>%
  data.frame() 

# df_unif <- (res_fib %>% cbind("real" = fib)) %>%
#   data.frame() %>%
#   Tjazi::clr_unif() %>%
#   t() %>%
#   dist(., diag = T, upper = T, method = "euclidean")  %>%
#   as.matrix %>%
#   data.frame() 

df_logunif <- (res_fib %>% cbind("real" = fib)) %>%
  data.frame() %>%
  Tjazi::clr_logunif() %>%
  t() %>%
  dist(., diag = T, upper = T, method = "euclidean")  %>%
  as.matrix %>%
  data.frame() 


plot_df = data.frame(constant = df_c[,"real"], 
                     new      = df_new[,"real"],
                     shrunk   = df_shr[,"real"], 
                     #unif      = df_unif[,"real"], 
                     logunif   = df_logunif[,"real"], 
                     
                     sample   = c(rep(seq(1000,10000, by = 1000), each = 100),"NA"))

plot_df %>% 
  rownames_to_column("ID") %>%
  pivot_longer(!c(ID, sample)) %>%
  filter(sample != "NA") %>%
  mutate(sample = factor(sample, levels = seq(1000,10000, by = 1000))) %>%
  #mutate(sample = as.numeric(sample)) %>%
  
  ggplot() +
  aes(x = sample, y = value, fill = name) +
  geom_boxplot() + 
  geom_point(shape = 21, position = position_dodge(0.75)) +
  theme_bw() +
  xlab("Number of counts taken") +
  ylab("Aitchison distance from ground truth") +
  theme(legend.position = 'bottom') 
```

<img src="README_files/figure-gfm/fib comparison-2.png" width="100%" />

``` r
df_shr <-  sCLR(res_fib %>% cbind("real" = fib)) %>%
  t() %>%
dist(., diag = T, upper = T, method = "euclidean")  %>%
  as.matrix %>%
  data.frame()

df_new <- (res_fib %>% cbind("real" = fib)) %>%
  data.frame() %>%
  getTableMeans() %>%
  t() %>%
  dist(., diag = T, upper = T, method = "euclidean")  %>%
  as.matrix %>%
  data.frame() 

df_c <- (res_fib %>% cbind("real" = fib)) %>%
  data.frame() %>%
  Tjazi::clr_c() %>%
  t() %>%
  dist(., diag = T, upper = T, method = "euclidean")  %>%
  as.matrix %>%
  data.frame() 

# df_unif <- (res_fib %>% cbind("real" = fib)) %>%
#   data.frame() %>%
#   Tjazi::clr_unif() %>%
#   t() %>%
#   dist(., diag = T, upper = T, method = "euclidean")  %>%
#   as.matrix %>%
#   data.frame() 
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

# 
# dist_logunif <- res_fib %>%
#   data.frame() %>%
#   Tjazi::clr_logunif() %>%
#   t() %>%
#   dist(x = ., method = "euclidean")  

dist_const <- res_fib %>%  
  data.frame() %>%
  Tjazi::clr_c() %>%
  t() %>%
  dist(x = ., method = "euclidean")  
# 
# dist_unif <- res_fib %>%
#   data.frame() %>%
#   Tjazi::clr_unif() %>%
#   t() %>%
#   dist(x = ., method = "euclidean")  

dist_shrunk <-  sCLR(res_fib) %>%
  t() %>%
  dist(x = .,method = "euclidean")

dist_new <- res_fib %>% 
  data.frame() %>%
  getTableMeans() %>%
  t() %>%
  dist(x = ., method = "euclidean")


lm_shrunk <- data.frame(col=colnames(as.matrix(dist_shrunk))[col(as.matrix(dist_shrunk))], 
                        row=rownames(as.matrix(dist_shrunk))[row(as.matrix(dist_shrunk))], 
                        dist=c(as.matrix(dist_shrunk))) %>%
  mutate(X1 = str_remove(col, "X") %>% str_remove("\\..*") %>% as.numeric(), 
         X2 = str_remove(row, "X") %>% str_remove("\\..*") %>% as.numeric())


lm_new = data.frame(col=colnames(as.matrix(dist_new))[col(as.matrix(dist_new))], 
                      row=rownames(as.matrix(dist_new))[row(as.matrix(dist_new))], 
                      dist=c(as.matrix(dist_new))) %>%
  mutate(X1 = str_remove(col, "X") %>% str_remove("\\..*") %>% as.numeric(), 
         X2 = str_remove(row, "X") %>% str_remove("\\..*") %>% as.numeric())

lm_const = data.frame(col=colnames(as.matrix(dist_const))[col(as.matrix(dist_const))], 
                      row=rownames(as.matrix(dist_const))[row(as.matrix(dist_const))], 
                      dist=c(as.matrix(dist_const))) %>%
  mutate(X1 = str_remove(col, "X") %>% str_remove("\\..*") %>% as.numeric(), 
         X2 = str_remove(row, "X") %>% str_remove("\\..*") %>% as.numeric())



lm_const %>%
  group_by(X1, X2) %>%
  summarise(dist = mean(dist)) %>%
  
  ggplot() +
  aes(x = (X1), y = (X2), label = round(dist, 3)) +
  geom_tile(aes(fill = dist)) +
  geom_label()
```

    ## `summarise()` has grouped output by 'X1'. You can override using the `.groups`
    ## argument.

![](README_files/figure-gfm/depth%20analysis-1.png)<!-- -->

``` r
lm_shrunk %>%
  group_by(X1, X2) %>%
  summarise(dist = mean(dist)) %>%
  
  ggplot() +
  aes(x = (X1), y = (X2), label = round(dist, 3)) +
  geom_tile(aes(fill = dist)) +
  geom_label()
```

    ## `summarise()` has grouped output by 'X1'. You can override using the `.groups`
    ## argument.

![](README_files/figure-gfm/depth%20analysis-2.png)<!-- -->

``` r
lm_new %>%
  group_by(X1, X2) %>%
  summarise(dist = mean(dist)) %>%
  
  ggplot() +
  aes(x = (X1), y = (X2), label = round(dist, 3)) +
  geom_tile(aes(fill = dist)) +
  geom_label()
```

    ## `summarise()` has grouped output by 'X1'. You can override using the `.groups`
    ## argument.

![](README_files/figure-gfm/depth%20analysis-3.png)<!-- -->
