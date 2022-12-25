``` r
#devtools::install_github("thomazbastiaanssen/volatility")
library(tidyverse)
library(deleuze)
library(patchwork)
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
| observed     | -8.645481 | 0.0854143 |
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

For benchmarking click
[Here](https://github.com/thomazbastiaanssen/deleuze/blob/main/docs/benchmarking.md)
(put on its own page as it’s slow to knit)

``` r
#real sampled data
data = volatility::vola_genus_table


a = data %>% 
  Tjazi::clr_c() %>%
  t 

b = data %>% 
  getTableMeans() %>%
  t

#Apply the base R principal component analysis function on our CLR-transformed data.

data.a.pca = rbind(a, b) %>% 
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

pca$Type = rep(c("lognorm", "estimated"), each = 120 )
pca$ID   = paste0("s",1:120)



#First, the main plot. Plot the first two components of the PCA
ggplot(pca, aes(x       = PC1,
                y       = PC2,
                fill    = Type)) +  
  
  #Create the points and ellipses
  geom_path(aes(group = ID), col = "black") +
  geom_point(size=3, col = "black", shape = 21) + 
  #Adjust appearance
  
  #Adjust labels
  ggtitle("lognorm vs new method") + 
  xlab(paste("PC1: ", pc1,  "%", sep="")) + 
  ylab(paste("PC2: ", pc2,  "%", sep="")) +
  theme_bw() +
  theme(legend.position = 'bottom') 
```

<img src="README_files/figure-gfm/comparing CLR to approx entire table-1.png" width="100%" />

``` r
plot(x = c(unlist(Tjazi::clr_c(data))),
     y = c(getTableMeans(data)))
```

<img src="README_files/figure-gfm/comparing CLR to approx entire table-2.png" width="100%" />
