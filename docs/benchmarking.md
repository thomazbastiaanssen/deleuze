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
library(microbenchmark)
```

    ## Loading required package: microbenchmarkCore

    ## Registered S3 methods overwritten by 'microbenchmark':
    ##   method                 from              
    ##   print.microbenchmark   microbenchmarkCore
    ##   summary.microbenchmark microbenchmarkCore

``` r
mbm <- microbenchmark(
               "sample" = {
                 b <- sampleCLR(samples = 10000, data)
                 },
               "approx" = {
                 b <- sampleCLRApprox(samples = 10000, data)
                 })

mbm
```

    ## Unit: milliseconds
    ##    expr      min       lq     mean   median       uq      max neval cld
    ##  sample 757.8101 786.7227 832.8274 851.6246 870.4794 965.9022   100   b
    ##  approx 169.9546 176.5457 197.2054 179.3692 188.3612 321.7788   100  a

``` r
library(microbenchmark)
library(Tjazi)

data = volatility::vola_genus_table

mbm <- microbenchmark(
               "sample" = {
                 b <- Tjazi::clr_c(data)
                 },
               "approx" = {
                 b <- getTableMeans(data)
                 })

mbm
```

    ## Unit: milliseconds
    ##    expr      min       lq     mean   median       uq       max neval cld
    ##  sample 789.8281 824.2436 844.4953 836.7131 855.1128 1053.7719   100   b
    ##  approx 129.7679 134.4234 138.7461 137.7347 141.2531  174.7399   100  a
