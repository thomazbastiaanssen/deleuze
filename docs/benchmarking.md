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
    ##    expr       min        lq      mean    median        uq      max neval cld
    ##  sample 357.59313 367.69937 413.20411 436.23503 441.75005 481.6932   100   b
    ##  approx  80.13142  82.28118  91.35211  84.48791  86.93723 169.2589   100  a

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
    ##    expr       min        lq      mean    median        uq       max neval cld
    ##  sample 370.21982 375.40983 383.55706 380.03307 383.74056 528.63205   100   b
    ##  approx  60.91575  62.44022  64.83038  63.81019  67.16897  79.26466   100  a
