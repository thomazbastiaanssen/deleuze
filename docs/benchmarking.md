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
    ##    expr       min       lq      mean    median        uq      max neval cld
    ##  sample 359.36411 367.9406 410.07523 436.00359 440.79826 466.8341   100   b
    ##  approx  80.15271  83.2360  96.59232  84.61616  88.47381 164.5329   100  a