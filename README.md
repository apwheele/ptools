
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ptools

<!-- badges: start -->
<!-- badges: end -->

The library ptools is a set of helper functions I have used over time to
help with analyzing count data, e.g. crime counts per month.

## Installation

To install the most recent version from CRAN, it is simply:

    install.packages('ptools')

You can install the current version on github using devtools:

    library(devtools)
    install_github("apwheele/ptools", build_vignettes = TRUE)
    library(ptools) # Hopefully works!

## Examples

Here is checking the difference in two Poisson means using an e-test:

``` r
library(ptools)
e_test(6,2)
#> [1] 0.1748748
```

Here is the Wheeler & Ratcliffe WDD test (see `help(wdd)` for academic
references):

``` r
wdd(c(20,20),c(20,10))
#> 
#>  The local WDD estimate is -10 (8.4)
#>  The displacement WDD estimate is 0 (0)
#>  The total WDD estimate is -10 (8.4)
#>  The 90% confidence interval is -23.8 to 3.8
#>    Est_Local     SE_Local Est_Displace  SE_Displace    Est_Total     SE_Total 
#>   -10.000000     8.366600     0.000000     0.000000   -10.000000     8.366600 
#>            Z        LowCI       HighCI 
#>    -1.195229   -23.761833     3.761833
```

Here is a quick example applying a small sample Benford’s analysis:

``` r
# Null probs for Benfords law
f <- 1:9
p_fd <- log10(1 + (1/f)) #first digit probabilities
# Example 12 purchases on my credit card
purch <- c( 72.00,
           328.36,
            11.57,
            90.80,
            21.47,
             7.31,
             9.99,
             2.78,
            10.17,
             2.96,
            27.92,
            14.49)
#artificial numbers, 72.00 is parking at DFW, 9.99 is Netflix
fdP <- substr(format(purch,trim=TRUE),1,1)
totP <- table(factor(fdP, levels=paste(f)))
resG_P <- small_samptest(d=totP,p=p_fd,type="G")
print(resG_P) # I have a nice print function
#> 
#>  Small Sample Test Object 
#>  Test Type is G 
#>  Statistic is: 12.5740089945434 
#>  p-value is:  0.1469451  
#>  Data are:  3 4 1 0 0 0 2 0 2 
#>  Null probabilities are:  0.3 0.18 0.12 0.097 0.079 0.067 0.058 0.051 0.046 
#>  Total permutations are:  125970
```

Here is an example checking the Poisson fit for a set of data:

``` r
x <- rpois(1000,0.5)
check_pois(x,0,max(x),mean(x))
#> 
#>  mean: 0.541 variance: 0.532851851851852
#>   Int Freq      PoisF      ResidF Prop      PoisD       ResidD
#> 1   0  579 582.165795 -3.16579540 57.9 58.2165795 -0.316579540
#> 2   1  321 314.951695  6.04830469 32.1 31.4951695  0.604830469
#> 3   2   82  85.194434 -3.19443358  8.2  8.5194434 -0.319443358
#> 4   3   16  15.363396  0.63660381  1.6  1.5363396  0.063660381
#> 5   4    2   2.077899 -0.07789933  0.2  0.2077899 -0.007789933
```

Here is an example extracting out near repeat strings (this is improved
version [from an old blog
post](https://andrewpwheeler.com/2017/04/12/identifying-near-repeat-crime-strings-in-r-or-python/)
using kdtrees):

``` r
# Not quite 15k rows for burglaries from motor vehicles
bmv <- read.csv('https://dl.dropbox.com/s/bpfd3l4ueyhvp7z/TheftFromMV.csv?dl=0')
print(Sys.time()) 
#> [1] "2023-02-07 09:53:24 EST"
BigStrings <- near_strings2(dat=bmv,id='incidentnu',x='xcoordinat',
                            y='ycoordinat',tim='DateInt',DistThresh=1000,TimeThresh=3)
print(Sys.time()) #very fast, only a few seconds on my machine
#> [1] "2023-02-07 09:53:25 EST"
print(head(BigStrings))
#>             CompId CompNum
#> 000036-2015      1       1
#> 000113-2015      2       1
#> 000192-2015      3       1
#> 000251-2015      4       1
#> 000360-2015      5       1
#> 000367-2015      6       1
```

## Contributing

Always feel free to contribute either directly on Github, or email me
with thoughts/suggestions. For citations for functions used, feel free
to cite the original papers I reference in the functions instead of the
package directly.

Things on the todo list:

-   Tests for spatial feature engineering
-   Figure out [no long doubles
    issues](https://andrewpwheeler.com/2022/07/22/my-journey-submitting-to-cran/)
    for small sample tests
-   Conversion so functions can take both sp/sf objects
-   Poisson z-score and weekly aggregation functions
-   Potential geo functions
    -   HDR raster
    -   Leaflet helpers
