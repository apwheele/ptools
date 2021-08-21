
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ptools

<!-- badges: start -->

<!-- badges: end -->

The library ptools is a set of helper functions I have used over time to
help with analyzing count data, e.g. crime counts per month.

## Installation

Hopefully in the future this will be on CRAN, but in the meantime, you
can install this via devtools:

    library(devtools)
    install_github("apwheele/ptools")
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
print(resG_P)
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
x <- stats::rpois(1000,0.5)
check_pois(x,0,max(x),mean(x))
#> [1] "mean: 0.502"
#> [1] "variance: 0.510506506506507"
#>   Int Freq       PoisF     ResidF Prop       PoisD      ResidD
#> 1   0  607 605.3188106  1.6811894 60.7 60.53188106  0.16811894
#> 2   1  301 303.8700429 -2.8700429 30.1 30.38700429 -0.28700429
#> 3   2   78  76.2713808  1.7286192  7.8  7.62713808  0.17286192
#> 4   3   12  12.7627444 -0.7627444  1.2  1.27627444 -0.07627444
#> 5   4    1   1.6017244 -0.6017244  0.1  0.16017244 -0.06017244
#> 6   5    1   0.1608131  0.8391869  0.1  0.01608131  0.08391869
```

## Contributing

Always feel free to contribute either directly on Github, or email me
with thoughts/suggestions. For citations for functions used, feel free
to cite the original papers I reference in the functions instead of the
package directly.

Things on the todo list:

  - unit tests
  - github actions
  - Poisson z-score
  - Contours for pre/post Poisson {NYC Data)
  - funnel charts
  - SPPT with already aggregated data, SPPT power
  - PAI/RRI functions
  - ?Potentially geo functions?
  - create grid
  - Theissen helpers
  - HDR raster
  - Near spacetime
  - Hexagons
  - Leaflet helpers
