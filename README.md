
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
#> [1] "The local WDD estimate is -10 (8.4)"
#> [1] "The displacement WDD estimate is 0 (0)"
#> [1] "The total WDD estimate is -10 (8.4)"
#> [1] "The 90% confidence interval is -23.8 to 3.8"
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
#> Small Sample Test Object 
#> Test Type is G 
#> Statistic is: 12.5740089945434 
#> p-value is:  0.1469451  
#> Data are:  3 4 1 0 0 0 2 0 2 
#> Null probabilities are:  0.3 0.18 0.12 0.097 0.079 0.067 0.058 0.051 0.046 
#> Total permutations are:  125970
```

Here is an example checking the Poisson fit for a set of data:

``` r
x <- stats::rpois(1000,0.5)
check_pois(x,0,max(x),mean(x))
#> [1] "mean: 0.458"
#> [1] "variance: 0.462698698698699"
#>   Int Freq      PoisF     ResidF Prop      PoisD      ResidD
#> 1   0  636 632.547476  3.4525238 63.6 63.2547476  0.34525238
#> 2   1  282 289.706744 -7.7067441 28.2 28.9706744 -0.77067441
#> 3   2   71  66.342844  4.6571556  7.1  6.6342844  0.46571556
#> 4   3   10  10.128341 -0.1283409  1.0  1.0128341 -0.01283409
#> 5   4    1   1.159695 -0.1596950  0.1  0.1159695 -0.01596950
```
