
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Package BootstrapTests

<!-- badges: start -->
<!-- badges: end -->

This package BootstrapTests implements several methods to perform
bootstrap-based hypothesis testing procedures on three statistical
problems: independence testing, testing the slope in a linear regression
setting and goodness-of-fit testing.

## Installation

You can install the development version of BootstrapTests from
[GitHub](https://github.com/) using the `devtools` package with:

``` r
# install.packages("devtools")
devtools::install_github("AlexisDerumigny/BootstrapTests")
```

## Examples

## Independence testing

``` r
library(BootstrapTests)
## basic example code
 n = 100

 # Under H1
 X1 = rnorm(n)
 X2 = X1 + rnorm(n)
 result = perform_independence_test(X1, X2, nBootstrap = 100)
 result$pvals_df
#>   type_boot type_stat norm_type bootstrapped_tests pvalues theoretically_valid
#> 1     indep      cent        L2       102.9971....    0.54               FALSE
#> 2     indep      cent        KS       0.175, 0....    0.64               FALSE
#> 3     indep        eq        L2       3.648968....    0.00                TRUE
#> 4     indep        eq        KS       0.04, 0.....    0.00                TRUE
#> 5        NP      cent        L2       7.893897....    0.00                TRUE
#> 6        NP      cent        KS       0.0419, ....    0.00                TRUE
#> 7        NP        eq        L2       123.8245....    0.60               FALSE
#> 8        NP        eq        KS       0.1832, ....    0.71               FALSE
 
 # Under H0
 X1 = rnorm(n)
 X2 = rnorm(n)
 result = perform_independence_test(X1, X2, nBootstrap = 100)
 result$pvals_df
#>   type_boot type_stat norm_type bootstrapped_tests pvalues theoretically_valid
#> 1     indep      cent        L2       10.87988....    1.00               FALSE
#> 2     indep      cent        KS       0.058500....    0.93               FALSE
#> 3     indep        eq        L2       7.838783....    0.65                TRUE
#> 4     indep        eq        KS       0.0474, ....    0.41                TRUE
#> 5        NP      cent        L2       5.675123....    0.49                TRUE
#> 6        NP      cent        KS       0.0517, ....    0.47                TRUE
#> 7        NP        eq        L2       7.421926....    0.99               FALSE
#> 8        NP        eq        KS       0.0563, ....    0.99               FALSE
```

## Slope testing in linear regression setting

``` r
library(BootstrapTests)
## basic example code

 n = 100
  # Under H1
  X_data = rgamma(n,2,3)
  result = perform_GoF_test(X_data, nBootstrap = 30)
  
   # Under H0
  X_data = rnorm(n)
  result = perform_GoF_test(X_data, nBootstrap = 30)
```

## Goodness-of-fit testing

``` r
library(BootstrapTests)
## basic example code

 n = 100
  # Under H1
  X_data = rgamma(n,2,3)
  result = perform_GoF_test(X_data, nBootstrap = 30)
  
   # Under H0
  X_data = rnorm(n)
  result = perform_GoF_test(X_data, nBootstrap = 30)
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
