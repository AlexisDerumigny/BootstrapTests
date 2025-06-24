
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
#> 1     indep      cent        L2       114.1842....    0.57               FALSE
#> 2     indep      cent        KS       0.1277, ....    0.69               FALSE
#> 3     indep        eq        L2       7.168031....    0.00                TRUE
#> 4     indep        eq        KS       0.0448, ....    0.00                TRUE
#> 5        NP      cent        L2       5.584801....    0.00                TRUE
#> 6        NP      cent        KS       0.0389, ....    0.00                TRUE
#> 7        NP        eq        L2       87.11525....    0.45               FALSE
#> 8        NP        eq        KS       0.1303, ....    0.65               FALSE
 
 # Under H0
 X1 = rnorm(n)
 X2 = rnorm(n)
 result = perform_independence_test(X1, X2, nBootstrap = 100)
 result$pvals_df
#>   type_boot type_stat norm_type bootstrapped_tests pvalues theoretically_valid
#> 1     indep      cent        L2       14.48551....    1.00               FALSE
#> 2     indep      cent        KS       0.0625, ....    0.97               FALSE
#> 3     indep        eq        L2       6.966453....    0.93                TRUE
#> 4     indep        eq        KS       0.0545, ....    0.47                TRUE
#> 5        NP      cent        L2       11.96466....    0.89                TRUE
#> 6        NP      cent        KS       0.064, 0....    0.43                TRUE
#> 7        NP        eq        L2       14.95260....    1.00               FALSE
#> 8        NP        eq        KS       0.0582, ....    0.89               FALSE
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
  perform_GoF_test(X_data, nBootstrap = 30)
#> $pvals_df
#>   type_boot type_stat param_bs bootstrapped_tests    pvalues
#> 1      null      cent       MD       1.022564.... 0.96666667
#> 2      null      cent  MD-cent       0.953210.... 0.73333333
#> 3      null        eq       MD       0.405204.... 0.00000000
#> 4      null        eq  MD-cent       0.616814.... 0.06666667
#> 5        NP      cent       MD       0.861836.... 0.00000000
#> 6        NP      cent  MD-cent       0.403206.... 0.00000000
#> 7        NP        eq       MD       1.027994.... 0.73333333
#> 8        NP        eq  MD-cent       1.265620.... 0.93333333
#>   theoretically_valid
#> 1               FALSE
#> 2               FALSE
#> 3                TRUE
#> 4               FALSE
#> 5               FALSE
#> 6                TRUE
#> 7               FALSE
#> 8               FALSE
#> 
#> $true_stat
#>       sup 
#> 0.9169471
  
   # Under H0
  X_data = rnorm(n)
  perform_GoF_test(X_data, nBootstrap = 30)
#> $pvals_df
#>   type_boot type_stat param_bs bootstrapped_tests   pvalues theoretically_valid
#> 1      null      cent       MD       0.659660.... 1.0000000               FALSE
#> 2      null      cent  MD-cent       0.561206.... 1.0000000               FALSE
#> 3      null        eq       MD       0.502760.... 0.8333333                TRUE
#> 4      null        eq  MD-cent       0.655867.... 0.9666667               FALSE
#> 5        NP      cent       MD       0.531751.... 1.0000000               FALSE
#> 6        NP      cent  MD-cent       0.442700.... 0.8666667                TRUE
#> 7        NP        eq       MD       0.630533.... 1.0000000               FALSE
#> 8        NP        eq  MD-cent       0.722202.... 1.0000000               FALSE
#> 
#> $true_stat
#>       sup 
#> 0.3277635
```

## References

To be included
