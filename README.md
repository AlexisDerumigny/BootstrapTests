
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Package BootstrapTests

<!-- badges: start -->
<!-- badges: end -->

This package BootstrapTests implements several methods to perform
bootstrap-based hypothesis testing procedures on three statistical
problems: independence testing, testing the slope in a linear regression
setting, and goodness-of-fit testing.

## Installation

You can install the development version of BootstrapTests from
[GitHub](https://github.com/) using the `devtools` package with:

``` r
# install.packages("devtools")
devtools::install_github("AlexisDerumigny/BootstrapTests")
```

## Examples

The following three subsections will showcase the use of the
‘BootstrapTests’ package for the independence testing, testing the slope
in a linear regression setting, and goodness-of-fit testing. In
particular, we will show the use of the functions:

- `perform_regression_test.R`
- `perform_independence_test.R`
- `perform_GoF_test.R`

### Independence testing

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
#> 1     indep      cent        L2       201.0342....    0.56               FALSE
#> 2     indep      cent        KS       0.1682, ....    0.87               FALSE
#> 3     indep        eq        L2       9.641677....    0.00                TRUE
#> 4     indep        eq        KS       0.048, 0....    0.00                TRUE
#> 5        NP      cent        L2       12.50736....    0.00                TRUE
#> 6        NP      cent        KS       0.0551, ....    0.00                TRUE
#> 7        NP        eq        L2       140.7076....    0.60               FALSE
#> 8        NP        eq        KS       0.1414, ....    0.88               FALSE
 
 # Under H0
 X1 = rnorm(n)
 X2 = rnorm(n)
 result = perform_independence_test(X1, X2, nBootstrap = 100)
 result$pvals_df
#>   type_boot type_stat norm_type bootstrapped_tests pvalues theoretically_valid
#> 1     indep      cent        L2       12.03751....    1.00               FALSE
#> 2     indep      cent        KS       0.0592, ....    1.00               FALSE
#> 3     indep        eq        L2       6.791147....    1.00                TRUE
#> 4     indep        eq        KS       0.0416, ....    1.00                TRUE
#> 5        NP      cent        L2       6.555012....    0.98                TRUE
#> 6        NP      cent        KS       0.028, 0....    0.98                TRUE
#> 7        NP        eq        L2       12.14011....    1.00               FALSE
#> 8        NP        eq        KS       0.044, 0....    1.00               FALSE
```

### Slope testing in linear regression setting

``` r
library(BootstrapTests)
## basic example code

 n = 100
  # Under H1
  X_data = rgamma(n,2,3)
  result_H1 = perform_GoF_test(X_data, nBootstrap = 30)
  result_H1
#> $pvals_df
#>   type_boot type_stat param_bs bootstrapped_tests   pvalues theoretically_valid
#> 1      null      cent       MD       1.030299.... 0.9666667               FALSE
#> 2      null      cent  MD-cent       0.880199.... 0.8000000               FALSE
#> 3      null        eq       MD       0.457955.... 0.0000000                TRUE
#> 4      null        eq  MD-cent       0.553973.... 0.2666667               FALSE
#> 5        NP      cent       MD       0.458079.... 0.2666667               FALSE
#> 6        NP      cent  MD-cent       0.248360.... 0.0000000                TRUE
#> 7        NP        eq       MD       0.686428.... 0.8666667               FALSE
#> 8        NP        eq  MD-cent       0.830352.... 1.0000000               FALSE
#> 
#> $true_stat
#>       sup 
#> 0.5882752
  
   # Under H0
  X_data = rnorm(n)
  result_H0 = perform_GoF_test(X_data, nBootstrap = 30)
  result_H0
#> $pvals_df
#>   type_boot type_stat param_bs bootstrapped_tests   pvalues theoretically_valid
#> 1      null      cent       MD       0.673032.... 1.0000000               FALSE
#> 2      null      cent  MD-cent       0.575452.... 0.9333333               FALSE
#> 3      null        eq       MD       0.329802.... 0.4333333                TRUE
#> 4      null        eq  MD-cent       0.501727.... 0.8000000               FALSE
#> 5        NP      cent       MD       0.446467.... 0.7000000               FALSE
#> 6        NP      cent  MD-cent       0.311159.... 0.3333333                TRUE
#> 7        NP        eq       MD       0.605169.... 0.9000000               FALSE
#> 8        NP        eq  MD-cent       0.726714.... 0.9666667               FALSE
#> 
#> $true_stat
#>       sup 
#> 0.4268942
```

### Goodness-of-fit testing

``` r
library(BootstrapTests)
## basic example code

 n = 100
  # Under H1
  X_data = rgamma(n,2,3)
  perform_GoF_test(X_data, nBootstrap = 30)
#> $pvals_df
#>   type_boot type_stat param_bs bootstrapped_tests    pvalues
#> 1      null      cent       MD       1.074937.... 1.00000000
#> 2      null      cent  MD-cent       0.980478.... 0.86666667
#> 3      null        eq       MD       0.372681.... 0.00000000
#> 4      null        eq  MD-cent       0.553987.... 0.13333333
#> 5        NP      cent       MD       0.459689.... 0.23333333
#> 6        NP      cent  MD-cent       0.395522.... 0.03333333
#> 7        NP        eq       MD       1.002338.... 0.90000000
#> 8        NP        eq  MD-cent       1.049557.... 0.96666667
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
#>     sup 
#> 0.70727
  
   # Under H0
  X_data = rnorm(n)
  perform_GoF_test(X_data, nBootstrap = 30)
#> $pvals_df
#>   type_boot type_stat param_bs bootstrapped_tests   pvalues theoretically_valid
#> 1      null      cent       MD       0.774044.... 1.0000000               FALSE
#> 2      null      cent  MD-cent       0.633241.... 1.0000000               FALSE
#> 3      null        eq       MD       0.437073.... 0.8666667                TRUE
#> 4      null        eq  MD-cent       0.627883.... 0.9666667               FALSE
#> 5        NP      cent       MD       0.392387.... 0.9333333               FALSE
#> 6        NP      cent  MD-cent       0.252147.... 0.6666667                TRUE
#> 7        NP        eq       MD       0.362972.... 1.0000000               FALSE
#> 8        NP        eq  MD-cent       0.470902.... 1.0000000               FALSE
#> 
#> $true_stat
#>      sup 
#> 0.336971
```

## References

To be included
