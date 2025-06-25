
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
particular, we will show the use of the functions:”

- ‘perform_regression_test.R’
- ‘perform_independence_test.R’
- ‘perform_GoF_test.R’

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
#> 1     indep      cent        L2       121.1739....    0.66               FALSE
#> 2     indep      cent        KS       0.1857, ....    0.76               FALSE
#> 3     indep        eq        L2       6.107756....    0.00                TRUE
#> 4     indep        eq        KS       0.052, 0....    0.00                TRUE
#> 5        NP      cent        L2       5.147545....    0.00                TRUE
#> 6        NP      cent        KS       0.053, 0....    0.00                TRUE
#> 7        NP        eq        L2       90.10248....    0.55               FALSE
#> 8        NP        eq        KS       0.1597, ....    0.77               FALSE
 
 # Under H0
 X1 = rnorm(n)
 X2 = rnorm(n)
 result = perform_independence_test(X1, X2, nBootstrap = 100)
 result$pvals_df
#>   type_boot type_stat norm_type bootstrapped_tests pvalues theoretically_valid
#> 1     indep      cent        L2       12.70475....    1.00               FALSE
#> 2     indep      cent        KS       0.0536, ....    1.00               FALSE
#> 3     indep        eq        L2       5.176731....    0.62                TRUE
#> 4     indep        eq        KS       0.0436, ....    0.57                TRUE
#> 5        NP      cent        L2       4.805981....    0.59                TRUE
#> 6        NP      cent        KS       0.0419, ....    0.69                TRUE
#> 7        NP        eq        L2       10.30805....    0.98               FALSE
#> 8        NP        eq        KS       0.0524, ....    0.98               FALSE
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
#> 1      null      cent       MD       1.181773.... 0.9666667               FALSE
#> 2      null      cent  MD-cent       0.968614.... 0.7000000               FALSE
#> 3      null        eq       MD       0.443629.... 0.0000000                TRUE
#> 4      null        eq  MD-cent       0.696895.... 0.2000000               FALSE
#> 5        NP      cent       MD       0.237417.... 0.2333333               FALSE
#> 6        NP      cent  MD-cent       0.201939.... 0.0000000                TRUE
#> 7        NP        eq       MD       0.845134.... 0.8333333               FALSE
#> 8        NP        eq  MD-cent       0.907363.... 0.9666667               FALSE
#> 
#> $true_stat
#>      sup 
#> 0.738144
  
   # Under H0
  X_data = rnorm(n)
  result_H0 = perform_GoF_test(X_data, nBootstrap = 30)
  result_H0
#> $pvals_df
#>   type_boot type_stat param_bs bootstrapped_tests   pvalues theoretically_valid
#> 1      null      cent       MD       0.537862.... 1.0000000               FALSE
#> 2      null      cent  MD-cent       0.476629.... 1.0000000               FALSE
#> 3      null        eq       MD       0.334839.... 1.0000000                TRUE
#> 4      null        eq  MD-cent       0.422519.... 1.0000000               FALSE
#> 5        NP      cent       MD       0.544356.... 1.0000000               FALSE
#> 6        NP      cent  MD-cent       0.433659.... 0.9666667                TRUE
#> 7        NP        eq       MD       0.572083.... 1.0000000               FALSE
#> 8        NP        eq  MD-cent       0.691304.... 1.0000000               FALSE
#> 
#> $true_stat
#>       sup 
#> 0.2576449
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
#>   type_boot type_stat param_bs bootstrapped_tests   pvalues theoretically_valid
#> 1      null      cent       MD       1.034243.... 0.9333333               FALSE
#> 2      null      cent  MD-cent       0.758614.... 0.6666667               FALSE
#> 3      null        eq       MD       0.425485.... 0.0000000                TRUE
#> 4      null        eq  MD-cent       0.746183.... 0.1000000               FALSE
#> 5        NP      cent       MD       0.569392.... 0.2666667               FALSE
#> 6        NP      cent  MD-cent       0.359043.... 0.0000000                TRUE
#> 7        NP        eq       MD       0.871202.... 0.8000000               FALSE
#> 8        NP        eq  MD-cent       1.081597.... 0.9333333               FALSE
#> 
#> $true_stat
#>       sup 
#> 0.7503946
  
   # Under H0
  X_data = rnorm(n)
  perform_GoF_test(X_data, nBootstrap = 30)
#> $pvals_df
#>   type_boot type_stat param_bs bootstrapped_tests   pvalues theoretically_valid
#> 1      null      cent       MD       0.553140.... 1.0000000               FALSE
#> 2      null      cent  MD-cent       0.506466.... 1.0000000               FALSE
#> 3      null        eq       MD       0.378099.... 0.9000000                TRUE
#> 4      null        eq  MD-cent       0.402968.... 0.9666667               FALSE
#> 5        NP      cent       MD       0.461855.... 0.9666667               FALSE
#> 6        NP      cent  MD-cent       0.407739.... 0.9333333                TRUE
#> 7        NP        eq       MD       0.531333.... 1.0000000               FALSE
#> 8        NP        eq  MD-cent       0.576894.... 1.0000000               FALSE
#> 
#> $true_stat
#>      sup 
#> 0.313057
```

## References

To be included
