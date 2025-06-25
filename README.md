
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
n = 100

# Under H1
X1 = rnorm(n)
X2 = X1 + rnorm(n)
result = perform_independence_test(X1, X2, nBootstrap = 100)
result$pvals_df
#>   type_boot type_stat norm_type bootstrapped_tests pvalues theoretically_valid
#> 1     indep      cent        L2       145.3632....    0.64               FALSE
#> 2     indep      cent        KS       0.1598, ....    0.78               FALSE
#> 3     indep        eq        L2       8.921129....    0.00                TRUE
#> 4     indep        eq        KS       0.054800....    0.00                TRUE
#> 5        NP      cent        L2       6.202671....    0.00                TRUE
#> 6        NP      cent        KS       0.0422, ....    0.00                TRUE
#> 7        NP        eq        L2       78.30805....    0.48               FALSE
#> 8        NP        eq        KS       0.1253, ....    0.64               FALSE

# Under H0
X1 = rnorm(n)
X2 = rnorm(n)
result = perform_independence_test(X1, X2, nBootstrap = 100)
result$pvals_df
#>   type_boot type_stat norm_type bootstrapped_tests pvalues theoretically_valid
#> 1     indep      cent        L2       15.19441....    1.00               FALSE
#> 2     indep      cent        KS       0.059, 0....    1.00               FALSE
#> 3     indep        eq        L2       9.301236....    0.63                TRUE
#> 4     indep        eq        KS       0.0405, ....    0.83                TRUE
#> 5        NP      cent        L2       7.203079....    0.60                TRUE
#> 6        NP      cent        KS       0.042299....    0.83                TRUE
#> 7        NP        eq        L2       16.94978....    0.98               FALSE
#> 8        NP        eq        KS       0.062, 0....    1.00               FALSE
```

### Slope testing in linear regression setting

``` r
library(BootstrapTests)

n = 100

# Under H1
X_data = rnorm(n)
Y_data =  X_data + rnorm(n)   #Y = X + epsilon
result_H1 = perform_regression_test(X_data, Y_data, nBootstrap = 30)
result_H1
#> $pvals_df
#>                type_boot type_stat   pvalues bootstrapped_tests
#> 1               indep_bs      cent 0.4333333       8.646800....
#> 2               indep_bs        eq 0.0000000       0.297259....
#> 3           empirical_bs      cent 0.0000000       1.591860....
#> 4           empirical_bs        eq 0.4666667       6.757681....
#> 5                 res_bs      cent 0.0000000       0.594338....
#> 6                 res_bs        eq 0.4333333       7.755202....
#> 7  fixed_design_bs_Hnull      cent 0.5000000       9.070417....
#> 8  fixed_design_bs_Hnull        eq 0.0000000       0.720876....
#> 9        fixed_design_bs      cent 0.0000000       0.272685....
#> 10       fixed_design_bs        eq 0.4333333       8.076855....
#> 11        hybrid_null_bs      cent 0.5666667       9.706460....
#> 12        hybrid_null_bs        eq 0.0000000       1.356919....
#> 
#> $true_stat
#> [1] 8.349541

# Under H0
X_data = rnorm(n)
Y_data =  rep(1, n)  #these values are exactly constant (as b = 0 under H0)
result_H0 = perform_regression_test(X_data, Y_data, nBootstrap = 30)
result_H0
#> $pvals_df
#>                type_boot type_stat   pvalues bootstrapped_tests
#> 1               indep_bs      cent 0.3333333       5.304485....
#> 2               indep_bs        eq 0.3666667       1.823906....
#> 3           empirical_bs      cent 0.6666667       1.063051....
#> 4           empirical_bs        eq 0.4333333       7.078092....
#> 5                 res_bs      cent 0.4000000       1.717198....
#> 6                 res_bs        eq 0.5000000       5.366275....
#> 7  fixed_design_bs_Hnull      cent 0.3333333       1.707789....
#> 8  fixed_design_bs_Hnull        eq 0.3333333       6.307126....
#> 9        fixed_design_bs      cent 0.3000000       1.606457....
#> 10       fixed_design_bs        eq 0.7333333       3.377318....
#> 11        hybrid_null_bs      cent 0.6000000       3.245884....
#> 12        hybrid_null_bs        eq 0.4333333       1.475023....
#> 
#> $true_stat
#> [1] 1.770861e-15
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
#> 1      null      cent       MD       0.659659.... 1.00000000
#> 2      null      cent  MD-cent       0.650906.... 0.90000000
#> 3      null        eq       MD       0.448813.... 0.00000000
#> 4      null        eq  MD-cent       0.571215.... 0.36666667
#> 5        NP      cent       MD       0.556913.... 0.30000000
#> 6        NP      cent  MD-cent       0.277839.... 0.03333333
#> 7        NP        eq       MD       0.738276.... 0.93333333
#> 8        NP        eq  MD-cent       0.898623.... 1.00000000
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
#> 0.6216888
  
   # Under H0
  X_data = rnorm(n)
  perform_GoF_test(X_data, nBootstrap = 30)
#> $pvals_df
#>   type_boot type_stat param_bs bootstrapped_tests   pvalues theoretically_valid
#> 1      null      cent       MD       0.688900.... 1.0000000               FALSE
#> 2      null      cent  MD-cent       0.643788.... 0.9666667               FALSE
#> 3      null        eq       MD       0.339896.... 0.3333333                TRUE
#> 4      null        eq  MD-cent       0.401846.... 0.9000000               FALSE
#> 5        NP      cent       MD       0.518145.... 0.9000000               FALSE
#> 6        NP      cent  MD-cent       0.327631.... 0.3333333                TRUE
#> 7        NP        eq       MD       0.501489.... 1.0000000               FALSE
#> 8        NP        eq  MD-cent       0.668045.... 1.0000000               FALSE
#> 
#> $true_stat
#>       sup 
#> 0.4019228
```

## References

To be included
