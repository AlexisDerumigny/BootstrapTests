
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

The following three subsections will showcase the use of the package for
the independence testing, testing the slope in a linear regression
setting, and goodness-of-fit testing. In particular, we will show the
use of the functions:”

- 

- 

- 

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
#> 1     indep      cent        L2       113.0720....    0.56               FALSE
#> 2     indep      cent        KS       0.174, 0....    0.64               FALSE
#> 3     indep        eq        L2       6.109411....    0.00                TRUE
#> 4     indep        eq        KS       0.0472, ....    0.00                TRUE
#> 5        NP      cent        L2       8.39059,....    0.00                TRUE
#> 6        NP      cent        KS       0.0547, ....    0.00                TRUE
#> 7        NP        eq        L2       71.31520....    0.52               FALSE
#> 8        NP        eq        KS       0.1436, ....    0.63               FALSE
 
 # Under H0
 X1 = rnorm(n)
 X2 = rnorm(n)
 result = perform_independence_test(X1, X2, nBootstrap = 100)
 result$pvals_df
#>   type_boot type_stat norm_type bootstrapped_tests pvalues theoretically_valid
#> 1     indep      cent        L2       13.34477....    1.00               FALSE
#> 2     indep      cent        KS       0.0646, ....    0.91               FALSE
#> 3     indep        eq        L2       9.747784....    0.87                TRUE
#> 4     indep        eq        KS       0.065, 0....    0.45                TRUE
#> 5        NP      cent        L2       16.22359....    0.74                TRUE
#> 6        NP      cent        KS       0.0526, ....    0.36                TRUE
#> 7        NP        eq        L2       26.21712....    1.00               FALSE
#> 8        NP        eq        KS       0.068, 0....    0.90               FALSE
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
#> 1      null      cent       MD       0.968112.... 1.0000000               FALSE
#> 2      null      cent  MD-cent       0.852567.... 0.7666667               FALSE
#> 3      null        eq       MD       0.429253.... 0.0000000                TRUE
#> 4      null        eq  MD-cent       0.596690.... 0.3000000               FALSE
#> 5        NP      cent       MD       0.469080.... 0.3333333               FALSE
#> 6        NP      cent  MD-cent       0.312553.... 0.0000000                TRUE
#> 7        NP        eq       MD       0.670788.... 0.8000000               FALSE
#> 8        NP        eq  MD-cent       0.839742.... 1.0000000               FALSE
#> 
#> $true_stat
#>       sup 
#> 0.6882148
  
   # Under H0
  X_data = rnorm(n)
  result_H0 = perform_GoF_test(X_data, nBootstrap = 30)
  result_H0
#> $pvals_df
#>   type_boot type_stat param_bs bootstrapped_tests   pvalues theoretically_valid
#> 1      null      cent       MD       0.542242.... 1.0000000               FALSE
#> 2      null      cent  MD-cent       0.507518.... 1.0000000               FALSE
#> 3      null        eq       MD       0.419064.... 0.9333333                TRUE
#> 4      null        eq  MD-cent       0.451872.... 1.0000000               FALSE
#> 5        NP      cent       MD       0.485994.... 0.9333333               FALSE
#> 6        NP      cent  MD-cent       0.417679.... 0.8000000                TRUE
#> 7        NP        eq       MD       0.501514.... 0.9666667               FALSE
#> 8        NP        eq  MD-cent       0.572264.... 1.0000000               FALSE
#> 
#> $true_stat
#>       sup 
#> 0.3388221
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
#> 1      null      cent       MD       0.952194.... 0.9000000               FALSE
#> 2      null      cent  MD-cent       0.909121.... 0.7333333               FALSE
#> 3      null        eq       MD       0.320805.... 0.0000000                TRUE
#> 4      null        eq  MD-cent       0.375722.... 0.1333333               FALSE
#> 5        NP      cent       MD       0.418689.... 0.2000000               FALSE
#> 6        NP      cent  MD-cent       0.322659.... 0.0000000                TRUE
#> 7        NP        eq       MD       0.692263.... 0.4666667               FALSE
#> 8        NP        eq  MD-cent       0.763360.... 0.8000000               FALSE
#> 
#> $true_stat
#>       sup 
#> 0.8126593
  
   # Under H0
  X_data = rnorm(n)
  perform_GoF_test(X_data, nBootstrap = 30)
#> $pvals_df
#>   type_boot type_stat param_bs bootstrapped_tests    pvalues
#> 1      null      cent       MD       0.940769.... 0.90000000
#> 2      null      cent  MD-cent       0.715179.... 0.86666667
#> 3      null        eq       MD       0.419008.... 0.10000000
#> 4      null        eq  MD-cent       0.734183.... 0.40000000
#> 5        NP      cent       MD       0.382465.... 0.40000000
#> 6        NP      cent  MD-cent       0.285965.... 0.03333333
#> 7        NP        eq       MD       0.645726.... 0.93333333
#> 8        NP        eq  MD-cent       0.717268.... 1.00000000
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
#> 0.5304876
```

## References

To be included
