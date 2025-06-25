
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
#> 1     indep      cent        L2       111.1141....    0.55               FALSE
#> 2     indep      cent        KS       0.1598, ....    0.62               FALSE
#> 3     indep        eq        L2       5.573282....    0.00                TRUE
#> 4     indep        eq        KS       0.04, 0.....    0.00                TRUE
#> 5        NP      cent        L2       24.12576....    0.00                TRUE
#> 6        NP      cent        KS       0.080000....    0.00                TRUE
#> 7        NP        eq        L2       245.0473....    0.53               FALSE
#> 8        NP        eq        KS       0.2048, ....    0.65               FALSE
 
 # Under H0
 X1 = rnorm(n)
 X2 = rnorm(n)
 result = perform_independence_test(X1, X2, nBootstrap = 100)
 result$pvals_df
#>   type_boot type_stat norm_type bootstrapped_tests pvalues theoretically_valid
#> 1     indep      cent        L2       28.85461....    0.98               FALSE
#> 2     indep      cent        KS       0.0812, ....    0.93               FALSE
#> 3     indep        eq        L2       14.26944....    0.27                TRUE
#> 4     indep        eq        KS       0.0555, ....    0.33                TRUE
#> 5        NP      cent        L2       5.300964....    0.24                TRUE
#> 6        NP      cent        KS       0.0367, ....    0.27                TRUE
#> 7        NP        eq        L2       18.28246....    0.84               FALSE
#> 8        NP        eq        KS       0.066, 0....    0.85               FALSE
```

## Slope testing in linear regression setting

``` r
library(BootstrapTests)
## basic example code

 n = 100
  # Under H1
  X_data = rgamma(n,2,3)
  result_H1 = perform_GoF_test(X_data, nBootstrap = 30)
  result_H1
#> $pvals_df
#>   type_boot type_stat param_bs bootstrapped_tests    pvalues
#> 1      null      cent       MD       1.188725.... 0.96666667
#> 2      null      cent  MD-cent       1.114725.... 0.76666667
#> 3      null        eq       MD       0.379970.... 0.00000000
#> 4      null        eq  MD-cent       0.412720.... 0.03333333
#> 5        NP      cent       MD       0.362772.... 0.13333333
#> 6        NP      cent  MD-cent       0.305665.... 0.00000000
#> 7        NP        eq       MD       0.883250.... 0.83333333
#> 8        NP        eq  MD-cent       1.074369.... 0.96666667
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
#> 0.8654417
  
   # Under H0
  X_data = rnorm(n)
  result_H0 = perform_GoF_test(X_data, nBootstrap = 30)
  result_H0
#> $pvals_df
#>   type_boot type_stat param_bs bootstrapped_tests   pvalues theoretically_valid
#> 1      null      cent       MD       0.538470.... 1.0000000               FALSE
#> 2      null      cent  MD-cent       0.460680.... 1.0000000               FALSE
#> 3      null        eq       MD       0.313343.... 0.9666667                TRUE
#> 4      null        eq  MD-cent       0.367562.... 1.0000000               FALSE
#> 5        NP      cent       MD       0.679022.... 0.9666667               FALSE
#> 6        NP      cent  MD-cent       0.549898.... 0.9666667                TRUE
#> 7        NP        eq       MD       0.717008.... 1.0000000               FALSE
#> 8        NP        eq  MD-cent       0.808679.... 1.0000000               FALSE
#> 
#> $true_stat
#>       sup 
#> 0.2627113
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
#>   type_boot type_stat param_bs bootstrapped_tests   pvalues theoretically_valid
#> 1      null      cent       MD       0.936419.... 0.9000000               FALSE
#> 2      null      cent  MD-cent       0.837811.... 0.7333333               FALSE
#> 3      null        eq       MD       0.340605.... 0.0000000                TRUE
#> 4      null        eq  MD-cent       0.504015.... 0.1333333               FALSE
#> 5        NP      cent       MD       0.778379.... 0.4000000               FALSE
#> 6        NP      cent  MD-cent       0.373715.... 0.0000000                TRUE
#> 7        NP        eq       MD       0.703700.... 0.8000000               FALSE
#> 8        NP        eq  MD-cent       1.057756.... 0.9666667               FALSE
#> 
#> $true_stat
#>       sup 
#> 0.7493941
  
   # Under H0
  X_data = rnorm(n)
  perform_GoF_test(X_data, nBootstrap = 30)
#> $pvals_df
#>   type_boot type_stat param_bs bootstrapped_tests   pvalues theoretically_valid
#> 1      null      cent       MD       0.640425.... 1.0000000               FALSE
#> 2      null      cent  MD-cent       0.576991.... 1.0000000               FALSE
#> 3      null        eq       MD       0.340854.... 0.5000000                TRUE
#> 4      null        eq  MD-cent       0.389741.... 0.8333333               FALSE
#> 5        NP      cent       MD       0.467433.... 0.8666667               FALSE
#> 6        NP      cent  MD-cent       0.363580.... 0.6333333                TRUE
#> 7        NP        eq       MD       0.431255.... 1.0000000               FALSE
#> 8        NP        eq  MD-cent       0.549495.... 1.0000000               FALSE
#> 
#> $true_stat
#>       sup 
#> 0.3934579
```

## References

To be included
