
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
‘BootstrapTests’ package for the independence hypothesis testing,
testing whether or not the slope is zero in a linear regression setting,
and goodness-of-fit testing. In particular, we will show the use of the
functions that implement these:

- `perform_independence_test.R`
- `perform_regression_test.R`
- `perform_GoF_test.R`

### Independence testing

We perform a hypothesis test of statistical independence by means of
bootstrapping. The null hypothesis is that of independence between the
two random variables, versus the alternative of dependence between them.
This procedure gives a total of 8 combinations of bootstrap resampling
schemes (nonparametric and independent), test statistics (centered and
equivalent), and Kolmogorov-Smirnov or L2-type of true test statistic.
This function gives the corresponding p-values, the true test statistic
and the bootstrap-version test statistics. The default (and valid)
method implemented in this function is the null bootstrap, together with
the equivalent test statistic and Kolmogorov-Smirnov test statistic. Via
the argument, the user can specify other bootstrap resampling schemes
and test statistics.

``` r
library(BootstrapTests)
n <- 100

# Under H1
X1 <- rnorm(n)
X2 <- X1 + rnorm(n)
result <- perform_independence_test(
  X1, X2, nBootstrap = 100,
  bootstrapOptions = list(type_boot = "indep",
                          type_stat = "eq",
                          norm_type = "KS") )
print(result)
#>          🎯Bootstrap Independence Test Results 🎯
#>  ======================================================== 
#> 
#> Performed test:
#>   Bootstrap type           : indep
#>   Bootstrap repetitions    : 100
#>   Type of test statistic   : eq
#>   Type of norm used        : KS
#>   p-value                  : 0
#>   True test statistic      : 0.1595
plot(result)
```

<img src="man/figures/README-example independence-1.png" width="100%" />

``` r

# Under H0
X1 <- rnorm(n)
X2 <- rnorm(n)
result <- perform_independence_test(X1, X2)
print(result)
#>          🎯Bootstrap Independence Test Results 🎯
#>  ======================================================== 
#> 
#> Performed test:
#>   Bootstrap type           : indep
#>   Bootstrap repetitions    : 100
#>   Type of test statistic   : eq
#>   Type of norm used        : KS
#>   p-value                  : 0.25
#>   True test statistic      : 0.0620
plot(result)
```

<img src="man/figures/README-example independence-2.png" width="100%" />

``` r


# Showing all theoretically valid options:
result_valid <- perform_independence_test(X1, X2, bootstrapOptions = "all")
print(result_valid)
#>          🎯Bootstrap Independence Test Results 🎯
#>  ======================================================== 
#> 
#> All test results:
#> 
#>  type_boot type_stat norm_type bootstrapped_tests pvalues theoretically_valid
#>      indep        eq        L2       5.614498....    0.14                TRUE
#>      indep        eq        KS       0.0456, ....    0.17                TRUE
#>         NP      cent        L2       5.501166....    0.13                TRUE
#>         NP      cent        KS       0.047, 0....    0.21                TRUE
#>  ci_upper_95 ci_upper_99
#>    23.389507   25.734951
#>     0.073005    0.086023
#>    21.958640   25.743274
#>     0.072925    0.080827
#> 
#> True test statistics:
#>      L2      KS 
#> 16.9997  0.0620

# Showing all bootstrap combinations, including invalid options:
result_invalid <- perform_independence_test(X1, X2, bootstrapOptions = "all and also invalid")
#> Warning in perform_independence_test(X1, X2, bootstrapOptions = "all and also
#> invalid"): Using 'all and also invalid' as bootstrapOptions is not recommended.
#> This will return all theoretically valid and invalid combinations of bootstrap
#> resampling schemes, test statistics, and norms. Please use with caution.
print(result_invalid)
#>          🎯Bootstrap Independence Test Results 🎯
#>  ======================================================== 
#> 
#> All test results:
#> 
#>  type_boot type_stat norm_type bootstrapped_tests pvalues theoretically_valid
#>      indep      cent        L2       54.0208,....    0.85               FALSE
#>      indep      cent        KS       0.1086, ....    0.92               FALSE
#>      indep        eq        L2       18.25465....    0.07                TRUE
#>      indep        eq        KS       0.058, 0....    0.11                TRUE
#>         NP      cent        L2       5.979504....    0.14                TRUE
#>         NP      cent        KS       0.0379, ....    0.18                TRUE
#>         NP        eq        L2       20.45643....    0.89               FALSE
#>         NP        eq        KS       0.074, 0....    0.91               FALSE
#>  ci_upper_95 ci_upper_99
#>    43.576013   56.420725
#>     0.108030    0.112460
#>    17.645728   21.958935
#>     0.068060    0.080064
#>    21.241101   27.681720
#>     0.073845    0.080874
#>    45.224090   48.175549
#>     0.100105    0.111205
#> 
#> True test statistics:
#>      L2      KS 
#> 16.9997  0.0620
```

### Slope testing in linear regression setting

This function performs a bootstrap regression test for given data X,Y.
The null hypothesis corresponds of a slope coefficient of zero, versus
the alternative hypothesis of a non-zero slope coefficient. It uses an
independence/null bootstrap , a non-parametric , a residual bootstrap ,
a fixed design bootstrap , a fixed design null bootstrap , a hybrid null
bootstrap as bootstrap resampling schemes to perform the bootstrap. This
function gives the corresponding p-values, the true test statistic and
the bootstrap-version test statistics. Furthermore, it also gives the
estimated slope.The default (and valid) method implemented in this
function is the null bootstrap, together with the equivalent test
statistic. Via the argument, the user can specify other bootstrap
resampling schemes and test statistics.

``` r
library(BootstrapTests)

n <- 100

# Under H1
X_data <- rnorm(n)
Y_data <-  X_data + rnorm(n)   #Y = X + epsilon
result <- perform_regression_test(X_data, Y_data, nBootstrap = 100,
                       bootstrapOptions =  list(type_boot = "indep",
                                                type_stat = "eq"))
print(result)
#>          🎯Bootstrap Regression Test Results 🎯
#>  ====================================================== 
#> 
#> Performed test:
#>   Bootstrap type           : indep
#>   Bootstrap repetitions    : 100
#>   Type of test statistic   : eq
#>   Beta                     : 0.9204966 
#>   p-value                  : 0
#>   True test statistic      : 9.2050
plot(result)
```

<img src="man/figures/README-example linear regression-1.png" width="100%" /><img src="man/figures/README-example linear regression-2.png" width="100%" />

``` r

# Under H0
X_data <- rnorm(n)
Y_data <-  rep(1, n)  #these values are exactly constant (as b = 0 under H0)
result <- perform_regression_test(X_data, Y_data, nBootstrap = 100)
print(result)
#>          🎯Bootstrap Regression Test Results 🎯
#>  ====================================================== 
#> 
#> Performed test:
#>   Bootstrap type           : indep
#>   Bootstrap repetitions    : 100
#>   Type of test statistic   : eq
#>   Beta                     : 1.971842e-16 
#>   p-value                  : 0.35
#>   True test statistic      : 0.0000
plot(result)
```

<img src="man/figures/README-example linear regression-3.png" width="100%" /><img src="man/figures/README-example linear regression-4.png" width="100%" />

``` r

# Showing all theoretically valid options:
result_valid <- perform_regression_test(X_data, Y_data, bootstrapOptions = "all")
print(result_valid)
#>          🎯Bootstrap Regression Test Results 🎯
#>  ====================================================== 
#> 
#> All test results:
#> 
#>  type_boot type_stat pvalues bootstrapped_tests theoretically_valid
#>      indep        eq    0.20       4.882052....                TRUE
#>         NP      cent    0.54       1.001348....                TRUE
#>   ci_upper_95  ci_upper_99
#>  3.355789e-15 3.611288e-15
#>  4.582757e-15 5.404115e-15
#> 
#> True test statistics:
#> [1] 1.971842e-15

# Showing all bootstrap combinations, including invalid options:
result_invalid <- perform_regression_test(X_data, Y_data, bootstrapOptions = "all and also invalid")
#> Warning in perform_regression_test(X_data, Y_data, bootstrapOptions = "all and
#> also invalid"): Using 'all and also invalid' as bootstrapOptions is not
#> recommended. This will return all theoretically valid and invalid combinations
#> of bootstrap resampling schemes, and test statistics. Please use with caution.
print(result_invalid)
#>          🎯Bootstrap Regression Test Results 🎯
#>  ====================================================== 
#> 
#> All test results:
#> 
#>              type_boot type_stat pvalues bootstrapped_tests theoretically_valid
#>                  indep      cent    0.54       8.462175....               FALSE
#>                  indep        eq    0.33       1.125624....                TRUE
#>                     NP      cent    0.43       3.786224....                TRUE
#>                     NP        eq    0.20       1.814382....               FALSE
#>                 res_bs      cent    0.39       6.550954....               FALSE
#>                 res_bs        eq    0.54       2.626937....               FALSE
#>  fixed_design_bs_Hnull      cent    0.19       1.971035....               FALSE
#>  fixed_design_bs_Hnull        eq    0.22       8.064584....               FALSE
#>        fixed_design_bs      cent    0.36       1.441583....               FALSE
#>        fixed_design_bs        eq    0.88       3.413425....               FALSE
#>         hybrid_null_bs      cent    0.60       5.146206....               FALSE
#>         hybrid_null_bs        eq    0.41       3.174364....               FALSE
#>   ci_upper_95  ci_upper_99
#>  4.504114e-15 5.877080e-15
#>  3.121898e-15 4.381178e-15
#>  4.153874e-15 4.990254e-15
#>  2.725698e-15 3.592837e-15
#>  5.126268e-15 6.480373e-15
#>  7.098110e-15 8.452215e-15
#>  3.468282e-15 4.465447e-15
#>  4.235973e-15 5.608171e-15
#>  4.483778e-15 7.505986e-15
#>  6.455619e-15 9.477828e-15
#>  5.562744e-15 6.082691e-15
#>  4.306166e-15 5.175889e-15
#> 
#> True test statistics:
#> [1] 1.971842e-15
```

### Goodness-of-fit testing

This function performs a bootstrap goodness-of-fit hypothesis test for a
specific univariate parametric family. The null hypothesis corresponds
to the sample coming from the specified parametric family, while the
alternative hypothesis corresponds to the sample not coming from the
specified parametric family. This function implements a null bootstrap
and a non-parametric bootstrap. The test statistic is the
Kolmogorov-Smirnov test statistic. To estimate the parameters of the
parametric family, either a minimum distance estimator, or a canonical
estimator (the sample mean and variance) is used. On the bootstrap
sample, we have also implemented a centered MD estimator, as in the
paper. For now, only a test of normality is implemented. This function
gives the corresponding p-values, the true test statistic and the
bootstrap-version test statistics. The default (and valid) method
implemented in this function is the null bootstrap, together with the
equivalent test statistic and the canonical parameter estimator. Via the
argument, the user can specify other bootstrap resampling schemes, test
statistics, and parameter estimators.

``` r
library(BootstrapTests)

n <- 100
# Under H1
X_data <- rgamma(n,2,3)
result <- perform_GoF_test(X_data,
                         nBootstrap = 100,
                         bootstrapOptions = list(type_boot = "null",
                                                 type_stat = "eq",
                                                 param_bs = "canonical")
                        )
print(result)
#>          🎯Bootstrap GoF Test Results 🎯
#>  =============================================== 
#> 
#> Performed test:
#>   Bootstrap type           : null
#>   Bootstrap repetitions    : 100
#>   Type of test statistic   : eq
#>   Bootstrap estimator used : canonical 
#>   p-value                  : 0.01
#>   True test statistic      : 1.1406
plot(result)
```

<img src="man/figures/README-example GOF-1.png" width="100%" />

``` r

# Under H0
X_data <- rnorm(n)
result <- perform_GoF_test(X_data, nBootstrap = 100)
print(result)
#>          🎯Bootstrap GoF Test Results 🎯
#>  =============================================== 
#> 
#> Performed test:
#>   Bootstrap type           : null
#>   Bootstrap repetitions    : 100
#>   Type of test statistic   : eq
#>   Bootstrap estimator used : canonical 
#>   p-value                  : 0.61
#>   True test statistic      : 0.4980
plot(result)
```

<img src="man/figures/README-example GOF-2.png" width="100%" />

## References

To be included
