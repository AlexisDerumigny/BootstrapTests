
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
the `bootstrapOptions` argument, the user can specify other bootstrap
resampling schemes and test statistics.

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
#>  quantile_95 quantile_99
#>    23.389507   25.734951
#>     0.073005    0.086023
#>    21.958640   25.743274
#>     0.072925    0.080827
#> 
#> True test statistics:
#>      L2      KS 
#> 16.9997  0.0620
```

### Slope testing in linear regression setting

This function performs a bootstrap regression test for given data X,Y.
The null hypothesis corresponds of a slope coefficient of zero, versus
the alternative hypothesis of a non-zero slope coefficient. It uses an
independence/null bootstrap `"indep"`, a non-parametric `"NP"`, a
residual bootstrap `"res_bs"`, a fixed design bootstrap
`"fixed_design_bs"`, a fixed design null bootstrap
`"fixed_design_bs_Hnull"`, a hybrid null bootstrap `"hybrid_null_bs"` as
bootstrap resampling schemes to perform the bootstrap. This function
gives the corresponding p-values, the true test statistic and the
bootstrap-version test statistics. Furthermore, it also gives the
estimated slope.The default (and valid) method implemented in this
function is the null bootstrap, together with the equivalent test
statistic. Via the `bootstrapOptions` argument, the user can specify
other bootstrap resampling schemes and test statistics.

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
#>   Slope coefficient β      : 0.7940995 
#>   p-value                  : 0
#>   True test statistic      : 7.9410
plot(result)
```

<img src="man/figures/README-example linear regression-1.png" width="100%" /><img src="man/figures/README-example linear regression-2.png" width="100%" />

``` r

# Under H0
X_data <- rnorm(n)
Y_data <- 0 * X_data + rnorm(n) 
result <- perform_regression_test(X_data, Y_data, nBootstrap = 100)
print(result)
#>          🎯Bootstrap Regression Test Results 🎯
#>  ====================================================== 
#> 
#> Performed test:
#>   Bootstrap type           : indep
#>   Bootstrap repetitions    : 100
#>   Type of test statistic   : eq
#>   Slope coefficient β      : 0.000209957 
#>   p-value                  : 1
#>   True test statistic      : 0.0021
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
#>  type_boot type_stat pvalues bootstrapped_tests theoretically_valid quantile_95
#>      indep        eq    1.00       1.491490....                TRUE    1.750612
#>         NP      cent    0.98       0.500202....                TRUE    1.929458
#>  quantile_99
#>     2.553541
#>     2.293660
#> 
#> True test statistics:
#> [1] 0.00209957
```

### Goodness-of-fit testing

This function performs a bootstrap goodness-of-fit hypothesis test for a
specific univariate parametric family. The null hypothesis corresponds
to the sample coming from the specified parametric family, while the
alternative hypothesis corresponds to the sample not coming from the
specified parametric family. This function implements a null bootstrap
and a non-parametric bootstrap. The test statistic is the
Kolmogorov-Smirnov test statistic. To estimate the parameters of the
parametric family, either a minimum distance estimator, or a MLE
estimator (the sample mean and variance) is used. On the bootstrap
sample, we have also implemented a centered MD estimator, as in the
paper. For now, only a test of normality is implemented. This function
gives the corresponding p-values, the true test statistic and the
bootstrap-version test statistics. The default (and valid) method
implemented in this function is the null bootstrap, together with the
equivalent test statistic and the MLE parameter estimator. Via the
`bootstrapOptions` argument, the user can specify other bootstrap
resampling schemes, test statistics, and parameter estimators.

``` r
library(BootstrapTests)

n <- 100
# Under H1
X_data <- rgamma(n,2,3)
result <- perform_GoF_test(X_data,
                         nBootstrap = 100,
                         bootstrapOptions = list(type_boot = "null",
                                                 type_stat = "eq",
                                                 param_bs = "MLE")
                        )
print(result)
#>          🎯Bootstrap GoF Test Results 🎯
#>  =============================================== 
#> 
#> Performed test:
#>   Bootstrap type           : null
#>   Bootstrap repetitions    : 100
#>   Type of test statistic   : eq
#>   Bootstrap estimator used : MLE 
#>   p-value                  : 0.04
#>   True test statistic      : 0.9334
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
#>   Bootstrap estimator used : MLE 
#>   p-value                  : 0.04
#>   True test statistic      : 0.8963
plot(result)
```

<img src="man/figures/README-example GOF-2.png" width="100%" />

``` r

# Showing all theoretically valid options:
result_valid <- perform_GoF_test(X_data, bootstrapOptions = "all")
print(result_valid)
#>          🎯Bootstrap GoF Test Results 🎯
#>  =============================================== 
#> 
#> All test results:
#> 
#>  type_boot type_stat param_bs bootstrapped_tests pvalues theoretically_valid
#>       null        eq       MD       0.394892....    0.09                TRUE
#>       null        eq      MLE       0.286271....    0.02                TRUE
#>         NP      cent  MD-cent       0.319439....    0.09                TRUE
#>         NP      cent      MLE       0.417932....    0.03                TRUE
#>  quantile_95 quantile_99
#>    0.6243186   0.6394306
#>    0.8198315   0.9002679
#>    0.5687391   0.6975290
#>    0.8120607   0.9138376
#> 
#> True test statistics:
#>  KS_with_MD KS_with_MLE 
#>   0.5509896   0.8963067
```

## References

To be included
