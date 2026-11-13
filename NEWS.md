# BootstrapTests 0.1.1

- Relax too strict unit tests with `identical()`, replaced by `all.equal()` in
  ([dd65b0a](https://github.com/AlexisDerumigny/BootstrapTests/commit/dd65b0a518ebb22629ee76b472991c0146e77fb0)), 
  as reported by CRAN on MKL
  `Failure ('test-perform_regression_test.R:64:3'): Different types of bootstrap options work as expected`.
  
- Fix non-existent 95 and 99 percent quantiles not being printed in
  `print.bootstrapTest()`.

- For the regression test in `R/perform_regression_test.R`, output estimated 
  regression intercept as well, to complement to the regression slope in the 
  returned list for `perform_regression_test()`.
  
- Clarify general error messages.

- Complete list of theoretically valid bootstrap regression schemes by adding 
  fixed design schemes in `perform_regression_test()`.


# BootstrapTests 0.1.0

- Initial CRAN submission.
