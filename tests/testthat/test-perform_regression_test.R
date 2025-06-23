
test_that("regression test output is well formatted", {

  result = perform_regression_test(rnorm(10), rnorm(10), nBootstrap = 10)

  expect_true(is.data.frame(result$pvals_df))

  expect_true(all(result$pvals_df$pval >= 0))

  expect_true(all(result$pvals_df$pval <= 1))
})
