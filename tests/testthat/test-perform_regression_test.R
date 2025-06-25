
test_that("regression test output is well formatted", {

  n = 100

  # Under H1
  X_data = rnorm(n)
  Y_data =  X_data + rnorm(n)   #Y = X + epsilon
  result = perform_regression_test(X_data, Y_data, nBootstrap = 10)

  expect_true(is.data.frame(result$pvals_df))

  expect_true(all(result$pvals_df$pval >= 0))

  expect_true(all(result$pvals_df$pval <= 1))

  # Under H0
  X_data = rnorm(n)
  Y_data =  rep(1, n)  #these values are exactly constant (as b = 0 under H0)
  result = perform_regression_test(X_data, Y_data, nBootstrap = 10)

  expect_true(is.data.frame(result$pvals_df))

  expect_true(all(result$pvals_df$pval >= 0))

  expect_true(all(result$pvals_df$pval <= 1))
})

