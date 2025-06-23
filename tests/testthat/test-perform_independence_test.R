test_that("independence test output is well formatted", {
  # Under H1
  n=100
  X1 = rnorm(n)
  X2 = X1 + rnorm(n)
  my_grid = seq(-5, 5, by = 0.1)
  result = perform_independence_test(X1, X2, my_grid, nBootstrap = 10)

  expect_true(is.data.frame(result$pvals_df))

  expect_true(all(result$pvals_df$pval >= 0))

  expect_true(all(result$pvals_df$pval <= 1))


  # Under H0
  X1 = rnorm(n)
  X2 = rnorm(n)
  my_grid = seq(-5, 5, by = 0.1)
  result = perform_independence_test(X1, X2, my_grid, nBootstrap = 10)

  expect_true(is.data.frame(result$pvals_df))

  expect_true(all(result$pvals_df$pval >= 0))

  expect_true(all(result$pvals_df$pval <= 1))
})
