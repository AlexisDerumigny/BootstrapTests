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

test_that("Independence test throws errors for wrong input", {
  X1 = rnorm(10)
  X2 = rnorm(10)
  expect_error(perform_independence_test(X1,X2, norm_type = ""))
  expect_error(perform_independence_test(X1,X2, type_stat = ""))
  expect_error(perform_independence_test(X1,X2, type_boot = ""))
  expect_error(perform_independence_test(X1,X2, nBootstrap = ""))
})


test_that("Different types of bootstrap options work as expected", {

  n = 100

  # Under H1
  X1 = rnorm(n)
  X2 = X1 + rnorm(n)
  result_1 = perform_independence_test(X1, X2, nBootstrap = 30)

  result_2 = perform_independence_test(
    X1, X2, nBootstrap = 30,
    bootstrapOptions = list(type_boot = "indep",
                            type_stat = "eq",
                            norm_type = "KS") )

  expect_identical(result_1, result_2)

  result_3 = perform_independence_test(
    X1, X2, nBootstrap = 30,
    bootstrapOptions = list(type_boot = "indep") )

  expect_identical(result_1, result_3)

  result_4 = perform_independence_test(
    X1, X2, nBootstrap = 30,
    bootstrapOptions = list(type_boot = "NP") )

  expect_warning({
    result_5 = perform_independence_test(
      X1, X2, nBootstrap = 30,
      bootstrapOptions = list(type_boot = "NP",
                              type_stat = "eq") )
  })

  result_6 = perform_independence_test(X1, X2, nBootstrap = 30,
                                       bootstrapOptions = "all")

  expect_warning({
    result_7 = perform_independence_test(X1, X2, nBootstrap = 30,
                                         bootstrapOptions = "all and also wrong")
  })

  expect_false(identical(result_6, result_7))
})


