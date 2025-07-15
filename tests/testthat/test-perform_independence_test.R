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
  expect_error(perform_independence_test(X1,X2,
                                         bootstrapOptions = list(norm_type = "")))
  expect_error(perform_independence_test(X1,X2,
                                         bootstrapOptions = list(type_stat = "")))
  expect_error(perform_independence_test(X1,X2,
                                         bootstrapOptions = list(type_boot = "")))
  expect_error(perform_independence_test(X1,X2, nBootstrap = ""))

  # Check the `bootstrapOptions`
  expect_error(perform_independence_test(X1,X2, bootstrapOptions = list(norm_type = "K S")))
  expect_error(perform_independence_test(X1,X2, bootstrapOptions = list(typo = "KS")))
  expect_error(perform_independence_test(X1,X2, bootstrapOptions = list(boot_type = "KS")))
})


test_that("Different types of bootstrap options work as expected", {

  n = 100

  # Under H1
  X1 = rnorm(n)
  X2 = X1 + rnorm(n)
  set.seed(10)
  result_1 = perform_independence_test(X1, X2, nBootstrap = 30)
  set.seed(10)
  result_2 = perform_independence_test(
    X1, X2, nBootstrap = 30,
    bootstrapOptions = list(type_boot = "indep",
                            type_stat = "eq",
                            norm_type = "KS") )

  expect_identical(result_1, result_2)
  set.seed(10)
  result_3 = perform_independence_test(
    X1, X2, nBootstrap = 30,
    bootstrapOptions = list(type_boot = "indep") )

  # use the same seed to have equal result
  expect_identical(result_1, result_3)

  # Give warning for theoreotically invalid bootstrap schemes
  expect_warning({ perform_independence_test(
    X1, X2, nBootstrap = 30,
    bootstrapOptions = list(type_boot = "NP",
                            type_stat = "eq") )
  })

  # Give warning for theoreotically invalid bootstrap schemes
  expect_warning({perform_independence_test(
    X1, X2, nBootstrap = 30,
    bootstrapOptions = list(type_boot = "indep",
                            type_stat = "cent") )
  })

  set.seed(10)
  result_6 = perform_independence_test(X1, X2, nBootstrap = 30,
                                       bootstrapOptions = "all")

  set.seed(10)
  expect_warning({
    result_7 = perform_independence_test(X1, X2, nBootstrap = 30,
                                         bootstrapOptions = "all and also invalid")
  })

  # expect that the results are not identical, as the `result_7' also gives
  # invalid bootstrap schemes
  expect_false(identical(result_6, result_7))
})


