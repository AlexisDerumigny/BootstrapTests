
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

test_that("Regression test throws errors for wrong input", {
  n = 10

  # Under H1
  X = rnorm(n)
  Y =  X + rnorm(n)   #Y = X + epsilon

  expect_error(perform_regression_test(X, Y, nBootstrap = ""))

  # Check the `bootstrapOptions`
  expect_error(perform_regression_test(X, Y, nBootstrap = 10,
                                       bootstrapOptions = list(type_norm = "K S")))
  expect_error(perform_regression_test(X, Y, nBootstrap = 10,
                                       bootstrapOptions = list(typo = "KS")))
  expect_error(perform_regression_test(X, Y, nBootstrap = 10,
                                       bootstrapOptions = list(boot_type = "KS")))
})


test_that("Different types of bootstrap options work as expected", {

  n = 10

  # Under H1
  X = rnorm(n)
  Y = X + rnorm(n)

  set.seed(10)
  result_1 = perform_regression_test(X, Y, nBootstrap = 10)
  set.seed(10)
  result_2 = perform_regression_test(
    X, Y, nBootstrap = 10,
    bootstrapOptions = list(type_boot = "indep",
                            type_stat = "eq") )

  expect_identical(result_1, result_2)

  set.seed(10)
  result_3 = perform_regression_test(
    X, Y, nBootstrap = 10,
    bootstrapOptions = list(type_boot = "indep") )

  # use the same seed to have equal result
  expect_identical(result_1, result_3)

  # Give warning for theoreotically invalid bootstrap schemes
  expect_warning({ perform_regression_test(
    X, Y, nBootstrap = 10,
    bootstrapOptions = list(type_boot = "NP",
                            type_stat = "eq") )
  })

  # Give warning for theoreotically invalid bootstrap schemes
  expect_warning({perform_regression_test(
    X, Y, nBootstrap = 10,
    bootstrapOptions = list(type_boot = "indep",
                            type_stat = "cent") )
  })

  set.seed(10)
  result_6 = perform_regression_test(X, Y, nBootstrap = 10,
                                       bootstrapOptions = "all")

  set.seed(10)
  expect_warning({
    result_7 = perform_regression_test(X, Y, nBootstrap = 10,
                                         bootstrapOptions = "all and also invalid")
  })

  # expect that the results are not identical, as the `result_7' also gives
  # invalid bootstrap schemes
  expect_false(identical(result_6, result_7))
})


test_that("Different types of bootstrap options work as expected", {

  set.seed(10)
  n <- 500

  # Under H1
  X_data <- rnorm(n)
  Y_data <-  X_data + rnorm(n)   #Y = X + epsilon

  all_types = c("indep", "NP", "res_bs", "hybrid_null_bs",
                "fixed_design_bs", "fixed_design_bs_Hnull")

  for (itype in 1:length(all_types)){
    type = all_types[itype]

    result <- perform_regression_test(X_data, Y_data, nBootstrap = 100,
                                      bootstrapOptions = list(type_boot = type),
                                      show_progress = FALSE)

    expect_equal(result$pvals_df$pvalues, 0)
  }
})


