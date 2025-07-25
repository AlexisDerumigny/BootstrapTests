
test_that("GoF test output is well formatted", {

  n = 10
  # Under H1
  X_data = rgamma(n,2,3)

  result = perform_GoF_test(X_data, nBootstrap = 10, verbose = 0)

  expect_true(is.data.frame(result$pvals_df))

  expect_true(all(result$pvals_df$pval >= 0))

  expect_true(all(result$pvals_df$pval <= 1))

  # Under H0
  X_data = rnorm(n)
  result = perform_GoF_test(X_data, nBootstrap = 10, verbose = 0)

  expect_true(is.data.frame(result$pvals_df))

  expect_true(all(result$pvals_df$pval >= 0))

  expect_true(all(result$pvals_df$pval <= 1))

})

test_that("GoF test throws errors for wrong input", {
  X_data = rgamma(10,2,3)
  expect_error(perform_GoF_test(X_data, type_estimator_bootstrap = "", verbose = 0) )
  expect_error(perform_GoF_test(X_data, type_stat = "", verbose = 0) )
  expect_error(perform_GoF_test(X_data, type_boot = "", verbose = 0) )
  expect_error(perform_GoF_test(X_data, parametric_fam = "", verbose = 0) )
  expect_error(perform_GoF_test(X_data, nBootstrap = "", verbose = 0) )
})


test_that("GoF test throws errors for wrong input", {
  X_data = rgamma(10,2,3)

  # invalid input data
  expect_error(perform_GoF_test(rnorm(0), verbose = 0) )
  expect_error(perform_GoF_test("string_input", nBootstrap = 0, verbose = 0))

  # `nBootstrap` argument
  expect_error(perform_GoF_test(X_data, nBootstrap = 0, verbose = 0) )
  expect_error(perform_GoF_test(X_data, nBootstrap = "", verbose = 0) )

  # Check the `bootstrapOptions`
  expect_error(perform_GoF_test(X_data, bootstrapOptions = list(type_stat = "eQ"),
                                verbose = 0) )

  expect_error(perform_GoF_test(X_data, bootstrapOptions = list(typo = "KS"),
                                verbose = 0) )

  expect_error(perform_GoF_test(X_data, bootstrapOptions = list(boot_type = "KS"),
                                verbose = 0) )
})


test_that("Different types of bootstrap options work as expected", {

  n = 10
  X_data = rnorm(n)

  set.seed(10)
  result_1 = perform_GoF_test(X_data, nBootstrap = 10, verbose = 0)
  set.seed(10)
  result_2 = perform_GoF_test(
    X_data, nBootstrap = 10,
    bootstrapOptions = list(type_boot = "null",
                            type_stat = "eq",
                            type_estimator_bootstrap = "MLE"),
    verbose = 0 )

  expect_identical(result_1, result_2)

  set.seed(10)
  result_3 = perform_GoF_test(
    X_data, nBootstrap = 10,
    bootstrapOptions = list(type_boot = "null"),
    verbose = 0 )

  # use the same seed to have equal result
  expect_identical(result_1, result_3)

  # Give warning for theoretically invalid bootstrap schemes
  expect_warning({
    result_4 = perform_GoF_test(
      X_data, nBootstrap = 10,
      bootstrapOptions = list(type_boot = "NP",
                              type_stat = "eq"),
      verbose = 0 )
  })

  # Give warning for theoretically invalid bootstrap schemes
  expect_warning({
    result_5 = perform_GoF_test(
      X_data, nBootstrap = 10,
      bootstrapOptions = list(type_boot = "null",
                              type_stat = "cent"),
      verbose = 0 )
  })

  set.seed(10)
  result_6 = perform_GoF_test(X_data, nBootstrap = 3,
                              bootstrapOptions = "all",
                              verbose = 0)

  expect_identical(nrow(result_6$pvals_df), 4L)

  set.seed(10)
  expect_warning({
    result_7 = perform_GoF_test(X_data, nBootstrap = 10,
                                bootstrapOptions = "all and also invalid",
                                verbose = 0)
  })

  total_number_bootstrap_combinations = 2L * 2L * 3L
  expect_identical(nrow(result_7$pvals_df), total_number_bootstrap_combinations)


  # expect that the results are not identical, as the `result_7' also gives
  # invalid bootstrap schemes
  expect_false(identical(result_6, result_7))
})

