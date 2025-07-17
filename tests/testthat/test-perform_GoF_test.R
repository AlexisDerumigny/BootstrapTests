
test_that("GoF test output is well formatted", {

  n = 10
  # Under H1
  X_data = rgamma(n,2,3)

  result = perform_GoF_test(X_data, nBootstrap = 10)

  expect_true(is.data.frame(result$pvals_df))

  expect_true(all(result$pvals_df$pval >= 0))

  expect_true(all(result$pvals_df$pval <= 1))

  # Under H0
  X_data = rnorm(n)
  result = perform_GoF_test(X_data, nBootstrap = 10)

  expect_true(is.data.frame(result$pvals_df))

  expect_true(all(result$pvals_df$pval >= 0))

  expect_true(all(result$pvals_df$pval <= 1))

})

test_that("GoF test throws errors for wrong input", {
  X_data = rgamma(10,2,3)
  expect_error(perform_GoF_test(X_data, param_bs_user = ""))
  expect_error(perform_GoF_test(X_data, type_stat_user = ""))
  expect_error(perform_GoF_test(X_data, type_boot_user = ""))
  expect_error(perform_GoF_test(X_data, parametric_fam = ""))
  expect_error(perform_GoF_test(X_data, nBootstrap = ""))
})


test_that("GoF test throws errors for wrong input", {
  X_data = rgamma(10,2,3)

  # invalid input data
  expect_error(perform_GoF_test(rnorm(0)))
  expect_error(perform_GoF_test("string_input", nBootstrap = 0))

  # `nBootstrap` argument
  expect_error(perform_GoF_test(X_data, nBootstrap = 0))
  expect_error(perform_GoF_test(X_data, nBootstrap = ""))

  # Check the `bootstrapOptions`
  expect_error(perform_GoF_test(X_data, bootstrapOptions = list(type_stat = "eQ")))
  expect_error(perform_GoF_test(X_data, bootstrapOptions = list(typo = "KS")))
  expect_error(perform_GoF_test(X_data, bootstrapOptions = list(boot_type = "KS")))
})


test_that("Different types of bootstrap options work as expected", {

  n = 10
  X_data = rnorm(n)

  set.seed(10)
  result_1 = perform_GoF_test(X_data, nBootstrap = 10)
  set.seed(10)
  result_2 = perform_GoF_test(
    X_data, nBootstrap = 10,
    bootstrapOptions = list(type_boot = "null",
                            type_stat = "eq",
                            param_bs = "MLE") )

  expect_identical(result_1, result_2)

  set.seed(10)
  result_3 = perform_GoF_test(
    X_data, nBootstrap = 10,
    bootstrapOptions = list(type_boot = "null") )

  # use the same seed to have equal result
  expect_identical(result_1, result_3)

  # Give warning for theoreotically invalid bootstrap schemes
  expect_warning({ perform_GoF_test(
    X_data, nBootstrap = 10,
    bootstrapOptions = list(type_boot = "NP",
                            type_stat = "eq") )
  })

  # Give warning for theoreotically invalid bootstrap schemes
  expect_warning({perform_GoF_test(
    X_data, nBootstrap = 10,
    bootstrapOptions = list(type_boot = "null",
                            type_stat = "cent") )
  })

  set.seed(10)
  result_6 = perform_GoF_test(X_data, nBootstrap = 10,
                                       bootstrapOptions = "all")

  set.seed(10)
  expect_warning({
    result_7 = perform_GoF_test(X_data, nBootstrap = 10,
                                         bootstrapOptions = "all and also invalid")
  })

  # expect that the results are not identical, as the `result_7' also gives
  # invalid bootstrap schemes
  expect_false(identical(result_6, result_7))
})

