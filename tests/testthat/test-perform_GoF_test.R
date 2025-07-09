
test_that("GoF test output is well formatted", {

  n = 100
  # Under H1
  X_data = rgamma(n,2,3)

  result = perform_GoF_test(X_data, nBootstrap = 30)

  expect_true(is.data.frame(result$pvals_df))

  expect_true(all(result$pvals_df$pval >= 0))

  expect_true(all(result$pvals_df$pval <= 1))

  # Under H0
  X_data = rnorm(n)
  result = perform_GoF_test(X_data, nBootstrap = 30)

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
