test_that("plot.bootstrapTest works for multiple bootstrapping types at the same time", {
  set.seed(10)
  nBootstrap = 10

  # Independence test

  n <- 10
  X1 <- rnorm(n)
  X2 <- rnorm(n)
  result <- perform_independence_test(X1, X2, nBootstrap = nBootstrap,
                                      bootstrapOptions = "all")

  expect_no_error(plot(result))

  expect_warning(
    result <- perform_independence_test(X1, X2, nBootstrap = nBootstrap,
                                        bootstrapOptions = "all and also invalid") )

  expect_no_error(plot(result))

  # GoF test
  result <- perform_GoF_test(X1, nBootstrap = nBootstrap,
                             bootstrapOptions = "all")

  expect_no_error(plot(result))

  expect_warning(
    result <- perform_GoF_test(X1, nBootstrap = nBootstrap,
                               bootstrapOptions = "all and also invalid") )

  expect_no_error(plot(result))

  # Regression test
  result <- perform_regression_test(X1, X2, nBootstrap = nBootstrap,
                                    bootstrapOptions = "all")

  expect_no_error(plot(result))

  expect_warning(
    result <- perform_regression_test(X1, X2, nBootstrap = nBootstrap,
                                      bootstrapOptions = "all and also invalid") )

  expect_no_error(plot(result))
})
