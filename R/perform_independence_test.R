
#' This function generates bootstrap samples for the independence testing, by
#' inputting the \code{X1} and \code{X2} data and the type_boot (the type of
#' bootstrap to be performed)
#'
#' @param X1 univariate data vector
#' @param X2 univariate data vector
#' @param type_boot type of bootstrap to resample the data, either  \code{"indep"}
#'        for independence bootstrap or \code{"NP"} for non-parametric bootstrap.
#'
#' @returns a list with two items, the bootstrap samples \code{X1_st}, \code{X2_st}.
#'
#' @noRd
#'
generateBootstrapSamples <- function(X1, X2, type_boot){
  n=length(X1)
  switch (
    type_boot,

    "indep" = {
      permutation1 = sample.int(n, replace = TRUE)
      permutation2 = sample.int(n, replace = TRUE)
      X1_st = X1[permutation1]
      X2_st = X2[permutation2]
    },

    "NP" = {
      permutation = sample.int(n, replace = TRUE)
      X1_st = X1[permutation]
      X2_st = X2[permutation]
    },

    {
      stop("unknown type_boot")
    }
  )
  return (list(X1_st = X1_st, X2_st = X2_st))
}


#' Function to compute empirical joint CDF at each point (x, y) in the grid
#' Use \code{outer} to compute the empirical joint CDF in a vectorized way
#'
#' @param X1 univariate data vector
#' @param X2 univariate data vector
#' @param my_grid grid to evaluate the ecdf on, equal for \code{X1} and \code{X2}
#'
#' @return joint ECDF
#' @noRd
#'
compute_joint_ecdf <- function(X1, X2, my_grid) {
  FX12_joint <- outer(my_grid, my_grid, Vectorize(function(x, y) {
    mean(X1 <= x & X2 <= y)
  }))

  return(FX12_joint)
}


#' Perform a test of  independence
#'
#' Perform a hypothesis test of statistical independence by means of bootstrapping.
#' This procedure gives a total of 8 combinations of bootstrap resampling schemes
#' (nonparametric and independent), test statistics (centered and equivalent),
#' and Kolmogorov-Smirnov or L2-type of true test statistic. This function
#' gives the corresponding p-values, the true test statistic and the
#' bootstrap-version test statistics.
#'
#' @param X1,X2 numerical vectors of the same size. The independence test tests
#' whether \code{X1} is independent from \code{X2}.
#'
#' @param my_grid the grid on which the empirical CDFs are estimated.
#  TODO: implement a different grid for X1 and X2.
#  TODO: can the grid be chosen automatically? For example quantiles of X1, X2?
#'
#' @param nBootstrap number of bootstrap repetitions.
#'
#' @param type_boot_user type of bootstrap to resample the data, either 'NP' or 'cent'.
#'
#' @param type_stat_user type of test statistic to use, either 'cent' for centered or 'eq' for equivalent.
#'
#' @param norm_type_user type of norm to use for test statistic, either 'L2' or 'KS'.
#'
#' @param give_all_test_information logical, whether or not to give all test information
#'
#' @return A list with components \itemize{
#'    \item \code{pvals_df}: df of p-values and bootstrapped test statistics:
#'
#'    These are the p-values for the 8 combinations of bootstrap resampling schemes
#'    (nonparametric and independent), test statistics (centered and equivalent),
#'    and Kolmogorov-Smirnov or L2-type of true test statistic. The column 'bootstrapped_tests'
#'    contains vectors of bootstrap test statistics.
#'
#'    \item \code{true_stats} a named vector of size 2 containing the true test
#'    statistics for the L2 and KS distances.
#' }
#'
#' @examples
#' n = 100
#'
#' # Under H1
#' X1 = rnorm(n)
#' X2 = X1 + rnorm(n)
#' result = perform_independence_test(X1, X2, nBootstrap = 100)
#' result
#' #
#' # Under H0
#' X1 = rnorm(n)
#' X2 = rnorm(n)
#' result = perform_independence_test(X1, X2, nBootstrap = 100)
#' result
#'
#' @export
#'
perform_independence_test <- function(X1, X2,
                                      my_grid = NULL,
                                      nBootstrap = 100,
                                      type_boot_user = "indep",
                                      type_stat_user = "eq",
                                      norm_type_user = "KS",
                                      give_all_test_information = FALSE)
{
  # Checking the validity of the inputs
  if (length(X1) != length(X2)){
    stop("X1 and X2 must have the same length. Here the length of X1 is ",
         length(X1), " while the length of X2 is ", length(X2))
  }

  if (length(nBootstrap) > 1 || !is.finite(nBootstrap) || nBootstrap <= 0){
    stop("nBootstrap must be a positive integer of length 1.")
  }

  # Computation of the original statistics ===========================

  # Define sample size
  n = length(X1)

  # Parameters of the grid
  if (is.null(my_grid)){
    my_grid = seq(min(X1,X2), max(X1,X2), length.out = 100)
  }


  # Estimation of the product of the marginal CDFs
  FX1 = stats::ecdf(X1)(my_grid)
  FX2 = stats::ecdf(X2)(my_grid)
  FX1FX2 = outer(FX1, FX2)

  # Compute joint ecdf
  FX12 <- compute_joint_ecdf(X1, X2, my_grid)

  # Vector containing all the true test statistics, for the different norms
  true_stats = c("L2" = (sum((FX1FX2 - FX12)^2))*sqrt(n) ,
                 # Kolmogorov-Smirnov test statistic, without sqrt(n)
                 "KS" = max(abs(FX1FX2 - FX12))          )


  # Bootstrapping ===========================================================

  list_results = list()

  vec_type_boot = c("indep", "NP")

  for (iBoot in 1:length(vec_type_boot)){
    type_boot = vec_type_boot[iBoot]

    #initialisation
    stat_st_cent_L2 = rep(NA, nBootstrap)
    stat_st_eq_L2 = rep(NA, nBootstrap)
    stat_st_cent_KS = rep(NA, nBootstrap)
    stat_st_eq_KS = rep(NA, nBootstrap)

    for (iBootstrap in 1:nBootstrap){
      # Generation of the bootstrapped data
      dataBoot = generateBootstrapSamples(X1 = X1, X2 = X2,
                                          type_boot = type_boot)
      X1_st = dataBoot$X1_st
      X2_st = dataBoot$X2_st

      # Estimation of the product of the marginal CDFs
      FX1_st = stats::ecdf(X1_st)(my_grid)
      FX2_st = stats::ecdf(X2_st)(my_grid)
      FX1FX2_st = outer(FX1_st, FX2_st)

      # Empirical joint CDF on the bootstrap data
      FX12_st <- compute_joint_ecdf(X1_st, X2_st, my_grid)

      stat_st_cent_L2[iBootstrap] =
        (sum((FX1FX2_st - FX1FX2 + FX12 - FX12_st)^2)) * sqrt(n)

      stat_st_cent_KS[iBootstrap] =
        max(abs(FX1FX2_st - FX1FX2 + FX12 - FX12_st))

      stat_st_eq_L2[iBootstrap] = (sum((FX1FX2_st - FX12_st)^2)) * sqrt(n)

      stat_st_eq_KS[iBootstrap] = max(abs(FX1FX2_st - FX12_st))

    }

    # Put dataframes in a list, alternating the entries
    list_results[[1 + (iBoot - 1)*2]] =
      data.frame(type_boot = type_boot,
                 type_stat = "cent",
                 norm_type = c("L2", "KS"),
                 bootstrapped_tests = I(list(stat_st_cent_L2,
                                             stat_st_cent_KS) )
      )
    list_results[[2 + (iBoot - 1)*2]] =
      data.frame(type_boot = type_boot,
                 type_stat = "eq",
                 norm_type = c("L2", "KS"),
                 bootstrapped_tests = I(list(stat_st_eq_L2,
                                             stat_st_eq_KS) )
      )
  }

  # Rowbind the dataframes in `list_results` into a dataframe
  pvals_df = do.call(what = rbind, args = list_results)

  # Calculate pvalues
  # list apply to the list `pvalues` and use `|> unlist()` to unlist it
  pvals_df$pvalues = lapply(
    X = 1:nrow(pvals_df),
    FUN = function(i){
      pval = mean(as.numeric(
        true_stats[pvals_df$norm_type[i]] < pvals_df$bootstrapped_tests[i][[1]]
      ) )
    }
  ) |> unlist()

  pvals_df$theoretically_valid =
    (pvals_df$type_boot == "indep" & pvals_df$type_stat == "eq")  |
    (pvals_df$type_boot == "NP"    & pvals_df$type_stat == "cent")

  result = ( list(
    # df of p-values
    pvals_df = pvals_df ,

    # true test statistics
    true_stats = true_stats ) )

  # make a class for the result object
  class(result) <- c("bootstrapTest_independence")

  return(result)
}

#' @export
print.bootstrapTest_independence <- function(x, ...){
  cat("Independence test results:\n")
  cat("P-values for the bootstrap tests:\n")
  print(x$pvals_df)
  cat("\nTrue test statistics:\n")
  print(x$true_stats)
}

