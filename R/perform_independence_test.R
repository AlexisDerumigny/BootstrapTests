
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


#' Perform a hypothesis test of independence
#'
#' Perform a hypothesis test of statistical independence by means of bootstrapping.
#' The null hypothesis is that of independence between the two random variables,
#' versus the alternative of dependence between them.
#' This procedure gives a total of 8 combinations of bootstrap resampling schemes
#' (nonparametric and independent), test statistics (centered and equivalent),
#' and Kolmogorov-Smirnov or L2-type of true test statistic. This function
#' gives the corresponding p-values, the true test statistic and the
#' bootstrap-version test statistics. The default (and valid) method implemented
#' in this function is the null bootstrap, together with the equivalent test
#' statistic and Kolmogorov-Smirnov test statistic.
#' Via the \code{bootstrapOptions} argument, the user can specify other
#' bootstrap resampling schemes and test statistics.
#'
#' @param X1,X2 numerical vectors of the same size. The independence test tests
#' whether \code{X1} is independent from \code{X2}.
#'
#' @param my_grid the grid on which the empirical CDFs are estimated. It defaults
#' to \code{NULL}, so it is chosen automatically. However, the user can input
#' a grid of their choice.
#  TODO: implement a different grid for X1 and X2.
#  TODO: can the grid be chosen automatically? For example quantiles of X1, X2?
#'
#' @param nBootstrap number of bootstrap repetitions.
#'
#' @param bootstrapOptions This can be one of \itemize{
#'   \item \code{NULL}
#'
#'   \item a list with at most 3 elements names \itemize{
#'         \item \code{type_boot} defaults to the \code{"indep"} bootstrap
#'         resampling scheme to be used. \code{type_boot} can be either
#'         \code{"indep"} for the independence/null bootstrap, or \code{"NP"}
#'         for the non-parametric bootstrap.
#'
#'         \item \code{type_stat} defaults to \code{"eq"} for the type of test
#'         statistic to be used. This can be either \code{"eq"} for the
#'         equivalent test statistic, or \code{"cent"} for the centered
#'         test statistic.
#'
#'         \item \code{norm_type} defaults to \code{"KS"} for the type of norm
#'         to bes used for the test statistic. \code{norm_type} can be either
#'         \code{"KS"} for the Kolmogorov-Smirnov type test statistic or
#'         \code{"L2"} for the L2-type test statistic.
#'   }
#'
#'   \item \code{"all"} this gives test results for all theoretically valid
#'   combinations of bootstrap resampling schemes.
#'
#'   \item \code{"all and also invalid"} this gives test results for all possible
#'   combinations of bootstrap resampling schemes and test statistics, including
#'   invalid ones.
#' }
#' A warning is raised if the given combination of \code{type_boot_user} and
#' \code{type_stat_user} is theoretically invalid.
#'
#'
#' @return A class object with components \itemize{
#'    \item \code{pvals_df}: a dataframe of p-values and bootstrapped test statistics:
#'
#'    These are the p-values for the 8 combinations of bootstrap resampling schemes
#'    (nonparametric and independent), test statistics (centered and equivalent),
#'    and Kolmogorov-Smirnov or L2-type of true test statistic.
#'
#'    It also contains the vectors of bootstrap test statistics
#'    for each of the combinations.
#'
#'    \item \code{true_stats} a named vector of size 2 containing the true test
#'    statistics for the L2 and KS distances.
#'
#'    \item \code{nBootstrap} Number of bootstrap repetitions.
#'
#'    \item \code{nameMethod} string for the name of the method used.
#' }
#'
#' @seealso \code{\link{perform_GoF_test},\link{perform_regression_test}}.
#' The print and plot methods, such as \code{\link{plot.bootstrapTest}}.
#'
#' @examples
#' n <- 100
#'
#' # Under H1
#' X1 <- rnorm(n)
#' X2 <- X1 + rnorm(n)
#' result <- perform_independence_test(
#'    X1, X2, nBootstrap = 100,
#'    bootstrapOptions = list(type_boot = "indep",
#'                            type_stat = "eq",
#'                            norm_type = "KS") )
#' print(result)
#' plot(result)
#'
#' # Under H0
#' X1 <- rnorm(n)
#' X2 <- rnorm(n)
#' result <- perform_independence_test(X1, X2, nBootstrap = 100)
#' print(result)
#' plot(result)
#'
#' @export
#'
perform_independence_test <- function(
    X1, X2,
    my_grid = NULL,
    nBootstrap = 100,
    bootstrapOptions = NULL)
{

  # Initialize default values for the bootstrap options
  type_boot_user = "indep"
  type_stat_user = "eq"
  norm_type_user = "KS"

  # Read in the `bootstrapOptions` and set the user-specified options
  if (is.list(bootstrapOptions) && length(bootstrapOptions) > 0){
    if ("type_boot" %in% names(bootstrapOptions)){
      type_boot_user = bootstrapOptions$type_boot
    }
    if ("type_stat" %in% names(bootstrapOptions)){
      type_stat_user = bootstrapOptions$type_stat
    }
    if ("norm_type" %in% names(bootstrapOptions)){
      norm_type_user = bootstrapOptions$norm_type
    }
    if ( !all(names(bootstrapOptions) %in% c( "type_boot", "type_stat", "norm_type" )) ){
      stop("Please provide correct argument names for `bootstrapOptions`.
            Valid names are: 'type_boot', 'type_stat', and 'norm_type'.")
    }
  } else if (!is.list(bootstrapOptions) &&
             !is.null(bootstrapOptions) &&
             !is.character(bootstrapOptions)){
    stop("Invalid bootstrap options. Please check your inputs.")
  }


  # Checking the validity of the inputs
  if (length(X1) != length(X2)){
    stop("X1 and X2 must have the same length. Here the length of X1 is ",
         length(X1), " while the length of X2 is ", length(X2))
  }

  if (length(nBootstrap) > 1 || !is.finite(nBootstrap) || nBootstrap <= 0){
    stop("nBootstrap must be a positive integer of length 1.")
  }

  if (length(X1) < 1 || length(X2) < 1 ){
    stop("X1 and X2 must contain at least one entry.")
  }

  if ( is.numeric(X1) == FALSE || is.numeric(X2) == FALSE ){
    stop("X1 and X2 must be numeric vectors. Please check your input data.")
  }

  if (type_boot_user %in% c("indep", "NP") == FALSE){
    stop("Choose valid type_boot_user: either 'indep' or 'NP'. current input is ",
         type_boot_user)
  }

  if (type_stat_user %in% c("eq", "cent") == FALSE){
    stop("Choose valid type_stat_user: either 'eq' or 'cent'. Current input is",
         type_stat_user)
  }

  if (norm_type_user %in% c("KS","L2") == FALSE){
    stop("Choose valid norm_type_user: either 'KS' or 'L2'. Current input is ",
         norm_type_user)
  }

  if (!is.list(bootstrapOptions) &&
      !is.null(bootstrapOptions) &&
      bootstrapOptions == "all and also invalid"){
    warning("Using 'all and also invalid' as bootstrapOptions is not recommended. ",
            "This will return all theoretically valid and invalid combinations of ",
            "bootstrap resampling schemes, test statistics, and norms. ",
            "Please use with caution.")
  }

  if (is.character(bootstrapOptions) &&
      bootstrapOptions != "all and also invalid"  &&
      bootstrapOptions != "all"){
    warning("Invalid choice for bootstrapOptions. ",
            "Please choose either 'all' or 'all and also invalid'. Current input is",
            bootstrapOptions )
  }

  # Give warning for theoreotically invalid bootstrap schemes
  if (is.list(bootstrapOptions) && length(bootstrapOptions) > 0){
    if (type_boot_user == "indep" && type_stat_user == "cent"){
      warning("The combination of type_boot = 'indep' and type_stat = 'cent' ",
              "is theoretically invalid. The p-values will not be valid.")
    }
    if (type_boot_user == "NP" && type_stat_user == "eq"){
      warning("The combination of type_boot = 'NP' and type_stat = 'eq' ",
              "is theoretically invalid. The p-values will not be valid.")
    }
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

  # Only calculate all combinations of bootstrap resampling schemes if user
  # asks for them.

  if (!is.list(bootstrapOptions) &&
      !is.null(bootstrapOptions) &&
      (bootstrapOptions == "all and also invalid" ||
       bootstrapOptions == "all")
      )
  {

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

  } else if( (is.list(bootstrapOptions) && length(bootstrapOptions) > 0) ||
             is.null(bootstrapOptions)){
    # If the user specified a combination of bootstrap options or simply nothing,
    # we only calculate the bootstrap test statistics for the user-specified
    # bootstrap options. That is what happens in this case.


    # Make a list to store the results
    list_results = list()
    # Check the user-specified bootstrap options
    type_boot = type_boot_user

    #initialisation
    stat_st = rep(NA, nBootstrap)

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

      switch (norm_type_user,
              # If the user specified the L2 norm
              "L2" = {
                switch (type_stat_user,
                  "cent" = {
                    # centered test statistic
                    stat_st[iBootstrap] =
                      (sum((FX1FX2_st - FX1FX2 + FX12 - FX12_st)^2)) * sqrt(n)
                    },
                  "eq" = {
                    # equivalent test statistic
                    stat_st[iBootstrap] =
                      (sum((FX1FX2_st - FX12_st)^2)) * sqrt(n)
                  },
                  {
                    stop("Unknown type_stat_user. Please choose either 'cent' or 'eq'.")
                  }
                )
              },
              # If the user specified the KS norm
              "KS" = { switch( type_stat_user,
                  "cent" = {
                    # centered test statistic
                    stat_st[iBootstrap] =
                      max(abs(FX1FX2_st - FX1FX2 + FX12 - FX12_st))
                  },
                  "eq" = {
                    # equivalent test statistic
                    stat_st[iBootstrap] =
                      max(abs(FX1FX2_st - FX12_st))
                  },
                  # If the user specified an unknown type_stat_user
                  {
                    stop("Unknown type_stat_user. Please choose either 'cent' or 'eq'.")
                  }
                )
              },
              {
                stop("Unknown norm_type_user. Please choose either 'L2' or 'KS'.")
              }
      )
    }

    # Put dataframe in a list to make it coherent with the previous case of
    # creating all bootstrap resmapling schemes
    list_results[[1]] =
      data.frame(type_boot = type_boot,
                 type_stat = type_stat_user,
                 norm_type = norm_type_user,
                 bootstrapped_tests = I(list(stat_st) )
      )
  }

  # Post-processing ========================================================


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

  # Select the right rows based on the user-specified bootstrap options
  if ( !is.list(bootstrapOptions) &&
       !is.null(bootstrapOptions) &&
       bootstrapOptions == "all") {
    # Return only rows where `theoretically_valid` is TRUE
    pvals_df = pvals_df[pvals_df$theoretically_valid == TRUE, ]
  } else if (!is.list(bootstrapOptions) &&
             !is.null(bootstrapOptions) &&
             bootstrapOptions == "all and also invalid"){
    # Return all rows, including theoretically invalid combinations
    pvals_df = pvals_df
  } else if ( (is.list(bootstrapOptions) && length(bootstrapOptions) > 0) ||
             is.null(bootstrapOptions)){
    # If the user specified a combination of bootstrap options or simply nothing
    pvals_df = pvals_df[
      pvals_df$type_boot == type_boot_user &
      pvals_df$type_stat == type_stat_user &
      pvals_df$norm_type == norm_type_user, ]
  }

  result = ( list(
    # df of p-values
    pvals_df = pvals_df ,

    # true test statistics
    true_stats = true_stats ,

    # Include number of bootstrap repetitions
    nBootstrap = nBootstrap,

    nameMethod = "Bootstrap Independence Test"
    ) )

  # make a class for the result object
  class(result) <- c("bootstrapTest_independence", "bootstrapTest")
  return(result)
}

