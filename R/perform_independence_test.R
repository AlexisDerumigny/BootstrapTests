
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
compute_joint_ecdf <- function(X1, X2, my_grid1, my_grid2) {
  FX12_joint <- outer(my_grid1, my_grid2, Vectorize(function(x, y) {
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
#' @param my_grid the grid on which the CDFs are estimated. This must be one of
#' \itemize{
#'   \item \code{NULL}: a regularly spaced grid from the minimum value to the
#'   maximum value of each variable with \code{20} points is used. This is the
#'   default.
#'
#'   \item A numeric of size 1. This is used at the length of both grids, replacing
#'   \code{20} in the above explanation.
#'
#'   \item A numeric vector of size larger than 1. This is directly used as the
#'   grid for both variables.
#'
#'   \item A list of two numeric vectors, which are used as the grids for both
#'   variables \code{X1} and \code{X2} respectively.
#' }
#'
#' @param nBootstrap number of bootstrap repetitions.
#'
#' @param show_progress logical value indicating whether to show a progress bar
#'
#' @param bootstrapOptions This can be one of \itemize{
#'   \item \code{NULL} This uses the default options \code{type_boot = "indep"},
#'   \code{type_stat = "eq"} and \code{type_norm = "KS"}.
#'
#'   \item a list with at most 3 elements names \itemize{
#'         \item \code{type_boot} type of bootstrap resampling scheme. It must be
#'         one of
#'         \itemize{
#'            \item \code{"indep"} for the independence bootstrap
#'            (i.e. under the null). This is the default.
#'            \item \code{"NP"} for the non-parametric bootstrap
#'            (i.e. n out of n bootstrap).
#'         }
#'
#'         \item \code{type_stat}  type of test statistic to be used.  It must be
#'         one of
#'         \itemize{
#'           \item \code{"eq"} for the equivalent test statistic
#'           \deqn{T_n^* = \sqrt{n} || \hat{F}_{(X,Y)}^* - \hat{F}_{X}^* \hat{F}_{Y}^* ||}
#'
#'           \item \code{"cent"} for the centered test statistic
#'           \deqn{T_n^* = \sqrt{n} || \hat{F}_{(X,Y)}^* - \hat{F}_{X}^* \hat{F}_{Y}^*
#'           -  (\hat{F}_{(X,Y)} - \hat{F}_{X} \hat{F}_{Y}) ||}
#'         }
#'         For each \code{type_boot} there is only one valid choice of \code{type_stat}
#'         to be made. If \code{type_stat} is not specified, the valid choice is
#'         automatically used.
#'
#'         \item \code{type_norm} type of norm to be used for the test statistic.
#'          It must be one of
#'         \itemize{
#'            \item \code{"KS"} for the Kolmogorov-Smirnov type test statistic.
#'            This is the default. It is given as
#'            \deqn{
#'                T_n = \sqrt{n} \sup_{(x, y) \in \mathbb{R}\rule{0pt}{0.6em}^{p+q}}
#'                \big| \hat{F}_{(X,Y),n}(x , y) - \hat{F}_{X,n}(x) \hat{F}_{Y,n}(y)
#'                \big|
#'                }
#'            \item \code{"L2"} for the squared L2-norm test statistic.
#'            \deqn{ T_n = \sqrt{n}\int_{(x, y) \in
#'                  \mathbb{R}\rule{0pt}{0.6em}^{p+q}}
#'                  \big( \hat{F}_{(X,Y),n}(x , y) -
#'                  \hat{F}_{X,n}(x) \hat{F}_{Y,n}(y) \big)^2
#'                  \mathrm{d}x\mathrm{d}y
#'                }
#'         }
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
#'    X1, X2, nBootstrap = 50,
#'    bootstrapOptions = list(type_boot = "indep",
#'                            type_stat = "eq",
#'                            type_norm = "KS") )
#' print(result)
#' plot(result)
#'
#' # Under H0
#' X1 <- rnorm(n)
#' X2 <- rnorm(n)
#' result <- perform_independence_test(X1, X2, nBootstrap = 50)
#' print(result)
#' plot(result)
#'
#' @export
#'
perform_independence_test <- function(
    X1, X2,
    my_grid = NULL,
    nBootstrap = 100,
    show_progress = TRUE,
    bootstrapOptions = NULL)
{

  # Initialize default values for the bootstrap options
  type_boot_user = "indep"
  type_stat_user = "eq"
  type_norm_user = "KS"

  # Read in the `bootstrapOptions` and set the user-specified options
  if (is.list(bootstrapOptions) && length(bootstrapOptions) > 0){
    if ("type_boot" %in% names(bootstrapOptions)){
      type_boot_user = bootstrapOptions$type_boot
    }
    if ("type_stat" %in% names(bootstrapOptions)){
      type_stat_user = bootstrapOptions$type_stat
    } else {
      mapping = c(indep = "eq", NP = "cent")
      type_stat_user = mapping[type_boot_user]
    }
    if ("type_norm" %in% names(bootstrapOptions)){
      type_norm_user = bootstrapOptions$type_norm
    }
    if ( !all(names(bootstrapOptions) %in% c( "type_boot", "type_stat", "type_norm" )) ){
      stop("Please provide correct argument names for `bootstrapOptions`.
            Valid names are: 'type_boot', 'type_stat', and 'type_norm'.")
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

  if (type_norm_user %in% c("KS","L2") == FALSE){
    stop("Choose valid type_norm_user: either 'KS' or 'L2'. Current input is ",
         type_norm_user)
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
    my_grid = 20
  }
  if (is.numeric(my_grid) && length(my_grid) == 1){
    my_grid1 = seq(min(X1), max(X1), length.out = my_grid)
    my_grid2 = seq(min(X2), max(X2), length.out = my_grid)
  } else if (is.numeric(my_grid)) {
    my_grid1 = my_grid
    my_grid2 = my_grid
  } else if (is.list(my_grid)) {
    my_grid1 = my_grid[[1]]
    my_grid2 = my_grid[[2]]
  }

  # Estimation of the product of the marginal CDFs
  FX1 = stats::ecdf(X1)(my_grid1)
  FX2 = stats::ecdf(X2)(my_grid2)
  FX1FX2 = outer(FX1, FX2)

  # Compute joint ecdf
  FX12 <- compute_joint_ecdf(X1, X2, my_grid1, my_grid2)

  # Vector containing all the true test statistics, for the different norms
  true_stats = c("L2" = (sum((FX1FX2 - FX12)^2)) * sqrt(n) ,
                 # Kolmogorov-Smirnov test statistic
                 "KS" = max(abs(FX1FX2 - FX12))  * sqrt(n) )


  # Bootstrapping ===========================================================

  # Only calculate all combinations of bootstrap resampling schemes if user
  # asks for them.

  if (!is.list(bootstrapOptions) &&
      !is.null(bootstrapOptions) &&
      (bootstrapOptions == "all and also invalid" ||
       bootstrapOptions == "all")
  )
  {
    # Initialisation
    list_results = list()
    vec_type_boot = c("indep", "NP")

    if (show_progress){
      # Progress bar
      total_steps <- length(vec_type_boot) * nBootstrap
      pb <- pbapply::startpb(min = 0, max = total_steps)
      step <- 0
    }

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
        FX1_st = stats::ecdf(X1_st)(my_grid1)
        FX2_st = stats::ecdf(X2_st)(my_grid2)
        FX1FX2_st = outer(FX1_st, FX2_st)

        # Empirical joint CDF on the bootstrap data
        FX12_st <- compute_joint_ecdf(X1_st, X2_st, my_grid1, my_grid2)


        # Calculate the test statistics based on the bootstrap data =========

        if (bootstrapOptions == "all" && type_boot == "indep"){

          # only calculate theoretically valid options in this case
          stat_st_eq_L2[iBootstrap] = (sum((FX1FX2_st - FX12_st)^2)) * sqrt(n)
          stat_st_eq_KS[iBootstrap] = max(abs(FX1FX2_st - FX12_st)) * sqrt(n)

        } else if (bootstrapOptions == "all" && type_boot == "NP"){

          # only calculate theoretically valid options in this case
          stat_st_cent_L2[iBootstrap] =
            (sum((FX1FX2_st - FX1FX2 + FX12 - FX12_st)^2)) * sqrt(n)
          stat_st_cent_KS[iBootstrap] =
            max(abs(FX1FX2_st - FX1FX2 + FX12 - FX12_st)) * sqrt(n)

        } else {
          stat_st_cent_L2[iBootstrap] =
            (sum((FX1FX2_st - FX1FX2 + FX12 - FX12_st)^2)) * sqrt(n)

          stat_st_cent_KS[iBootstrap] =
            max(abs(FX1FX2_st - FX1FX2 + FX12 - FX12_st)) * sqrt(n)

          stat_st_eq_L2[iBootstrap] = (sum((FX1FX2_st - FX12_st)^2)) * sqrt(n)

          stat_st_eq_KS[iBootstrap] = max(abs(FX1FX2_st - FX12_st)) * sqrt(n)

        }

        if (show_progress){
          # Update progress bar
          step <- step + 1
          pbapply::setpb(pb, step)
        }
      }



      # Storing the bootstrap test stats in list ============================

      if (bootstrapOptions == "all" && type_boot == "indep"){

        df_new <- data.frame(type_boot = type_boot,
                             type_stat = "eq",
                             type_norm = c("L2", "KS"),
                             list_stat_st = I(list(stat_st_eq_L2,
                                                   stat_st_eq_KS) ) )

        # Append new dataframe to the list
        list_results <- append(list_results, list(df_new))
      } else if (bootstrapOptions == "all" && type_boot == "NP"){

        df_new <- data.frame(type_boot = type_boot,
                             type_stat = "cent",
                             type_norm = c("L2", "KS"),
                             list_stat_st = I(list(stat_st_cent_L2,
                                                   stat_st_cent_KS) ) )

        # Append new dataframe to the list
        list_results <- append(list_results, list(df_new))

      } else {
        # Put dataframes in a list, alternating the entries
        list_results[[1 + (iBoot - 1)*2]] =
          data.frame(type_boot = type_boot,
                     type_stat = "cent",
                     type_norm = c("L2", "KS"),
                     list_stat_st = I(list(stat_st_cent_L2,
                                           stat_st_cent_KS) ) )
        list_results[[2 + (iBoot - 1)*2]] =
          data.frame(type_boot = type_boot,
                     type_stat = "eq",
                     type_norm = c("L2", "KS"),
                     list_stat_st = I(list(stat_st_eq_L2,
                                           stat_st_eq_KS) ) )
      }
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

    if (show_progress){
      # Progress bar
      total_steps <-  nBootstrap
      pb <- pbapply::startpb(min = 0, max = total_steps)
      step <- 0
    }

    #initialisation
    stat_st = rep(NA, nBootstrap)

    for (iBootstrap in 1:nBootstrap){
      # Generation of the bootstrapped data
      dataBoot = generateBootstrapSamples(X1 = X1, X2 = X2,
                                          type_boot = type_boot)
      X1_st = dataBoot$X1_st
      X2_st = dataBoot$X2_st

      # Estimation of the product of the marginal CDFs
      FX1_st = stats::ecdf(X1_st)(my_grid1)
      FX2_st = stats::ecdf(X2_st)(my_grid2)
      FX1FX2_st = outer(FX1_st, FX2_st)

      # Empirical joint CDF on the bootstrap data
      FX12_st <- compute_joint_ecdf(X1_st, X2_st, my_grid1, my_grid2)

      switch (type_norm_user,
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
                                   max(abs(FX1FX2_st - FX1FX2 + FX12 - FX12_st)) * sqrt(n)
                               },
                               "eq" = {
                                 # equivalent test statistic
                                 stat_st[iBootstrap] =
                                   max(abs(FX1FX2_st - FX12_st)) * sqrt(n)
                               },
                               # If the user specified an unknown type_stat_user
                               {
                                 stop("Unknown type_stat_user. Please choose either 'cent' or 'eq'.")
                               }
              )
              },
              {
                stop("Unknown type_norm_user. Please choose either 'L2' or 'KS'.")
              }
      )
      if (show_progress){
        # Update progress bar
        step <- step + 1
        pbapply::setpb(pb, step)
      }

    }

    # Put dataframe in a list to make it coherent with the previous case of
    # creating all bootstrap resmapling schemes
    df_new <- data.frame(type_boot = type_boot,
                         type_stat = type_stat_user,
                         type_norm = type_norm_user,
                         list_stat_st = I(list(stat_st) ))

    list_results <- append(list_results, list(df_new))
  }

  if (show_progress){
    # close progress bar
    pbapply::closepb(pb)
  }

  # Post-processing ========================================================

  # Rowbind the dataframes in `list_results` into a large dataframe
  pvals_df = do.call(what = rbind, args = list_results)

  # Compute pvalues
  list_pvalues = lapply(
    X = 1:nrow(pvals_df),
    FUN = function(i){
      pval = mean(as.numeric(
        true_stats[pvals_df$type_norm[i]] < pvals_df$list_stat_st[i][[1]]
      ) )
    }
  )
  pvals_df$pvalues = unlist(list_pvalues)

  # Add column to denote the theoretically valid combinations of bootstrap
  pvals_df$theoretically_valid =
    (pvals_df$type_boot == "indep" & pvals_df$type_stat == "eq")  |
    (pvals_df$type_boot == "NP"    & pvals_df$type_stat == "cent")

  # No rownames for the moment (maybe later if needed?)
  row.names(pvals_df) <- NULL

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

