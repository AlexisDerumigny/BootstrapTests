
#' This function creates initial guesses for parameter estimation for different
#' parametric distributions
#'
#' @param data dataset
#' @param parametric_fam name of the parametric family
#'
#' @returns a named vector of initial guess for distribution parameters
#'
#' @noRd
#'
estimate_params <- function(data, parametric_fam = "normal"){

  switch (parametric_fam,
          "normal" = {
            initial_params = c( mu = mean(data), sigma = stats::sd(data) )
          },

          {
            stop("unknown parametric family for which to generate initial params")
          }
  )
  return(initial_params)
}


#' Return the evaluated cdf of the parametric distribution on the
#' (vector-valued) grid_points.
#' This fitted distributions has parameters in the `param` vector
#'
#' @returns list of fitted_cdf_vals and generated_vals, the evaluated CDF values
#' at the observed_data and n random draws from the parametric CDF.
#'
#' @noRd
param_distr <- function(grid_points, parametric_fam = "normal", param){
  # Input checks
  if( anyNA(param) ){
    stop("It is necessary to input (estimated) paramaters in `param` ")
  }
  stopifnot(is.vector(grid_points) && is.numeric(grid_points) )
  stopifnot( sum(is.finite(param))==length(param) && is.numeric(param) )

  n = length(grid_points)

  if(parametric_fam == "normal"){
    fitted_mu = param[[1]]
    fitted_sigma = param[[2]]
    # If sigma is negative, set it to 1
    if(fitted_sigma < 0) {fitted_sigma = abs(fitted_sigma)}
    fitted_cdf_vals <- stats::pnorm(grid_points,
                                    mean = fitted_mu, sd = fitted_sigma)
    generated_vals <- stats::rnorm(n, mean = fitted_mu, sd = fitted_sigma)
    if( is.na(fitted_mu) | is.na(fitted_sigma) | anyNA(fitted_cdf_vals) | anyNA(generated_vals) ){
      print(c(fitted_mu, fitted_sigma))
      print(grid_points)
    }
  } else {
    stop("unknown parametric family")
  }

  return(list(fitted_cdf_vals = fitted_cdf_vals,
              generated_vals = generated_vals))
}


#' Calculate the infinity norm on the grid_points, given observed_data
#' and with a parametrized distribution with `params` as specific parameters.
#'
#' @returns `inf_norm`, the calculated infinity-norm between the ecdf and
#' the parametrized distribution with params as parameters.
#'
#' @noRd
infinity_norm_distance <- function(grid_points, observed_data,
                                   parametric_fam  = "normal", params) {

  # Calculate the empirical CDF values at grid_points, after fitting it on
  # the observed_data
  empirical_cdf_values <- stats::ecdf(observed_data)(grid_points)

  # Calculate the parametrized CDF values at these points
  parametrized_cdf_values <- param_distr(grid_points, parametric_fam = parametric_fam,
                                         params)$fitted_cdf_vals

  # Calculate the infinity norm (sup norm): maximum absolute difference
  inf_norm <- max(abs(empirical_cdf_values - parametrized_cdf_values))

  return(inf_norm)
}


#' Calculate the infinity norm for minimum distance estimator on grid_pionts,
#' based on observed data and with a parametrized family with `params` as
#' specific parameters. The bootstrap data `bs_data` and the `params_st` the
#' parameters for the bootstrap parametric distribution.
#' @returns `inf_norm_MD`, the centered infinity-norm used to calculate
#'  \eqn{theta_hat_n^{*,MD}} in the paper.
#'
#' @noRd
#'
infinity_norm_distance_MD <- function(grid_points, bs_data,
                                      observed_data, params, params_st,
                                      parametric_fam = "normal") {

  # Calculate the empirical CDF values at these points
  empirical_cdf_values <- stats::ecdf(observed_data)(grid_points)
  empirical_cdf_values_st <- stats::ecdf(bs_data)(grid_points)

  # Calculate the parametrized CDF values at these points
  parametrized_cdf_values <- param_distr(grid_points, parametric_fam = parametric_fam,
                                         params)$fitted_cdf_vals

  parametrized_cdf_values_st <- param_distr(grid_points, parametric_fam = parametric_fam,
                                            params_st)$fitted_cdf_vals

  # Calculate the infinity norm (sup norm): maximum absolute difference
  inf_norm_MD <- max(abs(empirical_cdf_values_st - parametrized_cdf_values_st
                         - empirical_cdf_values + parametrized_cdf_values))

  return(inf_norm_MD)
}


#' This function generates bootstrap samples for the GoF testing, by
#' inputting the data and the type_boot (the type of bootstrap to be performed).
#' Furthermore, in case of parametric "null" bootstrap, parameters need to be
#' inputted as well.
#'
#' @noRd
#'
generateBootstrapSamples_GOF <- function(X_data, type_boot, param = NA,
                                         parametric_fam = "normal"){

  # Input checks
  if( type_boot == "null" & anyNA(param) ){
    stop("For the null bootstrap you need estimated params")
  }
  stopifnot(type_boot == "null" | type_boot == "NP" )
  stopifnot(is.vector(X_data) && is.numeric(X_data) )

  # Extract sample size form X_data
  n = length(X_data)

  switch (
    type_boot,

    "null" = {
      estim_distr <- param_distr(grid_points = X_data, parametric_fam = parametric_fam, param)
      X_st = estim_distr$generated_vals
    },

    "NP" = {
      permutation = sample.int(n, replace = TRUE)
      X_st = X_data[permutation]
    },

    {
      stop("unknown type_boot")
    }
  )
  return (X_st)
}



#' Perform a univariate GoF hypothesis test via bootstrap resampling
#'
#' This function performs a bootstrap goodness-of-fit hypothesis test for a
#' specific univariate parametric family. The null hypothesis corresponds to the
#' sample coming from the specified parametric family, while the alternative
#' hypothesis corresponds to the sample not coming from the specified
#' parametric family. This function implements a null bootstrap and
#' a non-parametric bootstrap. The test statistic is the Kolmogorov-Smirnov test
#' statistic. To estimate the parameters of the parametric family, either a minimum
#' distance estimator, or a canonical estimator (the sample mean and variance)
#' is used. On the bootstrap sample, we have also implemented a centered MD estimator,
#' as in the paper. For now, only a test of normality is implemented. This function
#' gives the corresponding p-values, the true test statistic and the
#' bootstrap-version test statistics. The default (and valid) method implemented
#' in this function is the null bootstrap, together with the equivalent test statistic
#' and the canonical parameter estimator. Via the \code{bootstrapOptions}
#' argument, the user can specify other bootstrap resampling schemes,
#' test statistics, and parameter estimators.
#'
#'
#' @param X_data numerical input vector. Perform a GoF test whether or not this
#' sample comes from \code{"parametric_fam"}, a specified parametric distribution.
#'
#' @param parametric_fam name of the parametric family. For the moment, only
#' \code{"normal"} is supported.
#'
#' @param nBootstrap numeric value of the number of bootstrap resamples. Defaults
#' to 100.
#'
#' @param bootstrapOptions This can be one of \itemize{
#'   \item \code{NULL}
#'
#'   \item a list with at most 3 elements names \itemize{
#'         \item \code{type_boot} defaults to the \code{"null"} bootstrap
#'         resampling scheme to be used. \code{type_boot} can be either
#'         \code{"null"} for the null/parametric bootstrap, or \code{"NP"} for the
#'         non-parametric bootstrap.
#'
#'         \item \code{type_stat} defaults to \code{"eq"} for the type of test
#'         statistic to be used. This can be either \code{"eq"} for the
#'         equivalent test statistic, or \code{"cent"} for the centered
#'         test statistic.
#'
#'         \item \code{param_bs} defaults to \code{"canonical"} for the bootstrap
#'         parameter estimator to be used. \code{param_bs} can be either
#'         \code{"MD"} for the Minimum Distance estimator, \code{"MD-cent"} for
#'         the centered Minimum Distance estimator, or \code{"canonical"}
#'         for the canonical estimator (empirical mean and variance).
#'   }
#'   \item \code{"all"} this gives test results for all theoretically valid
#'   combinations of bootstrap resampling schemes.
#'
#'   \item \code{"all and also invalid"} this gives test results for all possible
#'   combinations of bootstrap resampling schemes and test statistics, including
#'   invalid ones.
#' }
#' A warning is raised if the given combination of \code{type_boot},
#' \code{type_stat}, and \code{param_bs} is theoretically invalid.
#'
#' @return A class object with components \itemize{
#'    \item \code{pvals_df} a dataframe of p-values and bootstrapped test statistics:
#'
#'    These are the p-values for the combinations of bootstrap resampling schemes,
#'    test statistics (centered and equivalent), and different parameter estimators.
#'
#'    It also contains the vectors of bootstrap test statistics
#'    for each of these combinations.
#'
#'    \item \code{true_stat} a named vector of size 2 containing the true test
#'    statistics. The first entry is the Kolmogorov-Smirnov test statistic for
#'    the Minimum Distance estimator, and the second entry is the Kolmogorov-Smirnov
#'    test statistic for the canonical parameter estimator.
#'
#'    \item \code{nBootstrap} number of bootstrap repetitions.
#'
#'    \item \code{nameMethod} string for the name of the method used.
#'
#' }
#'
#' @seealso \code{\link{perform_regression_test},\link{perform_independence_test}}
#'
#' @examples
#' n <- 100
#' # Under H1
#' X_data <- rgamma(n,2,3)
#' result <- perform_GoF_test(X_data,
#'                           nBootstrap = 100,
#'                           bootstrapOptions = list(type_boot = "null",
#'                                                   type_stat = "eq",
#'                                                   param_bs = "canonical")
#'                          )
#' print(result)
#' plot(result)
#'
#' # Under H0
#' X_data <- rnorm(n)
#' result <- perform_GoF_test(X_data, nBootstrap = 100)
#' print(result)
#' plot(result)
#'
#' @export
#'
perform_GoF_test <- function(X_data,
                             parametric_fam = "normal",
                             nBootstrap = 100,
                             bootstrapOptions = NULL)
{

  # Initialize default values for the bootstrap options
  type_boot_user = "null"
  type_stat_user = "eq"
  param_bs_user = "canonical"

  # Read in the `bootstrapOptions` and set the user-specified options
  if (is.list(bootstrapOptions) && length(bootstrapOptions) > 0){
    if ("type_boot" %in% names(bootstrapOptions)){
      type_boot_user = bootstrapOptions$type_boot
    }
    if ("type_stat" %in% names(bootstrapOptions)){
      type_stat_user = bootstrapOptions$type_stat
    }
    if ("param_bs" %in% names(bootstrapOptions)){
      param_bs_user = bootstrapOptions$param_bs
    }
    if ( !all(names(bootstrapOptions) %in% c( "type_boot", "type_stat", "param_bs" )) ){
      stop("Please provide correct argument names for `bootstrapOptions`.
            Valid names are: 'type_boot', 'type_stat', and 'param_bs'. ")
    }
  } else if (!is.list(bootstrapOptions) &&
             !is.null(bootstrapOptions) &&
             !is.character(bootstrapOptions)){
    stop("Invalid bootstrap options. Please check your inputs.")
  }


  # Checking the validity of the inputs
  if (length(nBootstrap) > 1 || !is.finite(nBootstrap) || nBootstrap <= 0){
    stop("nBootstrap must be a positive integer of length 1.")
  }

  if (length(X_data) < 1 ){
    stop("X_data must contain at least one entry.")
  }

  if ( !is.numeric(X_data) ){
    stop("X_data must be a numeric vector. Please check your input data.")
  }

  if (parametric_fam != "normal"){
    stop("parametric_fam can only be the normal family.")
  }

  if (type_boot_user %in% c("null", "NP") == FALSE){
    stop("Choose valid type_boot_user: either 'null' or 'NP'. Current input is ",
         type_boot_user)
  }

  if(type_stat_user %in% c("eq", "cent") == FALSE){
    stop("Choose valid type_stat: either 'eq' or 'cent'. Current input is",
         type_stat_user)
  }

  if (param_bs_user %in% c("MD", "MD-cent", "canonical") == FALSE){
    stop("Choose valid param_bs_user: either 'MD', 'MD-cent' or 'canonical'.
         Current input is", param_bs_user)
  }

  if (!is.list(bootstrapOptions) &&
      !is.null(bootstrapOptions) &&
      bootstrapOptions == "all and also invalid"){
    warning("Using 'all and also invalid' as bootstrapOptions is not recommended. ",
            "This will return all theoretically valid and invalid combinations of ",
            "bootstrap resampling schemes, and test statistics. ",
            "Please use with caution.")
  }

  if (is.character(bootstrapOptions) &&
      bootstrapOptions != "all and also invalid"  &&
      bootstrapOptions != "all"){
    warning("Invalid choice for bootstrapOptions. ",
            "Please choose either 'all' or 'all and also invalid'. Current input is",
            bootstrapOptions )
  }


  # Give warning for theoretically invalid bootstrap schemes
  if (is.list(bootstrapOptions) && length(bootstrapOptions) > 0){
    if (type_boot_user == "null" && type_stat_user == "cent"){
      warning("The combination of type_boot = 'null' and type_stat = 'cent' ",
              "is theoretically invalid. The p-values will not be valid.")
    }
    if (type_boot_user == "NP" && type_stat_user == "eq"){
      warning("The combination of type_boot = 'NP' and type_stat = 'eq' ",
              "is theoretically invalid. The p-values will not be valid.")
    }
    if (type_boot_user == "res_bs" && type_stat_user == "eq"){
      warning("The combination of type_boot = 're_bs' and type_stat = 'eq' ",
              "is theoretically invalid. The p-values will not be valid.")
    }
    if (type_boot_user == "NP" &&
        type_stat_user == "cent" &&
        param_bs_user == "MD"){
      warning("The combination of type_boot = 'NP', type_stat = 'cent', ",
              "and `param_bs` = 'MD' is theoretically invalid.
              The p-values will not be valid.")
    }
  }

  # Computation of the original statistics ===========================

  # TODO: make better grid
  # Grid points
  grid_points <- seq(min(X_data), max(X_data), length.out = 100)

  # Define sample size
  n <- length(X_data)

  # TODO: for other GoF-tests, change initial estimation parameters
  # Estimate unknown distribution parameters to minimize the norm distance
  # Initial guesses for the parameters
  initial_params <- estimate_params(X_data)

  # Use the 'stats::optim' function to minimize the infinity norm between ecdf and
  # parametric cdf. This gives parameter estimates in the Minimum Distance setting.
  fit <- stats::optim(initial_params, infinity_norm_distance,
                      observed_data = X_data,
                      grid_points = grid_points,
                      parametric_fam = parametric_fam)

  # Extract the fitted parameters (for normal family the mean and variance)
  estimated_param <- fit$par

  # Use standard empirical mean and variance as estimates
  estimated_param_canonical <- estimate_params(X_data, parametric_fam)

  # Calculate the empirical CDF values at the grid of X points
  ecdf_values <- stats::ecdf(X_data)(grid_points)

  # Calculate the parametrised CDF values at the grid of X points
  parametrized_cdf_values <- param_distr(grid_points = grid_points,
                                         parametric_fam = parametric_fam,
                                         param = estimated_param )$fitted_cdf_vals

  # Calculate the parametrised CDF values at the grid of X points
  parametrized_cdf_values_canonical <- param_distr(grid_points = grid_points,
                                                   parametric_fam = parametric_fam,
                                                   param = estimated_param_canonical )$fitted_cdf_vals

  # Calculate the infinity norm (sup norm): maximum absolute difference
  max_diff <- max(abs(ecdf_values - parametrized_cdf_values))
  max_diff_canonical <- max(abs(ecdf_values - parametrized_cdf_values_canonical))


  # Vector containing true test statistics, for the supremum norm
  true_stat = c(# Kolmogorov-Smirnov test statistic, with sqrt(n)
    "KS_with_MD" = max_diff*sqrt(n),
    "KS_with_canonical" = max_diff_canonical*sqrt(n)
  )


  # Bootstrapping ===========================================================

  list_results = list()

  vec_type_boot = c("null", "NP")

  for (iBoot in 1:length(vec_type_boot)){
    type_boot = vec_type_boot[iBoot]

    #initialisation
    stat_st_cent = rep(NA, nBootstrap)
    stat_st_eq  = rep(NA, nBootstrap)
    stat_st_cent_MD = rep(NA, nBootstrap)
    stat_st_eq_MD  = rep(NA, nBootstrap)
    stat_st_cent_canonical = rep(NA, nBootstrap)
    stat_st_eq_canonical  = rep(NA, nBootstrap)


    for (iBootstrap in 1:nBootstrap){
      # Generation of the bootstrapped data
      X_st <- generateBootstrapSamples_GOF(X_data,
                                           type_boot = type_boot,
                                           param = estimated_param)
      X_st_canonical <- generateBootstrapSamples_GOF(X_data,
                                                     type_boot = type_boot,
                                                     param = estimated_param_canonical)

      ## Estimate unknown distribution parameters to minimize the norm distance ##

      # Initial guesses for the mean and sd parameters
      initial_params_st <- estimate_params(X_st)

      # Use the 'stats::optim' function to minimize the dist. funct for the param. distri.
      fit_st <- stats::optim(initial_params_st, infinity_norm_distance,
                             grid_points = grid_points,
                             observed_data = X_st,
                             parametric_fam = parametric_fam)

      # Fitting the centered MD estimator for the NP bootstrap scheme
      fit_st_MD <- stats::optim(initial_params_st, infinity_norm_distance_MD,
                                grid_points = grid_points,
                                bs_data = X_st,
                                observed_data = X_data,
                                params = estimated_param,
                                parametric_fam = parametric_fam)

      # Extract the fitted `bootstrap-based` parameters
      estimated_param_st <- fit_st$par
      estimated_param_st_MD <- fit_st_MD$par
      estimated_param_st_canonical <- estimate_params(X_st_canonical,
                                                      parametric_fam)

      # Calculate the empirical CDF values at the grid of X_st points, after
      # fitting it on the X_st data (bootstrap data)
      ecdf_values_st <- stats::ecdf(X_st)(grid_points)
      ecdf_values_st_canonical <- stats::ecdf(X_st_canonical)(grid_points)

      # Calculate the parametric CDF values at the grid of X_st points
      parametrized_cdf_values_st <- param_distr(grid_points = grid_points,
                                                parametric_fam = parametric_fam,
                                                param = estimated_param_st )$fitted_cdf_vals

      parametrized_cdf_values_st_MD <- param_distr(grid_points,
                                                   parametric_fam = parametric_fam,
                                                   estimated_param_st_MD )$fitted_cdf_vals

      parametrized_cdf_values_st_canonical <- param_distr(grid_points,
                                                          parametric_fam = parametric_fam,
                                                          param = estimated_param_st_canonical )$fitted_cdf_vals

      # Calculate the infinity norm (sup norm): maximum absolute difference
      max_diff_cent_st <- max(abs(ecdf_values_st - parametrized_cdf_values_st
                                  - ecdf_values +  parametrized_cdf_values ))

      max_diff_cent_st_MD <- max(abs(ecdf_values_st
                                     - parametrized_cdf_values_st_MD
                                     - ecdf_values
                                     +  parametrized_cdf_values ))

      max_diff_eq_st <- infinity_norm_distance(grid_points,
                                               X_st,
                                               estimated_param_st,
                                               parametric_fam = parametric_fam)

      max_diff_eq_st_MD <- infinity_norm_distance(grid_points,
                                                  X_st,
                                                  estimated_param_st_MD,
                                                  parametric_fam = parametric_fam)
      max_diff_eq_st_canonical <- infinity_norm_distance(grid_points,
                                                         X_st_canonical,
                                                         estimated_param_st_canonical,
                                                         parametric_fam = parametric_fam)

      max_diff_cent_st_canonical <- max(abs(ecdf_values_st_canonical
                                            - parametrized_cdf_values_st_canonical
                                            - ecdf_values
                                            + parametrized_cdf_values_canonical ))

      # Calculating bootstrap test statistics
      stat_st_cent[iBootstrap]            = max_diff_cent_st * sqrt(n)
      stat_st_eq[iBootstrap]              = max_diff_eq_st * sqrt(n)
      stat_st_cent_MD[iBootstrap]         = max_diff_cent_st_MD * sqrt(n)
      stat_st_eq_MD[iBootstrap]           = max_diff_eq_st_MD * sqrt(n)
      stat_st_cent_canonical[iBootstrap]  = max_diff_cent_st_canonical * sqrt(n)
      stat_st_eq_canonical[iBootstrap]    = max_diff_eq_st_canonical * sqrt(n)

    }

    # Put dataframes in a list, alternating the entries
    # `param_bs` corresponds to the (MD) bootstrap parameter estimator, so either
    # the Minimum Distance (MD) or the canonical estimator.
    list_results[[1 + (iBoot - 1)*2]] =
      data.frame(type_boot = type_boot,
                 type_stat = "cent",
                 param_bs = c("MD", "MD-cent", "canonical"),
                 bootstrapped_tests = I(list(stat_st_cent,
                                             stat_st_cent_MD,
                                             stat_st_cent_canonical) )
      )
    list_results[[2 + (iBoot - 1)*2]] =
      data.frame(type_boot = type_boot,
                 type_stat = "eq",
                 param_bs = c("MD", "MD-cent", "canonical"),
                 bootstrapped_tests = I(list(stat_st_eq,
                                             stat_st_eq_MD,
                                             stat_st_eq_canonical) )
      )
  }

  # Rowbind the dataframes in `list_results` into a dataframe
  pvals_df = do.call(what = rbind, args = list_results)

  # Calculate pvalues
  # list apply to the list `pvalues` and use `|> unlist()` to unlist it
  pvals_df$pvalues = lapply(
    X = 1:nrow(pvals_df),
    FUN = function(i){
      # Choose the appropriate true_stat based on the value of param_bs
      true_stat_to_use <- if (pvals_df$param_bs[i] == "canonical") {
        true_stat["KS_with_canonical"]
      } else {
        true_stat["KS_with_MD"]
      }
      pval = mean(as.numeric(
        true_stat_to_use < pvals_df$bootstrapped_tests[i][[1]]
      ) )
      return(pval)
    }
  ) |> unlist()

  # The NP and centred test statistic also needs
  # a centred MD bootstrap parameter estimator.
  pvals_df$theoretically_valid =
    (pvals_df$type_boot == "null" &
       pvals_df$type_stat == "eq" &
       pvals_df$param_bs == "MD")  |
    (pvals_df$type_boot == "NP" &
       pvals_df$type_stat == "cent" &
       pvals_df$param_bs == "MD-cent") |
    (pvals_df$type_boot == "null" &
       pvals_df$type_stat == "eq" &
       pvals_df$param_bs == "canonical")|
    (pvals_df$type_boot == "NP" &
       pvals_df$type_stat == "cent" &
       pvals_df$param_bs == "canonical")

  ### post-processing ###

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
  } else if (is.list(bootstrapOptions) && length(bootstrapOptions) > 0 ||
             is.null(bootstrapOptions)){
    # If the user specified a combination of bootstrap options or simply nothing
    pvals_df = pvals_df[
      pvals_df$type_boot == type_boot_user &
        pvals_df$type_stat == type_stat_user &
        pvals_df$param_bs == param_bs_user, ]
  }


  ### Create the result object ###
  result <- list(
    # df of p-values
    pvals_df = pvals_df,
    # true test statistics
    true_stats = true_stat,
    # Include number of bootstrap repetitions
    nBootstrap = nBootstrap,
    # give bootstrap method a name
    nameMethod = "Bootstrap GoF Test"
  )

  # make a class for the result object
  class(result) <- c("bootstrapTest_GoF", "bootstrapTest")
  return(result)
}
