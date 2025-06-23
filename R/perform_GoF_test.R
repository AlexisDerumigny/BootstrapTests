
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
generate_initial_params <- function(data, parametric_fam = "normal"){

  switch (parametric_fam,
          "normal" = {
            initial_params = c( mu = mean(data), sigma = sd(data) )
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
    fitted_cdf_vals <- pnorm(grid_points,
                             mean = fitted_mu, sd = fitted_sigma)
    generated_vals <- rnorm(n, mean = fitted_mu, sd = fitted_sigma)
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
  empirical_cdf_values <- ecdf(observed_data)(grid_points)

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
  empirical_cdf_values <- ecdf(observed_data)(grid_points)
  empirical_cdf_values_st <- ecdf(bs_data)(grid_points)

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



#' Perform a GoF test
#'
#' This function performs a goodness-of-fit test for a specific univariate
#' parametric family.
#'
#'
#' @param X_data, numerical vector of the same size. the GoF tests whether this
#' sample comes from `parametric_fam`, a specified parametric distribution.
#'
#' @param parametric_fam name of the parametric family. For the moment, only
#' `normal` is supported.
#'
#' @param nBootstrap number of bootstrap repetitions.
#'
#' @return A list with components \itemize{
#'    \item \code{pvals_df} df of p-values and bootstrapped test statistics:
#'
#'    These are the p-values for the combinations of bootstrap resampling schemes
#'    (nonparametric and null), test statistics (centered and equivalent),
#'
#'    It also contains the vectors of bootstrap test statistics
#'    for each of the combinations.
#'
#'    \item \code{true_stat} a named vector of size 1 containing the true test
#'    statistic.
#' }
#'
#' @examples
#' n = 100
#'
#' # Under H1
#' X_data = rgamma(n,2,3)
#' result = GoF_test(X_data, nBootstrap = 30)
#' result$pvals_df
#' #
#' # Under H0
#' X_data = rnorm(n)
#' result = GoF_test(X_data, nBootstrap = 30)
#' result$pvals_df
#'
#' @export
#'
perform_GoF_test <- function(X_data, parametric_fam = "normal", nBootstrap)
{
  # Checking the validity of the inputs
  if (length(nBootstrap) > 1 || !is.finite(nBootstrap) || nBootstrap <= 0){
    stop("nBootstrap must be a positive integer of length 1.")
  }

  if (length(X_data) < 1 ){
    stop("X_data must contain at least one entry.")
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
  initial_params <- generate_initial_params(X_data)

  # Use the 'optim' function to minimize the infinity norm between ecdf and
  # parametric cdf. This gives parameter estimates in the Minimum Distance setting.
  fit <- optim(initial_params, infinity_norm_distance,
               observed_data = X_data,
               grid_points = grid_points,
               parametric_fam = parametric_fam)

  # Extract the fitted parameters (for normal family the mean and variance)
  estimated_param <- fit$par

  # Calculate the empirical CDF values at the grid of X points
  ecdf_values <- ecdf(X_data)(grid_points)

  # Calculate the parametrised CDF values at the grid of X points
  parametrized_cdf_values <- param_distr(grid_points = grid_points,
                                         parametric_fam = parametric_fam,
                                         param = estimated_param )$fitted_cdf_vals

  # Calculate the infinity norm (sup norm): maximum absolute difference
  max_diff <- max(abs(ecdf_values - parametrized_cdf_values))

  # Vector containing true test statistics, for the supremum norm
  true_stat = c(# Kolmogorov-Smirnov test statistic, with sqrt(n)
    "sup" = max_diff*sqrt(n)          )


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


    for (iBootstrap in 1:nBootstrap){
      # Generation of the bootstrapped data
      dataBoot = generateBootstrapSamples_GOF(X_data,
                                              type_boot = type_boot,
                                              param = estimated_param)
      # Extract bootstrap sample
      X_st = dataBoot

      # Estimate unknown distribution parameters to minimize the norm distance
      # Initial guesses for the mean and sd parameters
      initial_params_st <- generate_initial_params(X_st)

      # Use the 'optim' function to minimize the dist. funct for the param. distri.
      fit_st <- optim(initial_params_st, infinity_norm_distance,
                      grid_points = grid_points,
                      observed_data = X_st,
                      parametric_fam = parametric_fam)

      # Fitting the centered MD estimator for the NP bootstrap scheme
      fit_st_MD <- optim(initial_params_st, infinity_norm_distance_MD,
                         grid_points = grid_points,
                         bs_data = X_st,
                         observed_data = X_data,
                         params = estimated_param,
                         parametric_fam = parametric_fam)

      # Extract the fitted `bootstrap-based` parameters
      estimated_param_st <- fit_st$par
      estimated_param_st_MD <- fit_st_MD$par

      # Calculate the empirical CDF values at the grid of X_st points, after
      # fitting it on the X_st data (bootstrap data)
      ecdf_values_st <- ecdf(X_st)(grid_points)

      # Calculate the parametric CDF values at the grid of X_st points
      parametrized_cdf_values_st <- param_distr(grid_points = grid_points,
                                                parametric_fam = parametric_fam,
                                                param = estimated_param_st )$fitted_cdf_vals

      parametrized_cdf_values_st_MD <- param_distr(grid_points,
                                                   parametric_fam = parametric_fam,
                                                   estimated_param_st_MD )$fitted_cdf_vals

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

      # Calculating bootstrap test statistics
      stat_st_cent[iBootstrap]    = max_diff_cent_st * sqrt(n)
      stat_st_eq[iBootstrap]      = max_diff_eq_st * sqrt(n)
      stat_st_cent_MD[iBootstrap] = max_diff_cent_st_MD * sqrt(n)
      stat_st_eq_MD[iBootstrap]   = max_diff_eq_st_MD * sqrt(n)

    }

    # Put dataframes in a list, alternating the entries
    # `param_bs` corresponds to the MD bootstrap parameter estimator (centered or not)
    list_results[[1 + (iBoot - 1)*2]] =
      data.frame(type_boot = type_boot,
                 type_stat = "cent",
                 param_bs = c("MD", "MD-cent"),
                 bootstrapped_tests = I(list(stat_st_cent,
                                             stat_st_cent_MD) )
      )
    list_results[[2 + (iBoot - 1)*2]] =
      data.frame(type_boot = type_boot,
                 type_stat = "eq",
                 param_bs = c("MD", "MD-cent"),
                 bootstrapped_tests = I(list(stat_st_eq,
                                             stat_st_eq_MD) )
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
        true_stat[1] < pvals_df$bootstrapped_tests[i][[1]]
      ) )
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
       pvals_df$param_bs == "MD-cent")

  return( list(
    # df of p-values
    pvals_df = pvals_df ,

    # true test statistics
    true_stat = true_stat ) )
}



