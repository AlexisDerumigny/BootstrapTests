
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
estimate_params <- function(data, parametric_fam){

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
#' Furthermore, in case of "param" bootstrap, parameters need to be
#' inputted as well.
#'
#' @noRd
#'
generateBootstrapSamples_GOF <- function(X_data, type_boot, param = NA,
                                         parametric_fam = "normal"){

  # Input checks
  if( type_boot == "param" & anyNA(param) ){
    stop("For the param bootstrap you need estimated params")
  }
  stopifnot(type_boot == "param" | type_boot == "NP" )
  stopifnot(is.vector(X_data) && is.numeric(X_data) )

  # Extract sample size form X_data
  n = length(X_data)

  switch (
    type_boot,

    "param" = {
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





#' Transform a list of options into a dataframe of bootstrap schemes with 1 row
#'
#'
#' @noRd
make_df_bootstraps_GoF <- function(bootstrapOptions, verbose){

  if (is.null(bootstrapOptions) || length(bootstrapOptions) == 0){

    # Initialize default values for the bootstrap options
    df_bootstraps = data.frame(type_boot = "param",
                               type_stat = "eq",
                               type_estimator_bootstrap = "MLE")

  } else if (is.list(bootstrapOptions)){


    df_bootstraps = make_df_bootstraps_GoF_fromList(bootstrapOptions,
                                                    verbose = verbose)


  } else if (is.character(bootstrapOptions)){

    switch (
      bootstrapOptions,
      "all" = {

        df_bootstraps = data.frame(
          type_boot =
            c("param", "param", "NP"  , "NP" ),
          type_stat =
            c("eq"  , "eq"  , "cent", "cent" ),
          type_estimator_bootstrap =
            c("MLE" , "MD-eq"  , "MLE" , "MD-cent")
        )
      },

      "all and also invalid" = {

        df_bootstraps = expand.grid(
          type_boot = c("param", "NP"),
          type_stat = c("eq"  , "cent" ),
          type_estimator_bootstrap = c("MLE" , "MD-eq"  , "MD-cent"),
          stringsAsFactors = FALSE
        )

        warning("Using 'all and also invalid' as bootstrapOptions is not recommended. ",
                "This will return all theoretically valid and invalid combinations of ",
                "bootstrap resampling schemes, and test statistics. ",
                "Please use with caution.")

      },
      {
        stop("Invalid choice for bootstrapOptions. ",
             "Please choose either 'all' or 'all and also invalid'. Current input is",
             bootstrapOptions )
      }
    )
  }

  return (df_bootstraps)
}


make_df_bootstraps_GoF_fromList <- function(bootstrapOptions, verbose = verbose){

  if ("type_boot" %in% names(bootstrapOptions)){
    type_boot = bootstrapOptions$type_boot

    if (! (type_boot %in% c("param", "NP") ) ){
      stop("Invalid type_boot: either 'param' or 'NP'. Current input is ",
           type_boot)
    }

    if (verbose){
      cat("'type_boot' chosen by the user as: ", type_boot, "\n", sep = "")
    }

  } else {
    type_boot = "param"

    if (verbose){
      cat("'type_boot' chosen by default as: ", type_boot, "\n", sep = "")
    }
  }

  if ("type_stat" %in% names(bootstrapOptions)){
    type_stat = bootstrapOptions$type_stat

    if( ! (type_stat %in% c("eq", "cent") ) ){
      stop("Invalid type_stat: either 'eq' or 'cent'. Current input is",
           type_stat)
    }

    if (verbose){
      cat("'type_stat' chosen by the user as: ", type_stat, "\n", sep = "")
    }

  } else {

    mapping = c(param = "eq", NP = "cent")
    type_stat = mapping[type_boot]

    if (verbose){
      cat("'type_stat' chosen by default as: ", type_stat,
          " corresponding to type_boot = ", type_boot, "\n", sep = "")
    }
  }

  if ("type_estimator_bootstrap" %in% names(bootstrapOptions)){
    type_estimator_bootstrap = bootstrapOptions$type_estimator_bootstrap

    if ( ! (type_estimator_bootstrap %in% c("MD-eq", "MD-cent", "MLE") ) ){
      stop("Invalid type_estimator_bootstrap: either 'MD-eq', 'MD-cent' or 'MLE'.
            Current input is", type_estimator_bootstrap)
    }

    if (verbose){
      cat("'type_estimator_bootstrap' chosen by the user as: ",
          type_estimator_bootstrap, "\n", sep = "")
    }

  } else {
    type_estimator_bootstrap = "MLE"

    if (verbose){
      cat("'type_estimator_bootstrap' chosen by default as: ",
          type_estimator_bootstrap, "\n", sep = "")
    }
  }

  if ( !all(names(bootstrapOptions) %in%
            c( "type_boot", "type_stat", "type_estimator_bootstrap" ) ) ){
    stop("Please provide correct argument names for `bootstrapOptions`.
            Valid names are: 'type_boot', 'type_stat', and 'type_estimator_bootstrap'. ")
  }



  # Give warning for theoretically invalid bootstrap schemes

  if (type_boot == "param" && type_stat == "cent"){
    warning(warningInvalidCombination("param", "cent", type_stat_valid = "eq"))
  }
  if (type_boot == "NP" && type_stat == "eq"){
    warning(warningInvalidCombination("NP", "eq", type_stat_valid = "cent"))
  }
  if (type_boot == "NP" &&
      type_stat == "cent" &&
      type_estimator_bootstrap == "MD-eq"){
    warning(
      "The combination of type_boot = 'NP', type_stat = 'cent', ",
      "and `type_estimator_bootstrap` = 'MD-eq' is theoretically invalid: ",
      "the obtained p-value will not be valid. For this situation, you should ",
      "better use type_estimator_bootstrap = 'MD-cent'")
  }
  if (type_boot == "param" &&
      type_stat == "eq" &&
      type_estimator_bootstrap == "MD-cent"){
    warning(
      "The combination of type_boot = 'param', type_stat = 'eq', ",
      "and `type_estimator_bootstrap` = 'MD-eq' is theoretically invalid: ",
      "the obtained p-value will not be valid. For this situation, you should ",
      "better use type_estimator_bootstrap = 'MD-eq'")
  }


  # Initialize the bootstrap data frame
  df_bootstraps = data.frame(type_boot = type_boot,
                             type_stat = type_stat,
                             type_estimator_bootstrap = type_estimator_bootstrap)

  return (df_bootstraps)
}


warningInvalidCombination <- function(type_boot, type_stat, type_stat_valid){
  myWarning = warningCondition(
    message = paste0(
      "The combination of type_boot = '", type_boot ,
      "' and type_stat = '", type_stat,"' ",
      "is theoretically invalid: the obtained p-value will not be valid. ",
      "For this choice of 'type_boot', you should rather use 'type_stat' = ",
      type_stat_valid, ", which is theoretically valid."
    )
  )
}



#' Perform a univariate goodness-of-fit (GoF) hypothesis test via bootstrap resampling
#'
#' This function performs a bootstrap goodness-of-fit hypothesis test for a
#' specific univariate parametric family. The null hypothesis corresponds to the
#' sample coming from the specified parametric family, while the alternative
#' hypothesis corresponds to the sample not coming from the specified
#' parametric family. This function implements a parametric bootstrap and
#' a non-parametric bootstrap. The test statistic is the Kolmogorov-Smirnov test
#' statistic. To estimate the parameters of the parametric family, either a minimum
#' distance estimator, or a MLE estimator (the sample mean and variance)
#' is used. On the bootstrap sample, we have also implemented a centered MD estimator,
#' as in the paper. For now, only a test of normality is implemented. This function
#' gives the corresponding p-values, the true test statistic and the
#' bootstrap-version test statistics. The default (and valid) method implemented
#' in this function is the parametric bootstrap, together with the equivalent test statistic
#' and the MLE parameter estimator. Via the \code{bootstrapOptions}
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
#' @param mygrid description of the grid used to compute the CDFs on. This must be
#' one of \itemize{
#'   \item \code{NULL}: a regularly spaced grid from the minimum value to the
#'   maximum value with \code{100} points is used. This is the default.
#'
#'   \item A numeric of size 1. This is used at the length of the grid, replacing
#'   \code{100} in the above explanation.
#'
#'   \item A numeric vector of size larger than 1. This is directly used as the
#'   grid.
#' }
#'
#' @param show_progress logical value indicating whether to show a progress bar
#'
#' @param bootstrapOptions This can be one of \itemize{
#'   \item \code{NULL}. This uses the default options \code{type_boot = "param"},
#'   \code{type_stat = "eq"} and \code{type_estimator_bootstrap = "MLE"}.
#'
#'   \item a list with at most 3 elements named: \itemize{
#'         \item \code{type_boot} type of bootstrap resampling scheme. It must be
#'         one of
#'         \itemize{
#'           \item \code{"param"} for the parametric bootstrap (i.e. under the null).
#'           This is the default.
#'           \item \code{"NP"} for the non-parametric bootstrap
#'           (i.e. n out of n bootstrap).
#'         }
#'
#'         \item \code{type_stat} type of test statistic to be used.  It must be
#'         one of
#'         \itemize{
#'           \item \code{"eq"} for the equivalent test statistic
#'           \eqn{T_n^* = \sqrt{n} || \hat{F}^* - F_{\hat\theta^*} ||}
#'
#'           \item \code{"cent"} for the centered test statistic
#'           \eqn{T_n^* = \sqrt{n} || \hat{F}^* - \hat{F} + F_{\hat\theta} - F_{\hat\theta^*} ||}
#'         }
#'         For each \code{type_boot} there is only one valid choice of \code{type_stat}
#'         to be made. If \code{type_stat} is not specified, the valid choice is
#'         automatically used.
#'
#'         \item \code{type_estimator_bootstrap}: the bootstrap parameter
#'         estimator to be used. It must be one of:
#'         \itemize{
#'            \item \code{"MLE"} for the MLE estimator
#'            (for the normal distribution, this corresponds to the usual
#'            empirical mean and variance).
#'
#'            This is always a valid choice in the case that the combination
#'            \code{(type_boot, type_stat)} is valid (as defined above).
#'            Therefore, this is the default option. It is also the fastest type
#'            of estimator.
#'
#'            \item \code{"MD-eq"} for the Minimum Distance estimator.
#'            This is a valid choice if and only if \code{type_stat = "eq"}. It
#'            is necessary in this case to use an equivalent bootstrap
#'            estimator to match the equivalent bootstrap test statistic. This
#'            bootstrap parameter estimator is given as:
#'            \eqn{\theta_n^{*,MD}=\arg\min_{\theta} ||  \hat{F}^* - F_{\theta} ||}
#'
#'            \item \code{"MD-cent"} for the centered Minimum Distance estimator.
#'            This is a valid choice if and only if \code{type_stat = "cent"}. It
#'            is necessary in this case to perform a centering on the bootstrap
#'            estimator to match the centered bootstrap test statistic. This
#'            bootstrap parameter estimator is given as:
#'            \eqn{\theta_n^{*,MD, cent}=\arg\min_{\theta}
#'            || \hat{F}^* - F_{\theta}- \hat{F} + F_{\hat\theta} ||}
#'
#'         }
#'   }
#'   \item \code{"all"} this gives test results for all theoretically valid
#'   combinations of bootstrap resampling schemes.
#'
#'   \item \code{"all and also invalid"} this gives test results for all possible
#'   combinations of bootstrap resampling schemes and test statistics, including
#'   invalid ones.
#' }
#' A warning is raised if the given combination of \code{type_boot},
#' \code{type_stat}, and \code{type_estimator_bootstrap} is theoretically invalid.
#'
#'
#' @param verbose If \code{verbose = 0}, this function is silent and does not
#' print anything. Increasing values of \code{verbose} print more details about
#' the progress of the computations.
#'
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
#'    test statistic for the MLE parameter estimator.
#'
#'    \item \code{nBootstrap} number of bootstrap repetitions.
#'
#'    \item \code{nameMethod} string for the name of the method used.
#'
#' }
#'
#' @seealso \code{\link{perform_regression_test},\link{perform_independence_test}}.
#' The print and plot methods, such as \code{\link{plot.bootstrapTest}}.
#'
#' @references
#' Derumigny, A., Galanis, M., Schipper, W., & van der Vaart, A. (2025).
#' Bootstrapping not under the null?
#' ArXiv preprint, \doi{10.48550/arXiv.2512.10546}
#'
#' @examples
#' n <- 100
#' # Under H1
#' X_data <- rgamma(n,2,3)
#' result <- perform_GoF_test(X_data,
#'                           nBootstrap = 100,
#'                           bootstrapOptions = list(type_boot = "param",
#'                                                   type_stat = "eq",
#'                                                   type_estimator_bootstrap = "MLE")
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
                             mygrid = NULL,
                             show_progress = TRUE,
                             bootstrapOptions = NULL,
                             verbose = 0)
{
  # 1. First input checks  =====================================================

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



  # 2. Finding the bootstraps that needs to be done  ===========================

  df_bootstraps = make_df_bootstraps_GoF(bootstrapOptions, verbose = verbose)


  # 3. Computation of the original statistics  =================================

  # Define sample size
  n <- length(X_data)

  # Grid points
  if (is.null(mygrid)){
    mygrid = 100
  }
  if (is.numeric(mygrid) && length(mygrid) == 1){
    grid_points <- seq(min(X_data), max(X_data), length.out = mygrid)
  } else {
    grid_points = mygrid
  }

  # Calculate the empirical CDF values at the grid of X points
  ecdf_values <- stats::ecdf(X_data)(grid_points)


  estimated_param = list()
  parametrized_cdf_values = list()
  max_diff = numeric(0)

  # TODO: for other GoF-tests, change initial estimation parameters
  # Estimate unknown distribution parameters to minimize the norm distance
  # Initial guesses for the parameters in case of MD
  estimated_param[["MLE"]] <- estimate_params(X_data, parametric_fam)

  # Calculate the parametrised CDF values at the grid of X points
  parametrized_cdf_values[["MLE"]] <- param_distr(grid_points = grid_points,
                                                  parametric_fam = parametric_fam,
                                                  param = estimated_param[["MLE"]] )$fitted_cdf_vals

  max_diff[["MLE"]] <- max(abs(ecdf_values - parametrized_cdf_values[["MLE"]]))

  doComputationsWithMD =
    ("MD-eq" %in% df_bootstraps$type_estimator_bootstrap) ||
    ("MD-cent" %in% df_bootstraps$type_estimator_bootstrap)

  if(verbose == 2){
    cat(" value of 'doComputationsWithMD' is  ", doComputationsWithMD, "\n")
  }

  if (doComputationsWithMD){
    # Use the 'stats::optim' function to minimize the infinity norm between ecdf and
    # parametric cdf. This gives parameter estimates in the Minimum Distance setting.
    fit <- stats::optim(par = estimated_param[["MLE"]],
                        infinity_norm_distance,
                        observed_data = X_data,
                        grid_points = grid_points,
                        parametric_fam = parametric_fam)

    # Extract the fitted parameters (for normal family the mean and variance)
    estimated_param[["MD"]] <- fit$par

    # Calculate the parametrised CDF values at the grid of X points
    parametrized_cdf_values[["MD"]] <- param_distr(grid_points = grid_points,
                                                   parametric_fam = parametric_fam,
                                                   param = estimated_param[["MD"]] )$fitted_cdf_vals

    # Calculate the infinity norm (sup norm): maximum absolute difference
    max_diff[["MD"]] <- max(abs(ecdf_values - parametrized_cdf_values[["MD"]]))
  }


  # Vector containing true test statistics, for the supremum norm
  true_stat_KS = sqrt(n) * max_diff


  # 4. Bootstrapping  ==========================================================

  vec_type_boot = unique(df_bootstraps$type_boot)

  if (show_progress && verbose == 0) {
    # Progress bar
    total_steps <- nrow(df_bootstraps) * nBootstrap
    pb <- pbapply::startpb(min = 0, max = total_steps)
    step <- 1

    on.exit(pbapply::closepb(pb))
  }

  list_results = list()
  i_results = 1

  for (iBoot in 1:length(vec_type_boot)){

    type_boot = vec_type_boot[iBoot]

    # Extract "full" bootstrap method types corresponding to the current type_boot
    df_bootstraps_fixed_type_boot = df_bootstraps[df_bootstraps$type_boot == type_boot, ]

    if (verbose){
      cat("type_boot:", type_boot, "\n")
    }

    # make distinction between MLE and MD based bootstrap methods
    mapping_parametric_bootstrap = c(MLE = "MLE", `MD-eq` = "MD",
                                     `MD-cent` = "MD")

    # add column `type_estimator` with MLE or MD based estimator
    df_bootstraps_fixed_type_boot$type_estimator = mapping_parametric_bootstrap[
      df_bootstraps_fixed_type_boot$type_estimator_bootstrap]

    vec_type_estimator = unique(df_bootstraps_fixed_type_boot$type_estimator)

    for (i_type_estimator in 1:length(vec_type_estimator)) {
      type_estimator = vec_type_estimator[i_type_estimator]

      if (verbose){
        cat("type_estimator:", type_estimator, "\n")
      }

      # Extract "full" bootstrap method types corresponding to the current type_boot
      df_bootstraps_current_type = df_bootstraps_fixed_type_boot[
        df_bootstraps_fixed_type_boot$type_estimator == type_estimator, ]


      matrix_stat_st = matrix(nrow = nBootstrap,
                              ncol = nrow(df_bootstraps_current_type) )

      if (show_progress && verbose > 0) {
        pb <- pbapply::startpb(min = 0, max = nBootstrap)
        step <- 1
      }

      for (iBootstrap in 1:nBootstrap){

        if (verbose > 1){
          cat("Bootstrap replication:", iBootstrap, "\n")
        }

        # 4.1. Generating data  ==================================================

        X_st <- generateBootstrapSamples_GOF(X_data,
                                             type_boot = type_boot,
                                             param = estimated_param[[type_estimator]])

        # 4.2. Estimating non-parametric CDF  ====================================

        # Calculate the empirical CDF values at the grid of X_st points, after
        # fitting it on the X_st data (bootstrap data)
        ecdf_values_st <- stats::ecdf(X_st)(grid_points)


        # 4.3. Estimating parameters  ============================================

        estimated_param_st = list()

        # Extract the fitted `bootstrap-based` parameters
        # also used as initial guesses for the mean and sd parameters in case of MD
        estimated_param_st[["MLE"]] <- estimate_params(X_st, parametric_fam)


        if ("MD-eq" %in% df_bootstraps_current_type$type_estimator_bootstrap){
          # Use the 'stats::optim' function to minimize the dist. funct for the param. distri.
          fit_st <- stats::optim(estimated_param_st[["MLE"]], infinity_norm_distance,
                                 grid_points = grid_points,
                                 observed_data = X_st,
                                 parametric_fam = parametric_fam)

          # Extract the fitted `bootstrap-based` parameters
          estimated_param_st[["MD-eq"]] <- fit_st$par
        }

        if ("MD-cent" %in% df_bootstraps_current_type$type_estimator_bootstrap){
          # Fitting the centered MD estimator for the NP bootstrap scheme
          fit_st <- stats::optim(par = estimated_param_st[["MLE"]],
                                 infinity_norm_distance_MD,
                                 grid_points = grid_points,
                                 bs_data = X_st,
                                 observed_data = X_data,
                                 params = estimated_param[["MD"]],
                                 parametric_fam = parametric_fam)

          # Extract the fitted `bootstrap-based` parameters
          estimated_param_st[["MD-cent"]] <- fit_st$par
        }


        # 4.4. Computing the test statistics  ====================================


        for (iCombination in 1:nrow(df_bootstraps_current_type)){

          type_stat =
            df_bootstraps_current_type[iCombination, "type_stat"]

          type_estimator_bootstrap =
            df_bootstraps_current_type[iCombination, "type_estimator_bootstrap"]

          if (verbose > 1){
            cat("iCombination: ", iCombination,
                ", type_stat = '", type_stat,
                "', type_estimator_bootstrap = '", type_estimator_bootstrap, "'. ", sep = "")
          }

          switch(type_stat,
                 "cent" = {

                   # Calculate the parametric CDF values at the grid of X_st points
                   parametrized_cdf_values_st <- param_distr(
                     grid_points = grid_points,
                     parametric_fam = parametric_fam,
                     param = estimated_param_st[[type_estimator_bootstrap]] )$fitted_cdf_vals

                   # Calculate the infinity norm (sup norm): maximum absolute difference
                   max_diff_st <- max(abs(ecdf_values_st - parametrized_cdf_values_st
                                          - ecdf_values +  parametrized_cdf_values[[type_estimator]] ))
                 },
                 "eq" = {
                   max_diff_st <- infinity_norm_distance(grid_points,
                                                         X_st,
                                                         estimated_param_st[[type_estimator_bootstrap]],
                                                         parametric_fam = parametric_fam)

                 }
          )

          # Calculating bootstrap test statistics
          matrix_stat_st[iBootstrap, iCombination] = max_diff_st * sqrt(n)

          if (verbose > 1){
            cat("T^* =", matrix_stat_st[iBootstrap, iCombination], "\n")
          }

          if (show_progress) {
            pbapply::setpb(pb = pb, value = step)
            step = step + 1
          }
        }
      }



      if (show_progress && verbose > 0) {
        pbapply::closepb(pb)
      }

      for (iCombination in 1:nrow(df_bootstraps_current_type)){

        type_stat =
          df_bootstraps_current_type[iCombination, "type_stat"]

        type_estimator_bootstrap =
          df_bootstraps_current_type[iCombination, "type_estimator_bootstrap"]

        list_results[[i_results]] = data.frame(
          type_boot = type_boot,
          type_stat = type_stat,
          type_estimator_bootstrap = type_estimator_bootstrap,

          list_stat_st = I(list(matrix_stat_st[, iCombination]) ) )

        i_results = i_results + 1
      }
    }
  }

  # Done with all bootstrapping =============================================

  # Rowbind the dataframes in `list_results` into a dataframe
  pvals_df = do.call(what = rbind, args = list_results)

  # Compute pvalues
  list_pvalues = lapply(
    X = 1:nrow(pvals_df),
    FUN = function(i){

      type_estimator = if(pvals_df$type_estimator_bootstrap[i] == "MLE"){
        "MLE"
      } else "MD"

      true_stat_to_use = true_stat_KS[type_estimator]

      pval = mean(as.numeric(
        true_stat_to_use < pvals_df$list_stat_st[i][[1]]
      ) )
      return(pval)
    }
  )
  pvals_df$pvalues = unlist(list_pvalues)

  # The NP and centred test statistic also needs
  # a centred MD bootstrap parameter estimator.
  pvals_df$theoretically_valid =
    (pvals_df$type_boot == "param" &
       pvals_df$type_stat == "eq" &
       pvals_df$type_estimator_bootstrap == "MD-eq")  |
    (pvals_df$type_boot == "NP" &
       pvals_df$type_stat == "cent" &
       pvals_df$type_estimator_bootstrap == "MD-cent") |
    (pvals_df$type_boot == "param" &
       pvals_df$type_stat == "eq" &
       pvals_df$type_estimator_bootstrap == "MLE") |
    (pvals_df$type_boot == "NP" &
       pvals_df$type_stat == "cent" &
       pvals_df$type_estimator_bootstrap == "MLE")


  ### Create the result object ###
  result <- list(
    # df of p-values
    pvals_df = pvals_df,
    # true test statistics
    true_stats = true_stat_KS,
    # Include number of bootstrap repetitions
    nBootstrap = nBootstrap,
    # give bootstrap method a name
    nameMethod = "Bootstrap GoF Test"
  )

  # make a class for the result object
  class(result) <- c("bootstrapTest_GoF", "bootstrapTest")
  return(result)
}



