

#' Generating bootstrap samples in the regression setting
#'
#' @param X numeric input vector
#' @param Y numeric input vector
#' @param a_hat estimated intercept, from the regression model
#' @param b_hat estimated slope, from the regression model
#' @param epsilon_hat estimated residuals, from the regression model
#' @param resampling_type string of the bootstrap resampling scheme to be used.
#                  choose from \code{"indep"}, \code{NP}, \code{res_bs},
#                  \code{fixed_design_bs_Hnull}, \code{fixed_design_bs},
#                  \code{hybrid_null_bs}
#'
#' @return bsdata named list of X_st and Y_st, i.e. the resampled
#          bootstrap data for X,Y.
#' @noRd
#'
generate_bootstrap_data <- function(X, Y, a_hat = NA, b_hat = NA,
                                    epsilon_hat = NA, resampling_type)

{

  # Checking the validity of the inputs
  if (length(X) != length(Y)){
    stop("X and Y must have the same length. Here the length of X is ",
         length(X), " while the length of Y is ", length(Y))
  }

  # checking that relevant regression model inputs are given for resampling schemes
  if ( (is.na(a_hat) || is.na(b_hat) || (sum(is.na(epsilon_hat))>0) )
       &
       ( resampling_type == "res_bs" || resampling_type == "fixed_design_bs_Hnull"
         || resampling_type == "fixed_design_bs"
         || resampling_type == "hybrid_null_bs" )
  ){
    stop("Resampling scheme", resampling_type," needs a_hat, b_hat,
          epsilon_hat as inputs.")
  }

  # define the sample size
  n = length(X)

  # Bootstrap of Category 1: independence resampling bootstrap
  # Independently resample X* from X and resample Y* from Y. No pairs!
  if (resampling_type == 'indep') {
    # random resampling (X*) from X and (Y*) from (Y)
    permutation_1 = sample.int(n, replace = TRUE)
    X_st = X[permutation_1]
    permutation_2 = sample.int(n, replace = TRUE)
    Y_st = Y[permutation_2]
  }

  # Bootstrap of Category 2: empirical bootstrap (nonparametric)
  # Resample pairs (X*,Y*) from (X,Y).
  if (resampling_type == 'NP') {
    permutation = sample.int(n, replace = TRUE)
    X_st = X[permutation]
    Y_st = Y[permutation]
  }

  # Bootstrap of Category 3: residual bootstrap (should be equal to empirical bs)
  # Resample X*,Y* using the nonparametric bootstrap from pairs (X,epsilon_hat)
  # No use of H0
  if (resampling_type == 'res_bs') {
    permutation = sample.int(n, replace = TRUE)
    X_st = X[permutation]
    epsilon_st = epsilon_hat[permutation]
    Y_st = a_hat+b_hat*X_st+epsilon_st
  }

  # Bootstrap of Category 4: fixed design residual bootstrap (using H0)
  # Resample epsilon_1^*,...,epsilon_1^* from \hat{epsilon_hat}_1,..,\hat{epsilon}_n.
  # Set X_i^*:=X_i for all i=1,...n, i.e. use fixed design and no resampling on the X-values.
  if (resampling_type == 'fixed_design_bs_Hnull') {
    permutation = sample.int(n, replace = TRUE)
    epsilon_st = epsilon_hat[permutation]
    X_st = X
    Y_st = a_hat+epsilon_st
  }

  # Bootstrap of Category 4: fixed design residual bootstrap (not using H0)
  # Resample epsilon_1^*,...,epsilon_1^* from \hat{epsilon_hat}_1,..,\hat{epsilon}_n.
  # Set X_i^*:=X_i for all i=1,...n, i.e. use fixed design and no resampling on the X-values.
  if (resampling_type == 'fixed_design_bs') {
    permutation = sample.int(n, replace = TRUE)
    epsilon_st = epsilon_hat[permutation]
    X_st = X
    Y_st = a_hat + b_hat*X_st + epsilon_st
  }

  #Bootstrap of Category 5: hybrid null bootstrap
  #Resample X*,Y* using the nonparametric bootstrap from X,epsilon_hat
  #Using H0
  if (resampling_type == 'hybrid_null_bs') {
    permutation = sample.int(n, replace = TRUE)
    X_st = X[permutation]
    epsilon_st = epsilon_hat[permutation]
    Y_st = a_hat+epsilon_st
  }

  bs_data <- list( X_st = X_st, Y_st = Y_st)
  return(bs_data)
}



#' Perform a test on the slope coefficient of a univariate linear regression
#'
#' This function performs a bootstrap regression test for given data X,Y.
#' The null hypothesis corresponds of a slope coefficient of zero, versus the
#' alternative hypothesis of a non-zero slope coefficient.
#' It uses an independence/null bootstrap \code{"indep}, a non-parametric \code{"NP"},
#' a residual bootstrap \code{"res_bs"}, a fixed design bootstrap \code{"fixed_design_bs"},
#' a fixed design null bootstrap \code{"fixed_design_bs_Hnull"}, a hybrid null
#' bootstrap \code{"hybrid_null_bs"} as bootstrap resampling schemes to perform
#' the bootstrap. This function gives the corresponding p-values, the true test
#' statistic and the bootstrap-version test statistics. Furthermore, it also
#' gives the estimated slope.The default (and valid) method implemented
#' in this function is the null bootstrap, together with the equivalent test
#' statistic. Via the \code{bootstrapOptions} argument, the user can specify other
#' bootstrap resampling schemes and test statistics.
#'
#' @param X numeric univariate input vector resembling the independent variables
#' @param Y numeric univariate input vector the dependent variables
#' @param nBootstrap numeric value of the amount of bootstrap resamples
#' @param bootstrapOptions This can be one of \itemize{
#'   \item \code{NULL}
#'
#'   \item a list with at most 2 elements names \itemize{
#'         \item \code{type_boot} defaults to the \code{"indep"} bootstrap
#'         resampling scheme to be used. \code{type_boot} can be either
#'         \code{"indep"} for the independence/null bootstrap, or \code{"NP"}
#'         for the non-parametric bootstrap.
#'
#'         \item \code{type_stat} defaults to \code{"eq"} for the type of test
#'         statistic to be used. This can be either \code{"eq"} for the
#'         equivalent test statistic, or \code{"cent"} for the centered
#'         test statistic.
#'   }
#'   \item \code{"all"} this gives test results for all theoretically valid
#'   combinations of bootstrap resampling schemes.
#'
#'   \item \code{"all and also invalid"} this gives test results for all possible
#'   combinations of bootstrap resampling schemes and test statistics, including
#'   invalid ones.
#' }
#' A warning is raised if the given combination of \code{type_boot} and
#' \code{type_stat} is theoretically invalid.
#'
#'
#' @return A class object with components \itemize{
#'    \item \code{pvals_df} a dataframe of p-values and bootstrapped test statistics:
#'
#'    These are the p-values for the combinations of bootstrap resampling schemes,
#'    test statistics (centered and equivalent).
#'
#'    It also contains the vectors of bootstrap test statistics
#'    for each of the combinations.
#'
#'    \item \code{true_stat} a named vector of size 1 containing the true test
#'    statistic.
#'
#'    \item \code{nBootstrap} Number of bootstrap repetitions.
#'
#'    \item \code{data} named list of the used input data, i.e. X and Y.
#'
#'    \item \code{nameMethod} string for the name of the method used.
#'
#'    \item \code{beta} numeric value of the estimated slope of the regression model.
#' }
#'
#' @seealso \code{\link{perform_GoF_test},\link{perform_independence_test}}
#'
#' @examples
#' n <- 100
#'
#' # Under H1
#' X_data <- rnorm(n)
#' Y_data <-  X_data + rnorm(n)   #Y = X + epsilon
#' result <- perform_regression_test(X_data, Y_data, nBootstrap = 100,
#'                         bootstrapOptions =  list(type_boot = "indep",
#'                                                  type_stat = "eq"))
#' print(result)
#' plot(result)
#'
#' # Under H0
#' X_data <- rnorm(n)
#' Y_data <-  rep(1, n)  #these values are exactly constant (as b = 0 under H0)
#' result <- perform_regression_test(X_data, Y_data, nBootstrap = 100)
#' print(result)
#' plot(result)
#'
#' @export
perform_regression_test <- function(X, Y,
                                    nBootstrap = 100,
                                    bootstrapOptions = NULL){

  # Initialize default values for the bootstrap options
  type_boot_user = "indep"
  type_stat_user = "eq"

  # Read in the `bootstrapOptions` and set the user-specified options
  if (is.list(bootstrapOptions) && length(bootstrapOptions) > 0){
    if ("type_boot" %in% names(bootstrapOptions)){
      type_boot_user = bootstrapOptions$type_boot
    }
    if ("type_stat" %in% names(bootstrapOptions)){
      type_stat_user = bootstrapOptions$type_stat
    }
    if ( !all(names(bootstrapOptions) %in% c( "type_boot", "type_stat" )) ){
      stop("Please provide correct argument names for `bootstrapOptions`.
            Valid names are: 'type_boot' and 'type_stat'.")
    }
  } else if (!is.list(bootstrapOptions) &&
             !is.null(bootstrapOptions) &&
             !is.character(bootstrapOptions)){
    stop("Invalid bootstrap options. Please check your inputs.")
  }

  # Checking the validity of the inputs
  if (length(X) != length(Y)){
    stop("X and Y must have the same length. Here the length of X is ",
         length(X), " while the length of Y is ", length(Y))
  }

  if (length(nBootstrap) > 1 || !is.finite(nBootstrap) || nBootstrap <= 0){
    stop("nBootstrap must be a positive integer of length 1.")
  }

  if (length(X) < 1 || length(Y) < 1 ){
    stop("X and Y must contain at least one entry.")
  }

  if ( is.numeric(X) == FALSE || is.numeric(Y) == FALSE ){
    stop("X and Y must be numeric vectors. Please check your input data.")
  }

  bootstrap_names_check <- c("indep", "NP",
                             "res_bs", "fixed_design_bs_Hnull",
                             "fixed_design_bs", "hybrid_null_bs")

  if (type_boot_user %in% bootstrap_names_check == FALSE){
    stop("Choose valid type_boot: either 'indep', 'NP', 'res_bs',
    'fixed_design_bs_Hnull', 'fixed_design_bs', hybrid_null_bs'. Current input is ",
         type_boot_user)
  }

  if (type_stat_user %in% c("eq", "cent") == FALSE){
    stop("Choose valid type_stat: either 'eq' or 'cent'. Current input is",
         type_stat_user)
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
    if (type_boot_user == "indep" && type_stat_user == "cent"){
      warning("The combination of type_boot = 'indep' and type_stat = 'cent' ",
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
    if (type_boot_user == "hybrid_null_bs" && type_stat_user == "cent"){
      warning("The combination of type_boot = 'hybrid_null_bs' and type_stat = 'cent' ",
              "is theoretically invalid. The p-values will not be valid.")
    }
    if (type_boot_user == "fixed_design_bs_Hnull" && type_stat_user == "cent"){
      warning("The combination of type_boot = 'fixed_design_bs_Hnull' and type_stat = 'cent' ",
              "is theoretically invalid. The p-values will not be valid.")
    }
    if (type_boot_user == "fixed_design_bs" && type_stat_user == "eq"){
      warning("The combination of type_boot = 'fixed_design_bs' and type_stat = 'eq' ",
              "is theoretically invalid. The p-values will not be valid.")
    }
  }

  # define the sample size
  n = length(X)

  dataframe<- data.frame(X = X, Y = Y)

  model<- stats::lm(Y~X, data = dataframe)
  # obtain estimates for parameters and residuals from the model
  a_hat = model$coefficients[[1]]
  b_hat = model$coefficients[[2]]
  epsilon_hat = model$residuals

  # Check for NA
  stopifnot(!is.na(a_hat) & !is.na(b_hat) & !is.na(epsilon_hat))

  # calculate T_n = sqrt(n) * |b_hat|
  true_stat = sqrt(n) * abs(b_hat)

  bootstrap_names <- c("indep", "NP",
                       "res_bs", "fixed_design_bs_Hnull",
                       "fixed_design_bs", "hybrid_null_bs")

  # Create the p-values data frame
  pvals_df <- data.frame(
    # To prevent factors from being created
    stringsAsFactors = FALSE,
    # Repeat each category option from bootstrap_names
    type_boot = rep(bootstrap_names, each = 2),
    # Repeat each category option (cent, eq)
    type_stat = rep(c("cent", "eq"), times = 6),
    # Combine the vectors
    pvalues   = rep(NA,12),
    # # Combine the vectors
    bootstrapped_tests = I( rep(list( rep(NA, nBootstrap) ), 12) )
  )

  # For all possible bootstrap resampling schemes, perform the bootstrap
  # regression test.

  for (bs_scheme in bootstrap_names) {

    # initialisation of the bootstrap test statistics values
    stat_st_cent = rep(NA,nBootstrap)
    stat_st_eq = rep(NA,nBootstrap)

    for (iBootstrap in 1:nBootstrap){

      # Generate bootstrap data
      bootstrap_data = generate_bootstrap_data(X, Y, a_hat,
                                               b_hat, epsilon_hat,
                                               resampling_type = bs_scheme)
      X_st = bootstrap_data$X_st
      Y_st = bootstrap_data$Y_st

      # Fit linear regression model on bootstrap data
      bootstrap_sample <- data.frame(X_st = X_st,Y_st = Y_st)
      bootstrap_model <- stats::lm(Y_st~X_st, data = bootstrap_sample)

      # Calculate bootstrap test statistics T_n_^*, centered and equivalent
      b_hat_st = bootstrap_model$coefficients[[2]]
      stopifnot(!is.na(b_hat_st))
      stat_st_cent[iBootstrap] = abs(b_hat_st-b_hat) * sqrt(n)
      stat_st_eq[iBootstrap] = abs(b_hat_st) * sqrt(n)
      stopifnot(!is.na(stat_st_cent[iBootstrap])  && !is.na(stat_st_eq[iBootstrap]) )
    }

    # Calculate pvalues, for the centered and equivalent test statistics
    p_val_cent = mean(as.numeric(true_stat < stat_st_cent))
    p_val_eq = mean(as.numeric(true_stat < stat_st_eq))


    # add p-values to dataframe
    pvals_df$pvalues[pvals_df$type_boot == bs_scheme &
                       pvals_df$type_stat == "cent"] = p_val_cent

    pvals_df$pvalues[pvals_df$type_boot == bs_scheme &
                       pvals_df$type_stat=="eq"] = p_val_eq

    # add boostrapped test statistics to dataframe
    pvals_df$bootstrapped_tests[pvals_df$type_boot == bs_scheme &
                                  pvals_df$type_stat == "cent" ] <- list(stat_st_cent)

    pvals_df$bootstrapped_tests[pvals_df$type_boot == bs_scheme &
                                  pvals_df$type_stat == "eq" ] <- list(stat_st_eq)

  }

  ### post-processing ###


  # Add a column to indicate whether the combination of bootstrap and
  # test statistic is theoretically valid
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
  } else if (is.list(bootstrapOptions) && length(bootstrapOptions) > 0 ||
             is.null(bootstrapOptions)){
    # If the user specified a combination of bootstrap options or simply nothing
    pvals_df = pvals_df[
      pvals_df$type_boot == type_boot_user &
        pvals_df$type_stat == type_stat_user, ]
  }

  ### Create the result object ###
  result <- list(
    # df of p-values
    pvals_df = pvals_df,
    # true test statistics
    true_stats = true_stat,
    # beta
    beta = b_hat,
    # Include number of bootstrap repetitions
    nBootstrap = nBootstrap,
    # Include the input data
    data = list(X = X, Y = Y),
    # give bootstrap method a name
    nameMethod = "Bootstrap Regression Test"
    )

  # make a class for the result object
  class(result) <- c("bootstrapTest_regression", "bootstrapTest")
  return(result)
}
