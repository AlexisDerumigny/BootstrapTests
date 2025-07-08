

#' Generating bootstrap samples in the regression setting
#'
#' @param X numeric input vector
#' @param Y numeric input vector
#' @param a_hat estimated intercept, from the regression model
#' @param b_hat estimated slope, from the regression model
#' @param epsilon_hat estimated residuals, from the regression model
#' @param resampling_type string of the bootstrap resampling scheme to be used.
#                  choose from \code{"indep_bs"}, \code{empirical_bs}, \code{res_bs},
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
  if (resampling_type == 'indep_bs') {
    # random resampling (X*) from X and (Y*) from (Y)
    permutation_1 = sample.int(n, replace = TRUE)
    X_st = X[permutation_1]
    permutation_2 = sample.int(n, replace = TRUE)
    Y_st = Y[permutation_2]
  }

  # Bootstrap of Category 2: empirical bootstrap (nonparametric)
  # Resample pairs (X*,Y*) from (X,Y).
  if (resampling_type == 'empirical_bs') {
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



#' Performs a test on the coefficient of a univariate linear regression
#'
#' This function performs the bootstrap regression test for given data X,Y. It
#' uses indep_bs, empirical_bs, res_bs, fixed_design_bs_Hnull, fixed_design_bs,
#' hybrid_null_bs as bootstrap resampling schemes to perform the bootstrap and
#' stores all the corresponding p-values and bootstrapped test statistics in a
#' dataframe to keep track of the outputs.
#'
#' @param X numeric univariate input vector resembling the independent variables
#' @param Y numeric univariate input vector the dependent variables
#' @param nBootstrap numeric value of the amount of bootstrap resamples
#'
#' @returns a named list with `pvals_df` the df of p-values, with the vectors of
#' bootstrapped test statistics T_n^* included and `true_stat` the true test statistic.
#'
#' @return A list with components \itemize{
#'    \item \code{pvals_df} df of p-values and bootstrapped test statistics:
#'
#'    These are the p-values for the combinations of bootstrap resampling schemes,
#'    test statistics (centered and equivalent).
#'
#'    It also contains the vectors of bootstrap test statistics
#'    for each of the combinations.
#'
#'    \item \code{true_stat} a named vector of size 1 containing the true test
#'    statistic.
#' }
#'
#' @seealso \code{\link{perform_GoF_test},\link{perform_independence_test}}
#'
#' @examples
#' n = 100
#'
#' # Under H1
#' X_data = rnorm(n)
#' Y_data =  X_data + rnorm(n)   #Y = X + epsilon
#' result = perform_regression_test(X_data, Y_data, nBootstrap = 30)
#' print(result)
#' plot(result)
#'
#' # Under H0
#' X_data = rnorm(n)
#' Y_data =  rep(1, n)  #these values are exactly constant (as b = 0 under H0)
#' result = perform_regression_test(X_data, Y_data, nBootstrap = 30)
#' print(result)
#' plot(result)
#'
#' @export
perform_regression_test <- function(X, Y,
                                    nBootstrap = 100,
                                    type_boot_user = "indep_bs",
                                    type_stat_user = "eq",
                                    give_all_test_information = FALSE){

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

  bootstrap_names <- c("indep_bs", "empirical_bs",
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

  # Filter for the user-specified row dataframe
  selected_row <- subset(
    pvals_df,
    type_boot == type_boot_user &
    type_stat == type_stat_user
  )

  # If the selected row exists, extract it; otherwise return NULL
  highlighted_pval <- if (nrow(selected_row) > 0) {
    selected_row[1, , drop = FALSE]
  } else {
    NULL
  }


  ### Create the result object ###
  result <- list(
    # df of p-values
    pvals_df = pvals_df,
    # true test statistics
    true_stats = true_stat,
    # beta
    beta = b_hat,
    # highlighted user-specified df
    highlighted_pval = highlighted_pval,
    # Include number of bootstrap repetitions
    nBootstrap = nBootstrap,
    # give bootstrap method a name
    nameMethod = "Bootstrap Regression Test"
    )

  # make a class for the result object
  class(result) <- c("bootstrapTest_regression", "bootstrapTest")
  return(result)
}


#' @export
print.bootstrapTest_regression <- function(x,
                                             give_all_test_information = FALSE,
                                             ...){
  cat("         🎯 Bootstrap Regression Test Results 🎯\n")
  cat("         =========================================\n\n")

  # Highlighted row
  if (!is.null(x$highlighted_pval)) {
    row <- x$highlighted_pval

    # Get the true statistic
    norm_type_true_stat <- row$norm_type
    true_stat <- x$true_stats[[norm_type_true_stat]]

    # Get quantiles
    row$ci_upper_95 <- sapply(row$bootstrapped_tests, function(x) stats::quantile(x, 0.975))
    row$ci_upper_99 <- sapply(row$bootstrapped_tests, function(x) stats::quantile(x, 0.995))

    cat("Performed test:\n")
    cat(sprintf("  Bootstrap type           : %s\n", row$type_boot))
    cat(sprintf("  Bootstrap repetitions    : %d\n", x$nBootstrap))
    cat(sprintf("  Type of test statistic   : %s\n", row$type_stat))
    cat(sprintf("  Type of norm used        : %s\n", row$norm_type))
    cat( paste0("  p-value                  : ", row$pvalues,"\n"))
    #cat(sprintf("  p-value                  : %.4f\n", row$pvalues))
    cat(sprintf("  True test statistic      : %.4f\n", true_stat))
    cat(sprintf("  95%% Confidence Interval  : [%.4f, %.4f]\n", row$ci_lower_95, row$ci_upper_95))
    cat(sprintf("  99%% Confidence Interval  : [%.4f, %.4f]\n", row$ci_lower_99, row$ci_upper_99))
    cat("\n")
  } else {
    cat("No highlighted test selected.\n\n")
  }

  if (give_all_test_information == TRUE) {
    # Print all testing information

    # Print the full p-values dataframe
    df <- x$pvals_df

    # Get confidence intervals
    df$ci_lower_95 <- sapply(df$bootstrapped_tests, function(x) stats::quantile(x, 0.025))
    df$ci_upper_95 <- sapply(df$bootstrapped_tests, function(x) stats::quantile(x, 0.975))
    df$ci_lower_99 <- sapply(df$bootstrapped_tests, function(x) stats::quantile(x, 0.005))
    df$ci_upper_99 <- sapply(df$bootstrapped_tests, function(x) stats::quantile(x, 0.995))

    # Print all test results
    cat("All test results:\n\n")
    print(df, row.names = FALSE)

    # Print true test statistics
    cat("\nTrue test statistics:\n")
    print(x$true_stats)
  }
>>>>>>> Stashed changes
}
