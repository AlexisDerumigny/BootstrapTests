
#' Print the bootstrap test results
#' @param x an object of class \code{bootstrapTest_independence},
#'        \code{bootstrapTest_GoF}, \code{bootstrapTest_regression} or \code{bootstrapTest}
#'
#' @param ... additional arguments passed to the \code{print} function
#'
#' @export
print.bootstrapTest <- function(x,
                                ...){
  # print a nice layout
  welcome_message_name <- paste0("         🎯" , x$nameMethod, " Results 🎯\n")
  equal_signs <- paste(rep("=", nchar(welcome_message_name) + 6), collapse = "")
  cat(welcome_message_name, equal_signs, "\n\n")

  # Highlighted row
  if (nrow(x$pvals_df) == 1) {
    row <- x$pvals_df

    # Get the true statistic
    if ("bootstrapTest_independence" %in% class(x)){
      true_stat <- x$true_stats[[row$norm_type]]
    } else if("bootstrapTest_GoF" %in% class(x)){

      true_stat <- switch(
        row$param_bs,
        'canonical' = {
          x$true_stats["KS_with_canonical"]
        },
        'MD' = {
          x$true_stats["KS_with_MD"]
        },
        'MD-cent' = {
          x$true_stats["KS_with_MD"]
        },
        stop("Unknown 'param_bs': ", row$param_bs)
      )
    } else {
      true_stat <- x$true_stats
    }

    # Get quantiles
    row$quantile_95 <- sapply(row$bootstrapped_tests, function(x) stats::quantile(x, 0.95))
    row$quantile_99 <- sapply(row$bootstrapped_tests, function(x) stats::quantile(x, 0.99))

    cat("Performed test:\n")
    cat(sprintf("  Bootstrap type           : %s\n", row$type_boot))
    cat(sprintf("  Bootstrap repetitions    : %d\n", x$nBootstrap))
    cat(sprintf("  Type of test statistic   : %s\n", row$type_stat))
    if ("bootstrapTest_independence" %in% class(x)){
      cat(sprintf("  Type of norm used        : %s\n", row$norm_type))
    } else if ("bootstrapTest_regression" %in% class(x)){
      cat("  Beta                     :", x$beta, "\n")
    } else if ("bootstrapTest_GoF" %in% class(x)){
      cat("  Bootstrap estimator used :", row$param_bs, "\n")
    }
    cat( paste0("  p-value                  : ", row$pvalues,"\n"))
    #cat(sprintf("  p-value                  : %.4f\n", row$pvalues))
    cat(sprintf("  True test statistic      : %.4f\n", true_stat))
    cat(sprintf("  Critical value at 5%%     : %.4f\n", row$ci_upper_95))
    cat(sprintf("  Critical value at 1%%     : %.4f\n", row$ci_upper_99))
    cat("\n")
  } else if ( nrow(x$pvals_df) >= 1 ) {
    # Print all testing information

    # Print the full p-values dataframe
    df <- x$pvals_df

    # Get quantiles
    df$quantile_95 <- sapply(df$bootstrapped_tests, function(x) stats::quantile(x, 0.95))
    df$quantile_99 <- sapply(df$bootstrapped_tests, function(x) stats::quantile(x, 0.99))

    # Print all test results
    cat("All test results:\n\n")
    print(df, row.names = FALSE)

    # Print true test statistics
    cat("\nTrue test statistics:\n")
    print(x$true_stats)
  }
}

