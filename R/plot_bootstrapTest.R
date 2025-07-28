
#' Plot and print the bootstrap test statistics distribution
#'
#' The \code{plot} and \code{print} methods work for objects of class \code{bootstrapTest}.
#' The \code{print} method prints the summary of the bootstrap test results.
#' The \code{plot} method plots the distribution of bootstrapped test statistics
#' as a histogram, with the true test statistic and the 95% quantile of the
#' bootstrapped test statistics highlighted. In the regression test case, the
#' estimated regression line is plotted as well.
#'
#' @param x an object of class \code{bootstrapTest_independence} or \code{bootstrapTest}
#'
#' @param xlim limits for the x-axis of the histogram
#' @param breaks breaks for the histogram
#' @param legend.x position of the legend on the x-axis
#' @param legend.y position of the legend on the y-axis
#' @param ask if \code{TRUE}, the user is asked to press Return to see the next
#' plot. Used only if \code{x} is an object of class \code{bootstrapTest_regression}.
#'
#' @param plot_estimated_line Boolean describing whether to plot the estimated
#' regression line in case \code{x} is of class \code{"bootstrapTest_regression"},
#' i.e. output from \code{perform_regression_test}. By default,
#' \code{plot_estimated_line = NULL}, with the meaning that the plot is done
#' only if one estimated way of bootstrapping is given.
#'
#' @param ... additional arguments passed to the \code{hist} function
#' (in the case of the \code{plot} method) or ignored (in the case of the
#' \code{print} method).
#'
#' @returns These functions have no return value and are called solely for their
#' side effects.
#'
#' @seealso The functions that generate such object \code{x}:
#' \code{\link{perform_independence_test}, \link{perform_GoF_test},
#' \link{perform_regression_test}}.
#'
#' @export
plot.bootstrapTest <- function(x, xlim = NULL, breaks = NULL,
                               legend.x = NULL, legend.y = NULL,
                               ask = interactive(),
                               plot_estimated_line = NULL, ...){
  # assign the user-specfied highlighted dataframe

  if (nrow(x$pvals_df) == 1) {
    df <- x$pvals_df
  } else {
    N = ceiling(sqrt(nrow(x$pvals_df)))
    if (isTRUE(plot_estimated_line)){
      Nrow = N * 2
    } else {
      Nrow = N
      plot_estimated_line = FALSE
    }
    oldpar = graphics::par(mfrow = c(Nrow, N))
    on.exit(graphics::par(oldpar))

    for (i in 1:nrow(x$pvals_df)){
      y = x
      y$pvals_df <- x$pvals_df[i, ]

      plot(y, xlim = xlim, breaks = breaks,
           legend.x = legend.x, legend.y = legend.y, ask = FALSE,
           plot_estimated_line = plot_estimated_line, ...)
    }

    return (invisible(NULL))
  }
  # Get the true statistic
  if ("bootstrapTest_independence" %in% class(x)){
    true_stat <- x$true_stats[[df$norm_type]]
  } else if("bootstrapTest_GoF" %in% class(x)){

    # make distinction between MLE and MD based bootstrap methods
    mapping_parametric_bootstrap = c(MLE = "MLE", `MD-eq` = "MD",
                                     `MD-cent` = "MD")

    # add column `type_estimator` with MLE or MD based estimator
    type_estimator = mapping_parametric_bootstrap[df$type_estimator_bootstrap]

    true_stat <- x$true_stats[[type_estimator]]

  } else {
    true_stat <- x$true_stats
  }

  # Unlist and assign
  bootstrapped_test <- unlist(df$list_stat_st)

  # Make histogram of bootstrapped test statistics
  min_ = min(c(bootstrapped_test, true_stat))
  max_ = max(c(bootstrapped_test, true_stat))

  if (is.null(xlim)){
    xlim = c(0, 1.1 * max_)
  }
  if (is.null(breaks)){
    breaks = pretty(c(0, 1.1 * max_), n = 20)
  } else if (length(breaks) == 1){

    breaks = pretty(c(0, 1.1 * max_), n = breaks)
  }
  graphics::hist(bootstrapped_test,
                 main = x$nameMethod,
                 xlab = "Bootstrapped test statistic",
                 xlim = xlim,
                 breaks = breaks)

  # Get upper quantile
  quantile_upper_95 <- stats::quantile(bootstrapped_test, 0.95)

  # Show value of true statistic in the histogram
  graphics::abline(v = true_stat, col = "darkorange", lwd = 2, lty = 2)

  # Show 95% quantile in graph
  graphics::abline(v = quantile_upper_95, col = "darkblue", lwd = 2, lty = 2)

  if (is.null(legend.x)){
    legend.x = "topright"
  }

  # Legend
  graphics::legend(x = legend.x,
                   y = legend.y,
                   legend = c("True statistic", "5% critical value"),
                   col = c("darkorange", "darkblue"),
                   lty = 2,
                   lwd = 2,
                   cex = 1,           # Shrinks the text size (1 = default)
                   bty = "n",         # Removes the box around the legend
                   y.intersp = 0.7,   # Reduce vertical spacing between items
                   inset = 0.02)      # Slight inset from the edge of the plot


  ####### For regression test, also plot the slope #######

  if ("bootstrapTest_regression" %in% class(x) && !isFALSE(plot_estimated_line)) {
    if (ask) {
      oask <- grDevices::devAskNewPage(TRUE)
      on.exit(grDevices::devAskNewPage(oask))
    }

    data <- x$data
    plot(data$X, data$Y,
         main = paste0(x$nameMethod," - Regression Plot"),
         xlab = "X",
         ylab = "Y",)

    # add regression line
    linear_model <- stats::lm(Y ~ X, data = data)
    graphics::abline(a = linear_model$coefficients["(Intercept)"] ,
                     b = x$beta,
                     col = "darkorange",
                     lwd = 2,
                     lty = 1
                     )

    # Legend
    graphics::legend(x = "topright",
                     legend = c("Estimated regression line"),
                     col = c("darkorange"),
                     lty = 1,
                     lwd = 2,
                     bty = "n",         # Removes the box around the legend
    )
  }
}

