
#' Plot the bootstrap test statistics distribution
#' @param x an object of class \code{bootstrapTest_independence} or \code{bootstrapTest}
#'
#' @param xlim limits for the x-axis of the histogram
#' @param breaks breaks for the histogram
#' @param legend.x position of the legend on the x-axis
#' @param legend.y position of the legend on the y-axis
#' @param ... additional arguments passed to the \code{hist} function
#'
#' @export
plot.bootstrapTest <- function(x, xlim = NULL, breaks = NULL,
                               legend.x = NULL, legend.y = NULL, ...){

  # assign the user-specfied highlighted dataframe
  df <- x$highlighted_pval

  # Get the true statistic
  if ("bootstrapTest_independence" %in% class(x)){
    true_stat <- x$true_stats[[df$norm_type]]
  } else if("bootstrapTest_GoF" %in% class(x)){
    if (df$param_bs == "canonical") {
      # for the canonical parameter estimates, we need different true test stat
      true_stat <- x$true_stats[[2]]
    } else {
      # for the MD parameter estimates, we use the first true test stat
      true_stat <- x$true_stats[[1]]
    }
  } else {
    true_stat <- x$true_stats
  }

  # Unlist and assign
  bootstrapped_test <- unlist(df$bootstrapped_tests)

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
                 main = "Bootstrap test statistics distribution",
                 sub = x$nameMethod, # TODO: find something looking better
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
}

