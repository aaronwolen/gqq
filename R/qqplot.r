#' Downsampled QQ-plot
#'
#' @param p vector of p-values
#' @param pdown p-value threshold (on a log10 scale) for plot downsampling
#' @param downsample proportion of points to plot
#' @param pch plotting 'character' (i.e., symbol) to use. See
#'   \code{\link[graphics]{points}} for possible values.
#' @param col color code or name, see \code{\link[graphics]{par}}
#' @param cex character (or symbol) expansion: a numerical vector. This works as
#'   a multiple of \code{\link[graphics]{par}("cex")}.
#' @param qpoints logical, should the points be added to an existing plot?
#' @param ci.level confidence level between 0 and 1. Set \code{ci.level = NULL} to
#'   avoid plotting confidence intervals (ignored if \code{qpoints = TRUE})
#' @param ci.color color of confidence interval band
#' @param max.axix maximum range (on a log10 scale) of the X and Y axes
#' @param highlight p-values above this (log10) threshold will be plotted with
#'   points whose size is determined by \code{cex.highlight}, useful for
#'   highlighting strong associations of interest
#' @param cex.highlight the magnification applied to points above the
#'   \code{highlight} threshold
#'
#' @examples
#' library(pgcxd)
#' scz
#' qqPlot(scz$pval, highlight = 6)

qqPlot <-
  function(p,
           pdown = 3,
           downsample = 0.01,
           pch = 1,
           col = "grey20",
           cex = 0.1,
           qpoints = FALSE,
           ci.level = 0.9,
           ci.color = "grey70",
           highlight = NULL,
           cex.highlight = 1.2,
           max.axis = NULL) {

	p <- check_pvalues(p)

	logP1 <- -log10(sort(p, decreasing = F))
	N1 <- length(logP1) ## number of p-values

	### create the null distribution (-log10 of the uniform)
	null1 <- -log10(seq_len(N1) / N1)
	max.axis <- if (is.null(max.axis)) max(c(logP1, null1))

  # downsample values
	cutT <- runif(N1)
	index <- (null1 < pdown & cutT <= downsample) | null1 >= pdown
	null1F <- null1[index]
	logP1F <- logP1[index]

	# create new qqplot
	if(qpoints == FALSE) {

	  plot(NA,
	    type = "n",
	    ylim = c(0, max.axis), xlim = c(0, max.axis),
	    xlab = "Expected", ylab = "Observed"
	  )

	  # confidence intervals
	  if (!is.null(ci.level)) {
	    ci <- calc_ci(N1, ci.level)
	    lines(null1, ci$lo, col = ci.color)
	    lines(null1, ci$hi, col = ci.color)
	  }
	  abline(0, 1, col = ci.color)
	}

  points(null1F, logP1F, col = col, cex = cex, pch = pch)

	if (!is.null(highlight)) {
	  i <- which(null1F > highlight)
	  points(null1F[i], logP1F[i], col = col, cex = cex.highlight, pch = pch)
	}
}