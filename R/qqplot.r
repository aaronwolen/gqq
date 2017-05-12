#' Downsampled QQ-plot
#'
#' Downsampling reduces the number of points to be plotted. Since QQ-plots are
#' on a logarithmic scale, near the origin (approaching 1) is very dense and most
#' points are plotted over each other. However, the end of the distribution
#' (approaching 0) is sparse. In order to make plotting more efficient by not
#' plotting redundant points and also not lose information, there are two
#' parameters needed for downsampling. The first is the proportion of points to
#' randomly remove (-downsample). The second is the threshold (-pdown) for which
#' points will be downsampled. For example, the options "-downsample 0.01
#' -pdown 10e-3" would random choose 1% of p-values greater than 0.001. All
#' p-values < 0.001 would be plotted. For one million tests, this reduces the
#' number of points plotted to 10990 ((1,000,000*(1-0.001)*.01)+
#' (1,000,000*0.001)).
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
#' @param max.axis maximum range (on a log10 scale) of the X and Y axes
#' @param highlight p-values above this (log10) threshold will be plotted with
#'   points whose size is determined by \code{cex.highlight}, useful for
#'   highlighting strong associations of interest
#' @param cex.highlight the magnification applied to points above the
#'   \code{highlight} threshold
#' @export
#' @import stats graphics
#'
#' @examples
#' qq_plot(runif(1e5))
#'
#' if (requireNamespace("pgcxd")) {
#'   data("scz", package = "pgcxd")
#'   qq_plot(scz$pval, highlight = 6)
#' }
qq_plot <-
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

	p <- -log10(sort(p, decreasing = F))
	n <- length(p)

	# create the null distribution (-log10 of the uniform)
	null <- -log10(ppoints(n))
	max.axis <- if (is.null(max.axis)) max(c(p, null))

  # downsample values
	cutT <- runif(n)
	index <- (null < pdown & cutT <= downsample) | null >= pdown
	nullF <- null[index]
	pF <- p[index]

	# create new qqplot
	if(qpoints == FALSE) {

	  plot(NA,
	    type = "n",
	    ylim = c(0, max.axis), xlim = c(0, max.axis),
	    xlab = "Expected", ylab = "Observed"
	  )

	  # confidence intervals
	  if (!is.null(ci.level)) {
	    ci <- calc_ci(n, ci.level)
	    lines(null, ci$lo, col = ci.color)
	    lines(null, ci$hi, col = ci.color)
	  }
	  abline(0, 1, col = ci.color)
	}

  points(nullF, pF, col = col, cex = cex, pch = pch)

	if (!is.null(highlight)) {
	  i <- which(nullF > highlight)
	  points(nullF[i], pF[i], col = col, cex = cex.highlight, pch = pch)
	}
}