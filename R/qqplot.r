#' Fast QQ-plots for GWAS results
#'
#' Selectively downsample p-values to avoid overplotting redundant data points.
#'
#' Since QQ-plots are on a logarithmic scale, the vast majority of values are
#' located near the origin (approaching 1). Visualizing the quantiles of such a
#' skewed distribution wastes time plotting visually redudnant data points. To
#' make this process more efficient without sacrificing information,
#' \code{qq_plot()} uses downsampling to reduce the severe overplotting typical
#' of GWAS QQ-plots. The degree of downsampling can be tuned using two
#' parameters: \code{downsample}, which specifies the proportion of points to
#' randomly remove and \code{pdown}, the (-log10) threshold below which
#' points will be downsampled.
#'
#' For example, using the default values, \code{downsample = 0.01} and
#' \code{pdown = 3}, 1% of p-values > 0.001 will be randomly removed, while all
#' p-values < 0.001 will be plotted. For one million tests, this reduces the
#' number of points plotted to 10,990.
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