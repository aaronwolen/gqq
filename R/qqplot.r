#' Downsampled QQ-plot
#'
#' @param p vector of p-values
#' @param pdown p-value threshold (on a log10 scale) for plot downsampling
#' @param downsample proportion of points to plot
#' @param qColor color of points
#' @param qpoints logical, should the points be added to an existing plot?
#' @param pchSet either an integer specifying a symbol or a single character to
#'   be used as the default in plotting points
#' @param cexSet numerical value giving the amount by which the points should be
#'   magnified default = 1)
#' @param maxAxis maximum range (on a log10 scale) of the X and Y axes
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
           qColor = "grey20",
           qpoints = FALSE,
           pchSet = 1,
           cexSet = 0.1,
           highlight = NULL,
           cex.highlight = 1.2,
           maxAxis) {

	p <- check_pvalues(p)

	logP1 <- -log10(sort(p, decreasing = F))
	N1 <- length(logP1) ## number of p-values

	### create the null distribution (-log10 of the uniform)
	null1 <- -log10(seq_len(N1) / N1)
	MAX1 <- max(c(logP1, null1))

	if (missing(maxAxis))
	  MAXT<-MAX1
	else
	  MAXT <-maxAxis

	### create the confidence intervals
	c95_1 <- rep(0,N1)
	c05_1 <- rep(0,N1)

	### the jth order statistic from a uniform(0,1) sample has a beta(j,n-j+1) distribution
	###(Casella & Berger, 2002, 2nd edition, pg 230, Duxbury)
	for(ii in 1:N1) {
	  c95_1[ii] <- qbeta(0.95, ii, N1 - ii + 1)
	  c05_1[ii] <- qbeta(0.05, ii, N1 - ii + 1)
	}

	logConf95 <- (-log(c95_1, 10))
	logConf05 <- (-log(c05_1, 10))

  # downsample values
	cutT<-runif(N1)
	null1F<-null1[(null1 <pdown &cutT <=downsample)| null1>=pdown]
	logP1F<-logP1[(null1 <pdown &cutT <=downsample)| null1>=pdown]
	logConf95F<-logConf95[(null1 <pdown &cutT <=downsample)| null1>=pdown]
	logConf05F<-logConf05[(null1 <pdown &cutT <=downsample)| null1>=pdown]

	# create new qqplot
	if(qpoints == FALSE) {

	  plot(NA,
	    type = "n",
	    ylim = c(0, MAXT), xlim = c(0, MAXT),
	    xlab = "Expected", ylab = "Observed"
	  )

	  # confidence intervals
	  lines(null1F, logConf95F, col = "red")
	  lines(null1F, logConf05F, col = "red")
	  abline(0,1,col="red")
	}

  points(null1F, logP1F, col = qColor, cex = cexSet, pch = pchSet)

	if (!is.null(highlight)) {
	  i <- which(null1F > highlight)
	  points(null1F[i], logP1F[i], col = qColor, cex = cex.highlight, pch = pchSet)
	}
}