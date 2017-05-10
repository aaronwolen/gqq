#' Downsampled QQ-plot
#'
#' @param p vector of p-values
#' @param maxAxis maximum range (on a log10 scale) of the X and Y axes
#' @param pdown p-value threshold for plot downsampling
#' @param downsample proportion of points to plot
#' @param qpoints logical, should the points be added to an existing plot?
#' @param pchSet either an integer specifying a symbol or a single character to
#'   be used as the default in plotting points
#' @param cexSet numerical value giving the amount by which the points should be
#'   magnified default = 1)
#' @param qColor color of points
#' @param bigPoints TODO
#'
#' @examples
#' library(pgcxd)
#' scz
#' qqPlot(scz$p, pdown = 3, downsample = 0.01, qpoints = FALSE, pchSet = 1, cexSet = 1, qColor = "black", bigPoints = 6)

qqPlot <-
  function(p,
           maxAxis,
           qpoints,
           qColor,
           pdown,
           downsample,
           cexSet,
           pchSet,
           bigPoints) {

	#bigPoints<-5
	#cexSet<-0.1
	#pchSet<-1
	cexJump<-5

	p[p == 0] <- min(p[p > 0])

	logP1 <- -log10(sort(na.omit(p), decreasing = F))
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


  ###Make downsampled values for plotting
	cutT<-runif(N1)
	null1F<-null1[(null1 <pdown &cutT <=downsample)| null1>=pdown]
	logP1F<-logP1[(null1 <pdown &cutT <=downsample)| null1>=pdown]
	logConf95F<-logConf95[(null1 <pdown &cutT <=downsample)| null1>=pdown]
	logConf05F<-logConf05[(null1 <pdown &cutT <=downsample)| null1>=pdown]

	###Create new qqplot
	if(qpoints==F) {
	## plot the confidence lines
	plot(null1F, logConf95F, ylim=c(0,MAXT), xlim=c(0,MAXT), type="l",axes=FALSE, xlab="", ylab="",col="red")
	##No downsampling
#	plot(null1, logConf95, ylim=c(0,MAXT), xlim=c(0,MAXT), type="l",axes=FALSE, xlab="", ylab="",col="blue")
	par(new=T)
	plot(null1F, logConf05F, ylim=c(0,MAXT), xlim=c(0,MAXT), type="l",axes=FALSE, xlab="", ylab="", col="red")
	##No downsampling
#	plot(null1, logConf05, ylim=c(0,MAXT), xlim=c(0,MAXT), type="l",axes=FALSE, xlab="", ylab="", col="blue")
	par(new=T)
	## add the diagonal
	abline(0,1,col="red")
	par(new=T)
	#plot(null1F, logP1F, ylim=c(0,MAXT), xlim=c(0,MAXT),xlab="Expected", ylab="Observed",cex=.5,col=qColor,pch=3 )
	plot(null1F, logP1F, ylim=c(0,MAXT), xlim=c(0,MAXT),xlab="Expected", ylab="Observed",cex=cexSet,col=qColor,pch=pchSet )
	points(null1F[null1F>bigPoints], logP1F[null1F>bigPoints],col=qColor,cex=cexSet*cexJump,pch=pchSet )

	###No downsample for plotting
	#plot(null1, logP1, ylim=c(0,MAXT), xlim=c(0,MAXT),xlab="Expected", ylab="Observed",cex=.5,col=qColor )
					}

	###Add qqpoints
#	if(qpoints==T) {points(null1F, logP1F,col=qColor,cex=.5,pch=4 )}
	if(qpoints==T) {points(null1F, logP1F,col=qColor,cex=cexSet,pch=pchSet )
					points(null1F[null1F>bigPoints], logP1F[null1F>bigPoints],col=qColor,cex=cexSet*cexJump,pch=pchSet )}
#	if(qpoints==T) {points(null1, logP1,cex=.5,col=qColor,pch=4 )}
}