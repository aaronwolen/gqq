# verify validity of pvalues
check_pvalues <- function(p) {
  stopifnot(is.numeric(p))

  prange <- range(p, na.rm = TRUE)
  if (prange[1] < 0 | prange[2] > 1) {
    stop("p must be a numeric vector with values between 0 and 1.", call. = FALSE)
  }

  pna <- is.na(p)
  if (any(pna)) {
    msg <- sprintf("Omitted %d missing p-value(s)", sum(pna))
    warning(msg, call. = FALSE)
    p <- na.omit(p)
  }

  p0 <- p == 0
  if (any(p0)) {
    pmin <- min(p[!p0])
    msg <- sprintf("Replaced %d p-value(s) equal to zero with %g", sum(p0), pmin)
    warning(msg, call. = FALSE)
    p[p0] <- pmin
  }

  return(p)
}


# calculate confidence intervals
# the jth order statistic from a uniform(0,1) sample has a beta(j,n-j+1) distribution
# (Casella & Berger, 2002, 2nd edition, pg 230, Duxbury)
calc_ci <- function(n, level = 0.95) {
  tail <- (1 - level) / 2
  p <- c(0 + tail, 1 - tail)

  nseq <- seq_len(n)
  lo <- qbeta(0.95, nseq, n - nseq + 1)
	hi <- qbeta(0.05, nseq, n - nseq + 1)

	list(lo = -log10(lo), hi = -log10(hi))
}
