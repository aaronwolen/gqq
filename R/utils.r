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
