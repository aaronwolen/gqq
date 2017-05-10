context("Utilities")

p <- seq(0.1, 1, 0.1)

test_that("Invalid p-values throw errors", {
  expect_identical(check_pvalues(p), p)
  expect_error(check_pvalues(c(-0.1, p)))
  expect_error(check_pvalues(c(1.1, p)))
})

test_that("p-values equal to zero are replaced with lower limit", {
  expect_warning(p2 <- check_pvalues(c(0, p)))
  expect_identical(p2, c(min(p), p))
})

test_that("Missing p-values are omitted", {
  expect_warning(p2 <- check_pvalues(c(NA, p)))
  expect_identical(length(p2), length(p))
  expect_false(any(is.na(p2)))
})
