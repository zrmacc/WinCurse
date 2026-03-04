test_that("print.winCurse runs without error", {
  data(wc_data)
  fit <- fit.WinCurse(
    theta = wc_data$theta[1:3],
    se = wc_data$se[1:3],
    maxit = 10
  )
  expect_no_error(print(fit))
})

test_that("show,winCurse-method runs without error", {
  data(wc_data)
  fit <- fit.WinCurse(
    theta = wc_data$theta[1:3],
    se = wc_data$se[1:3],
    maxit = 10
  )
  expect_no_error(show(fit))
})

test_that("accessors return correct components", {
  data(wc_data)
  fit <- fit.WinCurse(
    theta = wc_data$theta[1:5],
    se = wc_data$se[1:5],
    maxit = 10
  )
  expect_equal(estimates(fit), fit@Estimates)
  expect_equal(expectations(fit), fit@Expectations)
  expect_equal(responsibilities(fit), fit@Responsibilities)
  expect_equal(assignments(fit), fit@Assignments)
  expect_equal(convergence(fit), fit@Convergence)
})

test_that("accessors error on non-winCurse", {
  expect_error(estimates(1))
  expect_error(expectations("x"))
})
