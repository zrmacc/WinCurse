test_that("Responsibility returns values in [0, 1] and sums sensibly", {
  theta <- c(0.1, -0.2, 0.5)
  se <- c(0.2, 0.2, 0.3)
  pi <- 0.7
  tau2 <- 0.05
  gamma <- WinCurse:::Responsibility(theta = theta, se = se, pi = pi, tau2 = tau2)
  expect_true(all(gamma >= 0 & gamma <= 1))
  expect_length(gamma, length(theta))
})

test_that("EM.Objective is finite for valid inputs", {
  theta <- c(0.1, -0.2)
  se <- c(0.2, 0.2)
  pi <- 0.7
  tau2 <- 0.05
  gamma <- c(0.8, 0.3)
  obj <- WinCurse:::EM.Objective(
    theta = theta, se = se, pi = pi, tau2 = tau2, gamma = gamma
  )
  expect_true(is.finite(obj))
  expect_length(obj, 1)
})

test_that("EM.Update returns list with pi, tau2, delta", {
  theta <- c(0.1, -0.2, 0.5)
  se <- c(0.2, 0.2, 0.3)
  pi <- 0.5
  tau2 <- 0.5
  out <- WinCurse:::EM.Update(theta = theta, se = se, pi = pi, tau2 = tau2)
  expect_named(out, c("pi", "tau2", "delta"))
  expect_true(out$pi >= 0 && out$pi <= 1)
  expect_true(out$tau2 > 0)
  expect_true(is.finite(out$delta))
})

test_that("PostExp returns numeric vector of correct length", {
  data(wc_data)
  pe <- PostExp(
    theta = wc_data$theta,
    se = wc_data$se,
    pi = 0.75,
    tau2 = 0.05
  )
  expect_type(pe, "double")
  expect_length(pe, nrow(wc_data))
})

test_that("fit.WinCurse returns winCurse object with expected slots", {
  data(wc_data)
  fit <- fit.WinCurse(
    theta = wc_data$theta,
    se = wc_data$se,
    pi = 0.5,
    tau2 = 1,
    maxit = 50
  )
  expect_s4_class(fit, "winCurse")
  expect_named(fit@Estimates, c("pi", "tau2"))
  expect_true(fit@Estimates$pi >= 0 && fit@Estimates$pi <= 1)
  expect_true(fit@Estimates$tau2 > 0)
  expect_equal(nrow(fit@Assignments), nrow(wc_data))
  expect_equal(nrow(fit@Responsibilities), nrow(wc_data))
  expect_length(fit@Expectations, nrow(wc_data))
  expect_named(fit@Assignments, c("non_null", "entropy"))
  expect_named(fit@Convergence, c("niter", "converged"))
  expect_true(is.integer(fit@Convergence$niter) || is.numeric(fit@Convergence$niter))
  expect_true(is.logical(fit@Convergence$converged))
})

test_that("fit.WinCurse with data= and column names works", {
  data(wc_data)
  fit_df <- fit.WinCurse(theta = "theta", se = "se", data = wc_data, maxit = 20)
  fit_vec <- fit.WinCurse(theta = wc_data$theta, se = wc_data$se, maxit = 20)
  expect_equal(fit_df@Estimates$pi, fit_vec@Estimates$pi)
  expect_equal(fit_df@Estimates$tau2, fit_vec@Estimates$tau2)
})

test_that("PostExp with data= and column names works", {
  data(wc_data)
  pe1 <- PostExp(theta = wc_data$theta, se = wc_data$se, pi = 0.75, tau2 = 0.05)
  pe2 <- PostExp(theta = "theta", se = "se", pi = 0.75, tau2 = 0.05, data = wc_data)
  expect_equal(pe1, pe2)
})

test_that("fit.WinCurse assignment entropy is finite (no NaN from 0*log(0))", {
  data(wc_data)
  fit <- fit.WinCurse(
    theta = wc_data$theta,
    se = wc_data$se,
    pi = 0.5,
    tau2 = 1,
    maxit = 50
  )
  expect_true(all(is.finite(fit@Assignments$entropy)))
})

test_that("PostExp gives error on invalid input", {
  data(wc_data)
  expect_error(PostExp(theta = c(1, NA), se = c(0.1, 0.1), pi = 0.5, tau2 = 0.05))
  expect_error(PostExp(theta = c(1, 2), se = c(0.1), pi = 0.5, tau2 = 0.05))
  expect_error(PostExp(theta = wc_data$theta, se = wc_data$se, pi = -0.1, tau2 = 0.05))
  expect_error(PostExp(theta = wc_data$theta, se = wc_data$se, pi = 0.5, tau2 = -0.01))
})

test_that("fit.WinCurse gives error on invalid input", {
  data(wc_data)
  expect_error(fit.WinCurse(theta = c(1, NA), se = c(0.1, 0.1), maxit = 5))
  expect_error(fit.WinCurse(theta = c(1, 2), se = c(0.1), maxit = 5))
  expect_error(fit.WinCurse(theta = c(1, 2), se = c(-0.1, 0.1), maxit = 5))
})

test_that("fit.WinCurse with report = TRUE does not error", {
  data(wc_data)
  expect_no_error(
    fit.WinCurse(
      theta = wc_data$theta[1:5],
      se = wc_data$se[1:5],
      report = TRUE,
      maxit = 5
    )
  )
})

test_that("wc_data has expected structure", {
  data(wc_data)
  expect_s3_class(wc_data, "data.frame")
  expect_true(all(c("non_null", "theta", "se") %in% names(wc_data)))
})
