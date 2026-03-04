# -----------------------------------------------------------------------------
# Input validation (internal)
# -----------------------------------------------------------------------------

#' Validate theta and se inputs
#'
#' @param theta Parameter estimates (numeric vector).
#' @param se Parameter standard errors (numeric vector).
#' @return Invisible NULL; stops with an error if invalid.
#' @keywords internal
.ValidateThetaSe <- function(theta, se) {
  if (!is.numeric(theta) || !is.numeric(se)) {
    stop("'theta' and 'se' must be numeric.")
  }
  if (any(is.na(theta)) || any(is.na(se))) {
    stop("'theta' and 'se' must not contain NA or NaN.")
  }
  if (length(theta) != length(se)) {
    stop("'theta' and 'se' must have the same length.")
  }
  if (any(se <= 0)) {
    stop("'se' must be positive.")
  }
  return(invisible(NULL))
}

#' Validate pi and tau2 for PostExp
#'
#' @param pi Proportion null (single number).
#' @param tau2 Variance component (single number).
#' @keywords internal
.ValidatePiTau2 <- function(pi, tau2) {
  if (!is.numeric(pi) || length(pi) != 1L || is.na(pi) || pi < 0 || pi > 1) {
    stop("'pi' must be a single number in [0, 1].")
  }
  if (!is.numeric(tau2) || length(tau2) != 1L || is.na(tau2) || tau2 <= 0) {
    stop("'tau2' must be a single positive number.")
  }
  return(invisible(NULL))
}

# -----------------------------------------------------------------------------
# Responsibility
# -----------------------------------------------------------------------------

#' Responsibility
#'
#' Computes posterior probability of null component membership.
#'
#' @param theta Parameter estimates.
#' @param se Parameter standard errors.
#' @param pi Proportion null parameters.
#' @param tau2 Variance component.
#' @importFrom stats dnorm
#' @return Vector of responsibilities (null component probabilities).
#' @keywords internal

Responsibility <- function(
  theta, 
  se,
  pi,
  tau2
) { 
  
  # Null probability.
  p0 <- dnorm(
    x = theta,
    mean = 0,
    sd = se
  ) * pi
  
  # Alternative probability.
  p1 <- dnorm(
    x = theta,
    mean = 0,
    sd = sqrt(se^2 + tau2)
  ) * (1 - pi)
  
  # Responsibilities.
  gamma <- p0 / (p0 + p1)
  return(gamma)
}


# -----------------------------------------------------------------------------

#' EM Objective
#'
#' Expected complete-data log-likelihood for the EM algorithm.
#'
#' @param theta Parameter estimates.
#' @param se Parameter standard errors.
#' @param pi Proportion null parameters.
#' @param tau2 Variance component.
#' @param gamma Responsibilities.
#' @importFrom stats dnorm
#' @return Numeric EM objective (expected complete-data log-likelihood).
#' @keywords internal 

EM.Objective <- function(
  theta,
  se,
  pi,
  tau2,
  gamma
) { 
  summand <- gamma * dnorm(
    x = theta,
    mean = 0, 
    sd = se,
    log = TRUE
  ) + (1 - gamma) * dnorm(
    x = theta,
    mean = 0,
    sd = sqrt(se^2 + tau2),
    log = TRUE
  )
  out <- sum(summand)
  return(out)
}


# -----------------------------------------------------------------------------

#' EM Score Equation for Tau2
#'
#' Score (derivative) of the EM objective with respect to tau2.
#'
#' @param theta Parameter estimates.
#' @param se Parameter standard errors.
#' @param tau2 Variance component.
#' @param gamma Responsibilities.
#' @importFrom stats dnorm
#' @return Numeric value of the EM score for tau2 (zero at the M-step optimum).
#' @keywords internal 

EM.Score <- function(
  theta,
  se,
  tau2,
  gamma
) {
  v <- se^2 + tau2
  summand <- -0.5 * (1 - gamma) * ((1 / v) - (theta^2 / v^2) )
  out <- sum(summand)
  return(out)
}


# -----------------------------------------------------------------------------

#' EM Update
#'
#' One iteration of the EM algorithm (E-step and M-step).
#'
#' @param theta Parameter estimates.
#' @param se Parameter standard errors.
#' @param pi Proportion null parameters.
#' @param tau2 Variance component.
#' @param tau2.upper Upper bound for tau2 in uniroot (optional).
#' @importFrom stats uniroot
#' @return List containing:
#' \itemize{
#'   \item 'pi' updated value of pi.
#'   \item 'tau2' updated value of tau2.
#'   \item 'delta' increment in the EM objective.
#' }
#' @keywords internal

EM.Update <- function(
  theta,
  se,
  pi,
  tau2,
  tau2.upper = 1
) {

  # Calculate responsibilities.
  gamma <- Responsibility(
    theta = theta,
    se = se,
    pi = pi,
    tau2 = tau2
  )

  # Initial objective.
  q0 <- EM.Objective(
    theta = theta,
    se = se,
    pi = pi,
    tau2 = tau2,
    gamma = gamma
  )

  # Update tau2 (data-dependent upper bound).
  tau2.new <- uniroot(
    f = function(x) {
      EM.Score(
        theta = theta, se = se, tau2 = x, gamma = gamma
      )
    },
    lower = 1e-8,
    upper = max(1, tau2.upper),
    extendInt = "downX"
  )$root

  # Update pi.
  pi.new <- sum(gamma) / length(theta)

  # Final objective.
  q1 <- EM.Objective(
    theta = theta,
    se = se,
    pi = pi.new,
    tau2 = tau2.new,
    gamma = gamma
  )

  # EM increment.
  out <- list(
    'pi' = pi.new,
    'tau2' = tau2.new,
    'delta' = q1 - q0
  )
  return(out)
}


# -----------------------------------------------------------------------------

#' Posterior Expectation
#'
#' Calculates the posterior expectation given available \code{pi} and \code{tau2}.
#' \code{theta} and \code{se} must have the same length and \code{se} must be positive.
#'
#' @param theta Parameter estimates (numeric vector), or column name if \code{data} is given.
#' @param se Parameter standard errors (numeric vector), or column name if \code{data} is given.
#' @param pi Proportion of null parameters (numeric scalar between zero and one).
#' @param tau2 Variance component (single positive number).
#' @param data Optional data frame; if provided, \code{theta} and \code{se} are taken as column names.
#' @export
#' @return Numeric vector of the posterior expected effect sizes.
#' @examples
#' data(wc_data)
#' post_exp <- PostExp(
#'   theta = wc_data$theta,
#'   se = wc_data$se,
#'   pi = 0.75,
#'   tau2 = 0.05
#' )
#' PostExp(theta = "theta", se = "se", pi = 0.75, tau2 = 0.05, data = wc_data)

PostExp <- function(
  theta,
  se,
  pi,
  tau2,
  data = NULL
) {

  if (!is.null(data)) {
    theta <- data[[theta]]
    se <- data[[se]]
  }
  .ValidateThetaSe(theta = theta, se = se)
  .ValidatePiTau2(pi = pi, tau2 = tau2)

  # Responsibilities.
  gamma <- Responsibility(
    theta = theta,
    se = se,
    pi = pi,
    tau2 = tau2
  )

  # Posterior expectation.
  out <- theta * (tau2) / (se^2 + tau2) * (1 - gamma)
  return(out)
}

# -----------------------------------------------------------------------------

#' Fit Winner's Curse Model
#' 
#' @param theta Parameter estimates.
#' @param se Parameter standard errors. 
#' @param pi Initial value of pi, the proportion of null parameters. 
#' @param tau2 Initial value of tau, the variance component.
#' @param eps Tolerance for EM iterations (minimum objective increment).
#' @param maxit Maximum number of EM iterations.
#' @param report Report fitting progress?
#' @param data Optional data frame; if provided, \code{theta} and \code{se} are column names.
#' @importFrom methods new
#' @importFrom stats var median
#' @export
#' @return Object of class 'winCurse' containing:
#' \itemize{
#'   \item `@Assignments`: Maximum a posteriori component assignment 
#'     and assignment entropy.
#'   \item `@Estimates`: Estimated model parameters. 
#'   \item `@Expectations`: Posterior expected effect sizes. 
#'   \item \code{Responsibilities}: Probabilities null and non-null.
#'   \item \code{Convergence}: List with \code{niter} and \code{converged}.
#' }
#' @examples
#' data(wc_data)
#' fit <- fit.WinCurse(theta = wc_data$theta, se = wc_data$se)
#' fit <- fit.WinCurse(theta = "theta", se = "se", data = wc_data)

fit.WinCurse <- function(
  theta,
  se,
  pi = NULL,
  tau2 = NULL,
  eps = 1e-6,
  maxit = 100,
  report = FALSE,
  data = NULL
) {

  if (!is.null(data)) {
    theta <- data[[theta]]
    se <- data[[se]]
  }
  .ValidateThetaSe(theta = theta, se = se)

  n <- length(theta)
  if (is.null(pi)) {
    pi <- 0.5
  }
  if (is.null(tau2)) {
    tau2 <- max(0.01, stats::var(theta) - stats::median(se^2))
    if (!is.finite(tau2) || tau2 <= 0) {
      tau2 <- 1
    }
  }

  tau2.upper <- max(1, 10 * max(se^2), stats::var(theta))

  niter <- 0L
  converged <- FALSE
  for (i in 1:maxit) {

    update <- EM.Update(
      theta = theta,
      se = se,
      pi = pi,
      tau2 = tau2,
      tau2.upper = tau2.upper
    )
    niter <- i

    if (update$delta <= 0) {
      warning("EM objective did not increase (iteration ", i, "); stopping.")
      break
    }

    pi <- update$pi
    tau2 <- update$tau2

    if (report) {
      cat("Objective increment: ", signif(update$delta, digits = 3), "\n")
    }

    if (update$delta < eps) {
      converged <- TRUE
      break
    }
  }

  gamma <- Responsibility(
    theta = theta,
    se = se,
    pi = pi,
    tau2 = tau2
  )
  response <- data.frame(
    'null' = gamma,
    'non_null' = 1 - gamma
  )

  rr <- pmax(response, .Machine$double.xmin)
  assignments <- data.frame(
    'non_null' = max.col(response) - 1L,
    'entropy' = -rowSums(response * log(rr)) / log(2)
  )

  expectations <- theta * (tau2) / (se^2 + tau2) * (1 - gamma)

  out <- new(
    Class = "winCurse",
    Assignments = assignments,
    Estimates = list(pi = pi, tau2 = tau2),
    Expectations = expectations,
    Responsibilities = response,
    Convergence = list(niter = niter, converged = converged)
  )
  return(out)
}