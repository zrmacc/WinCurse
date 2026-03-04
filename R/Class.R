#' Winner's Curse Object
#'
#' Defines the object class returned by \code{\link{fit.WinCurse}}.
#'
#' @slot Assignments Maximum a posteriori component assignment
#'     and assignment entropy.
#' @slot Estimates Estimated model parameters.
#' @slot Expectations Posterior expected effect sizes.
#' @slot Responsibilities Probabilities the parameter came from the null and
#'   non-null components.
#' @slot Convergence List with \code{niter} (number of EM iterations) and
#'   \code{converged} (logical).
#' @name winCurse-class
#' @rdname winCurse-class
#' @exportClass winCurse

setClass(
  Class = "winCurse",
  representation = representation(
    Assignments = "data.frame",
    Estimates = "list",
    Expectations = "numeric",
    Responsibilities = "data.frame",
    Convergence = "list"
  )
)

# -----------------------------------------------------------------------------
# Print Method
# -----------------------------------------------------------------------------

#' Print Method for Winner's Curse Object.
#'
#' Print method for objects of class \code{winCurse}.
#'
#' @param x An object of class \code{winCurse}.
#' @param ... Unused.
#' @export

print.winCurse <- function(x, ...) {

  cat("Estimated null proportion:\n")
  show(signif(x@Estimates$pi, digits = 3))
  cat("\n\n")
  cat("Estimated non-null variance component:\n")
  show(signif(x@Estimates$tau2, digits = 3))
  cat("\n\n")
  return(invisible(x))
}

# -----------------------------------------------------------------------------
# Show Method
# -----------------------------------------------------------------------------

#' Show Method for Winner's Curse Object
#'
#' @param object An object of class \code{winCurse}.
#' @rdname fit-method
#' @importFrom methods show

setMethod(
  f = "show",
  signature = c(object = "winCurse"),
  definition = function(object) {
    print.winCurse(x = object)
    return(invisible(object))
  }
)

# -----------------------------------------------------------------------------
# Accessors
# -----------------------------------------------------------------------------

#' Extract estimates from a winCurse fit
#'
#' @param x An object of class \code{winCurse}.
#' @return List with \code{pi} and \code{tau2}.
#' @export
estimates <- function(x) {
  if (!inherits(x, "winCurse")) {
    stop("'x' must be of class 'winCurse'.")
  }
  return(x@Estimates)
}

#' Extract posterior expectations from a winCurse fit
#'
#' @param x An object of class \code{winCurse}.
#' @return Numeric vector of posterior expected effect sizes.
#' @export
expectations <- function(x) {
  if (!inherits(x, "winCurse")) {
    stop("'x' must be of class 'winCurse'.")
  }
  return(x@Expectations)
}

#' Extract responsibilities from a winCurse fit
#'
#' @param x An object of class \code{winCurse}.
#' @return Data frame with columns \code{null} and \code{non_null}.
#' @export
responsibilities <- function(x) {
  if (!inherits(x, "winCurse")) {
    stop("'x' must be of class 'winCurse'.")
  }
  return(x@Responsibilities)
}

#' Extract assignments from a winCurse fit
#'
#' @param x An object of class \code{winCurse}.
#' @return Data frame with \code{non_null} (0/1) and \code{entropy}.
#' @export
assignments <- function(x) {
  if (!inherits(x, "winCurse")) {
    stop("'x' must be of class 'winCurse'.")
  }
  return(x@Assignments)
}

#' Extract convergence info from a winCurse fit
#'
#' @param x An object of class \code{winCurse}.
#' @return List with \code{niter} and \code{converged}.
#' @export
convergence <- function(x) {
  if (!inherits(x, "winCurse")) {
    stop("'x' must be of class 'winCurse'.")
  }
  return(x@Convergence)
}

