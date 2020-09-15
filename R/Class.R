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
#' @name winCurse-class
#' @rdname winCurse-class
#' @exportClass winCurse

setClass(
  Class = "winCurse",
  representation = representation(
   Assignments = "data.frame",
   Estimates = "list",
   Expectations = "numeric",
   Responsibilities = "data.frame"
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

print.winCurse <- function (x, ...) {
  
  # Parameter Estimates.
  cat('Estimated null proportion:\n')
  show(signif(x@Estimates$pi, digits = 3))
  cat('\n\n')
  
  cat('Estimated non-null variance component:\n')
  show(signif(x@Estimates$tau2, digits = 3))
  cat('\n\n')

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
  definition = function (object) {print.winCurse(x = object)}
)

