#' Responsibility
#' 
#' @param theta Parameter estimates.
#' @param se Parameter standard errors. 
#' @param pi Proportion null parameters. 
#' @param tau2 Variance component. 
#' @importFrom stats dnorm
#' @return Vector of responsibilities.

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
#' @param theta Parameter estimates.
#' @param se Parameter standard errors. 
#' @param pi Proportion null parameters. 
#' @param tau2 Variance component. 
#' @param gamma Responsibilities. 
#' @importFrom stats dnorm 
#' @return Numeric EM objective. 

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
#' @param theta Parameter estimates.
#' @param se Parameter standard errors. 
#' @param tau2 Variance component. 
#' @param gamma Responsibilities. 
#' @importFrom stats dnorm 
#' @return Numeric EM objective. 

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
#' @param theta Parameter estimates.
#' @param se Parameter standard errors. 
#' @param pi Proportion null parameters. 
#' @param tau2 Variance component. 
#' @importFrom stats uniroot
#' @return List containing:
#' \itemize{
#'   \item 'pi' updated value of pi.
#'   \item 'tau2' updated value of tau2.
#'   \item 'delta' increment in the EM objective. 
#' }

EM.Update <- function(
  theta,
  se,
  pi,
  tau2
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
  
  # Update tau2.
  tau2.new <- uniroot(
    f = function(x) {
      EM.Score(
        theta = theta, se = se, tau2 = x, gamma = gamma
      )
    },
    lower = 1e-6,
    upper = 1,
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

#' Fit Winner's Curse Model
#' 
#' @param theta Parameter estimates.
#' @param se Parameter standard errors. 
#' @param pi Initial value of pi, the proportion of null parameters. 
#' @param tau2 Initial value of tau, the variance component.
#' @param eps Tolerance for Newton-Raphson iterations.
#' @param maxit Maximum number of NR iterations.
#' @param report Report fitting progress?
#' @importFrom methods new
#' @export
#' @return Object of class 'winCurse' containing:
#' \itemize{
#'   \item `@Assignments`: Maximum a posteriori component assignment 
#'     and assignment entropy.
#'   \item `@Estimates`: Estimated model parameters. 
#'   \item `@Expectations`: Posterior expected effect sizes. 
#'   \item `@Responsibilities`: Probabilities the parameter came from the null and 
#'     non-null components. 
#' }

fit.WinCurse <- function(
  theta,
  se,
  pi = NULL,
  tau2 = NULL,
  eps = 1e-6,
  maxit = 100,
  report = FALSE
) {
  
  # Initialization (could be improved).
  if(is.null(pi)) {
    pi = 0.5
  }
  if(is.null(tau2)) {
    tau2 = 1
  }
  
  # ---------------------------------------------------------------------------
  
  # EM Iterations.
  for(i in 1:maxit) {
    
    update <- EM.Update(
      theta = theta,
      se = se,
      pi = pi,
      tau2 = tau2
    )
    
    # Accept if increment is positive.
    if(update$delta > 0){
      
      pi <- update$pi
      tau2 <- update$tau2
      
      # Report increment.
      if(report) {
        cat("Objective increment: ", signif(update$delta, digits = 3), "\n")
      }
    
      # Terminate if increment is below tolerance.
      if(update$delta < eps){
        break
      }
      
    }
    
  }
  
  # ---------------------------------------------------------------------------
  
  # Final responsibilities.
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
  
  # ---------------------------------------------------------------------------
  
  # Assignments
  assignments <- data.frame(
    'non_null' = apply(response, 1, which.max) - 1,
    'entroy' = apply(response, 1, function (x) {
      -sum(x * log(x)) / log(2)
    })
  )
  
  # ---------------------------------------------------------------------------
  
  # Expectations
  expectations <- theta * (tau2) / (se^2 + tau2) * (1 - gamma)
  
  # ---------------------------------------------------------------------------
  
  # Prepare output.
  out <- new(
    Class = 'winCurse',
    Assignments = assignments,
    Estimates = list('pi' = pi, 'tau2' = tau2),
    Expectations = expectations,
    Responsibilities = response
  )
  return(out)
}