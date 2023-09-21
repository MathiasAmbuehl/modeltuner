
#' Simulate data
#' 
#' Simulates a `data.frame` with `n` rows and 12 columns:
#' \itemize{
#' \item{`X1, ..., X10`, random standard normal vectors.}
#' \item{`g`, a factor with `ngroup` levels, assigned at random.}
#' \item{`Y`, the response, generated as a additive combination of 
#' 1) a linear effect in `X1` with (randomly generated) intercepts and slopes varying between groups of `g`, 
#' 2) linear effects `X1`, `X2` and `X3`.
#' 3) non-linear effects in `X6` and `X7`.
#' 4) a non-linear three-way interaction of `X3`, `X4` and `X5` and 
#' 5) residuals from a normal or t distributions with `df` degrees of freedom (`df=Inf` is normal).}
#' }
#' See examples and code for details.
#' 
#' @param n Number of observations
#' @param ngroup Number of groups in `g`.
#' @param df Number of degrees of freedom of t distribution for simulation of residuals.
#' Default `Inf` generates normal residuals.
#' @param b Numeric vector of length 4 (shorter will be expanded to length 4): 
#' multiplicators of the 4 effects, 
#' @param weights `NULL` or vector of `n` non-negative values: 
#' If not `NULL`, simulated residuals will be divided by `1/sqrt(weights)`.
#' 
#' @return `simuldat()` returns a `data.frame`.
#' 
#' @examples
#' # Simluate data
#' d <- simuldat()
#' 
#' # Fit the "true" model
#' if (require(lme4)){
#'   form <- Y ~ (X1|g) + X2 + X3 + abs(X3+X4+X5) + I(exp(2*X6)/(1+exp(2*X6))) + dnorm(X7, sd = 0.5)
#'   (truemodel <- lmer(form, d))
#'   \donttest{confint(truemodel)}
#' }
#' 
#' # Data set with a purely random response
#' str(simuldat(b = 0))
#' 
#' @importFrom stats rnorm dnorm rt confint
#' @export
simuldat <- function(n = 500, ngroup = 20, df = Inf, 
                     b = 1, weights = NULL){
  mydata <- data.frame(replicate(10, rnorm(n)))
  mydata$g <- factor(paste0("g", sample(1:ngroup, n, TRUE)))
  A <- rnorm(nlevels(mydata$g))
  B <- 1 + rnorm(nlevels(mydata$g))
  b <- rep(b, length.out = 4)
  if (is.null(weights)) weights <- 1
  stopifnot(length(weights) %in% c(1, n))
  mydata$Y <- with(mydata, 
    b[1] * (A[g] + B[g]*X1) + 
    b[2] * (X2 + X3) + 
    b[3] * (abs(X3+X4+X5)) + 
    b[4] * (3*exp(2*X6)/(1+exp(2*X6)) + 3*dnorm(X7, sd = 0.5)) + 
    rt(n, df) * 1/sqrt(weights))
  mydata
}
