
#' Smoothing spline model with `formula`-interface
#' 
#' This is a simple wrapper for \code{\link{smooth.spline}()} from the \code{stats} package.
#' 
#' @param formula A `formula` with only one variable on right hand side.
#' @param data A `data.frame`.
#' @param weights Case `weights`.
#' @param na.action A function which indicates what should happen when the data contain `NA`s.
#' \code{\link{na.omit}} is the default, \code{\link{na.exclude}} or \code{\link{na.fail}}
#' could be useful alternative settings. 
#' @param \dots Passed to \code{\link{smooth.spline}()}.
#' 
#' @return 
#' `fm_smooth_spline()` returns a list of class \dQuote{fm_smooth_spline} with components
#' \itemize{
#' \item{*smooth.spline*: the fitted spline, of class \dQuote{\link{smooth.spline}};}
#' \item{*formula*: the formula;}
#' \item{*weights*: the fitting weights;}
#' \item{*na.action*: the \code{\link{na.action}} used during data preparation;}
#' \item{*call*: the matched call generating the model.}
#' } 
#' 
#' @seealso \code{\link[stats]{smooth.spline}} (package **stats**)
#' 
#' @examples
#' (smooth <- fm_smooth_spline(dist ~ speed, data = cars))
#' newd <- data.frame(speed = seq(min(cars$speed), max(cars$speed), length.out = 100))
#' plot(dist ~speed, cars, col = 4)
#' lines(newd$speed, predict(smooth, newd), col = 2)
#' 
#' @importFrom stats smooth.spline
#' @export
fm_smooth_spline <- function(formula, data, weights = NULL, na.action = na.omit, ...){
  if (missing(formula)) formula <- NULL
  if (missing(data)) data <- NULL
  w <- eval(substitute(weights), data, parent.frame())
  xy <- get_xy(formula, data, weights = w, na.action = na.action, remove_intercept = TRUE)
  if (NCOL(xy$x) != 1) stop("Invalid formula, expecting exactly one variable on rhs.")
  out <- list(smooth.spline = with(xy, smooth.spline(x, y, w = w, ...)), 
              formula = xy$terms,
              weights = xy$w,
              na.action = xy$na.action, 
              call = match.call())
  out$smooth.spline$data$nms <- rownames(xy$x)
  class(out) <- c("fm_smooth_spline", "list")
  out
}

#' @inheritParams print.model
#' @export
print.fm_smooth_spline <- function(x, abbreviate = TRUE, ...){
  cat("Fitted model of class", sQuote("fm_smooth_spline"), "\n")
  prnt <- x[c("formula", NA, "call")]
  prnt[[2]] <- deparse(x$call$data)[[1]]
  names(prnt)[[2]] <- "data"
  render_list(prnt, abbreviate = abbreviate)
  ss <- x$smooth.spline
  ss$call <- NULL
  lns <- capture.output(print(ss, ...))
  writeLines(paste0("  ", lns))
  invisible(x)
}

#' @describeIn fm_smooth_spline `predict` method for class \dQuote{fm_smooth_spline}.
#' @param object Object of class \dQuote{fm_smooth_spline}.
#' @param newdata `data.frame` with the data to be predicted.
#' If missing, predictions for the model data will be returned.
#' @importFrom stats na.exclude
#' @export
predict.fm_smooth_spline <- function(object, newdata, ...){
  if (missing(newdata) && !is.null(object$smooth.spline$data)){
    mmnew <- object$smooth.spline$data$x
  } else {
    xy <- get_xy(object$formula, data = newdata, na.action = na.exclude,
                 x_only = TRUE)
    mmnew <- xy$x
  }
  out <- as.vector(predict(object[[1]], mmnew)$y)
  if (missing(newdata)){
    names(out) <- object$smooth.spline$data$nms
  } else {
    names(out) <- rownames(mmnew)
  } 
  if (missing(newdata) && !is.null(na.act <- object$na.action)){
    out <- napredict(na.act, out)
  } else if (!is.null(na.act <- xy$na.action)){
    out <- napredict(na.act, out)
  }
  out
}

#' @export
terms.fm_smooth_spline <- function(x, ...) x$formula

#' @export
formula.fm_smooth_spline <- function(x, ...) formula(terms(x), ...)

