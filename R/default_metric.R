
#' Get the default metric of an object
#' 
#' @description
#' This generic function returns the default metric of a model. 
#' The default metric is the metric that is used when evaluating a model with 
#' \code{\link{cv_performance}()}, \code{\link[modeltuner]{performance}()} or 
#' \code{\link{evaluation_log}()} and no `metric` argument is specified.
#' 
#' The default metric of a *model* is usually \code{\link[modeltuner]{rmse}} if the response is continuous and 
#' \code{\link[modeltuner]{logLoss}} if it is binary.
#' The only exception to this rule are models based on \code{\link{fm_xgb}()}, 
#' where the `eval_metric` used in \code{\link[xgboost]{xgb.train}()} is chosen as default.
#' 
#' If `x` is a \link{multimodel} or \dQuote{cv} object with several models, 
#' then the first model's default metric is returned.
#' 
#' @param x A \link{model}, \link{multimodel}, fitted model or an object of class \dQuote{\link{cv}}.
#' 
#' @return
#' A named list of length one containing the metric function, if available.
#' 
#' @seealso \code{\link{metrics}}, \code{\link{fm_xgb}}
#' 
#' @examples
#' n <- 100
#' d <- data.frame(x = rnorm(n), y = rnorm(n))
#' lm(y~x, d) |> default_metric()
#' glm(I(y>0)~x, d, family = binomial) |> default_metric()
#' fm_xgb(y~x, d, nrounds = 10) |> default_metric()
#' fm_xgb(y~x, d, nrounds = 10, objective = "reg:absoluteerror") |> default_metric()
#' fm_xgb(y~x, d, nrounds = 10, objective = "reg:pseudohubererror") |> 
#' default_metric()  # -> there is no function with this name
#' 
#' @export
default_metric <- function(x) UseMethod("default_metric")

#' @export
default_metric.model <- function(x){
  metric <- switch(x$response_type, binary = "logLoss", "rmse")
  get_metric(metric)
}
#' @export
default_metric.default <- function(x){
  default_metric(model(x))
} 
#' @export
default_metric.xgb.Booster <- function(x){
  xgb_metric(x)
} 
#' @export
default_metric.xgb.cv.synchronous <- function(x){
  xgb_metric(x)
} 
#' @export
default_metric.multimodel <- function(x){
  default_metric(extract_model(x, 1))
}
#' @export
default_metric.fm_xgb <- function(x){
  xgb_metric(extract_booster(x))
}
#' @export
default_metric.model_fm_xgb <- function(x){
  x <- update(x, nrounds = 1)
  x <- fit(x, verbose = FALSE)
  xgb_metric(extract_booster(x))
}
#' @export
default_metric.cv <- function(x){
  x$metric[1]
}

