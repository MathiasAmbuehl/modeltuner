
#' Special methods of `model()`
#' 
#' @description 
#' These methods account for special features of the `predict` methods of some popular model types:
#' \code{\link[stats]{glm}} (from package **stats**), 
#' \code{\link[robustbase]{glmrob}} (from package \CRANpkg{robustbase}),
#' \code{\link[mgcv]{gam}} (from package \CRANpkg{mgcv}),
#' \code{\link[ranger]{ranger}} (from package \CRANpkg{ranger}),
#' \code{\link[lme4]{lmer}}, \code{\link[lme4]{glmer}} (from package \CRANpkg{lme4}).
#' 
#' They all execute \code{\link{model.default}()} with an adjusted default value of `predict_function`.
#' 
#' @param x A fitted model.
#' @param \dots Passed to `model.default()`.
#' @param predict_function As in \code{\link{model.default}}.
#' @inheritParams model
#' 
#' @return All these methods of `model()` return a \link{model}.
#' 
#' @examples
#' # Simulate data
#' d <- simuldat()
#' 
#' # ranger fitted model (random forest):
#' if (require(ranger)){
#'   ranger_fitted <- ranger(Y ~ ., d)
#'   # Methods predict.ranger() returns a list:
#'   str(predict(ranger_fitted, data = d))
#'   
#'   # Method model.ranger() makes sure that a "model" object returns
#'   # a vector, as required:
#'   ranger_model <- model(ranger_fitted)
#'   predict(ranger_model)
#' }
#' 
#' @name model-methods
NULL


#' @rdname model-methods
#' @export
model.glm <- function(x, ..., 
  predict_function = function(object, ...) predict(object, ..., type = "response"), 
  env = parent.frame()){
  model.default(x, ..., predict_function = predict_function, env = env)
}

#' @rdname model-methods
#' @export
model.glmrob <- model.glm

#' @rdname model-methods
#' @export
model.gam <- model.glm

#' @rdname model-methods
#' @export
model.ranger <- function(x, ..., 
  predict_function = function(object, ...) predict(object, ...)$predictions, 
  env = parent.frame()){
  model.default(x, ..., predict_function = predict_function, env = env)
}

#' @rdname model-methods
#' @export
model.merMod <- function(x, ..., 
  predict_function = function(object, ..., type = "response", allow.new.levels = TRUE) 
    predict(object, ..., allow.new.levels = allow.new.levels), 
  env = parent.frame()){
  model.default(x, ..., predict_function = predict_function, env = env)
}

#' @rdname model-methods
#' @export
model.lmerMod <- model.merMod

#' @rdname model-methods
#' @export
model.glmerMod <- model.merMod
