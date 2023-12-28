
# methods(class = "model")
#  [1] add_cv_info    add_fit        best_subset    c              cv             cv_performance cv_predict     cv_resid       cv_simple     
# [10] default_metric evaluation_log fit            get_xy         has_fit        label          label<-        model          multimodel    
# [19] performance    plot           predict        print          prnt_cv_info   residuals      response       set_pref_iter  sort_models   
# [28] step_backward  step_extend    step_forward   step_reduce    terms          tune           update         weights       

#' Purely predictive (non-fittable) model
#' 
#' A `pmodel` is a \code{\link{model}} with reduced capacities.
#' It can be used to make predictions, but it cannot be updated or re-\link{fit}ted, 
#' and therefore not cross-validated.
#' 
#' @param x A fitted model.
#' @param data A `data.frame`.
#' @param response A character string, the name of the response.
#' @param label Character string: A label to be attributed to the output.
#' @param predict_function Predict function, often \code{\link{predict}} 
#' (see \dQuote{Details} section in \code{?\link{model}}).
#' @param class Class of the model `x`.
  #' @param weights Evaluation weights.
#' 
#' @details
#' The model object `x` does not have to fulfill the formal requirements of a \link{model}
#' (model formula, call component, etc.).
#' The only requirement is that for any data set of the same shape as `data`,
#' predictions can be obtained with the `predict_function`.
#' 
#' For tasks including only predicting, not re-fitting, the methods for class `model`
#' also work for a `pmodel`. 
#' In particular, the following methods remain applicable:
#' \code{\link{predict.model}},
#' \code{\link{residuals.model}},
#' \code{\link{response.model}},
#' \code{\link{performance.model}},
#' \code{\link{plot.model}},
#' \code{\link{label.model}},
#' \code{\link{weights.model}}.
#' Among the generics with no appropriate method for a `pmodel` are
#' \code{\link{fit}} (with arguments inducing a modification of the model),
#' \code{\link{cv}} and all functions \code{cv_*} 
#' (\code{\link{cv_performance}}, \code{\link{cv_predict}}, etc.).
#' 
#' @return
#' An object of class \dQuote{pmodel} (also inheriting of class \dQuote{model}),
#' a simplified version of a `model`, including the components
#' *label*, *data*, *response_type*, *response*, *fit*,             
#' *predict_function*, *class* and *weights*.
#' Unlike a `model`, it does not have components *formula* and *call*
#' 
#' @examples
#' mylm <- lm(Sepal.Length ~ ., iris)
#' mypm <- pmodel(mylm, iris, "Sepal.Length")
#' mypm
#' predict(mypm)
#' residuals(mypm)
#' response(mypm)
#' performance(mypm)       
#'  
#' @export
pmodel <- function(x, data, response, label = "pmodel", class = attr(x, "class")[[1]], 
                   weights = NULL, predict_function = predict){
  data <- as.data.frame(data)
  stopifnot(response %in% names(data))
  out <- list(label = label, data = data, response_type = NULL, 
              response = response, fit = x, predict_function = predict_function, 
              class = class, weights = weights)
  response_values <- data[[response]]
  response_values <- response_values[!is.na(response_values)]
  out$response_type <- if (all(match(response_values, 0:1, 0) > 0)){
    "binary"
  } else {
    "continuous"
  }
  class(out) <- c("pmodel", "model")
  out
}

#' @export
print.pmodel <- function(x, what = c("label", "class", "data", "response_type", 
                                     "weights", "fit"), 
                         abbreviate = TRUE, width = getOption("width"), 
                         indent = "", ...){
  cat(indent, "--- A ", dQuote("pmodel"), " object ---\n", sep = "") 
  prnt_model_lines(x, "  ", what = what, abbreviate = abbreviate, width = width)
  prnt_cv_info(x, ...)
  invisible(x)
}

#' @export
update.pmodel <- function(object, ..., label = label.model(object), add_fit = NA, ignore_changes_in_arg = NULL){
  if (length(list(...))){
    stop("Model ", dQuote(label(object)), " of class ", dQuote("pmodel"), " can not be updated.")
  }
  label(object) <- label
  object
}  

#' @export
fit.pmodel <- function(x, ...){
  if (nargs()>1) stop("Model ", dQuote(label(x)), " of class ", dQuote("pmodel"), " can not be re-fitted.")
  x$fit
}

#' @export
cv.pmodel <- function(x, ...){
  stop("Model ", dQuote(label(x)), " of class ", dQuote("pmodel"), " can not be cross-validated.")
}

#' @export
cv_simple.pmodel <- function(x, ...){
  stop("Model ", dQuote(label(x)), " of class ", dQuote("pmodel"), " can not be cross-validated.")
}

# ------------------------------------------------------------------------------

if (F){
  # More detailed examples:
  mylm <- lm(Sepal.Length ~ ., iris)
  
  mypm <- pmodel(mylm, iris, "Sepal.Length")
  mypm

  # methods below should work:
  predict(mypm)
  residuals(mypm)
  response(mypm)
  performance(mypm)       
  default_metric(mypm)
  evaluation_log(mypm)
  has_fit(mypm)
  add_fit(mypm)
  label(mypm)
  weights(mypm)
  plot(mypm)
  multimodel(mypm)
  update(mypm, label = "pred_model") # no modification of model!
  fit(mypm)                          # no modification of model!
  # cases below don't work 
  update(mypm, . ~ . - Species)
  fit(mypm, data = quote(iris))
  cv(mypm)
  # Combine model and pmodel in a multimodel:
  c(mypm, model(mylm))
  cv(c(mypm, model(mylm)))
  performance(c(mypm, model(mylm)), 
              newdata = car::some(iris, 50))
  plot(c(mypm, model(mylm)))
}  
