
#' Evaluate the model performance of a model
#' 
#' `performance()` \code{\link{fit}s} the model(s) in `x` and calculates a *performance table*, 
#' i.e. the same type of output as \code{\link{cv_performance}()}. 
#' The reported training error refers to the full model data, while test error is the error for 
#' the data provided in the `newdata` argument. 
#' Test error is `NA` if no `newdata` is specified.
#' 
#' @param x A model, or other object.
#' @param newdata Additional data for out-of-sample evaluation 
#' @param eval_weights,newweights Evaluation weights for training and test data, respectively.
#' @inheritParams cv_performance
#' 
#' @return
#' `performance()` returns a performance table. 
#' This is a \link{param_table} with additional class \dQuote{performance} 
#' having additional information in its attributes. 
#' 
#' @section Methods:
#' \itemize{
#' \item{`performance.model()` is the core method.}
#' \item{`performance.default(x, ...)` executes `x %>% model %>% performance(...)`, where `x` is a fitted model.}
#' \item{`performance.multimodel(x, ...)` runs `performance.model()` for the models included in `x`
#' and combines the results in performance table with `n_model(x)` rows. 
#' It uses \code{metric = \link{default_metric}(x)} by default.}
#' \item{`performance.cv()` extracts the multimodel from the \dQuote{cv} object and then applies `performance.multimodel()`, 
#' using the first cross-validated model's metric as default `metric`.}
#' }
#' 
#' @seealso \code{\link{cv_performance}}
#' 
#' @examples
#' even_ind <- seq(150)%%2 == 0
#' mylm <- lm(Sepal.Length ~., iris[even_ind, ])  # using half of the data to train
#' performance(mylm)
#' performance(mylm, newdata = iris[!even_ind, ]) # test set is complement of model data
#' 
#' @name performance
#' @export
performance <- function(x, ...) UseMethod("performance")
  

#' @rdname performance
#' @export
performance.model <- function(x, newdata = NULL, metric = NULL, 
                              eval_weights = x$weights, newweights = NULL, na.rm = FALSE, ...){
  actual <- response(x)
  fit <- fit(x)
  predicted <- predict(fit, x$data)
  
  has_newdata <- !is.null(newdata)
  if (has_newdata){
    xn <- x
    xn$data <- newdata
    actual_new <- response(xn)
    predicted_new <- predict(fit, newdata)
  }
  metric <- get_metric(metric, parent.frame(), x)
  
  if (na.rm){
    if (any(na <- is.na(predicted) | is.na(actual))){
      actual <- actual[!na]
      predicted <- predicted[!na]
      if (!is.null(eval_weights)) eval_weights <- eval_weights[!na]
    }
    if (has_newdata &&
        any(na_new <- is.na(predicted_new) | is.na(actual_new))){
      actual_new <- actual_new[!na_new]
      predicted_new <- predicted_new[!na_new]
      if (!is.null(newweights)) newweights <- newweights[!na]
    }
  }
  if (is.function(fn_metric <- metric[[1]])){
    out <- as.data.frame(list(
      fn_metric(actual, predicted, w = eval_weights, ...),
      if (has_newdata) 
        fn_metric(actual_new, predicted_new, w = newweights, ...)
      else NA_real_,
      NA_real_, NA_real_))
  } else {
    out <- data.frame(NA_real_, NA_real_, NA_real_, NA_real_)
  }
  names(out) <- paste(c("train", "test", "se_train", "se_test"), names(metric), sep = "_")
  rownames(out) <- label(x)
  out <- param_table(out)
  structure(out, 
            class = union("performance", class(out)), 
            metric = metric)
}

#' @rdname performance
#' @export
performance.model_fm_xgb <- function(x, newdata = NULL, metric = NULL, 
                                     eval_weights = x$weights, newweights = NULL, na.rm = FALSE, 
                                     ...){
  x <- add_fit(x, verbose = FALSE)
  metric <- get_metric(metric, parent.frame(), x)
  out <- NextMethod()
  if (is.na(out[[paste0("train_", names(metric))]])){
    # Handling case of an unknown metric (e.g. mphe):
    metric0 <- default_metric(x)
    default_metric <- names(metric0) == names(metric)
    default_weights <- (is.null(eval_weights) && is.null(x$weights)) || 
              (length(eval_weights) == length(x$weights) && all(eval_weights == x$weights))
    if (default_metric && default_weights){
      booster <- x$fit$booster
      traincol <- paste0("train_", names(metric))
      if (traincol %in% names(booster$evaluation_log))
        out[[1]] <- booster$evaluation_log[[traincol]][booster$niter]
    }
  }
  out$iteration <- x$fit$booster$niter
  out
}

#' @rdname performance
#' @export
performance.default <- function(x, newdata = NULL, metric = NULL, 
                                eval_weights = x$weights, newweights = NULL, na.rm = FALSE, 
                                ...){
  modelobj <- model(x, env = parent.frame(), eval_weights = eval_weights)
  metric <- get_metric(metric, parent.frame(), modelobj)
  performance(modelobj, newdata = newdata, metric = metric, eval_weights = eval_weights, 
              newweights = newweights, na.rm = na.rm, ...)
}


#' @rdname performance
#' @export
performance.multimodel <- function(x, newdata = NULL, metric = NULL, eval_weights = "default", 
                                   newweights = NULL, na.rm = FALSE, param = TRUE, 
                                   ...){
  metric <- get_metric(metric, parent.frame(), x)
  use_default_weights <- length(eval_weights) == 1 && eval_weights == "default"
  if (use_default_weights){
    wlist <- lapply(x$models, "[[", "weights")
    different_weights <- !all(duplicated(wlist)[-1])
    eval_weights <- if (different_weights) NULL else wlist[[1]]
    if (different_weights){
      message("multimodel includes models with different ", sQuote("weights"), ": ",
              "performance() uses constant ", sQuote("weights"), 
              fill = getOption("width"))
    }
  }
  out <- lapply(seq_len(n_model(x)), 
    function(i) performance(add_fit(extract_model(x, i)), newdata = newdata, metric = metric, 
                            eval_weights = eval_weights, newweights = newweights, na.rm = na.rm, ...))
  out <- do.call(rbind, c(out, list(force_list = FALSE)))
  if (param && !is.null(x$param)){
    out <- cbind(x$param, 
                 out[setdiff(names(out), names(x$param))])
  }
  out <- param_table(out)
  structure(out, class = union("performance", class(out)), metric = metric,
            weighted = !is.null(eval_weights))
}

#' @rdname performance
#' @export
performance.cv <- function(x, newdata = NULL, metric = x$metric[1], ...){
  mm <- extract_multimodel(x)
  performance(mm, newdata = newdata, metric = metric, ...)
}

