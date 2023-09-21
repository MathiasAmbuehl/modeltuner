
#' Change the default metric of a `cv` object 
#' 
#' @description
#' `set_metric` is applied to an object `x` of class \dQuote{cv} including one or several 
#' cross-validated models. It changes the default metric of all models in `x`.
#' 
#' @param x An object of class \dQuote{cv}.
#' @param metric A metric (see \link{metrics}).
#' @param \dots Passed to metric function.
#' @inheritParams cv_performance
#' 
#' @return
#' A (modified) object of class \code{cv}, with adapted metric. 
#' 
#' @details
#' Changing the metric is most useful when applied to models based on an iteratively fitted model (see \link{ifm}).
#' In this case, the evaluation log is recalculated for the new metric and
#' the preference criteria are evaluated based on this updated log.
#' These computations are only possible when the input \dQuote{cv} object contains the
#' cross-validation model fits, which requires that it has been generated in a
#' call \code{\link{cv}(..., keep_fits = TRUE)}.
#' 
#' @examples
#' m <- model(fm_xgb(Sepal.Length ~., iris), max_depth = 2, 
#'            label = "xgb_model")
#' cvm <- cv(m, keep_fits = TRUE)  # keep_fits=TRUE is required here
#' cv_performance(cvm)
#' # change the metric
#' cvm_medae <- set_metric(cvm, "medae")
#' cv_performance(cvm_medae)
#' 
#' @name set_metric
NULL

#' @rdname set_metric
#' @export
set_metric <- function(x, ...) UseMethod("set_metric")

#' Method for class cv
#' @rdname set_metric
#' @export
set_metric.cv <- function(x, metric = x$metric[1], eval_weights = "default", ...){  
  metric <- get_metric(metric, parent.frame(), x)
  outlist <- lapply(1:n_model(x), extract, x = x)
  use_default_weights <- length(eval_weights) == 1 && eval_weights == "default"
  for (i in seq_len(n_model(x))){
    if (use_default_weights){
      outlist[[i]] <- set_metric(outlist[[i]], metric = metric, eval_weights = eval_weights, ...)
    } else {
      outlist[[i]] <- set_metric(outlist[[i]], metric = metric, eval_weights = weights(x$model), ...)
    }
  }
  out <- cvlist2cv(outlist, multimodel = x$multimodel, tm = x$timing, keep_fits = TRUE)
  out
}

# Method for class cv_simple
# @rdname set_metric
#' @export
set_metric.cv_simple <- function(x, metric, eval_weights, ...){
  default_metric <- is_default_metric(metric, x)
  default_weights <- is_default_weights(eval_weights, x)
  if (default_metric && default_weights) return(x)

  # evaluation log and best iters
  eval_log <- evaluation_log(x, metric = metric, weights = eval_weights, ...)
  if (has_eval_log <- eval_log$has_log){
    eval_log <- with(eval_log, list(eval_log = log, metric = metric, label = label))
    pref_iter <- lapply(x$extras$pref_iter, 
                         function(cr) eval_crit(cr$crit, eval_log, metric = metric))
    pref_iter <- do.call(c, pref_iter)
    iters <- vapply(pref_iter, "[[", "iter", FUN.VALUE = integer(1))
  }
  metric_nm <- names(metric)
  tt <- c("train_", "test_")
  perf_nms <- c(paste0(tt, metric_nm), paste0("se_", tt, metric_nm))
  
  # cv_performance
  if (has_eval_log){
    perform <- vector("list", length(iters))
    for (j in seq_along(iters)){
      perform[[j]] <- eval_log$eval_log[iters[[j]], , drop = FALSE]
      perform[[j]]$iteration <- perform[[j]]$iter
      rownames(perform[[j]]) <- NULL
      perform[[j]] <- perform[[j]][c(perf_nms, "iteration")]
    }
  } else {
    perform <- x$performance
  }
  
  # cv predictions
  if (has_eval_log){
    xy <- get_xy(x$model, na.action = na.pass) # remove_interecpt, na.action, contrasts ???
    if (!has_cv_fits(x)){
      x$fits <- vector("list", length(x$folds))
      preds <- rep(list(matrix(NA_real_, length(xy$y), length(x$folds))), length(iters))
    } else if (x$model$class == "fm_glmnet"){
      preds <- get_predictions_glmnet(x$fits, model = x$model, env = xy, 
                                      lambda = x$extras$lambda[iters])
      preds <- structure(asplit(preds, 2), dim = NULL)
    } else if (x$model$class == "fm_xgb"){
      preds <- vector("list", length(iters))
      for (j in seq_along(iters)){
        it <- iters[[j]]
        if (is.na(it)){
          preds[[j]] <- matrix(NA_real_, nrow = nrow(xy$x), ncol = length(x$folds))
        } else {
          preds[[j]] <- vapply(x$fit, predict, newdata = xy$x, 
                               iterationrange = c(1, perform[[j]]$it+1), 
                               FUN.VALUE = numeric(length(response(x$model))))
        }
      }
    } else stop("Invalid model class: ", x$model$class)
  } else preds <- x$predictions
  
  # Prepare output
  out <- x
  out$metric <- metric
  out$performance <- perform
  out$predictions <- preds
  metric_nm <- names(metric)
  # eval_log_tbl <- eval_log$eval_log[c("iter", perf_nms)]
  if (has_eval_log)
    out$extras <- list(evaluation_log = eval_log$eval_log, pref_iter = pref_iter)
  
  # Return
  out
}
