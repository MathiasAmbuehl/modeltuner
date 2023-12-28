
#' Evaluation log
#' 
#' @description
#' This function is related to the concept of *preferred iterations* in the context iteratively fitted models (IFM). 
#' See \link{ifm} and `vignette("ifm")` for more information on the peculiarities of this type of models.
#' An evaluation log is essentially the collection of the training and test error for all iterations of an IFM
#' (or a related \dQuote{cv} object).
#' 
#' The most appealing way of displaying an evaluation log is usually by plotting it, 
#' see \code{\link{plot.evaluation_log}}.
#' 
#' @param x Object of class \dQuote{cv}, \dQuote{model}, \dQuote{multimodel} or other.
#' @param \dots Arguments passed to methods. 
#' @inheritParams cv_performance
#' @param n Integer: Information will be printed for `n` models at most.
#' @param n_row Integer: Evaluation log tables are printed by selecting `n_row` rows, 
#' approximately equally distributed from first to last iteration. 
#' Iteration corresponding to the choice according to criteria (`"min"` etc.) are added.
#' @param digits Number of digits to be printed.
#' @param label Model label
#' @param .data Passing data - for internal use.
#' 
#' @details
#' The evaluation log of a \dQuote{model} (or *fitted model*) includes the training error only, 
#' while the evaluation log of a *cross-validated model* (object of class \dQuote{cv}) has both training and test errors.
#' 
#' If there are several models and no `metric` is specified, the \code{\link{default_metric}} from the first model is used. 
#' If `x` includes models having different `weights`, `evaluation_log()` will use `eval_weights=NULL`.
#' 
#' Running `evaluation_log()` with a non-default `metric` requires a \dQuote{cv} object 
#' that has been created with `keep_fits=TRUE`.  
#' 
#' Whenever there are `NA` predictions and `na.rm=TRUE`, the errors are calculated based on observations 
#' with non-NA predictions for *all* iterations, such that the same subset of observations is used for all iterations.
#' 
#' @return 
#' `evaluation_log()` returns an object of class \dQuote{evaluation_log}.
#' If `x` has no evaluation log, a (essentially empty) dummy object is returned.
#' 
#' @seealso \code{\link{ifm}}, \code{\link{plot.evaluation_log}}, \code{\link{fm_xgb}}, \code{\link{fm_glmnet}}
#' 
#' @examples
#' # Evaluation log of a 'fm_xgb' model
#' fitted_xgb <- fm_xgb(Sepal.Length ~ ., iris, max_depth = 2)
#' evaluation_log(fitted_xgb)    # evaluation log of a model has no 
#' plot(evaluation_log(fitted_xgb))
#' 
#' # Evaluation log of cross-validated 'fm_xgb' model
#' cv_xgb <- cv(model(fitted_xgb, label = "xgb_depth2"))
#' evaluation_log(cv_xgb) 
#' plot(evaluation_log(cv_xgb))
#' 
#' # Evaluation log of several cross-validated models
#' mydata <- simuldat()
#' fitted_glmnet <- fm_glmnet(Y ~ ., mydata)
#' cv_glmnet <- cv(multimodel(fitted_glmnet, prefix = "glmnet", alpha = 0:1))
#' label(cv_glmnet) <- c("ridge", "lasso")
#' evaluation_log(cv_glmnet)
#' plot(evaluation_log(cv_glmnet))
#' 
#' @name evaluation_log
NULL

#' @name evaluation_log
#' @export
evaluation_log <- function(x, ...) UseMethod("evaluation_log")

#' @export
evaluation_log.cv_simple <- function(x, metric = x$metric, eval_weights = x$model$weights, na.rm, ...){
  metric <- get_metric(metric, parent.frame(), x)
  metric_available <- is.function(metric[[1]])
  nm_metric <- names(metric)

  has_log <- !is.null(x$extras$evaluation_log)
  evlog <- NULL
  
  if (has_log){
    evlog <- x$extras$evaluation_log
    default_metric <- is_default_metric(metric, x)
    default_weights <- is_default_weights(eval_weights, x)
    std_settings <- default_weights && default_metric
    if (std_settings){
      if (!na.rm){
        evlog[evlog$NA_train, paste0(c("train_", "se_train_"), nm_metric)] <- NA
        evlog[evlog$NA_test, paste0(c("test_", "se_test_"), nm_metric)] <- NA
      }
    } else {
      evlog_calc_possible <- has_cv_fits(x) && is.function(metric[[1]])
      if (evlog_calc_possible){
        evlog <- evlog_int(classObj = structure(list(), class = x$model$class), 
                           x = x, metric = metric, eval_weights = eval_weights, ...)
      } else {
        if (!has_cv_fits(x)){
          warning("evaluation_log() with non-default metric and/or eval_weights requires a ", dQuote("cv"), 
                  " object including the model fits;\nsee argument ", sQuote("keep_fits"), 
                  " in ?cv", call. = FALSE)
        }
        sel_cols <- match(paste0(c("train_", "test_", "se_train_", "se_test_"), names(x$metric)), 
                          names(evlog))
        names(evlog)[sel_cols] <- paste0(c("train_", "test_", "se_train_", "se_test_"), nm_metric)
        evlog[sel_cols] <- NA
      }
    }
    evlog$NA_train <- evlog$NA_test <- NULL
    rownames(evlog) <- NULL
    pref_iter <- data.frame(
      iter  = sapply(x$extras$pref_iter, "[[", "iter"),
      error = NA, 
      row.names = as.vector(vapply(x$extras$pref_iter, "[[", "label_suffix", 
                                   FUN.VALUE = character(1))))
    pref_iter$error <- evlog[pref_iter$iter, paste0("test_", nm_metric)]
  }
  out <- list(label = label(x$model),
              has_log = has_log,
              class = x$model$class,
              log = evlog,
              metric = metric, 
              pref_iter = if (has_log) pref_iter
              )
  out
}


#' @rdname evaluation_log
#' @export
evaluation_log.cv <- function(x, metric = x$metric[1], eval_weights = extract_model(x, 1)$weights, na.rm = FALSE, ...){
  metric <- get_metric(metric, parent.frame(), x)
  use_default_weights <- length(eval_weights) == 1 && eval_weights == "default"
  # Determine common default weights
  if (use_default_weights){
    wlist <- lapply(x$multimodel$models, "[[", "weights")
    different_weights <- !all(duplicated(wlist)[-1])
    eval_weights <- if (different_weights) NULL else wlist[[1]]
    if (different_weights){
      message(dQuote("cv"), "-object includes models fitted with different ", sQuote("weights"), ": ",
              "cv_performance() uses constant ", sQuote("eval_weights"), 
              fill = getOption("width"))
    }
  }
  lbls <- label(x)
  # Get evaluation_log model-wise
  out <- lapply(lbls, 
    function(lab) evaluation_log(extract(x, lab), metric = metric, eval_weights = eval_weights, na.rm = na.rm, ...)[-1])
  names(out) <- lbls
  class(out) <- c("evaluation_log", "list") 
  attr(out, "cv") <- TRUE
  err_cols <- paste0(c("train_", "test_"), names(metric))
  log_is_na <- vapply(out, 
    function(x) (!all(err_cols %in% names(x$log))) || all(is.na(unlist(x$log[err_cols]))), 
    FUN.VALUE = logical(1))
  if (any(log_is_na) && !is.function(metric[[1]])){
    warning("Metric not available: ", names(metric), call. = FALSE)
  }
  
  out
}

#' @rdname evaluation_log
#' @export
print.evaluation_log <- function(x, se = getOption("cv_show_se"), 
                                 n = getOption("print_max_model"), 
                                 n_row = getOption("print_rows_evaluation_log"),
                                 digits = 3, ...){
  cat(sQuote("evaluation_log"), ", ", length(x), 
      if (isTRUE(attr(x, "cv"))) " cross-validated",
      " model", if (length(x)>1) "s", 
      ":\n", sep = "")
  for (i in seq_len(min(length(x), n))){
    xi <- x[[i]]
    cat("\nModel ", sQuote(names(x)[[i]]), ":\n", 
        if (length(xi$class)) paste0("  model class: ", xi$class[[1]], "\n"), 
        sep = "")
    if (xi$has_log){
      print(prnt_evlog(xi$log, xi$pref_iter, n_row = n_row, se = se), digits = digits, row.names = FALSE)
    } else {
      cat("  No evaluation log.\n")
    }
  }
  if (se && any(vapply(head(x, n), "[[", "has_log", FUN.VALUE = logical(1)))){
    message("The reported standard errors may be inaccurate.")
  }
  if (length(x) > n){
    n_rest <- length(x) - n
    cat("\nand ", n_rest, " more model", if (n_rest>1) "s", 
        " labelled:\n", sep = "")
    do.call(cat, 
      c(as.list(paste0(sapply(names(x)[-(1:n)], sQuote), 
                       rep(c(", ", ""), c(n_rest-1, 1)))), 
        sep = "", labels = " ", 
        fill = min(max(getOption("width") - 15, 20), 130)))
    cat("\n")
  }
  invisible(x)
}

# aux fn used in print method:
prnt_evlog <- function(d, pref_iter, n_row = 6, se = FALSE){
  b <- pref_iter[["iter"]]
  names(b) <- rownames(pref_iter)
  if (length(b)){
    if (length(b)>1) names(b)[[1]] <- paste0(names(b)[[1]], "*")
    b <- b[!is.na(b)]
  }
  if (any(duplicated(b))){
    nms_b <- vapply(split(names(b), b), paste0, collapse = ",", 
                    FUN.VALUE = character(1))
    b <- setNames(as.integer(names(nms_b)), nms_b)
  }
  if (!se){
    se_cols <- grepl("^se_[train|test]", names(d))
    d[se_cols] <- NULL
  }
  n_row <- min(n_row, nrow(d))
  pr <- round(seq(1, nrow(d), length.out = n_row))
  pr <- sort(union(pr, b))
  out <- d[pr, , drop = F]
  if (length(b)){
    out$criterion <- ""
    out$criterion[match(b, pr, 0)] <- format(names(b), justify = "left")
  }
  out
}


#' @export
label.evaluation_log <- function(x, which) names(x)[which]

#' @export
subset.evaluation_log <- function(x, subset = TRUE, ...){
  structure(unclass(x)[subset], class = class(x))
}

# ------------------------------------------------------------------------------

# evlog_int: 
# * takes a dummy arg 'classObj' having the respective model class, a cv_simple object x 
#   with a model of that class plus args metric, eval_weight, ... 
# * "pseudo-generic" family of function dispatching on class of model in cv_simple obj x
# * all args except x passed to cv_perf_basic()
#' @details
#' \code{evlog_int()} is a generic function performing internal calculations 
#' in \code{\link{evaluation_log}()}.
#' @rdname modeltuner-internal
#' @export
evlog_int <- function(classObj, x, metric, eval_weights, na.rm, ...){
  UseMethod("evlog_int")
}

evlog_int.fm_glmnet <- function(classObj, x, metric = x$metric, eval_weights = x$model$weights, na.rm = NULL, ...){
  nfold <- length(x$folds)
  xy <- get_xy(x$model)
  folds <- adjust_folds(x$folds, i_remove = xy$na.action)
  lambda <- x$fits[[1]]$lambda

  # Shorter alternative (but much slower):
  predfun <- function(object, xmat, ...){
    predict(object, xmat, s = lambda)
  }
  eval_log <- get_evaluation_log_joint(
    fits = x$fits, folds = folds, predict_function = predfun, 
    y = xy$y, x = xy$x, w = xy$w,
    metric = metric, na.rm = na.rm, ...)
  data.frame(iter = seq_len(nrow(eval_log)), lambda, eval_log[-match("iter", names(eval_log))])
}

evlog_int.fm_xgb <- function(classObj, x, metric = x$metric, eval_weights = x$model$weights, na.rm = NULL, ...){
  nfold <- length(x$folds)
  xy <- get_xy(x$model, na.action = na.pass)
  n_iter <- nrow(x$extras$evaluation_log)
  predfun <- function(object, newdata, iter, ...){
    predict(object, newdata, iterationrange = c(1, iter+1), ...)
  }
  get_evaluation_log(
    fits = x$fits, folds = x$folds, niter = n_iter, 
    predict_function = predfun, y = xy$y, x = xy$x, w = xy$w, metric = metric, 
    na.rm = na.rm)
}

# ==============================================================================

# general-purpose aux fn
# folds=NULL for eval log of a model, not from crossval (only train)
# predict_function must have an argument iter for the iteration
get_evaluation_log <- 
  function(fits, niter, folds, predict_function,
           y, x, w, metric, na.rm, ...){ 
  n <- length(y)
  if (is.null(folds)) folds <- structure(list(integer(0)), n = n)
  nfold <- length(folds)
  pred_i <- array(NA_real_, dim = c(n, nfold))
  eval_log <- vector("list", niter)
  prep <- prep_cv_perf(actual = y, w = w, folds = folds)
  for (i in seq_len(niter)){
    for (fo in seq_along(folds)){
      pred_i[, fo] <- predict_function(fits[[fo]], x, iter = i)
    }
    eval_log[[i]] <- 
      cv_perf_basic(predicted = pred_i, metric = metric, na.rm = na.rm, ..., 
                    prep = prep)
  }
  out <- data.frame(iter = seq_len(niter), 
                    do.call(rbind, eval_log))
  rownames(out) <- NULL
  out
}

# predictions for all iterations are obtained in one call
get_evaluation_log_joint <- 
  function(fits, folds, predict_function,
           y, x, w, metric, na.rm, 
           ...){  
  n <- length(y)
  if (is.null(folds)) folds <- structure(list(integer(0)), n = n)
  nfold <- length(folds)
  predictions <- vector("list", nfold)
  for (i in seq_len(nfold)) {
    predictions[[i]] <- predict_function(fits[[i]], x)
  }
  niter <- ncol(predictions[[1]])
  predictions <- do.call(c, predictions)
  dim(predictions) <- c(n, niter, nfold) 
  prep <- prep_cv_perf(actual = y, w = w, folds = folds)
  eval_log <- lapply(seq_len(dim(predictions)[2]), function(i) {
    outi <- cv_perf_basic(prep = prep, 
                          predicted = array(predictions[, i, ], c(n, nfold)), 
                          metric = metric, na.rm = na.rm, 
                          ...)
    outi
  })
  out <- data.frame(iter = seq_along(eval_log),
                    do.call(rbind, eval_log))  
  rownames(out) <- NULL
  out
}

# ==============================================================================


# ADDITIONAL METHODS FOR (not cross-validated) MODELS:

# method for class "fm_xgb"
#' @name evaluation_log
#' @export
evaluation_log.fm_xgb <- function(x, label = deparse(substitute(x))[[1]], metric = NULL, 
                                  eval_weights = weights(x), ..., 
                                  .data = eval.parent(x$call$data)){
  w <- weights(x)
  metric <- get_metric(metric, parent.frame(), x)
  default_weights <- (is.null(eval_weights) && is.null(w)) ||
    (length(eval_weights) == length(w) && all(eval_weights == w))
  if (is.null(metric)) metric <- xgb_metric(x$booster)
  nm_metric <- names(metric)
  default_metric <- isTRUE(nm_metric == names(xgb_metric(x$booster)))
  if (default_weights && default_metric){
    out <- evaluation_log(extract_booster(x), label = label, ...)
    out[[1]]$class <- class(x)
    nms_log <- names(out[[1]]$log)
    for (prefix in c("test_", "se_train_", "se_test_")){
      nm <- paste0(prefix, nm_metric)
      if (!(nm %in% nms_log)) out[[1]]$log[[nm]] <- NA
    }
    return(out)
  }
  cl_xy <- call("get_xy", x$formula, .data, weights = w, remove_intercept = TRUE)
  if (!is.null(na.act <- x$na.action)) cl_xy$na.action <- na.act
  if (!is.null(contr <- x$contrasts)) cl_xy$contrasts <- contr
  xy <- eval(cl_xy)
  predfun <- function(object = x$booster, newdata, iter, ...){
    predict(object, newdata, 
            iterationrange = c(1, iter+1), ...)
  }
  evlog <- get_evaluation_log(
    fits = list(x$booster), folds = NULL, niter = x$booster$niter, 
    predict_function = predfun, y = xy$y, x = xy$x, w = w, metric = metric, 
    na.rm = FALSE, ...)
  out1 <- list(
    has_log = TRUE, 
    class = class(x)[[1]],
    log = evlog,
    metric = metric)
  out <- list(out1)
  names(out) <- label
  class(out) <- c("evaluation_log", "list")
  out
}


# method for class "fm_glmnet"
#' @name evaluation_log
#' @export
evaluation_log.fm_glmnet <- function (x, label = deparse(substitute(x))[[1]], metric = NULL,
                                      eval_weights = weights(x), na.rm = FALSE, ..., 
                                      .data = eval.parent(x$call$data)) {
  
  metric <- get_metric(metric, parent.frame(), x)
  cl_xy <- call("get_xy", x$formula, .data, weights = weights(x), remove_intercept = TRUE)
  cl_xy$na.action <- x$call$na.action
  cl_xy$contrasts <- x$contrasts
  if (!na.rm) cl_xy$na.action <- na.pass
  xy <- eval(cl_xy)
  xmat <- xy$x
  lambda <- x$fit$lambda
  predfun <- function(object, xmat, ...){
    predict(object, xmat, s = lambda, ...)
  }
  evlog <- get_evaluation_log_joint(
    fits = list(x$fit), folds = NULL, predict_function = predfun, 
    y = xy$y, x = xmat, w = xy$w, metric = metric, na.rm = na.rm, ...)
  evlog <- data.frame(evlog["iter"], lambda, evlog[-match("iter", names(evlog))])
  out1 <- list(
    has_log = TRUE, 
    class = class(x)[[1]],
    log = evlog,
    metric = metric)
  out <- list(out1)
  names(out) <- label
  class(out) <- c("evaluation_log", "list")
  out
}
  
#' @name evaluation_log
#' @importFrom utils methods
#' @export
evaluation_log.model <- function(x, metric = NULL, eval_weights = weights(x), ...){
  metric <- get_metric(metric, parent.frame(), x)
  ok_classes <- sub("^evaluation_log\\.", "", as.character(methods(evaluation_log)))
  if (any(x$class %in% ok_classes)){
    evaluation_log(fit(x, iter = NULL), label = label(x), metric = metric, 
                   eval_weights = eval_weights, ..., .data = x$data)
  } else {
    out <- list(label = label(x),
                class = x$class,
                has_log = FALSE,
                log = NULL,
                metric = metric, 
                pref_iter = data.frame())
  structure(list(out),
            .Names = out$label,
            class = c("evaluation_log", "list"))
  }
}

#' @rdname evaluation_log
#' @export
evaluation_log.multimodel <- function(x, metric = NULL, eval_weights = extract_model(x, 1)$weights, ...){
  metric <- get_metric(metric, parent.frame(), x)
  lbls <- label(x)
  out <- lapply(lbls, 
    function(lab) evaluation_log(extract_model(x, lab), metric = metric, eval_weights = eval_weights, ...)[[1]])
  names(out) <- lbls
  class(out) <- c("evaluation_log", "list")  
  out
}

# method for class "xgb.Booster" (simple extraction)
#' @name evaluation_log
#' @export
evaluation_log.xgb.Booster <- function(x, label = deparse(substitute(x))[[1]], ...){
  evlog <- as.data.frame(x$evaluation_log)
  metric <- xgb_metric(x)
  out <- list(
    has_log = TRUE, 
    class = class(x),
    log = evlog,
    metric = metric 
  )
  out <- list(out)
  names(out) <- label
  class(out) <- c("evaluation_log", "list")
  out
}

