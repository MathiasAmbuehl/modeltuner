
# Functions related to cross validation of "tm_xgb" models

#' @importFrom xgboost xgb.cv
#' @export
cv_simple.model_fm_xgb <- function(x, folds, metric = NULL, iter = getOption("cv_iter"),
                                   keep_fits = FALSE, verbose = TRUE, ...){
  if (!isFALSE(verbose)){
    verbose_prefix <- if (is.character(verbose)) paste0(verbose, " ") else ""
    verbose <- TRUE
    blank_line <- paste0(rep(" ", nchar(verbose_prefix) + nchar(x$label) + 60), 
                         collapse = "")
    on.exit(message("\r", blank_line, "\r", appendLF = FALSE))
  } else verbose_prefix <- verbose
  nfold <- length(folds)
  na.action <- eval(x$call$na.action, x$saved_objects)
  if (is.null(na.action)) na.action <- evalq(formals(fm_xgb)$na.action)
  xy <- get_xy(x, na.action = na.action)
  if (NCOL(xy$x) < 1){
    if (verbose){
      message("\r", blank_line, "\rcv model ", dQuote(label(x)), ": ",
              "Invalid formula, expecting at least one variable on rhs.")
    }
    out <- structure(list(model = x, 
                          folds = folds,
                          metric = default_metric(x),
                          predictions = array(NA, dim = c(nrow(x$data), nfold))
                          ), 
                     class = c("cv_simple", "list"))
    warning("Cross-validation results may be invalid or incomplete for model ", 
            dQuote(label(x)), call. = FALSE)
    return(out)
  }
  d <- xgbDMat(xy)
  weights <- getinfo(d, "weight")
  cv_call <- fit(x, iter = NULL, eval = FALSE)
  
  folds1 <- adjust_folds(folds, i_remove = xy$na.action)
  nfold <- length(folds1)
  
  if (nfold>1){
    # Use xgb.cv()
    cv_call[[1]] <- quote(xgb.cv)
    train_folds <- lapply(folds1, function(fo) setdiff(seq_len(nrow(d)), fo))
    modif <- list(
      formula = NULL, data = quote(d), nfold = nfold, folds = quote(folds1), 
      train_folds = quote(train_folds), prediction = TRUE, 
      na.action = NULL, keep_x = NULL)
    cb <- list(xgboost::cb.cv.predict(save_models = TRUE))
    cb_prefix <- paste0(verbose_prefix, dQuote(x$label), ": building boosters: ")
  } else {
    # Use xgb.train()
    cv_call[[1]] <- quote(xgb.train)
    ii_test <- folds1[[1]]
    ii_train <- setdiff(seq_len(nrow(d)), ii_test)
    modif <- list(formula = NULL, data = quote(d[ii_train]), 
                  watchlist = quote(list(train = d[ii_train], test = d[ii_test])), 
                  criterion = NULL)
    cb <- list()
    cb_prefix <- paste0(verbose_prefix, dQuote(x$label), ": building booster: ")
  }
  cv_call <- modify_call(cv_call, modif)
  if (is.null(cv_call$nrounds)) cv_call$nrounds <- formals(fm_xgb)$nrounds
  nrounds_fixed <- "early_stopping_rounds" %in% names(cv_call) && is.null(cv_call$early_stopping_rounds)
  if (!nrounds_fixed && is.null(cv_call$early_stopping_rounds)) 
    cv_call$early_stopping_rounds <- formals(fm_xgb)$early_stopping_rounds
  if (!is.null(cv_call$interaction_constraints)){
    cv_call$interaction_constraints <- 
      prep_interaction_constraints(eval(cv_call$interaction_constraints), xy)
  }
  if (verbose){
    cb <- c(cb, list(cb_xgb_iteration(prefix = cb_prefix)))
  }
  cv_call$callbacks <- cb
  cv_call$verbose <- FALSE
  # evaluate call
  xgb_cv_result <- try(eval(cv_call))
  
  # If xgb_cv/xgb.train failed:
  if (inherits(xgb_cv_result, "try-error")){
    out <- structure(list(model = x, 
                          folds = folds,
                          metric = default_metric(x),
                          predictions = array(NA, dim = c(nrow(x$data), nfold))
                          ), 
                     class = c("cv_simple", "list"))
    return(out)
  }
  
  # metric
  metric0 <- xgb_metric(xgb_cv_result)
  if (is.null(metric)){
    metric <- metric0
  }
  nm_metric <- names(metric)
  default_metric <- identical(names(metric0), nm_metric)

  # evaluation_log and  saved iterations
  if (verbose){
    message(blank_line, "\r", verbose_prefix, dQuote(x$label), ": Computing evaluation log...", 
            appendLF = FALSE)
  }
  if (default_metric && length(xy$na.action) == 0){
    # read evaluation_log from xgb output
    eval_log <- .xgb_eval_log(xgb_cv_result, folds, nm_metric)
  } else {
    if (length(xy$na.action)){
      xy <- get_xy(x, na.action = na.pass)  # predictions for all obs
      d <- xgbDMat(xy)
    }
    # obtain evaluation_log from predictions
    predfun <- function(object, newdata, iter, ...){
      predict(object, newdata, iterationrange = c(1, iter+1), ...)
    }
    eval_log <- get_evaluation_log(
      fits = xgb_cv_result$models, folds = folds, niter = xgb_cv_result$niter, 
      predict_function = predfun, y = xy$y, x = xy$x, w = xy$w, metric = metric, na.rm = NULL)
  }
  eval_log$NA_train <- eval_log$NA_test <- FALSE  # predict.fm_xgb() always returns predictions
  if (verbose){  
    message(blank_line, "\r", verbose_prefix, dQuote(x$label), ": Completing cv\r", 
            appendLF = FALSE)
  }
  pref_iter <- eval_crit(iter, list(eval_log = eval_log, metric = metric, label = x$label))

  # extras: evaluation_log, saved iteration(s)
  extras <- list(
    evaluation_log = eval_log,
    pref_iter = pref_iter)

  # Predictions & performance
  preds <- vector("list", length(pref_iter))
  names(preds) <- names(pref_iter)
  perform <- preds
  for (i in seq_along(pref_iter)){
    it <- pref_iter[[i]]$iter
    perform[[i]] <- extras$evaluation_log[it, , drop = FALSE]
    perform[[i]]$iteration <- perform[[i]]$iter
    perform[[i]]$iter <- rownames(perform[[i]]) <- NULL
    # cv predictions
    if (!is.finite(it)){
      preds[[i]] <- NA
      next
    }  
    if (nfold>1){
      preds[[i]] <- vapply(xgb_cv_result$models, predict, newdata = d, 
                           iterationrange = c(1, perform[[i]]$iteration+1), 
                           FUN.VALUE = numeric(length(response(x))))
    } else {
      preds[[i]] <- matrix(
        predict(xgb_cv_result, newdata = d, 
                iterationrange = c(1, perform[[i]]$iteration+1)), 
        ncol = 1)
    }
  }

  # Output
  out <- structure(list(model = x, 
                        folds = folds,
                        metric = metric,
                        performance = perform, 
                        predictions = preds
                        ), 
                   class = c("cv_simple", "list"))
  # append fits if required
  if (keep_fits){
    out$fits <- if (nfold>1) xgb_cv_result$models else xgb_cv_result
  }
  out$extras <- extras
  out
}

# ------------------------------------------------------------------------------

# Aux functions:

# Find default metric
xgb_metric <- function(obj, pattern = 
    switch(class(obj)[[1]], xgb.Booster = "train_(.+)$", xgb.cv.synchronous = "train_(.+)_mean$")){
  traincol <- grep(pattern, colnames(obj$evaluation_log), value = TRUE)
  nm <- sub(pattern, "\\1", traincol)
  out <- structure(list(NULL), names = nm)
  if (exists(nm)){
    fn_metric <- get(nm)
    if (is.function(fn_metric) && all(c("actual", "predicted") %in% names(formals(fn_metric)))){
      out[[1]] <- fn_metric
    }
  }
  out
}

#' @details
#' \code{cb_xgb_iteration()} is a callback function used when a xgboost model is cross-validated 
#' if \code{verbose=TRUE}.
#' It is a reduced version of \code{\link[xgboost]{cb.print.evaluation}}
#' @rdname modeltuner-internal
#' @export
cb_xgb_iteration <- function (period = 1, prefix = "", keep_final_msg = FALSE){
  callback <- function(env = parent.frame()){
    i <- env$iteration
    if ((i - 1)%%period == 0 || i == env$begin_iteration){
      msg <- paste0(prefix, "round ", i, " (nrounds=", env$nrounds, ")")
      message(msg, "\r", appendLF = FALSE)
    }
    if (i == env$end_iteration){
      if (keep_final_msg){
        message(appendLF = TRUE)
      } else {
        message("\r", rep(" ", nchar(msg)), "\r", appendLF = FALSE)
      }
    }
  }
    attr(callback, "call") <- match.call()
    attr(callback, "name") <- "cb.print.evaluation"
    callback
}

# Extract evaluation log from output of xgb.cv:
.xgb_eval_log <- function(cv_result, folds, metric){
  out <- as.data.frame(cv_result$evaluation_log)
  names(out) <- sub("_mean$", "", names(out))
  names(out) <- sub("^(.+)_std$", "se_\\1", names(out))
  evlognms <- paste0(c("train", "test"), "_", metric)
  evlognms <- c(evlognms, paste0("se_", evlognms))
  out <- out[c("iter", intersect(evlognms, names(out)))]
  out[setdiff(evlognms, names(out))] <- NA_real_
  # Adjust se_train and se_test in evaluation log
  # - train:
  nfold <- length(folds)
  if (nfold>1){
    std_corr <- sqrt(nfold/(nfold-1)) # corr for denominator n instead of n-1 in eval_log returned by xgb.cv()
    out[paste0("se_train_", metric)] <- 
      out[paste0("se_train_", metric)] * (nfold-1)/sqrt(nfold) * std_corr
    # - test:
    out[paste0("se_test_", metric)] <- 
      out[paste0("se_test_", metric)] / sqrt(nfold) * std_corr
  }
  out
}

