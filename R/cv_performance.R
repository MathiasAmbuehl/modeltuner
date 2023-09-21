
#' Calculate train and test errors based on cross-validation.
#' 
#' @description
#' `cv_performance` returns a *performance table*, a summary table of training and test errors for the models included in the
#' main argument `x`, of class \dQuote{performance}. 
#' `cv_performance.cv()` is the core method. All other methods run this method after some preprocessing steps –
#' see section \dQuote{Methods}.
#' 
#' The method `plot.performance()` generates a graphical display of the performance values in the \dQuote{performance} object `x`,  
#' a bar chart by default, alternatively a line plot (depending on parameter `xvar`).
#' 
#' @param x \dQuote{\link{cv}} object, or object of another class.
#' @param metric A metric (see \code{\link{metrics}}), specified either as a character string (name of the metric function), 
#' or as a named list of length 1, as in \code{list(rmse = rmse)}. 
#' `metric=NULL` selects the default metric, see \code{\link{default_metric}}. 
#' @param eval_weights Evaluation weights; see the \dQuote{Evaluation weights} in the 
#' \dQuote{Details} section of `?modeltuner`.
#' `"eval_weights=default` means \dQuote{use fitting weights} while `"eval_weights=NULL`
#' means unweighted evaluation.
#' @param na.rm Logical: Whether NA values should be excluded from computations.
#' @param param Logical: Keep parameters from the parameter table in the output?
#' @param \dots Passed to the \code{metric} function.
#' @param digits Integer: Number of digits to print.
#' @param n Integer: Maximal number of rows to print.
#' 
#' @details
#' While different models in a \dQuote{cv} object can have different `metric`s, `cv_performance()` always reports the same metric
#' for all models.
#' If `metric` is not specified in the call to `cv_performance()`, the metric from the *first model* will be chosen
#' (see \code{\link{default_metric}}). 
#' If `cv_performance()` is applied to a \dQuote{cv} object including models having different default `weights` 
#' (and `weights` are not given explicitly), `cv_performance()` will use `eval_weights=NULL`.
#' 
#' **Details on evaluation:** \cr
#' For each `fold`, the evaluation metric is calculated separately for the sets of training and test observations, 
#' yielding \eqn{k} pairs \eqn{(train\_err_i, test\_err_i)}, \eqn{i=1, \ldots, k}, where \eqn{k} is the number of folds. 
#' `cv_performance()` reports the average of the \eqn{train\_err_i}, \eqn{i=1, \ldots, k}, as the training error and 
#' the average of the \eqn{test\_err_i} as the test error.
#' In case of non-NULL `eval_weights`, weighted averages are calculated with group weights computed as 
#' the group-wise sums of the observations weights.
#' 
#' Standard errors of the reported errors are only printed if you set `se=TRUE` when printing the performance table, 
#' which is not the case by default (see the option `cv_show_se`, cf. \code{\link{modeltuner_options}}).
#' These standard errors are only reported if the number of folds is `>1`.
#' Their computation is based on the assumption of perfect independence of all residuals, and may thus be very unreliable.
#' As a rough guide, the standard errors are reasonable in case of a model with just a few free parameters and many observations, 
#' while they will severely underestimate the actual uncertainty in the case of models of high structural complexity.
#' 
#' @return
#' `cv_performance()` returns a performance table. 
#' This is a \link{param_table} with additional class \dQuote{performance} having some additional information stored in its attributes. 
#' Each row corresponds to a model. It has columns `train_`*metric* and `test_`*metric*
#' (e.g., `train_rmse` and `test_rmse`), `se_train_`*metric* and `se_test_`*metric*, 
#' `time_cv` (execution time of the cross-validation), and possibly
#' more columns being part of the parameter table of the multimodel (see \dQuote{Details} section in \code{\link{multimodel}}).
#' 
#' @section Methods:
#' \itemize{
#' \item{`cv_performance.cv()` is the core method described above.
#' It uses the first cross-validated model's metric as default metric.}
#' \item{`cv_performance.model(x, ...)` executes `x %>% cv %>% cv_performance(...)`.}
#' \item{`cv_performance.multimodel(x, ...)` executes `x %>% cv %>% cv_performance(...)`.
#' Its (implicit) default metric is \code{\link{default_metric}(x)}.}
#' \item{`cv_performance.default(x, ...)` executes `x %>% model %>% cv %>% cv_performance(...)`, where `x` is a fitted model.}
#' }
#' 
#' @seealso The sections on \dQuote{Metrics} and  \dQuote{Evaluation weights}
#' in \code{?\link{modeltuner}}; \code{\link{metrics}}, \code{\link{subset}}, \code{\link{sort_models}}.
#' 
#' @examples
#' # iris data: compare several model approaches
#' mm <- c(
#'   lm = model(lm(Sepal.Length ~., iris)), 
#'   lm2 = model(lm(Sepal.Length ~.^2, iris)), 
#'   glmnet = model(fm_glmnet(Sepal.Length ~., iris)), 
#'   glmnet2 = model(fm_glmnet(Sepal.Length ~.^2, iris)), 
#'   fm_xgb = model(fm_xgb(Sepal.Length ~., iris)))
#' cvobj <- cv(mm, nfold = 5)
#' 
#' # performance
#' cvperm <- cv_performance(cvobj)
#' cvperm
#' 
#' # Sort by test error
#' sort_models(cvperm, by = "test")
#' 
#' #' print performance table with estimated standard errors (unreliable!)
#' print(cvperm, se = TRUE)
#' 
#' @name cv_performance
#' @export
cv_performance <- function(x, ...){
  UseMethod("cv_performance")
}


#' @rdname cv_performance
#' @export
cv_performance.cv <- function(x, metric = x$metric[1], eval_weights = "default", 
                              na.rm = FALSE, param = TRUE, ...){
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
  nmodel <- n_model(x)
  # Get cv_performance model-wise
  outlist <- lapply(seq_len(nmodel), 
    function(i) cv_performance(extract(x, i), metric = metric, 
                               eval_weights = eval_weights, na.rm = na.rm, ...))
  out <- do.call(rbind, c(outlist, list(force_list = FALSE)))
  if (param && !is.null(x$multimodel$param)){
    out <- cbind(x$multimodel$param, 
                 out[setdiff(names(out), names(x$multimodel$param))])
  }
  rownames(out) <- label(x$multimodel)
  if (!is.function(metric[[1]]) && anyNA(out[1:4])){
    warning("Metric not available: ", names(metric), call. = FALSE)
  }
  out$time_cv <- x$timing
  out <- param_table(out)
  structure(out, class = union("performance", class(out)), metric = metric,
            weighted = !is.null(eval_weights))
}

#' @export
cv_performance.cv_simple <- function(x, metric = x$metric, eval_weights = x$model$weights, 
                                     na.rm = FALSE, ...){
  nm_metric <- names(metric)
  if (is.list(x$performance)){
    x$performance <- x$performance[[1]]
  }
  # Prepare output object
  if (length(x$performance)){
    if (!na.rm && isTRUE(x$performance$NA_train)) x$performance[paste0(c("train_", "se_train_"), nm_metric)] <- NA
    if (!na.rm && isTRUE(x$performance$NA_test)) x$performance[paste0(c("test_", "se_test_"), nm_metric)] <- NA
    x$performance$NA_train <- x$performance$NA_test <- NULL
    perfnms <- paste(c("train", "test", "se_train", "se_test"), nm_metric, 
                     sep = "_")
    extracols <- setdiff(colnames(x$performance), 
                         paste(c("train", "test", "se_train", "se_test"), 
                               names(x$metric), sep = "_"))
    performancenms <- c(perfnms, extracols)
    out <- as.data.frame(matrix(NA_real_, nrow = 1, ncol = length(performancenms)))
    names(out) <- performancenms
    out[extracols] <- x$performance[extracols]
    # If some results are available from pre-computed results, insert them:
    default_metric <- is_default_metric(metric, x)
    default_weights <- is_default_weights(eval_weights, x)
    if (default_weights && default_metric){
      insert <- intersect(names(x$performance), names(out)[is.na(out)])
      out[insert] <- x$performance[insert]
    }
  } else {
    out <- NULL
  }
  table_complete <- !is.null(out) && default_weights && default_metric &&
    all(perfnms %in% names(out)) && all(is.finite(unlist(out[perfnms])))
  if (!table_complete){
    # Calculate the remainig results
    predictions <- cv_pred_matrix(x)
    out <- cv_perf_basic(actual = response(x$model, complete = FALSE), predicted = predictions, 
                         w = eval_weights, metric = metric, 
                         folds = x$folds, na.rm = na.rm, ..., out = out)
  }
  # Return
  out <- param_table(out)
  class(out) <- union("performance", class(out))
  out
}

# Two aux fns used in cv_performance.cv_simple (and elsewhere)
is_default_metric <- function(metric, object){
  stopifnot(inherits(object, "cv_simple"))
  isTRUE(names(metric) == names(object$metric)) &&
    isTRUE(all.equal(metric[[1]], object$metric[[1]], check.environment = FALSE))
}
is_default_weights <- function(eval_weights, object){
  stopifnot(inherits(object, "cv_simple"))
  (is.null(eval_weights) && is.null(object$model$weights)) ||
    (length(eval_weights) == length(object$model$weights) && all(eval_weights == object$model$weights))
}



# Calculation of performance measures
# dots go to metric function
# out: optionally pass (complete or incomplete) performance table as input
# prep is optionally given instead of (actual, w, folds)
cv_perf_basic <- function(actual, predicted, w, metric, folds, na.rm, ..., out = NULL, 
                          prep = prep_cv_perf(actual, w, folds)){
  out_perf <- cv_perf(prep, predicted, fn_metric = metric[[1]], ...)
  if (keep_NA_info <- is.null(na.rm)) na.rm <- TRUE
  if (out_perf$NA_train && !na.rm) out_perf$train <- out_perf$se_train <- NA
  if (out_perf$NA_test && !na.rm) out_perf$test <- out_perf$se_test <- NA
  if (!keep_NA_info) out_perf$NA_train <- out_perf$NA_test <- NULL
  sel_cols <- if (keep_NA_info) !grepl("^NA_", names(out_perf)) else TRUE
  names(out_perf)[sel_cols] <- paste0(names(out_perf)[sel_cols], "_", names(metric)[1])
  if (is.null(out)){
    out <- out_perf
  } else {
    out[names(out_perf)] <- out_perf
  }
  out
}

#' @importFrom utils globalVariables
globalVariables(c("is_ind", "oos_ind", "train_folds", "actual_is", "actual_oos", "w_is", "w_oos"))

prep_cv_perf <- function(actual, w, folds){
  stopifnot(length(actual) == attr(folds, "n"))
    
  # 1) n and nfold
  n <- attr(folds, "n")
  nfold <- length(folds)
  
  # 2) objects depending on folds, w and metric only:
  oos_ind <- oos_indices(folds)
  ii_oos <- oos_ind[, 1]  # multiple??
  
  if (nfold == 1 && length(folds[[1]]) == 0){ # case "no folds" - evaluation of model (before cv)
    ii_is_mat <- seq_len(n)
  } else {
    ii_is_mat <- seq_len(n*nfold)[-(oos_ind[, 1] + (oos_ind[, 2]-1) * n)]
  }
  ii_is_vect <- (ii_is_mat-1) %% n + 1
  test_ns <- tabulate(oos_ind[, 2], nfold)
  train_folds <- rep(seq_len(nfold), each = n)[ii_is_mat]
  
  is_fold_ind <- split(seq_along(ii_is_mat), 
                       rep(seq_along(folds), tabulate(train_folds)))

  # 3) depending on weigths
  if (is.null(w)){
    w_is <- w_oos <- NULL
  } else {
    w_oos <- w[ii_oos]      # oos
    w_is <- w[ii_is_vect] # is
  }
  
  # 4) depending on actual
  actual_oos <- actual[ii_oos]
  actual_is <- actual[ii_is_vect]
  
  list(n = n, nfold = nfold, 
       is_ind = ii_is_mat, 
       oos_ind = oos_ind,
       train_folds = train_folds,
       actual_is = actual_is, 
       actual_oos = actual_oos,
       w_is = w_is, 
       w_oos = w_oos)
}

# cv_perf(): always excludes NAs from calculations, 
#            but passes info on presence of NAs in columns NA_train/NA_test
#' @importFrom stats weighted.mean cov.wt sd
cv_perf <- function(prep, predicted, fn_metric, ..., aggregate = TRUE){
  
  if (!is.matrix(predicted)){
    predicted <- matrix(predicted, nrow = prep$n, ncol = prep$nfold)
  }
  
  # output
  out <- rep(list(NA_real_), 4)
  names(out) <- c("train", "test", "se_train", "se_test")
  # if metric is not available -> return all NA 
  if (!is.function(fn_metric)) return(out)

  fold_errs <- list(train = NULL, test = NULL)
  unweighted <- is.null(prep$w_is)
  test_folds <- prep$oos_ind[, 2]
  
  # sort predictions by is/oos
  pred_is <- predicted[prep$is_ind]
  pred_oos <- predicted[prep$oos_ind]
  
  # training errors
  if (unweighted){
    fold_errs[[1]] <- sapply(seq_len(prep$nfold), function(i){
      sel <- prep$train_folds == i & !is.na(pred_is)
      fn_metric(prep$actual_is[sel], pred_is[sel], ...)
    })
  } else {
    fold_errs[[1]] <- sapply(seq_len(prep$nfold), function(i){
      sel <- prep$train_folds == i & !is.na(pred_is)
      fn_metric(prep$actual_is[sel], pred_is[sel], w = prep$w_is[sel], ...)
    })
  }
  # test errors
  if (unweighted){
    fold_errs[[2]] <- sapply(seq_len(prep$nfold), function(i){
      sel <- test_folds == i & !is.na(pred_oos)
      fn_metric(prep$actual_oos[sel], pred_oos[sel], ...)
    })
  } else {
    fold_errs[[2]] <- sapply(seq_len(prep$nfold), function(i){
      sel <- test_folds == i & !is.na(pred_oos)
      fn_metric(prep$actual_oos[sel], pred_oos[sel], w = prep$w_oos[sel], ...)
    })
  }
  # fold weights
  if (unweighted){
    fold_w <- list(
      train = tabulate(prep$train_folds, nbins = prep$nfold), 
      test = tabulate(test_folds, nbins = prep$nfold)
    )
  } else {
    fold_w <- list(
      train = as.vector(tapply(prep$w_is, prep$train_folds, sum)),
      test = as.vector(tapply(prep$w_oos, test_folds, sum))
    )
  }
  if (!aggregate){ # for internal use only...
    return(list(errs = fold_errs, weights = fold_w))
  }

  out <- data.frame(row.names = "")
  # training & test error
  out$train <- weighted.mean(fold_errs[[1]], fold_w[[1]])
  out$test <- weighted.mean(fold_errs[[2]], fold_w[[2]])
  # training & test se
  out$se_train <- wsd(fold_errs[[1]], fold_w[[1]])
  out$se_train <- out$se_train * (prep$nfold-1) / sqrt(prep$nfold)
  out$se_test <- wsd(fold_errs[[2]], fold_w[[2]])
  out$se_test <- out$se_test / sqrt(prep$nfold)
  # presence of NA predcitions
  out$NA_train <- anyNA(pred_is)
  out$NA_test <- anyNA(pred_oos)
  out
}

# weighted sd
wsd <- function(x, w){
  if (length(x) <= 1 || any(is.na(x))) return(NA)
  dim(x) <- c(length(x), 1)
  as.vector(sqrt(cov.wt(x, w)$cov))
}

#' @export
weights.cv_simple <- function(object, ...){
  object$weights
}

# ------------------------------------------------------------------------------

#' @rdname cv_performance
#' @export
cv_performance.model <- function(x, metric = NULL, eval_weights = "default", 
                                 na.rm = FALSE, ...){
  if (!is.null(metric)) metric <- get_metric(metric, parent.frame(), x)
  cvobj <- cv(x, metric = metric)
  cv_performance(cvobj, eval_weights = eval_weights, na.rm = na.rm, ...)
}

#' @rdname cv_performance
#' @export
cv_performance.multimodel <- function(x, metric = NULL, eval_weights = "default", 
                                      na.rm = FALSE, ...){
  if (!is.null(metric)) metric <- get_metric(metric, parent.frame(), x)
  mcvobj <- cv(x, metric = metric)
  cv_performance(mcvobj, eval_weights = eval_weights, na.rm = na.rm, ...)
}

#' @rdname cv_performance
#' @export
cv_performance.default <- function(x, metric = NULL, eval_weights = "default", 
                                   na.rm = FALSE, ...){
  modelobj <- model(x, env = parent.frame())
  if (!is.null(metric)) metric <- get_metric(metric, parent.frame(), modelobj)
  cvobj <- cv(modelobj, metric = metric)
  cv_performance(cvobj, eval_weights = eval_weights, na.rm = na.rm, ...)
}


# ------------------------------------------------------------------------------

#' @rdname cv_performance
#' @param se Logical: Show standard errors?
#' @export
print.performance <- function(x, se = getOption("cv_show_se"), 
                              n = getOption("print_max_row"), digits = 5, 
                              param = TRUE, ...){
  if (!se){
    x[grepl("^se_", names(x))] <- NULL
  }
  cat("--- Performance table ---", 
      "\nMetric: ", names(attr(x, "metric")),
      sep = "")
  cat("\n")
  print.param_table(x, n = n, digits = digits, ...)
  if (isTRUE(attr(x, "weighted")))
    cat("Performance calculated with weighted metric", fill = getOption("width"))
  if (se) message("The reported standard errors may be inaccurate.")
  invisible(x)
}

# ------------------------------------------------------------------------------

#' Plot method for class \dQuote{performance}. # The lengths of the errorbars corresponds to +/-1 standard error.
#' @rdname cv_performance
#' @inheritParams plot.model
#' @param xvar If `xvar` if not specified (default), a bar plot is drawn.
#' Alternatively, a line plot is generated, with `xvar` as the variable on x axis.
#' `xvar` should be a character string, the name of a numeric variable in the performance table `x`.
#' Typically, the `xvar` is some hyperparameter varying across models.
#' @param errorbars Logical: Whether to add error bars in plots.
#' @param size Graphic detail: Size of point.
#' @param lwd Graphic detail: Line width of interpolating line.
#' @param lwd_errorbars Graphic detail: Line width of errorbars.
#' @param alpha Graphic detail: Opacity of bars.
#' @param zeroline Logical: Whether to include a horizontal reference line at level 0.
#' 
#' @importFrom ggplot2 ggplot aes_string geom_point geom_line geom_hline geom_vline 
#' geom_bar geom_errorbar position_dodge ylab
#' 
#' @export 
plot.performance <- function(x, xvar = "model", errorbars = getOption("cv_show_se"), plot = TRUE, 
                             size = 2, lwd = 1, lwd_errorbars = 0.5, 
                             zeroline = TRUE, alpha = 0.3, ...){
  x$model <- label(x)
  nm_metric <- names(attr(x, "metric"))
  names(x)[match(paste0(c("train_", "test_"), nm_metric) , names(x))] <- 
    c("train", "test")
  out <- data.frame(
    x[rep(seq_len(nrow(x)), 2), 
      setdiff(names(x), c("train", "test"))], 
    error = with(x, c(train, test)), 
    .metric. = rep(c("train", "test"), each = nrow(x)))
  names(out)[names(out) == ".metric."] <- nm_metric
  if (errorbars)
    out[["se"]] <- ifelse(out[[nm_metric]] == "train",
                          out[[paste0("se_train_", nm_metric)]],
                          out[[paste0("se_test_", nm_metric)]])
  out[paste0("se_", c("train", "test"), "_", nm_metric)] <- out$time_cv <- NULL
  rownames(out) <- NULL
  if (!plot) return(out)
  
  if (all(is.na(out$se))) errorbars <- FALSE
  
  if (length(xvar) != 1 || !(xvar %in% names(out)))
    stop("Invalid choice of ", sQuote("xvar"), ".")
  if (is.list(out[[xvar]])){
    if (is_list_of_formulas(out[[xvar]]))  # special case column of formulas
      out[[xvar]] <- as.character(out[[xvar]])
    if (any(lennot1 <- (lengths(out[[xvar]]) != 1)))
      out[[xvar]][lennot1] <- list(NA)
    out[[xvar]] <- unlist(out[[xvar]], use.names = FALSE)
  }
  if (!is.numeric(out[[xvar]]) || xvar == "model") {
    out[[xvar]] <- as.character(out[[xvar]])
    out[[xvar]] <- factor(out[[xvar]], levels = unique(out[[xvar]]))
  }

  min_test <- min(x$test)
  min_x <- out[[xvar]][which.min(x$test)]
  if (errorbars){
    message("The standard errors shown as error bars may be inaccurate.")
    min_up <- with(subset(out, out[[xvar]] == min_x & out[[nm_metric]] == "test") , 
                   error + se)
  }
  if (is.numeric(out[[xvar]])){
    difs <- diff(sort(unique(out[[xvar]])))
    wid <- min(difs[difs>0])/4
    pl <- ggplot(out, aes_string(xvar, "error", color = nm_metric)) +
      ylab(nm_metric)
    if (zeroline) pl <- pl +
      geom_hline(yintercept = 0, lty = 2)
    pl <- pl + 
      geom_point(size = size) + geom_line(lwd = lwd) + 
      geom_hline(yintercept = min_test, lty = 3) + 
      geom_vline(xintercept = min_x, lty = 3)
    if (errorbars) pl <- pl +
      geom_errorbar(mapping = aes_string(ymin = "error - se", 
                                         ymax = "error + se"), 
                    width = wid, lwd = lwd_errorbars) + 
      geom_hline(yintercept = min_up, lty = 3)
  } else {
    pl <- ggplot(out, aes_string(xvar, "error", col = nm_metric, fill = nm_metric)) +
      ylab(nm_metric)
    if (zeroline) pl <- pl +
      geom_hline(yintercept = 0, lty = 2)
    pl <- pl +
      geom_bar(stat = "identity", position = position_dodge(width=.8), width = .8,
               alpha = alpha, lwd = lwd) + 
      geom_hline(yintercept = min_test, lty = 3) + 
      geom_vline(xintercept = min_x, lty = 3)
    if (errorbars) pl <- pl +
      geom_errorbar(mapping = aes_string(ymin = "error - se", 
                                         ymax = "error + se"), 
                    position = position_dodge(width=.8), 
                    width = 1/4, lwd = lwd_errorbars) + 
      geom_hline(yintercept = min_up, lty = 3)
  }
  pl
}

# Aux fun
is_list_of_formulas <- function(x) all(sapply(x, inherits, "formula"))

# --------------------------------------

# transform method for class "performance":
#   attributes are kept
#' @export
transform.performance <- function(`_data`, ...){
  out <- transform.data.frame(`_data`, ...)
  add_attr <- intersect(
    c("metric", "weighted", "class"),
    names(attributes(`_data`)))
  attributes(out)[add_attr] <- attributes(`_data`)[add_attr]
  out
}

# `[` method for class "performance":
#    attributes are kept
#' @export
`[.performance` <- function(x, i, j, drop = FALSE){
  Narg <- nargs() - !missing(drop)
  if (missing(i) && missing(j)) return(x)
  if (Narg < 3 && missing(j)){
    j <- i
    i <- TRUE
  } else if (missing(i)){
    i <- TRUE
  }
  attrib <- attributes(x)
  attrib$names <- attrib$row.names <- NULL
  out <- structure(x, class = "data.frame")[i, j, drop = drop]
  if (length(dim(out))){
    attributes(out)[names(attrib)] <- attrib
  }
  out
}
