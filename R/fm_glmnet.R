
#' `formula`-based wrapper for \code{glmnet()}
#' 
#' @description
#' `fm_glmnet()` is a wrapper for \code{\link{glmnet}()} (from package \CRANpkg{glmnet})
#' that fits into the `modeltuner` framework.
#' The model is specified by the arguments `formula` and `data`. 
#' The resulting models belong to the class of so-called *iteratively fitted models*, 
#' see \code{\link{ifm}} and `vignette("ifm")` for information.
#'  
#' @param formula A `formula`.
#' @param data A `data.frame`
#' @param weights weights
#' @param family,\dots Passed to \code{\link[glmnet]{glmnet}()}. Not all will work properly: 
#' avoid argument settings that change the structure of the output of \code{glmnet()}!
#' In `plot.fm_glmnet()`,  \dQuote{...} are passed to both `geom_point()` and `geom_line()`.
#' @param pref_iter An integer, the *preferred iteration*. This is the iteration that is used by default
#' when predictions from the model are computed with `predict()`.
#' If `pref_iter=NULL`, the *last* iteration will be used. 
#' See \code{\link{ifm}} and `vignette("ifm")` for information on the concepts of 
#' *iteratively fitted models* and preferred iterations.
#' The preferred iteration of a model can be changed without re-fitting the model, 
#' see \code{\link{set_pref_iter}()}.
#' @param na.action A function which indicates what should happen when the data contain `NA`s.
#' \code{\link{na.omit}} is the default, \code{\link{na.exclude}} or \code{\link{na.fail}}
#' could be useful alternative settings. 
#' @inheritParams fm_xgb
#' @param object,x Object of class \dQuote{fm_glmnet}.
#' @param newdata Data for prediction.
#' @param s Choice of lambda.
#' @param plot_type A character string, either `colors` (profiles of all coefficients are shown in same facet 
#' distinguished by colors) or `facet` (profiles of different coefficients appear in separate facets).
#' @param coefs Character vector: An optional subset of `x` variables' names to be included in plot.
#' By default, all are included.
#' @param intercept Logical: Whether to include the intercept's profile in the plot.
#' @inheritParams cv_performance
#' 
#' @details 
#' `family` must be one of `"gaussian"`, `"binomial"`, `"poisson"`.
#' 
#' The parameters `x` and `y` to be passed to `glmnet()` are extracted from 
#' `formula` and `data` by means of \code{\link{model.frame}}, \code{\link{model.matrix}}
#' and \code{\link{model.response}}.
#' 
#' *Features of cross-validation of models generated with* `fm_glmnet()`:
#' \itemize{
#' \item{The model class \dQuote{fm_glmnet} belongs to the class of so-called 
#' *iteratively fitted models*; see \link{ifm} and `vignette("ifm")` for information on
#' the peculiarities of cross-validating such models. In particular, 
#' note the role of the parameter `iter` in \code{\link{cv}()}.}
#' \item{When `cv()` is executed with `keep_fits=TRUE`, the fitted models from
#' cross-validation that are stored in the result (and returned by \code{\link{extract_fits}()}) 
#' will not be of class \dQuote{fm_glmnet}, but of class \dQuote{glmnet},}
#' }
#' 
#' @return
#' `fm_glmnet()` returns a list of class \dQuote{fm_glmnet} with components
#' \itemize{
#' \item{*fit*: the fitted model, of class \dQuote{glmnet};}
#' \item{*formula*: the formula;}
#' \item{*x*: the model matrix (resulting from the `formula` using `model.matrix()`);} 
#' \item{*weights*: the fitting weights;}
#' \item{*xlevels*: list of the levels of the factors included in the model;}
#' \item{*pref_iter*: the preferred iteration, an integer (see argument `pref_iter`);}
#' \item{*na.action*: the \code{\link{na.action}} used during data preparation;}
#' \item{*contrasts*: the \code{\link{contrasts}} used during data preparation;}
#' \item{*call*: the matched call generating the model.}
#' } 
#' 
#' @seealso \code{\link[glmnet]{glmnet}}, \code{\link[glmnet]{cv.glmnet}}  (both from package \CRANpkg{glmnet}); 
#' \link{ifm} and \code{vignette("ifm")}; \code{\link{fit.model_fm_glmnet}}; \code{\link{set_pref_iter}}
#' 
#' @examples
# Simulated data
#' d <- simuldat()  
#' (mod1 <- fm_glmnet(Y ~., d))
#' (mod2 <- fm_glmnet(Y ~.^2, d))
#' 
#' # Plot mod1:
#' plot(mod1)
#' # Plot profiles for a subset of the coefficients' only and the intercept
#' plot(mod1, coefs = paste0("X", 1:10), intercept = TRUE)
#' 
#' # Cross-validate
#' mycv <- cv(c(model(mod1, label = "mod1"), 
#'              model(mod2, label = "mod2")), 
#'            nfold = 5)
#' mycv
#' 
#' # Plot cv_performance and evaluation_log:
#' plot(cv_performance(mycv))
#' plot(evaluation_log(mycv))
#' 
#' @importFrom glmnet glmnet predict.glmnet
#' @importFrom stats model.frame model.matrix model.response
#' @export
fm_glmnet <- function(formula, data, weights = NULL, 
                      family = c("gaussian", "binomial", "poisson"), 
                      pref_iter = NULL, na.action = na.omit, keep_x = TRUE, ...){
  family <- match.arg(family)
  if (missing(formula)) formula <- NULL
  if (missing(data)) data <- NULL
  w <- eval(substitute(weights), data, parent.frame())
  xy <- get_xy(formula, data, weights = w, na.action = na.action, remove_intercept = TRUE)
  formula <- xy$terms
  if (NCOL(xy$x) < 2) 
    stop("Invalid formula, expecting at least two variables on rhs.")
  fit <- glmnet(xy$x, xy$y, weights = xy$w, ...)
  if (is.null(pref_iter)){
    pref_iter <- length(fit$lambda)
  } else{
    pref_iter <- min(pref_iter, length(fit$lambda))
  }  
  out <- structure(list(fit = fit,
                        formula = formula,
                        x = xy$x, 
                        weights = xy$w, 
                        xlevels = xy$xlevels,
                        iterations = length(fit$lambda),
                        pref_iter = pref_iter,
                        na.action = xy$na.action, 
                        contrasts = xy$contrasts,
                        call = match.call()), 
                   class = "fm_glmnet")
  if (!keep_x) out$x <- NULL
  out
}

globalVariables(c("x", "y", "w", "iteration", "coefficient", "variable"))

#' @describeIn fm_glmnet `predict()` method
#' @importFrom utils tail
#' @export
predict.fm_glmnet <- function(object, newdata, pref_iter = object$pref_iter, 
                              s = tail(object$fit$lambda, 1), ...){
  if (missing(newdata)){
    if (!is.null(object[["x"]])){
      xnew <- object[["x"]]
    } else {
      stop("Missing ", dQuote("newdata"), " argument and model data not stored: specify newdata.")
    }
  } else {
    xnew <- get_xy(object$formula, data = newdata, xlev = object$xlevels, 
                   na.action = na.pass, x_only = TRUE,
                   contrasts = object$contrasts)$x
  }
  if (is.null(pref_iter)){
    out <- drop(predict(object$fit, newx = xnew, s = s, ...))
  } else {
    stopifnot(length(pref_iter) == 1, is.numeric(pref_iter), pref_iter>0)
    if (is.na(pref_iter)) {
      out <- rep(NA_real_, nrow(xnew))
    } else {
      out <- drop(predict(object$fit, newx = xnew, 
                          s = object$fit$lambda[pref_iter], ...))
    }
  }
  if (missing(newdata) && !is.null(na.act <- object$na.action)){
    out <- napredict(na.act, out)
  }  
  out
}


#' @describeIn fm_glmnet `coef()` method
#' @importFrom stats coef
#' @export
coef.fm_glmnet <- function(object, s = tail(object$fit$lambda, 1), ...){
  drop(as.matrix(coef(object$fit, s = s, ...)))
}

#' @inheritParams print.model
#' @export
print.fm_glmnet <- function(x, abbreviate = TRUE, ...){
  cat("Fitted model of class", sQuote("fm_glmnet"), "\n")
  prnt <- x[c("formula", NA, "call", "iterations", "pref_iter")]
  prnt[[2]] <- deparse(x$call$data)[[1]]
  if (!is.null(d <- dim(x$x))) prnt[[2]] <- paste0(prnt[[2]], " (", d[1], " rows)")
  names(prnt)[[2]] <- "data"
  render_list(prnt, abbreviate = abbreviate)
  invisible(x)
}


#' @describeIn fm_glmnet `plot()` method. Produces a coefficient profile plot.
#' @export
plot.fm_glmnet <- function(x, coefs = NULL, intercept = FALSE,
                           plot_type = c("colors", "facets"), 
                           size = .6, lwd = 0.5, ..., 
                           plot = TRUE, zeroline = TRUE){
  
  plot_type <- match.arg(plot_type)
  
  b <- as.matrix(x$fit$beta)
  if (intercept){
    b <- rbind(`(Intercept)` = x$fit$a0, b)
    if (!is.null(coefs)) coefs <- c("(Intercept)", coefs)
  }
  
  plotdata <- data.frame(
    coefficient = as.vector(b), 
    iteration = rep(seq_len(ncol(b)), each = nrow(b)), 
    variable = factor(rep(rownames(b), ncol(b)), 
                         levels = rownames(b)))
  if (!is.null(coefs)){
    plotdata <- subset(plotdata, variable %in% coefs)
  }

  if (!plot) return(plotdata)
  
  if (plot_type == "colors"){
    out <- ggplot(plotdata, aes(iteration, coefficient, color = variable))
  } else {   # style = "facets"
    out <- ggplot(plotdata, aes(iteration, coefficient)) +
      facet_wrap(~ variable)
  }
  if (zeroline) out <- out + geom_hline(yintercept = 0, lty = 3)
  out + 
    geom_line(lwd = lwd, ...) +
    geom_point(size = size, ...)
}

# ------------------------------------------------------------------------------

#' @export
model.fm_glmnet <- function(x, 
    predict_function = function(object, ...) predict(object, ..., type = "response"), 
    ...){
  out <- NextMethod()
  out$predict_function <- predict_function
  out
}


#' @export
cv_simple.model_fm_glmnet <- function (x, folds, metric = NULL, iter = getOption("cv_iter"), 
                                       keep_fits = FALSE, verbose = TRUE, ...){
  # verbose
  if (!isFALSE(verbose)){
    verbose_prefix <- if (is.character(verbose)) verbose else ""
    verbose <- TRUE
    blank_line <- paste0(rep(" ", nchar(verbose_prefix) + nchar(x$label) + 60), 
                         collapse = "")
    on.exit(message("\r", blank_line, "\r", appendLF = FALSE))
  } else verbose_prefix <- verbose
  nfold <- length(folds)
  if (is.null(metric)) metric <- default_metric(x)
  # NAs are removed for cv model fitting
  xy <- get_xy(x, na.action = na.exclude)
  if (NCOL(xy$x) < 2){
    if (verbose){
      message("\r", blank_line, "\rcv model ", dQuote(label(x)), ": ",
              "Invalid formula, expecting at least two variables on rhs.")
    }
    out <- structure(list(model = x, 
                          folds = folds,
                          metric = metric,
                          predictions = array(NA, dim = c(nrow(x$data), nfold))
                          ), 
                     class = c("cv_simple", "list"))
    warning("Cross-validation results may be invalid or incomplete for model ", 
            dQuote(label(x)), call. = FALSE)
    return(out)
  }
  # Obtain lambdas
  lambda <- fit(x)$fit$lambda
  # prepare call for cv
  call0 <- x$call
  call0[c("formula", "data", "weights")] <- NULL
  cv_call <- quote(glmnet(x = x, y = y, weights = w))
  if (length(call0)>1) 
    cv_call <- modify_call(cv_call, as.list(call0[-1]))
  # cross-validation
  env <- c(unclass(xy)[c("x", "y", "w")], x$saved_objects)
  env <- list2env(env, parent = environment())
  env$folds <- adjust_folds(folds, i_remove = xy$na.action)  # adjusted folds in case of NAs
  cv_table <- get_cv_fits_glmnet(x, cv_call, lambda, env, verbose = verbose_prefix)
  # Evaluation log and iters
  if (verbose){
    pb <- modeltuner_progress_bar(
      prefix = paste0(verbose_prefix, " ", dQuote(x$label), ": Computing evaluation log... "),
      final = paste0(verbose_prefix, " ", dQuote(x$label), ": Evaluation log done."), 
      show_after = 0.5)
  }
  # cv predictions are required for all obs -> adjust yx in case of NAs
  if (length(xy$na.action)){
    xy <- get_xy(x, na.action = na.pass)
    env <- c(unclass(xy)[c("x", "y", "w")], x$saved_objects)
    env <- list2env(env, parent = environment())
  }
  n <- nrow(env$x)
  eval_log <- get_evaluation_log_joint(
    cv_table$fit, folds, 
    predict_function = function(object, newx) x$predict_function(object, newx, s = lambda),
    y = xy$y, x = xy$x, w = xy$w, metric = metric, na.rm = NULL, ...
  )
  eval_log$lambda <- lambda
  eval_log <- eval_log[union(c("iter", "lambda"), names(eval_log))]
  pref_iter <- eval_crit(iter, 
                        list(eval_log = eval_log, metric = metric, label = x$label))
  if (verbose){
    message(blank_line, "\r", verbose_prefix, dQuote(x$label), ": Completing cv", 
            appendLF = FALSE)
  }
  extras <- list(
    evaluation_log = eval_log,
    pref_iter = pref_iter, 
    lambda = lambda)

  # cv predictions
  iters <- vapply(pref_iter, "[[", "iter", FUN.VALUE = integer(1))
  preds <- get_predictions_glmnet(cv_table$fit, model = x, env = env, 
                                  lambda = lambda[iters])
  preds <- structure(asplit(preds, 2), dim = NULL)
  if (!is.null(na.action <- xy$na.action)){
    preds[] <- lapply(preds, napredict, omit = na.action)
  }
  # cv_performance
  perform <- vector("list", length(pref_iter))
  ttm <- paste0(c("train_", "test_"), names(metric))
  perf_nms <- c(ttm, paste0("se_", ttm), "NA_train", "NA_test")
  for (i in seq_along(pref_iter)){
    iters <- pref_iter[[i]]$iter
    perform[[i]] <- extras$evaluation_log[iters, c("iter", perf_nms), drop = FALSE]
    names(perform[[i]])[[1]] <- "iteration"
    rownames(perform[[i]]) <- NULL
  }
  #  Output object
  out <- structure(list(model = x, folds = folds, metric = metric, 
                        performance = perform, predictions = preds), 
                   class = c("cv_simple", "list"))
  if (keep_fits) 
    out$fits <- cv_table$fit
  # Append evaluation log and related information
  out$extras <- extras
  out
}

# Aux functions
get_cv_fits_glmnet <- function(model, cv_call, lambda, env, verbose = TRUE){
  nfold <- length(env$folds)
  cv_call <- substitute_call(cv_call, 
    list(x = quote(x[-folds[[i]], , drop = FALSE]), 
         y = quote(y[-folds[[i]]]),
         w = quote(w[-folds[[i]]])))
  cv_call$lambda <- lambda
  cv_table <- data.frame(i = seq_len(nfold))
  cv_table$fit <- vector("list", nfold)
  if (!isFALSE(verbose)){
    verbose_prefix <- if (is.character(verbose)) paste0(verbose, " ") else ""
    verbose <- TRUE
    pb <- modeltuner_progress_bar(
      prefix = paste0(verbose_prefix, dQuote(model$label), 
                      ": fitting model "),
      total = nfold, 
      final = paste0(verbose_prefix, dQuote(model$label), ": processing..."))
  }  
  for (i in seq_len(nfold)){
    env$i <- i
    cv_table$fit[[i]] <- try(eval(cv_call, env))
    if (verbose) pb$tick() 
  }  
  cv_table
}
get_predictions_glmnet <- function(fits, model, env, lambda){
  nfold <- length(fits)
  n <- nrow(env$x)
  if (is.null(fits) || 
      (is.list(fits) && all(vapply(fits, is.null, logical(1))))){
    predictions <- rep(list(matrix(NA_real_, n, nfold)), length(lambda))
  } else {
    predictions <- vector("list", nfold)
    predfun <- model$predict_function
    for (i in seq_len(nfold)) {
      predictions[[i]] <- predfun(fits[[i]], env$x, s = lambda)
    }
  }
  predictions <- do.call(c, predictions)
  dim(predictions) <- c(n, length(lambda), nfold) 
  predictions
}

#' @export
terms.fm_glmnet <- function(x, ...) x$formula

#' @export
formula.fm_glmnet <- function(x, ...) formula(terms(x), ...)

#' @rdname fit
#' @export
fit.model_fm_glmnet <- function(x, iter, eval = TRUE, force = FALSE, ...){
  # if (inherits(x$fit, x$class) && eval) return(x$fit)
  if (missing(iter)){
    x <- set_pref_iter(x, warn = FALSE, ...) # uses cv_info
    if (has_fit(x)) x$fit$pref_iter <- x$call$pref   
  } else if (!is.null(iter)) {
    x <- set_pref_iter(x, iter = iter, warn = TRUE, ...)
  }
  
  fit.model(x, eval = eval, force = force,
            pref_iter = x$cv_info$iters[[1]], ...)
}

