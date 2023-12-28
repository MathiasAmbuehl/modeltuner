
#' Run a cross-validation
#' 
#' @description
#' `cv()` executes a cross-validation procedure.
#' For each fold (specified in argument `nfold `or `folds`), the original model is re-fitted using the complement
#' of the fold as training data.
#' Cross-validations of multiple models are executed using identical `folds`.
#' 
#' @param x A \code{\link{model}}, \code{\link{multimodel}} or fitted model 
#' (see sections \dQuote{Methods}).
#' @param nfold,folds Passed to \code{\link{make_folds}}.
#' @param \dots These arguments are passed internally to methods of `cv_simple()`, 
#' a currently undocumented generic that runs the cross-validation on a single model.
#' @param metric A metric (see \code{\link{metrics}}). 
#' `metric=NULL` selects the default metric, see \code{\link{default_metric}}. 
#' @param iter A preference criterion, or a list of several criteria.
#' Only relevant for *iteratively fitted models* (see \link{ifm}), ignored otherwise.
#' @param param Logical. Include parameter table in output? See \code{?\link{multimodel}}.
#' @param keep_fits Logical: Keep the cross-validation's model fits?
#' @param verbose Logical: Output information on execution progress in console?
#' @param n Integer: Model details are printed for first `n` models in `print.cv()`.
#' @param what Which elements of the multimodel should be printed? See \code{\link{print.model}}.
#' @param show_metric Logical: Whether to print the cross-validated models' metric.
#' @param width Integer: Width of printed output.
#' 
#' @details
#' The same cross-validations groups (folds) are used for all models.
#' 
#' Each model in `x` is processed separately with the function \code{\link{cv_simple}()},
#' a generic function for internal use. 
#' Besides the standard method `cv_simple.model()`, there are currently specific methods of `cv_simple()` 
#' for models generated with \code{\link{fm_xgb}()} and \code{\link{fm_glmnet}()}.
#' 
#' @return
#' The output from `cv()` is a list of class \dQuote{cv} having the following elements:
#' \itemize{
#' \item{*multimodel*: a \code{multimodel};}
#' \item{*folds*: the folds, as defined in `nfold` or `folds` (see \code{\link{make_folds}});}
#' \item{*fits*: if `keep_fits=FALSE` (the default): `NULL`; if `keep_fits=TRUE`: the list of the model fits 
#' resulting from the cross-validation, see \code{\link{extract_fits}});}
#' \item{*metric*: a list: the default evaluation metrics, not necessarily the same for all models;}
#' \item{*predictions*: a list of matrices of dimension \eqn{n \times k} where 
#' \eqn{n} is the number of observations in the model data and \eqn{k} is the number of folds; 
#' each of these list entries corresponds to a model;}
#' \item{*performance*: a list of performance tables (see \code{\link{cv_performance}}), 
#' that are saved only for certain model classes; often \code{NULL};}
#' \item{*timing*: execution time of cross-validation;}
#' \item{*extras*: a list of extra results from cross-validation, 
#' which are saved only for certain model classes; often \code{NULL}.
#' If the model `x` is an iteratively fitted model (\link{ifm}), `extras` contain the 
#' cross-validated model's evaluation log and information on preferred iterations.}
#' }
#'  
#' @section Methods:
#' \itemize{
#' \item{`cv.multimodel()`, the core method.}
#' \item{`cv.model(x, ...)` corresponds to `x %>% multimodel %>% cv(...)`.}
#' \item{The default method essentially executes `x %>% model %>% cv(...)` and
#' thus expects a fitted model as its `x`.}
#' }
#' 
#' @seealso 
#' \code{\link{make_folds}}, \code{\link{multimodel}}, 
#' \code{\link{cv_performance}}, \code{\link{cv_predict}}, \code{\link{c.cv}}, \code{\link{extract_fits}}
#' 
#' @examples
#' mm <- multimodel(model(fm_knn(Sepal.Length ~ ., iris)), k = 1:5)
#' cv(mm)
#' 
#' mm_cars <- c(simpleLinear = model(lm(mpg ~ cyl, mtcars)),
#'              linear = model(lm(mpg ~ ., mtcars)),
#'              if (require(ranger)) model(ranger(mpg ~ ., mtcars), label = "forest"))
#' mm_cars
#' cv_cars <- cv(mm_cars, nfold = 5)
#' cv_cars
#' cv_performance(cv_cars)
#' 
#' # Non-default metric:
#' cv_performance(cv_cars, metric = "medae")
#' 
#' @name cv
NULL


#' @rdname cv
#' @export
cv <- function(x, ...){
  UseMethod("cv")
}


#' @rdname cv
#' @export
cv.model <- function(x, nfold = getOption("cv_nfold"), folds = NULL, ..., 
                     metric = NULL, iter = getOption("cv_iter"), 
                     param = TRUE, keep_fits = FALSE, 
                     verbose = getOption("cv_verbose")){ 
  # folds
  n <- length(response(x))
  folds <- make_folds(n, nfold, folds)
  if (!is.null(metric)) metric <- get_metric(metric, parent.frame(), x)
  if (verbose){
    final_msg <- extract_final_msg(verbose, label(x))
    verbose <- TRUE
  }
  # Cross-val
  prtm <- proc.time()
  out <- cv_simple(x, folds, metric = metric, iter = iter, keep_fits = keep_fits, 
                   verbose = if (verbose) "cv" else FALSE, ...)
  tm <- (proc.time() - prtm)[["elapsed"]]
  # output object
  out$model <- multimodel(out$model, simplify = FALSE)
  names(out)[[1]] <- "multimodel"
  out$performance <- list(out$performance)
  nms <- union(c("multimodel", "folds", "metric", "performance"), 
               names(out))  # reorder elements
  out <- out[nms]
  out$predictions <- list(out$predictions)
  if (keep_fits) out$fits <- list(out$fits)
  out$extras <- list(out$extras)
  out$timing <- tm
  class(out) <- c("cv", "list")
  set_last_cv(out)
  if (verbose && nchar(final_msg)){
    appendLF <- attr(final_msg, "appendLF")
    message(final_msg, if (!appendLF) "\t", appendLF = appendLF)
  }
  out
}

#' @rdname cv
#' @export
cv.multimodel <- function(x, nfold = getOption("cv_nfold"), folds = NULL, 
                          metric = NULL, iter = getOption("cv_iter"), 
                          param = TRUE, keep_fits = FALSE, 
                          verbose = getOption("cv_verbose"), ...){
  is_pmodel <- vapply(x$models, inherits, "pmodel", FUN.VALUE = logical(1))
  if (any(is_pmodel)){
    several_pmod <- sum(is_pmodel)>1
    warning("Model", if (several_pmod) "s", " ", 
            paste0(vapply(label(x)[is_pmodel], dQuote, character(1)), collapse = ", "), 
            if (several_pmod) " are" else " is", " of class ", dQuote("pmodel"), 
            " and can not be cross-validated. ", 
            if (several_pmod) "They are" else "It is", " dropped.")
    x <- subset(x, !is_pmodel)
  }
  if (n_model(x) == 0) return(empty_cv(folds))
  if (!param) x <- c(x, param = FALSE)
  if (verbose){
    verbose_arg <- verbose
    verbose <- TRUE
  }
  # folds
  n <- length(response(x$models[[1]]))
  folds <- make_folds(n, nfold, folds)
  nmod <- n_model(x)
  cvlist <- vector("list", nmod)
  tm <- numeric(nmod)
  if (!is.null(metric)) metric <- get_metric(metric, parent.frame(), x)
  for (i in seq_len(nmod)){
    if (verbose){
      verbose_arg[] <- if (n_model(x)>1){
        paste0("cv model ", i, "/", length(cvlist))
      } else "cv"
    } else verbose_arg <- verbose
    mod <- extract(x, i)
    prtm <- proc.time()
    cvlist[[i]] <- cv_simple(mod, folds = folds, metric = metric, iter = iter, 
                             keep_fits = keep_fits, verbose = verbose_arg, ...)
    tm[i] <- (proc.time() - prtm)[["elapsed"]]
  }
  out <- cvlist2cv(cvlist, multimodel = x, folds = folds, 
                   keep_fits = keep_fits, tm = tm)
  if (verbose){
    final_msg <- extract_final_msg(verbose_arg, label(x))
    if (nchar(final_msg)) message(final_msg, 
                                  appendLF = attr(final_msg, "appendLF"))
  }
  set_last_cv(out)
  out
}

#' @rdname cv 
#' @export
cv.default <- function(x, nfold = getOption("cv_nfold"), folds = NULL, ..., 
                       metric = NULL, iter = getOption("cv_iter"), 
                       param = TRUE, keep_fits = FALSE){ 
  modelobj <- model(x, env = parent.frame())
  if (!is.null(metric)) metric <- get_metric(metric, parent.frame(), x)
  cv(modelobj, nfold, folds, ..., metric = metric, iter = iter, param = param, 
     keep_fits = keep_fits) 
}

# ------------------------------------------------------------------------------

# ==== Auxiliary funtions ====

cvlist2cv <- function(cvlist, multimodel = do.call(c, lapply(cvlist, "[[", "model")), 
                      folds = cvlist[[1]]$folds, 
                      keep_fits = FALSE, tm = numeric(length(cvlist))){
  out <- list(multimodel = multimodel, folds = folds)
  out$metric <- do.call(c, lapply(cvlist, "[[", "metric"))
  out$performance <- lapply(cvlist, "[[", "performance")
  out$predictions <- lapply(cvlist, "[[", "predictions")
  if (keep_fits){
    out$fits <- lapply(cvlist, "[[", "fits")
  }
  out$extras <- lapply(cvlist, "[[", "extras")
  out$timing <- tm
  class(out) <- c("cv", "list")
  out
}

extract_final_msg <- function(verb, label){
  if (is.character(verb) || verb){
    final_msg <- attr(verb, "final")
    if (is.null(final_msg)){
      if (length(label) == 1){
        final_msg <- paste0("cv done for model ", dQuote(label))
      } else {
        final_msg <- paste0("cv done for ", length(label), " models.")
      }
    }
    structure(as.character(final_msg),
              appendLF = !isFALSE(attr(verb, "appendLF")))
  } else NULL
}


#' @details
#' \code{has_cv_fits()} is a generic auxiliary function that discerns whether a cv contains the
#' cross-validation fits or not.
#' @rdname modeltuner-internal
#' @export
has_cv_fits <- function(x) UseMethod("has_cv_fits")

#' @export
has_cv_fits.cv_simple <- function(x){
  out <- !is.null(x$fits)
  names(out) <- label(x$model)
  out
}
#' @export
has_cv_fits.cv <- function(x){
  if (is.null(x$fits)){
    out <- rep(FALSE, n_model(x))
  } else {
    out <- !vapply(x$fits, is.null, FUN.VALUE = logical(1))
  }
  names(out) <- label(x)
  out
}


# ------------------------------------------------------------------------------

# ==== Methods for class 'cv' ====

#' @rdname cv 
#' @inheritParams print.model
#' @export
print.cv <- function(x, what = c("class", "formula", "weights"), show_metric = TRUE, 
                     abbreviate = TRUE, n = getOption("print_max_model"), 
                     width = getOption("width"), param = TRUE, ...){
  mult <- n_model(x) != 1
  cat("--- A ", dQuote("cv"), " object containing ", 
      n_model(x), " validated model", 
      if (mult) "s", " ---\n\n", 
      sep = "")
  print(x$folds)
  if (n_model(x)){
    cat("\nModel", if (mult) "s", ":\n", sep = "")
    prnt_multimodel_models(x$multimodel, n = n, what = what, abbreviate = abbreviate, 
                           width = width, param = param, 
                           metric = if (show_metric) x$metric)
    pref_iter <- extract_pref_iter(x)
    if (any(lengths(pref_iter)>0)){
      cat("\n")
      print(pref_iter, n = n)
    }
  }
  invisible(x)
}

empty_cv <- function(folds){
  structure(
    list(multimodel = empty_multimodel(), 
         folds = folds, 
         metric = character(0), 
         performance = list(), 
         predictions = list(), 
         timing = numeric(0), 
         extras = list()), 
    class = c("cv", "list"))
}


