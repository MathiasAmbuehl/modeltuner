
#' Selection of the best-performing model in a \dQuote{cv} object
#' 
#' @description
#' `tune()` picks the \dQuote{best} model from a set of models, that is the model with 
#' the smallest test error.
#' 
#' @param x An object of class \dQuote{\link{cv}}, or any other object that can be transformed to a `cv`.
#' @param \dots See \dQuote{Details} and \dQuote{Methods}.
#' @param nfold,folds Passed to \code{\link{make_folds}()}.
#' @param label A character string: The label of the output model. If `NULL`, the selected model's current label is kept. 
#' @param verbose Passed to \code{\link{cv}()}.
#' @inheritParams multimodel
#' @inheritParams cv_performance
#' @param use_original_args,force Arguments passed to \code{\link{fit}()}.
#' 
#' @details
#' The core method is `tune.cv(x)`, that selects the model from those included in `x` having lowest test error 
#' (in terms of the `metric`).
#' All other methods will run `tune.cv()` after some preprocessing steps -- see section \dQuote{Methods}.
#' 
#' For *iteratively fitted models* (\link{ifm}, classes \dQuote{\link{fm_xgb}} and \dQuote{\link{fm_glmnet}}), 
#' `tune(x)` without additional arguments returns the model corresponding to the *preferred iteration*, thus
#' tuning the parameter `nrounds` or `lambda`, respectively.
#' For other models, `tune(x)` simply returns `x`.
#' 
#' Note the different role of the \dQuote{...} arguments in different methods:
#' \itemize{
#' \item{In `tune.cv()` and `tune.multimodel()` they are passed to \code{\link{cv_performance}()}, 
#' allowing specification of `eval_weights`;}
#' \item{In `tune.model()` and `tune.default()`, they are passed to \code{\link{multimodel}()}. 
#' This allows expanding a selection of alternative parameterizations, of which that with the smallest test error 
#' is finally returned. 
#' Selecting the best-performing parameterization thus reduces to one simple line of code: \cr
#' `tune(mymodel, hyperparms=candidate_values)`}
#' }
#' 
#' @return All methods return an object of class \dQuote{model}, except `tune.default()`, 
#' which returns a fitted model of the same class as its input `x`.
#' 
#' @section Methods:
#' \itemize{
#' \item{`tune.cv`: Executes `cv_performance(x, ...)`, finds the model with minimal test error and extracts that model
#' (using \code{\link{extract_model}}).}
#' \item{`tune.multimodel(x, ...)` essentially runs `x %>% cv %>% tune(...)`.}
#' \item{`tune.model(x, ...)` essentially runs `x %>% multimodel(...) %>% cv %>% tune`.}
#' \item{`tune.default(x, ...)` is applied to a fitted model `x` and essentially runs 
#' `x %>% model %>% multimodel(...) %>% cv %>% tune %>% fit`.}
#' }
#' 
#' @seealso \code{\link{multimodel}}, \code{\link{cv}}, \code{\link{cv_performance}}, \code{\link{fit}}
#' 
#' @examples
#' options(cv_nfold = 1/3)  # accelerates cross-validations, see ?modeltuner_options
#' 
#' # Tune xgboost parameter:
#' model_xgb <- model("fm_xgb", Sepal.Length~., iris, class = "fm_xgb")
#' mm_xgb <- multimodel(model_xgb, max_depth = 1:5)
#' cv_xgb <- cv(mm_xgb)
#' plot(cv_performance(cv_xgb), 
#'      xvar = "max_depth", zeroline = FALSE)
#' 
#' # tune() automatically selects the best-performing model:
#' tuned1 <- tune(mm_xgb, max_depth = 1:5, label = "tuned_model_1")
#' tuned1
#' \donttest{
#' # Several tuning steps in a pipe:
#' tuned2 <- tuned1 |> 
#'   tune(learning_rate = c(0.1, 0.3, 1)) |> 
#'   tune(min_child_weight = c(5, 10, 20), label = "tuned_model_2")
#' fit(tuned2, eval = FALSE, use_original_args = TRUE)  # extract selected model
#' }
#' # Alternatively test a number of random parameterizations
#' tuned3 <- tune(tuned1, learning_rate = c(0.1, 0.3, 1), 
#'                min_child_weight = c(5, 10, 20), 
#'                label = "tuned_model_3")
#' fit(tuned3, eval = FALSE, use_original_args = TRUE)  # extract selected model
#' 
#' @name tune
#' @export
tune <- function(x, ...) UseMethod("tune")

#' tune.cv(), more detailed
#' 1) Runs cv_performance(x, metric)
#' 2) Determines the model with best test performance and extracts this model's cv output
#' 3) Applies extract_tuned to cv obj with one element, thereby possibly extracting 
#'    information on "internal" parameter tuning stored in cv.
#'    Example: 'iteration' in fm_xgb()

#' @rdname tune
#' @export
tune.cv <- function(x, metric = x$metric[1], label = NULL, ...){
  metric <- get_metric(metric, parent.frame(), x)
  perf <- cv_performance(x, metric = metric, ...)
  perf_test <- perf[[paste0("test_", names(metric))]]
  if (!any(is.finite(perf_test))){
    warning("None of the candidate models have finite test performance.")
    return(NULL)
  }
  best <- which.min(perf_test)
  out <- extract_model(x, best)
  if (!is.null(label)) label(out) <- label
  out
}

#' @rdname tune
#' @export
tune.multimodel <- function(x, nfold = getOption("cv_nfold"), folds = NULL, 
                            metric = NULL, label = NULL, ...){
  if (!is.null(metric)) metric <- get_metric(metric, parent.frame(), x)
  cvobj <- cv(x, nfold = nfold, folds = folds, metric = metric)
  tune(cvobj, label = label, ...)
}

#' @rdname tune
#' @export
tune.model <- function(x, ..., nfold = getOption("cv_nfold"), folds = NULL, metric = NULL,
                       expand = TRUE, max_n_model = getOption("expand_max_model"), 
                       label = NULL, verbose = getOption("cv_verbose")){
  if (!is.null(metric)) metric <- get_metric(metric, parent.frame(), x)
  dots <- list(...)
  lab <- label(x)
  x <- multimodel(x, ..., max_n_model = max_n_model,
                  simplify = FALSE)  
  nmodel <- length(x$models)
  if (verbose && nmodel>1){
    nparam <- min(prod(lengths(dots)), max_n_model)
    message("Tuning parameter", if (length(dots)>1) "s", " ", 
            paste(names(dots), collapse = ", "), 
            " (", nparam, " parameterizations)")
  }
  if (verbose){
    verbose_prefix <- structure(TRUE, final_msg = "", appendLF = FALSE)
  }
  cvobj <- cv(x, nfold = nfold, folds = folds, metric = metric, 
              verbose = if (verbose) verbose_prefix else FALSE, 
              max_n_model = max_n_model)
  if (is.null(metric)) metric <- cvobj$metric[1]
  if (verbose){
    best <- which.min(cv_performance(cvobj)[[paste0("test_", names(metric))]])
    bestparams <- cvobj$multimodel$param[best, , drop = FALSE]
    parnms <- colnames(bestparams)
    bestparams <- unlist(bestparams, recursive = FALSE)
    val <- as.character(bestparams, "[[", 1)
    if (any(is_absent_or_null <- sapply(bestparams, inherits, c("absent", "null"))))
      val[is_absent_or_null] <- sapply(bestparams[is_absent_or_null], format)
    if (verbose && nmodel>1){
      message("  -> ", paste0(parnms, " = ", val, collapse = ", "), "")
    }
  }
  tune(cvobj, label = label)
}

#' @rdname tune
#' @export
tune.default <- function(x, ..., nfold = getOption("cv_nfold"), folds = NULL, metric = NULL,
                         expand = TRUE, max_n_model = getOption("expand_max_model"), 
                         verbose = getOption("cv_verbose"), 
                         use_original_args = FALSE, force = FALSE){
  modelobj <- model(x, env = parent.frame())
  if (!is.null(metric)) metric <- get_metric(metric, parent.frame(), modelobj)  
  modelobj <- tune(modelobj, ..., nfold = nfold, folds = folds, metric = metric,
                   max_n_model = max_n_model, verbose = verbose)
  fit(modelobj, data = get_call(x)$data, use_original_args = use_original_args, 
      force = force, env = parent.frame())
}
  
