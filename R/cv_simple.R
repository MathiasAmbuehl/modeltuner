
# cv_simple: cross-validate a single model

#' @details
#' \code{cv_simple()} is a generic function performing cross-validation for one model.
#' 
#' @rdname modeltuner-internal
#' @export
cv_simple <- function(x, folds, metric, iter, keep_fits, verbose, ...)
  UseMethod("cv_simple")


#' @export
cv_simple.model <- function(x, folds, metric = NULL, iter = getOption("cv_iter"), 
                            keep_fits = FALSE, verbose = TRUE, ...){
  nfold <- length(folds)
  # environment for execution of cv_call
  env <- c(x[c("data", "weights")], x$saved_objects, list(folds = folds))
  # Prepare call for cross-val
  cv_table <- get_cv_fits(x, env, verbose = verbose)
  cv_ok <- !vapply(cv_table$fit, inherits, "try-error", FUN.VALUE = logical(1))
  if (!all(cv_ok)){
    if (!isFALSE(verbose)){
      err_msg <- 
        paste0(
          "cv model ", dQuote(label(x)), ", fold ", which(!cv_ok), "/", nfold, ":", 
          paste0(rep(" ", 60), collapse = ""), "\n  ",
          lapply(cv_table$fit[!cv_ok], function(x) as.character(x)))
      message(err_msg, appendLF = FALSE)
    }
    warning("Cross-validation results may be invalid or incomplete for model ", 
            dQuote(label(x)), call. = FALSE)
  }
  # predictions
  predictions <- get_predictions(cv_table$fit, x, verbose = verbose)
  # metric
  if (is.null(metric)) metric <- default_metric(x)
  # output object
  out <- structure(
    list(model = x,
         folds = folds, 
         metric = metric, 
         predictions = predictions),
    class = c("cv_simple", "list"))
  # append fits if required
  if (keep_fits) out$fits <- cv_table$fit
  if (!isFALSE(verbose)){
    message("\r", rep(" ", nchar(verbose) + nchar(x$label) + 60), 
            "\r", appendLF = FALSE)    
  }
  out
}

# aux funs in cv_simple:
get_cv_fits <- function(model, env, verbose = TRUE){
  cv_call <- fit(model, eval = FALSE)
  folds <- env$folds
  if (!(length(folds) == 1 && length(folds[[1]]) == 0)){
    cv_call <- substitute_call(cv_call, 
      list(data = quote(data[-folds[[i]], , drop = FALSE]), 
           weights = quote(weights[-folds[[i]]])))
  }
  data <- model$data
  # create cv_table
  cv_table <- data.frame(i = seq_along(folds))
  nfold <- length(folds)
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
  for (i in seq_along(folds)) {
    env$i <- i
    cv_table$fit[[i]] <- try(eval(cv_call, env), silent = TRUE)
    if (verbose) pb$tick()
  }
  cv_table
}

get_predictions <- function(fits, model, verbose = TRUE){
  nfold <- length(fits)
  data <- model$data
  predfun <- model$predict_function
  out <- vector("list", nfold)  
  if (!isFALSE(verbose)){
    verbose_prefix <- if (is.character(verbose)) paste0(verbose, " ") else ""
    verbose <- TRUE
    pb <- modeltuner_progress_bar(
      prefix = paste0(verbose_prefix, dQuote(model$label), 
                      ": obtaining predictions "),
      total = nfold, 
      final = paste0(verbose_prefix, dQuote(model$label), ": Completing cv"), 
      show_after = 0.5)
  }
  for (i in seq_len(nfold)){
    out[[i]] <- as.vector(predfun(fits[[i]], data))
    if (verbose) pb$tick()
  }
  do.call(cbind, out)
}

# predict-method for class 'try-error':
# Returns NA vector of appropriate length in case of failed cv-fits
#' @export
`predict.try-error` <- function(object, newdata, ...){
  n <- nrow(newdata)
  out <- rep(NA, n)
  names(out) <- rownames(newdata)
  out
}


#' @importFrom progress progress_bar
modeltuner_progress_bar <- function(prefix, total = 1, clear = TRUE, show_after = 1, 
                                    final = ""){
  progress_bar$new(
    format = paste0(
      prefix, 
          ":current/:total (elapsed :elapsedfull, remaining ~:eta)"),
    total = total, clear = clear, show_after = show_after,
    callback = function(pb){
      if (pb$finished) message("\r", final, "\r", appendLF = FALSE)
    }
  )
}


get_metric <- function (metric, env = parent.frame(), obj, check = FALSE){
  if (is.null(metric) && !missing(obj)){
    metric <- default_metric(obj)
  }
  if (is.character(metric) && length(metric) == 1 && exists(metric, env)){
    metric <- structure(list(get(metric, env)), 
                        names = metric)
  } else if (is.character(metric)){
    metric <- structure(list(NULL), names = metric)
  }
  if (is.function(metric)){
    metric <- structure(list(metric), names = as.name(deparse(substitute(metric), width.cutoff = 20)[[1]]))
  }
  if (is.null(names(metric))) names(metric) <- "unnamed_metric"
  if (check){
    fn_metric <- metric[[1]]
    ok <- is.function(fn_metric) && all(c("actual", "predicted") %in% names(formals(fn_metric)))
    if (!ok) stop("No valid metric specified.")
  }
  metric
}

# print method -- for dev use
#' @export
print.cv_simple <- function(x, ...){
    message("...converting ", dQuote("cv_simple"), " object to ", dQuote("cv"), " for printing...")
    cvobj <- cvlist2cv(list(x), multimodel(x$model, simplify = FALSE), x$folds, keep_fits = FALSE, tm = x$timing)
    print(cvobj, ...)
}
