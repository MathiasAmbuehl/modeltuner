
#' Reorder models in an object of class \dQuote{multimodel}, \dQuote{cv}, \dQuote{performance} or \dQuote{evaluation_log}
#' 
#' @description
#' Models are reordered according to argument \code{order}.
#' If `order` is shorter than the number of models, models specified in \code{order} will appear 
#' before those not mentioned and the other will be appended.
#' 
#' The methods for classes \dQuote{cv} and \dQuote{performance}
#' can be sorted in order of model performance using the argument `by`.
#' 
#' @param x \code{multimodel} or \code{cv}
#' @param order A vector of model \code{\link{label}}s or integer indices.
#' If not complete, the remaining elements will be appended. 
#' Example: If there are 4 models, and `order=c(4, 2)`, output will have units in order `c(4,2,1,3)`.
#' @param ... Currently not used.
#' @param by Sort by the value of a column of the performance table
#' (only methods for classes \dQuote{cv} and \dQuote{performance}: 
#' Character string specifying a column in the table.
#' Partial matching is accepted, such that \code{"test"} is sufficient to sort by
#' test error for any \code{metric}.
#' @param decreasing Logical: Sort in decreasing order of the \code{by} variable? 
#' 
#' @return `sort_models()` returns its input `x` with rearranged models.
#' 
#' @seealso \code{\link[modeltuner]{subset}}
#' 
#' @examples
#' mm <- models(
#'   intercept = lm(Sepal.Length ~ 1 , iris),
#'   linear = lm(Sepal.Length ~ . , iris), 
#'   full = lm(Sepal.Length ~ .^2 , iris))
#' print(mm, what = "formula")
#' 
#' sort_models(mm, 3:1)
#' sort_models(mm, c(3:2))
#' sort_models(mm, c("full", "linear"))
#' 
#' # Sort by test performance
#' cvperf <- cv_performance(mm)
#' sort_models(cvperf, by = "test")
#' 
#' @name sort_models
#' @export
sort_models <- function(x, ...) UseMethod("sort_models")

#' @rdname sort_models
#' @export
sort_models.model <- function(x, ...) x
  
#' @rdname sort_models
#' @export
sort_models.multimodel <- function(x, order, ...){
  nms <- label(x)
  chk_subset_arg(order, nms)
  if (is.character(order)){
    order <- match(order, nms, NA)
  }
  subset(x, union(order, seq_along(nms)))
}

#' @rdname sort_models
#' @export
sort_models.cv <- function(x, order, by = NULL, decreasing = FALSE, ...){
  if (!missing(order) && !missing(by)){
    warning("You specified both ", sQuote("order"), " and ", sQuote("by"), ". ", 
            sQuote("by"), " is ignored.")
    by <- NULL
  }
  nms <- label(x)
  if (!missing(order)) chk_subset_arg(order, nms)
  if (!missing(order) && is.character(order)){
    order <- match(order, nms, NA)
  } else if (!is.null(by)){
    perf <- cv_performance(x, ...)
    by <- pmatch(by, colnames(perf))
    order <- do.call(base::order, list(perf[[by]], decreasing = decreasing))
  }
  subset(x, union(order, seq_along(nms)))
}

#' @rdname sort_models
#' @export
sort_models.performance <- function(x, order, by = NULL, decreasing = FALSE, ...){
  if (!missing(order) && !missing(by)){
    warning("You specified both ", sQuote("order"), " and ", sQuote("by"), ". ", 
            sQuote("by"), " is ignored.")
    by <- NULL
  }
  nms <- label(x)
  if (!missing(order)) chk_subset_arg(order, nms)
  if (!missing(order) && is.character(order)){
    order <- match(order, nms, NA)
  } else if (!is.null(by)){
    by <- pmatch(by, colnames(x))
    order <- do.call(base::order, list(x[[by]], decreasing = decreasing))
  }
  subset(x, union(order, seq_along(nms)))
}


#' @rdname sort_models
#' @export
sort_models.evaluation_log <- function(x, order, ...){
  nms <- label(x)
  chk_subset_arg(order, nms)
  if (is.character(order)){
    order <- match(order, nms, NA)
  }
  subset(x, union(order, seq_along(nms)))
}

