
#' Subset an object of class \dQuote{multimodel}, \dQuote{cv}, \dQuote{performance} or \dQuote{evaluation_log}
#' 
#' \code{\link[base]{subset}()} methods for classes \dQuote{\link{multimodel}}, 
#' \dQuote{\link{cv}} and \dQuote{performance}.
#' 
#' @param x \code{multimodel} or \code{cv} or other object.
#' @param subset Selection of models: An integer vector or a logical vector of appropriate length, 
#' or a character vector of model `label`s.
#' @param \dots Not used.
#' 
#' @return Object of same class as `x`.
#' 
#' @examples
#' mm <- c(speed_lm = model(lm(dist ~ speed, cars)),
#'         speed_loess = model(loess(dist ~ speed, cars,
#'                                   control = loess.control(surface = "direct"))),
#'         speed_rpart = model(rpart::rpart(dist ~ speed, cars)))
#' # subset.multimodel:
#' subset(mm, 1:2)
#' # subset.cv:
#' cv_mm <- cv(mm)
#' subset(cv_mm, 3:2)
#' # subset.performance:
#' cv_perf <- cv_performance(cv_mm)
#' subset(cv_perf, c(1, 3))
#' 
#' 
#' @seealso \code{\link{extract_model}} (in particular examples), 
#' \code{\link{label}}, \code{\link{sort_models}}
#' 
#' @name subset
NULL

#' @rdname subset
#' @export
subset.multimodel <- function(x, subset = TRUE, ...){
  lbl <- label(x)
  chk_subset_arg(subset, lbl)
  if (is.character(subset)) subset <- match(subset, lbl)
  x$models <- x$models[subset]
  if (!is.null(x$param))
    x$param <- structure(x$param[subset, , drop = FALSE], 
                         class = class(x$param))
  x 
}

chk_subset_arg <- function(subset, lbls, exactly_one = FALSE){
  if (any(is.na(subset)))
    stop("NA's are not allowed in argument ", sQuote("subset"), call. = FALSE)
  ok <- switch(
    mode(subset),
    numeric = all(subset %in% -length(lbls):length(lbls)),
    character = all(subset %in% lbls),
    logical = length(subset) %in% c(1, length(lbls)), 
    FALSE)
  if (ok && is.numeric(subset) && all(c(-1L, 1L) %in% sign(subset))) ok[] <- FALSE
  if (!all(ok) && !exactly_one)
    stop("Invalid selection of models (argument ", sQuote("subset"), ").", 
         call. = FALSE)
  if (exactly_one){
    ok <- ok & switch(
      mode(subset), 
      logical = sum(subset) == 1,
      length(subset) == 1)
    if (!all(ok)) stop("Invalid selection of one model (argument ", sQuote("subset"), ").", call. = FALSE)
  }
  invisible()
}


#' @rdname subset
#' @export
subset.cv <- function(x, subset = TRUE, ...){
  lbl <- label(x)
  chk_subset_arg(subset, lbl)
  if (is.character(subset)) subset <- match(subset, lbl)
  x$multimodel <- subset(x$multimodel, subset)
  if (!is.null(x$fits)){
    x$fits <- x$fits[subset]
  }
  x$metric <- x$metric[subset]
  x$performance <- x$performance[subset]
  x$predictions <- x$predictions[subset]
  x$timing <- x$timing[subset]
  x$extras <- x$extras[subset]
  x
}

#' @rdname subset
#' @export
subset.performance <- function (x, subset = TRUE, ...){
  lbl <- label(x)
  chk_subset_arg(subset, lbl)
  if (is.character(subset)) subset <- match(subset, lbl)
  x[subset, , drop = FALSE]
}

#' @rdname subset
#' @export
subset.evaluation_log <- function (x, subset = TRUE, ...){
  lbl <- label(x)
  chk_subset_arg(subset, lbl)
  if (is.character(subset)) subset <- match(subset, lbl)
  out <- x[subset]
  attrs <- modifyList(attributes(x), list(names = names(out)))
  attributes(out) <- attrs
  out
}
