
#' Preference criteria for iteratively fitted models
#' 
#' @description 
#' These functions are passed to \code{\link{cv}(model, ...)} via the argument `iter` 
#' in case of an iteratively fitted model (see \link{ifm}). 
#' 
#' By default, `iter=crit_min()`.
#' \itemize{
#' \item{`crit_min()` selects the iteration with minimal test error.}
#' \item{`crit_last()` selects the last iteration.}
#' \item{`crit_first()` selects the first iteration.}
#' \item{`crit_iter(iter)` selects  `iter`th iteration.}
#' \item{`crit_se(factor)` selects the iteration with minimal test error among those where 
#' the test error does not exceed the minimal test error by more than `factor` standard errors.}
#' \item{`crit_overfit(ratio)` selects the iteration with minimal test error among those where
#' the ratio of training and test error does not fall below `ratio`.}
#' \item{`crit_list(...)` combines several criteria, as in `crit_list(crit_min(), crit_se())`.
#' Writing `c(...)` with `...` being a number of criteria is equivalent with `crit_list(...)`.}
#' }
#' 
#' @param label_suffix Suffix used to create label.
#' @param iter Number of iteration
#' @param factor Factor applied to standard error.
#' @param ratio Ratio of training and test error.
#' @param warn Logical: Whether to warn in case of an invalid input.
#' @param \dots Enumeration of criteria.
#' 
#' @return These functions return an object inheriting of class \dQuote{def_crit}.
#' 
#' @examples
#' crit_min()
#' crit_last()
#' crit_iter(20)
#' crit_iter(c(10, 20)) 
#' crit_se(2)
#' crit_overfit()
#' crit_overfit(c(1, .9, .8))
#' 
#' # combine criteria with either crit_list() or c():
#' crit_list(crit_first(), crit_last())
#' c(crit_first(), crit_last())    # the same
#' 
#' @seealso \link{ifm}, \code{\link{extract_pref_iter}}, \code{\link{set_pref_iter}}
#' 
#' @name crit_iter 
NULL

# definition of criteria ====
#' @rdname crit_iter
# selects the iteration with minimal test error.
#' @export
crit_min <- function(label_suffix = "min"){
  structure(list(label_suffix = label_suffix), 
            class = c("def_crit_min", "def_crit"))
}
#' @export
print.def_crit_min <- function(x, ..., header = TRUE){
  if (header)
    cat("Preference criterion for an iteratively fitted model:\n")
  cat("  criterion:     ", "crit_min()", "\n",
      "  label suffix:  ", dQuote(x$label_suffix), "\n",
      "  Selects the iteration with minimal test error.\n", 
      sep = "")
  invisible(x)
}

#' @rdname crit_iter
# selects the last iteration.
#' @export
crit_last <- function(label_suffix = "last"){
  structure(list(label_suffix = label_suffix), 
            class = c("def_crit_last", "def_crit"))
}
#' @export
print.def_crit_last <- function(x, ..., header = TRUE){
  if (header)
    cat("Preference criterion for an iteratively fitted model:\n")
  cat("  criterion:     ", "crit_last()", "\n",
      "  label suffix:  ", dQuote(x$label_suffix), "\n",
      "  Selects the last iteration.\n", 
      sep = "")
  invisible(x)
}
    
#' @rdname crit_iter
# selects the first iteration.
#' @export
crit_first <- function(label_suffix = "first"){
  structure(list(label_suffix = label_suffix), 
            class = c("def_crit_first", "def_crit"))
}
#' @export
print.def_crit_first <- function(x, ..., header = TRUE){
  if (header)
    cat("Preference criterion for an iteratively fitted model:\n")
  cat("  criterion:     ", "crit_first()", "\n",
      "  label suffix:  ", dQuote(x$label_suffix), "\n",
      "  Selects the first iteration.\n", 
      sep = "")
  invisible(x)
}

#' @rdname crit_iter
# selects  `iter`th iteration., 
#' @export
crit_iter <- function(iter, label_suffix = paste0("iter", iter)){
  if (length(iter) > 1){
    out <- mapply(crit_iter, iter, label_suffix = label_suffix, 
                  SIMPLIFY = FALSE)
    out <- flatten_crit(out)
    return(out)
  }
  structure(list(iter = iter, label_suffix = label_suffix), 
            class = c("def_crit_iter", if (length(iter) == 1) "def_crit"))
}
#' @export
print.def_crit_iter <- function(x, ..., header = TRUE){
  if (header)
    cat("Preference criterion for an iteratively fitted model:\n")
  cat("  criterion:     ", "crit_iter(", x$iter, ")\n",
      "  label suffix:  ", dQuote(x$label_suffix), "\n",
      "  Selects iteration number min{", x$iter, ", # of iterations}.\n", 
      sep = "")
  invisible(x)
}

#' @rdname crit_iter
# selects the iteration with minimal test error among those where the test error 
# does not exceed the minimal test error by more than `factor` standard errors.
#' @export
crit_se <- function(factor = 1, label_suffix = paste0(factor, "se"), warn = TRUE){
  if (warn && any(factor<0)){
    warning("crit_se() expects factor>=0") 
    factor <- factor[factor>0]
  }
  if (length(factor) == 0){
    cl <- paste0(deparse(match.call()), collapse = "")
    stop("Invalid criterion: '", cl, "'.")
  }  
  if (length(factor) > 1){
    out <- mapply(crit_se, factor, label_suffix = label_suffix, 
                  MoreArgs = list(warn = FALSE), SIMPLIFY = FALSE)
    out <- flatten_crit(out)
    return(out)
  }
  structure(list(factor = factor, 
                 label_suffix = label_suffix), 
            class = c("def_crit_se", "def_crit"))
}
#' @export
print.def_crit_se <- function(x, ..., header = TRUE){
  if (header)
    cat("Preference criterion for an iteratively fitted model:\n")
  cat("  criterion:     ", "crit_se(", x$factor, ")\n",
      "  label suffix:  ", dQuote(x$label_suffix), "\n",
      "  Selects the first iteration where test error does not exceed\n",
      "    the minimal test error by more than ", x$factor, " standard error", 
      if (x$factor!=1)"s", ".\n", 
      sep = "")
  invisible(x)
}

#' @rdname crit_iter
# selects the iteration with minimal test error among those where 
# the ratio of training and test error does not fall below `ratio`.
#' @export
crit_overfit <- function(ratio = 0.9, label_suffix = paste0("overfit", ratio), warn = TRUE){
  if (warn && any(ratio>1)){
    warning("crit_overfit() expects ratio<=1") 
    ratio <- ratio[ratio<1]
  }  
  if (length(ratio) == 0){
    cl <- paste0(deparse(match.call()), collapse = "")
    stop("Invalid criterion: '", cl, "'.")
  }
  if (length(ratio) > 1){
    out <- mapply(crit_overfit, ratio, label_suffix = label_suffix, 
                  MoreArgs = list(warn = FALSE), SIMPLIFY = FALSE)
    out <- flatten_crit(out)
    return(out)
  }
  structure(list(ratio = ratio, 
                 label_suffix = label_suffix), 
            class = c("def_crit_overfit", "def_crit"))
}
#' @export
print.def_crit_overfit <- function(x, ..., header = TRUE){
  if (header)
    cat("Preference criterion for an iteratively fitted model:\n")
  cat("  criterion:     ", "crit_overfit(", x$ratio, ")\n",
      "  label suffix:  ", dQuote(x$label_suffix), "\n",
      "  Selects the iteration with minimal test error among those where\n",
      "  the ratio of training and test error does not fall below ", x$ratio, ".\n",
      sep = "")
  invisible(x)
}

# ------------------------------------------------------------------------------

flatten_crit <- function(x){
  is_def_crit <- sapply(x, inherits, "def_crit")
  if (all(is_def_crit)){
    out <- x
  } else{ 
    ll <- ifelse(vapply(x, inherits, "def_crit_list", FUN.VALUE = logical(1)), 
                 lengths(x), 1)
    rr <- rep(seq_along(x), ll)
    out <- vector("list", sum(ll))
    for (i in seq_along(x)) out[rr == i] <- if (is_def_crit[[i]]) x[i] else x[[i]]
  } 
  class(out) <- c("def_crit_list", "list")
  out
}

#' @rdname crit_iter
# combines several criteria, as in `crit_list(crit_min(), crit_se())`. You can write `c(...)` instead of `crit_list(...)`.
#' @export
crit_list <- function(...){
  flatten_crit(list(...))
}

#' @export
print.def_crit_list <- function(x, ...){
  l <- length(x)
  cat(l, "preference criteria for an iteratively fitted model:\n")
  for (i in seq_len(l)){
    cat("\n")
    print(x[[i]], ..., header = FALSE)
  }
  invisible(x)
}

#' @export
c.def_crit <- crit_list

#' @export
c.def_crit_list <- crit_list

# ------------------------------------------------------------------------------

#' @details
#' \code{eval_crit()} is a generic auxiliary function evaluating preference criteria for a given \link{ifm}s.
#' @rdname modeltuner-internal
#' @export
eval_crit <- function(crit, x, ...){
  UseMethod("eval_crit")
}

#' @export
eval_crit.def_crit_min <- function(crit, x, metric = x$metric, ...){
  x <- prep_x_for_eval_crit(x, metric = metric)
  if (is.null(x$eval_log)) return(NULL)
  out <- prep_crit(x, crit)
  errors <- x$eval_log[[paste0(out$err_type, "_", names(metric))]]
  if (any(is.finite(errors))){
    out$iter <- as.integer(which.min(errors))
    out$error <- errors[[out$iter]]
  }
  class(out) <- "evaluated_crit"
  out <- list(out)
  out  
}

#' @export
eval_crit.def_crit_last <- function(crit, x, metric = x$metric, ...){
  x <- prep_x_for_eval_crit(x, metric = metric)
  if (is.null(x$eval_log)) return(NULL)
  out <- prep_crit(x, crit)
  errors <- x$eval_log[[paste0(out$err_type, "_", names(metric))]]
  out$iter <- length(errors)
  out$error <- errors[[out$iter]]
  class(out) <- "evaluated_crit"
  out <- list(out)
  out
}

#' @export
eval_crit.def_crit_first <- function(crit, x, metric = x$metric, ...){
  x <- prep_x_for_eval_crit(x, metric = metric)
  if (is.null(x$eval_log)) return(NULL)
  out <- prep_crit(x, crit)
  errors <- x$eval_log[[paste0(out$err_type, "_", names(metric))]]
  if (length(errors)){
    out$iter <- min(1L, length(errors))
    out$error <- errors[[out$iter]]
  }
  class(out) <- "evaluated_crit"
  out <- list(out)
  out
}

#' @export
eval_crit.def_crit_iter <- function(crit, x, metric = x$metric, ...){
  x <- prep_x_for_eval_crit(x, metric = metric)
  if (is.null(x$eval_log)) return(NULL)
  out <- prep_crit(x, crit)
  errors <- x$eval_log[[paste0(out$err_type, "_", names(metric))]]
  if (length(errors)){
    out$iter <- as.integer(min(crit$iter, nrow(x$eval_log)))
    out$error <- errors[[out$iter]]
  }
  class(out) <- "evaluated_crit"
  out <- list(out)
  out
}

#' @export
eval_crit.def_crit_se <- function(crit, x, metric = x$metric, ...){
  x <- prep_x_for_eval_crit(x, metric = metric)
  if (is.null(x$eval_log)) return(NULL)
  out <- prep_crit(x, crit)
  errors <- x$eval_log[[paste0(out$err_type, "_", names(metric))]]
  se <- x$eval_log[[paste0("se_", out$err_type, "_", names(metric))]]
  whmin <- which.min(errors)
  sel <- errors <= (errors + crit$factor * se)[whmin]
  if (any(sel)){
    out$iter <- as.integer(which(sel)[which.min(errors[sel])])
    out$error <- errors[[out$iter]]
  }
  class(out) <- "evaluated_crit"
  out <- list(out)
  out
}

#' @export
eval_crit.def_crit_overfit <- function(crit, x, metric = x$metric, ...){
  x <- prep_x_for_eval_crit(x, metric = metric)
  if (is.null(x$eval_log)) return(NULL)
  out <- prep_crit(x, crit)
  test_err <- x$eval_log[[paste0("test_", names(metric))]]
  train_err <- x$eval_log[[paste0("train_", names(metric))]]
  r <- train_err/test_err
  sel <- r >= crit$ratio
  if (any(sel)){
    out$iter <- as.integer(which(sel)[which.min(test_err[sel])])
    out$error <- test_err[[out$iter]]
  }
  class(out) <- "evaluated_crit"
  out <- list(out)
  out
}

# ------------------------------------------------------------------------------

# Two aux functions
prep_crit <- function(x, crit){  
  err_type <- if (paste0("test_", names(x$metric)) %in% names(x$eval_log)){
    "test"
  } else if (paste0("train_", names(x$metric)) %in% names(x$eval_log)){
    "train"
  } else NA_character_
  out <- list(iter = NA_integer_, 
              error = NA_real_, 
              metric = x$metric,
              err_type = err_type,
              label = paste0(x$label, "_", crit$label_suffix), 
              label_base = x$label, 
              label_suffix = crit$label_suffix,
              crit = crit)
  class(out) <- "evaluated_crit"
  out
}

# x represents a single ifm, 
# must be either class 'cv', 'cv_simple' or 'evaluation_log' 
# or a list of eval_log, metric and label
prep_x_for_eval_crit <- function(x, metric = x$metric){
  if (inherits(x, "cv")){
    stopifnot(n_model(x) == 1)
    x <- extract(x, 1) # cv -> cv_simple
  }
  if (inherits(x, "cv_simple")){
    x <- list(eval_log = x$extras$evaluation_log,
              metric = metric,
              label = x$model$label)
  }
  if (inherits(x, "evaluation_log")){
    lbl <- label(x)[1]
    x <- x[[1]][c("log", "metric")]
    names(x) <- c("eval_log", "metric")
    x$label <- lbl
  }
  x
}
# print method for class 'evaluated_crit'
# unique: either FALSE (not duplicate removal) 
# or TRUE (remove criteria with duplicated iteration)
# or "crit_iter" (remove duplicateds only among those based on crit_iter)
#' @export
eval_crit.list <- function(crit, x, ..., unique = "crit_iter", 
                           verbose = getOption("cv_verbose")){
  crit <- flatten_crit(crit)
  out <- lapply(crit, eval_crit, x, ...)
  out <- unname(unlist(out, recursive = FALSE))
  if (identical(unique, "crit_iter")){
    is_crit_iter <- vapply(crit, inherits, "def_crit_iter", 
                           FUN.VALUE = logical(1))
    dup0 <- duplicated(vapply(out[is_crit_iter], "[[", "iter", 
                              FUN.VALUE = integer(1)))
    dup <- rep(FALSE, length(out))
    dup[is_crit_iter][dup0] <- TRUE
    out <- out[!dup]
  } else if (unique){
    dup <- duplicated(vapply(out, "[[", "iter", FUN.VALUE = integer(1)))
    if (verbose && any(dup))
      message("Removing duplicated iterations: ", 
              paste0(names(out)[dup], collapse = ", "))
    out <- out[!dup]
  }
  out  
}


#' @export
print.evaluated_crit <- function(x, digits = 5, ...){
  cat("Evaluated ", sQuote("crit_iter"), 
      " criterion (class ", dQuote(class(x)), "):\n", 
      sep = "")
  prntlist <- list(
    "base model" = x$label_base,
    "model label" =  x$label, 
    metric = names(x$metric),
    "selected iteration" =  x$iter,
    error = paste0(names(x$metric), "=", format(x$error, digits = digits))
    )
  names(prntlist)[match("error", names(prntlist))] <- paste(x$err_type, "error")
  render_list(prntlist)
  invisible(x)  
}

# ------------------------------------------------------------------------------

if (F){

  crit_min()
  crit_last()
  crit_first()
  crit_iter(20)
  crit_se()
  crit_se(2)
  crit_se(0:2)
  crit_overfit()
  crit_overfit(c(1, .9, .8))
  
  crit_iter(c(10, 20))
  crit_list(crit_min(), crit_last())
  c(crit_min(), crit_iter(c(10, 20)), crit_last())
  c(crit_iter(c(10, 20)), crit_min(), crit_last())
  
  mycv <- cv(fm_xgb(Sepal.Length ~ ., iris))
  plot(evaluation_log(mycv), errorbars = TRUE) + ggplot2::ylim(0, 0.5)
  eval_crit(crit_min(), mycv)
  eval_crit(crit_last(), mycv)
  eval_crit(crit_first(), mycv)
  eval_crit(crit_iter(20), mycv)
  eval_crit(crit_se(), mycv)
  eval_crit(crit_se(2), mycv)
  eval_crit(crit_overfit(), mycv)

  eval_crit(c(crit_min(), crit_overfit(c(.9, .8))), 
            mycv)
  eval_crit(crit_list(crit_min(), crit_overfit(c(.9, .8))), 
            mycv)
  eval_crit(list(crit_min(), crit_overfit(c(.9, .8))), 
            mycv)

    
  eval_crit(crit_min(), mycv)
  eval_crit(crit_se(0), mycv)
  eval_crit(crit_overfit(Inf), mycv)
  
  str(extract(mycv, 1)$extras)
}
