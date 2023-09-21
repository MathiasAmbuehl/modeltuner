
#' Set, extract and expand preference criteria for a \dQuote{cv} object for iteratively fitted models
#' 
#' @description
#' `set_iter()` modifies a \dQuote{cv} object by altering its preferences with respect to iterations,
#' without re-running the cross-validation.
#' 
#' `extract_iter()` extracts the information on preferred iterations from a \dQuote{cv} object.
#' 
#' `expand_iter()` converts a \dQuote{cv} object with multiple preferred iterations to a \dQuote{cv} object 
#' having several (identical) models, but different preferred iterations. 
#' 
#' @param x A cross-validated model, of class \dQuote{\link{cv}}, based on an *iteratively fitted model* (IFM, see \link{ifm}).
#' In `set_iter()`, `x` can also be a \link{model} being based on an IFM.
#' @param iter If `x` is a \dQuote{cv}: A criterion (see \code{\link{crit_iter}}, multiple criteria not allowed).\cr
#' If `x` is a \dQuote{model}: An integer value; if model has cross-validation information, `iter` is not required and 
#' the preferred iteration will be selected; otherwise `iter` is required.
#' @param which Character, integer or logical vector specifying the cross-validated models to be modified or expanded.
#' @param label \code{\link{label}} of output object.
#' @param keep_all Logical: If `TRUE`, all preference criteria are kept, if `FALSE`, only the selected one
#' (only relevant if there is more than one preference criterion).
#' @param lambda Vector lambda (of decreasing numeric values) to pass to \code{\link{glmnet}()}.
#' @param verbose Logical: Show information on modified arguments?
#' @param warn Logical: Whether to issue a warning when information for parameter setting is not possible.
#' @param \dots Arguments passed to methods. 
#' 
#' @return
#' A modified object of the same class as `x`, a \dQuote{model} or \dQuote{cv} object.
#' 
#' @seealso \code{\link{ifm}}, \code{\link{fm_xgb}}, \code{\link{fm_glmnet}}, \code{\link{fit}}, 
#' \code{\link{crit_iter}}.
#' 
#' @examples
#' d <- simuldat(n = 5000)
#' m <- model("fm_xgb", Y ~ ., d, nrounds = 200, class = "fm_xgb", 
#'            label = "xgb")
#' cvm <- cv(m, iter = c(crit_min(), crit_last(), crit_overfit(.5)), 
#'           nfold = .3)
#' print(cvm, what = "call")           
#' extract_iter(cvm)
#' cv_performance(cvm)
#' plot(evaluation_log(cvm))
#' 
#' # set_iter
#' cvm_last <- set_iter(cvm, crit_last(), label = "xgb_last")
#' cv_performance(c(cvm, cvm_last))
#' 
#' # expand_iter
#' cvm_expanded <- expand_iter(cvm)
#' print(cvm_expanded, what = c("call"))
#' cv_performance(cvm_expanded)       
#' plot(evaluation_log(cvm_expanded))
#' 
#' @name set_iter
NULL

#' @rdname set_iter
#' @export
set_iter <- function(x, iter, ...) UseMethod("set_iter")

#' @export
set_iter.cv_simple <- function(x, iter, label = x$model$label, 
                               keep_all = TRUE, ...){
  pref_it <- x$extras$pref_iter
  if (is.null(pref_it)) return(x)
  evaluated_crit <- eval_crit(iter, x)[[1]]
  match_crit <- vapply(pref_it, function(x, y) identical(x$crit, y$crit), evaluated_crit, 
                       FUN.VALUE = logical(1))
  match_iter <- vapply(pref_it, function(x, y) identical(x$iter, y$iter), evaluated_crit, 
                       FUN.VALUE = logical(1))
  if (any(match_crit)){
    pos <- which(match_crit)[[1]]
    if (keep_all){
      reorder <- union(pos, seq_along(pref_it))
    } else {
      reorder <- pos
    }
    x$extras$pref_iter <- pref_it[reorder]
    x$predictions <- x$predictions[reorder]
    x$performance <- x$performance[reorder]
    return(x)
  } else {
    x$extras$pref_iter <- c(list(evaluated_crit), 
                           if (keep_all) x$extras$pref_iter)
    if (any(match_iter)){
      wh <- which(match_iter)[[1]]
      x$predictions <- c(list(x$predictions[[wh]]), if (keep_all) x$predictions)
    } else {
      x$predictions <- c(list(NA), if (keep_all) x$predictions)
    }
    it <- evaluated_crit$iter
    perf0 <- x$extras$evaluation_log[it, ]
    perf0$iteration <- perf0$iter
    perf0$iter <- NULL
    x$performance <- c(list(perf0), 
                       if (keep_all) x$performance)
  }
  x
}

#' @rdname set_iter
#' @export
set_iter.cv <- function(x, iter, which = label.cv(x), label = label.cv(x), 
                        keep_all = TRUE, ...){
  if (inherits(iter, "def_crit_list")){
    stop("set_iter.cv() does not accept multiple criteria in ", sQuote("iter"), ".")
  }
  stopifnot(inherits(iter, "def_crit")) 
  if(n_model(x) == 1){
    out_simple <- set_iter(extract(x, 1), iter, keep_all = keep_all, 
                              ...)
    out <- set_label(x, label)
    out$fits <- NULL
    out$predictions <- list(out_simple$predictions)
    out$performance <- list(out_simple$performance)
    out$extras <- list(out_simple$extras)
    class(out) <- c("cv", "list")
    return(out)
  }
  lbls <- label(x)
  if (!is.character(which)) which <- lbls[which]
  outlist <- vector("list", n_model(x))
  for (i in seq_along(outlist)){
    if (lbls[[i]] %in% which){
      outlist[[i]] <- set_iter(extract(x, i), iter, keep_all = keep_all, 
                                  ...)
    } else {
      outlist[[i]] <- subset(x, i)
    }
  }
  out <- cvlist2cv(outlist, x$multimodel, x$folds, keep_fits = FALSE, tm = x$timing)
  out$fits <- x$fits
  label(out) <- label
  out
}

# --------------------------------------

#' @rdname set_iter
#' @export
extract_iter <- function(x, ...) UseMethod("extract_iter")

#' @export
extract_iter.cv <- function(x, compact = TRUE, ...){
  pref_it <- lapply(x$extras, "[[", "pref_iter")
  if (!compact){
    names(pref_it) <- label(x)
    return(pref_it)
  }
  out <- lapply(pref_it, 
    function(s){
      iters <- sapply(s, "[[", "iter")
      suffixes <- vapply(s, "[[", "label_suffix", FUN.VALUE = character(1))
      if (length(iters)){
        paste0(suffixes, " (iter=", iters, ")")
      } else character(0)
    }
  )
  names(out) <- label(x)
  class(out) <- c("extract_iter", "list")
  out
}

#' @export
print.extract_iter <- function(x, n = getOption("print_max_row"), ...){
  cat("Selection criteria:\n")
  x0 <- x
  names(x) <- paste0("model ", sQuote(names(x)))
  has_pref_iter <- lengths(x) > 0
  x[!has_pref_iter] <- 
    lapply(x[!has_pref_iter], 
           function(e){e[[1]] <- "no iterations"; e})
  if (length(x) <= n){
    render_list(x, distribute = TRUE)
  } else {
    n_drop <- length(x)-n
    x <- head(x, n)
    render_list(x, distribute = TRUE)
    cat("... and ", n_drop, " line",
        if (n_drop>1) "s",
        " more.\n", sep = "")
  }
  invisible(x0)
}

# --------------------------------------

#' @rdname set_iter
#' @export
expand_iter <- function(x, iter = NULL, which = label.cv(x)){
  stopifnot(inherits(x, "cv"))
  if (n_model(x) == 1){
    if (lengths(extract_iter(x)) == 0) return(x)
    if (is.null(iter)){
      iter <- do.call(c, lapply(x$extras[[1]]$pref_iter, function(e) e$crit))
    }
    if (inherits(iter, "def_crit")) iter <- list(iter)
    out <- vector("list", length(iter))
    for (i in seq_along(out)){
      it <- iter[[i]]
      out[[i]] <- set_iter(x, it, label = paste0(label(x), "_", it$suffix), 
                           keep_all = FALSE)
    }
    out <- do.call(c, out)
    label(out) <- vapply(out$extras, function(e) e$pref_iter[[1]]$label, 
                         FUN.VALUE = character(1))
    return(out)
  }
  lbls <- label(x)
  if (!is.character(which)) which <- lbls[which]
  outlist <- vector("list", n_model(x))
  for (i in seq_along(outlist)){
    if (lbls[[i]] %in% which){
      outlist[[i]] <- expand_iter(subset(x, i), iter)
    } else {
      outlist[[i]] <- subset(x, i)
    }
  }
  out <- do.call(c, outlist)
  out
}


# ------------------------------------------------------------------------------

#' @rdname set_iter
#' @export
set_iter.model <- function(x, ...){
  x
}

#' @rdname set_iter
#' @export
set_iter.model_fm_xgb <- function(x, iter, verbose = TRUE, warn = TRUE, ...){
  nrounds <- unchanged()
  # insert nrounds according to arg 'iter'
  if (missing(iter) && is.integer(x$cv_info$pref_iter)){
    nrounds <- as.integer(x$cv_info$pref_iter[[1]])
  } else if (missing(iter)){
    verbose <- FALSE  # no message if there is no cv_info
  } else if (is.numeric(iter) && iter > 0 && iter %% 1 == 0){
    nrounds <- min(iter, x$cv_info$nrounds)
  }
  if (inherits(nrounds, "unchanged")){
    if (warn) warning("Cannot set parameter - check 'iter'.")
  } else {
    x <- update(x, nrounds = nrounds, early_stopping_rounds = null(), ...)
    if (verbose){
      message("set_iter(), model ", sQuote(label(x)), ": setting parameters in call:",
              "\n  nrounds=", nrounds, ", early_stopping_rounds=NULL")
    }
  }
  x
}

#' @rdname set_iter
#' @export
set_iter.model_fm_glmnet <- function(x, iter, lambda = NULL, 
                                     verbose = TRUE, warn = TRUE, ...){
  it <- unchanged()
  if (is.null(lambda)){
    # insert iter (unless lambda has not been provided explicitly)
    if (missing(iter) && is.integer(x$cv_info$iters) && length(x$cv_info$iters)){
      it <-  x$cv_info$iters[[1]]
    } else if (missing(iter)){
      verbose <- FALSE  # no message if there is no cv_info
    } else if (is.numeric(iter) && iter > 0 && iter %% 1 == 0){
      it <- iter
    }
  }
  if (inherits(it, "unchanged")){
    if (warn) warning("Cannot set parameter - check 'iter'.")
  } else {
    lambda <- head(x$cv_info$lambda, it)
    x <- update(x, lambda = lambda, ...)
    if (verbose){
      l <- length(lambda)
      message("set_iter(), model ", sQuote(label(x)), ": setting parameter in call:",
              "\n  lambda=c(", format(lambda[[1]], digits = 3), 
              if (l>2) ", ..., ", if (l>1) format(lambda[[l]], digits = 3), ")", 
              if (l>2) paste0(" [length ", l, "]"))
    }
  }
  x
}
