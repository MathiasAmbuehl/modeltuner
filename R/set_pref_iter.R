
#' Extract set, and expand preference criteria for a \dQuote{cv} object for iteratively fitted models
#' 
#' @description
#' `set_pref_iter()` modifies a model or cross-validated model (usually an iteratively fitted model/\link{ifm}) 
#' by altering its preferences with respect to iterations,
#' without re-fitting the model or re-running the cross-validation, respectively.
#' This is a generic function and its action varies depending on the class of the object being modified. 
#' See the \dQuote{Details} section below. 
#' 
#' `extract_pref_iter()` extracts the information on preferred iterations from a \dQuote{cv} object.
#' 
#' `expand_pref_iter()` converts a \dQuote{cv} object with multiple preferred iterations to a \dQuote{cv} object 
#' having several (identical) models, but different preferred iterations. 
#' 
#' 
#' @param x A cross-validated model, of class \dQuote{\link{cv}}, usually based on an *iteratively fitted model* (IFM, see \link{ifm}).
#' In `set_pref_iter()`, `x` can also be a \link{model} or a fitted model.
#' @param iter Usually an integer value. If `x` is a \dQuote{cv}, it can also be a preference criterion 
#' (see \code{\link{crit_iter}}) -- multiple criteria are not allowed.
#' If `x` is a \dQuote{model} and has cross-validation information, `iter` can be omitted and 
#' the preferred iteration from cross-validation will then be selected.
#' @param which Character, integer or logical vector specifying the cross-validated models to be modified or expanded.
#' @param label \code{\link{label}} of output object.
#' @param keep_all Logical: If `TRUE`, all preference criteria are kept, if `FALSE`, only the selected one.
#' `keep_all` is relevant only if there is more than one preference criterion.
#' @param lambda Vector lambda (of decreasing numeric values) to pass to \code{\link{glmnet}()}.
#' @param verbose Logical: Show information on modification of arguments in the model generating call?
#' @param warn Logical: Whether to issue a warning if the required information on preferred iterations is not available.
#' @param \dots Arguments passed to methods. 
#' 
#' @details
#' What `set_pref_iter()` does varies between classes:
#' \itemize{
#' \item{
#' If `x` is a *fitted model*, `iter` is a required argument and must be a positive integer. 
#' The `pref_iter` component in the fitted object is set equal to `iter`, and the `call` is adjusted.
#' The adjustment of the call consists in setting `pref_iter` and making more changes that vary between model classes.
#' If the actual model is a \dQuote{fm_xgb}, `nrounds` and `early_stopping_rounds` are adjusted, 
#' in case of a \dQuote{fm_glmnet}, the value of `gamma` is changed.
#' These changes are such that 
#' \itemize{ 
#' \item{`predict()` will return predictions from the `iter`th iteration and}
#' \item{execution of the adjusted call generates a model that shares this behavior and stops the fitting
#' process immediately after that iteration.}
#' }
#' Note that the model is *not* re-fitted, and that the \sQuote{fitted} model returned by `set_pref_iter()`
#' is in some sense improper, because the object is not identical with what would result from executing the `call` stored in that model.
#' However, the resulting `predict`ions would be the same.
#' }
#' \item{
#' If `x` is an object of class \dQuote{\link{model}}, its `call` element is adjusted, exactly as described above
#' for *fitted* models. If `x` has a model fit attached to it (i.e. if `has_fit(x)` returns `TRUE`), 
#' this fitted model is adjusted, too.
#' If `x` is a model but not an IFM, then it is returned unchanged.
#' }
#' \item{
#' If `x` is a cross-validated model (class \dQuote{\link{cv}}), the information about preferred iterations
#' (stored as a part of the component `extras`) is modified and the \dQuote{model} is adapted, too.
#' }
#' }
#' There is currently no method `set_pref_iter.multimodel()`.
#' 
#' @return
#' A modified object of the same class as `x`.
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
#' extract_pref_iter(cvm)
#' cv_performance(cvm)
#' plot(evaluation_log(cvm))
#' 
#' # set_pref_iter
#' cvm_last <- set_pref_iter(cvm, crit_last(), label = "xgb_last")
#' cv_performance(c(cvm, cvm_last))
#' 
#' # expand_pref_iter
#' cvm_expanded <- expand_pref_iter(cvm)
#' print(cvm_expanded, what = c("call"))
#' cv_performance(cvm_expanded)       
#' plot(evaluation_log(cvm_expanded))
#' 
#' @name set_pref_iter
NULL

#' @rdname set_pref_iter
#' @export
set_pref_iter <- function(x, iter, ...) UseMethod("set_pref_iter")


#' @export
set_pref_iter.fm_xgb <- function(x, iter, ...){
  stopifnot(is.numeric(iter), iter>0, iter %% 1 == 0)
  if (iter > x$booster$niter){
    iter <- x$booster$niter
    warning("pref_iter is larger than the number of iterations;\n",
            "  setting pref_iter=", iter, " instead.")
  }
  x$pref_iter <- iter
  x$call$pref_iter <- x$call$nrounds <- iter
  x$call["early_stopping_rounds"] <- list(NULL)
  x
}

#' @export
set_pref_iter.fm_glmnet <- function(x, iter, ...){
  stopifnot(is.numeric(iter), iter>0, iter %% 1 == 0)
  lambda <- x$fit$lambda
  if (iter > length(lambda)){
    iter <- length(lambda)
    warning("pref_iter is larger than the number of iterations;\n",
            "  setting pref_iter=", iter, " instead.")
  }
  x$pref_iter <- iter
  x$call$lambda <- head(lambda, iter)
  x
}

# ------------------------------------------------------------------------------

#' @rdname set_pref_iter
#' @export
set_pref_iter.model <- function(x, ...){
  x
}

#' @rdname set_pref_iter
#' @export
set_pref_iter.model_fm_xgb <- function(x, iter, verbose = TRUE, warn = TRUE, ...){
  # insert nrounds according to arg 'iter'
  if (missing(iter) && is.integer(x$cv_info$pref_iter)){
    # case 1: use cv_info
    iter <- as.integer(x$cv_info$pref_iter[[1]])
  } else if (!missing(iter) && is.numeric(iter) && iter > 0 && iter %% 1 == 0){
    # case 2: valid iter specified 
    iter <- min(iter, x$cv_info$nrounds)
  } else if (missing(iter)){
    # case 3: no change
    if (warn) warning("No information on 'pref_iter' - returning unchanged model.")
    return(x)
  } else stop("Invalid specification of 'iter'.")
  x <- update(x, pref_iter = iter, nrounds = iter, early_stopping_rounds = null(),  
              ignore_changes_in_arg = c("pref_iter", "nrounds", "early_stopping_rounds")) 
  if (has_fit(x)){
    x$fit <- set_pref_iter(x$fit, iter = iter)
  }
  if (verbose){
    message("set_pref_iter(), model ", sQuote(label(x)), ", modifications made in call:",
            "\n  pref_iter=", iter, ", nrounds=", iter, ", early_stopping_rounds=NULL")
  }
  x
}

#' @rdname set_pref_iter
#' @export
set_pref_iter.model_fm_glmnet <- function(x, iter, lambda = NULL, 
                                          verbose = TRUE, warn = TRUE, ...){
  has_cv_info <- !is.null(x$cv_info)
  if (!is.null(lambda)){
    # case 1: explicit lambda
    iter <- length(lambda)
  } else if (!missing(iter) && (has_cv_info || has_fit(x))){
    # case 2: iter specified and lambda available
    lambda <- if (has_cv_info) x$cv_info$lambda else x$fit$fit$lambda
    iter <- min(iter, length(lambda))
  } else if (has_cv_info){
    # case 3: use cv_info
    lambda <- x$cv_info$lambda
    iter <- x$cv_info$iters[1]
  } else {
    # case 4: no change
    if (warn) warning("Cannot set parameter - not enough information.")
    return(x) 
  }
  lambda_new <- head(lambda, iter[[1]])
  x <- update(x, pref_iter = iter[[1]], lambda = lambda_new, 
              ignore_changes_in_arg = c("pref_iter", "lambda"))

  if (verbose){
    message("set_pref_iter(), model ", sQuote(label(x)), ": setting parameter in call:",
            "\n  pref_iter=", iter, 
            ", ", prt_lambda_compact(lambda_new))
  }
  x
}

prt_lambda_compact <- function(x, ntop = 1, digits = 3){
  ll <- length(x)
  ntop <- min(ntop, ll)
  c("lambda = c(", paste0(format(x[1:ntop], digits = digits), collapse = ", "),
    if (ll==ntop+1) ", ",
    if (ll>ntop+1) ", ..., ",
    if (ll>ntop) format(x[[ll]], digits = 3),
    ")",
    if (ll>ntop+1) paste0(" [length ", ll, "]"))
}

# ------------------------------------------------------------------------------

#' @export
set_pref_iter.cv_simple <- function(x, iter, label = x$model$label, 
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

#' @rdname set_pref_iter
#' @export
set_pref_iter.cv <- function(x, iter, which = label.cv(x), label = label.cv(x), 
                             keep_all = TRUE, ...){
  if (is.numeric(iter) && length(iter)>0 && all(iter %% 1 == 0)){
    iter <- crit_iter(iter)
  } 
  if (inherits(iter, "def_crit_list")){
    stop("set_pref_iter.cv() does not accept multiple criteria in ", sQuote("iter"), ".")
  } else if (!inherits(iter, "def_crit")){
    stop("Invalid choice of 'iter'.") 
  }
  if(n_model(x) == 1){
    out_simple <- set_pref_iter(extract(x, 1), iter, keep_all = keep_all, ...)
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
      outlist[[i]] <- set_pref_iter(extract(x, i), iter, keep_all = keep_all, ...)
    } else {
      outlist[[i]] <- extract(x, i)
    }
  }
  out <- cvlist2cv(outlist, x$multimodel, x$folds, keep_fits = FALSE, tm = x$timing)
  out$fits <- x$fits
  label(out) <- label
  out
}

# --------------------------------------

#' @rdname set_pref_iter
#' @export
extract_pref_iter <- function(x, ...) UseMethod("extract_pref_iter")

#' @export
extract_pref_iter.cv <- function(x, compact = TRUE, ...){
  pref_it <- lapply(x$extras, "[[", "pref_iter")
  if (!compact){
    pref_it <- lapply(
      pref_it, 
      function(l) setNames(l, vapply(l, "[[", "label_suffix", FUN.VALUE = character(1)))
    )
    names(pref_it) <- label(x)
    class(pref_it) <- c("extracted_pref_iter_detailed", class(pref_it))
    return(pref_it)
  }
  out <- lapply(pref_it, 
    function(s){
      iters <- sapply(s, "[[", "iter")
      names(iters) <- vapply(s, "[[", "label_suffix", FUN.VALUE = character(1))
      iters
    }
  )
  names(out) <- label(x)
  class(out) <- c("extracted_pref_iter_compact", "list")
  out
}

#' @export
print.extracted_pref_iter_compact <- function(x, n = getOption("print_max_row"), ...){
  cat("Preferred iterations:\n")
  x0 <- x
  names(x) <- paste0("model ", sQuote(names(x)))
  has_pref_iter <- lengths(x) > 0
  x[!has_pref_iter] <- 
    lapply(x[!has_pref_iter], function(e) "no iterations" )
  x[has_pref_iter] <- lapply(
    x[has_pref_iter], 
    function(e) paste0(names(e), " (iter=", e, ")"))
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

#' @rdname set_pref_iter
#' @export
expand_pref_iter <- function(x, iter = NULL, which = label.cv(x)){
  stopifnot(inherits(x, "cv"))
  if (n_model(x) == 1){
    if (lengths(extract_pref_iter(x)) == 0) return(x)
    if (is.null(iter)){
      iter <- do.call(c, lapply(x$extras[[1]]$pref_iter, function(e) e$crit))
    }
    if (inherits(iter, "def_crit")) iter <- list(iter)
    out <- vector("list", length(iter))
    for (i in seq_along(out)){
      it <- iter[[i]]
      out[[i]] <- set_pref_iter(x, it, label = paste0(label(x), "_", it$suffix), 
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
      outlist[[i]] <- expand_pref_iter(subset(x, i), iter)
    } else {
      outlist[[i]] <- subset(x, i)
    }
  }
  out <- do.call(c, outlist)
  out
}

