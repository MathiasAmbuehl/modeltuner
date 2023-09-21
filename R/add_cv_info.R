

#' @details
#' \code{add_cv_info()} is a generic auxiliary function adding info from cv to a model. Apply to model, returns model.
#' @rdname modeltuner-internal
#' @export
add_cv_info <- function(x, cvobj, ...) UseMethod("add_cv_info")

#' @export 
add_cv_info.model <- function(x, cvobj, ...) x  

#' @export 
add_cv_info.model_fm_xgb <- function(x, cvobj, ...){
  stopifnot(inherits(cvobj, "cv_simple"))
  pref_iter <- cvobj$extras$pref_iter[[1]]
  iter <- pref_iter$iter
  names(iter) <- pref_iter$label_suffix
  x$cv_info <- list(pref_iter = iter, nrounds = nrow(cvobj$extras$evaluation_log))
  x
}

#' @export 
add_cv_info.model_fm_glmnet <- function(x, cvobj, ...){
  stopifnot(inherits(cvobj, "cv_simple"))
  pref_iter <- cvobj$extras$pref_iter[[1]]
  iters <- pref_iter$iter
  names(iters) <- pref_iter$label_suffix
  lambda <- cvobj$extras$lambda
  x$cv_info <- list(iters = iters, lambda = lambda)
  x
}

# ------------------------------------------------------------------------------

# Print extras: nothing by default (but see fm_xgb, fm_glmnet)
#' @rdname modeltuner-internal
#' @export
prnt_cv_info <- function(x, ...) UseMethod("prnt_cv_info")

#' @export
prnt_cv_info.model <- function(x, ...) invisible()

#' @export
prnt_cv_info.model_fm_xgb <- function(x, ...){
  out <- ""
  if (!is.null(x$cv_info)){
    iters <- x$cv_info$pref_iter
    prnt <- list(paste0("iter=", iters))
    names(prnt) <- "Preferred iteration from cv"
    render_list(prnt, indent = "", distribute = TRUE)
  }
  invisible()
}

#' @export
prnt_cv_info.model_fm_glmnet <- function(x, ...){
  out <- ""
  if (!is.null(iters <- x$cv_info$iters)){
    prnt <- list(paste0("iter=", iters))
    names(prnt) <- "Preferred iteration from cv"
    render_list(prnt, indent = "", distribute = TRUE)
  }
  invisible()
}

