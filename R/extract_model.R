
#' @details
#' \code{extract()} is a generic extraction function used internally.
#' 
#' @rdname modeltuner-internal
#' @export
extract <- function(x, i) UseMethod("extract")

# extract a model from a multimodel
#' @export
extract.multimodel <- function(x, i){
  if (n_model(x) == 1 && missing(i)) i <- 1
  lbls <- label(x)
  chk_subset_arg(i, lbls, exactly_one = TRUE)
  if (is.character(i)) i <- match(i, lbls)
  if (is.logical(i)) i <- which(i)
  model <- x$models[[i]]
  model$label <- names(x$models)[[i]]
  nms <- union(names(model), c("call", "formula", "data"))
  model <- model[nms]
  class(model) <- c(paste0("model_", model$class), "model")
  model
}

# extract a cv_simple from a cv
#' @export
extract.cv <- function(x, i){
  if (n_model(x) == 1 && missing(i)) i <- 1
  if (is.character(i)) i <- match(i, label(x))
  if (is.logical(i)) i <- which(i, useNames = FALSE)
  out <- list(model = extract(x$multimodel, i),
              folds = x$folds)
  if (has_cv_fits(x)[i]){
    out$fits <- x$fits[[i]]
  }
  out$metric <- x$metric[i]
  out["performance"] <- list(x$performance[[i]])
  out$predictions <- x$predictions[[i]]
  out$timing <- x$timing[i]
  out$extras <- x$extras[[i]]
  class(out) <- c("cv_simple", "list")
  out
}

# ------------------------------------------------------------------------------


#' Extraction of a model and multimodel 
#' 
#' @description
#' `extract_model()` extracts a model from a \dQuote{multimodel} or \dQuote{cv} object.
#' 
#' `extract_multimodel()` extracts the multimodel from a \dQuote{cv} object.
#' 
#' @param x A \dQuote{multimodel} or \dQuote{cv} object.
#' @param which Selection of one model: An integer value  
#' or a logical vector of length `n_model(x)` having exactly one `TRUE`, 
#' or a character value (selection by name, a model's `label`).
#' If `n_model(x)==1`, the only model is selected by default. 
#' @param use_cv_info Logical: Whether to set the preferred iteration according to 
#' results from the cross-validation (if these are present).
#' Relevant only for *iteratively fitted models* (\link{ifm}).
#' @param \dots Arguments passed from or to methods.
#' 
#' @return
#' `extract_model()` returns a \code{\link{model}}, 
#' `extract_multimodel()` returns a \code{\link{multimodel}}.
#' 
#' @seealso \code{\link{multimodel}}, \code{\link{cv}}
#' 
#' @examples
#' mm_swiss <- c(model(lm(Fertility ~ Education, swiss)),
#'               model(lm(Fertility ~ Education + I(Education^2) , swiss)))
#' cv_swiss <- cv(mm_swiss)
#' 
#' extract_model(mm_swiss, 1) # a model
#' subset(mm_swiss, 1)        # a multimodel
#' 
#' extract_model(cv_swiss, 1)      # a model
#' extract_multimodel(cv_swiss, 1) # a multimodel
#' subset(cv_swiss, 1)             # a cv
#' 
#' @name extract_model
NULL

#' @name extract_model
#' @export
extract_model <- function(x, which, ...) UseMethod("extract_model")

#' @name extract_model
#' @export
extract_model.multimodel <- function(x, which, use_cv_info = TRUE, ...){
  if (missing(which) && n_model(x) == 1) which <- 1
  out <- extract.multimodel(x, i = which)
  if (!use_cv_info) out$cv_info <- NULL
  out
}

#' @name extract_model
#' @export
extract_model.cv <- function(x, which, use_cv_info = TRUE, ...){
  if (missing(which) && n_model(x) == 1) which <- 1
  out <- extract_model(x$multimodel, which)
  if (use_cv_info) 
    out <- add_cv_info(out, cvobj = extract(x, which)) 
  out
}

#' @name extract_model
#' @export
extract_multimodel <- function(x, ...) UseMethod("extract_multimodel")

#' @name extract_model
#' @export
extract_multimodel.cv <- function(x, use_cv_info = TRUE, ...){
  ii <- seq_len(n_model(x))
  model_list <- lapply(ii, extract_model, x = x, use_cv_info = use_cv_info)
  out <- do.call(c, c(model_list, list(simplify = FALSE)))
  out$param <- x$multimodel$param
  out
}

