
#' Query or set model label(s)
#' 
#' `label()` extracts the model label(s) from `x`.
#' The replacement functions `label<-` and `set_label()` are used to set model label(s) in `x`.
#' `n_model()` returns the number of models included in an object `x`.
#' 
#' These functions are generic and have methods for classes \dQuote{model}, \dQuote{multimodel}, \dQuote{cv}, 
#' \dQuote{performance}, \dQuote{evaluation_log}.
#' 
#' `n_model()` just returns `length(label(x))` and is thus available for any object having an appropriate `label()` method.
#' 
#' @param x Object of class \dQuote{model}, \dQuote{multimodel}, \dQuote{cv} or \dQuote{performance}.
#' @param which Optional integer index  - allows querying or setting only a subset of the model labels.
#' Must have the same length as `value`. Default: All models in `x`.
#' @param value Labels to be attributed to models.
#' 
#' @return
#' `label()` returns a character vector. 
#' `set_label()` returns its input object `x` with modified labels. 
#' `n_model()` returns an integer.
#' 
#' @seealso \code{\link{model}}, \code{\link{multimodel}}, \code{\link{cv}}, \code{\link{cv_performance}},
#' \code{\link{evaluation_log}}
#' 
#' @examples
#' xvars <- names(iris)[-1]
#' forms <- lapply(xvars, reformulate, response = names(iris)[1])
#' model0 <- model(lm(Sepal.Length~1, iris), label = "intercept_only")
#' mm <- c(model0, multimodel(model0, formula = forms))
#' n_model(mm)
#' 
#' label(mm)  
#' label(mm, 2:5) <- xvars
#' label(mm)
#' mm
#' 
#' set_label(mm, paste0("model_number_", 1:5))
#' 
#' @name label
NULL

#' @rdname label
#' @export
label <- function(x, which) UseMethod("label")

#' @rdname label
#' @export
`label<-` <- function(x, which, value) UseMethod("label<-")

#' @rdname label
#' 
#' @export
set_label <- function(x, value, which = seq_along(label(x))){
  label(x, which) <- value
  x
}

# ------------------------------------------------------------------------------

# 'label' methods

#' @rdname label
#' @export
label.model <- function(x, which){
  x$label
}

#' @rdname label
#' @export
label.multimodel <- function(x, which){
  names(x$models)[which]
}

#' @rdname label
#' @export
label.cv <- function(x, which){
  names(x$multimodel$models)[which]
}

#' @rdname label
#' @export
label.performance <- function(x, which){
  rownames(x)[which]
}

# ------------------------------------------------------------------------------

# 'label<-' methods

# @rdname label
#' @export
`label<-.model` <- function(x, which = 1L, value){
  value <- mk_uniq(label(x), value, which)
  x$label <- value
  x
}


# @rdname label
#' @export
`label<-.multimodel` <- function(x, which = seq_along(label(x)), value){
  if (length(which) != length(value)) stop("Wrong number of model labels.")
  value <- mk_uniq(label(x), value, which)
  names(x$models)[which] <- value
  rownames(x$param)[which] <- value
  x
}

# @rdname label
#' @export
`label<-.cv` <- function(x, which = seq_along(label(x)), value){
  if (length(which) != length(value)) stop("Wrong number of model labels.")
  value <- mk_uniq(label(x), value, which)
  names(x$multimodel$models)[which] <- value
  rownames(x$multimodel$param)[which] <- value
  x
}

# @rdname label
#' @export
`label<-.performance` <- function(x, which = seq_along(label(x)), value){
  if (length(which) != length(value)) stop("Wrong number of model labels.")
  value <- mk_uniq(label(x), value, which)
  rownames(x)[which] <- value
  x
}

# @rdname label
#' @export
`label<-.evaluation_log` <- function(x, which = seq_along(label(x)), value){
  if (length(which) != length(value)) stop("Wrong number of model labels.")
  value <- mk_uniq(label(x), value, which)
  names(x)[which] <- value
  x
}

# aux fn
mk_uniq <- function(actual, repl, which){
  actual[which] <- repl
  actual <- make.unique(actual, sep = "")
  actual[which]
}

# ------------------------------------------------------------------------------

#' @rdname label
#' @export
n_model <- function(x){
  length(label(x))
}
