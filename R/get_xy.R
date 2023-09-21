
#' @details
#' \code{get_xy()} is a generic taking formula and data, returns list of x matrix, y vector and terms object.
#' Designed for internal use in \code{formula}-interface wrappers.
#' @rdname modeltuner-internal
#' @export
get_xy <- function(object, data, weights = NULL, ...) UseMethod("get_xy")

# method for class 'formula', typically used in model fitting functions
#' @importFrom stats terms model.weights na.pass .getXlevels
#' @export
get_xy.formula <- function(object, data, weights = NULL, remove_intercept = FALSE, 
                           na.action = getOption("na.action"), contrasts = NULL, ...){
  op <- options(na.action = na.action) # na.action is passed to model.matrix() via option
  on.exit(options(op))
  if (remove_intercept) object <- noint(object)
  mf <- quote(model.frame(object, data = data, weights = weights, na.action = na.action, 
                          drop.unused.levels = TRUE))
  mf["weights"] <- list(weights) # circumvent non-standard evaluation of 'weights' in model.frame()
  mf <- eval(mf)
  trms <- terms(mf)
  newtrms <- terms(formula(trms), simplify = TRUE)
  if (isTRUE(all.equal(trms, newtrms))){
    trms <- newtrms
  } else {
    trms <- terms(formula(newtrms)) 
    # this avoids that variables present in formula as -var (being removed) be formally required for prediction
  }
  mm <- model.matrix(mf, data, contrasts.arg = contrasts)
  out <- list(x = mm, 
              y = as.vector(model.response(mf)),
              w = as.vector(model.weights(mf)),
              terms = trms, 
              xlevels = .getXlevels(trms, mf), 
              na.action = attr(mf, "na.action"), 
              contrasts = attr(mm, "contrasts"))
  class(out) <- c("modeldata_xy", "list")
  out
}

# default method, used if data is a matrix or a list of matrix x and vector y
# object is irrelevant (often NULL)
#' @export
get_xy.default <- function(object, data, weights = NULL, ...){
  get_xy2(data, weights, ...)
}

#' @details
#' \code{get_xy2()} is a generic dispatching on the second argument, `data`.
#' @rdname modeltuner-internal
#' @export
get_xy2 <- function(data, weights = NULL, ...) UseMethod("get_xy2")

get_xy2.default <- function(data, weights = NULL, ...){
  if (is.matrix(data) || is.data.frame(data)) data <- list(x = data)
  data["w"] <- list(weights)
  if (is.list(data)) class(data) <- c("modeldata_xy", class(data))
  data
}


# method for class "terms", typically used in predict method with x_only = TRUE
#' @importFrom stats delete.response
#' @export
get_xy.terms <- function(object, data, weights = NULL, remove_intercept = FALSE,
                         na.action = getOption("na.action"), contrasts = NULL,
                         x_only = FALSE, xlev = NULL, ...){
  op <- options(na.action = na.action) # na.action is passed to model.matrix() via option
  on.exit(options(op))
  if (!x_only){
    object <- formula(object)
    out <- NextMethod()
    return(out)
  }
  trms <- delete.response(terms(object, data = data))
  mf <- model.frame(trms, data, na.action = na.action)
  out <- list(x = model.matrix(mf, data, contrasts.arg = contrasts, xlev = xlev), 
              na.action = attr(mf, "na.action"))
  class(out) <- c("modeldata_xy", "list")
  out
}

# method for class "model":
#' @export
get_xy.model <- function(object, data = object$data, weights = object$weights, 
                         remove_intercept = attr(terms(object), "intercept")>0, 
                         na.action = object$call$na.action, ...){
  if (is.null(na.action)) na.action <- getOption("na.action")
  get_xy(object$formula, data = data, weights = weights, 
         remove_intercept = remove_intercept, na.action = na.action)
}


# aux fn
noint <- function(form){
  form <- formula(form)
  l <- length(form)
  form[[l]] <- call("-", form[[3]], 1)
  form
}

#' @export
print.modeldata_xy <- function(x, abbreviate = TRUE, ...){
  out <- list()
  out$formula <- x$terms
  out$y <- prnt_compact(x$y)
  out$x <- prnt_compact(x$x)
  out$w <- if (is.null(x$w)) "NULL" else prnt_compact(x$w)
  if (length(x$xlevels)){
    xlvls <- lengths(x$xlevels, use.names = TRUE)
    out$xlevels <- paste("factor levels of", paste0(names(xlvls), "[", xlvls, "]", collapse = ", "))
  }
  if (length(na.act <- x$na.action)){
    nNA <- length(na.act)
    out$na.action <- paste0(class(na.act), " ", nNA, " observation", if (nNA>1) "s")
  }
  cat("List of class ", sQuote("modeldata_xy"), ":\n", sep = "")
  render_list(out, width = getOption("width"), max_width = 120, min_width = 30, 
              abbreviate = abbreviate)
  invisible(x)
}


# j can not be set (included for formal reasons)
#' @export
`[.modeldata_xy` <- function(x, i, j, drop = FALSE){
  stopifnot(missing(j))
  x$x <- x$x[i, , drop = drop]
  if (length(x$y)) x$y <- x$y[i]
  if (length(x$w)) x$w <- x$w[i]
  x
}
  

if (F){
  formula <- Sepal.Width ~ .
  data <- iris
  
  xy <- get_xy(formula, data)
  xy
  str(xy)
  str(xyw <- get_xy(formula, data, weights = runif(150)), 1)
  str(xyw[1:10], 1)

  str(get_xy(data = xy), 1)
  str(get_xy(data = xyw, keep_terms = FALSE), 1)
  
  str(get_xy(Sepal.Length ~ Species, iris, weights = Sepal.Width), 1) # weights evaluated in data
  str(get_xy(Sepal.Length ~ Species, iris, weights = iris$Sepal.Width), 1) # weights evaluated in parent.frame()
  
  newdata <- car::some(iris, 1)
  str(get_xy(xy$terms, newdata))
  str(get_xy(xy$terms, newdata, keep_terms = TRUE))
  str(get_xy(xy$terms, iris)[1:3])

}

