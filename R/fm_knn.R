
#' k-Nearest Neighbors model
#' 
#' @description 
#' This is a `formula`-based implementation of k-Nearest Neighbor.  
#' `fm_knn()` \dQuote{fits} the model, which essentially amounts to
#' saving a reformatted copy of the model data.
#' The `predict` method finds the nearest neighbors of the points in `newdata` 
#' (using \code{\link{nn2}()} from package \CRANpkg{RANN}) and returns their averages.
#' 
#' @param formula A `formula`.
#' @param data A `data.frame`.
#' @param k Either a scalar integer, the number of neighbors, or alternatively
#' a vector of (typically decreasing) non-negative numeric values.
#' In the latter case, the model predictions will be weighted averages, 
#' with `k[1]` used as weight of the nearest neighbor, `k[2]` for the second nearest, etc.  
#' @param standardize Logical: Standardize the columns of the `x`, the design matrix? 
#' If `TRUE`, distances will be calculated after standardization using means and 
#' standard deviations from the `x` matrix. 
#' @param weights Weights used for averaging. Not used in the determination of neighbors.
#' @param na.action A function which indicates what should happen when the data contain `NA`s.
#' \code{\link{na.omit}} is the default, \code{\link{na.exclude}} or \code{\link{na.fail}}
#' could be useful alternative settings. 
#' @param \dots Not used in `fm_knn()`. In `predict.fm_knn()`: passed to \code{\link{nn2}()}.
#' 
#' @details
#' \dQuote{Fitting} a model with `fm_knn()` essentially amounts to saving the (possibly standardized) model data.
#' 
#' Bindings in distances are currently not handled in a clever way, such that the predictions may depend on the order of the data.
#' 
#' Combining an argument `k` of length `>1` and non-Null `weights` results in a mixture of two types of weights and 
#' is thus not recommended.
#' 
#' @return 
#' `fm_knn()` returns a list of class \dQuote{fm_knn} with components
#' \itemize{
#' \item{*formula*: the formula;}
#' \item{*x*: the model matrix (resulting from the `formula` using `model.matrix()`);} 
#' \item{*y*: the vector of the response values;} 
#' \item{*k*: the parameter `k`, the number of neighbors;} 
#' \item{*standardize*: the parameter `standardize`, a logical value;} 
#' \item{*weights*: the fitting weights;}
#' \item{*xlevels*: list of the levels of the factors included in the model;}
#' \item{*na.action*: the \code{\link{na.action}} used during data preparation;}
#' \item{*contrasts*: the \code{\link{contrasts}} used during data preparation;}
#' \item{*call*: the matched call generating the model.}
#' } 
#' 
#' @seealso \code{\link[RANN]{nn2}} (package \CRANpkg{RANN})
#' 
#' @examples
#' d <- simuldat()
#' nnmodel <- fm_knn(Y~ ., d)
#' nnmodel
#' 
#' # Predictions for new observations 
#' newd <- simuldat(n = 10)
#' data.frame(newd["Y"], 
#'            pred = predict(nnmodel, newdata = newd))
#' 
#' @importFrom stats na.omit
#' @importFrom matrixStats colWeightedMeans colWeightedSds
#' @export
fm_knn <- function(formula, data, k = 5, standardize = TRUE, weights = NULL, 
                   na.action = na.omit, ...){
  if (missing(formula)) formula <- NULL
  if (missing(data)) data <- NULL
  w <- eval(substitute(weights), data, parent.frame())
  xy <- get_xy(formula, data, w, na.action = na.action, remove_intercept = TRUE)
  if (NCOL(xy$x) < 1) 
    stop("Invalid formula, expecting at least one variable on rhs.")
  if (is.null(xy$w)){
    if (standardize) xy$x <- scale(xy$x)
  } else {
    if (length(xy$w) != nrow(xy$x)) stop("weights have inappropriate length.")
    if (any(is.na(xy$w))) stop("NAs in weights are not allowed.")
    xy <- xy[xy$w>0]
    if (standardize)
      xy$x <- scale(xy$x, 
                    center = colWeightedMeans(xy$x, w = xy$w), 
                    scale = colWeightedSds(xy$x, w = xy$w))
  }
  out <- list(
    formula = xy$terms,
    x = xy$x,
    y = xy$y, 
    k = k,
    standardize = standardize,
    weights = xy$w,
    xlevels = xy$xlevels, 
    na.action = xy$na.action, 
    contrasts = xy$contrasts,
    call = match.call())
  class(out) <- c("fm_knn", "list")
  out
}

deparse_cut <- function(expr, width.cutoff = 60){
  dep <- deparse(expr, width.cutoff)
  if (length(dep)>1)
    dep <- paste0(dep, "...")[[1]]
  dep
}

#' @inheritParams print.model
#' @export
print.fm_knn <- function(x, abbreviate = TRUE, ...){
  cat("k-nearest neighbors model (class ", sQuote("fm_knn"), ")\n", sep = "")
  prnt <- list(formula = x$formula, 
               data = deparse(x$call$data)[[1]],
               k = x$k, 
               n = nrow(x$x),
               standardize = x$standardize,
               weighted = !is.null(x$weights))
  render_list(prnt, abbreviate = abbreviate)
  invisible(x)
}


#' @describeIn fm_knn `predict` method for class \dQuote{fm_knn}.
#' @param object Object of class \dQuote{fm_knn}.
#' @param newdata `data.frame` with the data to be predicted.
#' If missing, predictions for the model data will be returned.
#' @importFrom RANN nn2
#' @importFrom utils head
#' @importFrom stats napredict
#' @export
predict.fm_knn <- function(object, newdata, k = object$k, ...){
  if (missing(newdata)){
    mmnew <- object$x
  } else {
    xy <- get_xy(object$formula, data = newdata, xlev = object$xlevels, 
                 na.action = na.exclude, x_only = TRUE, 
                 contrasts.arg = object$contrasts)
    mmnew <- xy$x
    if (object$standardize){
      mmnew <- scale(mmnew, center = attr(object$x, "scaled:center"), 
                     scale = attr(object$x, "scaled:scale"))
    }
  }
  w <- if (length(k) == 1) rep(1, k) else k
  w <- head(w, min(length(w), nrow(object$x)))
  nn <- nn2(object$x, mmnew, k = length(w), ...)
  if (is.null(object$weights)){
    w <- w/sum(w)
    prediction <- as.vector(array(object$y[nn$nn.idx], dim(nn$nn.idx)) %*% w)
  } else {
    w <- w * array(object$weights[as.vector(nn$nn.idx)], 
                   dim = dim(nn$nn.idx))
    w <- sweep(w, 1, rowSums(w), "/")
    prediction <- rowSums(array(object$y[nn$nn.idx], dim(nn$nn.idx)) * w)
  }
  names(prediction) <- rownames(mmnew)
  if (missing(newdata) && !is.null(na.act <- object$na.action)){
    prediction <- napredict(na.act, prediction)
  } else if (!is.null(na.act <- xy$na.action)){
    prediction <- napredict(na.act, prediction)
  }
  prediction
}


#' @export
terms.fm_knn <- function(x, ...) x$formula

#' @export
formula.fm_knn <- function(x, ...) formula(terms(x), ...)
