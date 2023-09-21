
#' `formula`-based wrapper for \code{xgb.train()}
#' 
#' @description
#' `fm_xgb()` is a convenience wrapper for tree boosting with 
#' \code{\link[xgboost]{xgb.train}()} (from package \CRANpkg{xgboost})
#' that fits into the `modeltuner` framework.
#' The model is specified by the arguments `formula` and `data`. 
#' The resulting models belong to the class of so-called *iteratively fitted models*, 
#' see \code{\link{ifm}} and `vignette("ifm")` for information.
#' 
#' @param formula A `formula`.
#' @param data A `data.frame`.
#' @param weights Fitting weights.
#' @param na.action A function which indicates what should happen when the data contain `NA`s.
#' \code{\link{na.pass}} is the default, \code{\link{na.omit}}, \code{\link{na.exclude}} or \code{\link{na.fail}}
#' could be meaningful alternative settings.
#' @param monotone_constraints Named vector with values in `c(-1, 0, 1)`. 
#' Names identify features, `1` means increasing, `-1` decreasing and `0` no constraint. 
#' Features not appearing will be assigned a `0` in the call to `xgb.train()`. 
#' Default is no constraints.
#' @param interaction_constraints List of named character vectors defining interaction constraints. Default is no constraints.
#' @param nrounds,early_stopping_rounds,obj,feval,maximize Passed to \code{xgb.train} 
#' (but note that some default values are different).
#' @param verbose Logical: Whether to print information on progress to console.
#' @param keep_x Logical: Whether to keep the model matrix `x` as a component of the return value.
#' @param ... Passed to `params` in `xgb.train()`.
#' @param x,object Object of class \dQuote{fm_xgb}.
#' @param newdata Data for prediction.
#' @inheritParams cv_performance
#' @inheritParams fm_glmnet
#' 
#' @details
#' Not all parameters of `xgb.train()` are available in `fm_xgb()`. 
#' In particular, those related to console output (`verbose`, `print_every_n`), 
#' those related to saving the result (`save_period`, `save_name`) and `callbacks` 
#' won't be passed to `xgb.train()`.
#' The parameters `x` and `y` to be passed to `xgb.train()` are extracted from 
#' `formula` and `data` by means of \code{\link{model.frame}}, \code{\link{model.matrix}}
#' and \code{\link{model.response}}.
#' 
#' *Features of cross-validation of models generated with* `fm_xgb()`:
#' \itemize{
#' \item{The model class \dQuote{fm_xgb} belongs to the class of so-called 
#' *iteratively fitted models*; see \link{ifm} and `vifnette("ifm")` for information on
#' the peculiarities of cross-validating such models. In particular, 
#' note the role of the parameter `iter` in \code{\link{cv}()}.}
#' \item{When `cv()` is executed with `keep_fits=TRUE`, the fitted models from
#' cross-validation that are stored in the result (and returned by \code{\link{extract_fits}()}) 
#' will not be of class \dQuote{fm_xgb}, but of class \dQuote{xgb.Booster},}
#' }
#' 
#' *Default metric:* 
#' Currently, xgboost models generated with `fm_xgb()` are the only models not having the default choice 
#' of its metric, `rmse` for continuous response and `logLoss` in the binary case.
#' Each xgboost model has an `eval_metric`. 
#' If not specified explicitly by the user, this metric is automatically chosen depending on the `objective` 
#' in the call to `xgb.train()` or `fm_xgb()`.
#' In `modeltuner`, when `cv()` is applied, the `eval_metric` is taken as the default metric 
#' of the resulting "cv" object.
#' (see \code{\link{default_metric}()}). 
#' 
#' `extract_booster()` returns the *booster*, an object of class \dQuote{xgb.Booster}, 
#' as returned by \code{\link[xgboost]{xgb.train}()}.
#' 
#' @return
#' `fm_xgb()` returns a list of class \dQuote{fm_xgb} with components
#' \itemize{
#' \item{*booster*: the xgboost booster, of class \dQuote{xgb.Booster};}
#' \item{*formula*: the formula;}
#' \item{*x*: the model matrix (resulting from the `formula` using `model.matrix()`);} 
#' \item{*weights*: the fitting weights;}
#' \item{*xlevels*: list of the levels of the factors included in the model;}
#' \item{*pref_iter*: the preferred iteration, an integer (see argument `pref_iter`);}
#' \item{*na.action*: the \code{\link{na.action}} used during data preparation;}
#' \item{*contrasts*: the \code{\link{contrasts}} used during data preparation;}
#' \item{*call*: the matched call generating the model.}
#' } 
#' 
#' `extract_booster()` returns the booster, of class \dQuote{xgb.Booster}.
#' 
#' @seealso \code{\link[xgboost]{xgb.train}}, \code{\link{xgb.cv}} (package \CRANpkg{xgboost});
#' \link{ifm} and \code{vignette("ifm")}; \code{\link{default_metric}}; 
#' \code{\link{fit.model_fm_xgb}}; \code{\link{set_pref_iter}}
#' 
#' @examples
#' # mtcars data
#' xgb_cars <- fm_xgb(mpg ~ ., mtcars)
#' # predict
#' predict(xgb_cars, newdata = head(mtcars))
#' 
#' # iris data
#' xgb_iris <- fm_xgb(Sepal.Width ~ ., iris)
#' # cross-validate
#' cv(xgb_iris)
#' # Plot evaluation log
#' plot(evaluation_log(last_cv()))
#' 
#' @name fm_xgb
#' @importFrom xgboost xgb.DMatrix xgb.train
#' @export
fm_xgb <- function(formula, data, nrounds = 100, early_stopping_rounds = 10, 
                   weights = NULL, na.action = na.pass, verbose = interactive(), 
                   monotone_constraints = 0, interaction_constraints = NULL, 
                   obj = NULL, feval = NULL, maximize = FALSE, 
                   pref_iter = NULL, keep_x = TRUE, ...){
  if (missing(formula)) formula <- NULL
  if (missing(data)) data <- NULL
  w <- if (inherits(data, "xgb.DMatrix")){
    eval(substitute(weights), parent.frame())
  } else {
    eval(substitute(weights), data, parent.frame())
  }
  xy <- get_xy(formula, data, weights = w, remove_intercept = TRUE,
               na.action = na.action)
  if (NCOL(xy$x) < 1) 
    stop("Invalid formula, expecting at least one variable on rhs.")
  d <- xgbDMat(xy)
  y <- getinfo(d, "label")
  w <- getinfo(d, "weight")
  xnms <- colnames(xy$x)
  if (!missing(monotone_constraints)){
    if (is.null(names(monotone_constraints))){
      stop(sQuote("monotone_constraints"), " must be a named vector.")
    }
    monotone_constraints <- prep_monotone_constraints(monotone_constraints, xnms)  
  }
  if (!is.null(interaction_constraints)){
    interaction_constraints <- 
      prep_interaction_constraints(interaction_constraints, xy)
  }
  if (is.null(interaction_constraints)){
    interaction_constraints <- list(seq_along(xnms)-1L)
  }
  booster <- xgb.train(data = d, nrounds = nrounds, early_stopping_rounds = early_stopping_rounds, 
                       params = list(booster = "gbtree", ...), watchlist = list(train = d), verbose = FALSE, 
                       monotone_constraints = monotone_constraints, interaction_constraints = interaction_constraints, 
                       callbacks  = if (verbose) list(cb_xgb_iteration(prefix = "building booster: ", 
                                                                       keep_final_msg = FALSE)))
  if (verbose) message("\rBooster built, ", booster$niter, " iterations",
                       "                                        \r")
  if (is.null(pref_iter)){
    pref_iter <- booster$niter
  } else {
    pref_iter <- min(pref_iter, booster$niter)
  }
  out <- structure(list(booster = booster, 
                        formula = if (inherits(xy, "xgb.DMatrix")) NULL else xy$terms, 
                        x = xy$x, 
                        weights = w,
                        xlevels = xy$xlevels,
                        pref_iter = pref_iter,
                        na.action = xy$na.action, 
                        contrasts = xy$contrasts,
                        call = match.call()), class = "fm_xgb")
  if (!keep_x) out$x <- NULL
  out
}

prep_monotone_constraints <- function(constr, nms){
  ok <- names(constr) %in% nms
  if (!all(ok)){
    message("Ignoring invalid entries in ", sQuote("monotone_constraints"), 
            " (", paste(names(constr)[!ok], collapse = ","), ")")
    constr <- constr[ok]
  }
  out <- rep(0, length(nms))
  out[match(names(constr), nms)] <- constr
  out
}

prep_interaction_constraints <- function(constr, xy){
  if (!is.list(constr)){
    stop(sQuote("interaction_constraints"), " must be a list.")
  }

  # xy <- eval.parent(quote(xy))
  all_nms <- unique(unlist(constr, use.names = FALSE))
  nms <- colnames(xy$x)
  ok <- all_nms %in% nms
  
  if (!all(ok)){
    assgn <- assign_coefs(xy)
    if (all(all_nms[!ok] %in% names(assgn))){
      repl <- assgn[all_nms[!ok]]
      for (i in seq_along(repl))
        constr <- lapply(constr, function(v){
          nm <- names(repl)[[i]]
          if (nm %in% v) v <- c(setdiff(v, nm), repl[[i]])
          v
        })
    }
    all_nms <- unique(unlist(constr, use.names = FALSE))
    ok <- all_nms %in% nms
  }

  if (!all(ok)){
    message("Ignoring invalid entries in ", sQuote("interaction_constraints"), 
            " (", paste(all_nms[!ok], collapse = ","), ")")
    constr <- lapply(constr, intersect, nms)
    constr <- constr[lengths(constr)>0]
    if (length(constr) == 0) constr <- NULL
  }
  out <- lapply(constr, function(x) sort.default(match(x, table = nms)) - 1)
  out
}

assign_coefs <- function(xy){
  termlbls <- attr(xy$terms, "term.labels")
  cnms <- colnames(xy$x)
  assgn <- attr(xy$x, "assign")
  out <- split(cnms[assgn>0], assgn[assgn>0])
  names(out) <- termlbls
  out
}

xgbDMat <- function(xy){
  if (inherits(xy, "modeldata_xgbDMat")) return(xy[[1]])
  cl_mat <- call("xgb.DMatrix", data = xy$x, label = as.vector(xy$y), weight = xy$w)
  eval(cl_mat)
}


# get_xy2() method for proper handling of xgb.DMatrix data input:
#' @export
get_xy2.xgb.DMatrix <- function(data, weights, ...){
  if (!identical(xgboost::getinfo(data, "weight"), weights))
    xgboost::setinfo(data, "weight", weights)
  structure(list(data), class = c("modeldata_xgbDMat", "list"))
}

#' @describeIn fm_xgb `print()` method
#' @inheritParams print.model
#' @export
print.fm_xgb <- function(x, abbreviate = TRUE, ...){
  cat("Fitted model of class", sQuote("fm_xgb"), "\n")
  prnt <- c(x[c("formula", NA, "call")], 
            nfeatures = x$booster$nfeatures,
            iterations = x$booster$niter, 
            x["pref_iter"])
  prnt[[2]] <- deparse(x$call$data)[[1]]
  if (!is.null(d <- dim(x$x))) prnt[[2]] <- paste0(prnt[[2]], " (", d[1], " rows)")
  names(prnt)[[2]] <- "data"
  if (!is.null(prnt$best_score))prnt$best_score <- format(prnt$best_score, ...)
  if (!is.null(prnt$best_msg)) prnt$best_msg <- sub("^.+\\t(.+)$", "\\1", prnt$best_msg)
  render_list(prnt, abbreviate = abbreviate)
  invisible(x)
}

  
#' @describeIn fm_xgb `predict()` method
#' @export
predict.fm_xgb <- function(object, newdata, pref_iter = object$pref_iter, ...){
  if (newdata_missing <- missing(newdata)){
    if (!is.null(object[["x"]])){
      newdata <- object[["x"]]
    } else {
      stop("Missing ", dQuote("newdata"), " argument and model data not stored: specify newdata.")
    }
  } else {  
    newdata <- get_xy(object$formula, data = newdata, xlev = object$xlevels, 
                      na.action = na.pass, x_only = TRUE,
                      contrasts.arg = object$contrasts)$x
  }
  rnms <- rownames(newdata)
  newdata <- xgb.DMatrix(newdata)
  if (is.null(pref_iter)){
    pr <- predict(object$booster, newdata, ...)
  } else {
    stopifnot(length(pref_iter) == 1, is.numeric(pref_iter), pref_iter>0)
    if (is.na(pref_iter)) {
      pr <- rep(NA_real_, nrow(newdata), ...)
    } else {
      pr <- drop(predict(object$booster, newdata, 
                         iterationrange = c(1, pref_iter+1), ...))
    }
  }
  names(pr) <- rnms
  if (newdata_missing && !is.null(na.act <- object$na.action)){
    pr <- napredict(na.act, pr)
  }    
  pr
}

#' @rdname fm_xgb
#' @export
extract_booster <- function(object){
  stopifnot(inherits(object$booster, "xgb.Booster"))
  object$booster
}

# ------------------------------------------------------------------------------

#' @export
model.fm_xgb <- function(x, ...){
  out <- NextMethod()
  out
}


#' @importFrom xgboost getinfo
#' @export
response.model_fm_xgb <- function(object, ...){
  if (inherits(object$data, "xgb.DMatrix"))
    return(xgboost::getinfo(object$data, "label"))
  NextMethod()
}

#' @importFrom xgboost getinfo
#' @export
weights.model_fm_xgb <- function(object, ...){
  if (inherits(object$data, "xgb.DMatrix"))
    return(xgboost::getinfo(object$data, "weights"))
  NextMethod()
}

# ------------------------------------------------------------------------------

# Special methods for classes 'model_fm_xgb'

#' @export
terms.fm_xgb <- function(x, ...) x$formula

#' @export
formula.fm_xgb <- function(x, ...) formula(terms(x), ...)

#' @rdname fit
#' @importFrom xgboost setinfo
#' @export
fit.model_fm_xgb <- function(x, iter, eval = TRUE, force = FALSE, ...){
  if (missing(iter)){
    x <- set_pref_iter(x, warn = FALSE, ...)  # uses cv_info
  } else if (!is.null(iter)){
    x <- set_pref_iter(x, iter = iter, warn = TRUE, ...)
  }
  fit.model(x, eval = eval, force = force, ...)
}
