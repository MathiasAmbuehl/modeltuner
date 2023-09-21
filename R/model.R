
#' Create a model object
#' 
#' @description 
#' `model()` takes a fitted statistical model `x`, e.g. a \code{\link{lm}}, 
#' and returns an output of class \dQuote{model}, 
#' an R object containing the model's structure, the data and optionally the fitting weights.
#' The \dQuote{model} object does not necessarily contain the model fit, but has all information needed to refit 
#' the model, possibly after making changes to the generating call. 
#' `model()` works for fitted models created by a model fitting function having a `formula` and a `data` argument.
#' 
#' `model()` is applicable to many popular model classes such as \code{\link{lm}}, 
#' \code{\link[robustbase]{lmrob}}, \code{\link[rpart]{rpart}}, \code{\link[lme4]{lmer}} 
#' and many more.
#' 
#' @param x A fitted model meeting the formal requirements described in the \dQuote{Details} section.
##' Alternatively, an unevaluated call to a model fitting function; in that case, 
##' the model class must be provided as argument `class` (see examples).  
#' @param label Character string: A label to be attributed to the model.
#' @param predict_function Predict function, often \code{\link{predict}} - see \dQuote{Details}.
#' @param class Class of the model object. Not required if `x` is a fitted model.
#' @param add_fit Logical: Save the fitted object as a component of the result? 
#' @param response_type Character string, `"continuous"` or `"binary"`. 
#' Default: `"binary"` if all response values (converted to numeric) are 0 or 1, else `"continuous"`.
#' @param env An environment. Used for internal purposes.
#' @param \dots Arguments passed to methods. Not used in the default method.
#' @param what What to print: Either a character vector being a subset of \{"label", 
#' "formula", "data", "call", "response_type", "response", "fit", 
#' "predict_function", "saved_objects", "class", "weights"\}
#' or `TRUE`, meaning all of these elements.
#' @param abbreviate Logical. If `TRUE` (the default), long formulas and calls are printed in abbreviated mode, 
#' such that they usually fit on 4 or fewer output lines; otherwise they are printed entirely, no matter how long they are.
#' @param width Integer: Width of printed output.
#' @param indent Used internally only.
#' 
#' @details
#' Model classes suited for \code{model()} must meet the following requirements:
#' \itemize{
#' \item{The model fitting function must have formal arguments `formula` and `data`,
#' accepting a \code{\link{formula}} and a \code{\link{data.frame}}, respectively.}
#' \item{In case of a binary response, the response must be numeric and have values 0 or 1.}
#' \item{\code{\link{getCall}(x)} with `x` a model of the class in question must return the call generating `x`.}
#' \item{There should usually be a \code{\link{predict}} method for the class of `x` 
#' returning a vector of length `nrow(data)`.
#' Alternatively, a real valued function with two arguments, a model and a `data.frame`, 
#' can be specified as the `predict_function`.}
#' \item{If weighted fitting is required, the model fitting function must have 
#' an argument `weights`. In that case, there should be an appropriate \code{\link[stats]{weights}} 
#' method for the class `class` returning either `NULL` or a vector of non-negative values of length `nrow(data)`.
#' However, a model class not supporting weighted fitting will work as long as you do not 
#' specify `weights` in `model()`.}
#' }
#' 
#' The *model generating call* is saved in the output object in a partially generic mode. 
#' Specifically, the arguments `data` and `weights` (if present) are given generic values `data=data` and
#' `weights=weights`, respectively. This is convenient for cross-validation with \code{\link{cv}()}, 
#' where the model generating call is repeatedly adapted and executed internally, using data and weights stored in the
#' \dQuote{model} object. See also the argument `use_original_args` in \code{\link{fit}()}, which is related to 
#' this reformulation of the model generating call.
#' 
#' @section Note:
#' If the current settings of the options `na.action` and `contrasts` (or other options) potentially change the behavior
#' of the model fitting function, then changing these options during an analysis involving that type of model must be avoided.
#' 
#' @return 
#' \code{model()} returns a list with class attribute `c(paste0("model_", class), "model")` having the following elements:
#' \itemize{
#' \item{*label*, a character string used as identifier of the model;}
#' \item{*formula*, a \code{\link{formula}} defining the model's structure;} 
#' \item{*data*, a `data.frame`;} 
#' \item{*call*, the call fitting the model;} 
#' \item{*response_type*, character string, `"continuous"` or `"binary"`;}
#' \item{*response*, character string containing the variable name or expression defining the response;} 
#' \item{*fit* (optional), the fitted object of class `class`;} 
#' \item{*predict_function*, a real valued function with two arguments: 
#' A model and a data of the same structure as *data*;} 
#' \item{*saved_objects*, a list of objects appearing in the *call* and not being part of the *data*;} 
#' \item{*class*, the class of the fitted model;} 
#' \item{*weights*, an optional vector of fitting weights.} 
#' } 
#' 
#' @section Methods:
#' \itemize{
#' \item{`model.call()` and `model.character()` allow creating a \dQuote{model} object  without having to fit the 
#' corresponding model. See examples. Specifying the  `class` is mandatory here, 
#' as it can not be queried from the input `x`.}
#' \item{There is a method `model.model()` that returns its input `x` unchanged.}
#' }
#' 
#' @seealso
#' \code{\link{multimodel}}, \code{\link{model-methods}}, \code{\link{update.model}}, \code{\link{fit}}
#' 
#' @examples
#' # lm
#' fitted <- lm(Sepal.Width ~ ., iris)
#' mod1 <- model(fitted, label = "LinearModel")
#' mod1
#' 
#' # weighted lm
#' model(lm(Sepal.Width ~ ., iris, weights = runif(nrow(iris))), 
#'       label = "weightedLm")
#' 
#' # print.model
#' print(mod1, what = TRUE)
#' print(mod1, what = c("label", "call", "class"))
#' print(mod1, what = NULL)
#' 
#' # model() applied to an unevaluated call:
#' lm_call <- quote(lm(Sepal.Width~., iris))
#' model(lm_call, class = "lm")
#' # Same using the method model.character():
#' model("lm", Sepal.Width~., iris, class = "lm")
#' 
#' @name model
NULL

#' @rdname model
#' @export
model <- function(x, ...){
  UseMethod("model")
}

#' @rdname model
#' @export
model.default <- function(x, label = "model", 
                          class = attr(x, "class"),
                          add_fit = TRUE, 
                          response_type = NULL,
                          predict_function = predict, 
                          env = parent.frame(), ...){
  call <- get_call(x)
  formula <- get_formula(x, call, env = parent.frame())
  data <- get_data(x, call, environment = env)
  if (inherits(formula, "formula"))
    formula <- formula(terms(formula, data = data, simplify = TRUE)) # expands dots, if any
  response <- get_response(formula)
  original_args <- list(data = call$data, weights = call$weights)
  call <- modify_call(call, val = list(data = quote(data), ...))
  if (hasWeights <- !is.null(call$weights)){
    call <- modify_call(call, val = list(weights = quote(weights), ...))
    .weights <- weights(x, environment = env)
    if (is.language(.weights)){
      .weights <- eval(.weights, data, env)
    }
    stopifnot(nrow(data) == length(.weights))
  }
  if (!is.call(x)) class <- class[[1]]
  obj_args <- as.character(
    unlist(lapply(
      as.list(call[-c(1, match(c("data", "weights"), names(call), 0))]), 
      all.vars), use.names = FALSE))
  obj_form <- setdiff(all.vars(formula), names(data))
  saved_objects <- get_objects(obj_args, env)
  if (!identical(env, environment(formula))){
    more_objects <- get_objects(obj_form, environment(formula))
    more_objects <- more_objects[!(names(more_objects) %in% names(saved_objects))]
    if (length(more_objects)) saved_objects <- c(saved_objects, more_objects)
  }
  if (length(saved_objects)){
    # Move objects appearing in formula having same length (or nrow) as response vector 
    # from saved_objects to data
    # -> it is assumed that these elements need to be subsetted in a cv!!
    v <- all.vars(formula)
    n <- length(eval(parse(text = response), data, env))
    v <- v[v %in% names(saved_objects)]
    v <- v[sapply(saved_objects[v], NROW) == n]
    if (length(v)){
      if (is.null(data)) data <- data.frame(row.names = seq_len(n))
      data[v] <- saved_objects[v]
      saved_objects[v] <- NULL
    }
  }
  out <- list(
    label = label, formula = formula, data = data, call = call, 
    original_args = original_args, response_type = NA, response = response, fit = NULL,
    predict_function = predict_function, saved_objects = saved_objects, 
    class = class)
  # fit
  if (add_fit) out$fit <- x  
  # response type
  if (is.null(response_type)){
    response_values <- response.model(out)
    response_values <- response_values[!is.na(response_values)]
    response_type <- 
      if (all(match(response_values, 0:1, 0) > 0, na.rm = TRUE)) "binary" else "continuous"
  } else {
    response_type <- match.arg(response_type, c("continuous", "binary"))
  }
  out$response_type <- response_type
  if (hasWeights){
    out$weights <- .weights
  } else {
    out["weights"] <- list(NULL)
  }
  structure(out, class = c(paste0("model_", class), "model"))
}

#' @rdname model
#' @export
model.call <- function(x, label = "model", class, add_fit = FALSE, 
                       env = parent.frame(), ...){
  x <- match.call(eval.parent(x[[1]]), x)  
  class(x) <- class
  out <- model(x, label = label, class = class, 
               add_fit = add_fit, env = env, ...)
  if (add_fit){
    out["fit"] <- list(NULL)
    out$fit <- fit(out)    
  }
  out
}
# model(quote(ranger(Sepal.Width ~ ., iris)), class = "ranger")
# .Last.value$predict_function

#' @rdname model
#' @export
model.character <- function(x, ..., label = "model", class, add_fit = FALSE, 
                            env = parent.frame()){
  cl <- match.call()
  cl[[1]] <- as.symbol(cl$x)
  cl$x <- cl$label <- cl$class <- cl$add_fit <- cl$env <- NULL
  model(cl, label = label, class = class, add_fit = add_fit, env = env)
}
# model("lmer", Sepal.Length ~ Petal.Width + (1|Species), iris, class = "lmerMod")
# .Last.value$predict_function

#' @rdname model
#' @export
model.model <- function(x, ...){
  x
}

# ------------------------------------------------------------------------------

#' @rdname model
#' @importFrom utils str capture.output
#' @export
print.model <- function(x, what = c("label", "class", "formula", "data", "response_type", 
                                    "weights", "call", "fit"), 
                        abbreviate = TRUE, width = getOption("width"), 
                        indent = "", ...){
  cat(indent, "--- A ", dQuote("model"), " object ---\n", sep = "") 
  prnt_model_lines(x, "  ", what = what, abbreviate = abbreviate, width = width)
  prnt_cv_info(x, ...)
  invisible(x)
}
prnt_model_lines <- function(x, indent = "", what = TRUE, width = getOption("width"), 
                             label = NULL, metric = NULL, abbreviate = TRUE){
  rnm <- names(x) %in% c("class", "data")
  min_width <- 30
  max_width <- 120
  width <- pmin(pmax(width, min_width), max_width)
  if (!is.null(label)) x$label <- label
  elements <- c("label", "class", "formula", "data", "response_type", "response", "weights", 
                "metric", "call", "predict_function", "saved_objects", "fit")
  if (isTRUE(what)) what <- elements
  if (length(metric)){
    x$metric <- metric
    what <- c(what, "metric")
  }
  original_args <- x$original_args
  what <- intersect(elements, what)
  what <- intersect(what, names(x))
  if (!length(what)) return(invisible())
  x <- x[what]
  if ("class" %in% what){
    names(x)[match("class", what)] <- "model class"
  }
  cutoff_data <- max(width - nchar(indent) - max(nchar(names(x))) - 40, 20)
  if ("data" %in% what){
    x$data <- prnt_compact(x$data)
    if (!is.null(original_args$data)){
      dep <- deparse(original_args$data, width.cutoff = cutoff_data)
      if (length(dep) > 1) dep <- paste0(dep[[1]], "...")
      x$data <- c(x$data, paste0("input as: ", 
                                 sQuote(paste0("data = ", dep))))
    }
  }
  if ("predict_function" %in% what){
    x$predict_function <- capture.output(str(x$predict_function, give.attr = FALSE))
  }
  if ("saved_objects" %in% what){
    x$saved_objects <- if (length(x$saved_objects)){
      paste0(names(x$saved_objects), collapse = ", ")
    } else {
      NULL
    }
  }
  if ("fit" %in% what){
    x$fit <- if (length(x$fit)){
      cl <- class(x$fit)
      paste0("Object of class", if (length(cl)>1) "es", " ", 
                                    paste(sQuote(cl), collapse = ", "))
    } else {
      NULL
    }
  }
  if (is.null(x$weights)){
    if ("weights" %in% names(x)) x <- x[names(x) != "weights"]
  } else if ("weights" %in% what){
    x$weights <- prnt_compact(x$weights)
    if (!is.null(original_args$weights)){
      if (is.numeric(original_args$weights)) 
        original_args$weights <- signif(original_args$weights, digits = 3)
      dep <- deparse(original_args$weights, width.cutoff = cutoff_data)
      if (length(dep) > 1) dep <- paste0(dep[[1]], "...")
      x$weights <- c(x$weights, paste0("input as: ", 
                                 sQuote(paste0("weights = ", dep))))
    }
  }
  render_list(x, width = width, abbreviate = abbreviate, 
              distribute = c("data", "weights"))
}

# Lists contents of a list in a sensible way
#   objects in deparseElements are parsed and aligned
#   all other list elements are rendered unchangedly - safest to convert to char strings!
render_list <- function(x, width = getOption("width"), max_width = 120, 
                        min_width = 30, 
                        deparseElements = intersect(names(x), c("formula", "call")), 
                        distribute = character(0),
                        indent = "  ", abbreviate = TRUE){ 
  x <- x_in <- as.list(x)
  width <- min(width, max_width)
  cutoff <- max(width - nchar(indent) - max(nchar(names(x))) - 20, min_width)
  x <- x[lengths(x)>0]
  for (objName in deparseElements){
    x[[objName]] <- deparse(x[[objName]], width.cutoff = cutoff)
    names(x[[objName]]) <- c(objName, rep("", length(x[[objName]])-1))
  }
  if (isTRUE(distribute)){
    distribute <- names(x)
  } else {
    distribute <- intersect(distribute, names(x))
  }
  for (objName in distribute){
    x[[objName]] <- capture.output(cat(x[[objName]], sep = ", ", fill = cutoff))
    names(x[[objName]]) <- c(objName, rep("", length(x[[objName]])-1))
  }
  
  if (abbreviate){
    # using x_in, cutoff, indent in {} section
    prtLns <- 3
    nlines_formula <- length(x$formula)
    if (nlines_formula > prtLns+1){
      # abbreviate long formula
      x$formula <- x$formula[1:prtLns]
      n_terms <- ncol(attr(terms(x_in$formula), "factors"))
      x$formula <- c(x$formula, paste0(indent, "  ... [formula cut off - ", n_terms, " terms on rhs]"))
    }
    if (length(x$call) > prtLns){
      # abbreviate long call
      cl <- x_in$call
      if ("formula" %in% names(cl) &&
          inherits(cl$formula, "formula") &&
          (length(deparsed_form <- deparse(cl$formula, width.cutoff = cutoff)) >= prtLns ||
           "formula" %in% names(x))){
        cl$formula <- as.name("<formula>")
      }
      for (.arg_name in setdiff(names(cl), "formula")){
        # render long atomic vectors in call in a compact way
        stopifnot(.arg_name %in% names(cl))
        deparsed_arg <- deparse(x_in$call[[.arg_name]], width.cutoff = cutoff)
        if (length(deparsed_arg) > 1 &&
            is.atomic(.evaluated_arg <- try(eval(x_in$call[[.arg_name]], parent.frame()), 
                                            silent = TRUE))){
          cl[[.arg_name]] <- as.name(paste0("<", prnt_compact(.evaluated_arg, blank = FALSE), ">"))
        }
      }
      x$call <- gsub("`", "", deparse(cl, width.cutoff = cutoff))
      names(x$call) <- c("call", rep("", length(x$call)-1))
    }
  }
  lns <- unlist(lapply(x, as.character))
  for (nm in setdiff(names(x), union(deparseElements, distribute)))
    names(x[[nm]]) <- names(x[nm])
  names(lns) <- unlist(lapply(x, names))
  names(lns)[nzchar(names(lns))] <- paste0(names(lns)[nzchar(names(lns))], ": ")
  writeLines(paste0(indent, format(names(lns)), " ", lns, sep = ""))
}


#' @details
#' \code{prnt_compact()} and \code{prnt_cv_info()} are generic utility functions used in some \code{print} methods.
#' @rdname modeltuner-internal
#' @export
prnt_compact <- function(x, ...) UseMethod("prnt_compact")
#' @export
prnt_compact.default <- function(x, dim = is.atomic(out), blank = TRUE, ...){
  if (is.null(x)) return(NULL)
  out <- class(x)[[1]]
  if (dim) out <- paste0(out, if (blank) " ",
                         "[", paste(DIM(x), collapse = " x "), "]")
  out
}
DIM <- function(x){
  if (!is.null(dim(x))) return(dim(x))
  if (is.vector(x)) return(length(x))
  NULL
}

#' @export
prnt_compact.modeldata_xy <- function(x, ...){
  out <- ""
  if (!is.null(x$y)) out <- paste0("y: ", prnt_compact(x$y))
  if (!is.null(x$x)) out <- paste0(out, ",  x: ", prnt_compact(x$x))
  out
}

# ------------------------------------------------------------------------------

#' Extract the (fitting) `weights` from a \dQuote{\link{model}} object
#' 
#' @param object A \dQuote{model} object.
#' @param \dots Currently not used.
#' 
#' @return
#' Returns either a vector of weights or `NULL`.
#' 
#' @seealso \code{\link{weights}}, \code{\link{model}}
#' 
#' @examples
#' # Simulate data
#' set.seed(314159)
#' n <- 50
#' x <- rnorm(n)
#' w <- runif(n)
#' y <- x + rnorm(n)/sqrt(w)
#' 
#' # Weighted lm
#' mymodel <- model(lm(y~x, weights = w))
#' 
#' # Extract weights
#' weights(mymodel)
#' 
#' @importFrom stats weights
#' @export
weights.model <- function(object, ...){
  object$weights
}

#' @importFrom stats weights
#' @export
weights.call <- function (object, environment = parent.env(), ...){
    eval(object$weights, environment)
}

