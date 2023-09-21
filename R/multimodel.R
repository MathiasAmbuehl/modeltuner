
#' Create a multimodel object
#'
#' @description 
#' A multimodel combines several \code{\link{model}}s with identical response in one object.
#' There are two ways to create a multimodel:
#' \itemize{
#' \item{`multimodel(x, ...)` takes an object of class \dQuote{model} or a fitted model as its input `x` 
#' and generates a number of variations of this model, with one or several parameters varying according to the specification in \dQuote{...}.}
#' \item{`c.model(...)` and `c.multimodel(...)` take a number of \code{model}s or \code{multimodel}s 
#' as their \dQuote{...} arguments and combine them in an object of class \dQuote{multimodel}. 
#' }}
#' 
#' @param x An object of class \dQuote{model} or a fitted model.
#' @param \dots In `multimodel()`: *named* parameters to be expanded; 
#' in `c.`(`multi`)`model`: one or several arguments of class \dQuote{model} or \dQuote{multimodel}.
#' @param prefix Prefix to use for the model \code{\link{label}}.
#' @param expand Logical: Expand the \dQuote{...} arguments (default) or join them element-wise?
#' If `expand=TRUE`, the vectors in \dQuote{...} will be expanded, the number of models will equal the product 
#' of the lengths of the \dQuote{...} arguments;
#' otherwise, all \dQuote{...} arguments must have equal lengths, and the number of models will be equal to their common length.
#' @param max_n_model Maximal number of models that will be included. 
#' If the number of specified models is greater than `max_n_model`, a subset will be selected at random.
#' @param param Logical: Keep or print parameter table?
#' @param simplify Logical: Simplify a `multimodel` comprising only one model to a \code{\link{model}}?
#' @param n Integer: Model details are printed for first `n` models.
#' @param what Which elements of the multimodel should be printed? See \code{\link{print.model}}.
#' @param width Integer: Width of printed output.
#' 
#' @details
#' \code{multimodel()} is a  (S3-)generic function. 
#' The core method is `multimodel.model()`, other methods are essentially wrappers to the core method and are described in the section \dQuote{Methods} below.
#' The vectors in \dQuote{...} are expanded or joined, according to the argument `expand`. 
#' Extraction of values for the single calls is done with \dQuote{`[[`} rather than \dQuote{`[`}.
#' 
#' `multimodel()` executes \code{\link{update.model}()} repeatedly, such that the helper functions \code{\link{absent}()}, 
#' \code{\link{unchanged}()} and \code{\link{null}()} can be used in a call to `multimodel()` exactly as within `update.model()`.
#' 
#' If `formula` is one of the arguments in the \dQuote{...} and some formula has a dot, it will be used to *update* the original model formula.
#' Enclose a formula in `I()` in order to *replace* the original formula instead. 
#' See the example below and \code{\link{update.model}}.
#' 
#' The output of `multimodel(...)` by default includes a *parameter table* as its element `param`, containing the values 
#' of the parameters specified in the \dQuote{...}. 
#' This is a `data.frame` having an additional class \dQuote{\link{param_table}}.
#' 
#' `c.`(`multi`)`model`: A call to `c(...)` can include both \dQuote{model} and \dQuote{multimodel} objects
#'  (but no fitted models) in its \dQuote{...}. 
#'  Using \code{\link{models}()} instead of these `c`-methods is more flexible in that it also accepts *fitted* models.
#'  
#' \dQuote{model}s specified as named arguments in the call will have their argument name taken as their \code{\link{label}}. 
#' Argument names for multimodels are ignored -- the `label`s already present in the multimodel will be used instead. 
#' Duplicate labels in the output will be adjusted using \code{\link{make.unique}()}.
#' 
#' @return
#' `multimodel()` and `c.`(`multi`)`model()` both return an object of class \dQuote{multimodel}, this is 
#' a `list` with the following elements:
#' \itemize{
#' \item{*models*: A list of the \code{\link{model}} objects.}
#' \item{*param*: The table of the parameter values resulting from `multimodel()`. 
#' Only included if there are any varying parameters and if `param = TRUE`}
#' }
#' 
#' @section Methods:
#' \itemize{
#' \item{`multimodel.model()` is described in the \dQuote{Details} section.}
#' \item{The default method expects a fitted model as its `x` and executes `x %>% model %>% multimodel(...)`.}
#' \item{`multimodel.multimodel()` returns its input `x` unchanged.}
#' }
#' 
#' @seealso \code{\link{label}}, \code{\link{extract_model}}, \code{\link{subset}}, \code{\link{sort_models}}, 
#' \code{\link{update.model}}, \code{\link{expand_formula}}.
#' 
#' \code{\link{models}()} is a more general version of `c.`(`multi`)`model` also accepting fitted models.
#' 
#' @examples
#' m1 <- model(lm(Sepal.Length ~ 1 , iris), label = "intercept")
#' m2 <- model(lm(Sepal.Length ~ . , iris), label = "linear")
#' m3 <- model(lm(Sepal.Length ~ .^2 , iris), label = "order2")  # without Species on rhs
#' 
#' # Combine models with c(...)
#' mm1 <- c(m1, m2, m3) 
#' mm1
#' 
#' # Specify elements to print:
#' print(mm1, what = c("class", "data"))
#' print(mm1, what = TRUE)
#' 
#' # Expand a model with multimodel(): random forests with different max.depth:
#' if (require(ranger)){
#'   mod_ranger <- model(ranger(Sepal.Length ~ ., iris), label = "ranger")
#'   multimodel(mod_ranger, max.depth = 1:10)
#' }
#' 
#' #' # Expand a model with multimodel(...)
#' # Joining models with three formulas may not give the expected result:
#' mm2 <- multimodel(m1, formula = list(Sepal.Length ~ 1, Sepal.Length ~ ., Sepal.Length ~ .^2))
#' mm2 
#' # The reason is that the formulas in the call above are used to *update* the existing one. 
#' # To *replace* the formula, use `I()` as in the call below:
#' mm3 <- multimodel(m1, formula = list(Sepal.Length ~ 1, I(Sepal.Length ~ .), I(Sepal.Length ~ .^2)))
#' mm3
#' # Also note the difference in the printed outputs of mm1 and mm2:
#' # The parameter table only contains parameters that were passed as '...' in multimodel().
#' 
#' # Three ways to attribute labels to models:
#' # 1) Use named arguments in c.model():
#' c(constant = m1, linear = m2, order2 = m3) 
#' # 2) replacement function label<-
#' mm1_lbls <- mm1
#' label(mm1_lbls) <- c("m1", "m2", "m3")
#' mm1_lbls
#' # 3) set_labels()
#' set_label(mm1, c("m1", "m2", "m3"))
#' 
#' # Combine mm1 and mm3:
#' print(c(mm1, mm3), what = "formula", n = 6)
#' 
#' # Unweighted and weighted model:
#' w <- runif(150)
#' mm3 <- multimodel(m1, weights = list(absent(), w))
#' label(mm3) <- c("unweighted", "weighted")
#' mm3
#' 
#' @name multimodel
NULL

#' @rdname multimodel
#' @export
multimodel <- function(x, ...) UseMethod("multimodel")

#' @rdname multimodel
#' @export
multimodel.model <- function(x, ..., prefix = label(x), 
                             expand = TRUE, max_n_model = getOption("expand_max_model"), 
                             param = TRUE, simplify = TRUE){
  nms <- names(list(...))
  if (length(list(...)) && 
       (is.null(nms) || !all(nzchar(nms)))){
    stop("All arguments in ", sQuote("..."), " must be named.")
  }
  outpar <- expand(..., .how = if (expand) "expand" else "join", 
                   n = max_n_model)
  nmodel <- nrow(outpar)
  model_list <- vector("list", nmodel)
  has_data_arg <- "data" %in% names(outpar)
  has_weights_arg <- "weights" %in% names(outpar)
  if (has_data_arg || has_weights_arg){
    expand_ind <- expand(..., .how = if (expand) "expand" else "join", n = max_n_model, 
                         .indices_only = TRUE)
    multimodel_applied_to_fitted <- sys.nframe()>2 &&
      as.character(sys.calls()[[sys.nframe()-2]][[1]]) == "multimodel.default"
    if (multimodel_applied_to_fitted){
      cl <- eval.parent(quote(match.call()))
    } else {
      cl <- match.call()
    }
  }
  if (has_data_arg)
    original_data_args <- decompose_arg(cl$data, length(list(...)$data))[expand_ind$data]
  if (has_weights_arg)
    original_weights_args <- decompose_arg(cl$weights, length(list(...)$weights))[expand_ind$weights]
  for (i in seq_along(model_list)){
    modif_args <- lapply(outpar[i, , drop = FALSE], "[[", 1)
    model_list[[i]] <- do.call(update, c(list(x), modif_args))
    if (has_data_arg && !inherits(modif_args$data, "unchanged"))
      model_list[[i]]$original_args$data <- original_data_args[[i]]
    if (has_weights_arg && !inherits(modif_args$weights, "unchanged"))
      model_list[[i]]$original_args$weights <- original_weights_args[[i]]
  }
  out <- do.call(c, c(model_list, list(simplify = simplify)))
  if (nmodel == 1){
    label(x) <- prefix
  } else { 
    label(out) <- paste0(prefix, if (nmodel>1) seq_len(nmodel))
  }
  outpar[] <- impute_unchanged(outpar, x)
  if (param) out$param <- param_table(outpar, nms = label(out))
  out
}

#' @rdname multimodel
#' @export
multimodel.default <- function(x, ..., prefix = "model", expand = TRUE, 
                               param = TRUE, simplify = TRUE){
  modelobj <- model(x, env = parent.frame())
  multimodel(modelobj, ..., prefix = prefix, expand = expand, 
             param = param, simplify = simplify)
}

#' @rdname multimodel
#' @export
multimodel.multimodel <- function(x, ...) x

# impute <unchanged> values in param_table (where model is the original model)
impute_unchanged <- function(partbl, mod){
  ro <- row(partbl)
  co <- col(partbl)
  for (i in seq_len(prod(dim(partbl)))){
    if (inherits(partbl[[co[i]]][[ro[i]]], "unchanged")){
      argnm <- colnames(partbl)[co[i]]
      partbl[[co[i]]][[ro[i]]] <- 
        eval(mod$call[[argnm]], c(list(data = mod$data), mod$saved_objects))
    }
  }
  partbl
}

# tentative: get quoted expression from '...' 
decompose_arg <- function(arg, l){
  if (is.call(arg) && as.character(arg[[1]]) == "list"){
    original <- as.list(arg[-1])
  } else {
    original <- lapply(seq_len(l), function(i) call("[[", arg, i))
  }
  original
}    



# ------------------------------------------------------------------------------

#' @rdname multimodel
#' @inheritParams print.model
#' @export
print.multimodel <- function(x, what = c("class", "formula", "data", "weights", "call", "cv_info"), 
                             abbreviate = TRUE, n = getOption("print_max_model"), 
                             param = TRUE, width = getOption("width"), ...){
  cat("--- A ", dQuote("multimodel"), " object containing ", length(x$models), " model", 
      if (length(x$models) != 1) "s", " ---\n", sep = "")
  prnt_multimodel_models(x, n = n, what = what, abbreviate = abbreviate, width = width, 
                         param = param, ...)
  invisible(x)
}


prnt_multimodel_models <- function(x, n, ..., what = TRUE, abbreviate = TRUE, 
                                   width = getOption("width"), param = TRUE, indent = "", 
                                   metric = NULL){ # metric only relevant in print.cv!
  lbl <- label(x)
  for (i in seq_len(min(n_model(x), n))){
    cat("\n", indent, sQuote(names(x$models)[[i]]), ":\n", sep = "")
    prnt_model_lines(x$models[[i]], indent = paste0(indent, "  "), 
                     what = what, abbreviate = abbreviate, width = width, label = lbl[[i]], 
                     metric = if (!is.null(metric)) names(metric)[[i]])
    if ("cv_info" %in% what) prnt_cv_info(x$models[[i]], ...)
  }
  if (n_model(x) > n){
    n_rest <- n_model(x) - n
    cat("\nand ", n_rest, " model", if (n_rest>1) "s", 
        " more, labelled:\n", sep = "")
    do.call(cat, 
      c(as.list(paste0(sapply(label(x)[-(1:n)], sQuote), 
                       rep(c(", ", ""), c(n_rest-1, 1)))), 
        sep = "", labels = " ", fill = width))
    cat("\n")
  }
  if (param && length(x$param)){
    cat(indent, "\nParameter table:\n", sep = "")
    print(x$param, n = n)
  }
  invisible(x)
}

# ------------------------------------------------------------------------------

#' @rdname multimodel
#' @export
c.model <- function(..., param = TRUE, simplify = TRUE){
  models <- list(...)
  simple <- sapply(models, inherits, "model")
  if (!all(simple)) 
    return(c.multimodel(..., param = param, simplify = TRUE))
  # Code from here is only executed if all ... are models
  if (!is.null(names(models))){
    has_arg_name <- nzchar(names(models))
    names(models)[!has_arg_name] <- sapply(models[!has_arg_name], label)
  } else {
    names(models) <- sapply(models, label)
  }
  names(models) <- make.unique(names(models), sep = "")
  if (length(models) == 1 && simplify){
    if (!is.null(names(models))) label(models[[1]]) <-  names(models)
    return(models[[1]])
  }
  # check models & data:
  if (length(unique(lapply(models, response))) > 1)
    stop("Models do not all have the same response.", call. = FALSE)
  if (length(unique(sapply(models, "[[", "response_type"))) > 1)
    stop("Models do not have the same response type.", call. = FALSE)
  for (i in seq_along(models)){
    models[[i]]$label <- NULL
  }
  structure(list(models = models, 
                 param = param_table(data.frame(row.names = names(models)))),
            class = "multimodel")
}
# any multimodel -> c.multimodel

#' @rdname multimodel
#' @export
c.multimodel <- function(..., param = TRUE, simplify = TRUE){
  models <- list(...)
  simple <- vapply(models, inherits, "model", FUN.VALUE = logical(1))
  if (any(!simple & !vapply(models, inherits, "multimodel", FUN.VALUE = logical(1))))
    stop("All arguments in ", sQuote("..."), " must be either models or multimodels.")
  models[simple] <- lapply(models[simple], multimodel, simplify = FALSE)
  # check models & responses:
  if (length(unique(lapply(models, "[[", "response"))) > 1)
    stop("Models do not have the same response.", call. = FALSE)
  if (length(unique(sapply(models, "[[", "response_type"))) > 1)
    stop("Models do not have the same response type.", call. = FALSE)
  # set names from dots
  if (!is.null(nms <- names(models))){
    setname <- nzchar(nms) & sapply(models, function(x) length(x$models)) == 1
    for (i in which(setname)) 
      models[[i]] <- set_label(models[[i]], nms[[i]])
  }  
  # output object
  out <- list()
  out$models <- unlist(lapply(unname(models), "[[", "models"), recursive = FALSE, use.names = TRUE)
  names(out$models) <- make.unique(names(out$models), sep = "")
  out$response <- models[[1]]$response
  if (param){
    out$param <- do.call(rbind, lapply(models, "[[", "param"))
  } else {
    param_table(data.frame(row.names = names(out$models)))
  }
  structure(out, class = "multimodel")
}

empty_multimodel <- function(param = FALSE){
  out <- structure(list(models = list()), 
                   class = "multimodel")
  if (param)
    out$param <- param_table(data.frame(row.names = character(0)))
  out
}
