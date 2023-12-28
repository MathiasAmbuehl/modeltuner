
#' Generate and cross-validate models resulting from adding or removing variables and stepwise procedures
#' 
#' @description
#' `step_extend()` combines all models resulting from adding one variable to a *base model* into a multimodel and subjects it to `cv()`.
#' `step_forward()` applies `step_extend()` repeatedly, selecting the best model with respect to test error at each step, 
#' thus performing a forward selection of variables.
#' 
#' `step_reduce()` combines all models resulting from removing one variable from a *full model* into a multimodel and subjects it to `cv()`.
#' `step_backward()` applies `step_reduce()` repeatedly, selecting the best model w.r.t. test error at each step,
#'  thus performing a backward  elimination of variables.
#' 
#' `best_subset()` combines submodels of the *full model* in a multimodel and subjects it to `cv()`. 
#' The desired range of the model sizes (number of effects) to include is specified in the parameter `nvars`.
#' 
#' @details
#' `formula1` `formula2` must be *nested* model formulas, i.e. one of the two formulas must include all terms present in the other.
#' They define the range of models to be considered: The larger of the two defines the *full model*, the other is taken as the *base model*.
#' 
#' By default, `formula1` and `formula2` are used to *update* the original model formula. 
#' Enclose a formula in `I()` to *replace* the model's formula.
#' This distinction is relevant whenever you specify a formula including a dot.
#' See the \dQuote{Details} section and examples in \code{?\link{update.model}}.
#' 
#' @param x Object of class \dQuote{model} or a fitted model.
#' @param formula1,formula2 Two *nested* model formulas defining the range of models to be considered. 
#' The larger of the two is taken as the *full model*, the simpler as the *base model*. See the \dQuote{Details} section.
#' @param max_step (`step_forward`, `step_backward`) Integer: Maximal number of steps.
#' @param include_full Logical: Whether to include the full model in the output.
#' @param include_base Logical: Whether to include the base model in the output.
#' @param cv (`step_extend`, `step_reduce`, `best_subset`) Logical: Run `cv` or just return the multimodel?
#' @param steps (`step_extend`, `step_reduce`) Integer: Number of variables to add/remove. Default: 1.
#' @param nvars (`best_subset`) Integer vector defining the number of variables.
#' @param verbose Logical: Output information on execution progress in console?
#' @param \dots Dots go to `cv()` in `step_extend()` and `step_reduce()` (provided `cv=TRUE`),
#' and to `tune()` in `step_forward()` and `step_backward()`.
#' @inheritParams cv
#' 
#' @return
#' All of these functions return an object of class \dQuote{\link{cv}}."
#' 
#' @seealso \code{\link{multimodel}()}, \code{\link{update.model}()}, \code{\link{null_formula}()}, 
#' \code{\link{cv}()}, \code{\link{tune}()}
#' 
#' @examples
#' mod <- model(lm(Sepal.Length ~ ., iris), 
#'              label = "sepLen")
#'              
#' # Add variables to base model
#' oneVarModels <- step_extend(mod)
#' cv_performance(oneVarModels)
#' 
#' # step_forwamrd
#' cv_fwd <- step_forward(mod)
#' cv_performance(cv_fwd)
#' 
#' # Remove variables from full model
#' mod |> step_reduce() |> cv_performance()
#' mod |> step_backward() |> cv_performance()
#' 
#' # best subset
#' mod |> best_subset(nvar = 2:3) |> cv_performance()
#' 
#' @name stepwise
NULL


#' @rdname stepwise 
#' @export
step_extend <- function(x, ...) UseMethod("step_extend")


# 'workhorse' function to be used in extension-type step_* functions
.step_ext_int <- function(x, base_formula, variables, steps, 
                          include_base = FALSE, include_full = FALSE, 
                          cv = TRUE, ...){
  steps <- steps[steps %in% seq_along(variables)]
  forms <- formula_extensions(base_formula, variables, steps = steps, include_base = include_base,
                              include_full = include_full)
  if (length(forms)>0){
    mm <- multimodel(x, formula = forms, simplify = FALSE, max_n_model = Inf)
    label(mm) <- names(forms)
  } else {
    mm <- empty_multimodel() #multimodel(x, simplify = FALSE)
  }
  if (!cv) return(mm)
  if (is.null(mm)) stop("No model to apply cv() to.", call. = FALSE)
  cv(mm, ...)
}
# Same for reduction-type procedures
.step_red_int <- function(x, full_formula, variables, steps, 
                          include_base = FALSE, include_full = FALSE, 
                          cv = TRUE, ...){
  steps <- steps[steps %in% seq_along(variables)]
  forms <- formula_reductions(full_formula, variables, steps = steps, include_base = include_base,
                              include_full = include_full)
  if (length(forms)>0){
    mm <- multimodel(x, formula = forms, simplify = FALSE, max_n_model = Inf)
    label(mm) <- names(forms)
  } else {
    mm <- empty_multimodel() # multimodel(x, simplify = FALSE)
  }
  if (!cv) return(mm)
  if (is.null(mm)) stop("No model to apply cv() to.", call. = FALSE)
  cv(mm, ...)
}


#' @rdname stepwise 
#' @export
step_extend.model <- function(x, formula1 = null_formula(x), formula2 = formula(x), 
                              steps = 1L, include_full = FALSE, include_base = FALSE, cv = TRUE, ...){
  if (isTRUE(all.equal(formula1, formula2))){
    warning("formula1 and formula2 are identical")
    if (!include_full && !include_base) stop("Model can not be extended or reduced")
  }
  comp <- compare_formulas(formula1, formula2, model = x)
  .step_ext_int(x, variables = comp$difftrms, steps = steps, base_formula = comp$smaller, 
                include_full = include_full, include_base = include_base, 
                cv = cv, ...)
}


#' @rdname stepwise 
#' @export
step_extend.default <- function(x, ...){
  step_extend(model(x, env = parent.frame()), ...)
}


# ------------------------------------------------------------------------------

#' @rdname stepwise 
#' @export
step_forward <- function(x, ...) UseMethod("step_forward")

#' @rdname stepwise 
#' @export
step_forward.model <- 
  function(x, formula1 = null_formula(x), formula2 = formula(x), 
           max_step = 10, include_base = TRUE, include_full = FALSE,
           nfold = getOption("cv_nfold"), folds = NULL, verbose = getOption("cv_verbose"), 
           ...){
  folds <- make_folds(length(response(x)), nfold, folds)
  if (isTRUE(all.equal(formula1, formula2))){
    warning("formula1 and formula2 are identical")
    if (!include_base) stop("Model can not be extended or reduced")
  }
  comp <- compare_formulas(formula1, formula2, model = x)
  current_model <- update(x, formula = comp$smaller, label = "base")
  if (verbose){
    message("step_forward, initial model:\n", 
            paste0("  ", deparse(current_model$formula, width.cutoff = min(max(getOption("width")-20, 50), 120)), 
                   collapse = "\n"))
    verbose <- structure(TRUE, final_msg = "", appendLF = FALSE)
  }
  variables <- comp$difftrms
  if (include_base){
    out <- cv(multimodel(current_model, formula = formula(current_model), simplify = FALSE), 
              folds = folds, verbose = verbose)
  } else {
    out <- empty_cv(folds = folds)
  }
  if (max_step > 0) for (st in seq_along(variables)){
    cv_ <- .step_ext_int(current_model, variables = comp$difftrms, steps = 1, 
                         base_formula = formula(current_model), include_base = FALSE, 
                         folds = folds, verbose = verbose)
    if (n_model(cv_) == 0){
      variables <- character(0)
      break
    }
    perf_ <- cv_performance(cv_)
    if (!any(is.finite(perf_[[paste0("test_", names(attr(perf_, "metric")))]]))){
      warning("Execution of step_forward() stopped at step ", st, 
              ", as none of the candidate models had finite test performance.")
      break
    }
    current_model <- tune(cv_, ...)
    out <- c(out, subset(cv_, label(current_model)))
    if (verbose){
      msg <- paste0("\rstep_forward, step ", st, ": . ~ . ", sub("^\\+", "+ ", label(current_model)))
      message(msg)
    }
    variables <- setdiff(variables, sub("^\\+", "", label(current_model)))
    if (st >= max_step) break
  }
  if (include_full && length(variables)>0){
    cv_full <- cv(multimodel(set_label(x, "full"), formula = comp$larger, simplify = FALSE), 
                  folds = folds, verbose = verbose)
    out <- c(out, cv_full)
  }
  out
}

#' @rdname stepwise 
#' @export
step_forward.default <- function(x, ...){
  step_forward(model(x, env = parent.frame()), ...)
}

# ------------------------------------------------------------------------------

#' @rdname stepwise 
#' @export
step_reduce <- function(x, ...) UseMethod("step_reduce")


#' @rdname stepwise 
#' @importFrom stats setNames
#' @export
step_reduce.model <- function(x, formula1 = null_formula(x), formula2 = formula(x), 
                              steps = 1L, include_full = FALSE, include_base = FALSE, 
                              cv = TRUE, ...){
  if (isTRUE(all.equal(formula1, formula2))){
    warning("formula1 and formula2 are identical")
    if (!include_full && !include_base) stop("Model can not be extended or reduced")
  }
  comp <- compare_formulas(formula1, formula2, model = x)
  nvar <- length(comp$difftrms)
  nvar - steps
  .step_red_int(x, variables = comp$difftrms, steps = steps, full_formula = comp$larger, 
                include_full = include_full, include_base = include_base, 
                cv = cv, ...)
}

#' @rdname stepwise 
#' @export
step_reduce.default <- function(x, ...){
  step_reduce(model(x, env = parent.frame()), ...)
}

# ------------------------------------------------------------------------------

#' @rdname stepwise 
#' @export
step_backward <- function(x, ...) UseMethod("step_backward")

#' @rdname stepwise 
#' @export
step_backward.model <- function(x, formula1 = null_formula(x), formula2 = formula(x), 
                                max_step = 10, include_full = TRUE, include_base = FALSE, 
                                nfold = getOption("cv_nfold"), folds = NULL, verbose = getOption("cv_verbose"), ...){
  folds <- make_folds(length(response(x)), nfold, folds)
  if (isTRUE(all.equal(formula1, formula2))){
    warning("formula1 and formula2 are identical")
    if (!include_base) stop("Model can not be extended or reduced")
  }
  comp <- compare_formulas(formula1, formula2, model = x)
  current_model <- update(x, formula = comp$larger, label = "full")
  if (verbose){
    message("step_backward, initial model:\n", 
            paste0("  ", deparse(current_model$formula, width.cutoff = min(max(getOption("width")-20, 50), 120)), 
                   collapse = "\n"))
    verbose <- structure(TRUE, final_msg = "", appendLF = FALSE)
  }
  variables <- comp$difftrms
  if (include_full){
    out <- cv(multimodel(current_model, formula = formula(current_model), simplify = FALSE), 
              folds = folds, verbose = verbose)
  } else {
    out <- empty_cv(folds = folds)
  }
  if (max_step > 0) for (st in seq_along(variables)){
    cv_ <- .step_red_int(current_model, full_formula = formula(current_model), 
                         variables = variables, steps = 1, include_full = FALSE, 
                         folds = folds, verbose = verbose)
    if (n_model(cv_) == 0){
      variables <- character(0)
      break
    }
    perf_ <- cv_performance(cv_)
    if (!any(is.finite(perf_[[paste0("test_", names(attr(perf_, "metric")))]]))){
      warning("Execution of step_backward() stopped at step ", st, 
              ", as none of the candidate models had finite test performance.")
      break
    }
    current_model <- tune(cv_, ...)
    out <- c(out, subset(cv_, label(current_model)))
    if (verbose){
      msg <- paste0("\rstep_backward, step ", st, ": . ~ . ", 
                    sub("^\\-", "- ", label(current_model)))
      message(msg)
    }
    variables <- setdiff(variables, sub("^\\-", "", label(current_model)))
    if (st >= max_step) break
  }
  if (include_base && length(variables)>0){
    cv_base <- cv(multimodel(set_label(x, "base"), 
                             formula = comp$smaller, simplify = FALSE), 
                  folds = folds, verbose = verbose)
    out <- c(out, cv_base)
  }
  out
}

#' @rdname stepwise 
#' @export
step_backward.default <- function(x, ...){
  step_backward(model(x, env = parent.frame()), ...)
}

# ------------------------------------------------------------------------------

#' @rdname stepwise 
#' @export
best_subset <- function(x, ...) UseMethod("best_subset")

#' @rdname stepwise 
#' @export
best_subset.model <- function(x, formula1 = null_formula(x), formula2 = formula(x), 
                              nvars = 1:5, include_base = any(nvars == 0), include_full = FALSE, 
                              cv = TRUE, ...){
  if (isTRUE(all.equal(formula1, formula2))){
    warning("formula1 and formula2 are identical")
    if (!include_full && !include_base) stop("Model can not be extended or reduced")
  }
  comp <- compare_formulas(formula1, formula2, model = x)
  .step_ext_int(x, variables = comp$difftrms, steps = nvars, base_formula = comp$smaller, 
                include_full = include_full, include_base = include_base, 
                cv = cv, ...)
}

#' @rdname stepwise 
#' @export
best_subset.default <- function(x, ...){
  best_subset(model(x, env = parent.frame()), ...)
}
