#' @export
terms.model <- function(x, simplify = TRUE, ...){
  out <- terms(x$formula, data = x$data, simplify = simplify, ...)
  out
}

#' \dQuote{Null formula} of a model
#' @description 
#' Returns the formula of an intercept-only model (\code{. ~ 1}) if the original formula `x` contains an intercept 
#' and the constant zero-model formula (\code{. ~ 0}) otherwise.
#' @param x Any object having a \code{\link{formula}} method and an \code{\link{update}} method 
#' accepting updating formula as second argument.
#' @return Returns a simple (\dQuote{null}) formula.
#' @examples
#' null_formula(lm(Sepal.Length ~ ., iris))
#' @seealso Used as default value of argument \dQuote{formula1} in \link{stepwise} functions.
#' @export
null_formula <- function(x){
    form <- formula(x)
  if (as.logical(attr(terms(form), "intercept"))){
    update(form, . ~ 1)
  } else {
    update(form, . ~ 0)
  }
}


#' Add terms to existing formula. Designed as aux fun used within formula_extensions
#' @param formula a formula
#' @param vars char vector of variable names or more generally terms; 
#' extend_formula(form, var="") returns form unchanged.
#' @param simplify_to_formula  
#' @param reduce 
#' @return A formula with vars added to formula or 
#' NULL if vars contains no variable that is not present in formula or
#' a 'terms' obj if simplify_to_formula=FALSE
#' @importFrom stats reformulate
#' @noRd
extend_formula <- function(formula, vars, simplify_to_formula = TRUE, 
                           reduce = FALSE){
  vars <- unname(vars)
  if (length(vars)==0) return(NULL)
  if (identical(vars, "")){
    update_form <- . ~ .
  } else {
    update_form <- reformulate(union(".", vars), response = ".")
    if (reduce) update_form[[3]] <- plus2minus(update_form[[3]])
  }
  form <- update(formula, update_form)
  out <- terms(form, simplify = TRUE)
  if (isTRUE(all.equal(out, formula)) && !identical(vars, "")) return(NULL)
  if (simplify_to_formula) out <- formula(out)
  out
}
plus2minus <- function(x){
  if (length(x) == 3 && as.character(x[[1]]) == "+"){
    call("-", plus2minus(x[[2]]), plus2minus(x[[3]]))
  } else x
}  
  
#' Return all possible extensions of formula resulting from adding step variables from vars
#' @param trms inherits from "formula" or "terms"
#' @param vars char vector of variable names
#' @param steps integer scalar
#' @param include_base whether to include the base model in the output
#' @param include_full whether to include the full model in the output
#' (default is by increasing complexity)
#' @return A list of extended formulas
#' @examples
#' modeltuner:::formula_extensions(y ~ a + b + c, c("d", "e"))
#' modeltuner:::formula_reductions(y ~ a + b + c + d + e, c("d", "e"))
#' @importFrom utils combn
#' @noRd
formula_extensions <- function(trms, vars, steps = 1L, include_base = any(steps == 0), 
                               include_full = FALSE){
  trms <- terms(trms)
  force(include_base)
  steps <- setdiff(steps, 0)
  if (!include_base && !include_full && (length(vars)==0 || length(steps)==0)) 
    return(list())
  if (include_full) steps <- union(steps, length(vars))
  newtrms <- setdiff(labels(extend_formula(trms, vars, simplify_to_formula = FALSE)), 
                     labels(trms))
  steps <- sort(steps[steps<=length(newtrms)])
  newtrms <- unlist(lapply(steps, combn, x = newtrms, simplify = FALSE), 
                    recursive = FALSE)
  if (include_base) newtrms <- c(list(""), newtrms)
  out <- sapply(newtrms, extend_formula, formula = trms,  
                simplify = FALSE, USE.NAMES = TRUE)
  names(out) <- sapply(newtrms, function(x) paste0(ifelse(nzchar(x), "+", "base"), x, collapse = ""))
  if (include_full) names(out)[lengths(newtrms) == length(vars)] <- "full"
  out <- Filter(Negate(is.null), out)
  out
}  

formula_reductions <- function(trms, vars, steps = 1L, include_full = TRUE, 
                               include_base = FALSE){
  trms <- terms(trms)
  force(include_full)
  steps <- setdiff(steps, 0)
  if (!include_base && !include_full && (length(vars)==0 || length(steps)==0)) 
    return(list())
  if (include_base) steps <- union(steps, length(vars))
  steps <- sort(steps[steps<=length(vars)])
  rmtrms <- unlist(lapply(steps, combn, x = vars, simplify = FALSE), 
                   recursive = FALSE)
  if (include_full) rmtrms <- c(list(""), rmtrms)
  out <- sapply(rmtrms, extend_formula, formula = trms, reduce = TRUE,
                simplify = FALSE, USE.NAMES = TRUE)
  names(out) <- sapply(rmtrms, function(x) paste0(ifelse(nzchar(x), "-", "full"), x, collapse = ""))
  if (include_base) names(out)[lengths(rmtrms) == length(vars)] <- "base"
  out <- Filter(Negate(is.null), out)
  out
}  


# aux fun: return terms.labels in a standardized form
# (used to avoid same interaction having different order (e.g. a:b and b:a) in different formulas)
std_terms <- function(trms, vars){
  lbls <- attr(trms, "term.labels")  
  if (length(lbls) == 0){  # case no variables in formula
    out <- character(0)
  } else if (all(attr(trms, "order") <= 1)){ # case no interacations in formula
    out <- lbls
  } else {
    strms <- strsplit(lbls, ":", fixed = TRUE)
    ustrms <- unlist(strms)
    rstrms <- relist(ustrms[order(rep(seq_along(strms), lengths(strms)), 
                                  match(ustrms, vars))], 
                     strms)
    out <- sapply(rstrms, paste, collapse = ":")
  }
  if (attr(trms, "intercept") > 0 && "1" %in% vars)
    out <- union("1", out)
  out
}

# aux fun: check if one formula is a simplification of the other, return relevant information in a list
compare_formulas <- function(f1, f2, model){
  f1 <- if (inherits(f1, "AsIs")) f1 else update(formula(model), f1)
  f2 <- if (inherits(f2, "AsIs")) f2 else update(formula(model), f2)
  f1 <- terms(f1, data = model$data)
  f2 <- terms(f2, data = model$data)
  objnms <- union(all.vars(f1), all.vars(f2))
  ok <- objnms %in% c(names(model$data), 
                      ls(model$saved_objects, all.names = TRUE))
  if (!all(ok)){
    stop("Unknown variable(s) in formula: ", paste0(objnms[!ok], collapse = ","))
  }
  resp1 <- with(attributes(f1), as.character(variables)[-1][response])
  resp2 <- with(attributes(f2), as.character(variables)[-1][response])
  if (resp1 != resp2) stop("formulas have different responses")
  vars1 <- with(attributes(f1), 
    c(if (intercept) "1",
      as.character(setdiff(rownames(factors), resp1))))
  vars2 <- with(attributes(f2), 
    c(if (intercept) "1",
      as.character(setdiff(rownames(factors), resp2))))
  vars <- union(vars1, vars2)
  stdtrms1 <- std_terms(f1, vars)
  stdtrms2 <- std_terms(f2, vars)
  comp_1in2 <- all(stdtrms1 %in% stdtrms2)
  comp_2in1 <- all(stdtrms2 %in% stdtrms1)
  out <- list()
  if (comp_1in2 && comp_2in1){
    out$comparison <- 0
    out$larger <- out$smaller <- char2formula(stdtrms1, resp1, environment(f1))
  } else if (comp_1in2){
    out$comparison <- 1
    out$larger <- char2formula(stdtrms2, resp2, environment(f2))
    out$smaller <- char2formula(stdtrms1, resp1, environment(f1))
    out$difftrms <- setdiff(stdtrms2, stdtrms1)
  } else if(comp_2in1){
    out$comparison <- -1
    out$larger <- char2formula(stdtrms1, resp1, environment(f1))
    out$smaller <- char2formula(stdtrms2, resp2, environment(f2))
    out$difftrms <- setdiff(stdtrms1, stdtrms2)
  } else {
    stop("formulas are not nested.")
  }
  out
}
char2formula <- function(x, resp, env){
  if (length(setdiff(x, "1")) == 0){
    f <- as.formula(paste0(resp, "~ 1", if (!length(x)) "-1"), 
                    env = env)
    return(f)
  }
  reformulate(setdiff(x, "1"), response = resp, 
              intercept = "1" %in% x, env = env)
}

