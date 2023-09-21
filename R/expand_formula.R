
#' Expand a formula
#' 
#' Replicate a formula with varying parameter values.
#' 
#' @param formula A formula.
#' @param \dots Named parameters to be expanded.
#' @inheritParams multimodel
#' 
#' @return A list as `formula`s.
#' 
#' @examples
#' expand_formula(y ~ ns(x, df = k), k = 1:10)
#' expand_formula(y ~ I(x^e1) + I(z^e2), e1 = 1:2, e2 = 1:3)
#' expand_formula(y ~ I(x^e1) + I(z^e2), e1 = 1:3, e2 = 1:3, expand = FALSE)
#' 
#' @export
expand_formula <- function(formula, ..., expand = TRUE){
  expanded <- expand(..., .how = if (expand) "expand" else "join")
  out <- sapply(seq_len(nrow(expanded)), 
    function(i) eval(call("substitute", formula, expanded[i, , drop = FALSE])))
  lapply(out, as.formula, env = environment(formula))
}

