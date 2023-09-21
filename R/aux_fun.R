
#' @importFrom utils modifyList
modify_call <- function (call, val, ...){
  # "unchanged" elements: remove from val
  unch <- sapply(val, inherits, "unchanged")
  if (any(unch)) {
    val <- val[!unch]
    if (length(val) == 0) 
      return(call)
  }
  mod <- modifyList(as.list(call), val, ...)
  # "absent" elements
  emp <- sapply(val, inherits, "absent")
  if (any(emp) && !is.null(names(emp))) 
    mod[names(emp[emp])] <- NULL
  # "null" elements
  nul <- sapply(val, inherits, "null")
  if (any(nul) && !is.null(names(nul))) 
    mod[names(nul[nul])] <- list(NULL)
  as.call(mod)
}

substitute_call <- function(call, mod){
  eval(call("substitute", call, env = mod))  
}

# all args in ... must be named
expand <- function(..., .how = c("expand", "join"), n = 30, 
                   .indices_only = FALSE){
  dots <- list(...)
  # if some element in dots is not a vector: try to fix (example: absent() instead of list(absent())
  isvec <- sapply(dots, is.vector)
  dots[!isvec] <- lapply(dots[!isvec], list)
  if (length(dots) == 0)
    return(data.frame(row.names = ""))
  nms <- names(dots)
  .how <- match.arg(.how)
  if (.how == "expand"){
    out <- do.call(expand.grid, 
      c(lapply(rev(dots), seq_along), 
        list(KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)))
    out <- rev(out)
    if ((nro <- nrow(out)) > n) {
      out <- out[sort.default(sample(seq_len(nro), n)), , drop = FALSE]
      message("Selecting max_n_model=", n, 
              " models at random (out of ", nro, ").") 
    }
  } else {
    ll <- lengths(dots)
    if (!all(ll == ll[1])){
      stop("Cannot join vectors of different length: \n", 
           paste0("  ", format(paste0(nms, ":")), " length=" , ll, 
                  collapse = "\n"), 
           call. = FALSE)
    }
    out <- do.call(data.frame, lapply(dots, seq_along))
  }
  if (.indices_only) return(out)
  for (i in seq_along(dots)){
    out[[i]] <- dots[[i]][out[[i]]]
  }
  out
}
# expnd <- expand(num = c(3, 1, 7), letters = head(LETTERS, 2), list = list("hello", pi))
# expnd
# str(expnd)

# ------------------------------------------------------------------------------

# Extract call from a model object
get_call <- function(x){
  if (is.call(x)){
    cl <- x
  } else {
    cl <- stats::getCall(x)
  }
  if (is.symbol(cl[[1]])){
    match.call(match.fun(cl[[1]]), cl)
  } else {
    cl_ <- cl
    match.call(eval(cl_[[1]]), 
               cl)
  }
}

#' @importFrom stats formula as.formula
get_formula <- function(x, call = get_call(x), env = parent.frame()){
  f <- try(formula(x), silent = TRUE)
  if (length(f) && as.character(f[[1]]) == "~"){
    if (length(f) == 3) return(f)
    stop("formula must be two-sided.")
  }
  f <- try(call$formula, silent = TRUE)
  if (inherits(f, "formula") && length(f) == 3) return(f)
  f <- try(eval(f, env))
  if (inherits(f, "formula") && length(f) == 3) return(f)
  if (is.list(x) || "formula" %in% names(x) || is.null(x$formula)) return(NULL)
  stop("could not determine model formula from ", call)
}

# Extract data from call
get_data <- function(x, call, environment = parent.frame()){
  call <- as.call(call)
  out <- eval(call$data, environment)
  if (is.null(out)){
    # case no data argument, try to get vars from environment
    if (length(form <- call$formula) == 3){
      n <- length(eval(form[[2]], environment))  # length of evaluated response
      out <- data.frame(row.names = seq_len(n))
    # vars <- mget(all.vars(call$formula), envir = environment)
    # if (length(unique(lengths(vars))) == 1) out <- do.call(data.frame, vars)
    }
  }
  ok <- chk_data(out)
  if (!ok){ # try to recover data -- this alternative approach may help in case of a pipe
    trms <- try(terms(x), silent = TRUE)
    if (!inherits(trms, "try-error")){
      trms_env <- try(environment(trms), silent = FALSE) 
      if (is.environment(trms_env)){
        out <- eval(call$data, trms_env)
      }
    }
  }
  ok <- chk_data(out)
  if (!ok) stop("Could not retrieve model from ", deparse(call)[[1]])
  if (is.list(out) && !is.data.frame(out) && is.null(nrow(out$x))){ 
    dim(out$x) <- c(NROW(out$x), NCOL(out$x))
    class(out) <- c("modeldata_xy", class(out))
  }
  out
}
chk_data <- function(d){
  dd <- dim(d)
  if (length(dd) == 2 && is.numeric(dd) && length(dd) == 2) return(TRUE) # case data.frame or matrix
  if (is.list(d) && !is.data.frame(d) && # case list of x and y
      c("x", "y") %in% names(d) && NROW(d$x) == length(d$y))
    return(TRUE)
  FALSE
}

get_response <- function(formula){
  if (is.null(formula)) return("y")
  if (length(formula) != 3) stop("formula must be two-sided.")
  deparse(formula[[2]])
}

get_objects <- function(nms, env, ignore = character(0)){ 
  if (length(nms)) nms <- nms[vapply(nms, exists, envir = env, FUN.VALUE = logical(1))]
  if ("T" %in% nms && identical(eval(T, env), TRUE)) nms <- setdiff(nms, "T")
  if ("F" %in% nms && identical(eval(F, env), FALSE)) nms <- setdiff(nms, "F")
  if (length(nms)){ 
    mget(nms, env, inherits = TRUE)
  } else list()
}

