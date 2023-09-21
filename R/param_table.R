
#' Table format with informative `print` method. 
#' 
#' @description
#' `param_table()` creates an table inheriting from class \dQuote{param_table},
#' generalizing the class \dQuote{data.frame} and having a more informative `print()` method for list columns.
#' It is designed for internal usage in the modeltuner package.
#' 
#' The *parameter table* of a (multi)model (element `param`) is a  \dQuote{param_table},
#' and the output of \code{\link{cv_performance}()} and \code{\link{performance}()}, too.
#' 
#' @param x A `data.frame`.
#' @param nms Row names to be attributed to the output table.
#' 
#' @details
#' The class \dQuote{param_table} has a `print`-method that is often more informative that `print.data.frame()`.
#' It has some similarities with `tibble` regarding the rendering of `list` columns, but it differs 
#' in that list elements of length 1 are displayed in a more informative way (see examples).
#' 
#' The class \dQuote{param_table} has additional methods of \code{\link{cbind}()}, \code{\link{rbind}()} and
#' \code{\link{as.data.frame}()}.
#' 
#' @return
#' A parameter table.
#' 
#' @examples
#' # Create a data.frame with a list as its single column
#' tbl <- data.frame(row.names = 1:4)
#' tbl$mixed_list <- list(pi, "anything", y~x, 1:10)
#' 
#' # print as data.frame:
#' tbl
#' 
#' if (require(tibble)){
#'   # print as tibble:
#'   tibble(tbl)
#' }
#'    
#' # print as param_table:
#' param_table(tbl)
# 
#' @export
param_table <- function(x, nms = rownames(x)){ 
  if (!inherits(x, "param_table")) x <- as.data.frame(x)
  # convert factors to character
  is_fact <- vapply(x, is.factor, FUN.VALUE = logical(1))
  x[is_fact] <- lapply(x[is_fact], as.character)
  # output
  rownames(x) <- nms
  structure(x, class = c("param_table", "data.frame")) 
}

#' @export
print.param_table <- function(x, digits = 5, n = getOption("print_max_row"), 
                              right = TRUE, justify = "right", ...){
  prt <- lapply(x, formatParam, digits = digits, right = right, justify = justify)
  prt <- mapply(c, colnames(x), prt, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  prt <- lapply(prt, format, justify = justify)
  lns <- do.call(paste, c(list(format(c("", rownames(x)), justify = "left")), prt))
  if ((nro <- nrow(x)) > n+1){
    lns <- c(head(lns, n-nro),
             paste0("... ", nro - n, " rows omitted (nrow=", nro, ")"))
  }
  writeLines(lns)
  invisible(x)
}
formatParam <- function(x, digits = 5, right = TRUE, justify = "right", ...){
  if (is.list(x)){
    formatList(x, digits = digits, right = right, justify = justify, ...)
  } else {
    format(x, digits = digits, right = right, justify = justify, ...)
  }
}
formatList <- function(x, digits = 5, right = TRUE, justify = "right", ...){
  if (length(x) == 0) return(character(0))
  form <- sapply(x, inherits, "formula")
  if (any(form)) justify <- "left" # formulas are left-aligned
  x[form] <- lapply(x[form], deparse_formula)
  vec <- sapply(x, is.vector)
  x[!vec] <- class_compact(x[!vec])
  len1 <- lengths(x) == 1
  num <- sapply(x, is.numeric)
  out <- character(length(x))
  out[len1 & num] <- format(unlist(x[len1 & num]), right = right, digits = digits, ...)
  out[len1 & !num] <- format(sapply(x[len1 & !num], format, justify = justify, ...), 
                             justify = justify, ...)
  out[!len1] <- paste0(class_compact(x[!len1]), "[", lengths(x[!len1]), "]")
  format(out, justify = justify, ..., )
}
deparse_formula <- function(x, width.cutoff = 60){
  x <- deparse(x, width.cutoff = width.cutoff)
  if (length(x)>1) x <- paste0(x[[1]], "...")
  x
}
class_compact <- function(x) paste0("<", sapply(x, cl1), ">")
cl1 <- function(x){
  out <- class(x)[[1]]
  if (out == "logical") out <- "logi"
  if (out == "integer") out <- "int"
  if (out == "numeric") out <- "num"
  if (out == "character") out <- "char"
  out
}

#' @export
rbind.param_table <- function(..., force_list = TRUE){
  dots <- list(...)
  parnms <- as.character(unique(unlist(lapply(dots, colnames), use.names = FALSE)))
  modelnms <- make.unique(unlist(lapply(dots, rownames)), sep = "")
  dots <- lapply(dots, add_unknown_cols, parnms, force_list = force_list)
  if (length(parnms) == 0)
    return(param_table(data.frame(row.names = modelnms)))
  # columns are converted to a list if necessary
  for (nm in colnames(dots[[1]])){
    list_vec_mix <- length(unique(sapply(dots, function(e) is.list(e[[nm]]))))>1
    if (list_vec_mix || different_modes(dots)){
      for (i in seq_along(dots))
        dots[[i]][[nm]] <- as.list(dots[[i]][[nm]])
    }
  }  
  out <- do.call(rbind.data.frame, dots)
  rownames(out) <- modelnms
  param_table(out)
}
add_unknown_cols <- function(df, nms, force_list = TRUE){
  if (all(nms %in% names(df))) return(df)
  if (force_list) df[] <- lapply(df, as.list)
  for (newvar in setdiff(nms, names(df)))
    df[[newvar]] <- rep(if (force_list) list(unknown()) else NA, NROW(df))
  df[nms]
}
different_modes <- function(x){
  if (length(x) <= 1) return(FALSE)
  modes <- sapply(x, mode)
  !all(modes == modes[[1]])
}

#' @export
cbind.param_table <- function(...){
  dots <- list(...)
  out <- dots[[1]]
  if (length(dots) > 1) for (i in seq(2, length(dots))){
    if (length(dots[[i]]) == 0) next
    names(dots[[i]]) <- make.unique(c(names(out), names(dots[[i]])))[ncol(out) + seq_along(dots[[i]])]
    out[names(dots[[i]])] <- dots[[i]]
  }
  out
}


#' @export
as.data.frame.param_table <- function(x, ...){
  listCols <- sapply(x, is.list)
  if (any(listCols))
    x[listCols] <- lapply(x[listCols], simplify2array)
  attributes(x) <- attributes(x)[c("names", "row.names", "class")]
  class(x) <- "data.frame"
  x
}
