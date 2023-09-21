
#' Create folds (cross-validation groups)
#' 
#' @description
#' Obtains test sets (\dQuote{folds}) for cross-validation procedures with \code{\link{cv}()}.
#' The user inputs specifications of the *test* sets; the respective complements will be taken as *training* sets.
#' 
#' @param n An integer (corresponding to the number of observations in data) or a `data.frame`.
#' @param nfold Either an integer vector of length 1 or 2, or a numeric value between 0 and 1 
#' (see \dQuote{Details}).
#' @param folds A list of integer vectors (optional), predetermined group structure. 
#' If `folds` is given, the arguments `nfold` and `strata` will be ignored.
#' @param strata A vector or list of vectors: Strata for cross validation.
#' If specified and in accordance with `n`, the groups defined in `folds` are output, 
#' ignoring `n` and `nfold`.
#' 
#' @details
#' There are three ways to define the number of groups and group sizes in `nfold`:
#' \itemize{
#' \item{A positive integer value: *Complete* `nfold`-fold cross-validation; 
#' `1:n` is partitioned into `nfold` groups of (nearly) equal size at random.
#' }
#' \item{Two positive integer values, the second smaller than the first: *Incomplete* `nfold`-fold cross-validation; 
#' `1:n` is partitioned into `nfold[1]` groups of (nearly) equal size at random, but only 
#' `nfold[2]` of them are kept.
#' }
#' \item{A numeric value between 0 and 1: Hold-out validation; 
#' there will be only one test group that will contain approximately `n*nfold` indices from `1:n`.
#' }
#' } 
#' 
#' @return
#' A list of integer vectors defining the test sets, each of them a subset of `1:n` (or `1:nrow(n)` if `n` is a `data.frame`), 
#' of class \dQuote{folds}.
#' 
#' @seealso \code{\link{cv}}
#' 
#' @examples
#' make_folds(100, 10)       # Complete 10-fold CV
#' make_folds(100, c(10, 4)) # Incomplete 10-fold CV
#' make_folds(100, 0.3)      # Hold-out Validation with 30% test data
#' make_folds(100, c(3, 1))  # Almost the same as make_folds(100, 1/3)
#' make_folds(iris)          # data as input
#' make_folds(100, folds = list(1:10, 11:40, 41:100))  # Unequal group sizes
#' 
#' @export
make_folds <- function(n, nfold = getOption("cv_nfold"), folds = NULL, strata = NULL){
  if (is.data.frame(n) || inherits(n, "xgb.DMatrix")){
    n <- nrow(n)
  } else if (is.list(n)){
    n <- nrow(n$x)
  }
  if (n==0) stop(sQuote("n"), "must be >0")
  if (!is.null(folds)){
    ul <- unlist(folds, use.names = FALSE)
    ok <- is.list(folds) && length(folds) > 0 && all(ul %in% seq_len(n)) && all(lengths(folds)<n)
    if (!ok) stop("Invalid specification of ", sQuote("folds"))
    if (anyDuplicated(ul)) warning("The test sets overlap. Results may be misleading.")
    type <- if (length(folds) == 1) "simple"
      else if (setequal(ul, seq_len(n))) "complete"
      else if (all(ul %in% seq_len(n))) "incomplete"
      else "unknown" 
    out <- structure(folds, type = type, n = n, class = c("folds", "list"))
    return(out)
  }
  # check input
  type <- NULL
  if (!(length(nfold %in% 1:2)) || any(nfold<=0) || any(nfold>n)) type <- "invalid"
  else if (length(nfold) == 1 && nfold>0 && nfold <1) type <- "simple"
  else if (length(nfold) == 1 && nfold %% 1 == 0) type <- "complete"
  else if (length(nfold) == 2 && all(nfold %% 1 == 0) && nfold[2]==nfold[1]) type <- "complete"
  else if (length(nfold) == 2 && all(nfold %% 1 == 0) && nfold[2]<nfold[1]) type <- "incomplete"
  else type <- "invalid"
  if (type == "invalid") stop("Invalid specification of ", sQuote("nfold"))
  # Prepare strata (if required)
  stratified <- !is.null(strata)
  if (stratified){
    if (!is.list(strata)) strata <- list(strata)
    strata[] <- lapply(strata, as.character)
    strata <- do.call(paste, c(strata, list(sep = "\r")))
    strata <- as.integer(factor(strata, sample1(unique(strata))))
  }
  # create folds
  if (type == "simple"){
    if (!stratified){
      out <- list(sort(sample(n, round(n*nfold))))
    } else {
      out <- list(sample_stratified(n, round(n*nfold), strata))
    }
  } else {
    if (type == "complete") nfold <- rep(nfold, 2)
    fo <- rep(seq_len(nfold[[1]]), length.out = n)
    if (!stratified){
      foldid <- sample(fo)
    } else {
      stratfreqs <- tabulate(strata)
      strata1 <- rep(seq_along(stratfreqs), stratfreqs) # cuts across folds as defined in fo
      foldid <- integer(n)
      for (i in seq_along(stratfreqs)){
        foldid[strata == i] <- sample1(fo[strata1 == i])
      }
    }
    out <- split(seq_len(n), foldid)
    if (nfold[2]<nfold[1]) out <- sample1(out, nfold[2])
    out <- unname(lapply(out, sort.default))
  }
  # output
  class(out) <- c("folds", "list")
  attr(out, "type") <- type
  attr(out, "n") <- n
  attr(out, "stratified") <- stratified
  out
}

# stratified sampling
#   strata expected to be integer vector with values 1,...,n_value,
#   all having frequency >=1
#' @importFrom stats runif
sample_stratified <- function(n, size, strata){
  stratfreqs <- tabulate(strata)
  # Determine size of strata in sample
  strat_sizes <- stratfreqs / n * size
  difference <- sum(round(strat_sizes)) - size
  # Adjust if necessary
  if (difference < 0){
    diffs <- round(strat_sizes) - strat_sizes
    more <- head(order(diffs, runif(length(strat_sizes))),
                 -difference)
    strat_sizes[more] <- strat_sizes[more] + 1
  }
  if (difference > 0){
    diffs <- round(strat_sizes) - strat_sizes
    less <- head(order(diffs, runif(length(strat_sizes)), decreasing = TRUE), 
                 difference)
    strat_sizes[less] <- strat_sizes[less] - 1
  }
  # output
  strat_sizes <- round(strat_sizes)
  out <- lapply(seq_along(stratfreqs), function(i) sample1(which(strata == i), strat_sizes[[i]]))
  sort(unlist(out))
}

# x is expected to be a vector to sample from
sample1 <- function(x, ...){
  if (length(x) <= 1){
    x
  } else {
    sample(x, ...)
  }
}

#' @export
print.folds <- function(x, ...){
  ngrp <- length(x)
  n <- attr(x, "n")
  ntest <- lengths(x)
  nmodel <- n - ntest
  pl <- if (ngrp>1) "s" else ""
  cat("Validation procedure: ", 
      switch(attr(x, "type"),
             complete = "Complete k-fold Cross-Validation",
             incomplete = "Incomplete k-fold Cross-Validation",
             simple = "Simple Hold-out Validation", 
             unknwon = "Unknwon"),
      "\n", 
      paste0(format(c(        "  Number of obs in data:  ",
                              "  Number of test sets:    ",
                      sprintf("  Size of test set%s:", pl),
                      sprintf("  Size of training set%s:", pl)),
                    justify = "left"),
             format(c(n, ngrp, format_range(ntest), format_range(nmodel)),
                    justify = "right"),
             "\n"),
      if (isTRUE(attr(x, "stratified"))) "  Stratified groups\n",
      sep = "")
  invisible(x)
}
format_range <- function(x){
  rg <- range(x)
  if (diff(rg) == 1){
    paste0("~", round(mean(x)))
  } else {
    paste(unique(range(x)), collapse = "-")
  }
}

fold_indices <- function(folds){
  nfold <- length(folds)
  foldsizes <- lengths(folds)
  n <- sum(foldsizes)
  rep(seq(nfold), foldsizes)[match(seq_len(n), unlist(folds, use.names = FALSE))]
}

# ------------------------------------------------------------------------------

# Internal function: adjust folds in case of NAs
adjust_folds <- function(folds, i_remove){
  if (!length(i_remove)) return(folds)
  n <- attr(folds, "n")
  folds1 <- folds
  keep <- setdiff(seq_len(n), i_remove)
  new_n <- length(keep)
  folds1[] <- lapply(folds1, intersect, keep) # remove indices of removed obs
  folds1[] <- lapply(folds1, match, keep)     # adjust remaining obs numbers
  folds1 <- folds1[lengths(folds1) > 0]     # remove potential completely NA  folds
  attr(folds1, "n") <- new_n
  folds1
}
