
#' Get and set the last cv object
#' 
#' @description 
#' \code{last_cv()} returns the last generated \dQuote{cv} object.
#' This allows recovering a cross-validation if you have (for example) used the 
#' method \code{\link{cv_performance}.multimodel()} directly, in which case
#' the \dQuote{cv} is not saved.
#' 
#' \code{set_last_cv(value)} causes that \code{value} will be returned by \code{last_cv()}. 
#' 
#' @param value Any object of class \dQuote{cv}.
#' 
#' @return
#' `last_cv()` returns a \code{\link{cv}} object,
#' 
#' @seealso \code{\link{cv}}
#' 
#' @examples
#' data(mcycle, package = "MASS")
#' m <- lm(accel ~ times, mcycle)
#' cv_performance(m)  # m %>% cv %>% cv_performance
#' last_cv()      # recovers unsaved output from cv() in previous call
#' 
#' @name last_cv
NULL

.save_last_cv <- function() {
  .last_cv <- NULL
  list(
    get = function() .last_cv,
    set = function(value) .last_cv <<- value
  )
}
.last_cv <- .save_last_cv()

#' @rdname last_cv
#' @export
last_cv <- function() .last_cv$get()

#' @rdname last_cv
#' @export
set_last_cv <- function(value) .last_cv$set(value)

