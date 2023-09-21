
# Function .onLoad
.onLoad <- function(libname, pkgname){
  options(cv_iter = crit_min(), cv_nfold = 10,
          cv_verbose = interactive(), cv_show_se = FALSE, 
          expand_max_model = 30, 
          print_max_model = 3, print_max_row = 30, 
          print_rows_evaluation_log = 6)
}


#' List all options defined in package \dQuote{modeltuner}
#' 
#' `modeltuner_options()` displays the current values of all options set in the package. 
#' These options define default values for certain function parameters.
#' 
#' \itemize{
#' \item{\strong{cv_nfold} (value at package startup: \code{=10}): Default value of argument `nfold` in \code{\link{cv}()}, \code{\link{tune}()} and 
#' \link{stepwise} functions.}
#' \item{\strong{cv_verbose}  (\code{=\link{interactive}()} at startup): Default value of argument `verbose` in \code{\link{cv}()},}
#' \item{\strong{cv_show_se} (\code{=FALSE} at startup): Default value of argument `se` in \code{\link{print.performance}()} and 
#' \code{\link{print.evaluation_log}()} and default of `errorbars` in \code{\link{plot.evaluation_log}()},}
#' \item{\strong{cv_iter} (\code{=\link{crit_min}()} at startup): Default value of argument `iter` in \code{\link{cv}()} applied to models of class
#' \dQuote{\link{fm_xgb}} or \dQuote{\link{fm_glmnet}},}
#' \item{\strong{expand_max_model}  (\code{=30} at startup): Default value of argument `max_n_model` in 
#' \code{\link{multimodel.model}()} and \code{\link{tune.model}()},}
#' \item{\strong{print_max_model} (\code{=3} at startup): Default value of `n` in \code{\link{print.multimodel}()}, \code{\link{print.cv}()} and
#' \code{\link{print.evaluation_log}()},}
#' \item{\strong{print_max_row} (\code{=30} at startup): Default value of `n` in \code{\link{print.performance}()} and 
#' \code{print.\link{extract_pref_iter}()}},
#' \item{\strong{print_rows_evaluation_log} (\code{=6} at startup): Default value of `n_row` in \code{\link{print.evaluation_log}()}.}
#' }
#' 
#' @return Returns a list of option settings.
#' 
#' @examples
#' modeltuner_options()
#' 
#' @export
modeltuner_options <- function()
  options()[c("cv_nfold", "cv_verbose", "cv_show_se", "cv_iter", 
              "expand_max_model", "print_max_model", "print_max_row", 
              "print_rows_evaluation_log")]

