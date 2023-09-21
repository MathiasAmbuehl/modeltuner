
#' Metrics
#' 
#' @description
#' A metric is a function having arguments \code{actual}, \code{predicted} and optionally \code{w} (weights)
#' and returning a non-negative real value.
#' 
#' The following metrics taken from package \CRANpkg{MetricsWeighted} are included in `modeltuner`:  
#' \itemize{
#' \item{Metrics for continuous response: 
#' \code{\link[MetricsWeighted]{rmse}}, \code{\link[MetricsWeighted]{mae}}, \code{\link[MetricsWeighted]{medae}} and  
#' \code{\link[MetricsWeighted]{mse}}.}
#' \item{Metrics for binary response: 
#' \code{\link[MetricsWeighted]{logLoss}} and \code{\link[MetricsWeighted]{classification_error}}.
#' }}
#' 
#' @details
#' The two metrics for binary response are slightly different from their `MetricsWeighted` counterparts:
#' \itemize{
#' \item{
#' \code{logLoss()}: Values of 0 and 1 in `predicted` are replaced by `eps` and `1-eps`, respectively,
#' before applying \code{MetricsWeighted::\link[MetricsWeighted]{logLoss}()}.
#' This prevents errors in case of prediction values of exactly 0 or 1.}
#' \item{
#' \code{classification_error()}: `predicted` is replaced by `ifelse(predicted>=cut_value, 1, 0)` 
#' before applying \code{MetricsWeighted::\link[MetricsWeighted]{classification_error}()}.
#' }}
#' 
#' @param actual Observed values.
#' @param predicted Predicted values.
#' @param w Optional case weights.
#' @param  ... Passed to other functions or methods.
#' 
#' @return
#' All of these functions return a numeric value.
#' 
#' @seealso \code{\link{default_metric}}
#' 
#' @examples
#' data(mcycle, package = "MASS")
#' mod <- lm(accel ~ times, mcycle)
#' actual <- mcycle$accel
#' pred <- predict(mod)
#' rmse(actual, pred)
#' medae(actual, pred)
#' 
#' # performance() uses these metrics:
#' performance(mod)
#' performance(mod, metric = "medae")
#' performance(mod, metric = list(med_abs_err = medae))
#' @name metrics
NULL


#' @rdname metrics
#' @importFrom MetricsWeighted rmse
#' @export
rmse <- MetricsWeighted::rmse

#' @rdname metrics
#' @importFrom MetricsWeighted mae
#' @export
mae <- MetricsWeighted::mae

#' @rdname metrics
#' @importFrom MetricsWeighted medae
#' @export
medae <- MetricsWeighted::medae

#' @rdname metrics
#' @importFrom MetricsWeighted rmse
#' @export
mse <- MetricsWeighted::mse

#' @rdname metrics
#' @param eps Adjustment value in `logLoss`.
#' 
#' @importFrom MetricsWeighted logLoss
#' @export
logLoss <- function(actual, predicted, w = NULL, ..., eps = .Machine$double.neg.eps){
  predicted <- pmin(pmax(predicted, eps), 1-eps)
  MetricsWeighted::logLoss(actual, predicted, w = NULL, ...)
}

#' @rdname metrics
#' @param cut_value Cut value for binary classification.
#' 
#' @importFrom MetricsWeighted classification_error
#' @export
classification_error <- function(actual, predicted, w = NULL, ..., cut_value = 0.5){
  predicted <- ifelse(predicted < cut_value, 0, 1)
  MetricsWeighted::classification_error(actual, predicted, w = NULL, ...)
}

