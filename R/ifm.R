
#' Iteratively fitted models (IFM) and preferred iterations
#' 
#' @description
#' This topic covers the concept of **iteratively fitted models (IFM)** 
#' and the related selection mechanism for *preferred iterations*.
#' For an introduction with application examples, refer to the vignette `vignette("ifm")`.
#' 
#' @section Iteratively fitted models:
#' We use the term *iteratively fitted model* for classes of models where 
#' the fitting process returns not just one single model (or model parameterization), 
#' but rather a sequence of models (or model parameterizations) of increasing structural complexity. 
#' We will refer to a single models in this sequence as a *(model) iteration*.
#' 
#' Prominent examples (and currently the only two instances implemented in the package) are:
#' \itemize{
#' \item{gradient boosting: \code{\link{fm_xgb}()} is a wrapper for \code{\link[xgboost]{xgb.train}()} from package \CRANpkg{xgboost}.
#' The iterations arise from successively adding trees to the current fitted model.}
#' \item{Lasso regression and elastic net models: 
#' \code{\link{fm_glmnet}()} is a wrapper for \code{\link[glmnet]{glmnet}()} from package \CRANpkg{glmnet}. 
#' Here, iterations results from successively lowering the penalty parameter `lambda`.}
#' }
#' 
#' In the conceptual framework described here, it is crucial that predictions can be computed from each iteration, 
#' such that iteration-wise training and test errors can be calculated, too.
#' 
#' @section Evaluation log:
#' An *evaluation log* is essentially a table having one row per iteration 
#' and including columns for the training and/or test error of the respective iteration.
#' The evaluation log of a \dQuote{model} (or *fitted model*) includes the training error only, 
#' while the evaluation log of a *cross-validated model* (object of class \dQuote{cv}) has both training and test errors.
#' 
#' The modeltuner package has a function \code{\link{evaluation_log}()} that computes evaluation logs for models and \dQuote{cv} objects, 
#' and a plot method, \code{\link{plot.evaluation_log}()}.
#' 
#' @section Selection criteria for iterations:
#' The function \code{\link{cv}()} has a mechanism for specification of *preferred iterations*.
#' The user specifies one or several selection criteria in the parameter `iter=` in a call to \code{cv()}.
#' By default (at package startup), the iteration having *minimal test error* will be selected. 
#' This default can however be modified by setting the option `"cv_iter"` (see \code{\link{modeltuner_options}}).
#' The effect of determining a preference criterion is that application of \code{\link{cv_performance}()} 
#' to the \dQuote{cv} object will show the results (training and test error) for the iteration selected based on this criterion.
#' 
#' The *preferred iteration* can be altered by specifying `iter=`*criterion* in `cv()`, 
#' where *criterion* is a selection rule that is applied internally to the `cv`'s evaluation log, 
#' resulting in a choice of the preferred iteration.
#' The minimal test error rule corresponds to setting `iter=crit_min()`; 
#' alternative selection criteria are presented in \code{\link{crit_iter}}. 
#' 
#' Multiple criteria can be requested, as in `crit_list(crit_min(), crit_1se())`. 
#' In this case, `cv_performance()` will show the results related to *first* criterion 
#' (that listed at first position in `crit_list(...)`).
#' 
#' A \dQuote{cv} object's selection criteria and selected iterations are displayed when you `print` it.
#' You can extract the information related to iterations with \code{\link{extract_pref_iter}()}.
#' 
#' @section Modifying the preference (criteria) of a \dQuote{cv} object:
#' \code{\link{set_pref_iter}()} and \code{\link{expand_pref_iter}()} offer options to change a `cv`'s preferences with respect to selection of iterations subsequently.
#' Note however that certain analyses remain available only for iterations related to the criteria stated in 
#' the `iter` argument of the original call to `cv()`. 
#' In particular, this is the case for running `cv_performance()` with alternative `metric`s. 
#' The reason of this limitation is that the full sets of predictions from cross-validation 
#' are stored only for the preferred iterations resulting from the originally stated criteria.
#' 
#' Setting `keep_fits=TRUE` in `cv()` is an option to circumvent there limitations (but can be excessively memory-consuming).
#' In this case, the model fits from cross-validation are stored in the resulting object, 
#' such that predictions from all iterations remain obtainable.
#' In particular, this makes it possible to compute evaluation logs with alternative `metric`s.
#' 
#' See \code{\link{expand_pref_iter}()} and \code{\link{set_metric}()} for more manipulations of \dQuote{cv} objects.
#'
#' @section Fitting an IFM after a cross-validation:
#' When a \dQuote{cv} object based on an IFM is converted back to a model (with \code{\link{tune}()} or with \code{\link{extract_model}()}),
#' the information on preferred iterations is stored in an attribute. 
#' Likewise, when \code{\link{fit}()} is used to obtain the resulting fitted model, the model corresponding to the preferred iteration is returned.
#' 
#' @name ifm
NULL


