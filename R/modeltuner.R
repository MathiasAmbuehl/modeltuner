
#' modeltuner package overview
#' 
#' @description
# #' \CRANpkg{modeltuner} 
#' **modeltuner** is a package designed for evaluation of predictive performance of statistical models and hyperparameter tuning 
#' based on cross-validation. The current version of the package supports models with a continuous or binary (0/1) response.
#' 
#' In this help topic, the main conceptual elements of the package are introduced and explained.
#' 
#' The package also offers some particularly attractive tools for handling what is called **iteratively fitted models (IFM)** here. 
#' For such a model, the fitting process returns not just one single model (or model parameterization), 
#' but rather a range of models (or model parameterizations) of increasing structural complexity. 
#' Prominent examples (and currently the only instances implemented in the package) are 
#' gradient boosting (as implemented in package **xgboost**) and Lasso regression and elastic nets (available from package **glmnet**). 
#' Refer to the dedicated help topic (\link{ifm}) and vignette (`vignette("ifm")`) for more information.
#' 
#' @section Overview and workflow:
#' The package's key concept is the class \dQuote{model}. If you have a fitted model called `myfm`, say,  (e.g., a linear model), 
#' you convert it to a \dQuote{model} object by simply typing `mymodel<-model(myfm)`, which gives you access to the world of modeltuner.
#' `model()` is designed for model classes being based on the `formula`/`data` interface.
#' 
#' An analysis will typically comprise the following successive steps:
#' \enumerate{
#' \item{First, you set up a base **model**, or several. This is done using the function \code{\link{model}()}.
#' Its output, an object of class \dQuote{model}, includes the formula, the data, the call to the model fitting function, 
#' optionally the fitting weights, and more components. 
#' It contains the information needed to repeatedly refit the model under slightly modified conditions, 
#' as is done in a cross-validation. It does *not* necessarily contain the actual fit of the model.
#' Properly distinguishing *fitted models* (e.g., an \dQuote{lm}) and objects of class \dQuote{model} is crucial for 
#' a good understanding of how the functions in the modeltuner package work.
#' }
#' \item{In many applications, you are not just considering a single model, 
#' but you want to study and compare the predictive performance of several candidate models, 
#' and/or you want to find optimal values of certain hyperparameters. 
#' In order to do so, you combine several models or model parameterizations into a **multimodel**.
#' A \dQuote{\link{multimodel}} is essentially a generalization of the class \dQuote{model} to more than one model. 
#' }
#' \item{Next, you run a **cross-validation** procedure, by applying the function `cv()` to 
#' a model or multimodel, thus obtaining an object of class \dQuote{cv}. 
#' This is the most time-consuming part of your analysis. 
#' The key function to evaluate and display the contents of a \dQuote{cv} object is \code{\link{cv_performance}()}, 
#' which computes training and test errors of the model(s) under investigation.
#' }
#' \item{To extract the best-performing model from a \code{cv}-object, you may use \code{\link{tune}()}. 
#' Alternatively, you might prefer to pick your favorite model manually. To do so, use \code{\link{extract_model}()}.
#' }
#' \item{Finally you may use \code{\link{fit}()} to convert the selected model (an object of class \dQuote{model}) back to a *fitted* model, 
#' e.g. to a \dQuote{lm} object, thereby leaving the modeltuner world.
#' }}
#' Not all of these steps are required in every analysis, and some steps may have to be executed repeatedly
#' before achieving satisfaction.
#' 
#' `modeltuner` comes with a number of utility functions that are helpful for inspection and basic manipulations 
#' of object of classes \dQuote{model}, \dQuote{multimodel}, \dQuote{cv} and other:
#' \code{\link{label}()}, \code{\link{n_model}()}, \code{\link{extract_model}()}, \code{\link[modeltuner]{subset}()}, 
#' \code{\link{sort_models}()}. These are all S3 generic functions with specific methods for these classes.
#' 
#' To complement this, the package has wrappers of a number of model fitting functions from other packages, 
#' most notably to \code{\link{glmnet}()} and \code{\link{xgb.train}()}, that are based on a `formula`/`data` interface, 
#' so that the resulting classes can be easily handled in the `modeltuner` framework.
#' See the help topics \code{?\link{fm_glmnet}}, \code{?\link{fm_xgb}}, \code{?\link{fm_knn}}, \code{?\link{fm_smooth_spline}}, 
#' \code{?\link{fm_const}}.
#' 
#' @section Models and multimodels:
#' `model(x, ...)` is a S3 generic function. The default method converts a fitted model, such as an \dQuote{lm}, into an object of class 
#' \dQuote{model}. The original fitted model `x` must meet certain formal requirements, 
#' e.g. its fitting function must have formal arguments `formula` and `data`, and it must have a `call` component. 
#' The exact requirements are described in detail in \code{?\link{model}}, as well as the structure of a \dQuote{model} object.
#'  
#' A multimodel combines several models in one object. There are two ways to create a `multimodel`. 
#' Either you combine different \dQuote{model} objects in a call `c(...)`, as in
#' `c(model1, model2, model3)`, or you run the (generic) function `multimodel(x, ...)`, 
#' where `x` is a \dQuote{model} or fitted model and the \dQuote{...} specify alternative values of one or several (hyper-)parameters 
#' of the model-fitting function. 
#' Models combined in a `multimodel` must have identical responses. 
#' See \code{?\link{multimodel}} for more information and examples.
#' 
#' 
#' @section Folds:
#' A set of \dQuote{folds} is part of every model validation procedure executed with `cv()`. It defines the grouping pattern of the procedure. 
#' Technically, an object of class \dQuote{folds}, created by the function `make_folds()`,  
#' is a list of integer vectors with special attributes. Each of these vectors defines a test set of observations in the model data. 
#' In validation procedures, the complement of each test set is used as the corresponding training set.
#' 
#' Three (cross-)validation patterns are implemented: 
#' Complete `n`-fold cross-validation, incomplete `n`-fold cross-validation,  
#' and simple hold-out validation. 
#' See \code{?\link{make_folds}} for details.
#' 
#' 
#' @section \code{cv()} and \code{cv_performance()}:
#' The function \code{\link{cv}()} executes a validation procedure. 
#' Its two main inputs are an object of class \dQuote{model} or \dQuote{multimodel} and a set of folds -- 
#' the result is an object of class \dQuote{cv}.
#' By default, the model fits from the cross-validation are not saved as a part of the output, 
#' but the predictions are saved and used in evaluation. 
#' 
#' The main evaluation function for a \dQuote{cv} object is \code{\link{cv_performance}()}.
#' Among its parameters are a *metric* (formal argument `metric`) and evaluation weights (`eval_weights`).:
#' \itemize{
#' \item{*Metrics*: The default metric is `rmse()` for a continuous response and `logLoss()` for a binary response.
#' In `modeltuner`, we use metrics from the package \CRANpkg{MetricsWeighted}. 
#' See the section \dQuote{Metrics} below and \code{?\link[modeltuner]{metrics}}.}
#' \item{*Evaluation weights:*
#' We distinguish *fitting weights* --these are in action when a cross-validation is executed-- and
#' *evaluation weights*, that are a parameter in the evaluation of a cross-validation. 
#' See the section on \dQuote{Evaluation weights} later in this help topic.}
#' }
#' 
#' When executing `cv()`, the default `metric` and `eval_weights` are chose automatically and saved separately for each model. 
#' They are thus not necessarily the same for all models in a `multimodel`.
#' However, each execution of `cv_performance()` uses a common `metric` and a common `eval_weights`. 
#' 
#' The help topic \code{?\link{cv}} has more details, in particular on the structure of an object of class \dQuote{cv}.
#' 
#' 
#' @section Evaluation and Metrics:
#' \code{\link{cv_performance}()} reports the training (in-sample) and test (out-of-sample) predictive performance 
#' of the models included in the \dQuote{cv} object by evaluating the `metric`. 
#' It returns a *performance table*, an object of class \dQuote{performance}, a `data.frame` with additional attributes
#' and a convenient `print()` method.
#' 
#' Roughly speaking, first the accuracy of (training and test) predictions are evaluated in each fold separately 
#' and then these results are averaged. The help topic \code{?\link{cv_performance}} has more details.
#'  
#' A set of evaluation *metrics* from the package \CRANpkg{MetricsWeighted} is used in the package:
#' \itemize{
#' \item{Metrics for continuous response: \code{\link[MetricsWeighted]{rmse}}, \code{\link[MetricsWeighted]{mse}}, 
#' \code{\link[MetricsWeighted]{mae}}, \code{\link[MetricsWeighted]{medae}};}
#' \item{Modified metrics for binary response: \code{\link[modeltuner]{logLoss}}, 
#' \code{\link[modeltuner]{classification_error}}.}
#' }
#' The two metrics for binary response are slightly modified in order to make them more convenient for use in the context of `modeltuner`.
#' 
#' All of these functions have formal arguments `actual`, `predicted` and, optionally, `weights`.
#' Alternative metrics may be used and must have these formal arguments, too.
#' You can use a metric having no `weights` argument, but evaluation will only work if `eval_weights` is `NULL`.
#' 
#' See \code{?\link[modeltuner]{metrics}} for more details on metrics. 
#' 
#'  
#' @section Evaluation weights:
#' By default, a model's fitting `weights` are chosen as its default `eval_weights`.
#' 
#' The \dQuote{model} objects included in a \dQuote{cv} object can have different fitting and evaluation weights. If this is the case,
#' \code{\link{cv_performance}()} will use `eval_weights=NULL` by default. 
#' This default can be overridden by the argument `eval_weights`.
#' 
#' 
#' @section Tune and fit:
#' \code{\link{tune}()} selects the best-performing model among those included in a \dQuote{cv} object, 
#' \code{\link{fit}()} converts that \dQuote{model} back to a *fitted model*, using the complete model data for the fit. 
#' 
#' See \code{?\link{tune}} and \code{?\link{fit}}.
#' 
#' 
#' @aliases modeltuner
#' @docType package
#' @name modeltuner-package
NULL


