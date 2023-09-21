
# modeltuner

### Contents of the package

**modeltuner** is designed for evaluation of the predictive performance of statistical models and hyperparameter tuning based on cross-validation. The current version of the package supports models with a continuous or binary (0/1) response.

The package also offers some particularly attractive tools for handling what is called *iteratively fitted models (IFM)* here.
For such a model, the fitting process returns not just one single model (or model parameterization),
but rather a range of models (or model parameterizations) of increasing structural complexity.
Prominent examples (and currently the only instances implemented in the package) are
gradient boosting (as implemented in package **xgboost**) and Lasso regression and elastic nets (available from package **glmnet**).

### Installation
The package can be installed from github by executing
```r
devtools::install_github("MathiasAmbuehl/modeltuner")
```

#### Dependencies
All package dependencies are available from CRAN. The packages **xgboost** (>= 1.5), **glmnet**, **ggplot2**, **MetricsWeighted**, **matrixStats**, **RANN**, **progress** are required, 
while **lme4**, **robustbase**, **mgcv**, **rpart**, **quantreg**, **ranger**, **magrittr**, **tibble**, **MASS** and **gridExtra** are packages required to reproduced all examples.

### Where to start
The best place to start are the two vignettes included in the package:

* `vignette("modeltuner")` presents the basic concepts of the package (reading this text first is recommended).
* `vignette("ifm")` introduces tools suited for developing and evaluating *iteratively fitted models* (XGBoost and glmnet models).

