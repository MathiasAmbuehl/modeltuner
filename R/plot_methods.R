
#' Plot methods for classes \dQuote{model}, \dQuote{multimodel} and \dQuote{cv}
#' 
#' @description
#' `plot.model()` and `plot.multimodel()` \code{\link{fit}} the model(s) in `x` and draw scatter plot(s) of
#' actual response values versus fitted values in case of a continuous response. 
#' With binary response, a violin plot of the fitted values versus the response variable is produced
#'  (using \code{\link{geom_violin}()}).
#' 
#' `plot.cv` creates a similar plot, using the predictions resulting from cross-validation 
#' (generated with \code{\link{cv_predict}}) as fitted values.
#' 
#' @details 
#' The lengths of the errorbars corresponds to +/-1 standard error.
#' 
#' @param x Object of appropriate class.
#' @param plot Logical: If `TRUE`, a ggplot is returned, if `FALSE` a \code{data.frame}. 
#' `plot()` first prepares a `data.frame` and then draws some ggplot using this data, 
#' with limited options for customization. 
#' If you want to design your own plot, you can set `plot=FALSE`, and use the `data.frame` returned by `plot()` 
#' to create your plot.
#' @param n_max Integer: Maximal number of points to draw in a scatter plot.
#' If size of data is larger than `n_max`, a random sample will be displayed. 
#' @param \dots Passed to \code{\link{geom_point}()} or \code{\link{geom_violin}()} (in case of binary response), respectively.
#' 
#' @return These `plot()` methods return either a \link[ggplot2]{ggplot} or, if `plot=FALSE`, a `data.frame`.
#' 
#' @seealso \code{\link{model}}, \code{\link{multimodel}}, \code{\link{cv}}
#' 
#' @examples
# Continuous response: binomial response
#' # Simulate data 
#' set.seed(1)
#' n <- 50
#' x <- rnorm(n)
#' y <- 3*x + rnorm(n)
#' mymodel <- model(lm(y~x))
#' # Plot in-sample and out-of-sample predictions:
#' if (require(ggplot2) && require(gridExtra)){
#'   plot(gridExtra::arrangeGrob(
#'        plot(mymodel) + ggtitle("response vs. in-sample predictions"),
#'        plot(cv(mymodel)) + ggtitle("response vs. out-of-sample predictions"), 
#'        nrow = 1))
#' }   
#' 
#' # Binary response: binomial response
#' # Simulate data 
#' n <- 100
#' p <- 10
#' x <- matrix(rnorm(p*n), nrow = n)
#' y <- (0.1 * rowSums(x) + rnorm(n)) > 0
#' mymodel <- model(glm(y~x, family = binomial))
#' # Plot in-sample and out-of-sample predictions:
#' if (require(ggplot2) && require(gridExtra)){
#'   plot(gridExtra::arrangeGrob(
#'        plot(mymodel) + ggtitle("response vs. in-sample predictions"),
#'        plot(cv(mymodel)) + ggtitle("response vs. out-of-sample predictions"), 
#'        nrow = 1))
#' }
#' 
#' @name plot.model
NULL


#' Plot method for class \dQuote{model}
#' @rdname plot.model
#' @importFrom ggplot2 geom_abline
#' @export 
plot.model <- function(x, plot = TRUE, n_max = 5000, ...){
  m <- multimodel(x, simplify = FALSE)
  plot(m, plot = plot, ...)
}

#' Plot method for class \dQuote{multimodel}
#' @rdname plot.model
#' @importFrom ggplot2 facet_wrap geom_violin
#' @export 
plot.multimodel <- function(x, plot = TRUE, n_max = 5000, ...){
  nmodel <- n_model(x)
  resp <- response(x)
  d <- data.frame(
    response = rep(resp, nmodel), 
    fitted = as.vector(predict(x)), 
    model = rep(label(x), each = length(resp)))
  resp_type <- extract_model(x, 1)$response_type
  if (plot && resp_type == "continuous" && n_max < nrow(d)){
    message("Only ", n_max, " (out of ", nrow(d), ") observations are displayed due to n_max=",
            n_max)
    sel <- sample(nrow(d), n_max)
    d <- d[sel, ]
  }
  d$model <- factor(d$model, levels = unique(d$model))
  rownames(d) <- NULL
  if (!plot) return(d)
  if (resp_type == "continuous"){
    out <- ggplot(d, aes_string("fitted", "response"))
    out <- out + geom_point(...) + geom_abline()
  }
  if (resp_type == "binary"){
    d$response <- factor(d$response)
    out <- ggplot(d, aes_string("fitted", "response"))
    out <- out + geom_violin(..., orientation = "y")
  }
  out + facet_wrap(~model)
}

#' Plot method for class \dQuote{cv}
#' @rdname plot.model
#' @importFrom ggplot2 geom_rug
#' @export 
plot.cv <- function(x, plot = TRUE, n_max = 5000, ...){
  nmodel <- n_model(x)
  resp <- response(x)
  d <- data.frame(response = rep(resp, nmodel), 
                  cv_prediction = as.vector(cv_predict(x)), 
                  model = rep(label(x), each = length(resp)))
  resp_type <- extract_model(x, 1)$response_type
  if (plot && resp_type == "continuous" && n_max < nrow(d)){
    message("Only ", n_max, " (out of ", nrow(d), ") observations are displayed due to n_max=",
            n_max)
    sel <- sample(nrow(d), n_max)
    d <- d[sel, ]
  }
  d$model <- factor(d$model, levels = unique(d$model))
  rownames(d) <- NULL
  if (!plot) return(d)
  if (resp_type == "continuous"){
    out <- ggplot(d, aes_string("cv_prediction", "response"))
    out <- out + 
      geom_point(..., na.rm = TRUE) + geom_abline() +
      geom_rug(data = subset(d, is.na(d[["cv_prediction"]])), col = 2)
  }
  if (resp_type == "binary"){
    d$response <- factor(d$response)
    out <- ggplot(d, aes_string("cv_prediction", "response"))
    out <- out + geom_violin(..., orientation = "y")
  }  
  out + facet_wrap(~model)
}


