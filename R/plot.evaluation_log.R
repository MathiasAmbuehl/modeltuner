
#' Plot method for class \dQuote{evaluation_log}
#' 
#' Draws line plots of training and test errors in an evaluation log, if available.
#' 
#' @param x An object of class \code{\link{evaluation_log}}.
#' @param \dots Currently not used.
#' @inheritParams cv_performance
#' 
#' @details 
#' The lengths of the errorbars corresponds to +/-1 standard error.
#' 
#' @return `plot.evaluation_log()` returns either a \link[ggplot2]{ggplot} or, if `plot=FALSE`, a `data.frame`.
#' 
#' @examples
#' # Evaluation log of a 'fm_xgb' model
#' fitted_xgb <- fm_xgb(Sepal.Length ~ ., iris, max_depth = 2)
#' evaluation_log(fitted_xgb)    # evaluation log of a model has no 
#' plot(evaluation_log(fitted_xgb))
#' 
#' # Evaluation log of cross-validated 'fm_xgb' model
#' cv_xgb <- cv(model(fitted_xgb, label = "xgb_depth2"))
#' evaluation_log(cv_xgb) 
#' plot(evaluation_log(cv_xgb))
#' 
#' # Evaluation log of several cross-validated models
#' mydata <- simuldat()
#' fitted_glmnet <- fm_glmnet(Y ~ ., mydata)
#' cv_glmnet <- cv(multimodel(fitted_glmnet, prefix = "glmnet", alpha = 0:1))
#' label(cv_glmnet) <- c("ridge", "lasso")
#' evaluation_log(cv_glmnet)
#' plot(evaluation_log(cv_glmnet))
#' 
#' @seealso \code{\link{evaluation_log}}, \code{\link{ifm}}
#' 
#' @importFrom ggplot2 aes scale_linetype scale_linetype_identity
#' @export
plot.evaluation_log <- function(x, errorbars = getOption("cv_show_se"), plot = TRUE, size = 2, lwd = 1, 
                                lwd_errorbars = 0.5, zeroline = TRUE, ...){
  nm_metric <- names(x[[1]]$metric)
  has_log <- sapply(x, "[[", "has_log")
  if (!any(has_log)) stop("No model with an evaluation_log") 
  evlog <- lapply(x[has_log], "[[", "log")
  commonCols <- do.call(m_intersect, lapply(evlog, names))
  evlog <- lapply(evlog, "[", commonCols)
  evlog <- mapply(data.frame, model = names(evlog), evlog, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  evlog <- do.call(rbind, evlog)
  # Remove absent columns (possibly se_* cols)
  absentCol <- sapply(evlog, 
    function(x) !is.character(x) && !is.factor(x) && (all(is.na(x) | !is.finite(x))))
  absentCol <- absentCol & grepl("^se\\_", names(absentCol))
  evlog <- evlog[!absentCol]
  rownames(evlog) <- NULL

  # saved iterations  
  my_pref_iter <- lapply(x[has_log], "[[", "pref_iter")
  .tmp_adapt <- function(nm, d){
    d <- data.frame(model = rep(nm, NROW(d)), d)
    if (nrow(d))
      d[["primary_model"]] <- rep(c(TRUE, FALSE), c(1, nrow(d) - 1))
    d
  }
  my_pref_iter <- mapply(.tmp_adapt, names(my_pref_iter), my_pref_iter, SIMPLIFY = FALSE)
  my_pref_iter <- do.call(rbind, my_pref_iter)
  rownames(my_pref_iter) <- NULL

  # reshaped evlog
  xvar <- "iter"
  has_train <- any(grepl("^train_", names(evlog)))
  has_test <- any(grepl("^test_", names(evlog)))
  plotdata <- data.frame(
    evlog[c("model", "iter")][rep(seq_len(nrow(evlog)), has_train + has_test), ],
    .metric. = rep(c(if (has_train) "train", if (has_test) "test"), each = nrow(evlog)))
  names(plotdata)[names(plotdata) == ".metric."] <- nm_metric
  plotdata[["error"]] <- ifelse(plotdata[[nm_metric]] == "train",
                                evlog[[paste0("train_", nm_metric)]],
                                evlog[[paste0("test_", nm_metric)]])
  if (!any(grepl("^se_", names(evlog)))) errorbars <- FALSE
  if (errorbars){
    message("The standard errors shown as error bars may be inaccurate.")
    if (has_train){
      plotdata[["se"]] <- ifelse(plotdata[[nm_metric]] == "train",
                                 evlog[[paste0("se_train_", nm_metric)]],
                                 evlog[[paste0("se_test_", nm_metric)]])
    } else {
      plotdata[["se"]] <- evlog[["se_test"]]
    }
  }
  plotdata$model <- factor(plotdata$model, levels = names(x)[has_log])
  rownames(plotdata) <- NULL
  mode(plotdata[["error"]]) <- "numeric" # overwrites logical if all are NA
  if (errorbars)
    mode(plotdata[["se"]]) <- "numeric"
  if (!plot) return(plotdata)
  
  wid <- min(diff(sort(unique(evlog[[xvar]]))))/4
  pl <- ggplot(plotdata, aes_string(xvar, "error", color = nm_metric)) +
    ylab(nm_metric) +
    facet_wrap(~model, scales = "free_x")
  if (zeroline) pl <- pl +
    geom_hline(yintercept = 0, lty = 2)
  pl <- pl + 
    geom_point(size = size) + 
    geom_line(lwd = lwd)
  if (nrow(my_pref_iter)){ 
    my_pref_iter <- my_pref_iter[is.finite(my_pref_iter$iter) & is.finite(my_pref_iter$error), , drop = FALSE]
    my_pref_iter$model <- factor(my_pref_iter$model, levels = names(x)[has_log])
    pl <- pl + 
      geom_vline(data = my_pref_iter, 
                 mapping = aes_string(xintercept = "iter", 
                               linetype = "ifelse(primary_model, 1, 3)")) +
      scale_linetype_identity() + 
      geom_hline(data = my_pref_iter, 
                 mapping = aes_string(yintercept = "error"), 
                 lty = 3) 
  }
  if (errorbars) pl <- pl +
    geom_errorbar(mapping = aes_string(ymin = "error - se", 
                                       ymax = "error + se"), 
                  lwd = lwd_errorbars, width = wid)
  pl
}

# aux fun: intersection of several vectors
m_intersect <- function(...){
  dots <- list(...)
  if (length(dots) == 0) return(character(0))
  if (length(dots) == 1) return(dots[[1]])
  if (length(dots) == 2) return(intersect(dots[[1]], dots[[2]]))
  do.call(m_intersect, c(list(intersect(dots[[1]], dots[[2]])), dots[-(1:2)]))
}
# m_intersect(letters, letters[1:10], letters[6:20])
# m_intersect(1:100, 20:70, sample(100, 50), 50:90)

