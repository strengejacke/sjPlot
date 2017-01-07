# bind global variables
utils::globalVariables(c("train", "model", "test", ".response", "sse", "sst"))

#' @title Plot model fit from k-fold cross-validation
#' @name sjp.kfold_cv
#'
#' @description This function plots the aggregated residuals of k-fold cross-validated
#'                models against the outcome. This allows to evaluate how the model performs
#'                according over- or underestimation of the outcome.
#'
#' @param data A data frame, used to split the data into \code{k} trainig-test-pairs.
#' @param formula A model formula, used to fit linear models (\code{\link[stats]{lm}})
#'          over all \code{k} training data sets.
#' @param k Number of folds.
#'
#' @details This function, first, generates \code{k} cross-validated test-training
#'            pairs (using the \code{\link[modelr]{crossv_kfold}}-function) and
#'            fits the same linear model, specified in the \code{formula}-argument,
#'            over all training data sets. \cr \cr
#'            Then, the quality measures Root Mean Squared Error (\emph{rmse}),
#'            R-squared and Mean Squared Error (\emph{mse}) for each model are 
#'            calculated. The average for each of these measures is returned 
#'            from this function. \cr \cr
#'            Finally, the test data is used to predict the outcome from all
#'            models that have been fit on the training data, and the residuals
#'            from all test data is plotted against the observed values (outcome)
#'            from the test data. This plot can be used to validate the model
#'            and see, whether it over- (residuals > 0) or underestimates 
#'            (residuals < 0) the model's outcome.
#'
#' @note Currently, only linear models are supported.
#'
#' @examples 
#' library(sjmisc)
#' data(efc)
#' 
#' sjp.kfold_cv(efc, neg_c_7 ~ e42dep + c172code + c12hour)
#' sjp.kfold_cv(mtcars, mpg ~.)
#'
#' @import ggplot2
#' @importFrom tibble is.tibble as_tibble
#' @importFrom sjmisc get_label
#' @importFrom modelr crossv_kfold
#' @importFrom dplyr mutate group_by_ ungroup summarise
#' @importFrom purrr map map2
#' @importFrom broom augment
#' @importFrom tidyr unnest
#' @importFrom graphics plot
#' @importFrom stats as.formula
#' @export
sjp.kfold_cv <- function(data, formula, k = 5) {
  # make sure that data is a tibble
  if (!tibble::is.tibble(data)) data <- tibble::as_tibble(data)
  
  # make sure we have a formula
  if (!inherits(formula, "formula")) formula <- stats::as.formula(formula)
  
  # get name of response variable and get variable label, if
  # there is any... used for labelling plot axis
  resp <- formula[[2]]
  resp.name <- sjmisc::get_label(data[[deparse(resp)]], def.value = deparse(resp))
    
  # create cross-validated test-training pairs, run linear model on each
  # pair, get predicted values and quality measures for models fitted on the
  # train data
  res <- modelr::crossv_kfold(data, k = k) %>% 
    dplyr::mutate(model = purrr::map(train, ~ lm(formula, data = .))) %>% 
    dplyr::mutate(predicted = purrr::map2(model, test, ~ broom::augment(.x, newdata = .y))) %>% 
    tidyr::unnest(predicted)

  # make sure that response vector has an identifiably name
  colnames(res)[which(colnames(res) == deparse(resp))] <- ".response"

  # compute residuals for each k-fold model
  res <- res %>% 
    dplyr::mutate(residuals = .response - .fitted)
  
  # compute model quality measures r2 and rmse
  gof <- res %>% 
    dplyr::group_by_(".id") %>% 
    dplyr::summarise(
      sst = sum((.response - mean(.response, na.rm = T)) ^ 2, na.rm = T), # Sum of Squares Total
      sse = sum(residuals ^ 2, na.rm = T),                                # Sum of Squares Residual/Error
      rsquared = 1 - sse / sst,                                           # Proportion of variance accounted for
      rmse = sqrt(mean(residuals ^ 2, na.rm = T))                         # Root Mean Squared Error
    ) %>% 
    dplyr::ungroup()
  
  # plot response against residuals, to see where our model over- or
  # underestimates the outcome
  p <- ggplot(data = res, aes_string(x = ".response", y = "residuals")) +
    geom_hline(yintercept = 0) +
    geom_point() +
    stat_smooth(method = "loess") +
    theme_minimal() +
    labs(y = "Residuals", x = resp.name)
  # plot it
  graphics::plot(p)

  # ouput of quality measures to console  
  cat("Quality Measures of k-Fold Cross-Validation\n")
  cat(sprintf("   R-squared: %.3f\n", mean(gof$rsquared)))
  cat(sprintf("        RMSE: %.3f\n\n", mean(gof$rmse)))
  
  # return plot and quality measures
  invisible(structure(class = "sjp.kfold_cv",
                      list(plot = p,
                           rsquared = mean(gof$rsquared),
                           rmse = mean(gof$rmse),
                           mse = mean(gof$sse))))
}
