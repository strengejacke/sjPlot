#' @title Plot model fit from k-fold cross-validation
#' @name plot_kfold_cv
#'
#' @description This function plots the aggregated residuals of k-fold cross-validated
#'   models against the outcome. This allows to evaluate how the model performs
#'   according over- or underestimation of the outcome.
#'
#' @param data A data frame, used to split the data into \code{k} training-test-pairs.
#' @param formula A model formula, used to fit linear models (\code{\link[stats]{lm}})
#'          over all \code{k} training data sets. Use \code{fit} to specify a
#'          fitted model (also other models than linear models), which will be used
#'          to compute cross validation. If \code{fit} is not missing, \code{formula}
#'          will be ignored.
#' @param k Number of folds.
#' @param fit Model object, which will be used to compute cross validation. If
#'          \code{fit} is not missing, \code{formula} will be ignored. Currently,
#'          only linear, poisson and negative binomial regression models are supported.
#'
#' @details This function, first, generates \code{k} cross-validated test-training
#'            pairs and
#'            fits the same model, specified in the \code{formula}- or \code{fit}-
#'            argument, over all training data sets. \cr \cr
#'            Then, the test data is used to predict the outcome from all
#'            models that have been fit on the training data, and the residuals
#'            from all test data is plotted against the observed values (outcome)
#'            from the test data (note: for poisson or negative binomial models, the
#'            deviance residuals are calculated). This plot can be used to validate the model
#'            and see, whether it over- (residuals > 0) or underestimates
#'            (residuals < 0) the model's outcome.
#'
#' @note Currently, only linear, poisson and negative binomial regression models are supported.
#'
#' @examples
#' data(efc)
#'
#' plot_kfold_cv(efc, neg_c_7 ~ e42dep + c172code + c12hour)
#' plot_kfold_cv(mtcars, mpg ~.)
#'
#' # for poisson models. need to fit a model and use 'fit'-argument
#' fit <- glm(tot_sc_e ~ neg_c_7 + c172code, data = efc, family = poisson)
#' plot_kfold_cv(efc, fit = fit)
#'
#' # and for negative binomial models
#' fit <- MASS::glm.nb(tot_sc_e ~ neg_c_7 + c172code, data = efc)
#' plot_kfold_cv(efc, fit = fit)
#'
#' @import ggplot2
#' @export
plot_kfold_cv <- function(data, formula, k = 5, fit) {
  # make sure that data is a data frame
  if (!is.data.frame(data)) data <- as.data.frame(data)

  # check if a formula was passed as argument...
  if (!missing(formula)) {
    # make sure we have a formula
    if (!inherits(formula, "formula")) formula <- stats::as.formula(formula)
    # reset fam
    fam <- NULL
  } else if (!missing(fit)) {
    # ... or a fitted model
    formula <- stats::formula(fit)

    # get model family for glm
    if (inherits(fit, "glm"))
      fam <- stats::family(fit)
    else
      fam <- NULL
  } else {
    stop("Either `formula` or `fit` must be supplied.", call. = FALSE)
  }

  # get name of response variable and get variable label, if
  # there is any... used for labelling plot axis
  resp <- formula[[2]]
  resp.name <- sjlabelled::get_label(data[[deparse(resp)]], def.value = deparse(resp))

  # check if fit parameter was specified, and we have a model family
  if (!is.null(fam)) {
    # for poisson models, show deviance residuals
    if (fam$family == "poisson") {
      # create cross-validated test-training pairs, run poisson-model on each
      # pair, get deviance residuals and response value
      kfolds <- do.call(rbind, lapply(1:k, function(i) {
        out <- datawizard::data_partition(data, training_proportion = .8)
        data.frame(train = I(list(out[[1]])), test = I(list(out$test)))
      }))
      res <- kfolds %>%
        dplyr::mutate(model = purrr::map(.data$train, ~ stats::glm(formula, data = .x, family = stats::poisson(link = "log")))) %>%
        dplyr::mutate(residuals = purrr::map(.data$model, ~ stats::residuals(.x, "deviance"))) %>%
        dplyr::mutate(.response = purrr::map(.data$model, ~ insight::get_response(.x)))
    # for negative binomial models, show deviance residuals
    } else if (inherits(fit, "negbin")) {
      # create cross-validated test-training pairs, run poisson-model on each
      # pair, get deviance residuals and response value
      kfolds <- do.call(rbind, lapply(1:k, function(i) {
        out <- datawizard::data_partition(data, training_proportion = .8)
        data.frame(train = I(list(out[[1]])), test = I(list(out$test)))
      }))
      res <- kfolds %>%
        dplyr::mutate(model = purrr::map(.data$train, ~ MASS::glm.nb(formula, data = .))) %>%
        dplyr::mutate(residuals = purrr::map(.data$model, ~ stats::residuals(.x, "deviance"))) %>%
        dplyr::mutate(.response = purrr::map(.data$model, ~ insight::get_response(.x)))
    }

    # unnest residuals and response values
    res <- suppressWarnings(res %>% tidyr::unnest(residuals, .data$.response))

  } else {
    # create cross-validated test-training pairs, run linear model on each
    # pair, get predicted values and quality measures for models fitted on the
    # train data
    kfolds <- do.call(rbind, lapply(1:k, function(i) {
      out <- datawizard::data_partition(data, training_proportion = .8)
      data.frame(train = I(list(out[[1]])), test = I(list(out$test)))
    }))
    res <- kfolds %>%
      dplyr::mutate(model = purrr::map(.data$train, ~ stats::lm(formula, data = .))) %>%
      dplyr::mutate(predicted = purrr::map2(.data$model, .data$test, function(.x, .y) {
        out <- data.frame(.fitted = stats::predict(.x, newdata = .y))
        cbind(.y, out)
      })) %>%
      tidyr::unnest(cols = .data$predicted)

    # make sure that response vector has an identifiably name
    colnames(res)[which(colnames(res) == deparse(resp))] <- ".response"

    # compute residuals for each k-fold model
    res <- res %>%
      dplyr::mutate(residuals = .data$.response - .data$.fitted)
  }

  # plot response against residuals, to see where our model over- or
  # underestimates the outcome
  p <- ggplot(data = res, aes_string(x = ".response", y = "residuals")) +
    geom_hline(yintercept = 0) +
    geom_point() +
    stat_smooth(method = "loess") +
    theme_minimal() +
    labs(y = "Residuals", x = resp.name)

  # plot it
  p
}
