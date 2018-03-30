#' @title Print regression models to HTML table
#' @name tab_model
#'
#' @description
#'   \code{tab_model()} creates HTML tables from regression models.
#'
#' @param transform A character vector, naming a function that will be applied
#'   on estimates and confidence intervals. By default, \code{transform} will
#'   automatically use \code{"exp"} as transformation for applicable classes of
#'   regression models (e.g. logistic or poisson regression). Estimates of linear
#'   models remain untransformed. Use \code{NULL} if you want the raw,
#'   non-transformed estimates.
#' @param terms Character vector with names of those terms (variables) that should
#'    be printed in the table. All other terms are removed from the output. If
#'    \code{NULL}, all terms are printed.
#' @param rm.terms Character vector with names that indicate which terms should
#'    be removed from the output Counterpart to \code{terms}. \code{rm.terms =
#'    "t_name"} would remove the term \emph{t_name}. Default is \code{NULL}, i.e.
#'    all terms are used.
#' @param pred.labels Character vector with labels of predictor variables.
#'    If not \code{NULL}, \code{pred.labels} will be used in the first
#'    table column with the predictors' names. If \code{NULL}, label
#'    attributes will be used in the output (see \code{\link[sjlabelled]{get_label}}).
#'    If \code{pred.labels = ""} or \code{auto.label = FALSE}, the raw
#'    variable names as used in the model formula are used as predictor
#'    labels. See 'Examples'.
#' @param dv.labels Character vector with labels of dependent variables of all
#'    fitted models. See 'Examples'.
#' @param show.intercept Logical, if \code{TRUE}, the intercepts are printed.
#' @param show.est Logical, if \code{TRUE}, the estimates are printed.
#' @param show.ci Logical, if \code{TRUE}, the confidence interval
#'    is printed to the table.
#' @param show.std Indicates whether standardized beta-coefficients should
#'    also printed, and if yes, which type of standardization is done.
#'    See 'Details'.
#' @param show.se Logical, if \code{TRUE}, the standard errors are also printed.
#' @param ci.hyphen Character vector, indicating the hyphen for confidence interval range.
#'    May be an HTML entity. See 'Examples'.
#' @param minus.sign string, indicating the minus sign for negative numbers.
#'    May be an HTML entity. See 'Examples'.
#' @param digits Amount of decimals for estimates
#' @param digits.p Amount of decimals for p-values
#' @param separate.ci.col Logical, if \code{TRUE}, the CI values are shown in
#'    a separate table column.
#' @param separate.se.col Logical, if \code{TRUE}, the SE values are shown in
#'    a separate table column.
#'
#' @inheritParams plot_models
#' @inheritParams plot_model
#'
#' @importFrom dplyr full_join select if_else mutate
#' @importFrom tibble lst add_case as_tibble
#' @importFrom purrr reduce map2 map_if map_df
#' @importFrom sjlabelled get_dv_labels get_term_labels
#' @importFrom sjmisc word_wrap var_rename add_columns
#' @importFrom sjstats std_beta
#' @importFrom rlang .data
#' @export
tab_model <- function(
  ...,
  transform,
  show.intercept = TRUE,
  show.est = TRUE,
  show.ci = .95,
  show.se = NULL,
  show.std = NULL,
  show.p = TRUE,
  show.stat = FALSE,

  show.header = FALSE,
  show.col.header = TRUE,

  show.zeroinf = TRUE,
  show.r2 = TRUE,
  show.icc = FALSE,
  show.re.var = FALSE,
  show.fstat = FALSE,
  show.aic = FALSE,
  show.aicc = FALSE,
  show.dev = FALSE,

  terms = NULL,
  rm.terms = NULL,
  group.terms = NULL,
  order.terms = NULL,

  title = NULL,
  pred.labels = NULL,
  dv.labels = NULL,
  wrap.labels = 25,

  ci.hyphen = "&nbsp;&ndash;&nbsp;",
  minus.sign = "&#45;",
  separate.ci.col = TRUE,
  separate.se.col = TRUE,


  digits = 2,
  digits.p = 3,

  case = "parsed",
  auto.label = TRUE,
  bpe = "median"
) {

  models <- tibble::lst(...)
  auto.transform <- missing(transform)
  ci.lvl <- ifelse(is.null(show.ci), .95, show.ci)
  zeroinf.list <- list()

  model.list <- purrr::map2(
    models,
    1:length(models),
    function(model, i) {

      # get info on model family
      fam.info <- get_glm_family(model)

      # check whether estimates should be transformed or not

      if (auto.transform) {
        if (fam.info$is_linear)
          transform <- NULL
        else
          transform <- "exp"
      }


      ## TODO probably indicate estimate with "*"

      # get tidy output of summary ----

      dat <- model %>%
        tidy_model(ci.lvl = ci.lvl, transform, type = "est", bpe, se = show.se, facets = FALSE, show.zeroinf = show.zeroinf) %>%
        dplyr::mutate(conf.int = sprintf(
          "%.*f%s%.*f",
          digits,
          .data$conf.low,
          ci.hyphen,
          digits,
          .data$conf.high
        )) %>%
        dplyr::select(-.data$conf.low, -.data$conf.high) %>%
        dplyr::mutate(p.value = sprintf("%.*f", digits.p, .data$p.value))


      # get inner probability (i.e. 2nd CI for Stan-models) ----

      if (is.stan(model)) {
        dat <- dat %>%
          dplyr::mutate(hdi.inner = sprintf(
            "%.*f%s%.*f",
            digits,
            .data$conf.low50,
            ci.hyphen,
            digits,
            .data$conf.high50
          )) %>%
            dplyr::select(-.data$conf.low50, -.data$conf.high50)
      }


      # handle zero-inflation part ----

      if (tibble::has_name(dat, "wrap.facet")) {
        zi <- which(dat$wrap.facet == "Zero-Inflated Model")

        if (show.zeroinf && !sjmisc::is_empty(zi)) {
          zidat <- dat %>%
            dplyr::slice(!! zi) %>%
            dplyr::select(-.data$wrap.facet)
          zeroinf.list[[i]] <- zidat
        } else {
          zeroinf.list[[i]] <- NULL
        }

        if (!sjmisc::is_empty(zi)) dat <- dplyr::slice(dat, !! -zi)
        dat <- dplyr::select(dat, -.data$wrap.facet)
      }


      # fix term-names from brmsfit ----

      if (inherits(model, "brmsfit")) {
        dat$term <- sub(pattern = "b_Intercept", replacement = "(Intercept)", x = dat$term, fixed = T)
        dat$term <- sub(pattern = "b_", replacement = "", x = dat$term, fixed = T)
      }


      # indicate p <0.001

      pv <- paste0("0.", paste(rep("0", digits.p), collapse = ""))
      dat$p.value[dat$p.value == pv] <- "&lt;0.001"


      # switch column for p-value and conf. int.

      if (is.stan(model))
        dat <- dat[, c(1, 2, 4, 5, 6)]
      else
        dat <- dat[, c(1, 2, 3, 6, 4, 5)]


      # tidy output of standardized values ----

      if (!is.null(show.std) && fam.info$is_linear && !is.stan(model)) {
        dat <- model %>%
          sjstats::std_beta(type = show.std, ci.lvl = ci.lvl) %>%
          sjmisc::var_rename(
            std.error = "std.se",
            conf.low = "std.conf.low",
            conf.high = "std.conf.high"
          ) %>%
          tibble::add_case(.before = 1) %>%
          dplyr::select(-1) %>%
          sjmisc::add_columns(dat) %>%
          dplyr::mutate(std.conf.int = sprintf(
            "%.*f%s%.*f",
            digits,
            .data$std.conf.low,
            ci.hyphen,
            digits,
            .data$std.conf.high
          )) %>%
          dplyr::select(-.data$std.conf.low, -.data$std.conf.high)

        dat <- dat[, c(1, 2, 3, 4, 7, 8, 9, 5, 6)]
      }


      # add suffix to column names, so we can distinguish models later

      cn <- colnames(dat)[2:ncol(dat)]
      colnames(dat)[2:ncol(dat)] <- sprintf("%s_%i", cn, i)


      # for HTML, convert numerics to character

      dat <- dat %>%
        purrr::map_if(is.numeric, ~ sprintf("%.*f", digits, .x)) %>%
        tibble::as_tibble()


      ## TODO optionally insert linebreak for new-line-CI / SE

      # merge estimates and CI / SE columns, if requested ----

      if (!separate.ci.col) {
        est.cols <- tidyselect::starts_with("estimate", vars = colnames(dat))
        dat[[est.cols]] <- sprintf("%s (%s)", dat[[est.cols]], dat[[est.cols + 2]])
        dat <- dplyr::select(dat, -tidyselect::starts_with("conf.int"))

        # for stan models, we also have 50% HDI
        if (!sjmisc::is_empty(tidyselect::starts_with("hdi.inner", vars = colnames(dat)))) {
          dat[[est.cols]] <- sprintf("%s (%s)", dat[[est.cols]], dat[[est.cols + 2]])
          dat <- dplyr::select(dat, -tidyselect::starts_with("hdi.inner"))
        }

        std.cols <- tidyselect::starts_with("std.estimate", vars = colnames(dat))
        if (!sjmisc::is_empty(std.cols)) {
          dat[[std.cols]] <- sprintf("%s (%s)", dat[[std.cols]], dat[[std.cols + 2]])
          dat <- dplyr::select(dat, -tidyselect::starts_with("std.conf.int"))
        }
      }

      if (!separate.se.col) {
        est.cols <- tidyselect::starts_with("estimate", vars = colnames(dat))
        dat[[est.cols]] <- sprintf("%s (%s)", dat[[est.cols]], dat[[est.cols + 1]])
        dat <- dplyr::select(dat, -tidyselect::starts_with("std.error"))

        std.cols <- tidyselect::starts_with("std.estimate", vars = colnames(dat))
        if (!sjmisc::is_empty(std.cols)) {
          dat[[std.cols]] <- sprintf("%s (%s)", dat[[std.cols]], dat[[std.cols + 1]])
          dat <- dplyr::select(dat, -tidyselect::starts_with("std.se"))
        }
      }

      dat
    }
  )


  # join all model data frames and convert to character ----

  na.vals <- c("NA", sprintf("NA%sNA", ci.hyphen), sprintf("NA (NA%sNA)", ci.hyphen), sprintf("NA (NA%sNA) (NA)", ci.hyphen))

  dat <- model.list %>%
    purrr::reduce(~ dplyr::full_join(.x, .y, by = "term")) %>%
    purrr::map_df(~ dplyr::if_else(.x %in% na.vals | is.na(.x), "", .x))


  # remove unwanted columns and rows ----

  dat <- remove_unwanted(dat, show.intercept, show.est, show.std, show.ci, show.se, show.stat, show.p, terms, rm.terms)


  # get default labels for dv and terms ----

  if (isTRUE(auto.label) && sjmisc::is_empty(pred.labels)) {
    pred.labels <- sjlabelled::get_term_labels(models, mark.cat = TRUE, case = case)
    pred.labels <- pred.labels[!duplicated(names(pred.labels))]
    labs <- sjmisc::word_wrap(pred.labels, wrap = wrap.labels, linesep = "<br>")
    # some labels may not match. in this case, we only need to replace those
    # elements in the vector that match a specific label, but
    # at the correct position inside "dat$term"
    tr <- 1:nrow(dat)
    tr <- tr[-which(is.na(match(dat$term, names(pred.labels))))]
    rp <- as.vector(na.omit(match(dat$term, names(pred.labels))))

    dat$term[tr] <- unname(labs[rp])
  }

  if (isTRUE(auto.label) && sjmisc::is_empty(dv.labels)) {
    dv.labels <- sjmisc::word_wrap(
      sjlabelled::get_dv_labels(models, mark.cat = TRUE, case = case),
      wrap = wrap.labels,
      linesep = "<br>"
    )
  }


  # does user want a specific order for terms?

  if (!is.null(order.terms)) {
    if (length(order.terms) == nrow(dat)) {
      dat <- dat[order.terms, ]
    } else {
      message("Number of values in `order.terms` does not match number of terms. Terms are not sorted.")
    }
  }

  tab_model_df(dat, zeroinf.list, title = title)
}


#' @importFrom tidyselect starts_with
#' @importFrom dplyr select slice
remove_unwanted <- function(dat, show.intercept, show.est, show.std, show.ci, show.se, show.stat, show.p, terms, rm.terms) {
  if (!show.intercept) {
    dat <- dplyr::slice(dat, -1)
  }

  if (show.est == FALSE) {
    dat <- dplyr::select(
      dat,
      -tidyselect::starts_with("estimate"),
      -tidyselect::starts_with("conf"),
      -tidyselect::starts_with("std.error")
    )
  }

  if (is.null(show.std) || show.std == FALSE) {
    dat <- dplyr::select(dat, -tidyselect::starts_with("std.estimate"))
  }

  if (is.null(show.ci) || show.ci == FALSE) {
    dat <- dplyr::select(dat, -tidyselect::starts_with("conf"), -tidyselect::starts_with("std.conf"))
  }

  if (is.null(show.se) || show.se == FALSE) {
    dat <- dplyr::select(dat, -tidyselect::starts_with("std.error"), -tidyselect::starts_with("std.se"))
  }

  if (show.stat == FALSE) {
    dat <- dplyr::select(dat, -tidyselect::starts_with("statistic"))
  }

  if (show.p == FALSE) {
    dat <- dplyr::select(dat, -tidyselect::starts_with("p.value"))
  }

  if (!is.null(terms)) {
    keep <- which(dat$term %in% terms)
    dat <- dplyr::slice(dat, !! keep)
  }

  if (!is.null(rm.terms)) {
    keep <- which(!(dat$term %in% rm.terms))
    dat <- dplyr::slice(dat, !! keep)
  }

  dat
}
