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
#'    \code{NULL}, all terms are printed. Note that the term names must match
#'    the names of the model's coefficients. For factors, this means that
#'    the variable name is suffixed with the related factor level, and each
#'    category counts as one term. E.g. \code{rm.terms = "t_name [2,3]"}
#'    would remove the terms \code{"t_name2"} and \code{"t_name3"} (assuming
#'    that the variable \code{t_name} is categorical and has at least
#'    the factor levels \code{2} and \code{3}). Another example for the
#'    \emph{iris}-dataset: \code{terms = "Species"} would not work, instead
#'    you would \code{terms = "Species [versicolor,virginica]"}.
#' @param rm.terms Character vector with names that indicate which terms should
#'    be removed from the output Counterpart to \code{terms}. \code{rm.terms =
#'    "t_name"} would remove the term \emph{t_name}. Default is \code{NULL}, i.e.
#'    all terms are used. For factors, levels that should be removed from the plot
#'    need to be explicitely indicated in square brackets, and match the model's
#'    coefficient names, e.g. \code{rm.terms = "t_name [2,3]"} would remove the terms
#'    \code{"t_name2"} and \code{"t_name3"} (assuming that the variable \code{t_name}
#'    was categorical and has at least the factor levels \code{2} and \code{3}).
#' @param pred.labels Character vector with labels of predictor variables.
#'    If not \code{NULL}, \code{pred.labels} will be used in the first
#'    table column with the predictors' names. By default, if \code{auto.label = TRUE}
#'    and \code{\link[sjlabelled]{get_term_labels}} is called to retrieve the labels
#'    of the coefficients, which will be used as predictor labels.
#'    If \code{pred.labels = ""} or \code{auto.label = FALSE}, the raw
#'    variable names as used in the model formula are used as predictor
#'    labels. If \code{pred.labels} is a named vector, predictor labels (by
#'    default, the names of the model's coefficients) will be matched with the
#'    names of \code{pred.labels}. This ensures that labels always match the
#'    related predictor in the table, no matter in which way the predictors
#'    are sorted. See 'Examples'.
#' @param dv.labels Character vector with labels of dependent variables of all
#'    fitted models. See 'Examples'.
#' @param show.intercept Logical, if \code{TRUE}, the intercepts are printed.
#' @param show.est Logical, if \code{TRUE}, the estimates are printed.
#' @param show.zeroinf Logical, if \code{TRUE} and model has a zero-inflated
#'    model part, this is also printed to the table.
#' @param show.re.var Logical, if \code{TRUE}, prints the random effect variances
#'    for mixed models. See \code{\link[sjstats]{re_var}} for details.
#' @param show.icc Logical, if \code{TRUE}, prints the intraclass correlation
#'    coefficient for mixed models. See \code{\link[sjstats]{icc}} for details.
#' @param show.dev Logical, if \code{TRUE}, shows the deviance of the model.
#' @param show.ci Either logical, and if \code{TRUE}, the confidence intervals
#'    is printed to the table; if \code{FALSE}, confidence intervals are
#'    omitted. Or numeric, between 0 and 1, indicating the range of the
#'    confidence intervals.
#' @param show.std Indicates whether standardized beta-coefficients should
#'    also printed, and if yes, which type of standardization is done.
#'    See 'Details'.
#' @param show.se Logical, if \code{TRUE}, the standard errors are also printed.
#' @param show.r2 Logical, if \code{TRUE}, the r-squared value is also printed.
#'    Depending on the model, these might be pseudo-r-squared values, or Bayesian
#'    r-squared etc. See \code{\link[sjstats]{r2}} for details.
#' @param show.stat Logical, if \code{TRUE}, the coefficients' test statistic
#'    is also printed.
#' @param show.df Logical, if \code{TRUE} and \code{p.val = "kr"}, the p-values
#'    for linear mixed models are based on df with Kenward-Rogers approximation.
#'    These df-values are printed. See \code{\link[sjstats]{p_value}} for details.
#' @param string.pred Character vector,used as headline for the predictor column.
#'    Default is \code{"Predictors"}.
#' @param string.std Character vector, used for the column heading of standardized beta coefficients. Default is \code{"std. Beta"}.
#' @param string.ci Character vector, used for the column heading of confidence interval values. Default is \code{"CI"}.
#' @param string.se Character vector, used for the column heading of standard error values. Default is \code{"std. Error"}.
#' @param string.p Character vector, used for the column heading of p values. Default is \code{"p"}.
#' @param ci.hyphen Character vector, indicating the hyphen for confidence interval range.
#'    May be an HTML entity. See 'Examples'.
#' @param minus.sign string, indicating the minus sign for negative numbers.
#'    May be an HTML entity. See 'Examples'.
#' @param emph.p Logical, if \code{TRUE}, significant p-values are shown bold faced.
#' @param digits Amount of decimals for estimates
#' @param digits.p Amount of decimals for p-values
#' @param separate.ci.col Logical, if \code{TRUE}, the CI values are shown in
#'    a separate table column.
#' @param separate.se.col Logical, if \code{TRUE}, the SE values are shown in
#'    a separate table column.
#' @param group.terms Logical, if \code{TRUE} (default), automatically groups table rows with
#'    factor levels of same factor, i.e. predictors of type \code{\link{factor}} will
#'    be grouped, if the factor has more than two levels. Grouping means that a separate headline
#'    row is inserted to the table just before the predictor values.
#'
#' @inheritParams plot_models
#' @inheritParams plot_model
#'
#
#' @importFrom dplyr full_join select if_else mutate
#' @importFrom tibble lst add_case as_tibble
#' @importFrom purrr reduce map2 map_if map_df compact map_lgl map_chr flatten_chr
#' @importFrom sjlabelled get_dv_labels get_term_labels
#' @importFrom sjmisc word_wrap var_rename add_columns
#' @importFrom sjstats std_beta model_family r2 icc resp_var
#' @importFrom stats nobs
#' @importFrom rlang .data
#' @export
tab_model <- function(
  ...,
  transform,

  show.intercept = TRUE,
  show.est = TRUE,
  show.ci = .95,
  show.hdi50 = TRUE,
  show.se = NULL,
  show.std = NULL,
  show.p = TRUE,
  show.stat = FALSE,
  show.df = FALSE,

  show.zeroinf = TRUE,
  show.r2 = TRUE,
  show.icc = TRUE,
  show.re.var = TRUE,
  show.fstat = FALSE,
  show.aic = FALSE,
  show.aicc = FALSE,
  show.dev = FALSE,
  show.obs = TRUE,

  terms = NULL,
  rm.terms = NULL,
  group.terms = TRUE,
  order.terms = NULL,

  title = NULL,
  pred.labels = NULL,
  dv.labels = NULL,
  wrap.labels = 25,

  string.pred = "Predictors",
  string.std = "std. Beta",
  string.ci = "CI",
  string.se = "std. Error",
  string.p = "p",
  string.df = "df",
  string.stat = "Statistic",
  ci.hyphen = "&nbsp;&ndash;&nbsp;",
  minus.sign = "&#45;",

  separate.ci.col = TRUE,
  separate.se.col = TRUE,

  digits = 2,
  digits.p = 3,
  emph.p = TRUE,
  p.val = c("wald", "kr"),

  case = "parsed",
  auto.label = TRUE,
  bpe = "median",
  CSS = css_theme("regression")
) {

  p.val <- match.arg(p.val)

  models <- tibble::lst(...)
  auto.transform <- missing(transform)
  ci.lvl <- ifelse(is.null(show.ci), .95, show.ci)

  model.list <- purrr::map2(
    models,
    1:length(models),
    function(model, i) {

      # get info on model family
      fam.info <- sjstats::model_family(model)

      # check whether estimates should be transformed or not

      if (auto.transform) {
        if (fam.info$is_linear)
          transform <- NULL
        else
          transform <- "exp"
      }

      # get tidy output of summary ----

      dat <- tidy_model(
        model,
        ci.lvl = ci.lvl,
        transform,
        type = "est",
        bpe,
        se = show.se,
        facets = FALSE,
        show.zeroinf = show.zeroinf,
        p.val = p.val
      )


      # transform estimates

      if (!is.stan(model) && !is.null(transform)) {
        funtrans <- match.fun(transform)
        dat[["estimate"]] <- funtrans(dat[["estimate"]])
        dat[["conf.low"]] <- funtrans(dat[["conf.low"]])
        dat[["conf.high"]] <- funtrans(dat[["conf.high"]])
      }


      # merge CI columns

      dat <- dat %>%
        dplyr::mutate(conf.int = sprintf(
          "%.*f%s%.*f",
          digits,
          .data$conf.low,
          ci.hyphen,
          digits,
          .data$conf.high
        )) %>%
        dplyr::select(-.data$conf.low, -.data$conf.high) %>%
        dplyr::mutate(
          p.sig = .data$p.value < .05,
          p.value = sprintf("%.*f", digits.p, .data$p.value)
        )


      # emphasize p-values ----

      if (emph.p) dat$p.value[dat$p.sig] <- sprintf("<strong>%s</strong>", dat$p.value[dat$p.sig])
      dat <- dplyr::select(dat, -.data$p.sig)


      # get inner probability (i.e. 2nd CI for Stan-models) ----

      if (is.stan(model)) {
        dat <- dat %>%
          sjmisc::var_rename(conf.int = "hdi.outer") %>%
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


      # indicate p <0.001 ----

      pv <- paste0("0.", paste(rep("0", digits.p), collapse = ""))
      dat$p.value[dat$p.value == pv] <- "&lt;0.001"

      pv <- paste0("<strong>0.", paste(rep("0", digits.p), collapse = ""), "</strong>")
      dat$p.value[dat$p.value == pv] <- "<strong>&lt;0.001"


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

      }


      # switch column for p-value and conf. int. ----

      dat <- dat[, sort_columns(colnames(dat), is.stan(model))]


      # add suffix to column names, so we can distinguish models later

      cn <- colnames(dat)[2:ncol(dat)]
      colnames(dat)[2:ncol(dat)] <- sprintf("%s_%i", cn, i)


      # for HTML, convert numerics to character ----

      dat <- dat %>%
        purrr::map_if(is.numeric, ~ sprintf("%.*f", digits, .x)) %>%
        tibble::as_tibble()


      # remove 2nd HDI if requested ----

      if (!show.hdi50)
        dat <- dplyr::select(dat, -tidyselect::starts_with("hdi.inner"))


      ## TODO optionally insert linebreak for new-line-CI / SE

      # merge estimates and CI / SE columns, if requested ----

      if (!separate.ci.col) {
        est.cols <- tidyselect::starts_with("estimate", vars = colnames(dat))
        dat[[est.cols]] <- sprintf("%s (%s)", dat[[est.cols]], dat[[est.cols + 2]])

        # for stan models, we also have 50% HDI
        if (!sjmisc::is_empty(tidyselect::starts_with("hdi", vars = colnames(dat)))) {
          dat <- dplyr::select(dat, -tidyselect::starts_with("hdi.outer"))
          dat[[est.cols]] <- sprintf("%s (%s)", dat[[est.cols]], dat[[est.cols + 2]])
          dat <- dplyr::select(dat, -tidyselect::starts_with("hdi.inner"))
        } else {
          dat <- dplyr::select(dat, -tidyselect::starts_with("conf.int"))
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


      # handle zero-inflation part ----

      zidat <- NULL
      wf <- tidyselect::starts_with("wrap.facet", vars = colnames(dat))

      if (!sjmisc::is_empty(wf)) {
        zi <- which(dat[[wf]] == "Zero-Inflated Model")

        if (show.zeroinf && !sjmisc::is_empty(zi)) {
          zidat <- dat %>%
            dplyr::slice(!! zi) %>%
            dplyr::select(!! -wf)
        }

        if (!sjmisc::is_empty(zi)) dat <- dplyr::slice(dat, !! -zi)
        dat <- dplyr::select(dat, !! -wf)
      }


      ## TODO add another object with summary information, e.g. ICC, F-Stat, AIC etc.

      # Add r-squared statistic ----

      rsq <- NULL

      if (show.r2) {
        rsq <- tryCatch(
          suppressWarnings(sjstats::r2(model)),
          error = function(x) { NULL }
        )
      }


      # Add no of observations statistic ----

      n_obs <- NULL

      if (show.obs) {
        n_obs <- tryCatch(
          stats::nobs(model),
          error = function(x) { NULL }
        )
      }


      # Add ICC statistic ----

      icc <- NULL

      if ((show.icc || show.re.var) && is_mixed_model(model)) {
        icc <- tryCatch(
          sjstats::icc(model),
          error = function(x) { NULL }
        )
      }


      # Add deviance statistic ----

      dev <- NULL
      if (show.dev) dev <- model_deviance(model)


      list(
        dat = dat,
        transform = transform,
        zeroinf = zidat,
        rsq = rsq,
        n_obs = n_obs,
        icc = icc,
        dev = dev
      )
    }
  )


  # join all model data frames and convert to character ----

  na.vals <- c(
    "NA",
    sprintf("NA%sNA", ci.hyphen),
    sprintf("NA (NA%sNA)", ci.hyphen),
    sprintf("NA (NA%sNA) (NA)", ci.hyphen)
  )

  # we have data for fixed effects and zero inflation part as
  # well as transformation of coefficients in a list, so separate
  # them out into own objects

  model.data <- purrr::map(model.list, ~.x[[1]])
  transform.data <- purrr::map(model.list, ~.x[[2]])
  zeroinf.data <- purrr::map(model.list, ~.x[[3]])
  rsq.data <- purrr::map(model.list, ~.x[[4]])
  n_obs.data <- purrr::map(model.list, ~.x[[5]])
  icc.data <- purrr::map(model.list, ~.x[[6]])
  dev.data <- purrr::map(model.list, ~.x[[7]])
  is.zeroinf <- purrr::map_lgl(model.list, ~ !is.null(.x[[3]]))

  zeroinf.data <- purrr::compact(zeroinf.data)


  # sort multivariate response models by response level

  model.data <- purrr::map(model.data, function(.x) {
    resp.col <- tidyselect::starts_with("response.level", vars = colnames(.x))
    if (!sjmisc::is_empty(resp.col))
      .x[order(match(.x[[resp.col]], unique(.x[[resp.col]]))), ]
    else
      .x
  })


  # if only one multivariate response model, split data
  # to print models side by side, and update labels of
  # dependent variables

  show.response <- TRUE

  if (length(model.data) == 1) {
    fi <- sjstats::model_family(models[[1]])
    if (fi$is_multivariate) {
      show.response <- FALSE

      dv.labels <- sjmisc::word_wrap(
        sjlabelled::get_dv_labels(models, multi.resp = TRUE, case = case),
        wrap = wrap.labels,
        linesep = "<br>"
      )

      if (sjmisc::is_empty(dv.labels) || !isTRUE(auto.label))
        dv.labels <- sjstats::resp_var(models[[1]])

      model.data <- split(model.data[[1]], model.data[[1]]["response.level_1"])
      dv.labels <- dv.labels[match(names(dv.labels), names(model.data))]

      model.data <- purrr::map2(model.data, 1:length(model.data), function(x, y) {
        colnames(x) <- gsub(
          pattern = "_1",
          replacement = sprintf("_%i", y),
          x = colnames(x)
        )
        x
      })
    }
  }


  # Join all models into one data frame, and replace NA by empty strings

  dat <- model.data %>%
    purrr::reduce(~ dplyr::full_join(.x, .y, by = "term")) %>%
    purrr::map_df(~ dplyr::if_else(.x %in% na.vals | is.na(.x), "", .x))

  # remove unwanted columns and rows ----

  dat <-
    remove_unwanted(
      dat,
      show.intercept,
      show.est,
      show.std,
      show.ci,
      show.se,
      show.stat,
      show.p,
      show.df,
      show.response,
      terms,
      rm.terms
    )


  # same for zero-inflated parts ----

  zeroinf <- NULL
  if (!sjmisc::is_empty(zeroinf.data)) {
    zeroinf <- zeroinf.data %>%
      purrr::reduce(~ dplyr::full_join(.x, .y, by = "term")) %>%
      purrr::map_df(~ dplyr::if_else(.x %in% na.vals | is.na(.x), "", .x))

    zeroinf <-
      remove_unwanted(
        zeroinf,
        show.intercept,
        show.est,
        show.std,
        show.ci,
        show.se,
        show.stat,
        show.p,
        show.df,
        show.response,
        terms,
        rm.terms
      )
  }


  # get default labels for dv and terms ----

  if (isTRUE(auto.label) && sjmisc::is_empty(pred.labels)) {
    pred.labels <- sjlabelled::get_term_labels(models, mark.cat = TRUE, case = case)
    pred.labels <- pred.labels[!duplicated(names(pred.labels))]
    pred.labels <- prepare.labels(pred.labels, grp = group.terms)
  } else {
    # no automatic grouping of table rows for categorical variables
    # when user supplies own labels
    group.terms <- FALSE
  }


  # to insert "header" rows for categorical variables, we need to
  # save the original term names first.

  # remember.terms <- dat$term


  # named vector for predictor labels means we try to match labels
  # with model terms

  if (!sjmisc::is_empty(pred.labels)) {

    if (!is.null(names(pred.labels))) {
      labs <- sjmisc::word_wrap(pred.labels, wrap = wrap.labels, linesep = "<br>")
      # some labels may not match. in this case, we only need to replace those
      # elements in the vector that match a specific label, but
      # at the correct position inside "dat$term"
      tr <- 1:nrow(dat)
      find.matches <- match(dat$term, names(pred.labels))
      find.na <- which(is.na(find.matches))
      if (!sjmisc::is_empty(find.na)) tr <- tr[-find.na]
      rp <- as.vector(stats::na.omit(find.matches))

      dat$term[tr] <- unname(labs[rp])

      # also label zero-inflated part

      if (!is.null(zeroinf)) {
        tr <- 1:nrow(zeroinf)
        find.matches <- match(zeroinf$term, names(pred.labels))
        find.na <- which(is.na(find.matches))
        if (!sjmisc::is_empty(find.na)) tr <- tr[-find.na]
        rp <- as.vector(stats::na.omit(find.matches))

        zeroinf$term[tr] <- unname(labs[rp])
      }

    } else {
      if (length(pred.labels) == nrow(dat))
        dat$term <- pred.labels
      else
        message("Length of `pred.labels` does not equal number of predictors, no labelling applied.")
    }
  }


  if (isTRUE(auto.label) && sjmisc::is_empty(dv.labels)) {
    dv.labels <- sjmisc::word_wrap(
      sjlabelled::get_dv_labels(models, case = case),
      wrap = wrap.labels,
      linesep = "<br>"
    )
  } else if (sjmisc::is_empty(dv.labels)) {
    dv.labels <- purrr::map(models, sjstats::resp_var) %>% purrr::flatten_chr()
  }


  # group terms ----

  # if (group.terms) {
  #   ## TODO group terms by variables, so category values of factors are "grouped"
  #   remember.terms[attr(pred.labels, "category.value")]
  # }


  # does user want a specific order for terms?

  if (!is.null(order.terms)) {
    if (length(order.terms) == nrow(dat)) {
      dat <- dat[order.terms, ]
    } else {
      message("Number of values in `order.terms` does not match number of terms. Terms are not sorted.")
    }
  }


  # get proper column header labels ----

  col.header <- purrr::map_chr(colnames(dat), function(x) {
    pos <- grep("estimate", x, fixed = T)

    if (!sjmisc::is_empty(pos)) {
      i <- as.numeric(sub("estimate_", "", x = x, fixed = T))

      if (i <= length(models)) {
        x <- estimate_axis_title(
          models[[i]],
          axis.title = NULL,
          type = "est",
          transform = transform.data[[i]],
          multi.resp = NULL
        )
      } else if (length(models) == 1) {

        fi <- sjstats::model_family(models[[1]])

        if (fi$is_multivariate)
          mr <- i
        else
          mr <- NULL

        x <- estimate_axis_title(
          models[[1]],
          axis.title = NULL,
          type = "est",
          transform = transform.data[[1]],
          multi.resp = mr
        )

      } else {
        x <- "Estimate"
      }
    }


    pos <- grep("term", x, fixed = T)
    if (!sjmisc::is_empty(pos)) x <- string.pred

    pos <- grep("conf.int", x, fixed = T)
    if (!sjmisc::is_empty(pos)) x <- string.ci

    pos <- grep("std.error", x, fixed = T)
    if (!sjmisc::is_empty(pos)) x <- string.se

    pos <- grep("std.estimate", x, fixed = T)
    if (!sjmisc::is_empty(pos)) x <- string.std

    pos <- grep("std.se", x, fixed = T)
    if (!sjmisc::is_empty(pos)) x <- paste("std.", string.se)

    pos <- grep("std.conf.int", x, fixed = T)
    if (!sjmisc::is_empty(pos)) x <- paste("std.", string.ci)

    pos <- grep("p.value", x, fixed = T)
    if (!sjmisc::is_empty(pos)) x <- string.p

    pos <- grep("df", x, fixed = T)
    if (!sjmisc::is_empty(pos)) x <- string.df

    pos <- grep("statistic", x, fixed = T)
    if (!sjmisc::is_empty(pos)) x <- string.stat

    pos <- grep("response.level", x, fixed = T)
    if (!sjmisc::is_empty(pos)) x <- "Response"

    pos <- grep("hdi.inner", x, fixed = T)
    if (!sjmisc::is_empty(pos)) x <- "HDI (50%)"

    pos <- grep("hdi.outer", x, fixed = T)
    if (!sjmisc::is_empty(pos)) x <- sprintf("HDI (%i%%)", round(100 * show.ci))

    x
  })

  tab_model_df(
    x = dat,
    zeroinf = zeroinf,
    is.zeroinf = is.zeroinf,
    title = title,
    col.header = col.header,
    dv.labels = dv.labels,
    rsq.list = rsq.data,
    n_obs.list = n_obs.data,
    icc.list = icc.data,
    dev.list = dev.data,
    n.models = length(model.list),
    show.re.var = show.re.var,
    show.icc = show.icc,
    CSS = CSS
  )
}


#' @importFrom stats na.omit
sort_columns <- function(x, is.stan) {
  ## TODO check code for multiple response models

  reihe <- c(
    "term",
    "estimate",
    "std.error",
    "std.estimate",
    "std.se",
    "conf.int",
    "std.conf.int",
    "hdi.inner",
    "hdi.outer",
    "statistic",
    "p.value",
    "df",
    "wrap.facet",
    "response.level"
  )

  if (is.stan) reihe <- reihe[-which(reihe == "p.value")]
  as.vector(stats::na.omit(match(reihe, x)))
}


#' @importFrom tidyselect starts_with
#' @importFrom dplyr select slice
remove_unwanted <- function(dat, show.intercept, show.est, show.std, show.ci, show.se, show.stat, show.p, show.df, show.response, terms, rm.terms) {
  if (!show.intercept) {
    ints1 <- tidyselect::contains("(Intercept", vars = dat$term)
    ints2 <- tidyselect::contains("b_Intercept", vars = dat$term)
    ints3 <- tidyselect::contains("b_zi_Intercept", vars = dat$term)
    ints4 <- which(dat$term %in% "Intercept")

    ints <- c(ints1, ints2, ints3, ints4)

    if (!sjmisc::is_empty(ints))
      dat <- dplyr::slice(dat, !! -ints)
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
    dat <- dplyr::select(
      dat,
      -tidyselect::starts_with("conf"),
      -tidyselect::starts_with("std.conf"),
      -tidyselect::starts_with("hdi")
    )
  }

  if (is.null(show.se) || show.se == FALSE) {
    dat <- dplyr::select(dat, -tidyselect::starts_with("std.error"), -tidyselect::starts_with("std.se"))
  }

  if (show.stat == FALSE) {
    dat <- dplyr::select(dat, -tidyselect::starts_with("statistic"))
  }

  if (show.response == FALSE) {
    dat <- dplyr::select(dat, -tidyselect::starts_with("response.level"))
  }

  if (show.p == FALSE) {
    dat <- dplyr::select(dat, -tidyselect::starts_with("p.value"))
  }

  if (show.df == FALSE) {
    dat <- dplyr::select(dat, -tidyselect::starts_with("df"))
  }

  if (!is.null(terms)) {
    terms <- parse_terms(terms)
    keep <- which(dat$term %in% terms)
    dat <- dplyr::slice(dat, !! keep)
  }

  if (!is.null(rm.terms)) {
    rm.terms <- parse_terms(rm.terms)
    keep <- which(!(dat$term %in% rm.terms))
    dat <- dplyr::slice(dat, !! keep)
  }

  dat
}


#' @importFrom tidyselect starts_with
prepare.labels <- function(x, grp) {
  x_var <- names(x[attr(x, "category.value") == FALSE])
  x_val <- names(x[attr(x, "category.value") == TRUE])

  for (i in x_var) {
    pos <- tidyselect::starts_with(i, vars = x_val)

    if (!grp || (length(pos) > 0 && length(pos) < 3)) {
      match.vals <- x_val[pos]
      x[match.vals] <- sprintf("%s: %s", x[i], x[match.vals])
    }
  }

  x
}
