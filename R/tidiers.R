## TODO provide own tidier for not-supported models

tidy_model <- function(model, ci.lvl, exponentiate, type, bpe, ...) {
  if (is.stan(model))
    tidy_stan_model(model, ci.lvl, exponentiate, type, bpe, ...)
  else if (inherits(model, "lme"))
    tidy_lme_model(model, ci.lvl)
  else if (inherits(model, "gls"))
    tidy_gls_model(model, ci.lvl)
  else if (inherits(model, "coxph"))
    tidy_cox_model(model, ci.lvl)
  else
    tidy_generic(model, ci.lvl)
}



#' @importFrom broom tidy
#' @importFrom tibble has_name
#' @importFrom sjstats p_value
tidy_generic <- function(model, ci.lvl) {
  # tidy the model
  dat <- broom::tidy(model, conf.int = TRUE, conf.level = ci.lvl, effects = "fixed")

  # see if we have p-values. if not, add them
  if (!tibble::has_name(dat, "p.value"))
    dat$p.value <- sjstats::p_value(model)[["p.value"]]

  dat
}


#' @importFrom broom tidy
#' @importFrom tibble has_name
#' @importFrom sjstats p_value
tidy_cox_model <- function(model, ci.lvl) {
  # tidy the model
  dat <- broom::tidy(model, conf.int = ci.lvl)

  # see if we have p-values. if not, add them
  if (!tibble::has_name(dat, "p.value"))
    dat$p.value <- sjstats::p_value(model)[["p.value"]]

  dat
}


#' @importFrom sjstats hdi typical_value
#' @importFrom sjmisc var_rename add_columns is_empty
#' @importFrom dplyr select filter slice
#' @importFrom tibble add_column
#' @importFrom purrr map_dbl
#' @importFrom rlang .data
#' @importFrom tidyselect starts_with
tidy_stan_model <- function(model, ci.lvl, exponentiate, type, bpe, ...) {

  # check if values should be exponentiated

  if (exponentiate)
    funtrans <- "exp"
  else
    funtrans <- NULL


  # set defaults

  p.inner <- .5
  p.outer <- ci.lvl


  # additional arguments for 'effects()'-function?
  add.args <- lapply(match.call(expand.dots = F)$`...`, function(x) x)

  # check whether we have "prob.inner" and "prob.outer" argument
  # and if so, use these for HDI and Bayesian point estimate

  if ("prob.inner" %in% names(add.args)) p.inner <- add.args[["prob.inner"]]
  if ("prob.outer" %in% names(add.args)) p.outer <- add.args[["prob.outer"]]


  # get two HDI-intervals

  d1 <- sjstats::hdi(model, prob = p.outer, trans = funtrans, type = "all")
  d2 <- sjstats::hdi(model, prob = p.inner, trans = funtrans, type = "all")


  # bind columns, so we have inner and outer hdi interval

  dat <- d2 %>%
    dplyr::select(.data$hdi.low, .data$hdi.high) %>%
    sjmisc::var_rename(hdi.low = "conf.low50", hdi.high = "conf.high50") %>%
    sjmisc::add_columns(d1) %>%
    sjmisc::var_rename(hdi.low = "conf.low", hdi.high = "conf.high")


  # add bayesian point estimate

  dat <- dat %>%
    tibble::add_column(
      estimate = purrr::map_dbl(as.data.frame(model), sjstats::typical_value, bpe),
      .after = 1
    ) %>%
    tibble::add_column(p.value = 0)


  # remove sigma and lp__ row

  if ("sigma" %in% dat$term) dat <- dplyr::filter(dat, .data$term != "sigma")
  if ("lp__" %in% dat$term) dat <- dplyr::filter(dat, .data$term != "lp__")


  # check if we need to remove random effects

  if (type == "est") {
    # remove all random effect intercepts
    re <- tidyselect::starts_with("b[", vars = dat$term)
    if (!sjmisc::is_empty(re)) dat <- dplyr::slice(dat, !! -re)

    # and all random effect error terms
    re.s <- tidyselect::starts_with("Sigma[", vars = dat$term)
    if (!sjmisc::is_empty(re.s)) dat <- dplyr::slice(dat, !! -re.s)
  }


  # need to transform point estimate as well
  if (exponentiate) dat$estimate <- exp(dat$estimate)

  dat
}


#' @importFrom broom tidy
#' @importFrom tibble has_name
#' @importFrom sjstats p_value
#' @importFrom nlme intervals
tidy_lme_model <- function(model, ci.lvl) {
  # get tidy summary. for lme, this excludes CI,
  # so we compute them separately

  dat <- broom::tidy(model, conf.int = TRUE, conf.level = ci.lvl, effects = "fixed")
  ci <- as.data.frame(nlme::intervals(model, level = ci.lvl, which = "fixed")$fixed)

  dat$conf.low <- ci$lower
  dat$conf.high <- ci$upper

  # see if we have p-values. if not, add them
  if (!tibble::has_name(dat, "p.value"))
    dat$p.value <- sjstats::p_value(model)[["p.value"]]

  dat
}


#' @importFrom sjmisc var_rename
#' @importFrom nlme intervals
#' @importFrom dplyr select
#' @importFrom tibble rownames_to_column
tidy_gls_model <- function(model, ci.lvl) {
  # get tidy summary. for lme, this excludes CI,
  # so we compute them separately

  dat <- as.data.frame(summary(model)$tTable)
  ci <- as.data.frame(nlme::intervals(model, level = ci.lvl, which = "coef")$coef)

  dat$conf.low <- ci$lower
  dat$conf.high <- ci$upper


  sjmisc::var_rename(
    dat,
    Value = "estimate",
    Std.Error = "std.error",
    `t-value` = "statistic",
    `p-value` = "p.value"
  ) %>%
    tibble::rownames_to_column("term")
}
