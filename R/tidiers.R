#' @importFrom sjstats hdi typical_value
#' @importFrom sjmisc var_rename add_columns
#' @importFrom dplyr select
#' @importFrom tibble add_column
#' @importFrom purrr map_dbl
tidy_stan <- function(fit, ci.lvl, exponentiate, ...) {
  # check if values should be exponentiated
  if (exponentiate)
    funtrans <- "exp"
  else
    funtrans <- NULL


  # set defaults
  p.inner <- .5
  p.outer <- ci.lvl
  best <- "median"


  # additional arguments for 'effects()'-function?
  add.args <- lapply(match.call(expand.dots = F)$`...`, function(x) x)

  # check whether we have "prob.inner", "prob.outer" and "bpe" argument
  # and if so, use these for HDI and Bayesian point estimate
  if ("prob.inner" %in% names(add.args)) p.inner <- add.args[["prob.inner"]]
  if ("prob.outer" %in% names(add.args)) p.outer <- add.args[["prob.outer"]]
  if ("bpe" %in% names(add.args)) best <- add.args[["bpe"]]


  # get two HDI-intervals
  d1 <- sjstats::hdi(fit, prob = p.outer, trans = funtrans)
  d2 <- sjstats::hdi(fit, prob = p.inner, trans = funtrans)

  # bind columns, so we have inner and outer hdi interval
  dat <- d2 %>%
    dplyr::select(.data$hdi.low, .data$hdi.high) %>%
    sjmisc::var_rename(hdi.low = "conf.low50", hdi.high = "conf.high50") %>%
    sjmisc::add_columns(d1) %>%
    sjmisc::var_rename(hdi.low = "conf.low", hdi.high = "conf.high")

  # add bayesian point estimate
  dat <- dat %>%
    tibble::add_column(
      estimate = purrr::map_dbl(as.data.frame(fit), sjstats::typical_value, best),
      .after = 1
    ) %>%
    tibble::add_column(p.value = 0)

  # need to transform point estimate as well
  if (exponentiate) dat$estimate <- exp(dat$estimate)

  dat
}



