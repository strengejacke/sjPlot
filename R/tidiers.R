#' @importFrom sjstats hdi
#' @importFrom sjmisc var_rename add_columns
#' @importFrom dplyr select
#' @importFrom tibble add_column
#' @importFrom  purrr map_dbl
#' @importFrom stats median
tidy_stan <- function(fit, ci.lvl, exponentiate) {
  # check if values should be exponentiated
  if (exponentiate)
    funtrans <- "exp"
  else
    funtrans <- NULL

  # get two HDI-intervals
  d1 <- sjstats::hdi(fit, prob = ci.lvl, trans = funtrans)
  d2 <- sjstats::hdi(fit, prob = .5, trans = funtrans)

  # bind columns, so we have inner and outer hdi interval
  dat <- d2 %>%
    dplyr::select(.data$hdi.low, .data$hdi.high) %>%
    sjmisc::var_rename(hdi.low = "conf.low50", hdi.high = "conf.high50") %>%
    sjmisc::add_columns(d1) %>%
    sjmisc::var_rename(hdi.low = "conf.low", hdi.high = "conf.high")

  dat %>%
    tibble::add_column(
      estimate = purrr::map_dbl(as.data.frame(fit), stats::median),
      .after = 1
    ) %>%
    tibble::add_column(p.value = 0)
}



