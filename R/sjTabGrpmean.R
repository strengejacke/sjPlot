#' @title Deprecated functions
#' @name sjt.grpmean
#'
#' @description A list of deprecated functions.
#'
#' @param ... Not used.
#' @return Nothing.
#'
#' @export
sjt.grpmean <- function(...) {
  .Deprecated("grpmean", package = "sjstats", msg = "`sjt.grpmean()` is deprecated. Please use `sjstats::grpmean()` instead.")
}

#' @rdname sjt.grpmean
#' @export
sjt.df <- function(...) {
  .Deprecated("descr", package = "sjmisc", msg = "`sjt.df()` is deprecated. Please use `sjmisc::descr()` or `sjPlot::tab_df()` instead.")
}

#' @rdname sjt.grpmean
#' @export
sjt.mwu <- function(...) {
  .Deprecated("mwu", package = "sjstats", msg = "`sjt.mwu()` is deprecated. Please use `sjstats::mwu()` instead.")
}
