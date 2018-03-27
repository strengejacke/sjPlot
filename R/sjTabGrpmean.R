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
  .Defunct("grpmean", package = "sjstats", msg = "`sjt.grpmean()` is defunct. Please use `sjstats::grpmean()` instead.")
}

#' @rdname sjt.grpmean
#' @export
sjt.df <- function(...) {
  .Defunct("descr", package = "sjmisc", msg = "`sjt.df()` is defunct. Please use `sjmisc::descr()` or `sjPlot::tab_df()` instead.")
}

#' @rdname sjt.grpmean
#' @export
sjt.mwu <- function(...) {
  .Defunct("mwu", package = "sjstats", msg = "`sjt.mwu()` is defunct. Please use `sjstats::mwu()` instead.")
}
