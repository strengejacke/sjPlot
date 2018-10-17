#' @title Deprecated functions
#' @name sjt.glmer
#'
#' @description A list of deprecated functions.
#'
#' @param ... Not used.
#' @return Nothing.
#'
#' @export
sjt.glmer <- function(...) {
  .Defunct("tab_model", package = "sjPlot", msg = "`sjt.glmer()` is defunct. Please use `tab_model()` instead.")
}

#' @rdname sjt.glmer
#' @export
sjt.glm <- function(...) {
  .Defunct("tab_model", package = "sjPlot", msg = "`sjt.glm()` is defunct. Please use `tab_model()` instead.")
}


#' @rdname sjt.glmer
#' @export
sjt.lmer <- function(...) {
  .Defunct("tab_model", package = "sjPlot", msg = "`sjt.lmer()` is defunct. Please use `tab_model()` instead.")
}


#' @rdname sjt.glmer
#' @export
sjt.lm <- function(...) {
  .Defunct("tab_model", package = "sjPlot", msg = "`sjt.lm()` is defunct. Please use `tab_model()` instead.")
}
