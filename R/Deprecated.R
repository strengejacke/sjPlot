#' @title Deprecated functions
#' @name sjp.glmer
#'
#' @description A list of deprecated functions.
#'
#' @param ... Not used.
#' @return Nothing.
#'
#' @export
sjp.glmer <- function(...) {
  .Defunct("plot_model", package = "sjPlot", msg = "`sjp.glmer()` is defunct. Please use `plot_model()` instead.")
}

#' @rdname sjp.glmer
#' @export
sjp.glm <- function(...) {
  .Defunct("plot_model", package = "sjPlot", msg = "`sjp.glm()` is defunct. Please use `plot_model()` instead.")
}


#' @rdname sjp.glmer
#' @export
sjp.lmer <- function(...) {
  .Defunct("plot_model", package = "sjPlot", msg = "`sjp.lmer()` is defunct. Please use `plot_model()` instead.")
}


#' @rdname sjp.glmer
#' @export
sjp.lm <- function(...) {
  .Defunct("plot_model", package = "sjPlot", msg = "`sjp.lm()` is defunct. Please use `plot_model()` instead.")
}

#' @rdname sjp.glmer
#' @export
sjp.int <- function(...) {
  .Defunct("plot_model", package = "sjPlot", msg = "`sjp.int()` is defunct. Please use `plot_model()` instead.")
}

#' @rdname sjp.glmer
#' @export
sjt.frq <- function(...) {
  .Defunct("frq", package = "sjmisc", msg = "`sjt.frq()` is defunct. Please use `sjmisc::frq()` instead.")
}

#' @rdname sjp.glmer
#' @export
sjp.scatter <- function(...) {
  .Defunct("plot_scatter", package = "sjPlot", msg = "`sjp.scatter()` is defunct. Please use `plot_scatter()` instead.")
}

#' @rdname sjp.glmer
#' @export
sjp.resid <- function(...) {
  .Defunct("plot_residuals", package = "sjPlot", msg = "`sjp.resid()` is defunct. Please use `plot_residuals()` instead.")
}

#' @rdname sjp.glmer
#' @export
sjp.likert <- function(...) {
  .Defunct("plot_likert", package = "sjPlot", msg = "`sjp.likert()` is defunct. Please use `plot_likert()` instead.")
}

#' @rdname sjp.glmer
#' @export
sjp.gpt <- function(...) {
  .Defunct("plot_gpt", package = "sjPlot", msg = "`sjp.gpt()` is defunct. Please use `plot_gpt()` instead.")
}
