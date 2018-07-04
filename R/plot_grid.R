#' @title Arrange list of plots as grid
#' @name plot_grid
#'
#' @description Plot multiple ggplot-objects as a grid-arranged single plot.
#'
#' @param x A list of ggplot-objects. See 'Details'.
#' @param margin A numeric vector of length 4, indicating the top, right, bottom
#'        and left margin for each plot, in centimetres.
#'
#' @return An object of class \code{gtable}.
#'
#' @details This function takes a \code{list} of ggplot-objects as argument.
#'          Plotting functions of this package that produce multiple plot
#'          objects (e.g., when there is an argument \code{facet.grid}) usually
#'          return multiple plots as list (the return value is named \code{plot.list}).
#'          To arrange these plots as grid as a single plot, use \code{plot_grid}.
#'
#' @examples
#' library(ggeffects)
#' data(efc)
#'
#' # fit model
#' fit <- glm(
#'   tot_sc_e ~ c12hour + e17age + e42dep + neg_c_7,
#'   data = efc,
#'   family = poisson
#' )
#'
#' # plot marginal effects for each predictor, each as single plot
#' p1 <- ggpredict(fit, "c12hour") %>%
#'   plot(show.y.title = FALSE, show.title = FALSE)
#' p2 <- ggpredict(fit, "e17age") %>%
#'   plot(show.y.title = FALSE, show.title = FALSE)
#' p3 <- ggpredict(fit, "e42dep") %>%
#'   plot(show.y.title = FALSE, show.title = FALSE)
#' p4 <- ggpredict(fit, "neg_c_7") %>%
#'   plot(show.y.title = FALSE, show.title = FALSE)
#'
#' # plot grid
#' plot_grid(list(p1, p2, p3, p4))
#'
#' @export
plot_grid <- function(x, margin = c(1, 1, 1, 1)) {
  # check package availability -----
  if (!requireNamespace("gridExtra", quietly = TRUE)) {
    stop("Package `gridExtra` needed for this function to work. Please install it.", call. = F)
  }

  # if user did not pass plot.list value, but the complete object returned
  # by sjPlot-functions, get plot-list then
  if (!inherits(x, "list") || inherits(x, "sjPlot")) x <- x[["plot.list"]]

  # add margin to each plot, so no axis labels are cropped
  x <- lapply(x, function(pl) {
    pl + theme(plot.margin = unit(margin, "cm"))
  })

  # compute amount of columns and rows
  ncol <- round(sqrt(length(x)))
  nrow <- ceiling(length(x) / ncol)

  f <- eval(bquote(gridExtra::"grid.arrange"))
  do.call(f, c(x, nrow = nrow, ncol = ncol))
}
