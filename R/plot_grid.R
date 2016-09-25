#' @title Arrange list of plots as grid
#' @name plot_grid
#'
#' @description Plot multiple ggplot-objects as a grid-arranged single plot.
#'
#' @param x A list of ggplot-objects. See 'Details'.
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
#' library(sjmisc)
#' data(efc)
#' # fit model
#' fit <- lm(tot_sc_e ~ c12hour + e17age + e42dep + neg_c_7, data = efc)
#' # plot marginal effects for each predictor, each as single plot
#' p <- sjp.lm(fit, type = "eff", facet.grid = FALSE)
#' 
#' # plot grid
#' plot_grid(p$plot.list)
#' 
#' # or
#' plot_grid(p)
#'
#' @export
plot_grid <- function(x) {
  # check package availability -----
  if (!requireNamespace("gridExtra", quietly = TRUE)) {
    stop("Package `gridExtra` needed for this function to work. Please install it.", call. = F)
  }
  
  # if user did not pass plot.list value, but the complete object returned
  # by sjPlot-functions, get plot-list then
  if (class(x) != "list" || any(class(x) == "sjPlot"))
    x <- x[["plot.list"]]
  
  f <- eval(bquote(gridExtra::"grid.arrange"))
  do.call(f, c(x, ncol = floor(sqrt(length(x)))))
}
