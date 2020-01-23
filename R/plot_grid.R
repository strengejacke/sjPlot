#' @title Arrange list of plots as grid
#' @name plot_grid
#'
#' @description Plot multiple ggplot-objects as a grid-arranged single plot.
#'
#' @param x A list of ggplot-objects. See 'Details'.
#' @param margin A numeric vector of length 4, indicating the top, right, bottom
#'        and left margin for each plot, in centimetres.
#' @param tags Add tags to your subfigures. Can be \code{TRUE} (letter tags)
#'   or character vector containing tags labels.
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
#' if (require("dplyr") && require("gridExtra")) {
#'   library(ggeffects)
#'   data(efc)
#'
#'   # fit model
#'   fit <- glm(
#'     tot_sc_e ~ c12hour + e17age + e42dep + neg_c_7,
#'     data = efc,
#'     family = poisson
#'   )
#'
#'   # plot marginal effects for each predictor, each as single plot
#'   p1 <- ggpredict(fit, "c12hour") %>%
#'     plot(show.y.title = FALSE, show.title = FALSE)
#'   p2 <- ggpredict(fit, "e17age") %>%
#'     plot(show.y.title = FALSE, show.title = FALSE)
#'   p3 <- ggpredict(fit, "e42dep") %>%
#'     plot(show.y.title = FALSE, show.title = FALSE)
#'   p4 <- ggpredict(fit, "neg_c_7") %>%
#'     plot(show.y.title = FALSE, show.title = FALSE)
#'
#'   # plot grid
#'   plot_grid(list(p1, p2, p3, p4))
#'
#'   # plot grid
#'   plot_grid(list(p1, p2, p3, p4), tags = TRUE)
#' }
#' @export
plot_grid <- function(x, margin = c(1, 1, 1, 1), tags = NULL) {
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

  tags_labels <- NULL

  # Add tags
  if (isTRUE(tags)) {
    tags_labels = LETTERS
  } else{
    if (length(tags) < length(x)) {
      warning("Not enough tags labels in list. Using letters instead.")
      tags_labels = LETTERS
    } else{
      tags_labels = tags
    }
  }

  if (!is.null(tags_labels)) {
    for (i in 1:length(x)) {
      x[[i]] <- x[[i]] + labs(tag = tags_labels[i])
    }
  }


  # compute amount of columns and rows
  ncol <- round(sqrt(length(x)))
  nrow <- ceiling(length(x) / ncol)

  f <- eval(bquote(gridExtra::"grid.arrange"))
  do.call(f, c(x, nrow = nrow, ncol = ncol))
}
