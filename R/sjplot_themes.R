#' @title Set default themes for plots
#' @name sjPlot-themes
#'
#' @description Set default theme plots.

#' @rdname sjPlot-themes
#' @export
theme_sjplot <- function(base_size = 12, base_family = "") {
  (ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
     ggplot2::theme(
       axis.line.x      = ggplot2::element_line(colour = "grey80"),
       axis.line.y      = ggplot2::element_line(colour = "grey80"),
       axis.text        = ggplot2::element_text(colour = "grey50"),
       axis.title       = ggplot2::element_text(colour = "grey30"),
       strip.background = ggplot2::element_rect(colour = "grey70", fill = "grey90"),
       strip.text       = ggplot2::element_text(colour = "grey30"),
       legend.title     = ggplot2::element_text(colour = "grey30"),
       legend.text      = ggplot2::element_text(colour = "grey30")
     ))
}
