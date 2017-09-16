#' @title Set default themes for plots
#' @name sjPlot-themes
#'
#' @description Set default theme plots.
#'
#' @param base_size
#' @param base_family
#' @param title
#' @param axis_title.x
#' @param axis_title.y
#' @param labels.x
#' @param labels.y


#' @rdname sjPlot-themes
#' @export
theme_sjplot <- function(base_size = 12, base_family = "") {
  (theme_minimal(base_size = base_size, base_family = base_family) +
     theme(
       axis.line.x      = element_line(colour = "grey80"),
       axis.line.y      = element_line(colour = "grey80"),
       axis.text        = element_text(colour = "grey50"),
       axis.title       = element_text(colour = "grey30"),
       strip.background = element_rect(colour = "grey70", fill = "grey90"),
       strip.text       = element_text(colour = "grey30"),
       legend.title     = element_text(colour = "grey30"),
       legend.text      = element_text(colour = "grey30")
     ))
}


#' @rdname sjPlot-themes
#' @export
theme_blank <- function(base_size = 12, base_family = "") {
  (theme_minimal(base_size = base_size, base_family = base_family) +
     theme(
       axis.line.x      = element_line(colour = "white"),
       axis.line.y      = element_line(colour = "white"),
       axis.text        = element_text(colour = "grey50"),
       axis.title       = element_text(colour = "grey30"),
       panel.grid.minor = element_line(colour = "white", linetype = 1),
       panel.grid.major = element_line(colour = "white", linetype = 1)
     ))
}


#' @rdname sjPlot-themes
#' @export
theme_538 <- function(base_size = 12, base_family = "") {
  (theme_minimal(base_size = base_size, base_family = base_family) +
     theme(
       axis.line.x        = element_line(colour = "#F0F0F0"),
       axis.line.y        = element_line(colour = "#F0F0F0"),
       axis.text          = element_text(colour = "#737373"),
       axis.title         = element_text(colour = "#525252"),
       plot.background    = element_rect(colour = "#F0F0F0", fill = "#F0F0F0"),
       panel.grid.minor.x = element_line(colour = "#F0F0F0", linetype = 1),
       panel.grid.major   = element_line(colour = "#BDBDBD", linetype = 1),
       panel.grid.major.y = element_line(colour = "#F0F0F0", linetype = 1),
       panel.grid.minor.y = element_line(colour = "#F0F0F0", linetype = 1)
     ))
}


#' @rdname sjPlot-themes
#' @export
font_size <- function(title, axis_title.x, axis_title.y, labels.x, labels.y) {
  # get current theme
  cur.theme <- theme_get()

  if (!missing(title)) {
    cur.theme <- cur.theme +
      theme(title = element_text(size = title))
  }

  if (!missing(axis_title.x)) {
    cur.theme <- cur.theme +
      theme(axis.title.x = element_text(size = axis_title.x))
  }

  if (!missing(axis_title.y)) {
    cur.theme <- cur.theme +
      theme(axis.title.y = element_text(size = axis_title.y))
  }

  if (!missing(labels.x)) {
    cur.theme <- cur.theme +
      theme(axis.text.x =  element_text(size = labels.x))
  }

  if (!missing(labels.y)) {
    cur.theme <- cur.theme +
      theme(axis.text.y =  element_text(size = labels.y))
  }

  cur.theme
}
