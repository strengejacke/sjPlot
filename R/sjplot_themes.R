#' @title Modify plot appearance
#' @name sjPlot-themes
#'
#' @description Set default theme plots or modify plot appearance.
#'
#' @param base_size Base font size.
#' @param base_family Base font family.
#' @param title Font size for plot titles.
#' @param axis_title.x Font size for x-axis titles.
#' @param axis_title.y Font size for y-axis titles.
#' @param labels.x Font size for x-axis labels.
#' @param labels.y Font size for y-axis labels.
#' @param angle.x Angle for x-axis labels.
#' @param angle.y Angle for y-axis labels.
#' @param offset.x Offset for x-axis titles.
#' @param offset.y Offset for y-axis titles.
#' @param pos Position of the legend, if a legend is drawn.
#'        \describe{
#'          \item{\emph{Legend outside plot}}{
#'            Use \code{"bottom"}, \code{"top"}, \code{"left"} or \code{"right"}
#'            to position the legend above, below, on the left or right side
#'            of the diagram.
#'          }
#'          \item{\emph{Legend inside plot}}{
#'            If \code{inside = TRUE}, legend can be placed inside
#'            plot. Use \code{"top left"}, \code{"top right"}, \code{"bottom left"}
#'            and \code{"bottom right"} to position legend in any of these corners,
#'            or a two-element numeric vector with values from 0-1. See also
#'            \code{inside}.
#'          }
#'        }
#' @param justify Justification of legend, relative to its position (\code{"center"} or
#'          two-element numeric vector with values from 0-1.
#' @param inside Logical, use \code{TRUE} to put legend inside the plotting area.
#'        See also \code{pos}.
#' @param base.theme Optional ggplot-theme-object, which is needed in case multiple
#'        functions should be combined, e.g. \code{theme_sjplot() + label_angle()}.
#'        In such cases, use \code{label_angle(base.theme = theme_sjplot())}.
#'
#' @examples
#' # prepare data
#' library(sjmisc)
#' data(efc)
#' efc <- to_factor(efc, c161sex, e42dep, c172code)
#' m <- lm(neg_c_7 ~ pos_v_4 + c12hour + e42dep + c172code, data = efc)
#'
#' # create plot-object
#' p <- plot_model(m)
#'
#' # change theme
#' p + theme_sjplot()
#'
#' # change font-size
#' p + font_size(axis_title.x = 30)
#'
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
theme_sjplot2 <- function(base_size = 12, base_family = "") {
  (theme_minimal(base_size = base_size, base_family = base_family) +
     theme(
       axis.line.x      = element_line(colour = "grey50"),
       axis.line.y      = element_line(colour = "grey50"),
       axis.text        = element_text(colour = "grey10"),
       axis.title       = element_text(colour = "black"),
       strip.background = element_rect(colour = "grey50", fill = "grey70"),
       strip.text       = element_text(colour = "grey20"),
       legend.title     = element_text(colour = "grey10"),
       legend.text      = element_text(colour = "grey20")
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
font_size <- function(title, axis_title.x, axis_title.y, labels.x, labels.y, offset.x, offset.y, base.theme) {
  # get current theme
  if (!missing(base.theme))
    cur.theme <- base.theme
  else
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

  if (!missing(offset.x)) {
    cur.theme <- cur.theme +
      theme(axis.title.x = element_text(vjust = offset.x))
  }

  if (!missing(offset.y)) {
    cur.theme <- cur.theme +
      theme(axis.title.y = element_text(vjust = offset.y))
  }

  cur.theme
}


#' @rdname sjPlot-themes
#' @export
label_angle <- function(angle.x, angle.y, base.theme) {
  # get current theme
  if (!missing(base.theme))
    cur.theme <- base.theme
  else
    cur.theme <- theme_get()

  if (!missing(angle.x)) {
    cur.theme <- cur.theme +
      theme(axis.text.x = element_text(angle = angle.x))
  }

  if (!missing(angle.y)) {
    cur.theme <- cur.theme +
      theme(axis.text.y = element_text(angle = angle.y))
  }

  cur.theme
}


#' @rdname sjPlot-themes
#' @export
legend_style <- function(inside, pos, justify, base.theme) {
  # get current theme
  if (!missing(base.theme))
    cur.theme <- base.theme
  else
    cur.theme <- theme_get()

  # convert legend position from character to numeric index
  if (!missing(inside) && inside) {
    if (!missing(pos) && is.character(pos)) {
      pos <- dplyr::case_when(
        pos == "top right" ~ c(1, 1),
        pos == "bottom right" ~ c(1, 0),
        pos == "bottom left" ~ c(0, 0),
        pos == "top left" ~ c(0, 1),
        TRUE ~ c(1, 1)
      )

      if (missing(justify)) justify <- pos
    }
  }

  # set default justification
  if (missing(justify)) justify <- "center"

  if (!missing(pos)) {
    cur.theme <- cur.theme +
      theme(
        legend.position = pos,
        legend.justification = justify
      )
  }

  cur.theme
}
