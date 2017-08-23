#' @importFrom ggplot2 theme_get theme theme_set element_text
font_size <- function(title, axis_title.x, axis_title.y, labels.x, labels.y) {
  # get current theme
  cur.theme <- ggplot2::theme_get()

  if (!missing(title)) {
    cur.theme <- cur.theme +
      ggplot2::theme(title = ggplot2::element_text(size = title))
  }

  if (!missing(axis_title.x)) {
    cur.theme <- cur.theme +
      ggplot2::theme(axis.title.x = ggplot2::element_text(size = axis_title.x))
  }

  if (!missing(axis_title.y)) {
    cur.theme <- cur.theme +
      ggplot2::theme(axis.title.y = ggplot2::element_text(size = axis_title.y))
  }

  if (!missing(labels.x)) {
    cur.theme <- cur.theme +
      ggplot2::theme(axis.text.x =  ggplot2::element_text(size = labels.x))
  }

  if (!missing(labels.y)) {
    cur.theme <- cur.theme +
      ggplot2::theme(axis.text.y =  ggplot2::element_text(size = labels.y))
  }

  ggplot2::theme_set(cur.theme)
}
