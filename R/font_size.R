#' @importFrom ggplot2 theme_get theme theme_set element_text
#' @export
font_size <- function(fs_title, fs_axis_title.x, fs_axis_title.y, fs_labels.x, fs_labels.y) {
  # get current theme
  cur.theme <- ggplot2::theme_get()

  if (!missing(fs_title)) {
    cur.theme <- cur.theme +
      ggplot2::theme(title = ggplot2::element_text(size = fs_title))
  }

  if (!missing(fs_axis_title.x)) {
    cur.theme <- cur.theme +
      ggplot2::theme(axis.title.x = ggplot2::element_text(size = fs_axis_title.x))
  }

  if (!missing(fs_axis_title.y)) {
    cur.theme <- cur.theme +
      ggplot2::theme(axis.title.y = ggplot2::element_text(size = fs_axis_title.y))
  }

  if (!missing(fs_labels.x)) {
    cur.theme <- cur.theme +
      ggplot2::theme(axis.text.x =  ggplot2::element_text(size = fs_labels.x))
  }

  if (!missing(fs_labels.y)) {
    cur.theme <- cur.theme +
      ggplot2::theme(axis.text.y =  ggplot2::element_text(size = fs_labels.y))
  }

  ggplot2::theme_set(cur.theme)
}
