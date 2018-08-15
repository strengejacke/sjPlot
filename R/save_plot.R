#' @title Save ggplot-figure for print publication
#' @name save_plot
#'
#' @description Convenient function to save the last ggplot-figure in
#'                high quality for publication.
#'
#' @param filename Name of the output file; filename must end with one
#'    of the following accepted file types: ".png", ".jpg", ".svg" or ".tif".
#' @param fig The plot that should be saved. By default, the last plot is saved.
#' @param width Width of the figure, in centimetres.
#' @param height Height of the figure, in centimetres.
#' @param dpi Resolution in dpi (dots per inch). Ignored for vector formats, such as ".svg".
#' @param theme The default theme to use when saving the plot.
#' @param label.color Color value for labels (axis, plot, etc.).
#' @param label.size Fontsize of value labels inside plot area.
#' @param axis.textsize Fontsize of axis labels.
#' @param axis.titlesize Fontsize of axis titles.
#' @param legend.textsize Fontsize of legend labels.
#' @param legend.titlesize Fontsize of legend title.
#' @param legend.itemsize Size of legend's item (legend key), in centimetres.
#'
#' @note This is a convenient function with some default settings that should
#'         come close to most of the needs for fontsize and scaling in figures
#'         when saving them for printing or publishing. It uses cairographics
#'         anti-aliasing (see \code{\link[grDevices]{png}}).
#'         \cr \cr
#'         For adjusting plot appearance, see also \code{\link{sjPlot-themes}}.
#'
#' @import ggplot2
#' @importFrom grDevices png jpeg tiff dev.off cm svg
#' @export
save_plot <- function(filename,
                      fig = last_plot(),
                      width = 12,
                      height = 9,
                      dpi = 300,
                      theme = theme_get(),
                      label.color = "black",
                      label.size = 2.4,
                      axis.textsize = .8,
                      axis.titlesize = .75,
                      legend.textsize = .6,
                      legend.titlesize = .65,
                      legend.itemsize = .5) {
  # get file extension
  ext <- tolower(substring(filename,
                           regexpr("\\.[^\\.]*$", filename) + 1,
                           nchar(filename)))

  # valid file ytpe?
  if (!ext %in% c("png", "jpg", "tif", "svg"))
    stop("filetype must be one of `.png`, `.jpg`, '.svg' or `.tif`.", call. = F)

  # set printable theme, adjust font sizes.
  # this is the most critical point...

  set_theme(
    base = theme,
    geom.label.color = label.color,
    axis.title.color = label.color,
    axis.textcolor = label.color,
    legend.title.color = label.color,
    legend.color = label.color,
    geom.label.size = label.size,
    axis.textsize = axis.textsize,
    axis.title.size = axis.titlesize,
    legend.size = legend.textsize,
    legend.title.size = legend.titlesize,
    legend.item.size = legend.itemsize
  )


  # prapare save

  if (ext == "png")
    grDevices::png(
      filename = filename,
      width = width,
      height = height,
      units = "cm",
      res = dpi,
      type = "cairo"
    )
  else if (ext == "jpg")
    grDevices::jpeg(
      filename = filename,
      width = width,
      height = height,
      units = "cm",
      res = dpi,
      type = "cairo"
    )
  else if (ext == "tif")
    grDevices::tiff(
      filename = filename,
      width = width,
      height = height,
      units = "cm",
      res = dpi,
      type = "cairo"
    )
  else if (ext == 'svg')
    grDevices::svg(
      filename = filename,
      width = width / grDevices::cm(1),
      height = height / grDevices::cm(1)
    )

  # print plot to device
  graphics::plot(fig)

  # close device
  grDevices::dev.off()
}
