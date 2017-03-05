#' @title Set default theme for sjp-functions
#' @name set_theme
#'
#' @description Set default theme for sjp-functions.
#'
#' @param theme Name of a pre-set theme. May be one of:
#'        \describe{
#'          \item{\code{"blank"}}{a theme with no grids and axes.}
#'          \item{\code{"forest"}}{a theme for forest plots, with no grids, in "539" style.}
#'          \item{\code{"538"}}{a grey-scaled theme inspired by \href{http://fivethirtyeight.com}{538-charts}, adapted from \href{http://minimaxir.com/2015/02/ggplot-tutorial/}{minimaxir.com}.}
#'          \item{\code{"539"}}{a slight modification of the 538-theme.}
#'          \item{\code{"scatter"}}{a theme for scatter plots in 539-theme-style.}
#'          \item{\code{"538w"}, \code{"539w"}, \code{"scatterw"} and \code{"forestw"}}{for themes as described above, however all with white backgrounds.}
#'        }
#' @param ... Other arguments passed down to \code{\link{sjp.setTheme}}.
#'
#' @return The customized theme object.
#'
#' @seealso \href{http://www.strengejacke.de/sjPlot/custplot/}{sjPlot manual: customize plot appearance}
#'
#' @references \itemize{
#'              \item \href{http://zevross.com/blog/2014/08/04/beautiful-plotting-in-r-a-ggplot2-cheatsheet-3/}{Beautiful plotting in R: A ggplot2 cheatsheet}
#'              \item \href{http://minimaxir.com/2015/02/ggplot-tutorial/}{An Introduction on How to Make Beautiful Charts With R and ggplot2}
#'             }
#'
#' @examples
#' library(sjmisc)
#' data(efc)
#'
#' # of the ggplot base theme
#' set_theme("539")
#' sjp.xtab(efc$e42dep, efc$e16sex)
#'
#' @import ggplot2
#' @importFrom scales brewer_pal grey_pal
#' @importFrom dplyr case_when
#' @export
set_theme <- function(theme = c("forest", "538", "539", "scatter", "forestw",
                                "538w", "539w", "scatterw", "blank"),
                      ...) {

  theme <- match.arg(theme)

  if (theme == "blank") {
    return(sjp.setTheme(
      base = theme_classic(),
      axis.linecolor = "white",
      axis.tickscol = "white",
      panel.gridcol = "white",
      plot.col = "white",
      ...
    ))
  }

  g.palette <- scales::brewer_pal(palette = "Greys")(9)

  col.ind <- dplyr::case_when(
    theme == "538" ~ 2,
    theme == "539" ~ 2,
    theme == "forest" ~ 2,
    theme == "scatter" ~ 2,
    theme == "539w" ~ 1,
    theme == "538w" ~ 1,
    theme == "forestw" ~ 1,
    theme == "scatterw" ~ 1,
    TRUE ~ 1
  )

  col.ind2 <- dplyr::case_when(
    theme %in% c("forest", "forestw") ~ col.ind,
    TRUE ~ 4
  )

  axis.linecolor <- dplyr::case_when(
    theme == "538" ~ g.palette[col.ind],
    theme == "538w" ~ g.palette[col.ind],
    TRUE ~ g.palette[5]
  )

  axis.linecolor.x <- dplyr::case_when(
    theme == "538" ~ g.palette[col.ind],
    theme == "538w" ~ g.palette[col.ind],
    TRUE ~ g.palette[5]
  )

  axis.linecolor.y <- dplyr::case_when(
    theme == "scatter" ~ g.palette[5],
    theme == "scatterw" ~ g.palette[5],
    TRUE ~ g.palette[col.ind]
  )

  panel.gridcol.x <- dplyr::case_when(
    theme %in% c("539", "539w", "538", "538w", "forest", "forestw") ~ g.palette[col.ind],
    TRUE ~ g.palette[4]
  )

  if (theme %in% c("scatter", "scatterw")) {
    panel.major.linetype <- panel.minor.linetype <- 2
  } else {
    panel.major.linetype <- panel.minor.linetype <- NULL
  }

  return(sjp.setTheme(
    base = ggplot2::theme_bw(),
    # plot
    plot.backcol = g.palette[col.ind],
    plot.bordercol = g.palette[col.ind],
    plot.col = g.palette[col.ind],
    plot.margins = ggplot2::unit(c(1, .5, 1, 0.5), "cm"),
    # panel
    panel.bordercol = g.palette[col.ind],
    panel.backcol = g.palette[col.ind],
    panel.col = g.palette[col.ind],
    panel.gridcol.x = panel.gridcol.x,
    panel.gridcol.y = NULL,
    panel.major.gridcol = g.palette[col.ind2],
    panel.minor.gridcol = g.palette[col.ind],
    panel.major.linetype = panel.major.linetype,
    panel.minor.linetype = panel.minor.linetype,
    # title
    title.color = g.palette[9],
    title.align = "center",
    title.vjust = 1.75,
    # axes
    axis.tickslen = 0,
    axis.textcolor = g.palette[6],
    axis.title.color = g.palette[7],
    axis.linecolor = axis.linecolor,
    axis.linecolor.x = axis.linecolor.x,
    axis.linecolor.y = axis.linecolor.y,
    axis.title.x.vjust = -1,
    axis.title.y.vjust = 1.5,
    # legend
    legend.title.color = g.palette[7],
    legend.color = g.palette[6],
    legend.item.backcol = g.palette[col.ind],
    legend.item.bordercol = g.palette[col.ind],
    legend.backgroundcol = g.palette[col.ind],
    legend.bordercol = g.palette[col.ind],
    # geom
    geom.label.color = g.palette[6],
    ...
  ))
}
