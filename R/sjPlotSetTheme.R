#' @title Set global theme options for sjp-functions
#' @name sjp.setTheme
#' 
#' @description Set global theme options for sjp-functions.
#' 
#' @param title.size Size of the plot title. Default is 1.3.
#' @param title.color Color of the plot title. Default is \code{"black"}.
#' @param title.align Alignment of plot title. Must be one of \code{"left"} (default),
#'          \code{"center"} or \code{"right"}. You may use initial letter only.
#' @param geom.outline.size Size of the bar outlines. Default is 0.1. Use
#'          size of \code{0} to remove geom outline.
#' @param geom.outline.color Color of geom outline. Only applies, if \code{geom.outline.size}
#'          is larger than 0.
#' @param geom.boxoutline.size Size of outlines and median bar especially for boxplots.
#'          Default is 0.5. Use size of \code{0} to remove boxplot outline.
#' @param geom.boxoutline.color Color of outlines and median bar especially for boxplots.
#'          Only applies, if \code{geom.boxoutline.size} is larger than 0.
#' @param geom.alpha Specify the transparancy (alpha value) of geoms
#' @param geom.linetype Linetype of line geoms. Default is \code{1} (solid line).
#' @param geom.errorbar.size Size (thickness) of error bars. Default is \code{0.8}
#' @param geom.errorbar.linetype Linetype of error bars. Default is \code{1} (solid line).
#' @param geom.label.color Color of geom's value and annotation labels.
#' @param geom.label.size Size of geom's value and annotation labels.
#' @param geom.label.alpha Alpha level of geom's value and annotation labels.
#' @param geom.label.angle Angle of geom's value and annotation labels.
#' @param axis.title.color Color of x- and y-axis title labels.
#' @param axis.title.size Size of x- and y-axis title labels.
#' @param axis.angle.x Angle for x-axis labels.
#' @param axis.angle.y Angle for y-axis labels.
#' @param axis.angle Angle for x- and y-axis labels. If set, overrides both \code{axis.angle.x} and \code{axis.angle.y}
#' @param axis.textcolor.x Color for x-axis labels. If not specified, a default dark gray
#'          color palette will be used for the labels.
#' @param axis.textcolor.y Color for y-axis labels. If not specified, a default dark gray
#'          color palette will be used for the labels.
#' @param axis.textcolor Color for both x- and y-axis labels. 
#'          If set, overrides both \code{axis.textcolor.x} and \code{axis.textcolor.y}
#' @param axis.linecolor.x color of x-axis border.
#' @param axis.linecolor.y color of y-axis border.
#' @param axis.linecolor Color for both x- and y-axis borders. 
#'          If set, overrides both \code{axis.linecolor.x} and \code{axis.linecolor.y}.
#' @param axis.line.size Size (thickness) of axis lines. Only affected, if \code{axis.linecolor}
#'          is set.
#' @param axis.textsize.x Size of x-axis labels.
#' @param axis.textsize.y Size of y-axis labels.
#' @param axis.textsize Size for both x- and y-axis labels. 
#'          If set, overrides both \code{axis.textsize.x} and \code{axis.textsize.y}.
#' @param axis.tickslen Length of axis tick marks
#' @param axis.ticksol Color of axis tick marks.
#' @param axis.ticksmar Margin between axis labels and tick marks.
#' @param panel.bordercol Color of whole diagram border (panel border).
#' @param panel.backcol Color of the diagram's background.
#' @param panel.col Color of both diagram's border and background.
#'          If set, overrides both \code{panel.bordercol} and \code{panel.backcol}.
#' @param panel.major.gridcol Color of the major grid lines of the diagram background.
#' @param panel.minor.gridcol Color of the minor grid lines of the diagram background.
#' @param panel.gridcol Color for both minor and major grid lines of the diagram background.
#'          If set, overrides both \code{panel.major.gridcol} and \code{panel.minor.gridcol}.
#' @param panel.major.linetype Line type for major grid lines.
#' @param panel.minor.linetype Line type for minor grid lines.
#' @param plot.backcol Color of the plot's background.
#' @param plot.bordercol Color of whole plot's border (panel border).
#' @param plot.col Color of both plot's region border and background.
#'          If set, overrides both \code{plot.backcol} and \code{plot.bordercol}.
#' @param legend.pos Position of the legend, if a legend is drawn.\cr
#'          \emph{legend outside plot} \cr
#'          Use \code{"bottom"}, \code{"top"}, \code{"left"}
#'          or \code{"right"} to position the legend above, below, on the left or right side of the diagram. 
#'          Right positioning is default. \cr
#'          \emph{legend inside plot} \cr
#'          If \code{legend.inside} is \code{TRUE}, legend can be placed inside
#'          plot. Use \code{"top left"}, \code{"top right"}, \code{"bottom left"} and \code{"bottom right"}
#'          to position legend in any of these corners, or a two-element numeric vector with values from 0-1. \cr
#'          See also \code{legend.inside}.
#' @param legend.just Justification of legend, relative to its position ("center" or 
#'          two-element numeric vector with values from 0-1. By default (outside legend),
#'          justification is centered. If legend is inside and justification not specified,
#'          legend justification is set according to legend position.
#' @param legend.inside Logical, use \code{TRUE} to put legend inside the plotting area. See \code{legend.pos}.
#' @param legend.size Text size of the legend. Default is 1. Relative size, so recommended values are from 0.3 to
#'          2.5
#' @param legend.color Color of the legend labels.
#' @param legend.title.size Text size of the legend title.
#' @param legend.title.color Color of the legend title.
#' @param legend.title.face Font face of the legend title. By default, \code{"bold"} face is used.
#' @param legend.bordercol Color of the legend's border. Default is \code{"white"}, so no visible border is drawn.
#' @param legend.backgroundcol Fill color of the legend's background. Default is \code{"white"}, so no visible background is drawn.
#' @param legend.item.bordercol Color of the legend's item-border. Default is \code{"white"}.
#' @param legend.item.backcol Fill color of the legend's item-background. Default is \code{"grey90"}.
#' @param theme valid parameter for ggplot default-themes are:
#'        \itemize{
#'          \item \code{theme_bw}
#'          \item \code{theme_classic}
#'          \item \code{theme_grey}
#'          \item \code{theme_light}
#'          \item \code{theme_linedraw}
#'          \item \code{theme_minimal}
#'        }
#'        Furthermore, there are some theme-presets, which can be used:
#'        \itemize{
#'          \item \code{"blank"}: a theme with no grids and axes.
#'          \item \code{"forest"}: a theme for forest plots, with no grids.
#'          \item \code{"forestgrey"}: a theme for forest plots, with no grids, in "539" style.
#'          \item \code{"538"}: a grey-scaled theme inspired by \href{http://fivethirtyeight.com}{538-charts}, adapted from \href{http://minimaxir.com/2015/02/ggplot-tutorial/}{minimaxir.com}.
#'          \item \code{"539"}: a slight modification of the 538-theme.
#'          \item \code{"scatter"}: a theme for scatter plots in 539-theme-style.
#'          \item \code{"blues"}: a blue-colored scheme based on the Blues color-brewer-palette.
#'          \item \code{"greens"}: a green-colored scheme.
#'        }
#' @param base Base theme where theme is built on. By default, all 
#'          metrics from \code{theme_gray()} are used.
#' 
#' @return The customized theme object, or \code{NULL}, if a ggplot-theme
#'           was used.
#' 
#' @seealso \href{http://www.strengejacke.de/sjPlot/custplot/}{sjPlot manual: customize plot appearance}
#' 
#' @references \itemize{
#'              \item \href{http://zevross.com/blog/2014/08/04/beautiful-plotting-in-r-a-ggplot2-cheatsheet-3/}{Beautiful plotting in R: A ggplot2 cheatsheet}
#'              \item \href{http://minimaxir.com/2015/02/ggplot-tutorial/}{An Introduction on How to Make Beautiful Charts With R and ggplot2}
#'             }
#' 
#' @examples
#' \dontrun{
#' library(sjmisc)
#' data(efc)
#' # set sjPlot-defaults, a slightly modification
#' # of the ggplot base theme
#' sjp.setTheme()
#' 
#' # legends of all plots inside
#' sjp.setTheme(legend.pos = "top left", 
#'              legend.inside = TRUE)
#' sjp.xtab(efc$e42dep, 
#'          efc$e16sex,
#'          showTableSummary = FALSE)
#' 
#' # Use classic-theme as base. you may need to
#' # load the ggplot2-library.
#' library(ggplot2)
#' sjp.setTheme(base = theme_classic())
#' sjp.frq(efc$e42dep)
#' 
#' # adjust value labels
#' sjp.setTheme(geom.label.size = 3.5,
#'              geom.label.color = "#3366cc",
#'              geom.label.angle = 90)
#' # hjust-aes needs adjustment for this
#' update_geom_defaults('text', list(hjust = -0.1))
#' sjp.xtab(efc$e42dep, 
#'          efc$e16sex,
#'          showTableSummary = FALSE,
#'          labelPos = "center")
#' 
#' # Create own theme based on classic-theme
#' sjp.setTheme(base = theme_classic(),
#'              axis.linecolor = "grey50",
#'              axis.textcolor = "#6699cc")
#' sjp.frq(efc$e42dep)
#'
#' # use theme pre-set, taken from tutorial
#' # http://minimaxir.com/2015/02/ggplot-tutorial/
#' sjp.setTheme(theme = "538",
#'              geom.alpha = 0.8)
#' library(ggplot2) # for custom base-line
#' sjp.frq(efc$e42dep, 
#'         geom.color = "#c0392b",
#'         expand.grid = TRUE,
#'         printPlot = FALSE)$plot + 
#'   geom_hline(yintercept = 0, 
#'              size = 0.5, 
#'              colour = "black")}
#' 
#' @import ggplot2
#' @importFrom grid unit
#' @importFrom scales brewer_pal
#' @export
sjp.setTheme <- function(title.color="black",
                         title.size=1.3,
                         title.align = "left",
                         # geom defaults
                         # geom.colors=NULL,
                         geom.outline.color=NULL,
                         geom.outline.size=0.1,
                         geom.boxoutline.size=0.5,
                         geom.boxoutline.color="black",
                         geom.alpha=1,
                         geom.linetype=1,
                         geom.errorbar.size=0.7,
                         geom.errorbar.linetype=1,
                         # value labels
                         geom.label.color=NULL,
                         geom.label.size=4.5,
                         geom.label.alpha=1,
                         geom.label.angle=0,
                         # axis titles
                         axis.title.color="grey30",
                         axis.title.size=1.2,
                         # axis text angle
                         axis.angle.x=0,
                         axis.angle.y=0,
                         axis.angle=NULL,
                         # axis text colors
                         axis.textcolor.x="grey30",
                         axis.textcolor.y="grey30",
                         axis.textcolor=NULL,
                         # axis line colors
                         axis.linecolor.x=NULL,
                         axis.linecolor.y=NULL,
                         axis.linecolor=NULL,
                         axis.line.size=0.5,
                         # axis text size
                         axis.textsize.x=1.1,
                         axis.textsize.y=1.1,
                         axis.textsize=NULL,
                         # axis ticks
                         axis.tickslen=NULL,
                         axis.ticksol=NULL,
                         axis.ticksmar=NULL,
                         # panel defaults
                         panel.backcol=NULL,
                         panel.bordercol=NULL,
                         panel.col=NULL,
                         panel.major.gridcol=NULL,
                         panel.minor.gridcol=NULL,
                         panel.gridcol=NULL,
                         panel.major.linetype = 1,
                         panel.minor.linetype = 1,
                         # plot background color
                         plot.backcol=NULL,
                         plot.bordercol=NULL,
                         plot.col=NULL,
                         # legend
                         legend.pos="right",
                         legend.just=NULL,
                         legend.inside=FALSE,
                         legend.size=1,
                         legend.color="black",
                         legend.title.size=1,
                         legend.title.color="black",
                         legend.title.face="bold",
                         legend.backgroundcol="white",
                         legend.bordercol="white",
                         legend.item.backcol="grey90",
                         legend.item.bordercol="white",
                         # base theme
                         theme=NULL,
                         base=theme_grey()) {
  sjtheme <- NULL
  title.vjust <- NULL
  axis.title.x.vjust <- NULL
  axis.title.y.vjust <- NULL
  plot.margins <- NULL
  panel.gridcol.x <- NULL
  panel.gridcol.y <- NULL
  # ----------------------------------------  
  # check for blank theme, i.e. if user requires special
  # theme without any grids or axis lines
  # ----------------------------------------  
  if (!is.null(theme) && theme=="blank") {
    base <- theme_classic()
    axis.linecolor <- "white"
    axis.ticksol <- "white"
    panel.gridcol <- "white"
    plot.col <- "white"
  }
  if (!is.null(theme) && theme=="forest") {
    base <- theme_bw()
    panel.gridcol <- "white"
    axis.tickslen <- 0
  }  
  if (!is.null(theme) && theme=="538") {
    base <- theme_bw()
    g.palette <- scales::brewer_pal(palette = "Greys")(9)
    panel.bordercol <- panel.backcol <- panel.col <- g.palette[2]
    plot.backcol <- plot.bordercol <- plot.col <- g.palette[2]
    panel.major.gridcol <- g.palette[4]
    panel.minor.gridcol <- g.palette[2]
    axis.linecolor.x  <- axis.linecolor.y <- axis.linecolor <- g.palette[2]
    legend.item.backcol <- legend.item.bordercol <- legend.backgroundcol <- legend.bordercol <- g.palette[2]
    title.color <- g.palette[9]
    axis.textcolor <- g.palette[6]
    axis.title.color <- g.palette[7]
    if (is.null(geom.label.color)) geom.label.color <- g.palette[6]
    legend.title.color <- g.palette[7]
    legend.color <- g.palette[6]
    axis.tickslen <- 0
    # custom modifications
    title.align <- "center"
    axis.title.x.vjust <- -1
    axis.title.y.vjust <- 1.5
    title.vjust <- 1.75
    plot.margins <- unit(c(1, .5, 1, 0.5), "cm")
    message("Theme '538' looks better with panel margins. You may want to use parameter 'expand.grid = TRUE' in sjp-functions.")
  }  
  if (!is.null(theme) && (theme == "539" || theme == "forestgrey")) {
    base <- theme_bw()
    g.palette <- scales::brewer_pal(palette = "Greys")(9)
    panel.bordercol <- panel.backcol <- panel.col <- g.palette[2]
    plot.backcol <- plot.bordercol <- plot.col <- g.palette[2]
    if (theme == "539") {
      panel.major.gridcol <- g.palette[4]
      panel.minor.gridcol <- g.palette[2]
      panel.gridcol.x <- g.palette[2]
    } else {
      panel.major.gridcol <- panel.minor.gridcol <- g.palette[2]
    }
    axis.linecolor <- NULL
    if (is.null(axis.linecolor.y)) axis.linecolor.y <- g.palette[2]
    if (is.null(axis.linecolor.x)) axis.linecolor.x <- g.palette[9]
    legend.item.backcol <- legend.item.bordercol <- legend.backgroundcol <- legend.bordercol <- g.palette[2]
    title.color <- g.palette[9]
    axis.textcolor <- g.palette[6]
    axis.title.color <- g.palette[7]
    if (is.null(geom.label.color)) geom.label.color <- g.palette[6]
    legend.title.color <- g.palette[7]
    legend.color <- g.palette[6]
    axis.tickslen <- 0
    # custom modifications
    title.align <- "center"
    axis.title.x.vjust <- -1
    axis.title.y.vjust <- 1.5
    title.vjust <- 1.75
    plot.margins <- unit(c(1, .5, 1, 0.5), "cm")
  }  
  if (!is.null(theme) && theme=="scatter") {
    base <- theme_bw()
    g.palette <- scales::brewer_pal(palette = "Greys")(9)
    panel.bordercol <- panel.backcol <- panel.col <- g.palette[2]
    plot.backcol <- plot.bordercol <- plot.col <- g.palette[2]
    panel.major.gridcol <- panel.minor.gridcol <- g.palette[4]
    axis.linecolor <- g.palette[5]
    if (is.null(axis.linecolor.y)) axis.linecolor.y <- g.palette[2]
    if (is.null(axis.linecolor.x)) axis.linecolor.x <- g.palette[2]
    legend.item.backcol <- legend.item.bordercol <- legend.backgroundcol <- legend.bordercol <- g.palette[2]
    title.color <- g.palette[9]
    axis.textcolor <- g.palette[6]
    axis.title.color <- g.palette[7]
    if (is.null(geom.label.color)) geom.label.color <- g.palette[6]
    legend.title.color <- g.palette[7]
    legend.color <- g.palette[6]
    axis.tickslen <- 0
    # custom modifications
    panel.major.linetype <- panel.minor.linetype <- 2
    title.align <- "center"
    axis.title.x.vjust <- -1
    axis.title.y.vjust <- 1.5
    title.vjust <- 1.75
    plot.margins <- unit(c(1, .5, 1, 0.5), "cm")
  }  
  if (!is.null(theme) && theme=="blues") {
    base <- theme_bw()
    g.palette <- scales::brewer_pal(palette = "Blues")(9)
    panel.bordercol <- panel.backcol <- panel.col <- g.palette[1]
    plot.backcol <- plot.bordercol <- plot.col <- g.palette[1]
    panel.major.gridcol <- g.palette[3]
    panel.minor.gridcol <- g.palette[1]
    axis.linecolor <- NULL
    axis.linecolor.y  <- g.palette[1]
    axis.linecolor.x <- g.palette[9]
    panel.gridcol.x <- g.palette[1]
    legend.item.backcol <- legend.item.bordercol <- legend.backgroundcol <- legend.bordercol <- g.palette[1]
    title.color <- "black"
    axis.textcolor <- g.palette[9]
    axis.title.color <- "black"
    if (is.null(geom.label.color)) geom.label.color <- g.palette[5]
    legend.title.color <- g.palette[8]
    legend.color <- g.palette[6]
    axis.tickslen <- 0
    # custom modifications
    title.align <- "center"
    axis.title.x.vjust <- -1
    axis.title.y.vjust <- 1.5
    title.vjust <- 1.75
    plot.margins <- unit(c(1, .5, 1, 0.5), "cm")
  }  
  if (!is.null(theme) && theme=="greens") {
    base <- theme_bw()
    g.palette <- scales::brewer_pal(palette = "BrBG")(9)
    g.palette[5] <- "#f5faf5"
    panel.bordercol <- panel.backcol <- panel.col <- g.palette[5]
    plot.backcol <- plot.bordercol <- plot.col <- g.palette[5]
    panel.major.gridcol <- g.palette[6]
    panel.minor.gridcol <- g.palette[5]
    axis.linecolor <- NULL
    axis.linecolor.y  <- g.palette[5]
    axis.linecolor.x <- g.palette[9]
    panel.gridcol.x <- g.palette[5]
    legend.item.backcol <- legend.item.bordercol <- legend.backgroundcol <- legend.bordercol <- g.palette[5]
    title.color <- "black"
    axis.textcolor <- g.palette[9]
    axis.title.color <- "black"
    if (is.null(geom.label.color)) geom.label.color <- g.palette[8]
    legend.title.color <- g.palette[9]
    legend.color <- g.palette[8]
    axis.tickslen <- 0
    # custom modifications
    title.align <- "center"
    axis.title.x.vjust <- -1
    axis.title.y.vjust <- 1.5
    title.vjust <- 1.75
    plot.margins <- unit(c(1, .5, 1, 0.5), "cm")
  }  
  # ----------------------------------------  
  # set defaults for geom label colors
  # ----------------------------------------  
  if (is.null(geom.label.color)) {
    geom.label.color <- "black"
  }
  # ----------------------------------------  
  # set defaults for axis text angle
  # ----------------------------------------  
  if (!is.null(axis.angle)) {
    axis.angle.x <- axis.angle.y <- axis.angle 
  }
  else {
    axis.angle <- axis.angle.x
  }
  # ----------------------------------------
  # set defaults for axis text color
  # ----------------------------------------  
  if (!is.null(axis.textcolor)) {
    axis.textcolor.x <- axis.textcolor.y <- axis.textcolor
  }
  else {
    if (is.null(axis.textcolor.x)) 
      axis.textcolor <- axis.textcolor.y 
    else 
      axis.textcolor <- axis.textcolor.x
  }
  # ----------------------------------------
  # set defaults for axis line color
  # ----------------------------------------  
  if (!is.null(axis.linecolor)) {
    axis.linecolor.x <- axis.linecolor.y <- axis.linecolor
  }
  else {
    if (is.null(axis.linecolor.x)) 
      axis.linecolor <- axis.linecolor.y 
    else 
      axis.linecolor <- axis.linecolor.x
  }
  # ----------------------------------------
  # set defaults for axis text size
  # ----------------------------------------
  if (!is.null(axis.textsize)) {
    axis.textsize.x <- axis.textsize.y <- axis.textsize
  }
  else {
    if (is.null(axis.textsize.x)) 
      axis.textsize <- axis.textsize.y 
    else 
      axis.textsize <- axis.textsize.x
  }
  # ----------------------------------------
  # set defaults for grid colors
  # ----------------------------------------
  if (!is.null(panel.gridcol)) {
    panel.major.gridcol <- panel.minor.gridcol <- panel.gridcol
  }
  else {
    if (is.null(panel.major.gridcol)) 
      panel.gridcol <- panel.minor.gridcol 
    else 
      panel.gridcol <- panel.major.gridcol
  }
  # ----------------------------------------
  # set defaults for panel colors
  # ----------------------------------------
  if (!is.null(panel.col)) {
    panel.backcol <- panel.bordercol <- panel.col
  }
  else {
    if (is.null(panel.backcol)) 
      panel.col <- panel.bordercol 
    else 
      panel.col <- panel.backcol
  }
  # ----------------------------------------
  # set title alignment
  # ----------------------------------------
  if (!is.null(title.align)) {
    if (title.align == "left" || title.align == "l") title.align <- 0
    if (title.align == "right" || title.align == "r") title.align <- 1
    if (title.align == "center" || title.align == "c") title.align <- 0.5
  }
  else {
    title.align <- 0
  }
  # ----------------------------------------
  # set defaults for plot colors
  # ----------------------------------------
  if (!is.null(plot.col)) {
    plot.backcol <- plot.bordercol <- plot.col
  }
  else {
    if (is.null(plot.backcol)) 
      plot.col <- plot.bordercol 
    else 
      plot.col <- plot.backcol
  }
  # ----------------------------------------
  # set defaults for legend pos
  # ----------------------------------------
  if (legend.inside) {
    # check if character constants have been used and if so,
    # convert to numeric vector
    if (is.character(legend.pos)) {
      if (legend.pos == "top right") legend.pos <- c(1,1)
      else if (legend.pos == "bottom right") legend.pos <- c(1,0)
      else if (legend.pos == "bottom left") legend.pos <- c(0,0)
      else if (legend.pos == "top left") legend.pos <- c(0,1)
      if (is.null(legend.just)) legend.just <- legend.pos
    }
  }
  # set justification default
  if (is.null(legend.just)) legend.just <- "center"
  # ----------------------------------------
  # check if theme-preset is requested
  # ----------------------------------------
  if (!is.null(theme) && any(class(theme)=="theme") && any(class(theme)=="gg")) {
    theme_set(theme)
  }
  # ----------------------------------------
  # else, customize theme
  # ----------------------------------------
  else if (!is.null(base) && any(class(base)=="theme") && any(class(base)=="gg")) {
    sjtheme <- base +
      # ----------------------------------------
      # set base elements that are always set
      # ----------------------------------------
      theme(plot.title = element_text(size = rel(title.size), 
                                      colour = title.color,
                                      hjust = title.align),
            axis.text = element_text(angle = axis.angle, 
                                     size = rel(axis.textsize), 
                                     colour = axis.textcolor),
            axis.text.x = element_text(angle = axis.angle.x, 
                                       size = rel(axis.textsize.x), 
                                       colour = axis.textcolor.x), 
            axis.text.y = element_text(angle = axis.angle.y, 
                                       size = rel(axis.textsize.y), 
                                       colour = axis.textcolor.y), 
            axis.title = element_text(size = rel(axis.title.size), 
                                      colour = axis.title.color),
            legend.position = legend.pos,
            legend.justification = legend.just,
            legend.text = element_text(size = rel(legend.size),
                                       colour = legend.color),
            legend.title = element_text(size = rel(legend.title.size),
                                        colour = legend.title.color,
                                        face = legend.title.face),
            legend.background = element_rect(colour = legend.bordercol, 
                                             fill = legend.backgroundcol))
    # ----------------------------------------
    # set legend items background-color
    # ----------------------------------------
    if (!is.null(legend.item.backcol)) {
      sjtheme <- sjtheme +
        theme(legend.key = element_rect(colour = legend.item.bordercol, 
                                        fill = legend.item.backcol))
    }
    # ----------------------------------------
    # set axis line colors, if defined
    # ----------------------------------------
    if (!is.null(axis.linecolor)) {
      sjtheme <- sjtheme +
        theme(axis.line = element_line(colour = axis.linecolor, 
                                       size = axis.line.size),
              axis.line.x = element_line(colour = axis.linecolor.x),
              axis.line.y = element_line(colour = axis.linecolor.y))
    }
    # ----------------------------------------
    # set axis ticks, if defined
    # ----------------------------------------
    if (!is.null(axis.ticksol)) {
      sjtheme <- sjtheme +
        theme(axis.ticks = element_line(colour = axis.ticksol))
    }
    if (!is.null(axis.tickslen)) {
      sjtheme <- sjtheme +
        theme(axis.ticks.length = unit(axis.tickslen, "cm"))
    }
    if (!is.null(axis.ticksmar)) {
      sjtheme <- sjtheme +
        theme(axis.ticks.margin = unit(axis.ticksmar, "cm"))
    }
    # ----------------------------------------
    # set plot colors, if defined
    # ----------------------------------------
    if (!is.null(plot.col)) {
      sjtheme <- sjtheme +
        theme(plot.background = element_rect(colour = plot.bordercol, 
                                             fill = plot.backcol))
    }
    # ----------------------------------------
    # set panel colors, if defined
    # ----------------------------------------
    if (!is.null(panel.col)) {
      sjtheme <- sjtheme +
        theme(panel.background = element_rect(colour = panel.bordercol, 
                                              fill = panel.backcol),
              panel.border = element_rect(colour = panel.bordercol))
    }
    # ----------------------------------------
    # set panel grids, if defined
    # ----------------------------------------
    if (!is.null(panel.gridcol)) {
      sjtheme <- sjtheme +
        theme(panel.grid.minor = element_line(colour = panel.minor.gridcol, linetype = panel.minor.linetype),
              panel.grid.major = element_line(colour = panel.major.gridcol, linetype = panel.major.linetype))
    }
    # ----------------------------------------
    # set plot margins. onyl applies to pre-set themes
    # ----------------------------------------
    if (!is.null(plot.margins)) {
      sjtheme <- sjtheme +
        theme(plot.margin = plot.margins)
    }
    # ----------------------------------------
    # set title adjustments. only applies to
    # pre-set themes
    # ----------------------------------------
    if (!is.null(plot.margins)) {
      sjtheme <- sjtheme +
        theme(plot.margin = plot.margins)
    }
    if (!is.null(title.vjust)) {
      sjtheme <- sjtheme +
        theme(plot.title = element_text(vjust = title.vjust))
    }
    if (!is.null(axis.title.x.vjust)) {
      sjtheme <- sjtheme +
        theme(axis.title.x = element_text(vjust = axis.title.x.vjust))
    }
    if (!is.null(axis.title.y.vjust)) {
      sjtheme <- sjtheme +
        theme(axis.title.y = element_text(vjust = axis.title.y.vjust))
    }
    # ----------------------------------------
    # panel grid colors
    # ----------------------------------------
    if (!is.null(panel.gridcol.x)) {
      sjtheme <- sjtheme +
        theme(panel.grid.minor.x = element_line(colour = panel.gridcol.x, linetype = panel.minor.linetype),
              panel.grid.major.x = element_line(colour = panel.gridcol.x, linetype = panel.major.linetype))
    }
    if (!is.null(panel.gridcol.y)) {
      sjtheme <- sjtheme +
        theme(panel.grid.minor.y = element_line(colour = panel.gridcol.y, linetype = panel.minor.linetype),
              panel.grid.major.y = element_line(colour = panel.gridcol.y, linetype = panel.major.linetype))
    }
    # ----------------------------------------
    # finally, set theme
    # ----------------------------------------
    theme_set(sjtheme)
  }
  else {
    warning("Either 'theme' or 'base' must be supplied as ggplot-theme-object to set global theme options for sjPlot.")
  }
  
  # ----------------------------------------
  # set defaults for geoms
  # ----------------------------------------
  # if (is.null(geom.colors)) geom.colors <- diverge_hcl(9)
  # sj.theme_scales(geom.colors)
  sj.theme_geoms(geom.alpha,
                 geom.linetype,
                 geom.outline.size,
                 geom.outline.color,
                 geom.boxoutline.size,
                 geom.boxoutline.color,
                 geom.errorbar.size,
                 geom.errorbar.linetype,
                 geom.label.size,
                 geom.label.color,
                 geom.label.alpha,
                 geom.label.angle)
  # return custom theme object
  invisible(sjtheme)
}


sj.theme_geoms <- function(geom.alpha,
                           geom.linetype,
                           geom.outline.size,
                           geom.outline.color,
                           geom.boxoutline.size,
                           geom.boxoutline.color,
                           geom.errorbar.size,
                           geom.errorbar.linetype,
                           geom.label.size,
                           geom.label.color,
                           geom.label.alpha,
                           geom.label.angle) {
  # ----------------------------------------
  # helper function to customize geoms
  # ----------------------------------------
  updateGeoms <- function(geoms, parameters) {
    for (geom in geoms) update_geom_defaults(geom, parameters)
  }
  
  # Geoms that only require a default colour.
  updateGeoms(c('abline', 
                'jitter', 
                'point', 
                'density', 
                'errorbar', 
                'errorbarh', 
                'freqpoly', 
                'hline', 
                'line', 
                'area', 
                'tile', 
                'dotplot', 
                'bar', 
                'histogram'), list(alpha = geom.alpha))
  
  update_geom_defaults('text', list(size = geom.label.size, 
                                    colour = geom.label.color,
                                    alpha = geom.label.alpha,
                                    angle = geom.label.angle))
  
  # Special geoms.
  update_geom_defaults('boxplot', list(size = geom.boxoutline.size, colour = geom.boxoutline.color, alpha = geom.alpha, outlier.colour = NA))
  update_geom_defaults('line', list(linetype = geom.linetype))
  updateGeoms(c('errorbar', 'errorbarh'), list(size = geom.errorbar.size, linetype = geom.errorbar.linetype))
  
  if (!is.null(geom.outline.color)) 
    updateGeoms(c('histogram', 'bar', 'dotplot', 'area', 'tile'), list(size = geom.outline.size, colour = geom.outline.color))
  else
    updateGeoms(c('histogram', 'bar', 'dotplot', 'area', 'tile'), list(size = 0, colour = NA))  
}


#' @title Helper function to set geom colors
#' @name sj.setGeomColors
#' 
#' @param plot a ggplot object where scales (geom colors) should be set
#' @param geom.colors the color palette for the scales to be used
#' @param pal.len the length of the required colors in \code{geom.colors}
#' @param show.guide whether or not legend should be displayed
#' @param labels a character vector with legend labels
#' @param reverse.colors If \code{TRUE}, the color scale is reversed.
#' 
#' @importFrom scales brewer_pal grey_pal
sj.setGeomColors <- function(plot, 
                             geom.colors, 
                             pal.len, 
                             show.guide = TRUE, 
                             labels = NULL,
                             reverse.colors = FALSE) {
  # ---------------------------------------------------------
  # check for themr options
  # ---------------------------------------------------------
  if (!is.null(geom.colors) && geom.colors == "themr") {
    return (plot)
  }
  # ---------------------------------------------------------
  # dummy function for setting legend labels and geom-colors
  # ---------------------------------------------------------
  usenormalscale <- function(plot, geom.colors, labels) {
    if (!show.guide) {
      plot <- plot + 
        scale_fill_manual(values = geom.colors, guide = FALSE) +
        scale_colour_manual(values = geom.colors, guide = FALSE) +
        guides(fill = FALSE)
    }
    else {
      plot <- plot + 
        scale_fill_manual(values = geom.colors, labels = labels) +
        scale_colour_manual(values = geom.colors, labels = labels)
    }
    return (plot)
  }
  # ---------------------------------------------------------
  # dummy function for only setting legend labels, but no
  # geom-colors
  # ---------------------------------------------------------
  uselegendscale <- function(plot, labels) {
    if (!show.guide) {
      plot <- plot + 
        scale_fill_discrete(guide = FALSE) +
        scale_colour_discrete(guide = FALSE) +
        guides(fill = FALSE)
    }
    else {
      plot <- plot + 
        scale_fill_discrete(labels = labels) +
        scale_colour_discrete(labels = labels)
    }
    return (plot)
  }
  # ---------------------------------------------------------
  # set geom colors
  # ---------------------------------------------------------
  if (!is.null(geom.colors)) {
    # brewer palette?
    if (is.brewer.pal(geom.colors[1])) {
      if (length(geom.colors) > 1) {
        neutral.color <- geom.colors[2]
        pal.len <- pal.len - 1
      }
      else {
        neutral.color <- NULL
      }
      geom.colors <- scales::brewer_pal(palette=geom.colors[1])(pal.len)
      if (reverse.colors) geom.colors <- rev(geom.colors)
      if (!is.null(neutral.color)) geom.colors <- c(geom.colors, neutral.color)
    }
    else if (geom.colors[1] == "gs") {
      geom.colors <- scales::grey_pal()(pal.len)
      if (reverse.colors) geom.colors <- rev(geom.colors)
    }
    else if (length(geom.colors) > pal.len) {
      warning("More colors provided than needed. Shortening color palette.")
      geom.colors <- geom.colors[1:pal.len]
      if (reverse.colors) geom.colors <- rev(geom.colors)
    }
    
    if (length(geom.colors) < pal.len) {
      warning("Too less colors provided for plot. Using default color palette.")
      plot <- uselegendscale(plot, labels)
    }
    else {
      plot <- usenormalscale(plot, geom.colors, labels)
    }
  }
  else {
    plot <- uselegendscale(plot, labels)
  }
  return (plot)
}