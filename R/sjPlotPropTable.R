# bind global variables
utils::globalVariables(c("rowname", "total", "ges", "prc", "n", "Count", "Group", "line.break"))

#' @title Plot contingency tables
#' @name sjp.xtab
#' 
#' @seealso \itemize{
#'              \item \href{http://www.strengejacke.de/sjPlot/sjp.xtab}{sjPlot manual: sjp.xtab}
#'              \item \code{\link{sjt.xtab}}
#'              }
#' 
#' @description Plot proportional crosstables (contingency tables) of two variables as ggplot diagram.
#' 
#' @param x a vector of values (variable) describing the bars which make up the plot.
#' @param grp grouping variable of same length as \code{x}, where \code{x} 
#'          is grouped into the categories represented by \code{grp}.
#' @param weightBy weight factor that will be applied to weight all cases from \code{x}.
#'          Must be a vector of same length as \code{x}. Default is \code{NULL}, so no weights are used.
#' @param type plot type. may be either \code{"b"}, \code{"bar"}, \code{"bars"} (default) for bar charts,
#'          or \code{"l"}, \code{"line"}, \code{"lines"} for line diagram.
#' @param tableIndex indicates which data of the proportional table should be plotted. Use \code{"row"} for
#'          calculating row percentages, \code{"col"} for column percentages and \code{"cell"} for cell percentages.
#'          If \code{tableIndex = "col"}, an additional bar with the total sum of each column
#'          can be added to the plot (see \code{showTotalColumn}).
#' @param barPosition indicates whether bars should be positioned side-by-side (default)
#'          or stacked (use \code{"stack"} as argument).
#' @param reverseOrder logical, whether categories along the x-axis should apper in reversed order or not.
#' @param geom.colors user defined color palette for geoms. If specified, must either be vector with color values 
#'          of same length as groups defined in \code{x}, or a specific color palette code.
#'          See 'Note' in \code{\link{sjp.grpfrq}}.
#' @param geom.size size resp. width of the geoms (bar width).
#' @param lineDotSize dot size, only applies, when argument \code{type = "lines"}.
#' @param smoothLines prints a smooth line curve. Only applies, when argument \code{type = "lines"}.
#' @param stringTotal string for the legend label when a total-column is added. Only applies
#'          if \code{showTotalColumn = TRUE}. Default is \code{"Total"}.
#' @param showCategoryLabels whether x-axis text (category names) should be shown or not.
#' @param showTotalColumn when \code{tableIndex = "col"}, an additional bar 
#'          with the sum within each category and it's percentages will be added 
#'          to each category.
#' @param axisTitle.x title for the x-axis. Default is \code{NULL}, so variable name
#'          of \code{x} will automatically be detected and used as axis title
#'          (see \code{\link[sjmisc]{set_label}}) for details).
#' @param axisTitle.y title for the y-axis. Default is \code{NULL}, so variable name
#'          of \code{grp} will automatically be detected and used as axis title
#'          (see \code{\link[sjmisc]{set_label}}) for details).
#'          
#' @inheritParams sjp.grpfrq
#' 
#' @return (Insisibily) returns the ggplot-object with the complete plot (\code{plot}) as well as the data frame that
#'           was used for setting up the ggplot-object (\code{mydf}).
#' 
#' @examples
#' # create 4-category-items
#' grp <- sample(1:4, 100, replace = TRUE)
#' # create 3-category-items
#' x <- sample(1:3, 100, replace = TRUE)
#' 
#' # plot "cross tablulation" of x and grp
#' sjp.xtab(x, grp)
#' 
#' # plot "cross tablulation" of x and y, including labels
#' sjp.xtab(x, grp, 
#'          axisLabels.x = c("low", "mid", "high"),
#'          legendLabels = c("Grp 1", "Grp 2", "Grp 3", "Grp 4"))
#' 
#' # plot "cross tablulation" of x and grp
#' # as stacked proportional bars
#' sjp.xtab(x, grp, 
#'          tableIndex = "row", 
#'          barPosition = "stack", 
#'          showTableSummary = TRUE,
#'          coord.flip = TRUE)
#' 
#' # example with vertical labels
#' library(sjmisc)
#' data(efc)
#' sjp.setTheme(geom.label.angle = 90)
#' sjp.xtab(efc$e42dep, 
#'          efc$e16sex,
#'          vjust = "center",
#'          hjust = "bottom")
#' 
#' # grouped bars with EUROFAMCARE sample dataset
#' # dataset was importet from an SPSS-file,
#' # see ?sjmisc::read_spss
#' data(efc)
#' efc.val <- get_labels(efc)
#' efc.var <- get_label(efc)
#' 
#' sjp.xtab(efc$e42dep,
#'          efc$e16sex,
#'          title = efc.var['e42dep'],
#'          axisLabels.x = efc.val[['e42dep']],
#'          legendTitle = efc.var['e16sex'],
#'          legendLabels = efc.val[['e16sex']])
#'          
#' sjp.xtab(efc$e16sex,
#'          efc$e42dep,
#'          title = efc.var['e16sex'],
#'          axisLabels.x = efc.val[['e16sex']],
#'          legendTitle = efc.var['e42dep'],
#'          legendLabels = efc.val[['e42dep']])
#'          
#' # -------------------------------
#' # auto-detection of labels works here
#' # so no need to specify labels. For
#' # title-auto-detection, use NULL
#' # -------------------------------
#' sjp.xtab(efc$e16sex, efc$e42dep, title = NULL)
#' 
#' sjp.xtab(efc$e16sex,
#'          efc$e42dep,
#'          tableIndex = "row",
#'          barPosition = "stack",
#'          coord.flip = TRUE)
#'
#'
#' @import ggplot2
#' @import sjmisc
#' @importFrom dplyr group_by mutate arrange add_rownames filter select summarize
#' @importFrom tidyr gather
#' @importFrom scales percent
#' @importFrom stats na.omit
#' @export
sjp.xtab <- function(x,
                     grp,
                     title = "",
                     legendTitle = NULL,
                     weightBy = NULL,
                     weightByTitleString = NULL,
                     type = "bars",
                     tableIndex = "col",
                     reverseOrder = FALSE,
                     axisLimits.y = NULL,
                     axisLabels.x = NULL,
                     legendLabels = NULL,
                     vjust = "bottom",
                     hjust = "center",
                     y.offset = NULL,
                     stringTotal = "Total",
                     breakTitleAt = 50,
                     breakLabelsAt = 15,
                     breakLegendTitleAt = 20,
                     breakLegendLabelsAt = 20,
                     gridBreaksAt = 0.2,
                     geom.size = 0.7,
                     geom.spacing = 0.1,
                     geom.colors = "Paired",
                     barPosition = "dodge",
                     lineDotSize = 3,
                     smoothLines = FALSE,
                     expand.grid = FALSE,
                     showValueLabels = TRUE,
                     showCountValues = TRUE,
                     showPercentageValues = TRUE,
                     showCategoryLabels = TRUE,
                     showTableSummary = FALSE,
                     tableSummaryPos = "r",
                     showTotalColumn = TRUE,
                     hideLegend = FALSE,
                     axisTitle.x = NULL,
                     axisTitle.y = NULL,
                     coord.flip = FALSE,
                     printPlot = TRUE) {
  # --------------------------------------------------------
  # get variable name
  # --------------------------------------------------------
  var.name.cnt <- get_var_name(deparse(substitute(x)))
  var.name.grp <- get_var_name(deparse(substitute(grp)))
  # --------------------------------------------------------
  # We have several options to name the diagram type
  # Here we will reduce it to a unique value
  # --------------------------------------------------------
  if (type == "b" || type == "bar") type <- "bars"
  if (type == "l" || type == "line") type <- "lines"
  if (expand.grid == TRUE) {
    expand.grid <- ggplot2::waiver()
  } else {
    expand.grid <- c(0, 0)
  }
  # --------------------------------------------------------
  # set text label offset
  # --------------------------------------------------------
  if (is.null(y.offset)) {
    # stacked bars?
    if (barPosition == "stack") {
      y_offset <- 0
    } else {
      y.offset <- .005
      if (coord.flip) {
        if (missing(vjust)) vjust <- "center"
        if (missing(hjust)) hjust <- "bottom"
        if (hjust == "bottom")
          y_offset <- y.offset
        else if (hjust == "top")
          y_offset <- -y.offset
        else
          y_offset <- 0
      } else {
        if (vjust == "bottom")
          y_offset <- y.offset
        else if (vjust == "top")
          y_offset <- -y.offset
        else
          y_offset <- 0
      }
    }
  } else {
    y_offset <- y.offset
  }
  # --------------------------------------------------------
  # total column only applies to column percentages
  # --------------------------------------------------------
  if (tableIndex != "col") showTotalColumn <- FALSE
  # --------------------------------------------------------
  # create cross table of frequencies and percentages
  # --------------------------------------------------------
  mydat <- create.xtab.df(x,
                          grp,
                          round.prz = 2,
                          na.rm = T,
                          weightBy = weightBy)
  # --------------------------------------------------------
  # x-position as numeric factor, added later after
  # tidying
  # --------------------------------------------------------
  bars.xpos <- 1:nrow(mydat$mydat)
  # --------------------------------------------------------
  # try to automatically set labels is not passed as argument
  # --------------------------------------------------------
  if (is.null(axisLabels.x)) axisLabels.x <- mydat$labels.cnt
  if (is.null(legendLabels)) legendLabels <- mydat$labels.grp
  if (is.null(axisTitle.x)) axisTitle.x <- sjmisc::get_label(x, def.value = var.name.cnt)
  if (is.null(legendTitle)) legendTitle <- sjmisc::get_label(grp, def.value = var.name.grp)
  if (is.null(title)) {
    t1 <- sjmisc::get_label(x, def.value = var.name.cnt)
    t2 <- sjmisc::get_label(grp, def.value = var.name.grp)
    if (!is.null(t1) && !is.null(t2)) title <- paste0(t1, " by ", t2)
  }
  # --------------------------------------------------------
  # remove titles if empty
  # --------------------------------------------------------
  if (!is.null(legendTitle) && legendTitle == "") legendTitle <- NULL
  if (!is.null(axisTitle.x) && axisTitle.x == "") axisTitle.x <- NULL
  if (!is.null(axisTitle.y) && axisTitle.y == "") axisTitle.y <- NULL  
  if (!is.null(title) && title == "") title <- NULL    
  # --------------------------------------------------------
  # Check if user wants to add total column, and if so,
  # define amount of categories
  # --------------------------------------------------------
  if (showTotalColumn) legendLabels <- c(legendLabels, stringTotal)
  grpcount <- length(legendLabels)
  # -----------------------------------------------
  # check whether row, column or cell percentages are requested
  #---------------------------------------------------
  if (tableIndex == "cell")
    myptab <- mydat$proptab.cell
  else if (tableIndex == "col")
    myptab <- mydat$proptab.col
  else if (tableIndex == "row")
    myptab <- mydat$proptab.row
  myptab <- dplyr::add_rownames(data.frame(myptab))
  # -----------------------------------------------
  # tidy data
  #---------------------------------------------------
  mydf <- tidyr::gather(myptab,
                        "group",
                        "prc",
                        2:(grpcount + 1),
                        factor_key = TRUE)
  # -----------------------------------------------
  # add total column and row to n-values
  #---------------------------------------------------
  if (tableIndex != "row")
    mydat$mydat$total <- unname(rowSums(mydat$mydat[, -1]))
  if (tableIndex != "col")
    mydat$mydat <-
    rbind(mydat$mydat, c("total", unname(colSums(mydat$mydat[, -1]))))
  # -----------------------------------------------
  # add n-values to tidy data frame
  #---------------------------------------------------
  dummydf <- tidyr::gather(mydat$mydat,
                           "group",
                           "n",
                           2:(grpcount + 1),
                           factor_key = TRUE)
  mydf$n <- as.numeric(dummydf$n)
  # -----------------------------------------------
  # remove total for row and column index
  #---------------------------------------------------
  if (tableIndex != "col") mydf <- dplyr::filter(mydf, rowname != "total")
  if (tableIndex == "cell") mydf <- dplyr::select(mydf, -total)
  # --------------------------------------------------------
  # add xpos now
  # --------------------------------------------------------
  mydf$xpos <- as.factor(as.numeric(bars.xpos))
  # --------------------------------------------------------
  # add half of Percentage values as new y-position for stacked bars
  # --------------------------------------------------------
  mydf <- mydf %>%
    dplyr::group_by(xpos) %>%
    dplyr::mutate(ypos = cumsum(prc) - 0.5 * prc) %>%
    dplyr::arrange(group)
  # --------------------------------------------------------
  # add line-break char
  # --------------------------------------------------------
  if (showPercentageValues && showCountValues) {
    mydf$line.break <- ifelse(coord.flip == TRUE, ' ', '\n')
  } else {
    mydf$line.break <- ""
  }
  # --------------------------------------------------------
  # define label position for dodged bars
  # --------------------------------------------------------
  if (barPosition == "dodge") mydf$ypos <- mydf$prc
  # --------------------------------------------------------
  # finally, percentage values need to be between 0 and 1
  # --------------------------------------------------------
  mydf$prc <- mydf$prc / 100
  mydf$ypos <- mydf$ypos / 100
  # --------------------------------------------------------
  # Prepare and trim legend labels to appropriate size
  # --------------------------------------------------------
  if (!is.null(legendLabels)) legendLabels <- sjmisc::word_wrap(legendLabels, breakLegendLabelsAt)
  if (!is.null(legendTitle)) legendTitle <- sjmisc::word_wrap(legendTitle, breakLegendTitleAt)
  if (!is.null(title)) {
    # if we have weighted values, say that in diagram's title
    if (!is.null(weightByTitleString)) title <- paste(title, weightByTitleString, sep = "")
    title <- sjmisc::word_wrap(title, breakTitleAt)
  }
  if (!is.null(axisTitle.x)) axisTitle.x <- sjmisc::word_wrap(axisTitle.x, breakTitleAt)
  if (!is.null(axisTitle.y)) axisTitle.y <- sjmisc::word_wrap(axisTitle.y, breakTitleAt)
  if (!is.null(axisLabels.x)) axisLabels.x <- sjmisc::word_wrap(axisLabels.x, breakLabelsAt)
  # ----------------------------
  # create expression with model summarys. used
  # for plotting in the diagram later
  # ----------------------------
  if (showTableSummary) {
    modsum <- crosstabsum(x, grp, weightBy)
  } else {
    modsum <- NULL
  }
  # --------------------------------------------------------
  # Prepare bar charts
  # --------------------------------------------------------
  # calculate upper y-axis-range
  # if we have a fixed value, use this one here
  lower_lim <- 0
  # calculate upper y-axis-range
  # if we have a fixed value, use this one here
  if (!is.null(axisLimits.y) && length(axisLimits.y) == 2) {
    lower_lim <- axisLimits.y[1]
    upper_lim <- axisLimits.y[2]
  } else if (barPosition == "stack") {
    # check upper limits. we may have rounding errors, so values
    # sum up to more than 100%
    ul <- max(mydf %>% 
                dplyr::group_by(rowname) %>% 
                dplyr::summarize(ges = sum(prc)) %>% 
                dplyr::select(ges), na.rm = T)
    if (ul > 1L)
      upper_lim <- ul
    else
      upper_lim <- 1
  } else {
    # factor depends on labels
    if (showValueLabels == TRUE)
      mlp <- 1.2
    else
      mlp <- 1.1
    # else calculate upper y-axis-range depending
    # on the amount of max. answers per category
    upper_lim <- max(mydf$prc) * mlp
  }
  # --------------------------------------------------------
  # check if category-oder on x-axis should be reversed
  # change category label order then
  # --------------------------------------------------------
  if (reverseOrder) {
    axisLabels.x <- rev(axisLabels.x)
    mydf$xpos <- rev(mydf$xpos)
  }
  # --------------------------------------------------------
  # align dodged position of labels to bar positions
  # --------------------------------------------------------
  posdodge <- ifelse(type == "lines", 0, geom.size + geom.spacing)
  if (!showCategoryLabels) axisLabels.x <- c("")
  # --------------------------------------------------------
  # Set value labels
  # --------------------------------------------------------
  if (showValueLabels) {
    # if we have dodged bars or dots, we have to use a slightly dodged position for labels
    # as well, sofor better reading
    if (barPosition == "dodge") {
      if (showPercentageValues && showCountValues) {
        ggvaluelabels <- geom_text(aes(y = ypos + y_offset, label = sprintf("%.01f%%%s(n=%i)", 100 * prc, line.break, n)),
                                   position = position_dodge(posdodge),
                                   vjust = vjust,
                                   hjust = hjust)
      } else if (showPercentageValues) {
        ggvaluelabels <- geom_text(aes(y = ypos + y_offset, label = sprintf("%.01f%%", 100 * prc)),
                                   position = position_dodge(posdodge),
                                   vjust = vjust,
                                   hjust = hjust)
      } else if (showCountValues) {
        ggvaluelabels <- geom_text(aes(y = ypos + y_offset, label = sprintf("n=%i", n)),
                                   position = position_dodge(posdodge),
                                   vjust = vjust,
                                   hjust = hjust)
      }
    } else {
      if (showPercentageValues && showCountValues) {
        ggvaluelabels <- geom_text(aes(y = ypos, label = sprintf("%.01f%%%s(n=%i)", 100 * prc, line.break, n)),
                                   vjust = vjust,
                                   hjust = hjust)
      } else if (showPercentageValues) {
        ggvaluelabels <- geom_text(aes(y = ypos, label = sprintf("%.01f%%", 100 * prc)),
                                   vjust = vjust,
                                   hjust = hjust)
      } else if (showCountValues) {
        ggvaluelabels <- geom_text(aes(y = ypos, label = sprintf("n=%i", n)),
                                   vjust = vjust,
                                   hjust = hjust)
      }
    }
  } else {
    ggvaluelabels <- geom_text(aes(y = ypos), label = "")
  }
  # --------------------------------------------------------
  # Set up grid breaks
  # --------------------------------------------------------
  if (is.null(gridBreaksAt)) {
    gridbreaks <- ggplot2::waiver()
  } else {
    gridbreaks <- c(seq(lower_lim, upper_lim, by = gridBreaksAt))
  }
  # ----------------------------------
  # construct final plot, base constructor
  # first, set x scale
  # ----------------------------------
  scalex <- scale_x_discrete(labels = axisLabels.x)
  # ----------------------------------
  # check whether bars or lines should be printed
  # ----------------------------------
  if (type == "bars") {
    if (barPosition == "dodge") {
      geob <- geom_bar(stat = "identity", 
                       position = position_dodge(posdodge), 
                       width = geom.size)
    } else {
      geob <- geom_bar(stat = "identity",
                       position = "stack", 
                       width = geom.size)
    }
  # check if we have lines
  } else if (type == "lines") {
    # for lines, numeric scale
    mydf$xpos <- sjmisc::to_value(mydf$xpos, keep.labels = F)
    line.stat <- ifelse(smoothLines == TRUE, "smooth", "identity")
    geob <- geom_line(aes(colour = group),
                      size = geom.size, 
                      stat = line.stat)
  }
  # --------------------------------------------------------
  # start plot here
  # --------------------------------------------------------
  baseplot <- ggplot(mydf, aes(x = xpos, y = prc, fill = group)) + geob
  # if we have line diagram, print lines here
  if (type == "lines") {
    baseplot <- baseplot + 
      geom_point(size = lineDotSize, 
                 shape = 21, 
                 show.legend = FALSE)
  }
  # ------------------------------------------
  # check whether table summary should be printed
  # ------------------------------------------
  baseplot <- print.table.summary(baseplot,
                                  modsum,
                                  tableSummaryPos)
  baseplot <- baseplot +
    # show absolute and percentage value of each bar.
    ggvaluelabels +
    # no additional labels for the x- and y-axis, only diagram title
    labs(title = title, 
         x = axisTitle.x, 
         y = axisTitle.y, 
         fill = legendTitle) +
    # print value labels to the x-axis.
    # If argument "axisLabels.x" is NULL, the category numbers (1 to ...) 
    # appear on the x-axis
    scalex +
    # set Y-axis, depending on the calculated upper y-range.
    # It either corresponds to the maximum amount of cases in the data set
    # (length of var) or to the highest count of var's categories.
    scale_y_continuous(breaks = gridbreaks, 
                       limits = c(lower_lim, upper_lim), 
                       expand = expand.grid, 
                       labels = percent)
  # check whether coordinates should be flipped, i.e.
  # swap x and y axis
  if (coord.flip) baseplot <- baseplot + coord_flip()
  # ---------------------------------------------------------
  # set geom colors
  # ---------------------------------------------------------
  baseplot <- sj.setGeomColors(baseplot, 
                               geom.colors, 
                               length(legendLabels), 
                               ifelse(hideLegend == TRUE, FALSE, TRUE), 
                               legendLabels)
  # ---------------------------------------------------------
  # Check whether ggplot object should be returned or plotted
  # ---------------------------------------------------------
  if (printPlot) plot(baseplot)
  # -------------------------------------
  # return results
  # -------------------------------------
  invisible(structure(class = "sjpxtab",
                      list(plot = baseplot,
                           mydf = mydf)))
}
