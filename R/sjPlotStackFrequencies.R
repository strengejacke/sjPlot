#' @title Plot stacked proportional bars
#' @name sjp.stackfrq
#' 
#' @seealso \itemize{
#'              \item \href{http://www.strengejacke.de/sjPlot/sjp.stackfrq/}{sjPlot manual: sjp.stackfrq}
#'              \item \code{\link{sjt.stackfrq}}
#'              }
#' 
#' @description Plot items (variables) of a scale as stacked proportional bars. This
#'                function is useful when several items with identical scale/categoroies
#'                should be plotted to compare the distribution of answers.
#' 
#' @note Thanks to \href{http://www.clas.ufl.edu/users/forrest/}{Forrest Stevens} for bug fixes.
#' 
#' @param items \code{\link{data.frame}} with each column representing one item.
#' @param legendLabels list or vector of strings that indicate the items' categories.
#'          Will also appear as legend text.
#' @param sort.frq indicates whether the \code{items} should be ordered by
#'          by highest count of first or last category of \code{items}.
#'          \describe{
#'            \item{\code{"first.asc"}}{to order ascending by lowest count of first category,}
#'            \item{\code{"first.desc"}}{to order descending by lowest count of first category,}
#'            \item{\code{"last.asc"}}{to order ascending by lowest count of last category,}
#'            \item{\code{"last.desc"}}{to order descending by lowest count of last category,}
#'            \item{\code{NULL}}{(default) for no sorting.}
#'          }
#' @param weightBy weight factor that will be applied to weight all cases from \code{items}.
#'          Must be a vector of same length as \code{nrow(items)}. Default is \code{NULL}, so no weights are used.
#' @param weightByTitleString suffix (as string) for the plot's title, if \code{weightBy} is specified,
#'          e.g. \code{weightByTitleString=" (weighted)"}. Default is \code{NULL}, so plot's 
#'          title will not have a suffix when cases are weighted.
#' @param hideLegend logical, indicates whether legend (guide) should be shown or not.
#' @param title plot's title
#' @param legendTitle title of plot's legend
#' @param includeN logical, if \code{TRUE} (default), the N of each item is included into axis labels.
#' @param geom.colors user defined color palette for geoms. If specified, must either be vector with color values 
#'          of same length as groups defined in \code{legendLabels}, or a specific color palette code.
#'          See 'Note' in \code{\link{sjp.grpfrq}}.
#' @param geom.size size resp. width of the geoms (bar width)
#' @param axisLabels.y character vector with labels for the y-axis (variable names 
#'          of \code{items}). Example: \code{axisLabels.y = c("Q1", "Q2", "Q3")}
#'          Axis labels will automatically be detected, when they have
#'          label attributes (see \code{\link[sjmisc]{set_label}}) for details).
#' @param breakTitleAt determines how many chars of the title are displayed in
#'          one line and when a line break is inserted into the title.
#' @param breakLabelsAt determines how many chars of the category labels are displayed in 
#'          one line and when a line break is inserted.
#' @param breakLegendTitleAt determines how many chars of the legend's title 
#'          are displayed in one line and when a line break is inserted.
#' @param breakLegendLabelsAt determines how many chars of the legend labels are 
#'          displayed in one line and when a line break is inserted.
#' @param gridBreaksAt set breaks for the axis, i.e. at every \code{gridBreaksAt}'th 
#'          position a major grid is being printed. Valid values range from 0 to 1.
#' @param expand.grid logical, if \code{TRUE} (default), the diagram has margins, 
#'          i.e. the y-axis is not exceeded to the diagram's boundaries.
#' @param axisTitle.x title for the x-axis. Default is \code{NULL} (no title).
#' @param axisTitle.y title for the y-axis. Default is \code{NULL} (no title).
#' @param showValueLabels Whether counts and percentage values should be plotted to each bar.
#' @param labelDigits The amount of digits for rounding \code{value.labels}. Default is 1, 
#'          i.e. value labels have 1 digit after decimal point.
#' @param showPercentageAxis If \code{TRUE} (default), the percentage values at the x-axis are shown.
#' @param showItemLabels Whether x axis text (category names) should be shown or not.
#' @param showSeparatorLine If \code{TRUE}, a line is drawn to visually "separate" each bar in the diagram.
#' @param separatorLineColor The color of the separator line. only applies, if \code{showSeparatorLine} is \code{TRUE}.
#' @param separatorLineSize The size of the separator line. only applies, if \code{showSeparatorLine} is \code{TRUE}.
#' @param coord.flip If \code{TRUE}, the x and y axis are swapped.
#' @return (Insisibily) returns the ggplot-object with the complete plot (\code{plot}) as well as the data frame that
#'           was used for setting up the ggplot-object (\code{df}).
#' 
#' @inheritParams sjp.grpfrq
#' @inheritParams sjp.frq
#' 
#' @examples
#' # -------------------------------
#' # random sample
#' # -------------------------------
#' # prepare data for 4-category likert scale, 5 items
#' Q1 <- as.factor(sample(1:4, 500, replace = TRUE, 
#'                        prob = c(0.2, 0.3, 0.1, 0.4)))
#' Q2 <- as.factor(sample(1:4, 500, replace = TRUE, 
#'                        prob = c(0.5, 0.25, 0.15, 0.1)))
#' Q3 <- as.factor(sample(1:4, 500, replace = TRUE, 
#'                        prob = c(0.25, 0.1, 0.4, 0.25)))
#' Q4 <- as.factor(sample(1:4, 500, replace = TRUE, 
#'                        prob = c(0.1, 0.4, 0.4, 0.1)))
#' Q5 <- as.factor(sample(1:4, 500, replace = TRUE, 
#'                        prob = c(0.35, 0.25, 0.15, 0.25)))
#' 
#' likert_4 <- data.frame(Q1, Q2, Q3, Q4, Q5)
#' 
#' # create labels
#' levels_4 <- c("Independent", 
#'               "Slightly dependent", 
#'               "Dependent", 
#'               "Severely dependent")
#' 
#' # plot stacked frequencies of 5 (ordered) item-scales
#' sjp.stackfrq(likert_4, legendLabels = levels_4)
#' 
#' 
#' # -------------------------------
#' # Data from the EUROFAMCARE sample dataset
#' # -------------------------------
#' library(sjmisc)
#' data(efc)
#' # recveive first item of COPE-index scale
#' start <- which(colnames(efc) == "c82cop1")
#' # recveive first item of COPE-index scale
#' end <- which(colnames(efc) == "c90cop9")
#' # auto-detection of labels
#' sjp.stackfrq(efc[, c(start:end)])
#' 
#' 
#' @import ggplot2
#' @importFrom dplyr group_by mutate arrange
#' @import sjmisc
#' @importFrom scales percent
#' @importFrom stats na.omit xtabs
#' @export
sjp.stackfrq <- function(items,
                         legendLabels = NULL,
                         sort.frq = NULL,
                         weightBy = NULL,
                         weightByTitleString = NULL,
                         hideLegend = FALSE,
                         title = NULL,
                         legendTitle = NULL,
                         includeN = TRUE,
                         axisLabels.y = NULL,
                         breakTitleAt = 50,
                         breakLabelsAt = 30,
                         breakLegendTitleAt = 30,
                         breakLegendLabelsAt = 28,
                         gridBreaksAt = 0.2,
                         expand.grid = FALSE,
                         geom.size = 0.5,
                         geom.colors = "Blues",
                         axisTitle.x = NULL,
                         axisTitle.y = NULL,
                         showValueLabels = TRUE,
                         labelDigits = 1,
                         vjust = "center",
                         showPercentageAxis = TRUE,
                         showItemLabels = TRUE,
                         showSeparatorLine = FALSE,
                         separatorLineColor = "grey80",
                         separatorLineSize = 0.3,
                         coord.flip = TRUE,
                         printPlot = TRUE) {
  # --------------------------------------------------------
  # check param. if we have a single vector instead of
  # a data frame with several items, convert vector to data frame
  # --------------------------------------------------------
  if (!is.data.frame(items) && !is.matrix(items)) items <- as.data.frame(items)
  # --------------------------------------------------------
  # check sorting
  # --------------------------------------------------------
  if (!is.null(sort.frq)) {
    if (sort.frq == "first.asc") {
      sort.frq  <- "first"
      reverseOrder <- FALSE
    } else if (sort.frq == "first.desc") {
      sort.frq  <- "first"
      reverseOrder <- TRUE
    } else if (sort.frq == "last.asc") {
      sort.frq  <- "last"
      reverseOrder <- TRUE
    } else if (sort.frq == "last.desc") {
      sort.frq  <- "last"
      reverseOrder <- FALSE
    } else {
      sort.frq  <- NULL
      reverseOrder <- FALSE
    }
  } else {
    reverseOrder <- FALSE
  }
  # --------------------------------------------------------
  # try to automatically set labels is not passed as parameter
  # --------------------------------------------------------
  if (is.null(legendLabels)) legendLabels <- sjmisc::get_labels(items[[1]],
                                                                attr.only = F,
                                                                include.values = NULL,
                                                                include.non.labelled = T)
  if (is.null(axisLabels.y)) {
    axisLabels.y <- c()
    # if yes, iterate each variable
    for (i in 1:ncol(items)) {
      # retrieve variable name attribute
      axisLabels.y <- c(axisLabels.y, sjmisc::get_label(items[[i]], def.value = colnames(items)[i]))
    }
  }
  # --------------------------------------------------------
  # If axisLabels.y were not defined, simply use column names
  # --------------------------------------------------------
  if (is.null(axisLabels.y)) axisLabels.y <- colnames(items)
  # --------------------------------------------------------
  # unlist/ unname axis labels
  # --------------------------------------------------------
  if (!is.null(axisLabels.y)) {
    # unname labels, if necessary, so we have a simple
    # character vector
    if (!is.null(names(axisLabels.y))) axisLabels.y <- as.vector(axisLabels.y)
  } 
  # --------------------------------------------------------
  # unlist/ unname axis labels
  # --------------------------------------------------------
  if (!is.null(legendLabels)) {
    # unname labels, if necessary, so we have a simple
    # character vector
    if (!is.null(names(legendLabels))) legendLabels <- as.vector(legendLabels)
  } 
  if (is.null(legendLabels)) {
    # if we have no legend labels, we iterate all data frame's
    # columns to find all unique items of the data frame.
    # In case one item has missing categories, this may be
    # "compensated" by looking at all items, so we have the
    # actual values of all items.
    legendLabels <- as.character(sort(unique(unlist(
      apply(items, 2, function(x) unique(stats::na.omit(x)))))))
  }
  # --------------------------------------------------------
  # Check whether N of each item should be included into
  # axis labels
  # --------------------------------------------------------
  if (includeN && !is.null(axisLabels.y)) {
    for (i in 1:length(axisLabels.y)) {
      axisLabels.y[i] <- paste(axisLabels.y[i], 
                               sprintf(" (n=%i)", length(stats::na.omit(items[[i]]))), 
                               sep = "")
    }
  }
  # -----------------------------------------------
  # if we have legend labels, we know the exact
  # amount of groups
  # -----------------------------------------------
  countlen <- length(legendLabels)
  # -----------------------------------------------
  # create cross table for stats, summary etc.
  # and weight variable. do this for each item that was
  # passed as parameter
  #---------------------------------------------------
  mydat <- c()
  # ----------------------------
  # determine minimum value. if 0, add one, because
  # vector indexing starts with 1
  # ----------------------------
  if (any(apply(items, c(1, 2), is.factor)) || any(apply(items, c(1, 2), is.character))) {
    diff <- ifelse(min(apply(items, c(1, 2), as.numeric), na.rm = TRUE) == 0, 1, 0)
  } else {
    diff <- ifelse(min(items, na.rm = TRUE) == 0, 1, 0)
  }
  # iterate item-list
  for (i in 1:ncol(items)) {
    # get each single items
    variable <- items[[i]]
    # -----------------------------------------------
    # create proportional table so we have the percentage
    # values that should be used as y-value for the bar charts
    # We now have a data frame with categories, group-association
    # and percentage values (i.e. each cell as separate row in the
    # data frame)
    # -----------------------------------------------
    # check whether counts should be weighted or not
    if (is.null(weightBy)) {
      df <- as.data.frame(prop.table(table(variable)))
    } else {
      df <- as.data.frame(prop.table(round(stats::xtabs(weightBy ~ variable), 0)))
    }
    # give columns names
    names(df) <- c("var", "prc")
    # need to be numeric, so percentage values (see below) are
    # correctly assigned, i.e. missing categories are considered
    df$var <- sjmisc::to_value(df$var, keep.labels = F) + diff # if categories start with zero, fix this here
    # Create a vector of zeros 
    prc <- rep(0, countlen)
    # Replace the values in prc for those indices which equal df$var
    prc[df$var] <- df$prc
    # create new data frame. We now have a data frame with all
    # variable categories abd their related percentages, including
    # zero counts, but no(!) missings!
    mydf <- data.frame(grp = i, 
                       cat = 1:countlen, 
                       prc)
    # now, append data frames
    mydat <- data.frame(rbind(mydat, mydf))
  }
  # ----------------------------
  # make sure group and count variable 
  # are factor values
  # ----------------------------
  mydat$grp <- as.factor(mydat$grp)
  mydat$cat <- as.factor(mydat$cat)
  # add half of Percentage values as new y-position for stacked bars
  mydat <- mydat %>% 
    dplyr::group_by(grp) %>% 
    dplyr::mutate(ypos = cumsum(prc) - 0.5 * prc) %>% 
    dplyr::arrange(grp)
  # --------------------------------------------------------
  # Prepare and trim legend labels to appropriate size
  # --------------------------------------------------------
  # wrap legend text lines
  legendLabels <- sjmisc::word_wrap(legendLabels, breakLegendLabelsAt)    
  # check whether we have a title for the legend
  # if yes, wrap legend title line
  if (!is.null(legendTitle)) legendTitle <- sjmisc::word_wrap(legendTitle, breakLegendTitleAt)
  # check length of diagram title and split longer string at into new lines
  # every 50 chars
  if (!is.null(title)) {
    # if we have weighted values, say that in diagram's title
    if (!is.null(weightByTitleString)) title <- paste0(title, weightByTitleString)
    title <- sjmisc::word_wrap(title, breakTitleAt)    
  }
  # check length of x-axis-labels and split longer strings at into new lines
  # every 10 chars, so labels don't overlap
  if (!is.null(axisLabels.y)) axisLabels.y <- sjmisc::word_wrap(axisLabels.y, breakLabelsAt)    
  # ----------------------------
  # Check if ordering was requested
  # ----------------------------
  if (!is.null(sort.frq)) {
    # order by first cat
    if (sort.frq == "first") {
      facord <- order(mydat$prc[which(mydat$cat == 1)])
    } else {
      # order by last cat
      facord <- order(mydat$prc[which(mydat$cat == countlen)])
    }
    # create dummy vectors from 1 to itemlength
    dummy1 <- dummy2 <- c(1:length(facord))
    # facords holds the ordered item indices! we now need to
    # change the original item-index with its ordered position index.
    # example:
    # we have 4 items, and they may be ordered like this:
    # 1 3 4 2
    # so the first item is the one with the lowest count , item 3 is on second postion, 
    # item 4 is on third position and item 2 is the last item (with highest count)
    # we now need their order as subsequent vector: 1 4 2 3
    # (i.e. item 1 is on first pos, item 2 is on fourth pos, item 3 is on
    # second pos and item 4 is on third pos in order)
    if (reverseOrder) {
      dummy2[rev(facord)] <- dummy1
    } else {
      dummy2[facord] <- dummy1
    }
    # now we have the order of either lowest to highest counts of first
    # or last category of "items". We now need to repeat these values as 
    # often as we have answer categories
    orderedrow <- unlist(tapply(dummy2, 1:length(dummy2), function(x) rep(x, countlen)))
    # replace old grp-order by new order
    mydat$grp <- as.factor(orderedrow)
    # reorder axis labels as well
    axisLabels.y <- axisLabels.y[order(dummy2)]
  }
  # --------------------------------------------------------
  # check if category-oder on x-axis should be reversed
  # change category label order then
  # --------------------------------------------------------
  if (reverseOrder && is.null(sort.frq)) axisLabels.y <- rev(axisLabels.y)
  # --------------------------------------------------------
  # set diagram margins
  # --------------------------------------------------------
  if (expand.grid) {
    expgrid <- ggplot2::waiver()
  } else {
    expgrid <- c(0, 0)
  }
  # --------------------------------------------------------
  # Set value labels and label digits
  # --------------------------------------------------------
  mydat$labelDigits <- labelDigits
  if (showValueLabels) {
    ggvaluelabels <-  geom_text(aes(y = ypos, label = sprintf("%.*f%%", labelDigits, 100 * prc)),
                                vjust = vjust)
  } else {
    ggvaluelabels <-  geom_text(aes(y = ypos), label = "")
  }
  # --------------------------------------------------------
  # Set up grid breaks
  # --------------------------------------------------------
  if (is.null(gridBreaksAt)) {
    gridbreaks <- ggplot2::waiver()
  } else {
    gridbreaks <- c(seq(0, 1, by = gridBreaksAt))
  }
  # --------------------------------------------------------
  # check if category-oder on x-axis should be reversed
  # change x axis order then
  # --------------------------------------------------------
  if (reverseOrder && is.null(sort.frq)) {
    baseplot <- ggplot(mydat, aes(x = rev(grp), y = prc, fill = cat))
  } else {
    baseplot <- ggplot(mydat, aes(x = grp, y = prc, fill = cat))
  }  
  baseplot <- baseplot +
    # plot bar chart
    geom_bar(stat = "identity", position = "stack", width = geom.size)
  # --------------------------------------------------------
  # check whether bars should be visually separated by an 
  # additional separator line
  # --------------------------------------------------------
  if (showSeparatorLine) {
    baseplot <- baseplot +
      geom_vline(xintercept = c(seq(1.5, length(items), by = 1)), 
                 size = separatorLineSize, 
                 colour = separatorLineColor)
  }
  # -----------------
  # show/hide percentage values on x axis
  # ----------------------------
  if (isTRUE(showPercentageAxis))
    perc.val <- scales::percent
  else
    perc.val <- NULL
  # -----------------
  # start plot here
  # ----------------------------
  baseplot <- baseplot +
    # show absolute and percentage value of each bar.
    ggvaluelabels +
    # no additional labels for the x- and y-axis, only diagram title
    labs(title = title, x = axisTitle.x, y = axisTitle.y, fill = legendTitle) +
    # print value labels to the x-axis.
    # If parameter "axisLabels.y" is NULL, the category numbers (1 to ...) 
    # appear on the x-axis
    scale_x_discrete(labels = axisLabels.y) +
    # set Y-axis, depending on the calculated upper y-range.
    # It either corresponds to the maximum amount of cases in the data set
    # (length of var) or to the highest count of var's categories.
    scale_y_continuous(breaks = gridbreaks, 
                       limits = c(0, 1), 
                       expand = expgrid, 
                       labels = perc.val)
  # check whether coordinates should be flipped, i.e.
  # swap x and y axis
  if (coord.flip) baseplot <- baseplot + coord_flip()
  # ---------------------------------------------------------
  # set geom colors
  # ---------------------------------------------------------
  baseplot <- sj.setGeomColors(baseplot, 
                               geom.colors, 
                               length(legendLabels), 
                               ifelse(isTRUE(hideLegend), FALSE, TRUE), 
                               legendLabels)
  # ---------------------------------------------------------
  # Check whether ggplot object should be returned or plotted
  # ---------------------------------------------------------
  if (printPlot) graphics::plot(baseplot)
  # -------------------------------------
  # return results
  # -------------------------------------
  invisible(structure(class = "sjpstackfrq",
                      list(plot = baseplot,
                           df = mydat)))
}
