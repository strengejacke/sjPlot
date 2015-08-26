# bind global variables
utils::globalVariables(c("Perc", "Sum", "Count", "Group", "line.break"))

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
#' @param jitterValueLabels logical, if \code{TRUE}, the value labels on the bars will be "jittered", 
#'          i.e. they have alternating vertical positions to avoid overlapping of labels in case bars are
#'          very short. Default is \code{FALSE}.
#' @param labelPos positioning of value labels. If \code{barPosition = "dodge"} 
#'          (default), use either \code{"inside"} or \code{"outside"} (default) to put labels in-
#'          or outside the bars. You may specify initial letter only. Use \code{"center"} 
#'          to center labels (useful if label angle is changes via \code{\link{sjp.setTheme}}).
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
#' # hjust-aes needs adjustment for this
#' library(ggplot2)
#' update_geom_defaults('text', list(hjust = -0.1))
#' sjp.xtab(efc$e42dep, 
#'          efc$e16sex,
#'          labelPos = "center")
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
#'          coord.flip = TRUE,
#'          jitterValueLabels = TRUE)
#'
#'
#' @import ggplot2
#' @importFrom dplyr group_by mutate arrange
#' @import sjmisc
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
                     labelPos = "outside",
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
                     jitterValueLabels = FALSE,
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
  # try to automatically set labels is not passed as argument
  # --------------------------------------------------------
  if (is.null(axisLabels.x)) axisLabels.x <- sjmisc:::autoSetValueLabels(x)
  if (is.null(legendLabels)) legendLabels <- sjmisc:::autoSetValueLabels(grp)
  if (is.null(axisTitle.x)) axisTitle.x <- sjmisc:::autoSetVariableLabels(x)
  if (is.null(legendTitle)) legendTitle <- sjmisc:::autoSetVariableLabels(grp)  
  if (is.null(title)) {
    t1 <- sjmisc:::autoSetVariableLabels(x)
    t2 <- sjmisc:::autoSetVariableLabels(grp)
    if (!is.null(t1) && !is.null(t2)) title <- paste0(t1, " by ", t2)
  }
  # --------------------------------------------------------
  # remove titles if empty
  # --------------------------------------------------------
  if (!is.null(legendTitle) && legendTitle == "") legendTitle <- NULL
  if (!is.null(axisTitle.x) && axisTitle.x == "") axisTitle.x <- NULL
  if (!is.null(axisTitle.y) && axisTitle.y == "") axisTitle.y <- NULL  
  if (!is.null(title) && title == "") title <- NULL    
  # determine table index, i.e. if row-percentages, column-percentages
  # or cell-percentages should be displayed
  tindex <- ifelse(tableIndex == "row", 1, 2)
  # --------------------------------------------------------
  # convert factor to numeric
  # --------------------------------------------------------
  if (is.factor(grp)) grp <- sjmisc::to_value(grp, keep.labels = F)
  if (is.factor(x)) x <- sjmisc::to_value(x, keep.labels = F)
  # --------------------------------------------------------
  # We have several options to name the diagram type
  # Here we will reduce it to a unique value
  # --------------------------------------------------------
  if (type == "b" || type == "bar") type <- c("bars")
  if (type == "l" || type == "line") type <- c("lines")
  if (expand.grid == TRUE) {
    expand.grid <- ggplot2::waiver()
  } else {
    expand.grid <- c(0, 0)
  }
  # --------------------------------------------------------
  # unlist labels
  # --------------------------------------------------------
  if (!is.null(axisLabels.x) && is.list(axisLabels.x)) {
    axisLabels.x <- unlistlabels(axisLabels.x)
  }
  if (!is.null(legendLabels) && is.list(legendLabels)) {
    legendLabels <- unlistlabels(legendLabels)
  }
  # -----------------------------------------------
  # handle zero-counts
  # -----------------------------------------------
  # Determine length of count and group var
  grplen <- length(unique(stats::na.omit(grp)))
  countlen <- length(unique(stats::na.omit(x)))
  # if we have legend labels, we know the exact
  # amount of groups
  if (is.null(legendLabels)) {
    grpcount <- grplen
  } else {
    grpcount <- length(legendLabels)
  }
  # if we have category labels, we know the exact
  # amount of categories
  if (is.null(axisLabels.x)) {
    catcount <- countlen
  } else {
    catcount <- length(axisLabels.x)
  }
  # -----------------------------------------------
  # create cross table for stats, summary etc.
  # and weight variable
  #---------------------------------------------------
  if (is.null(weightBy)) {
    ftab <- table(x, grp)
  } else {
    ftab <- round(xtabs(weightBy ~ x + grp), 0)
  }
  # -----------------------------------------------
  # create proportional table so we have the percentage
  # values that should be used as y-value for the bar charts
  # We now have a data frame with categories, group-association
  # and percentage values (i.e. each cell as separate row in the
  # data frame)
  # -----------------------------------------------
  if (tableIndex == "cell") {
    mydf <- as.data.frame(prop.table(ftab))
  } else {
    mydf <- as.data.frame(prop.table(ftab, tindex))
  }
  # -----------------------------------------------
  # Bind N-values as extra column to the data frame
  # -----------------------------------------------
  mydf <- cbind(mydf, as.data.frame(ftab)[, 3])
  names(mydf) <- c("Count", "Group", "Perc", "Sum")
  # -----------------------------------------------
  # don't show bar with category sum score when we 
  # have column or cell percentages
  # -----------------------------------------------
  if (tableIndex == "row" || tableIndex == "cell") showTotalColumn <- FALSE
  # -----------------------------------------------
  # Sum scores / total percentages for each category
  # -----------------------------------------------
  if (showTotalColumn) {
    # retrieve category counts / percentages, exclude missings of both category and count variable
    dummy <- as.data.frame(prop.table(table(x[which(!is.na(grp))])))
    # "insert" dummy column
    dummy <- dummy[, c(1, 1, 2)]
    # bind sum score
    dummy <- cbind(dummy, c(apply(ftab, 1, function(y) sum(y))))
    names(dummy) <- c("Count", "Group", "Perc", "Sum")
    # "modify" resp. correct the Group-column
    dummy$Group <- as.factor(rep(max(grp, na.rm = TRUE) + 1))
    # bind data to data frame
    mydf <- rbind(mydf, dummy)
  }
  # --------------------------------------------------------
  # Define amount of categories, include zero counts
  # --------------------------------------------------------
  # Zero counts of categories are not plotted by default because
  # these categories don't appear in the data. If we assume a
  # "quasi-continuous" scale (categories from 1 to 4 etc.), we now
  # identify the zero counts and add / insert them into the data frame.
  # This enables us to plot zero counts as well.
  # We guess the maximum amount of categories either by the amount
  # of supplied category labels. If no category labels were passed
  # as argument, we assume that the maximum value found in the category
  # columns represents the highest category number
  # -----------------------------------------------
  # Handle zero-counts in group-variable
  # only possible if we know the exact number of groups,
  # by passing legend labels
  # -----------------------------------------------
  if (grplen != grpcount) {
    # if the maximum value of the group variable differs from the estimated
    # group length we probably have missing categories, i.e. one group has no
    # cases. Then, we insert an empty row here
    mydf$Group <- sjmisc::to_value(mydf$Group, keep.labels = F)
    # range of groups from lowest to highest group value
    allgroups <- factor(c(min(mydf$Group):max(mydf$Group)))
    # retrieve zero-counts, i.e. which group is missing in the data frame
    miss <- sjmisc::to_value(allgroups[!allgroups %in% mydf$Group], keep.labels = F)
    # retrieve subset of all rows where group is from lowest group-value to 
    # missing group. Column 2 is the group-column
    dummy1 <- mydf[apply(mydf, 1, function(y) all(y[2] < miss)), ]
    # retrieve subset of all rows where group is from missing group to
    # highest group-value. Column 2 is the group-column
    dummy2 <- mydf[apply(mydf, 1, function(y) all(y[2] > miss)), ]
    # create dummy-data frame that contains the missing row with zero-values
    emptyrows <- data.frame(Count = c(1:countlen), 
                            Group = miss, 
                            Perc = 0.00, 
                            Sum = 0)
    emptyrows$Count <- as.factor(as.character(emptyrows$Count))
    emptyrows$Group <- as.factor(as.character(emptyrows$Group))
    # bind all three subsets together to a complete data frame
    mydf <- rbind(dummy1, emptyrows, dummy2)
  }
  # set group-variable as factor
  mydf$Group <- as.factor(mydf$Group)
  # -----------------------------------------------
  # Handle zero-counts in count-variable
  # only possible if we know the exact number of categories,
  # by passing category labels
  # -----------------------------------------------
  if (countlen != catcount) {
    # separate data frame for grouping variable. we need this to
    # determine the number of groups
    dfgrp <- as.data.frame(table(mydf$Group))
    # determine the number of groups
    gcnt <- nrow(dfgrp)
    mydat <- NULL
    # fill in possible zero counts in each group
    for (i in 1:gcnt) {
      # get subset of data frame with each group
      subdf <- mydf[mydf$Group == dfgrp$Var1[i], ]
      # convert factors to numeric (due to calculations they have
      # to be treated like that)
      subdf$Count <- sjmisc::to_value(subdf$Count, keep.labels = F)
      subdf$Group <- sjmisc::to_value(subdf$Group, keep.labels = F)
      # Create a vector of zeros 
      frq <- rep(0, catcount)
      sm <- rep(0, catcount)
      gp <- rep(dfgrp$Var1[i], catcount)
      # Replace the values in freq for those indices which equal dummyf$xa
      # by dummyf$ya so that remaining indices are ones which you 
      # intended to insert 
      frq[subdf$Count] <- subdf$Perc
      sm[subdf$Count] <- subdf$Sum
      # create new data frame. We now have a data frame with all
      # variable categories abd their related counts, including
      # zero counts, but no(!) missings!
      dummydat <- data.frame(Count = 1:catcount, 
                             Group = gp, 
                             Perc = frq, 
                             Sum = sm)
      # append dummy data frame to final data frame
      mydat <- as.data.frame(rbind(mydat, dummydat))
    }
    # copy final data frame
    mydf <- mydat
  }
  # ----------------------------
  # make sure group and count variable 
  # are factor values
  # ----------------------------
  mydf$Count <- as.factor(mydf$Count)
  mydf$Group <- as.factor(mydf$Group)
  # add half of Percentage values as new y-position for stacked bars
  mydf <- mydf %>% 
    dplyr::group_by(Count) %>% 
    dplyr::mutate(ypos = cumsum(Perc) - 0.5 * Perc) %>% 
    dplyr::arrange(Count)
  # add line-break char
  if (showPercentageValues && showCountValues) {
    mydf$line.break <- ifelse(coord.flip == TRUE, ' ', '\n')
  } else {
    mydf$line.break <- ""
  }
  if (barPosition == "dodge") mydf$ypos <- mydf$Perc
  # --------------------------------------------------------
  # Caculate vertical adjustment to avoid overlapping labels
  # --------------------------------------------------------
  jvert <- rep(c(1.1, -0.1), length.out = length(unique(mydf$Group)))
  jvert <- rep(jvert, length.out = nrow(mydf))
  # ----------------------------
  # create expression with model summarys. used
  # for plotting in the diagram later
  # ----------------------------
  if (showTableSummary) {
    modsum <- crosstabsum(ftab)
  } else {
    modsum <- NULL
  }
  # --------------------------------------------------------
  # Prepare and trim legend labels to appropriate size
  # --------------------------------------------------------
  # Check whether we have any labels passed as argument
  if (is.null(legendLabels)) {
    # if not, use category text of group variable as legend text
    if (!showTotalColumn) {
      legendLabels <- c(order(unique(mydf$Group)))
    } else {
      # in case we have the total column added,
      # we need to remove the last group-label (which
      # indicates the total column), because the string for
      # the total column is added below
      ll <- c(order(unique(mydf$Group)))
      legendLabels <- ll[-length(ll)]
    }
  }
  legendLabels <- c(legendLabels, stringTotal)
  # wrap legend text lines
  legendLabels <- sjmisc::word_wrap(legendLabels, breakLegendLabelsAt)
  # check whether we have a title for the legend
  if (!is.null(legendTitle)) {
    # if yes, wrap legend title line
    legendTitle <- sjmisc::word_wrap(legendTitle, breakLegendTitleAt)
  }
  # --------------------------------------------------------
  # Trim labels and title to appropriate size
  # --------------------------------------------------------
  # check length of diagram title and split longer string at into new lines
  # every 50 chars
  if (!is.null(title)) {
    # if we have weighted values, say that in diagram's title
    if (!is.null(weightByTitleString)) {
      title <- paste(title, weightByTitleString, sep = "")
    }
    title <- sjmisc::word_wrap(title, breakTitleAt)
  }
  # check length of x-axis-labels and split longer strings at into new lines
  # every 10 chars, so labels don't overlap
  if (!is.null(axisLabels.x)) {
    axisLabels.x <- sjmisc::word_wrap(axisLabels.x, breakLabelsAt)
  } else  {
    # If axisLabels.x were not defined, simply set numbers from 1 to
    # amount of categories (=number of rows) in dataframe instead
    axisLabels.x <- c(1:catcount)
  }
  # --------------------------------------------------------
  # check if category-oder on x-axis should be reversed
  # change category label order then
  # --------------------------------------------------------
  if (reverseOrder) axisLabels.x <- rev(axisLabels.x)
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
    upper_lim <- 1
  } else {
    # else calculate upper y-axis-range depending
    # on the amount of max. answers per category
    upper_lim <- max(((100 * mydf$Perc) + 10) / 100)
    if (upper_lim > 1) upper_lim <- 1
  }
  # --------------------------------------------------------
  # define vertical position for labels
  # --------------------------------------------------------
  if (coord.flip) {
    # if we flip coordinates, we have to use other arguments
    # than for the default layout
    vert <- 0.35
    if (labelPos == "outside" || labelPos == "o")
      hpos = -0.2
    else if (labelPos == "inside" || labelPos == "i")
      hpos = 1.2
    else
      hpos <- ggplot2::waiver()
    hort <- ifelse(barPosition == "dodge", hpos, ggplot2::waiver())
  } else {
    hort <- ggplot2::waiver()
    if (labelPos == "outside" || labelPos == "o")
      vpos = -0.4
    else if (labelPos == "inside" || labelPos == "i")
      vpos = 1.2
    else
      vpos <- ggplot2::waiver()
    vert <- ifelse(barPosition == "dodge", vpos, ggplot2::waiver())
  }
  # check for jitter value labels
  if (jitterValueLabels) vert <- jvert
  # align dodged position of labels to bar positions
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
        ggvaluelabels <- geom_text(aes(y = ypos, label = sprintf("%.01f%%%s(n=%i)", 100 * Perc, line.break, Sum)),
                                   position = position_dodge(posdodge),
                                   vjust = vert,
                                   hjust = hort)
      } else if (showPercentageValues) {
        ggvaluelabels <- geom_text(aes(y = ypos, label = sprintf("%.01f%%", 100 * Perc)),
                                   position = position_dodge(posdodge),
                                   vjust = vert,
                                   hjust = hort)
      } else if (showCountValues) {
        ggvaluelabels <- geom_text(aes(y = ypos, label = sprintf("n=%i", Sum)),
                                   position = position_dodge(posdodge),
                                   vjust = vert,
                                   hjust = hort)
      }
    } else {
      if (showPercentageValues && showCountValues) {
        ggvaluelabels <- geom_text(aes(y = ypos, label = sprintf("%.01f%%%s(n=%i)", 100 * Perc, line.break, Sum)),
                                   vjust = vert,
                                   hjust = hort)
      } else if (showPercentageValues) {
        ggvaluelabels <- geom_text(aes(y = ypos, label = sprintf("%.01f%%", 100 * Perc)),
                                   vjust = vert,
                                   hjust = hort)
      } else if (showCountValues) {
        ggvaluelabels <- geom_text(aes(y = ypos, label = sprintf("n=%i", Sum)),
                                   vjust = vert,
                                   hjust = hort)
      }
    }
  } else {
    ggvaluelabels <- geom_text(label = "")
  }
  # --------------------------------------------------------
  # Set up grid breaks
  # --------------------------------------------------------
  if (is.null(gridBreaksAt)) {
    gridbreaks <- ggplot2::waiver()
  } else {
    gridbreaks <- c(seq(0, upper_lim, by = gridBreaksAt))
  }
  # ----------------------------------
  # construct final plot, base constructor
  # first, set x scale
  # ----------------------------------
  scalex <- scale_x_discrete(labels = axisLabels.x)
  # --------------------------------------------------------
  # check if category-oder on x-axis should be reversed
  # change x axis order then
  # --------------------------------------------------------
  if (reverseOrder) mydf$Count <- rev(mydf$Count)
  # ----------------------------------
  # check whether bars or lines should be printed
  # ----------------------------------
  if (type == "bars") {
    if (barPosition == "dodge") {
      geob <- geom_bar(stat = "identity", 
                       position = position_dodge(geom.size + geom.spacing), 
                       width = geom.size)
    } else {
      geob <- geom_bar(stat = "identity",
                       position = "stack", 
                       width = geom.size)
    }
  # check if we have lines
  } else if (type == "lines") {
    line.stat <- ifelse(smoothLines == TRUE, "smooth", "identity")
    geob <- geom_line(aes(x = as.numeric(Count),
                          y = Perc,
                          colour = Group),
                      data = mydf,
                      size = geom.size, 
                      stat = line.stat)
  }
  # --------------------------------------------------------
  # start plot here
  # --------------------------------------------------------
  baseplot <- ggplot(mydf, aes(x = Count, 
                               y = Perc, 
                               fill = Group)) + geob
  # if we have line diagram, print lines here
  if (type == "lines") {
    baseplot <- baseplot + 
      geom_point(size = lineDotSize, 
                 shape = 21, 
                 show_guide = FALSE)
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
                       limits = c(0, upper_lim), 
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
