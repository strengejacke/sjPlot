# bind global variables
if(getRversion() >= "2.15.1") utils::globalVariables(c("Perc", "Sum", "Count", "Group", "line.break"))

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
#' @param var The variable which proportions (percentage values) should be plotted. The percentage proportions
#'          (within table row, table column or complete table, see parameter \code{tableIndex} of this variable) 
#'          are plotted along the y-axis, the variable's categories on the x-axis.
#' @param grp The grouping variable, where each value represents a single bar chart 
#'          within each category of \code{var}.
#' @param weightBy A weight factor that will be applied to weight all cases from \code{var}.
#'          Must be a vector of same length as \code{var}. Default is \code{NULL}, so no weights are used.
#' @param weightByTitleString If a weight factor is supplied via the parameter \code{weightBy}, the diagram's title
#'          may indicate this with a remark. Default is \code{NULL}, so the diagram's title will not be modified when
#'          cases are weighted. Use a string as parameter, e.g.: \code{weightByTitleString=" (weighted)"}.
#' @param type The plot type. may be either \code{"b"}, \code{"bar"}, \code{"bars"} (default) for bar charts,
#'          or \code{"l"}, \code{"line"}, \code{"lines"} for line diagram.
#' @param tableIndex Indicates which data from the proportional table should be plotted. Use \code{"row"} for
#'          calculating row percentages, \code{"col"} for column percentages and \code{"cell"} for cell percentages.
#'          Only when \code{tableIndex} is \code{"col"}, an additional bar chart with the total sum of each column (i.e.
#'          of each category on the x-axis) can be added with the parameter \code{showTotalColumn}.
#' @param barPosition Indicates whether bars should be positioned side-by-side (default, or use \code{"dodge"} as
#'          parameter) or stacked (use \code{"stack"} as parameter).
#' @param hideLegend Indicates whether legend (guide) should be shown or not. Default is \code{FALSE}, thus
#'          the legend is shown.
#' @param reverseOrder Whether the categories along the x-axis should apper in reversed order or not.
#' @param axisLimits.y A numeric vector of length two, defining lower and upper axis limits
#'          of the y scale. By default, this parameter is set to \code{NULL}, i.e. the 
#'          y-axis ranges from 0 to required maximum. Note that the values are percentages, so valid
#'          range is between 0 and 1.
#' @param title Title of the diagram, plotted above the whole diagram panel.
#'          Use \code{NULL} to automatically detect variable names that will be used as title
#'          (see \code{\link[sjmisc]{set_var_labels}}) for details).
#' @param legendTitle Title of the diagram's legend.
#' @param axisLabels.x Labels for the x-axis breaks.
#' @param legendLabels Labels for the guide/legend.
#' @param geom.colors User defined color palette for geoms. If specified, must either be vector with color values 
#'          of same length as groups defined in \code{x}, or a specific color palette code (see below).
#'          \itemize{
#'            \item If not specified, the qualitative \code{"Paired"} color brewer palette will be used.
#'            \item If \code{"gs"}, a greyscale will be used.
#'            \item If \code{geom.colors} is any valid color brewer palette name, the related \href{http://colorbrewer2.org}{color brewer} palette will be used. Use \code{display.brewer.all()} from the \code{RColorBrewer} package to view all available palette names.
#'            \item Else specify your own color values as vector (e.g. \code{geom.colors = c("#f00000", "#00ff00")}).
#'          }
#' @param geom.size size resp. width of the geoms (bar width).
#' @param geom.spacing the spacing between geoms (i.e. bar spacing)
#' @param breakTitleAt Wordwrap for diagram title. Determines how many chars of the title are displayed in
#'          one line and when a line break is inserted into the title.
#' @param breakLabelsAt Wordwrap for diagram labels. Determines how many chars of the category labels are displayed in 
#'          one line and when a line break is inserted.
#' @param breakLegendTitleAt Wordwrap for diagram legend title. Determines how many chars of the legend's title 
#'          are displayed in one line and when a line break is inserted.
#' @param breakLegendLabelsAt Wordwrap for diagram legend labels. Determines how many chars of the legend labels are 
#'          displayed in one line and when a line break is inserted.
#' @param gridBreaksAt Sets the breaks on the y axis, i.e. at every n'th position a major
#'          grid is being printed. Valid values range from 0 to 1.
#' @param lineDotSize Size of dots. Only applies, when parameter \code{type}
#'          is set to \code{"lines"}.
#' @param smoothLines Prints a smooth line curve. Only applies, when parameter \code{type}
#'          is set to \code{"lines"}.
#' @param expand.grid If \code{TRUE}, the plot grid is expanded, i.e. there is a small margin between
#'          axes and plotting region. Default is \code{FALSE}.
#' @param showValueLabels Whether counts and percentage values should be plotted to each bar
#' @param showCountValues If \code{TRUE} (default), count values are be plotted to each bar. If \code{FALSE},
#'          count values are removed.
#' @param showPercentageValues If \code{TRUE} (default), percentage values are be plotted to each bar, if \code{FALSE},
#'          percentage-values are removed.
#' @param jitterValueLabels If \code{TRUE}, the value labels on the bars will be "jittered", i.e. they have
#'          alternating vertical positions to avoid overlapping of labels in case bars are
#'          very short. Default is \code{FALSE}.
#' @param labelPos Positioning of value labels. If \code{barPosition} is \code{"dodge"} 
#'          (default), use either \code{"inside"} or \code{"outside"} (default) to put labels in-
#'          or outside the bars. You may specify initial letter only. Use \code{"center"} 
#'          to center labels (useful if label angle is changes via \code{\link{sjp.setTheme}}).
#' @param stringTotal The string for the legend label when a total-column is added. Only applies
#'          if \code{showTotalColumn} is \code{TRUE}. Default is \code{"Total"}.
#' @param showCategoryLabels Whether x axis text (category names) should be shown or not.
#' @param showTableSummary If \code{TRUE} (default), a summary of the cross tabulation with N, Chi-square (see \code{\link{chisq.test}}),
#'          df, Cramer's V or Phi-value and p-value is printed to the upper right corner of the diagram. If a cell contains expected 
#'          values lower than five (or lower than 10 if df is 1),
#'          the Fisher's excact test (see \code{\link{fisher.test}}) is computed instead of Chi-square test. 
#'          If the table's matrix is larger than 2x2, Fisher's excact test with Monte Carlo simulation is computed.
#'          Only applies to bar-charts or dot-plots, i.e. when parameter \code{type} is either \code{"bars"} or \code{"dots"}.
#' @param tableSummaryPos Position of the model summary which is printed when \code{showTableSummary} is \code{TRUE}. Default is
#'          \code{"r"}, i.e. it's printed to the upper right corner. Use \code{"l"} for upper left corner.
#' @param showTotalColumn if \code{tableIndex} is \code{"col"}, an additional bar chart with the sum within each category and
#'          it's percentages will be added to each category.
#' @param axisTitle.x A label for the x axis. useful when plotting histograms with metric scales where no category labels
#'          are assigned to the x axis.
#'          Use \code{NULL} to automatically detect variable names that will be used as title
#'          (see \code{\link[sjmisc]{set_var_labels}}) for details).
#' @param axisTitle.y A label for the y axis. useful when plotting histograms with metric scales where no category labels
#'          are assigned to the y axis.
#' @param coord.flip If \code{TRUE}, the x and y axis are swapped.
#' @param printPlot If \code{TRUE} (default), plots the results as graph. Use \code{FALSE} if you don't
#'          want to plot any graphs. In either case, the ggplot-object will be returned as value.
#' @return (Insisibily) returns the ggplot-object with the complete plot (\code{plot}) as well as the data frame that
#'           was used for setting up the ggplot-object (\code{mydf}).
#' 
#' @examples
#' # create 4-category-items
#' grp <- sample(1:4, 100, replace=TRUE)
#' # create 3-category-items
#' var <- sample(1:3, 100, replace=TRUE)
#' 
#' # plot "cross tablulation" of var and grp
#' sjp.xtab(var, grp)
#' 
#' # plot "cross tablulation" of x and y, including labels
#' sjp.xtab(var, grp, 
#'          axisLabels.x = c("low", "mid", "high"),
#'          legendLabels = c("Grp 1", "Grp 2", "Grp 3", "Grp 4"))
#' 
#' # plot "cross tablulation" of var and grp
#' # as stacked proportional bars
#' sjp.xtab(var, grp, 
#'          tableIndex = "row", 
#'          barPosition = "stack", 
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
#'          showTableSummary = FALSE,
#'          labelPos = "center")
#' 
#' # grouped bars with EUROFAMCARE sample dataset
#' # dataset was importet from an SPSS-file,
#' # see ?sjmisc::read_spss
#' data(efc)
#' efc.val <- get_val_labels(efc)
#' efc.var <- get_var_labels(efc)
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
#' @import dplyr
#' @import sjmisc
#' @importFrom scales percent
#' @export
sjp.xtab <- function(var,
                    grp,
                    title="", 
                    legendTitle=NULL,
                    weightBy=NULL,
                    weightByTitleString=NULL,
                    type="bars",
                    tableIndex="col",
                    reverseOrder=FALSE,
                    axisLimits.y = NULL,
                    axisLabels.x=NULL, 
                    legendLabels=NULL,
                    labelPos="outside",
                    stringTotal="Total",
                    breakTitleAt=50, 
                    breakLabelsAt=15, 
                    breakLegendTitleAt=20, 
                    breakLegendLabelsAt=20,
                    gridBreaksAt=0.2,
                    geom.size=0.7,
                    geom.spacing=0.1,
                    geom.colors="Paired",
                    barPosition="dodge",
                    lineDotSize=3,
                    smoothLines=FALSE,
                    expand.grid=FALSE,
                    showValueLabels=TRUE,
                    jitterValueLabels=FALSE,
                    showCountValues=TRUE,
                    showPercentageValues=TRUE,
                    showCategoryLabels=TRUE,
                    showTableSummary=TRUE,
                    tableSummaryPos="r",
                    showTotalColumn=TRUE,
                    hideLegend=FALSE,
                    axisTitle.x=NULL,
                    axisTitle.y=NULL,
                    coord.flip=FALSE,
                    printPlot=TRUE) {
  # --------------------------------------------------------
  # try to automatically set labels is not passed as parameter
  # --------------------------------------------------------
  if (is.null(axisLabels.x)) axisLabels.x <- sjmisc:::autoSetValueLabels(var)
  if (is.null(legendLabels)) legendLabels <- sjmisc:::autoSetValueLabels(grp)
  if (is.null(axisTitle.x)) axisTitle.x <- sjmisc:::autoSetVariableLabels(var)
  if (is.null(legendTitle)) legendTitle <- sjmisc:::autoSetVariableLabels(grp)  
  if (is.null(title)) {
    t1 <- sjmisc:::autoSetVariableLabels(var)
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
  tindex <- ifelse (tableIndex == "row", 1, 2)
  # --------------------------------------------------------
  # convert factor to numeric
  # --------------------------------------------------------
  if (is.factor(grp)) grp <- sjmisc::to_value(grp, keep.labels = F)
  if (is.factor(var)) var <- sjmisc::to_value(var, keep.labels = F)
  # --------------------------------------------------------
  # We have several options to name the diagram type
  # Here we will reduce it to a unique value
  # --------------------------------------------------------
  if (type == "b" || type == "bar") type <- c("bars")
  if (type == "l" || type == "line") type <- c("lines")
  if (expand.grid == TRUE) {
    expand.grid <- waiver()
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
  grplen <- length(unique(na.omit(grp)))
  countlen <- length(unique(na.omit(var)))
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
    ftab <- table(var, grp)
  } else {
    ftab <- round(xtabs(weightBy ~ var + grp), 0)
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
    dummy <- as.data.frame(prop.table(table(var[which(!is.na(grp))])))
    # "insert" dummy column
    dummy <- dummy[, c(1, 1, 2)]
    # bind sum score
    dummy <- cbind(dummy, c(apply(ftab, 1, function(x) sum(x))))
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
  # as parameter, we assume that the maximum value found in the category
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
    dummy1 <- mydf[apply(mydf, 1, function(x) all(x[2] < miss)), ]
    # retrieve subset of all rows where group is from missing group to
    # highest group-value. Column 2 is the group-column
    dummy2 <- mydf[apply(mydf, 1, function(x) all(x[2] > miss)), ]
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
    mydf$line.break <- ifelse (coord.flip == TRUE, ' ', '\n')
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
  # Check whether we have any labels passed as parameter
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
    # if we flip coordinates, we have to use other parameters
    # than for the default layout
    vert <- 0.35
    if (labelPos == "outside" || labelPos == "o")
      hpos = -0.2
    else if (labelPos == "inside" || labelPos == "i")
      hpos = 1.2
    else
      hpos <- waiver()
    hort <- ifelse (barPosition == "dodge", hpos, waiver())
  } else {
    hort <- waiver()
    if (labelPos == "outside" || labelPos == "o")
      vpos = -0.4
    else if (labelPos == "inside" || labelPos == "i")
      vpos = 1.2
    else
      vpos <- waiver()
    vert <- ifelse (barPosition == "dodge", vpos, waiver())
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
        ggvaluelabels <- geom_text(aes(y = ypos, label=sprintf("n=%i", Sum)),
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
    gridbreaks <- waiver()
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
    # If parameter "axisLabels.x" is NULL, the category numbers (1 to ...) 
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
  invisible (structure(class = "sjpxtab",
                       list(plot = baseplot,
                            mydf = mydf)))
}
