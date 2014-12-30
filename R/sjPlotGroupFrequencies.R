# bind global variables
if(getRversion() >= "2.15.1") utils::globalVariables(c("ypos", "wb", "ia", "mw", "stddev", "count"))


#' @title Plot grouped or stacked frequencies
#' @name sjp.grpfrq
#' 
#' @seealso \itemize{
#'              \item \href{http://www.strengejacke.de/sjPlot/sjp.grpfrq/}{sjPlot manual: sjp.grpfrq}
#'              }
#'             
#' @description Plot grouped or stacked frequencies of variables 
#'                as bar/dor graphs, box or violin plots, histograms etc.
#'                using ggplot. 
#' 
#' @param varCount The variable which frequencies should be plotted. The counts of this variable are along the
#'          y-axis, the variable's categories on the x-axis.
#' @param varGroup the grouping variable, where each value represents a single bar chart within each category of
#'          the \code{varCount} variable.
#' @param weightBy A weight factor that will be applied to weight all cases from \code{varCount}.
#' @param weightByTitleString If a weight factor is supplied via the parameter \code{weightBy}, the diagram's title
#'          may indicate this with a remark. Default is \code{NULL}, so the diagram's title will not be modified when
#'          cases are weighted. Use a string as parameter, e.g.: \code{weightByTitleString=" (weighted)"}.
#' @param interactionVar An interaction variable which can be used for box plots. Divides each category indicated
#'          by \code{varGroup} into the factors of \code{interactionVar}, so that each category of \code{varGroup}
#'          is subgrouped into \code{interactionVar}'s categories. Only applies when parameter \code{type}
#'          is \code{box} or \code{violin} (resp. their alternative strings like \code{"boxplot"}, \code{"boxplots"} or \code{"v"}).
#' @param barPosition Indicates whether bars should be positioned side-by-side (default, or use \code{"dodge"} as
#'          parameter) or stacked (use \code{"stack"} as parameter).
#'          If \code{type} is \code{"histogram"}, you can use either \code{"dodge"} (default value), which displays the bars side-by-side,
#'          or \code{"identity"}, which results in overlaying bars. In the latter case, it's recommended to adjust the 
#'          alpha value (see \code{\link{sjp.setTheme}}).
#' @param type The plot type. May be one of the following:
#'          \itemize{
#'            \item \code{"b"}, \code{"bar"}, \code{"bars"} (default) for bar charts
#'            \item \code{"l"}, \code{"line"}, \code{"lines"} for line diagram
#'            \item \code{"d"}, \code{"dot"}, \code{"dots"} for dot plots
#'            \item \code{"h"}, \code{"hist"}, \code{"histogram"} for grouped histograms
#'            \item \code{"box"}, \code{"boxplot"}, \code{"boxplots"} for box plots
#'            \item \code{"v"}, \code{"violin"} for violin box plots
#'            }
#' @param hideLegend Indicates whether legend (guide) should be shown or not.
#' @param axisLimits.y A numeric vector of length two, defining lower and upper axis limits
#'          of the y scale. By default, this parameter is set to \code{NULL}, i.e. the 
#'          y-axis ranges from 0 to required maximum.
#' @param facet.grid \code{TRUE} when bar charts should be plotted as facet grids instead of integrated single
#'          bar charts. Ideal for larger amount of groups. This parameter wraps a single panel into 
#'          \code{varGroup} amount of panels, i.e. each group is represented within a new panel.
#' @param title Title of the diagram, plotted above the whole diagram panel.
#'          Use \code{NULL} to automatically detect variable names that will be used as title
#'          (see \code{\link{sji.setVariableLabels}}) for details).
#' @param legendTitle Title of the diagram's legend.
#' @param axisLabels.x Labels for the x-axis breaks. Passed as vector of strings. \emph{Note:} This parameter
#'          is not necessary when data was either imported with \code{\link{sji.SPSS}} or has named factor levels 
#'          (see examples below). Else, specifiy parameter like this:
#'          \code{axisLabels.x=c("Label1", "Label2", "Label3")}.
#'          Note: If you use the \code{\link{sji.SPSS}} function and the \code{\link{sji.getValueLabels}} function, you receive a
#'          list object with label string. The labels may also be passed as list object. They will be unlisted and
#'          converted to character vector automatically.
#' @param interactionVarLabels Labels for the x-axis breaks when having interaction variables included.
#'          These labels replace the \code{axisLabels.x}. Only applies, when using box or violin plots
#'          (i.e. \code{"type"} is \code{"box"} or \code{"violin"}) and \code{interactionVar} is not \code{NULL}.
#'          Example: See \code{axisLabels.x}.
#' @param legendLabels Labels for the guide/legend.
#'          Example: See \code{axisLabels.x}.
#' @param breakTitleAt Wordwrap for diagram title. Determines how many chars of the title are displayed in
#'          one line and when a line break is inserted into the title.
#' @param breakLabelsAt Wordwrap for diagram labels. Determines how many chars of the category labels are displayed in 
#'          one line and when a line break is inserted.
#' @param breakLegendTitleAt Wordwrap for diagram legend title. Determines how many chars of the legend's title 
#'          are displayed in one line and when a line break is inserted.
#' @param breakLegendLabelsAt Wordwrap for diagram legend labels. Determines how many chars of the legend labels are 
#'          displayed in one line and when a line break is inserted.
#' @param gridBreaksAt Sets the breaks on the y axis, i.e. at every n'th position a major
#'          grid is being printed.
#' @param innerBoxPlotWidth The width of the inner box plot that is plotted inside of violin plots. Only applies 
#'          if \code{type} is \code{"violin"}. Default value is 0.15
#' @param innerBoxPlotDotSize Size of mean dot insie a violin or box plot. Applies only when \code{type} is set to 
#'          \code{"violin"} or \code{"box"}.
#' @param geom.colors User defined color palette for geoms. If specified, must either be vector with color values 
#'          of same length as groups defined in \code{varGroup}, or a specific color palette code (see below).
#'          \itemize{
#'            \item If not specified, the diverging \code{"Paired"} color brewer palette will be used.
#'            \item If \code{"gs"}, a greyscale will be used.
#'            \item If \code{geom.colors} is any valid color brewer palette name, the related \href{http://colorbrewer2.org}{color brewer} palette will be used. Use \code{display.brewer.all()} from the \code{RColorBrewer} package to view all available palette names.
#'          }
#'          Else specify your own color values as vector (e.g. \code{geom.colors=c("#f00000", "#00ff00", "#0080ff")}).
#' @param geom.size size resp. width of the geoms (bar width or point size, depending on \code{type} parameter).
#' @param geom.spacing the spacing between geoms (i.e. bar spacing)
#' @param smoothLines Prints a smooth line curve. Only applies, when parameter \code{type}
#'          is set to \code{"lines"}.
#' @param expand.grid If \code{TRUE}, the plot grid is expanded, i.e. there is a small margin between
#'          axes and plotting region. Default is \code{FALSE}.
#' @param showValueLabels Whether counts and percentage values should be plotted to each bar. Default
#'          is \code{TRUE}.
#' @param showCountValues If \code{TRUE} (default), count values are be plotted to each bar. If \code{FALSE},
#'          count values are removed.
#' @param showPercentageValues If \code{TRUE} (default), percentage values are be plotted to each bar, if \code{FALSE},
#'          percentage-values are removed.
#' @param showAxisLabels.x Whether x axis labels (category names) should be shown or not.
#' @param showAxisLabels.y Whether y axis labels (count values) should be shown or not.
#' @param showPlotAnnotation If \code{TRUE}, the groups of dots in a dot-plot are highlighted with a shaded rectangle.
#' @param showMeanIntercept if \code{TRUE}, a vertical line in histograms is drawn to indicate the mean value of the count
#'          variables. Only applies to histogram-charts.
#' @param showMeanValue If \code{TRUE} (default value), the mean value is printed to the vertical line that indicates the mean value
#'          of the count variables. Only applies to histogram-charts.
#' @param showStandardDeviation If \code{TRUE}, the standard deviation is annotated as shaded rectangle around the mean intercept
#'          line. Only applies to histogram-charts. The shaded rectangles have borders in the group colors, so it's easier to see
#'          which shaded area belongs to which mean value resp. group
#' @param showTableSummary If \code{TRUE} (default), a summary of the cross tabulation with N, Chi-square (see \code{\link{chisq.test}}),
#'          df, Cramer's V or Phi-value and p-value is printed to the upper right corner of the diagram. If a cell contains expected 
#'          values lower than five (or lower than 10 if df is 1),
#'          the Fisher's excact test (see \code{\link{fisher.test}}) is computed instead of Chi-square test. 
#'          If the table's matrix is larger than 2x2, Fisher's excact test with Monte Carlo simulation is computed.
#'          Only applies to bar-charts or dot-plots, i.e. when parameter \code{type} is either \code{"bars"} or \code{"dots"}.
#' @param showGroupCount if \code{TRUE}, the count within each group is added to the category labels (e.g. \code{"Cat 1 (n=87)"}).
#'          Default value is \code{FALSE}.
#' @param tableSummaryPos Position of the model summary which is printed when \code{showTableSummary} is \code{TRUE}. Default is
#'          \code{"r"}, i.e. it's printed to the upper right corner. Use \code{"l"} for upper left corner.
#' @param meanInterceptLineType The linetype of the mean intercept line. Only applies to histogram-charts and when
#'          \code{showMeanIntercept} is \code{TRUE}.
#' @param meanInterceptLineSize The size of the mean intercept line. Only applies to histogram-charts and when
#'          \code{showMeanIntercept} is \code{TRUE}.
#' @param axisTitle.x A label for the x axis. Useful when plotting histograms with metric scales where no category labels
#'          are assigned to the x axis. By default, \code{""} is used, i.e. no title
#'          is printed.
#'          Use \code{NULL} to automatically detect variable names that will be used as title
#'          (see \code{\link{sji.setVariableLabels}}) for details).
#' @param axisTitle.y A label for the y axis. Useful when plotting histograms with metric scales where no category labels
#'          are assigned to the y axis. By default, \code{""} is used, i.e. no title
#'          is printed.
#'          Use \code{NULL} to automatically detect variable names that will be used as title
#'          (see \code{\link{sji.setVariableLabels}}) for details).
#' @param autoGroupAt A value indicating at which length of unique values of \code{varCount} the variable
#'          is automatically grouped into smaller units (see \code{\link{sju.groupVar}}). If \code{varCount} has large 
#'          numbers of unique values, too many bars for the graph have to be plotted. Hence it's recommended 
#'          to group such variables. For example, if \code{autoGroupAt} is 50, i.e. if \code{varCount} has 50 and more unique values 
#'          it will be grouped using \code{\link{sju.groupVar}} with \code{groupsize="auto"} parameter. By default, 
#'          the maximum group count is 30. However, if \code{autoGroupAt} is less than 30, \code{autoGroupAt} 
#'          groups are built. Default value for \code{autoGroupAt} is \code{NULL}, i.e. auto-grouping is off.
#' @param startAxisAt Determines the first value on the x-axis. By default, this value is set
#'          to \code{"auto"}, i.e. the value range on the x axis starts with the lowest value of \code{varCount}.
#'          If you set \code{startAxisAt} to 1, you may have zero counts if the lowest value of \code{varCount}
#'          is larger than 1 and hence no bars plotted for these values in such cases.
#' @param coord.flip If \code{TRUE}, the x and y axis are swapped.
#' @param labelPos Positioning of value labels. If \code{coord.flip} is \code{TRUE}, use 
#'          either \code{"inside"} or \code{"outside"} (default) to place labels inside or
#'          outside of bars. You may specify initial letter only. If \code{coord.flip} is \code{FALSE}, 
#'          use \code{"center"} to center labels (useful if label angle is changes via \code{\link{sjp.setTheme}}).
#' @param na.rm If \code{TRUE}, missings are not included in the frequency calculation and diagram plot.
#' @param printPlot If \code{TRUE} (default), plots the results as graph. Use \code{FALSE} if you don't
#'          want to plot any graphs. In either case, the ggplot-object will be returned as value.
#' @return (Insisibily) returns the ggplot-object with the complete plot (\code{plot}) as well as the data frame that
#'           was used for setting up the ggplot-object (\code{df}).
#' 
#' @examples
#' # histrogram with EUROFAMCARE sample dataset
#' data(efc)
#' efc.val <- sji.getValueLabels(efc)
#' efc.var <- sji.getVariableLabels(efc)
#' sjp.grpfrq(efc$e17age,
#'            efc$e16sex,
#'            title = efc.var['e17age'],
#'            legendTitle = efc.var['e16sex'],
#'            type = "hist",
#'            showValueLabels = FALSE,
#'            showMeanIntercept = TRUE)
#' 
#' # boxplot
#' sjp.grpfrq(efc$e17age, 
#'            efc$e42dep, 
#'            type = "box")
#' 
#' # -------------------------------------------------
#' # auto-detection of value labels and variable names
#' # -------------------------------------------------
#' efc.var <- sji.getVariableLabels(efc)
#' efc <- sji.setVariableLabels(efc, efc.var)
#' # grouped bars using necessary y-limit            
#' sjp.grpfrq(efc$e42dep, 
#'            efc$e16sex, 
#'            title = NULL)
#'
#' # box plots with interaction variable            
#' sjp.grpfrq(efc$e17age,
#'            efc$e42dep,
#'            interactionVar = efc$e16sex,
#'            title = paste(efc.var['e17age'], 
#'                          "by", 
#'                          efc.var['e42dep'], 
#'                          "and", 
#'                          efc.var['e16sex']),
#'            axisLabels.x = efc.val[['e17age']],
#'            interactionVarLabels = efc.val[['e16sex']],
#'            legendTitle = efc.var['e42dep'],
#'            legendLabels = efc.val[['e42dep']],
#'            type = "box")
#' 
#' # Grouped bar plot ranging from 1 to 28 (though scale starts with 7)
#' sjp.grpfrq(efc$neg_c_7, 
#'            efc$e42dep, 
#'            showValueLabels = FALSE, 
#'            startAxisAt = 1)
#' # Same grouped bar plot ranging from 7 to 28
#' sjp.grpfrq(efc$neg_c_7, 
#'            efc$e42dep, 
#'            showValueLabels = FALSE)
#' 
#' @import ggplot2
#' @importFrom plyr ddply
#' @importFrom MASS loglm
#' @export
sjp.grpfrq <- function(varCount,
                       varGroup,
                       weightBy=NULL,
                       weightByTitleString=NULL,
                       interactionVar=NULL,
                       type="bars",
                       geom.size=0.6,
                       geom.spacing=0.4,
                       geom.colors="Paired",
                       hideLegend=FALSE,
                       facet.grid=FALSE,
                       title="", 
                       legendTitle=NULL,
                       axisLabels.x=NULL, 
                       interactionVarLabels=NULL,
                       legendLabels=NULL,
                       axisLimits.y = NULL,
                       breakTitleAt=50, 
                       breakLabelsAt=15, 
                       breakLegendTitleAt=20, 
                       breakLegendLabelsAt=20,
                       gridBreaksAt=NULL,
                       barPosition="dodge",
                       innerBoxPlotWidth=0.15,
                       innerBoxPlotDotSize=3,
                       smoothLines=FALSE,
                       expand.grid=FALSE,
                       showValueLabels=TRUE,
                       showCountValues=TRUE,
                       showPercentageValues=TRUE,
                       showAxisLabels.x=TRUE,
                       showAxisLabels.y=TRUE,
                       showPlotAnnotation=TRUE,
                       showMeanIntercept=FALSE,
                       showMeanValue=TRUE,
                       showStandardDeviation=FALSE,
                       showTableSummary=TRUE,
                       showGroupCount=FALSE,
                       tableSummaryPos="r",
                       meanInterceptLineType=2,
                       meanInterceptLineSize=0.5,
                       axisTitle.x="",
                       axisTitle.y="",
                       autoGroupAt=NULL,
                       startAxisAt="auto",
                       coord.flip=FALSE,
                       labelPos="outside",
                       na.rm=TRUE,
                       printPlot=TRUE) {
  # --------------------------------------------------------
  # We have several options to name the diagram type
  # Here we will reduce it to a unique value
  # --------------------------------------------------------
  if (type=="b" || type=="bar") {
    type <- c("bars")
  }
  if (type=="l" || type=="line") {
    type <- c("lines")
  }
  if (type=="d" || type=="dot") {
    type <- c("dots")
  }
  if (type=="h" || type=="hist") {
    type <- c("histogram")
    # no table summary and no group count for
    # ctageory labels (to avoid overlapping)
    showTableSummary <- FALSE
    showGroupCount <- FALSE
  }
  if (type=="box" || type=="boxplot") {
    type <- c("boxplots")
  }
  if (type=="v") {
    type <- c("violin")
  }
  if (expand.grid==TRUE) {
    expand.grid <- waiver()
  }
  else {
    expand.grid <- c(0,0)
  }
  # --------------------------------------------------------
  # try to automatically set labels is not passed as parameter
  # --------------------------------------------------------
  if (is.null(axisLabels.x)) {
    axisLabels.x <- autoSetValueLabels(varCount)
    # if we have box or violin plots, but no axis labels on x axis (NULL),
    # we need to hide x-axis, because automatically retrieved labels are
    # equal to unique values of varCount, and not varGroup.
    if (type=="boxplots" || type=="violin") showAxisLabels.x <- FALSE
  }
  if (is.null(legendLabels)) legendLabels <- autoSetValueLabels(varGroup)
  if (is.null(interactionVarLabels) && !is.null(interactionVar)) interactionVarLabels <- autoSetValueLabels(interactionVar)
  if (is.null(axisTitle.x)) axisTitle.x <- autoSetVariableLabels(varCount)
  if (is.null(legendTitle)) legendTitle <- autoSetVariableLabels(varGroup)  
  if (is.null(title)) {
    t1 <- autoSetVariableLabels(varCount)
    t2 <- autoSetVariableLabels(varGroup)
    if (!is.null(t1) && !is.null(t2)) {
      title <- paste0(t1, " by ", t2)
    }
  }
  # --------------------------------------------------------
  # remove titles if empty
  # --------------------------------------------------------
  if (!is.null(legendTitle) && legendTitle=="") legendTitle <- NULL
  if (!is.null(axisTitle.x) && axisTitle.x=="") axisTitle.x <- NULL
  if (!is.null(axisTitle.y) && axisTitle.y=="") axisTitle.y <- NULL  
  if (!is.null(title) && title=="") title <- NULL    
  # --------------------------------------------------------
  # count variable may not be a factor!
  # --------------------------------------------------------
  varCount <- as.numeric(varCount)
  varGroup <- as.numeric(varGroup)
  #---------------------------------------------------
  # check whether variable should be auto-grouped
  #---------------------------------------------------
  if (!is.null(autoGroupAt) && length(unique(varCount))>=autoGroupAt) {
    message(sprintf("Variable has %i unique values and was grouped...", length(unique(varCount))))
    agcnt <- ifelse (autoGroupAt<30, autoGroupAt, 30)
    axisLabels.x <- sju.groupVarLabels(varCount, groupsize="auto", autoGroupCount=agcnt)
    varCount <- sju.groupVar(varCount, groupsize="auto", asNumeric=TRUE, autoGroupCount=agcnt)
  }
  # --------------------------------------------------------
  # unlist labels
  # --------------------------------------------------------
  if (!is.null(legendLabels) && is.list(legendLabels)) {
    legendLabels <- unlistlabels(legendLabels)
  }
  if (!is.null(axisLabels.x) && is.list(axisLabels.x)) {
    axisLabels.x <- unlistlabels(axisLabels.x)
  }
  if (!is.null(interactionVarLabels) && is.list(interactionVarLabels)) {
    interactionVarLabels <- unlistlabels(interactionVarLabels)
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
  # handle zero-counts
  # -----------------------------------------------
  # Determine length of count and group var
  grplen <- length(unique(na.omit(varGroup)))
  # determine maximum values
  # first, check the total amount of different factor levels
  catcount_1 <- length(unique(na.omit(varCount)))
  # second, check the maximum factor level
  catcount_2 <- max(varCount, na.rm=TRUE)
  # if categories start with zero, fix this here
  if (min(varCount, na.rm=TRUE)==0) {
    catcount_2 <- catcount_2+1
  }
  # catcount should contain the higher values, i.e. the maximum count of
  # categories (factor levels) corresponds either to the highest factor level
  # value or to the amount of different factor levels, depending on which one
  # is larger
  catcount <- ifelse (catcount_1 > catcount_2, catcount_1, catcount_2)
  catmin <- min(varCount, na.rm=TRUE)
  # ----------------------------------------------
  # check for axis start, depending on lowest value
  # ----------------------------------------------
  if (startAxisAt=="auto") {
    startAxisAt <- as.numeric(catmin)
    if (startAxisAt==0) startAxisAt <- 1
  }
  # get the highest answer category of "variable", so we know where the
  # range of the x-axis ends
  if (!is.null(axisLabels.x)) {
    catcount <- length(axisLabels.x)
  }
  # if we have legend labels, we know the exact
  # amount of groups
  if (is.null(legendLabels)) {
    grpcount <- grplen
  }
  else {
    grpcount <- length(legendLabels)
  }
  # -----------------------------------------------
  # create cross table for stats, summary etc.
  # and weight variable
  #---------------------------------------------------
  if (is.null(weightBy)) {
    ftab <- table(varCount, varGroup)
  }
  else {
    ftab <- round(xtabs(weightBy ~ varCount + varGroup),0)
  }
  # new data frame from variables
  df <- as.data.frame(ftab)
  # separate data frame for grouping variable. we need this to
  # determine the number of groups
  dfgrp <- as.data.frame(table(df$varGroup))
  # Factors have to be transformed into numeric values
  # for continiuos x-axis-scale
  df$varCount <- as.numeric(as.character(df$varCount))
  # if categories start with zero, fix this here
  if (min(df$varCount)==0) {
    df$varCount<- df$varCount+1
  }
  # convcert group variables to chars
  df$varGroup <- as.character(df$varGroup)
  dfgrp$Var1 <- as.character(dfgrp$Var1)
  # determine the number of groups
  grpcnt <- nrow(dfgrp)
  # init data frame. this data frame will contain the final variables
  # needed for plotting with ggplot
  mydat <- NULL
  # fill in possible zero counts in each group
  for (i in 1:grpcnt) {
    # get subset of data frame with each group
    subdf <- df[df$varGroup == dfgrp$Var1[i],]
    # Create a vector of zeros 
    frq <- rep(0,catcount)
    # Replace the values in freq for those indices which equal dummyf$xa
    # by dummyf$ya so that remaining indices are ones which you 
    # intended to insert 
    frq[subdf$varCount] <- subdf$Freq
    # retrieve group variable as character. grouping might be indicated by
    # "A" or "B", not just 1 or 2.
    group <- as.character(dfgrp$Var1[i])
    # create new data frame. We now have a data frame with all
    # variable categories abd their related counts, including
    # zero counts, but no(!) missings!
    dummydat <- as.data.frame(cbind(count = 1:catcount, group, frq, layer=1:catcount))
    # --------------------------------------------------------
    # Handle missings
    # --------------------------------------------------------
    if (!na.rm) {
      # get amount of missings
      frq <- length(which(is.na(varCount[which(varGroup==i)])))
      # create data frame
      tmpdf <- as.data.frame(cbind(count=catcount+1, group, frq, layer=catcount+1))
      # append dummy data frame to final data frame
      dummydat <- as.data.frame(rbind(dummydat, tmpdf))
    }
    # append dummy data frame to final data frame
    mydat <- as.data.frame(rbind(mydat, dummydat))
  }
  # convert grouping variable to character
  mydat$count <- as.numeric(as.character(mydat$count))
  # convert grouping variable to character
  mydat$group <- as.numeric(as.character(mydat$group))
  # convert frequencies to numeric
  mydat$frq <- as.numeric(as.character(mydat$frq))
  # convert layer to numeric
  mydat$layer <- as.numeric(as.character(mydat$layer))
  # -----------------------------------------------
  # Handle zero-counts in group-variable
  # only possible if we know the exact number of groups,
  # by passing legend labels
  # -----------------------------------------------
  if (grplen != grpcount) {
    # if the maximum value of the group variable differs from the estimated
    # group length we probably have missing categoriesm, i.e. one group has no
    # cases. Then, we insert an empty row here
    # range of groups from lowest to highest group value
    allgroups <- factor(c(min(mydat$group):max(mydat$group)))
    # retrieve zero-counts, i.e. which group is missing in the data frame
    miss <- as.numeric(as.character(allgroups[!allgroups %in% mydat$group]))
    # retrieve subset of all rows where group is from lowest group-value to 
    # missing group
    dummy1 <- mydat[apply(mydat, MARGIN=1, function(xy) all(xy[2] < miss)), ]
    # retrieve subset of all rows where group is from missing group to
    # highest group-value
    dummy2 <- mydat[apply(mydat, MARGIN=1, function(xy) all(xy[2] > miss)), ]
    # create dummy-data frame that contains the missing row with zero-values
    emptyrows <- as.data.frame(cbind(count=c(1:catcount), group=miss, frq=0, layer=1:catcount))
    emptyrows$count <- as.factor(as.character(emptyrows$count))
    emptyrows$group <- as.factor(as.character(emptyrows$group))
    # bind all three subsets together to a complete data frame
    mydat <- rbind(dummy1, emptyrows, dummy2)
  }
  # set group-variable as factor
  mydat$group <- as.factor(mydat$group)
  # --------------------------------------------------------
  # calculate percentages
  # --------------------------------------------------------
  # init empty vector, with length of data frame's rows
  prz <- rep(0,nrow(mydat))
  # iterate all data frame rows
  for (k in 1: length(prz)) {
    # if we have facet grids, calculate percentage
    # within each group
    if (facet.grid) {
      # get frequency value of each row and divide it by the sum of frequencies of
      # all frequencies of the current row's group
      prz[k] <- c(round(100*mydat[k,3]/sum(mydat$frq[mydat$group==mydat[k,2]]),2))
    }
    # if we have dodged/stacked bars or plots, calculate percentage
    # within each category
    else {
      # get frequency value of each row and divide it by the sum of frequencies of
      # all frequencies of the current row's category
      prz[k] <- c(round(100*mydat[k,3]/sum(mydat$frq[mydat$count==mydat[k,1]]),2))
    } 
  }
  # bind percentage as final column
  mydat <- as.data.frame(cbind(mydat, prz))
  # convert prz to numeric
  mydat$texty <- as.numeric(as.character(mydat$prz))
  # add half of Percentage values as new y-position for stacked bars
  mydat <- ddply(mydat, "count", transform, ypos = cumsum(frq) - 0.5*frq)
  # --------------------------------------------------------
  # If we have boxplots, use different data frame structure
  # --------------------------------------------------------
  if (type=="boxplots" || type=="violin") {
    w <- ifelse(is.null(weightBy), 1, weightBy)
    if (is.null(interactionVar)) {
      mydat <- na.omit(data.frame(cbind(group=varGroup, frq=varCount, wb=w)))
    }
    else {
      mydat <- na.omit(data.frame(cbind(group=varGroup, frq=varCount, ia=interactionVar, wb=w)))
      mydat$ia <- as.factor(mydat$ia)
    }
    mydat$group <- as.factor(mydat$group)
  }
  # ----------------------------
  # create expression with model summarys. used
  # for plotting in the diagram later
  # ----------------------------
  mannwhitneyu <- function(count, grp) {
    if (min(grp, na.rm=TRUE)==0) {
      grp <- grp+1
    }
    completeString <- c("")
    cnt <- length(unique(na.omit(grp)))
    for (i in 1:cnt) {
      for (j in i:cnt) {
        if (i!=j) {
          xsub <- count[which(grp==i | grp==j)]
          ysub <- grp[which(grp==i | grp==j)]
          ysub <- ysub[which(!is.na(xsub))]
          xsub <- as.numeric(na.omit(xsub))
          ysub <- as.numeric(na.omit(ysub))
          wt <- wilcox.test(xsub ~ ysub)
          
          if (wt$p.value < 0.001) {
            modsum <- as.character(as.expression(
              substitute(p[pgrp] < pval, list(pgrp=sprintf("(%i|%i)", i, j),
                                               pval=0.001))))
          }
          else {
            modsum <- as.character(as.expression(
              substitute(p[pgrp] == pval, list(pgrp=sprintf("(%i|%i)", i, j),
                                               pval=sprintf("%.3f", wt$p.value)))))
          }
          completeString <- sprintf("%s * \",\" ~ ~ %s", 
                                    completeString, 
                                    modsum)
        }
      }
    }
    return (paste("\"Mann-Whitney-U:\" ~ ~ ", substring(completeString, 12), sep=""))
    # return (paste("Mann-Whitney-U", completeString, sep=""))
    # return (substring(completeString, 12))
  }
  # -----------------------------------------------------------
  # Check whether table summary should be printed
  # -----------------------------------------------------------
  modsum <- NULL
  if (showTableSummary) {
    if (type=="boxplots" || type=="violin") {
      modsum <- mannwhitneyu(varCount, varGroup)
    }
    else {
      modsum <- crosstabsum(ftab)
    }
  }  
  # --------------------------------------------------------
  # If we have a histogram, caluclate means of groups
  # --------------------------------------------------------
  if (type=="histogram") {
    # retrieve all unique factor levels
    faclvl <- unique(na.omit(varGroup))
    # order factors
    faclvl <- faclvl[order(faclvl)]
    # create new data frame for the geom object that prints the
    # vertical line
    vldat <- NULL
    # convert table to df
    ftabdf <- as.data.frame(apply(ftab, MARGIN=2, function(x) cbind(x)))
    colvalues <- as.numeric(attr(ftab, "dimnames")[[1]])
    ftabdf$fac <- colvalues
    # iterate all unique categories, so we can calculate
    # mean of "varCount" in each group
    for (f in 1:length(faclvl)) {
      # get mean from each group
      # m <- mean(na.omit(varCount[which(x==faclvl[f])]))
      m <- sum(ftabdf[,f]*ftabdf$fac) / sum(ftabdf[,f])
      # get standard deviation from each group
      stdv <- sd(na.omit(varCount[which(varGroup==faclvl[f])]))
      # add new row with group and associated mean
      vldat <- as.data.frame(rbind(vldat, c(faclvl[f], m, stdv, yfactor=f)))
    }
    # add row names
    names(vldat) <- c("group", "mw", "stddev", "yfactor")
    # convert group to factor
    vldat$group <- as.factor(vldat$group)
  }
  # --------------------------------------------------------
  # Prepare and trim legend labels to appropriate size
  # --------------------------------------------------------
  # Check whether we have any labels passed as parameter
  if (is.null(legendLabels)) {
    # if not, use category text of group variable as legend text
    legendLabels <- c(dfgrp$Var1)
  }
  # wrap legend text lines
  legendLabels <- sju.wordwrap(legendLabels, breakLegendLabelsAt)
  # check whether we have a title for the legend
  if (!is.null(legendTitle)) {
    # if yes, wrap legend title line
    legendTitle <- sju.wordwrap(legendTitle, breakLegendTitleAt)
  }
  # check length of diagram title and split longer string at into new lines
  # every 50 chars
  if (!is.null(title)) {
    # if we have weighted values, say that in diagram's title
    if (!is.null(weightByTitleString)) {
      title <- paste(title, weightByTitleString, sep="")
    }
    title <- sju.wordwrap(title, breakTitleAt)
  }
  # check length of x-axis title and split longer string at into new lines
  # every 50 chars
  if (!is.null(axisTitle.x)) {
    axisTitle.x <- sju.wordwrap(axisTitle.x, breakTitleAt)
  }
  # check length of x-axis title and split longer string at into new lines
  # every 50 chars
  if (!is.null(axisTitle.y)) {
    axisTitle.y <- sju.wordwrap(axisTitle.y, breakTitleAt)
  }
  # check length of x-axis-labels and split longer strings at into new lines
  # every 10 chars, so labels don't overlap
  if (!is.null(axisLabels.x)) {
    axisLabels.x <- sju.wordwrap(axisLabels.x, breakLabelsAt)
  }
  # If axisLabels.x were not defined, simply set numbers from 1 to
  # amount of categories (=number of rows) in dataframe instead
  else  {
    axisLabels.x <- c(startAxisAt:catcount)
  }
  # check length of x-axis-labels of interaction variable and split 
  # longer strings into new lines
  if (!is.null(interactionVar)) {
    if (!is.null(interactionVarLabels)) {
      interactionVarLabels <- sju.wordwrap(interactionVarLabels, breakLabelsAt)
    }
    # If interaction-variable-labels were not defined, simply set numbers from 1 to
    # amount of categories instead
    else  {
      iavarLabLength <- length(unique(na.omit(interactionVar)))
      interactionVarLabels <- c(1:iavarLabLength)
    }
  }
  # If missings are not removed, add an
  # "NA" to labels and a new row to data frame which contains the missings
  if (!na.rm) {
    axisLabels.x = c(axisLabels.x, "NA")
  }
  # --------------------------------------------------------
  # add group counts to category labels
  # --------------------------------------------------------
  if (showGroupCount) {
    nas <- ifelse(na.rm==TRUE, "ifany", "no")
    # check whether we have interaction variables or not
    if (!is.null(interactionVarLabels)) {
      # retrieve group counts by converting data column
      # into table
      if (is.null(weightBy)) {
        gc <- table(varGroup, interactionVar, useNA=nas)
      }
      else {
        gc <- table(sju.weight2(varGroup, weightBy), interactionVar, useNA=nas)
      }
      # determinte loop-steps
      lst <- length(interactionVarLabels)
      # iterate category labels
      for (i in 1:lst) {
        # remember original label
        ial <- interactionVarLabels[i]
        # add group count to each cat. label
        interactionVarLabels[i] <- paste(ial, " (n=", gc[1,i], ")", sep="")
        interactionVarLabels[i+lst] <- paste(ial, " (n=", gc[2,i], ")", sep="")
      }
    }
    else {
      sums <- unname(rowSums(ftab))
      # iterate category labels
      for (i in 1:length(sums)) {
        # add group count to each cat. label
        axisLabels.x[i] <- paste(axisLabels.x[i], " (n=", sums[i], ")", sep="")
      }
      sums <- unname(colSums(ftab))
      # iterate category labels
      for (i in 1:length(sums)) {
        # add group count to each cat. label
        legendLabels[i] <- paste(legendLabels[i], " (n=", sums[i], ")", sep="")
      }
    }
  }
  # --------------------------------------------------------
  # Prepare bar charts
  # --------------------------------------------------------
  trimViolin <- FALSE
  lower_lim <- 0
  # calculate upper y-axis-range
  # if we have a fixed value, use this one here
  if (!is.null(axisLimits.y) && length(axisLimits.y) == 2) {
    lower_lim <- axisLimits.y[1]
    upper_lim <- axisLimits.y[2]
  }
  else {
    # if we have boxplots, we have different ranges, so we can adjust
    # the y axis
    if (type=="boxplots" || type=="violin") {
      # use an extra standard-deviation as limits for the y-axis when we have boxplots
      lower_lim <- min(varCount, na.rm=TRUE) - floor(sd(varCount, na.rm=TRUE))
      upper_lim <- max(varCount, na.rm=TRUE) + ceiling(sd(varCount, na.rm=TRUE))
      # make sure that the y-axis is not below zero
      if (lower_lim < 0) {
        lower_lim <- 0
        trimViolin <- TRUE
      }
    }
    # else calculate upper y-axis-range depending
    # on the amount of cases...
    else if (barPosition=="stack") {
      upper_lim <- grpBasisYlim(length(varCount))
    }
    else {
      # ... or the amount of max. answers per category
      upper_lim <- grpFreqYlim(mydat$frq)
    }
  }
  # --------------------------------------------------------
  # define bar colors
  # --------------------------------------------------------
  # define vertical position for labels
  if (coord.flip) {
    # if we flip coordinates, we have to use other parameters
    # than for the default layout
    vert <- ifelse(type == "dots", 0.45, 0.35)
    if (labelPos=="inside" || labelPos=="i") {
      hort <- 1.1
    }
    else {
      hort <- -0.1
    }
  }
  else {
    hort <- waiver()
    if (labelPos=="inside" || labelPos=="i") {
      vert <- 1.1
    }
    else if (barPosition=="stack" || labelPos == "center" || labelPos == "c") {
      vert <- waiver()
    }
    else if (showPercentageValues && showCountValues) {
      # value labels need a different vertical adjustement, depending on
      # whether we plot dots or bars
      vert <- ifelse(type == "dots", -0.5, -0.2)
    }
    else {
      vert <- ifelse(type == "dots", -0.9, -0.5)
    }
  }
  # align dodged position of labels to bar positions
  posdodge <- ifelse(type=="lines", 0, geom.size + geom.spacing)
  # init shaded rectangles for plot
  ganno <- NULL
  # check whether we have dots or bars
  if (type=="dots") {
    # position_dodge displays dots in a dodged position so we avoid overlay here. This may lead
    # to a more difficult distinction of group belongings, since the dots are "horizontally spread"
    # over the digram. For a better overview, we can add a "PlotAnnotation" (see "showPlotAnnotation) here.
    geob <- geom_point(position=position_dodge(0.8), size=geom.size, shape=16)
    # create shaded rectangle, so we know which dots belong to the same category
    if (showPlotAnnotation) {
      ganno <- annotate("rect", xmin=mydat$layer-0.4, xmax=mydat$layer+0.4, ymin=lower_lim, ymax=upper_lim, fill="grey80", alpha=0.1)
    }
  }
  else if (type=="bars") {
    if (barPosition=="dodge") {
      geob <- geom_bar(stat="identity", width = geom.size, position=position_dodge(geom.size+geom.spacing))
    }
    else {
      geob <- geom_bar(stat="identity", width = geom.size, position="stack")
    }
  }
  else if (type=="lines") {
    if (smoothLines) {
      geob <- geom_line(size=geom.size, stat="smooth")
    }
    else {
      geob <- geom_line(size=geom.size)
    }
  }
  else if (type=="boxplots") {
    geob <- geom_boxplot(width=geom.size)
  }
  else if (type=="violin") {
    geob <- geom_violin(trim=trimViolin, width=geom.size)
  }
  else {
    geob <- geom_histogram(stat="identity", position=barPosition, binwidth=geom.size)
  }
  if (!showAxisLabels.x) {
    axisLabels.x <- c("")
  }
  # --------------------------------------------------------
  # Set value labels
  # --------------------------------------------------------
  # don't display value labels when we have boxplots or violin plots
  if (type == "boxplots" || type == "violin") showValueLabels <- FALSE
  if (showValueLabels) {
    # ---------------------------------------------------------
    # if we have facet grids, we have different x and y positions for the value labels
    # so we need to take this into account here
    # ---------------------------------------------------------
    if (facet.grid) {
      # ---------------------------------------------------------
      # if we want percentage values, we have different sprintf-parameters
      # ---------------------------------------------------------
      if (showPercentageValues && showCountValues) {
        ggvaluelabels <-  geom_text(aes(x = count, 
                                        y = frq, 
                                        label = sprintf("%i\n(%.01f%%)", frq, prz), 
                                        group = group),
                                    vjust = vert)
      }
      else if (showCountValues) {
        ggvaluelabels <-  geom_text(aes(x = count, 
                                        y = frq, 
                                        label = sprintf("%i", frq), 
                                        group = group),
                                    vjust = vert)
      }
      else if (showPercentageValues) {
        ggvaluelabels <-  geom_text(aes(x = count, 
                                        y = frq, 
                                        label = sprintf("%.01f%%", prz), 
                                        group = group),
                                    vjust=vert)
      }
      else {
        ggvaluelabels <-  geom_text(label="")
      }
    }
    else {
      # ---------------------------------------------------------
      # if we have stacked bars, we need to apply 
      # this stacked y-position to the labels as well
      # ---------------------------------------------------------
      if (barPosition=="stack") {
        if (showPercentageValues && showCountValues) {
          ggvaluelabels <-  geom_text(aes(y = ypos, 
                                          label = sprintf("%i\n(%.01f%%)", frq, prz)),
                                      vjust = vert)
        }
        if (showCountValues) {
          ggvaluelabels <-  geom_text(aes(y = ypos, 
                                          label = sprintf("%i", frq)),
                                      vjust = vert)
        }
        if (showPercentageValues) {
          ggvaluelabels <-  geom_text(aes(y = ypos, 
                                          label = sprintf("%.01f%%", prz)),
                                      vjust = vert)
        }
        else {
          ggvaluelabels <-  geom_text(label="")
        }
      }
      else {
        # ---------------------------------------------------------
        # if we have dodged bars or dots, we have to use a slightly 
        # dodged position for labels
        # as well, sofor better reading
        # ---------------------------------------------------------
        if (showPercentageValues && showCountValues) {
          if (coord.flip) {
            ggvaluelabels <-  geom_text(aes(y = frq, 
                                            label = sprintf("%i (%.01f%%)", frq, prz)),
                                        position = position_dodge(posdodge),
                                        vjust = vert,
                                        hjust = hort)
          }
          else {
            ggvaluelabels <-  geom_text(aes(y = frq, 
                                            label = sprintf("%i\n(%.01f%%)", frq, prz)),
                                        position = position_dodge(posdodge),
                                        vjust = vert,
                                        hjust = hort)
          }
        }
        else if (showCountValues) {
          ggvaluelabels <-  geom_text(aes(y = frq, 
                                          label = sprintf("%i", frq)),
                                      position = position_dodge(posdodge),
                                      hjust = hort,
                                      vjust = vert)
        }
        else if (showPercentageValues) {
          ggvaluelabels <-  geom_text(aes(y = frq, 
                                          label = sprintf("%.01f%%", prz)),
                                      position = position_dodge(posdodge),
                                      hjust = hort,
                                      vjust = vert)
        }
        else {
          ggvaluelabels <-  geom_text(label="")
        }
      }
    }
  }
  else {
    ggvaluelabels <-  geom_text(label="")
  }
  # --------------------------------------------------------
  # Set up grid breaks
  # --------------------------------------------------------
  if (is.null(gridBreaksAt)) {
    gridbreaks <- waiver()
  }
  else {
    gridbreaks <- c(seq(lower_lim, upper_lim, by=gridBreaksAt))
  }
  # ----------------------------------
  # Print plot
  # ----------------------------------
  # plot object for histogram style
  # ----------------------------------
  if (type=="histogram" || type=="lines") {
    mydat$count <- as.numeric(as.character(mydat$count))
    if (type=="histogram") {
      # histrogram need fill aes
      baseplot <- ggplot(mydat, aes(x=count, y=frq, fill=group)) + geob
    }
    else {
      # lines need colour aes
      baseplot <- ggplot(mydat, aes(x = count, y=frq, colour=group)) + geob
    }
    scalex <- scale_x_continuous(limits = c(catmin, catcount))
    # -----------------------------------------
    # show mean line for histograms
    # -----------------------------------------
    if (showMeanIntercept) {
      # -----------------------------------------
      # vertical lines indicating the mean
      # -----------------------------------------
      baseplot <- baseplot + 
        geom_vline(data = vldat, 
                   aes(xintercept = mw, 
                       colour = group), 
                   linetype = meanInterceptLineType, 
                   size = meanInterceptLineSize)
      # -----------------------------------------
      # check whether meanvalue should be shown.
      # -----------------------------------------
      if (showMeanValue) {
        # -----------------------------------------
        # use annotation instead of geomtext, because we 
        # need mean value only printed once
        # -----------------------------------------
        baseplot <- baseplot + 
          annotate("text", 
                   x = vldat$mw, 
                   y = upper_lim, 
                   parse = TRUE, 
                   label = sprintf("italic(bar(x)[%i]) == %.2f", 
                                   vldat$yfactor, 
                                   vldat$mw), 
                   hjust = 1.05, 
                   vjust = vldat$yfactor * 2)
      }
      # -----------------------------------------
      # check whether the user wants to plot standard deviation area
      # -----------------------------------------
      if (showStandardDeviation) {
        baseplot <- baseplot +
          # -----------------------------------------
          # first draw shaded rectangle. these are by default 
          # in grey colour with very high transparancy
          # -----------------------------------------
          annotate("rect", 
                   xmin = vldat$mw - vldat$stddev, 
                   xmax = vldat$mw+vldat$stddev, 
                   fill = "grey50", 
                   ymin = 0, 
                   ymax = upper_lim, 
                   alpha = 0.1) +
          # -----------------------------------------
          # draw border-lines for shaded rectangles 
          # in the related group colours.
          # -----------------------------------------
          geom_vline(data = vldat, 
                     aes(xintercept = mw - stddev, 
                         colour = group), 
                     linetype = 3, 
                     size = meanInterceptLineSize, 
                     alpha = 0.7) +
          geom_vline(data = vldat, 
                     aes(xintercept = mw + stddev, 
                         colour = group), 
                     linetype = 3, 
                     size = meanInterceptLineSize, 
                     alpha = 0.7)
        # -----------------------------------------
        # if mean values are plotted, plot standard 
        # deviation values as well
        # -----------------------------------------
        if (showMeanValue) {
          baseplot <- baseplot + 
            # -----------------------------------------
            # use annotation instead of geomtext, because we 
            # need standard deviations only printed once
            # -----------------------------------------
            annotate("text", 
                     x = vldat$mw + vldat$stddev, 
                     y = upper_lim, 
                     parse = TRUE, 
                     label = sprintf("italic(s[%i]) == %.2f", 
                                     vldat$yfactor, 
                                     round(vldat$stddev, 1)), 
                     hjust = 1.1, 
                     vjust = vldat$yfactor * 2)
        }
      }
    }
  }
  else if (type=="boxplots" || type=="violin") {
    if (is.null(interactionVar)) {
      baseplot <- ggplot(mydat, 
                         aes(x = group, 
                             y = frq, 
                             fill = group, 
                             weight = wb)) + geob
      scalex <- scale_x_discrete(labels = axisLabels.x)
    }
    else {
      baseplot <- ggplot(mydat, 
                         aes(x = interaction(ia, group), 
                             y = frq, 
                             fill = group, 
                             weight = wb)) + geob
      scalex <- scale_x_discrete(labels=interactionVarLabels)
    }
    # if we have a violin plot, add an additional boxplot inside to show
    # more information
    if (type=="violin") {
      baseplot <- baseplot +
        geom_boxplot(width=innerBoxPlotWidth, fill="white", outlier.colour=NA)
    }
    # ---------------------------------------------------------
    # if we have boxplots or violon plots, also add a point that indicates
    # the mean value
    # different fill colours, because violin boxplots have white background
    # ---------------------------------------------------------
    fcsp <- ifelse(type=="boxplots", "white", "black")
    baseplot <- baseplot +
      stat_summary(fun.y = "mean", 
                   geom = "point", 
                   shape = 21, 
                   size = innerBoxPlotDotSize, 
                   fill = fcsp)
  }
  else {
    if (type == "dots") {
      baseplot <- ggplot(mydat, 
                         aes(x = factor(count), 
                             y = frq, 
                             colour = group))
    }
    else {
      baseplot <- ggplot(mydat, 
                         aes(x = factor(count), 
                             y = frq, 
                             fill = group))
    }
    # ---------------------------------------------------------
    # check whether we have dots plotted, and if so, use annotation
    # We have to use annotation first, because the diagram's layers are plotted
    # in the order as they're passed to the ggplot-command. Since we don't want the
    # shaded rectangles to overlay the dots, we add them first
    # ---------------------------------------------------------
    if (!is.null(ganno) && !facet.grid) {
      baseplot <- baseplot + ganno
    }
    # add geom
    baseplot <- baseplot + geob
    if (startAxisAt>1) {
      scalex <- scale_x_discrete(labels = axisLabels.x, 
                                 limits = as.factor(seq(from = startAxisAt,
                                                        to = catcount,
                                                        by = 1)))
    }
    else {
      scalex <- scale_x_discrete(labels=axisLabels.x)
    }
  }
  # ------------------------------------------
  # If we have bars or dot plots, we show 
  # Pearson's chi-square test results
  # ------------------------------------------
  baseplot <- print.table.summary(baseplot,
                                  modsum,
                                  tableSummaryPos)
  # ------------------------------
  # prepare y-axis and
  # show or hide y-axis-labels
  # ------------------------------
  if (showAxisLabels.y) {
    y_scale <- scale_y_continuous(breaks = gridbreaks, 
                                  limits = c(lower_lim, upper_lim), 
                                  expand = expand.grid)
  }
  else {
    y_scale <- scale_y_continuous(breaks = gridbreaks, 
                                  limits = c(lower_lim, upper_lim), 
                                  expand = expand.grid, 
                                  labels = NULL)
  }
  # ------------------------------
  # continue with plot objects...
  # ------------------------------
  baseplot <- baseplot +
    # set Y-axis, depending on the calculated upper y-range.
    # It either corresponds to the maximum amount of cases in the data set
    # (length of var) or to the highest count of var's categories.
    # coord_cartesian(ylim=c(0, upper_lim)) +
    y_scale +
    # show absolute and percentage value of each bar.
    ggvaluelabels +
    # no additional labels for the x- and y-axis, only diagram title
    labs(title=title, x=axisTitle.x, y=axisTitle.y, fill=legendTitle) +
    # print value labels to the x-axis.
    # If parameter "axisLabels.x" is NULL, the category numbers (1 to ...) 
    # appear on the x-axis
    scalex
  # check whether coordinates should be flipped, i.e.
  # swap x and y axis
  if (coord.flip) {
    baseplot <- baseplot + coord_flip()
  }
  if (facet.grid) {
    # --------------------------------------------------
    # Here we start when we have a faces grid instead of
    # a grouped bar plot.
    # --------------------------------------------------
    baseplot <- baseplot + 
      # set font size for axes.
      theme(strip.text = element_text(face="bold",size=rel(1.2))) +
      facet_wrap( ~ group)
  }
  # ---------------------------------------------------------
  # set geom colors
  # ---------------------------------------------------------
  baseplot <- sj.setGeomColors(baseplot, geom.colors, length(legendLabels), ifelse(hideLegend==TRUE, FALSE, TRUE), legendLabels)
  # ----------------------------------
  # Plot integrated bar chart here
  # ----------------------------------
  if (printPlot) plot(baseplot)
  # -------------------------------------
  # return results
  # -------------------------------------
  invisible (structure(class = "sjpgrpfrq",
                       list(plot = baseplot,
                            df = mydat)))
}


# Berechnet die aufgerundete Obergrenze der y-Achse anhand
# der maximal möglichen Fallzahl einer Antwortmöglichkeit
# Dadurch werden Balkendiagramme eines Datensatzes immer im
# gleichen Vergältnis dargestellt, da die y-Achse nie variiert,
# sondern immer von 0 bis (Anzahl der Fälle) geht.
#
# Parameter:
# - len: die Anzahl an max. möglichen Fällen
grpBasisYlim <- function(len) {
  anzahl <- 1
  while (len>=(10*anzahl)) {
    anzahl <- anzahl * 10
  }
  
  while(len>=anzahl) {
    anzahl <- anzahl + round(anzahl/10,0)
  }
  
  #  retval <- (ceiling(len/anzahl)*anzahl)
  #  return (retval)
  return (anzahl)
}

# Berechnet die aufgerundete Obergrenze der y-Achse anhand
# des höchsten Datenwertes einer Antwortmöglichkeit.
# Dadurch werden Balkendiagramme eines Datensatzes immer unterschiedlich
# dargestellt, je nach Anzahl der häufigsten Antworten. Die y-Achse
# geht immer von 0 bis (maximale Antworthäufigkeit einer Variable)
#
# Parameter:
# - var: die Variable mit den Antwortmöglichkeiten
grpFreqYlim <- function(var) {
  # suche die Antwort mit den häufigsten Antworten,
  # also den höchsten Wert einer Variablenausprägung
  len <- max(var, na.rm = T)
  
  anzahl <- 5
  while (len>=(10*anzahl)) {
    anzahl <- anzahl + 5
  }
  return (10*anzahl)
}

grpFreqBaseYlim <- function(var) {
  # suche die Antwort mit den häufigsten Antworten,
  # also den höchsten Wert einer Variablenausprägung
  len <- min(var, na.rm = T)
  
  anzahl <- max(var, na.rm = T)
  while (len>=(10*anzahl)) {
    anzahl <- anzahl - 5
  }
  return (10*anzahl)
}
