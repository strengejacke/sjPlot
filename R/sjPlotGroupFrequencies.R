# bind global variables
utils::globalVariables(c("ypos", "wb", "ia", "mw", "stddev", "count"))


#' @title Plot grouped or stacked frequencies
#' @name sjp.grpfrq
#' 
#' @seealso \href{http://www.strengejacke.de/sjPlot/sjp.grpfrq/}{sjPlot manual: sjp.grpfrq}
#'             
#' @description Plot grouped or stacked frequencies of variables 
#'                as bar/dor graphs, box or violin plots, histograms etc.
#' 
#' @param varCount a vector of values (variable) describing the bars which make up the plot.
#' @param varGroup grouping variable of same length as \code{varCount}, where \code{varCount} 
#'          is grouped into the categories represented by \code{varGrp}.
#' @param weightBy weight factor that will be applied to weight all cases from \code{varCount}.
#'          Must be a vector of same length as \code{varCount}. Default is \code{NULL}, so no weights are used.
#' @param weightByTitleString suffix (as string) for the plot's title, if \code{weightBy} is specified,
#'          e.g. \code{weightByTitleString=" (weighted)"}. Default is \code{NULL}, so plot's 
#'          title will not have a suffix when cases are weighted.
#' @param interactionVar an interaction variable which can be used for box plots. Divides each category indicated
#'          by \code{varGroup} into the factors of \code{interactionVar}, so that each category of \code{varGroup}
#'          is subgrouped into \code{interactionVar}'s categories. Only applies when argument \code{type}
#'          is \code{box} or \code{violin} (resp. their alternative strings like \code{"boxplot"}, \code{"boxplots"} or \code{"v"}).
#' @param barPosition indicates whether bars should be positioned side-by-side (default)
#'          or stacked (use \code{"stack"} as argument).
#'          If \code{type = "histogram"}, you can use either \code{"dodge"} (default value), 
#'          which displays the bars side-by-side, or \code{"identity"}, which results in 
#'          overlaying bars. In the latter case, it's recommended to adjust the 
#'          alpha value (see \code{\link{sjp.setTheme}}).
#' @param type The plot type. May be one of the following:
#'          \describe{
#'            \item{\code{"bars"}}{for simple bars (the default setting)}
#'            \item{\code{"dots"}}{for dot plots}
#'            \item{\code{"histogram"}}{for grouped histograms}
#'            \item{\code{"lines"}}{for grouped line-styled histogram with filled area}
#'            \item{\code{"boxplots"}}{for grouped box plots}
#'            \item{\code{"violins"}}{for grouped violin plots}
#'            }
#'            You may use initial letter for \code{type} options, except for
#'            \code{type = "boxplots"}, which may be abbreviated \code{type = "box"}
#' @param hideLegend logical, indicates whether legend (guide) should be shown or not.
#' @param axisLimits.x numeric vector of length two, defining lower and upper axis limits
#'          of the x scale. By default, this argument is set to \code{NULL}, i.e. the 
#'          x-axis fits to the required range to plot all data. \strong{Note} that limiting
#'          the x-axis-range may result in warnings from \code{ggplot} due to values
#'          outside this range that could not be plotted.
#' @param axisLimits.y numeric vector of length two, defining lower and upper axis limits
#'          of the y scale. By default, this argument is set to \code{NULL}, i.e. the 
#'          y-axis ranges from 0 to required maximum.
#' @param facet.grid \code{TRUE} when bar charts should be plotted as facet grids instead of integrated single
#'          bar charts. Ideal for larger amount of groups. This argument wraps a single panel into 
#'          \code{varGroup} amount of panels, i.e. each group is represented within a new panel.
#' @param title plot title as string. Example: \code{title = "my title"}.
#'          Use \code{NULL} to automatically detect variable names that will be used as title
#'          (see \code{\link[sjmisc]{set_label}}) for details). If \code{title = ""},
#'          no title is printed.
#' @param legendTitle title of the plot legend, as string.
#' @param axisLabels.x a character vector with labels for the x-axis breaks. \strong{Note:} 
#'          Axis labels will be automatically detected, when data was either imported 
#'          with \code{\link[sjmisc]{read_spss}} or has named factor levels 
#'          (see 'Examples'). Else, specifiy argument like this:
#'          \code{axisLabels.x = c("Label1", "Label2", "Label3")}.
#'          The labels may also be passed as \code{\link{list}} object. They will be coerced
#'          to character vector automatically.
#' @param interactionVarLabels a character vector with labels for the x-axis breaks
#'          when having interaction variables included.
#'          These labels replace the \code{axisLabels.x}. Only applies, when using box or violin plots
#'          (i.e. \code{type = "boxplots"} or \code{"violins"}) and \code{interactionVar} is not \code{NULL}.
#'          Example: See \code{axisLabels.x}.
#' @param legendLabels a character vector with labels for the guide/legend.
#' @param breakTitleAt determines how many chars of the plot title are displayed in
#'          one line and when a line break is inserted into the title.
#' @param breakLabelsAt determines how many chars of the category labels are displayed in 
#'          one line and when a line break is inserted.
#' @param breakLegendTitleAt determines how many chars of the legend's title 
#'          are displayed in one line and when a line break is inserted.
#' @param breakLegendLabelsAt determines how many chars of the legend labels are 
#'          displayed in one line and when a line break is inserted.
#' @param gridBreaksAt set breaks for the axis, i.e. at every \code{gridBreaksAt}'th 
#'          position a major grid is being printed.
#' @param innerBoxPlotWidth width of the inner box plot that is plotted inside of violin plots. Only applies 
#'          if \code{type = "violins"}. Default value is 0.15
#' @param innerBoxPlotDotSize size of mean dot insie a violin or box plot. Applies only 
#'          when \code{type = "violins"} or \code{"boxplots"}.
#' @param geom.colors User defined color palette for geoms. If specified, must either be vector with color values 
#'          of same length as groups defined in \code{varGroup}, or a specific color brewer palette code (see 'Note').
#' @param geom.size size resp. width of the geoms (bar width or point size, depending on \code{type} argument).
#'          Note that  bar and bin widths mostly need smaller values than dot sizes (i.e. if \code{type = "dots"}).
#'          By default, \code{geom.size = NULL}, which means that this argument is automatically
#'          adjusted depending on the plot type.
#' @param geom.spacing the spacing between geoms (i.e. bar spacing)
#' @param smoothLines prints a smooth line curve. Only applies, when argument \code{type = "lines"}.
#' @param expand.grid logical, if \code{TRUE}, the plot grid is expanded, i.e. there is a small margin between
#'          axes and plotting region. Default is \code{FALSE}.
#' @param showValueLabels logical, whether count and percentage values should be plotted to each bar. Default
#'          is \code{TRUE}.
#' @param showCountValues logical, if \code{TRUE} (default), count values are plotted to each bar. 
#'          If \code{FALSE}, count values are removed.
#' @param showPercentageValues logical, if \code{TRUE} (default), percentage values are plotted to each bar
#'          If \code{FALSE}, percentage values are removed.
#' @param showAxisLabels.x logical, whether x-axis labels (category names) should be shown or not.
#' @param showAxisLabels.y logical, whether y-axis labels (count values) should be shown or not.
#' @param showPlotAnnotation logical, if \code{TRUE}, the groups of dots in a dot-plot are highlighted 
#'          with a shaded rectangle.
#' @param showMeanIntercept logical, if \code{TRUE}, a vertical line in histograms is drawn 
#'          to indicate the mean value of the count variables. Only applies to histogram-charts.
#' @param showMeanValue logical, if \code{TRUE} (default value), the mean value is printed 
#'          to the vertical line that indicates the mean value
#'          of the count variables. Only applies to histogram-charts.
#' @param showStandardDeviation logical, if \code{TRUE}, the standard deviation is annotated 
#'          as shaded rectangle around the mean intercept line. Only applies to histogram-charts. 
#'          The shaded rectangles have borders in the group colors, so it's easier to see
#'          which shaded area belongs to which mean value resp. group.
#' @param showTableSummary logical, if \code{TRUE}, a summary of the cross tabulation with N, 
#'          chi-squared, df, Cramer's V or Phi-value and p-value is printed to the upper 
#'          right corner of the plot (see \code{tableSummaryPos}. If a cell contains expected
#'          values lower than five(or lower than 10 if df is 1), the Fisher's excact test 
#'          (see \code{\link{fisher.test}}) is computed instead of Chi-squared test. 
#'          If the table's matrix is larger than 2x2, Fisher's excact test with Monte Carlo 
#'          simulation is computed. Only applies to barcharts or dotplots, i.e. 
#'          when argument \code{type = "bars"} or \code{"dots"}.
#' @param showGroupCount logical, if \code{TRUE}, the count within each group is added 
#'          to the category labels (e.g. \code{"Cat 1 (n=87)"}). Default value is \code{FALSE}.
#' @param tableSummaryPos position of the model summary which is printed when \code{showTableSummary} 
#'          is \code{TRUE}. Default is \code{"r"}, i.e. it's printed to the upper right corner. 
#'          Use \code{"l"} for upper left corner.
#' @param meanInterceptLineType linetype of the mean intercept line. Only applies to histogram-charts and when
#'          \code{showMeanIntercept = TRUE}.
#' @param meanInterceptLineSize size of the mean intercept line. Only applies to histogram-charts and when
#'          \code{showMeanIntercept = TRUE}.
#' @param axisTitle.x title for the x-axis. By default, \code{""} is used, i.e. no title
#'          is printed. If \code{axisTitle.x = NULL}, the variable name will be 
#'          automatically detected and used as title (see \code{\link[sjmisc]{set_label}}) 
#'          for details).
#' @param axisTitle.y title for the-y axis. By default, \code{""} is used, i.e. no title
#'          is printed. If \code{axisTitle.y = NULL}, variable name will be automatically 
#'          detected and used as title (see \code{\link[sjmisc]{set_label}}) for details).
#' @param autoGroupAt numeric value, indicating at which length of unique values of \code{varCount}, 
#'          automatic grouping into smaller units is done (see \code{\link[sjmisc]{group_var}}).
#'          If \code{varCount} has large numbers of unique values, there may be too many bars 
#'          for the plot. Hence it's practical to group such variables. For example, 
#'          if \code{autoGroupAt = 50} and \code{varCount} has more than 50 unique values,
#'          it will be grouped (using the \code{\link[sjmisc]{group_var}} function). 
#'          Default value for \code{autoGroupAt} is \code{NULL}, i.e. auto-grouping is off.
#'          See \code{\link[sjmisc]{group_var}} for examples on grouping.
#' @param startAxisAt numeric, determines the lower limit of the x-axis. By default, this value is set
#'          to \code{"auto"}, i.e. the value range on the x-axis starts with the lowest value of \code{varCount}.
#'          If \code{startAxisAt = 1}, plot may have zero counts if the lowest value of \code{varCount}
#'          is larger than 1 and hence no bars plotted for these values in such cases.
#' @param coord.flip logical, if \code{TRUE}, the x and y axis are swapped.
#' @param labelPos string, indicating the position of value labels, when \code{coord.flip = TRUE}.
#'          Can be either \code{"inside"} or \code{"outside"} (default). You may specify
#'          initial letter only. If \code{coord.flip = FALSE}, use \code{"center"} 
#'          to center labels (useful if label angle is changes via \code{\link{sjp.setTheme}}).
#' @param na.rm logical, if \code{TRUE}, missings are not included in the frequency plot.
#' @param printPlot logical, if \code{TRUE} (default), plots the results as graph. Use \code{FALSE} if you don't
#'          want to plot any graphs. In either case, the ggplot-object will be returned as value.
#' @return (Insisibily) returns the ggplot-object with the complete plot (\code{plot}) as well as the data frame that
#'           was used for setting up the ggplot-object (\code{df}).
#' 
#' @note \code{geom.colors} may be a acharacter vector of color values 
#'         in hex-format, or a name of a \href{http://colorbrewer2.org}{color brewer} palette.
#'         Following options are valid for the \code{geom.colors} argument:
#'         \itemize{
#'            \item If not specified, a default color brewer palette will be used, which is suitable for the plot style (i.e. diverging for likert scales, qualitative for grouped bars etc.).
#'            \item If \code{"gs"}, a greyscale will be used.
#'            \item If \code{geom.colors} is any valid color brewer palette name, the related palette will be used. Use \code{\link[RColorBrewer]{display.brewer.all}} to view all available palette names.
#'            \item Else specify own color values as vector (e.g. \code{geom.colors = c("#f00000", "#00ff00")}).
#'          }

#' @examples
#' # histrogram with EUROFAMCARE sample dataset
#' library(sjmisc)
#' data(efc)
#' efc.val <- get_labels(efc)
#' efc.var <- get_label(efc)
#' sjp.grpfrq(efc$e17age,
#'            efc$e16sex,
#'            title = efc.var['e17age'],
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
#' @import sjmisc
#' @importFrom dplyr group_by mutate arrange
#' @importFrom stats na.omit xtabs wilcox.test
#' @export
sjp.grpfrq <- function(varCount,
                       varGroup,
                       weightBy = NULL,
                       weightByTitleString = NULL,
                       interactionVar = NULL,
                       type = "bars",
                       geom.size = NULL,
                       geom.spacing = 0.15,
                       geom.colors = "Paired",
                       hideLegend = FALSE,
                       facet.grid = FALSE,
                       title = "",
                       legendTitle = NULL,
                       axisLabels.x = NULL,
                       interactionVarLabels = NULL,
                       legendLabels = NULL,
                       axisLimits.x = NULL,
                       axisLimits.y = NULL,
                       breakTitleAt = 50,
                       breakLabelsAt = 15,
                       breakLegendTitleAt = 20,
                       breakLegendLabelsAt = 20,
                       gridBreaksAt = NULL,
                       barPosition = "dodge",
                       innerBoxPlotWidth = 0.15,
                       innerBoxPlotDotSize = 3,
                       smoothLines = FALSE,
                       expand.grid = FALSE,
                       showValueLabels = TRUE,
                       showCountValues = TRUE,
                       showPercentageValues = TRUE,
                       showAxisLabels.x = TRUE,
                       showAxisLabels.y = TRUE,
                       showPlotAnnotation = TRUE,
                       showMeanIntercept = FALSE,
                       showMeanValue = TRUE,
                       showStandardDeviation = FALSE,
                       showTableSummary = FALSE,
                       showGroupCount = FALSE,
                       tableSummaryPos = "r",
                       meanInterceptLineType = 2,
                       meanInterceptLineSize = 0.5,
                       axisTitle.x = "",
                       axisTitle.y = "",
                       autoGroupAt = NULL,
                       startAxisAt = "auto",
                       coord.flip = FALSE,
                       labelPos = "outside",
                       na.rm = TRUE,
                       printPlot = TRUE) {
  # --------------------------------------------------------
  # We have several options to name the diagram type
  # Here we will reduce it to a unique value
  # --------------------------------------------------------
  if (type == "b" || type == "bar") type <- "bars"
  if (type == "l" || type == "line") type <- "lines"
  if (type == "d" || type == "dot") type <- "dots"
  if (type == "h" || type == "hist") {
    type <- c("histogram")
    # no table summary and no group count for
    # ctageory labels (to avoid overlapping)
    showTableSummary <- FALSE
    showGroupCount <- FALSE
  }
  if (type == "box" || type == "boxplot") type <- "boxplots"
  if (type == "v" || type == "violins") type <- "violin"
  if (expand.grid == TRUE) {
    expand.grid <- ggplot2::waiver()
  } else {
    expand.grid <- c(0, 0)
  }
  # --------------------------------------------------------
  # check default geom.size
  # --------------------------------------------------------
  if (is.null(geom.size)) {
    if (type == "bars") 
      geom.size <- .7
    else if (type == "dots") 
      geom.size <- 3
    else if (type == "histogram") 
      geom.size <- .6
    else if (type == "lines") 
      geom.size <- .8
    else if (type == "boxplots") 
      geom.size <- .5
    else if (type == "violin") 
      geom.size <- .6
  }
  #---------------------------------------------------
  # check whether variable should be auto-grouped
  #---------------------------------------------------
  if (!is.null(interactionVar) && type != "boxplots" && type != "violin") {
    warning("'interactionVar' only applies to boxplots and violinplots (see 'type') and will be ignored.", call. = F)
  }
  # --------------------------------------------------------
  # try to automatically set labels is not passed as argument
  # --------------------------------------------------------
  if (is.null(axisLabels.x)) {
    axisLabels.x <- sjmisc:::autoSetValueLabels(varCount)
    # if we have box or violin plots, but no axis labels on x axis (NULL),
    # we need to hide x-axis, because automatically retrieved labels are
    # equal to unique values of varCount, and not varGroup.
    if (type == "boxplots" || type == "violin") showAxisLabels.x <- FALSE
  }
  if (is.null(legendLabels)) legendLabels <- sjmisc:::autoSetValueLabels(varGroup)
  if (is.null(interactionVarLabels) && !is.null(interactionVar)) interactionVarLabels <- sjmisc:::autoSetValueLabels(interactionVar)
  if (is.null(axisTitle.x)) axisTitle.x <- sjmisc:::autoSetVariableLabels(varCount)
  if (is.null(legendTitle)) legendTitle <- sjmisc:::autoSetVariableLabels(varGroup)  
  if (is.null(title)) {
    t1 <- sjmisc:::autoSetVariableLabels(varCount)
    t2 <- sjmisc:::autoSetVariableLabels(varGroup)
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
  # count variable may not be a factor!
  # --------------------------------------------------------
  varCount <- as.numeric(varCount)
  varGroup <- as.numeric(varGroup)
  #---------------------------------------------------
  # check whether variable should be auto-grouped
  #---------------------------------------------------
  if (!is.null(autoGroupAt) && length(unique(varCount)) >= autoGroupAt) {
    message(sprintf("Variable has %i unique values and was grouped...", 
                    length(unique(varCount))))
    # check for default auto-group-size or user-defined groups
    agcnt <- ifelse(autoGroupAt < 30, autoGroupAt, 30)
    # group axis labels
    axisLabels.x <- sjmisc::group_labels(varCount, 
                                         groupsize = "auto", 
                                         groupcount = agcnt)
    # group variable
    varCount <- sjmisc::group_var(varCount, 
                                  groupsize = "auto", 
                                  as.num = TRUE, 
                                  groupcount = agcnt)
  }
  # --------------------------------------------------------
  # unlist labels
  # --------------------------------------------------------
  if (!is.null(legendLabels) && is.list(legendLabels)) legendLabels <- unlistlabels(legendLabels)
  if (!is.null(axisLabels.x) && is.list(axisLabels.x)) axisLabels.x <- unlistlabels(axisLabels.x)
  if (!is.null(interactionVarLabels) && is.list(interactionVarLabels)) interactionVarLabels <- unlistlabels(interactionVarLabels)
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
  # handle zero-counts
  # -----------------------------------------------
  # Determine length of count and group var
  grplen <- length(unique(stats::na.omit(varGroup)))
  # determine maximum values
  # first, check the total amount of different factor levels
  catcount_1 <- length(unique(stats::na.omit(varCount)))
  # second, check the maximum factor level
  catcount_2 <- max(varCount, na.rm = TRUE)
  # if categories start with zero, fix this here
  if (min(varCount, na.rm = TRUE) == 0) {
    catcount_2 <- catcount_2 + 1
  }
  # catcount should contain the higher values, i.e. the maximum count of
  # categories (factor levels) corresponds either to the highest factor level
  # value or to the amount of different factor levels, depending on which one
  # is larger
  catcount <- ifelse(catcount_1 > catcount_2, catcount_1, catcount_2)
  catmin <- min(varCount, na.rm = TRUE)
  # ----------------------------------------------
  # check for axis start, depending on lowest value
  # ----------------------------------------------
  if (startAxisAt == "auto") {
    startAxisAt <- as.numeric(catmin)
    if (startAxisAt == 0) startAxisAt <- 1
  }
  # get the highest answer category of "variable", so we know where the
  # range of the x-axis ends
  if (!is.null(axisLabels.x)) catcount <- length(axisLabels.x)
  # if we have legend labels, we know the exact
  # amount of groups
  if (is.null(legendLabels)) {
    grpcount <- grplen
  } else {
    grpcount <- length(legendLabels)
  }
  # -----------------------------------------------
  # define x-axis limits
  # -----------------------------------------------
  if (is.null(axisLimits.x)) axisLimits.x <- c(catmin, catcount)
  # -----------------------------------------------
  # create cross table for stats, summary etc.
  # and weight variable
  #---------------------------------------------------
  if (is.null(weightBy)) {
    ftab <- table(varCount, varGroup)
  } else {
    ftab <- round(stats::xtabs(weightBy ~ varCount + varGroup), 0)
  }
  # new data frame from variables
  df <- as.data.frame(ftab)
  # separate data frame for grouping variable. we need this to
  # determine the number of groups
  dfgrp <- as.data.frame(table(df$varGroup))
  # Factors have to be transformed into numeric values
  # for continiuos x-axis-scale
  df$varCount <- sjmisc::to_value(df$varCount, keep.labels = F)
  # if categories start with zero, fix this here
  if (min(df$varCount) == 0) df$varCount <- df$varCount + 1
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
    subdf <- df[df$varGroup == dfgrp$Var1[i], ]
    # Create a vector of zeros 
    frq <- rep(0, catcount)
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
    dummydat <- data.frame(cbind(count = 1:catcount, 
                                 group, 
                                 frq, 
                                 layer = 1:catcount))
    # --------------------------------------------------------
    # Handle missings
    # --------------------------------------------------------
    if (!na.rm) {
      # get amount of missings
      frq <- sum(is.na(varCount[which(varGroup == as.numeric(dfgrp$Var1[i]))]))
      # create data frame
      tmpdf <- data.frame(cbind(count = catcount + 1, 
                                group, 
                                frq, 
                                layer = catcount + 1))
      # append dummy data frame to final data frame
      dummydat <- data.frame(rbind(dummydat, tmpdf))
    }
    # append dummy data frame to final data frame
    mydat <- data.frame(rbind(mydat, dummydat))
  }
  # convert grouping variable to character
  mydat$count <- sjmisc::to_value(mydat$count, keep.labels = F)
  # convert grouping variable to character
  mydat$group <- sjmisc::to_value(mydat$group, keep.labels = F)
  # convert frequencies to numeric
  mydat$frq <- sjmisc::to_value(mydat$frq, keep.labels = F)
  # convert layer to numeric
  mydat$layer <- sjmisc::to_value(mydat$layer, keep.labels = F)
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
    miss <- sjmisc::to_value(allgroups[!allgroups %in% mydat$group], keep.labels = F)
    # retrieve subset of all rows where group is from lowest group-value to 
    # missing group
    dummy1 <- mydat[apply(mydat, MARGIN = 1, function(xy) all(xy[2] < miss)), ]
    # retrieve subset of all rows where group is from missing group to
    # highest group-value
    dummy2 <- mydat[apply(mydat, MARGIN = 1, function(xy) all(xy[2] > miss)), ]
    # create dummy-data frame that contains the missing row with zero-values
    emptyrows <- data.frame(cbind(count = c(1:catcount), 
                                  group = miss, 
                                  frq = 0, 
                                  layer = 1:catcount))
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
  prz <- rep(0, nrow(mydat))
  # iterate all data frame rows
  for (k in 1:length(prz)) {
    # if we have facet grids, calculate percentage
    # within each group
    if (facet.grid) {
      # get frequency value of each row and divide it by the sum of frequencies of
      # all frequencies of the current row's group
      prz[k] <- c(round(100 * mydat[k, 3] / sum(mydat$frq[mydat$group == mydat[k, 2]]), 2))
    # if we have dodged/stacked bars or plots, calculate percentage
    # within each category
    } else {
      # get frequency value of each row and divide it by the sum of frequencies of
      # all frequencies of the current row's category
      prz[k] <- c(round(100 * mydat[k, 3] / sum(mydat$frq[mydat$count == mydat[k, 1]]), 2))
    } 
  }
  # bind percentage as final column
  mydat <- as.data.frame(cbind(mydat, prz))
  # convert prz to numeric
  mydat$texty <- sjmisc::to_value(mydat$prz, keep.labels = F)
  # add half of Percentage values as new y-position for stacked bars
  # mydat <- ddply(mydat, "count", transform, ypos = cumsum(frq) - 0.5*frq)
  mydat <- mydat %>%
    dplyr::group_by(count) %>%
    dplyr::mutate(ypos = cumsum(frq) - 0.5 * frq) %>%
    dplyr::arrange(count)
  # --------------------------------------------------------
  # If we have boxplots, use different data frame structure
  # --------------------------------------------------------
  if (type == "boxplots" || type == "violin") {
    w <- ifelse(is.null(weightBy), 1, weightBy)
    if (is.null(interactionVar)) {
      mydat <- stats::na.omit(data.frame(cbind(group = varGroup, 
                                               frq = varCount, 
                                               wb = w)))
    } else {
      mydat <- stats::na.omit(data.frame(cbind(group = varGroup, 
                                               frq = varCount, 
                                               ia = interactionVar, 
                                               wb = w)))
      mydat$ia <- as.factor(mydat$ia)
    }
    mydat$group <- as.factor(mydat$group)
  }
  # ----------------------------
  # create expression with model summarys. used
  # for plotting in the diagram later
  # ----------------------------
  mannwhitneyu <- function(count, grp) {
    if (min(grp, na.rm = TRUE) == 0) grp <- grp + 1
    completeString <- c("")
    cnt <- length(unique(stats::na.omit(grp)))
    for (i in 1:cnt) {
      for (j in i:cnt) {
        if (i != j) {
          xsub <- count[which(grp == i | grp == j)]
          ysub <- grp[which(grp == i | grp == j)]
          ysub <- ysub[which(!is.na(xsub))]
          xsub <- as.numeric(stats::na.omit(xsub))
          ysub <- as.numeric(stats::na.omit(ysub))
          wt <- stats::wilcox.test(xsub ~ ysub)
          
          if (wt$p.value < 0.001) {
            modsum <- as.character(as.expression(
              substitute(p[pgrp] < pval, list(pgrp = sprintf("(%i|%i)", i, j),
                                              pval = 0.001))))
          } else {
            modsum <- as.character(as.expression(
              substitute(p[pgrp] == pval, list(pgrp = sprintf("(%i|%i)", i, j),
                                               pval = sprintf("%.3f", wt$p.value)))))
          }
          completeString <- sprintf("%s * \",\" ~ ~ %s", 
                                    completeString, 
                                    modsum)
        }
      }
    }
    return(paste("\"Mann-Whitney-U:\" ~ ~ ", substring(completeString, 12), sep = ""))
    # return (paste("Mann-Whitney-U", completeString, sep=""))
    # return (substring(completeString, 12))
  }
  # -----------------------------------------------------------
  # Check whether table summary should be printed
  # -----------------------------------------------------------
  modsum <- NULL
  if (showTableSummary) {
    if (type == "boxplots" || type == "violin") {
      modsum <- mannwhitneyu(varCount, varGroup)
    } else {
      modsum <- crosstabsum(ftab)
    }
  }  
  # --------------------------------------------------------
  # If we have a histogram, caluclate means of groups
  # --------------------------------------------------------
  if (type == "histogram") {
    # retrieve all unique factor levels
    faclvl <- unique(stats::na.omit(varGroup))
    # order factors
    faclvl <- faclvl[order(faclvl)]
    # create new data frame for the geom object that prints the
    # vertical line
    vldat <- NULL
    # convert table to df
    ftabdf <- as.data.frame(apply(ftab, MARGIN = 2, function(x) cbind(x)))
    colvalues <- as.numeric(attr(ftab, "dimnames")[[1]])
    ftabdf$fac <- colvalues
    # iterate all unique categories, so we can calculate
    # mean of "varCount" in each group
    for (f in 1:length(faclvl)) {
      # get mean from each group
      m <- sum(ftabdf[, f] * ftabdf$fac) / sum(ftabdf[, f])
      # get standard deviation from each group
      stdv <- sd(stats::na.omit(varCount[which(varGroup == faclvl[f])]))
      # add new row with group and associated mean
      vldat <- data.frame(rbind(vldat, c(faclvl[f], m, stdv, yfactor = f)))
    }
    # add row names
    names(vldat) <- c("group", "mw", "stddev", "yfactor")
    # convert group to factor
    vldat$group <- as.factor(vldat$group)
  }
  # --------------------------------------------------------
  # Prepare and trim legend labels to appropriate size
  # --------------------------------------------------------
  # Check whether we have any labels passed as argument
  # if not, use category text of group variable as legend text
  if (is.null(legendLabels)) legendLabels <- c(dfgrp$Var1)
  # wrap legend text lines
  legendLabels <- sjmisc::word_wrap(legendLabels, breakLegendLabelsAt)
  # check whether we have a title for the legend
  if (!is.null(legendTitle)) legendTitle <- sjmisc::word_wrap(legendTitle, breakLegendTitleAt)
  # check length of diagram title and split longer string at into new lines
  # every 50 chars
  if (!is.null(title)) {
    # if we have weighted values, say that in diagram's title
    if (!is.null(weightByTitleString)) title <- paste(title, weightByTitleString, sep = "")
    title <- sjmisc::word_wrap(title, breakTitleAt)
  }
  # check length of x-axis title and split longer string at into new lines
  # every 50 chars
  if (!is.null(axisTitle.x)) axisTitle.x <- sjmisc::word_wrap(axisTitle.x, breakTitleAt)
  # check length of x-axis title and split longer string at into new lines
  # every 50 chars
  if (!is.null(axisTitle.y)) axisTitle.y <- sjmisc::word_wrap(axisTitle.y, breakTitleAt)
  # check length of x-axis-labels and split longer strings at into new lines
  # every 10 chars, so labels don't overlap
  if (!is.null(axisLabels.x)) axisLabels.x <- sjmisc::word_wrap(axisLabels.x, breakLabelsAt)
  # If axisLabels.x were not defined, simply set numbers from 1 to
  # amount of categories (=number of rows) in dataframe instead
  else axisLabels.x <- c(startAxisAt:catcount)
  # check length of x-axis-labels of interaction variable and split 
  # longer strings into new lines
  if (!is.null(interactionVar)) {
    if (!is.null(interactionVarLabels)) {
      interactionVarLabels <- sjmisc::word_wrap(interactionVarLabels, breakLabelsAt)
    }
    # If interaction-variable-labels were not defined, simply set numbers from 1 to
    # amount of categories instead
    else  {
      iavarLabLength <- length(unique(stats::na.omit(interactionVar)))
      interactionVarLabels <- c(1:iavarLabLength)
    }
  }
  # If missings are not removed, add an
  # "NA" to labels and a new row to data frame which contains the missings
  if (!na.rm) axisLabels.x = c(axisLabels.x, "NA")
  # --------------------------------------------------------
  # add group counts to category labels
  # --------------------------------------------------------
  if (showGroupCount) {
    nas <- ifelse(na.rm == TRUE, "ifany", "no")
    # check whether we have interaction variables or not
    if (!is.null(interactionVarLabels)) {
      # retrieve group counts by converting data column
      # into table
      if (is.null(weightBy)) {
        gc <- table(varGroup, interactionVar, useNA = nas)
      } else {
        gc <- table(sjmisc::weight2(varGroup, weightBy), interactionVar, useNA = nas)
      }
      # determinte loop-steps
      lst <- length(interactionVarLabels)
      # iterate category labels
      for (i in 1:lst) {
        # remember original label
        ial <- interactionVarLabels[i]
        # add group count to each cat. label
        interactionVarLabels[i] <- paste(ial, " (n=", gc[1, i], ")", sep = "")
        interactionVarLabels[i + lst] <- paste(ial, " (n=", gc[2, i], ")", sep = "")
      }
    } else {
      sums <- unname(rowSums(ftab))
      # add group count to each cat. label
      axisLabels.x <- paste(axisLabels.x, " (n=", sums, ")", sep = "")
      sums <- unname(colSums(ftab))
      # add group count to each cat. label
      legendLabels <- paste(legendLabels, " (n=", sums, ")", sep = "")
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
  } else {
    # if we have boxplots, we have different ranges, so we can adjust
    # the y axis
    if (type == "boxplots" || type == "violin") {
      # use an extra standard-deviation as limits for the y-axis when we have boxplots
      lower_lim <- min(varCount, na.rm = TRUE) - floor(sd(varCount, na.rm = TRUE))
      upper_lim <- max(varCount, na.rm = TRUE) + ceiling(sd(varCount, na.rm = TRUE))
      # make sure that the y-axis is not below zero
      if (lower_lim < 0) {
        lower_lim <- 0
        trimViolin <- TRUE
      }
    # else calculate upper y-axis-range depending
    # on the amount of cases...
    } else if (barPosition == "stack") {
      upper_lim <- max(pretty(table(varCount) * 1.05))
    } else {
      # ... or the amount of max. answers per category
      upper_lim <- max(pretty(table(varCount, varGroup) * 1.05))
    }
  }
  # --------------------------------------------------------
  # define bar colors
  # --------------------------------------------------------
  # define vertical position for labels
  if (coord.flip) {
    # if we flip coordinates, we have to use other arguments
    # than for the default layout
    vert <- ifelse(type == "dots", 0.45, 0.35)
    if (labelPos == "inside" || labelPos == "i") {
      hort <- 1.1
    } else {
      hort <- -0.1
    }
  } else {
    hort <- ggplot2::waiver()
    if (labelPos == "inside" || labelPos == "i") {
      vert <- 1.1
    } else if (barPosition == "stack" || labelPos == "center" || labelPos == "c") {
      vert <- ggplot2::waiver()
    } else if (showPercentageValues && showCountValues) {
      # value labels need a different vertical adjustement, depending on
      # whether we plot dots or bars
      vert <- ifelse(type == "dots", -0.5, -0.2)
    } else {
      vert <- ifelse(type == "dots", -0.9, -0.5)
    }
  }
  # align dodged position of labels to bar positions
  posdodge <- ifelse(type == "lines", 0, geom.size + geom.spacing)
  # init shaded rectangles for plot
  ganno <- NULL
  # check whether we have dots or bars
  if (type == "dots") {
    # position_dodge displays dots in a dodged position so we avoid overlay here. This may lead
    # to a more difficult distinction of group belongings, since the dots are "horizontally spread"
    # over the digram. For a better overview, we can add a "PlotAnnotation" (see "showPlotAnnotation) here.
    geob <- geom_point(position = position_dodge(0.8), size = geom.size, shape = 16)
    # create shaded rectangle, so we know which dots belong to the same category
    if (showPlotAnnotation) {
      ganno <- annotate("rect", 
                        xmin = mydat$layer - 0.4, 
                        xmax = mydat$layer + 0.4, 
                        ymin = lower_lim, 
                        ymax = upper_lim, 
                        fill = "grey80", 
                        alpha = 0.1)
    }
  } else if (type == "bars") {
    if (barPosition == "dodge") {
      geob <- geom_bar(stat = "identity", 
                       width = geom.size, 
                       position = position_dodge(geom.size + geom.spacing))
    } else {
      geob <- geom_bar(stat = "identity", 
                       width = geom.size, 
                       position = "stack")
    }
  } else if (type == "lines") {
    if (smoothLines) {
      geob <- geom_line(size = geom.size, 
                        stat = "smooth", 
                        method = "loess")
    } else {
      geob <- geom_line(size = geom.size)
    }
  } else if (type == "boxplots") {
    geob <- geom_boxplot(width = geom.size)
  } else if (type == "violin") {
    geob <- geom_violin(trim = trimViolin, width = geom.size)
  } else {
    geob <- geom_histogram(stat = "identity", 
                           position = barPosition, 
                           binwidth = geom.size)
  }
  if (!showAxisLabels.x) axisLabels.x <- c("")
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
      # if we want percentage values, we have different sprintf-arguments
      # ---------------------------------------------------------
      if (showPercentageValues && showCountValues) {
        ggvaluelabels <-  geom_text(aes(x = count, 
                                        y = frq, 
                                        label = sprintf("%i\n(%.01f%%)", frq, prz), 
                                        group = group),
                                    vjust = vert,
                                    show_guide = FALSE)
      } else if (showCountValues) {
        ggvaluelabels <-  geom_text(aes(x = count, 
                                        y = frq, 
                                        label = sprintf("%i", frq), 
                                        group = group),
                                    vjust = vert,
                                    show_guide = FALSE)
      } else if (showPercentageValues) {
        ggvaluelabels <-  geom_text(aes(x = count, 
                                        y = frq, 
                                        label = sprintf("%.01f%%", prz), 
                                        group = group),
                                    vjust = vert,
                                    show_guide = FALSE)
      } else {
        ggvaluelabels <-  geom_text(label = "", show_guide = FALSE)
      }
    } else {
      # ---------------------------------------------------------
      # if we have stacked bars, we need to apply 
      # this stacked y-position to the labels as well
      # ---------------------------------------------------------
      if (barPosition == "stack") {
        if (showPercentageValues && showCountValues) {
          ggvaluelabels <-  geom_text(aes(y = ypos, label = sprintf("%i\n(%.01f%%)", frq, prz)),
                                      vjust = vert,
                                      show_guide = FALSE)
        } else if (showCountValues) {
          ggvaluelabels <-  geom_text(aes(y = ypos, label = sprintf("%i", frq)),
                                      vjust = vert,
                                      show_guide = FALSE)
        } else if (showPercentageValues) {
          ggvaluelabels <-  geom_text(aes(y = ypos, label = sprintf("%.01f%%", prz)),
                                      vjust = vert,
                                      show_guide = FALSE)
        } else {
          ggvaluelabels <-  geom_text(label = "", show_guide = FALSE)
        }
      } else {
        # ---------------------------------------------------------
        # if we have dodged bars or dots, we have to use a slightly 
        # dodged position for labels
        # as well, sofor better reading
        # ---------------------------------------------------------
        if (showPercentageValues && showCountValues) {
          if (coord.flip) {
            ggvaluelabels <-  geom_text(aes(y = frq, label = sprintf("%i (%.01f%%)", frq, prz)),
                                        position = position_dodge(posdodge),
                                        vjust = vert,
                                        hjust = hort,
                                        show_guide = FALSE)
          } else {
            ggvaluelabels <-  geom_text(aes(y = frq, label = sprintf("%i\n(%.01f%%)", frq, prz)),
                                        position = position_dodge(posdodge),
                                        vjust = vert,
                                        hjust = hort,
                                        show_guide = FALSE)
          }
        } else if (showCountValues) {
          ggvaluelabels <-  geom_text(aes(y = frq, label = sprintf("%i", frq)),
                                      position = position_dodge(posdodge),
                                      hjust = hort,
                                      vjust = vert,
                                      show_guide = FALSE)
        } else if (showPercentageValues) {
          ggvaluelabels <-  geom_text(aes(y = frq, label = sprintf("%.01f%%", prz)),
                                      position = position_dodge(posdodge),
                                      hjust = hort,
                                      vjust = vert,
                                      show_guide = FALSE)
        } else {
          ggvaluelabels <-  geom_text(label = "", show_guide = FALSE)
        }
      }
    }
  } else {
    ggvaluelabels <-  geom_text(label = "", show_guide = FALSE)
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
  # Print plot
  # ----------------------------------
  # plot object for histogram style
  # ----------------------------------
  if (type == "histogram" || type == "lines") {
    mydat$count <- sjmisc::to_value(mydat$count, keep.labels = F)
    if (type == "histogram") {
      # histrogram need fill aes
      baseplot <- ggplot(mydat, aes(x = count, y = frq, fill = group)) + geob
    } else {
      # lines need colour aes
      baseplot <- ggplot(mydat, aes(x = count, y = frq, colour = group)) + geob
    }
    scalex <- scale_x_continuous(limits = axisLimits.x)
    # -----------------------------------------
    # show mean line for histograms
    # -----------------------------------------
    if (showMeanIntercept) {
      # -----------------------------------------
      # vertical lines indicating the mean
      # -----------------------------------------
      baseplot <- baseplot + 
        geom_vline(data = vldat, 
                   aes(xintercept = mw, colour = group), 
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
                   xmax = vldat$mw + vldat$stddev, 
                   fill = "grey50", 
                   ymin = 0, 
                   ymax = upper_lim, 
                   alpha = 0.1) +
          # -----------------------------------------
          # draw border-lines for shaded rectangles 
          # in the related group colours.
          # -----------------------------------------
          geom_vline(data = vldat, 
                     aes(xintercept = mw - stddev, colour = group), 
                     linetype = 3, 
                     size = meanInterceptLineSize, 
                     alpha = 0.7) +
          geom_vline(data = vldat, 
                     aes(xintercept = mw + stddev, colour = group), 
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
  } else if (type == "boxplots" || type == "violin") {
    if (is.null(interactionVar)) {
      baseplot <- ggplot(mydat, 
                         aes(x = group, 
                             y = frq, 
                             fill = group, 
                             weight = wb)) + geob
      scalex <- scale_x_discrete(labels = axisLabels.x)
    } else {
      baseplot <- ggplot(mydat, 
                         aes(x = interaction(ia, group), 
                             y = frq, 
                             fill = group, 
                             weight = wb)) + geob
      scalex <- scale_x_discrete(labels = interactionVarLabels)
    }
    # if we have a violin plot, add an additional boxplot inside to show
    # more information
    if (type == "violin") {
      baseplot <- baseplot +
        geom_boxplot(width = innerBoxPlotWidth, 
                     fill = "white", 
                     outlier.colour = NA)
    }
    # ---------------------------------------------------------
    # if we have boxplots or violon plots, also add a point that indicates
    # the mean value
    # different fill colours, because violin boxplots have white background
    # ---------------------------------------------------------
    fcsp <- ifelse(type == "boxplots", "white", "black")
    baseplot <- baseplot +
      stat_summary(fun.y = "mean", 
                   geom = "point", 
                   shape = 21, 
                   size = innerBoxPlotDotSize, 
                   fill = fcsp)
  } else {
    if (type == "dots") {
      baseplot <- ggplot(mydat, 
                         aes(x = factor(count), 
                             y = frq, 
                             colour = group))
    } else {
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
    if (!is.null(ganno) && !facet.grid) baseplot <- baseplot + ganno
    # add geom
    baseplot <- baseplot + geob
    if (startAxisAt > 1) {
      scalex <- scale_x_discrete(labels = axisLabels.x, 
                                 limits = as.factor(seq(from = startAxisAt,
                                                        to = catcount,
                                                        by = 1)))
    } else {
      scalex <- scale_x_discrete(labels = axisLabels.x)
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
  } else {
    y_scale <- scale_y_continuous(breaks = gridbreaks, 
                                  limits = c(lower_lim, upper_lim), 
                                  expand = expand.grid, 
                                  labels = NULL)
  }
  # ------------------------------
  # continue with plot objects...
  # ------------------------------
  baseplot <- baseplot +
    # show absolute and percentage value of each bar.
    ggvaluelabels +
    # no additional labels for the x- and y-axis, only diagram title
    labs(title = title, 
         x = axisTitle.x, 
         y = axisTitle.y, 
         fill = legendTitle,
         colour = legendTitle) +
    # print value labels to the x-axis.
    # If argument "axisLabels.x" is NULL, the category numbers (1 to ...) 
    # appear on the x-axis
    scalex +
    # set Y-axis, depending on the calculated upper y-range.
    # It either corresponds to the maximum amount of cases in the data set
    # (length of var) or to the highest count of var's categories.
    # coord_cartesian(ylim=c(0, upper_lim)) +
    y_scale
  # check whether coordinates should be flipped, i.e.
  # swap x and y axis
  if (coord.flip) baseplot <- baseplot + coord_flip()
  # --------------------------------------------------
  # Here we start when we have a faces grid instead of
  # a grouped bar plot.
  # --------------------------------------------------
  if (facet.grid) {
    baseplot <- baseplot + 
      # set font size for axes.
      theme(strip.text = element_text(face = "bold",size = rel(1.2))) +
      facet_wrap(~group)
  }
  # ---------------------------------------------------------
  # set geom colors
  # ---------------------------------------------------------
  baseplot <- sj.setGeomColors(baseplot, geom.colors, length(legendLabels), ifelse(hideLegend == TRUE, FALSE, TRUE), legendLabels)
  # ----------------------------------
  # Plot integrated bar chart here
  # ----------------------------------
  if (printPlot) plot(baseplot)
  # -------------------------------------
  # return results
  # -------------------------------------
  invisible(structure(class = "sjpgrpfrq",
                      list(plot = baseplot,
                           df = mydat)))
}
