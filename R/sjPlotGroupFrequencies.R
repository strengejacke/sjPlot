# bind global variables
utils::globalVariables(c(".", "label", "prz", "frq", "ypos", "wb", "ia", "mw", "stddev", "count"))


#' @title Plot grouped or stacked frequencies
#' @name sjp.grpfrq
#' 
#' @seealso \href{http://www.strengejacke.de/sjPlot/sjp.grpfrq/}{sjPlot manual: sjp.grpfrq}
#'             
#' @description Plot grouped or stacked frequencies of variables as bar/dot, 
#'                box or violin plots, or line plot.
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
#' @param type The plot type. May be one of the following:
#'          \describe{
#'            \item{\code{"bars"}}{for simple bars (the default setting)}
#'            \item{\code{"dots"}}{for dot plots}
#'            \item{\code{"lines"}}{for grouped line-styled plot}
#'            \item{\code{"boxplots"}}{for grouped box plots}
#'            \item{\code{"violins"}}{for grouped violin plots}
#'            }
#'            You may use initial letter for \code{type} options, except for
#'            \code{type = "boxplots"}, which may be abbreviated \code{type = "box"}
#' @param hideLegend logical, indicates whether legend (guide) should be shown or not.
#' @param axisLimits.y numeric vector of length two, defining lower and upper axis limits
#'          of the y scale. By default, this argument is set to \code{NULL}, i.e. the 
#'          y-axis fits to the required range of the data.
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
#' @param geom.size size resp. width of the geoms (bar width, line thickness or point size, depending on \code{type} argument).
#'          Note that bar and bin widths mostly need smaller values than dot sizes (i.e. if \code{type = "dots"}).
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
#' @param showTableSummary logical, if \code{TRUE}, a summary of the cross tabulation with N, 
#'          chi-squared, df, Cramer's V or Phi-value and p-value is printed to the upper 
#'          right corner of the plot (see \code{tableSummaryPos}. If a cell contains expected
#'          values lower than five(or lower than 10 if df is 1), the Fisher's excact test 
#'          (see \code{\link{fisher.test}}) is computed instead of chi-square test. 
#'          If the table's matrix is larger than 2x2, Fisher's excact test with Monte Carlo 
#'          simulation is computed. Only applies to barcharts or dotplots, i.e. 
#'          when argument \code{type = "bars"} or \code{"dots"}.
#' @param showGroupCount logical, if \code{TRUE}, the count within each group is added 
#'          to the category labels (e.g. \code{"Cat 1 (n=87)"}). Default value is \code{FALSE}.
#' @param tableSummaryPos position of the model summary which is printed when \code{showTableSummary} 
#'          is \code{TRUE}. Default is \code{"r"}, i.e. it's printed to the upper right corner. 
#'          Use \code{"l"} for upper left corner.
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
#' @param coord.flip logical, if \code{TRUE}, the x and y axis are swapped.
#' @param vjust character vector, indicating the vertical position of value 
#'          labels. Allowed are same values as for \code{vjust} aesthetics from 
#'          \code{ggplot2}: "left", "center", "right", "bottom", "middle", "top" and
#'          new options like "inward" and "outward", which align text towards and 
#'          away from the center of the plot respectively.
#' @param hjust character vector, indicating the horizontal position of value 
#'          labels. Allowed are same values as for \code{vjust} aesthetics from 
#'          \code{ggplot2}: "left", "center", "right", "bottom", "middle", "top" and
#'          new options like "inward" and "outward", which align text towards and 
#'          away from the center of the plot respectively.
#' @param y.offset numeric, offset for text labels when their alignment is adjusted 
#'          to the top/bottom of the geom (see \code{hjust} and \code{vjust}).
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
#' sjp.grpfrq(efc$e17age,
#'            efc$e16sex,
#'            showValueLabels = FALSE)
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
#'            type = "box")
#' 
#' # Grouped bar plot ranging from 7 to 28
#' sjp.grpfrq(efc$neg_c_7, 
#'            efc$e42dep, 
#'            showValueLabels = FALSE)
#' 
#' # same data as line plot
#' sjp.grpfrq(efc$neg_c_7, 
#'            efc$e42dep, 
#'            type = "lines")
#'            
#' @import ggplot2
#' @import sjmisc
#' @importFrom tidyr gather
#' @importFrom dplyr group_by mutate arrange summarise add_rownames
#' @importFrom stats na.omit xtabs wilcox.test sd
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
                       showTableSummary = FALSE,
                       showGroupCount = FALSE,
                       tableSummaryPos = "r",
                       axisTitle.x = "",
                       axisTitle.y = "",
                       autoGroupAt = NULL,
                       coord.flip = FALSE,
                       vjust = "bottom",
                       hjust = "center",
                       y.offset = NULL,
                       na.rm = TRUE,
                       printPlot = TRUE) {
  # --------------------------------------------------------
  # get variable name
  # --------------------------------------------------------
  var.name.cnt <- get_var_name(deparse(substitute(varCount)))
  var.name.grp <- get_var_name(deparse(substitute(varGroup)))
  # --------------------------------------------------------
  # We have several options to name the diagram type
  # Here we will reduce it to a unique value
  # --------------------------------------------------------
  if (type == "b" || type == "bar")
    type <- "bars"
  if (type == "l" || type == "line")
    type <- "lines"
  if (type == "d" || type == "dot")
    type <- "dots"
  if (type == "box" || type == "boxplot")
    type <- "boxplots"
  if (type == "v" || type == "violins")
    type <- "violin"
  # --------------------------------------------------------
  # Plot margins
  # --------------------------------------------------------
  if (isTRUE(expand.grid))
    expand.grid <- ggplot2::waiver()
  else
    expand.grid <- c(0, 0)
  # --------------------------------------------------------
  # check default geom.size
  # --------------------------------------------------------
  if (is.null(geom.size)) {
    if (type == "bars")
      geom.size <- .7
    else if (type == "dots")
      geom.size <- 3
    else if (type == "lines")
      geom.size <- .8
    else if (type == "boxplots")
      geom.size <- .5
    else if (type == "violin")
      geom.size <- .6
    else
      geom.size <- .7
  }
  # --------------------------------------------------------
  # set text label offset
  # --------------------------------------------------------
  if (is.null(y.offset)) {
    # get maximum y-pos
    y.offset <- ceiling(max(table(varCount, varGroup)) / 100)
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
  } else {
    y_offset <- y.offset
  }
  #---------------------------------------------------
  # Interaction variable defined for invalid plot type?
  #---------------------------------------------------
  if (!is.null(interactionVar) && type != "boxplots" && type != "violin") {
    warning("'interactionVar' only applies to boxplots and violinplots (see 'type') and will be ignored.", call. = F)
  }
  #---------------------------------------------------
  # auto-set plot title for box plots?
  #---------------------------------------------------
  if (missing(title) && (type == "boxplots" || type == "violin"))
    title <- NULL
  #---------------------------------------------------
  # check whether variable should be auto-grouped
  #---------------------------------------------------
  if (!is.null(autoGroupAt) && length(unique(varCount)) >= autoGroupAt) {
    message(sprintf("%s has %i unique values and was grouped...",
                    var.name.cnt,
                    length(unique(varCount))))
    # check for default auto-group-size or user-defined groups
    agcnt <- ifelse(autoGroupAt < 30, autoGroupAt, 30)
    # group axis labels
    axisLabels.x <- sjmisc::group_labels(sjmisc::to_value(varCount, keep.labels = F),
                                         groupsize = "auto",
                                         groupcount = agcnt)
    # group variable
    grp.varCount <- sjmisc::group_var(sjmisc::to_value(varCount, keep.labels = F),
                                      groupsize = "auto",
                                      as.num = TRUE,
                                      groupcount = agcnt)
    # set value labels
    sjmisc::set_labels(grp.varCount) <- axisLabels.x
  } else {
    grp.varCount <- varCount
  }
  # --------------------------------------------------------
  # create cross table of frequencies and percentages
  # --------------------------------------------------------
  mydat <- create.xtab.df(grp.varCount,
                          varGroup,
                          round.prz = 2,
                          na.rm = na.rm,
                          weightBy = weightBy)
  # --------------------------------------------------------
  # x-position as numeric factor, added later after
  # tidying
  # --------------------------------------------------------
  bars.xpos <- 1:nrow(mydat$mydat)
  # --------------------------------------------------------
  # try to automatically set labels if not passed as argument
  # --------------------------------------------------------
  if (missing(axisLabels.x) && (type == "boxplots" || type == "violin")) {
    axisLabels.x <- mydat$labels.grp
    if (missing(hideLegend)) hideLegend <- is.null(interactionVar)
  }
  if (is.null(axisLabels.x)) axisLabels.x <- mydat$labels.cnt
  if (is.null(legendLabels)) legendLabels <- mydat$labels.grp
  if (is.null(interactionVarLabels) && !is.null(interactionVar)) {
    interactionVarLabels <- sjmisc::get_labels(interactionVar,
                                               attr.only = F,
                                               include.values = F,
                                               include.non.labelled = T)
    # create repeating label for x-axis
    interactionVarLabels <- rep(interactionVarLabels, 
                                length.out = length(axisLabels.x) * length(interactionVarLabels))
    # we need a legend, cause x axis is labelled with interaction var value
    hideLegend <- FALSE
    legendLabels <- axisLabels.x
  }
  if (is.null(axisTitle.x)) axisTitle.x <- sjmisc::get_label(varCount, def.value = var.name.cnt)
  if (is.null(legendTitle)) legendTitle <- sjmisc::get_label(varGroup, def.value = var.name.grp)
  if (is.null(title)) {
    t1 <- sjmisc::get_label(varCount, def.value = var.name.cnt)
    t2 <- sjmisc::get_label(varGroup, def.value = var.name.grp)
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
  if (anyNA(as.numeric(na.omit(varCount))))
    varCount <- sjmisc::to_value(varCount, keep.labels = F)
  else
    varCount <- as.numeric(varCount)
  if (anyNA(as.numeric(na.omit(varGroup))))
    varGroup <- sjmisc::to_value(varGroup, keep.labels = F)
  else
    varGroup <- as.numeric(varGroup)
  # --------------------------------------------------------
  # Define amount of categories
  # --------------------------------------------------------
  grpcount <- length(legendLabels)
  # -----------------------------------------------
  # create cross table for stats, summary etc.
  # and weight variable
  #---------------------------------------------------
  mydf <- tidyr::gather(mydat$mydat, 
                        "group", 
                        "frq", 
                        2:(grpcount + 1), 
                        factor_key = TRUE)
  # --------------------------------------------------------
  # add xpos now
  # --------------------------------------------------------
  mydf$xpos <- as.factor(as.numeric(bars.xpos))
  # --------------------------------------------------------
  # add half of Percentage values as new y-position for stacked bars
  # mydat <- ddply(mydat, "count", transform, ypos = cumsum(frq) - 0.5*frq)
  # --------------------------------------------------------
  mydf <- mydf %>%
    dplyr::group_by(label) %>%
    dplyr::mutate(ypos = cumsum(frq) - 0.5 * frq) %>%
    dplyr::arrange(label)
  # add percentages
  mydf$prz <- round(100 * mydf$frq / sum(mydf$frq), 2)
  # --------------------------------------------------------
  # If we have boxplots, use different data frame structure
  # --------------------------------------------------------
  if (type == "boxplots" || type == "violin") {
    # weight variable
    w <- ifelse(is.null(weightBy), 1, weightBy)
    # interaction variable
    if (is.null(interactionVar)) 
      iav <- 1
    else 
      iav <- interactionVar
    mydf <- stats::na.omit(data.frame(cbind(group = varGroup,
                                            frq = varCount,
                                            ia = iav,
                                            wb = w)))
    mydf$ia <- as.factor(mydf$ia)
    mydf$group <- as.factor(mydf$group)
  }
  # ----------------------------
  # create expression with model summarys. used
  # for plotting in the diagram later
  # ----------------------------
  mannwhitneyu <- function(count, grp) {
    if (min(grp, na.rm = TRUE) == 0)
      grp <- grp + 1
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
            modsum <- as.character(as.expression(substitute(
              p[pgrp] < pval, list(pgrp = sprintf("(%i|%i)", i, j),
                                   pval = 0.001)
            )))
          } else {
            modsum <- as.character(as.expression(substitute(
              p[pgrp] == pval, 
              list(pgrp = sprintf("(%i|%i)", i, j),
                   pval = sprintf("%.3f", wt$p.value)))))
          }
          completeString <- sprintf("%s * \",\" ~ ~ %s",
                                    completeString,
                                    modsum)
        }
      }
    }
    return(paste("\"Mann-Whitney-U:\" ~ ~ ",
                 substring(completeString, 12),
                 sep = ""))
  }
  # -----------------------------------------------------------
  # Check whether table summary should be printed
  # -----------------------------------------------------------
  modsum <- NULL
  if (showTableSummary) {
    if (type == "boxplots" || type == "violin")
      modsum <- mannwhitneyu(varCount, varGroup)
    else
      modsum <- crosstabsum(varCount, varGroup, weightBy)
  }
  # --------------------------------------------------------
  # If we have a histogram, caluclate means of groups
  # --------------------------------------------------------
  # if (type == "histogram") {
  #   vldat <- na.omit(data.frame(x = oriVarCount, group = varGroup))
  #   vldat <- vldat %>% 
  #     dplyr::group_by(group) %>% 
  #     dplyr::summarize(mw = mean(x, na.rm = T),
  #                      stddev = stats::sd(x, na.rm = T)) %>%
  #     dplyr::mutate(yfactor = 1:nrow(.))
  # }
  # --------------------------------------------------------
  # Prepare and trim legend labels to appropriate size
  # --------------------------------------------------------
  if (!is.null(legendLabels))
    legendLabels <- sjmisc::word_wrap(legendLabels, breakLegendLabelsAt)
  if (!is.null(legendTitle))
    legendTitle <- sjmisc::word_wrap(legendTitle, breakLegendTitleAt)
  if (!is.null(title)) {
    # if we have weighted values, say that in diagram's title
    if (!is.null(weightByTitleString))
      title <- paste(title, weightByTitleString, sep = "")
    title <- sjmisc::word_wrap(title, breakTitleAt)
  }
  if (!is.null(axisTitle.x))
    axisTitle.x <- sjmisc::word_wrap(axisTitle.x, breakTitleAt)
  if (!is.null(axisTitle.y))
    axisTitle.y <- sjmisc::word_wrap(axisTitle.y, breakTitleAt)
  if (!is.null(axisLabels.x))
    axisLabels.x <- sjmisc::word_wrap(axisLabels.x, breakLabelsAt)
  if (!is.null(interactionVar)) {
    if (!is.null(interactionVarLabels)) {
      interactionVarLabels <- sjmisc::word_wrap(interactionVarLabels, breakLabelsAt)
    }
    # If interaction-variable-labels were not defined, simply set numbers from 1 to
    # amount of categories instead
    else {
      iavarLabLength <- length(unique(stats::na.omit(interactionVar)))
      interactionVarLabels <- c(1:iavarLabLength)
    }
  }
  # --------------------------------------------------------
  # add group counts to category labels
  # --------------------------------------------------------
  if (showGroupCount) {
    nas <- ifelse(isTRUE(na.rm), "ifany", "no")
    # check whether we have interaction variables or not
    if (!is.null(interactionVarLabels)) {
      # retrieve group counts by converting data column
      # into table
      if (is.null(weightBy)) {
        gc <- table(varGroup, interactionVar, useNA = nas)
      } else {
        gc <-
          table(sjmisc::weight2(varGroup, weightBy),
                interactionVar,
                useNA = nas)
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
      sums <- unname(rowSums(mydat$mydat[, -1]))
      # add group count to each cat. label
      axisLabels.x <- paste(axisLabels.x, " (n=", sums, ")", sep = "")
      sums <- unname(colSums(mydat$mydat[, -1]))
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
      lower_lim <- min(varCount, na.rm = TRUE) - floor(stats::sd(varCount, na.rm = TRUE))
      upper_lim <- max(varCount, na.rm = TRUE) + ceiling(stats::sd(varCount, na.rm = TRUE))
      # make sure that the y-axis is not below zero
      if (lower_lim < 0) {
        lower_lim <- 0
        trimViolin <- TRUE
      }
      # else calculate upper y-axis-range depending
      # on the amount of cases...
    } else if (barPosition == "stack") {
      upper_lim <- max(pretty(table(grp.varCount) * 1.05))
    } else {
      # ... or the amount of max. answers per category
      upper_lim <- max(pretty(table(grp.varCount, varGroup) * 1.05))
    }
  }
  # align dodged position of labels to bar positions
  if (type == "lines")
    posdodge <- 0
  else if (type == "dots")
    posdodge <- geom.spacing
  else
    posdodge <- geom.size + geom.spacing
  # init shaded rectangles for plot
  ganno <- NULL
  # check whether we have dots or bars
  if (type == "dots") {
    # position_dodge displays dots in a dodged position so we avoid overlay here. This may lead
    # to a more difficult distinction of group belongings, since the dots are "horizontally spread"
    # over the digram. For a better overview, we can add a "PlotAnnotation" (see "showPlotAnnotation) here.
    geob <- geom_point(position = position_dodge(posdodge),
                       size = geom.size,
                       shape = 16)
    # create shaded rectangle, so we know which dots belong to the same category
    if (showPlotAnnotation) {
      ganno <- annotate("rect",
                        xmin = as.numeric(mydf$xpos) - 0.4,
                        xmax = as.numeric(mydf$xpos) + 0.4,
                        ymin = lower_lim,
                        ymax = upper_lim,
                        fill = "grey80",
                        alpha = 0.1)
    }
  } else if (type == "bars") {
    if (barPosition == "dodge") {
      geob <- geom_bar(stat = "identity",
                       width = geom.size,
                       position = position_dodge(posdodge))
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
    geob <- geom_bar(stat = "identity",
                     position = barPosition,
                     width = geom.size)
  }
  if (!showAxisLabels.x) axisLabels.x <- c("")
  # --------------------------------------------------------
  # Set value labels
  # --------------------------------------------------------
  # don't display value labels when we have boxplots or violin plots
  if (type == "boxplots" || type == "violin") showValueLabels <- FALSE
  if (showValueLabels) {
    # set text positioning
    if (facet.grid)
      text.pos <- "identity"
    else
      text.pos <- position_dodge(posdodge)
    # ---------------------------------------------------------
    # if we have facet grids, we have different x and y positions for the value labels
    # so we need to take this into account here
    # ---------------------------------------------------------
    # ---------------------------------------------------------
    # if we have stacked bars, we need to apply
    # this stacked y-position to the labels as well
    # ---------------------------------------------------------
    if (barPosition == "stack") {
      if (showPercentageValues && showCountValues) {
        ggvaluelabels <-
          geom_text(aes(y = ypos, label = sprintf("%i\n(%.01f%%)", frq, prz)),
                    show.legend = FALSE)
      } else if (showCountValues) {
        ggvaluelabels <-
          geom_text(aes(y = ypos, label = sprintf("%i", frq)),
                    show.legend = FALSE)
      } else if (showPercentageValues) {
        ggvaluelabels <-
          geom_text(aes(y = ypos, label = sprintf("%.01f%%", prz)),
                    show.legend = FALSE)
      } else {
        ggvaluelabels <- geom_text(aes(y = frq), label = "", show.legend = FALSE)
      }
    } else {
      # ---------------------------------------------------------
      # if we have dodged bars or dots, we have to use a slightly
      # dodged position for labels
      # as well, sofor better reading
      # ---------------------------------------------------------
      if (showPercentageValues && showCountValues) {
        if (coord.flip) {
          ggvaluelabels <-
            geom_text(aes(y = frq + y_offset, label = sprintf("%i (%.01f%%)", frq, prz)),
                      position = text.pos,
                      vjust = vjust,
                      hjust = hjust,
                      show.legend = FALSE)
        } else {
          ggvaluelabels <-
            geom_text(aes(y = frq + y_offset, label = sprintf("%i\n(%.01f%%)", frq, prz)),
                      position = text.pos,
                      vjust = vjust,
                      hjust = hjust,
                      show.legend = FALSE)
        }
      } else if (showCountValues) {
        ggvaluelabels <-
          geom_text(aes(y = frq + y_offset, label = sprintf("%i", frq)),
                    position = text.pos,
                    hjust = hjust,
                    vjust = vjust,
                    show.legend = FALSE)
      } else if (showPercentageValues) {
        ggvaluelabels <-
          geom_text(aes(y = frq + y_offset, label = sprintf("%.01f%%", prz)),
                    position = text.pos,
                    hjust = hjust,
                    vjust = vjust,
                    show.legend = FALSE)
      } else {
        ggvaluelabels <- geom_text(aes(y = frq), label = "", show.legend = FALSE)
      }
    }
  } else {
    ggvaluelabels <- geom_text(aes(y = frq), label = "", show.legend = FALSE)
  }
  # --------------------------------------------------------
  # Set up grid breaks
  # --------------------------------------------------------
  if (is.null(gridBreaksAt)) {
    gridbreaks <- ggplot2::waiver()
  } else {
    gridbreaks <- seq(lower_lim, upper_lim, by = gridBreaksAt)
  }
  # ----------------------------------
  # Print plot
  # ----------------------------------
  if (type == "lines") {
    # line plot need numeric x-scale
    mydf$xpos <- sjmisc::to_value(mydf$xpos, keep.labels = FALSE)
    # lines need colour aes
    baseplot <-
      ggplot(mydf, aes(x = xpos, y = frq, colour = group)) + geob
    scalex <- scale_x_continuous()
  } else if (type == "boxplots" || type == "violin") {
    if (is.null(interactionVar)) {
      baseplot <- ggplot(mydf,aes(x = group,
                                  y = frq,
                                  fill = group,
                                  weight = wb)) + geob
      scalex <- scale_x_discrete(labels = axisLabels.x)
    } else {
      baseplot <- ggplot(mydf, aes(x = interaction(ia, group),
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
      baseplot <- ggplot(mydf, aes(x = xpos,
                                   y = frq,
                                   colour = group))
      # ---------------------------------------------------------
      # check whether we have dots plotted, and if so, use annotation
      # We have to use annotation first, because the diagram's layers are plotted
      # in the order as they're passed to the ggplot-command. Since we don't want the
      # shaded rectangles to overlay the dots, we add them first
      # ---------------------------------------------------------
      if (!is.null(ganno) && !facet.grid)
        baseplot <- baseplot + ganno
    } else {
      baseplot <- ggplot(mydf, aes(x = xpos,
                                   y = frq,
                                   fill = group))
    }
    # add geom
    baseplot <- baseplot + geob
    # define x acis
    scalex <- scale_x_discrete(labels = axisLabels.x)
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
    # show absolute and percentage values for each bar
    ggvaluelabels +
    # add labels to x- and y-axis, and diagram title
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
  # check whether coordinates should be flipped
  if (coord.flip) baseplot <- baseplot + coord_flip()
  # --------------------------------------------------
  # Here we start when we have a faces grid instead of
  # a grouped bar plot.
  # --------------------------------------------------
  if (facet.grid) {
    baseplot <- baseplot +
      # set font size for axes.
      theme(strip.text = element_text(face = "bold", size = rel(1.2))) +
      facet_wrap(~group)
  }
  # ---------------------------------------------------------
  # set geom colors
  # ---------------------------------------------------------
  baseplot <-
    sj.setGeomColors(baseplot,
                     geom.colors,
                     length(legendLabels),
                     ifelse(isTRUE(hideLegend), FALSE, TRUE),
                     legendLabels)
  # ----------------------------------
  # Plot integrated bar chart here
  # ----------------------------------
  if (printPlot) graphics::plot(baseplot)
  # -------------------------------------
  # return results
  # -------------------------------------
  invisible(structure(class = c("sjp", "sjpgrpfrq"),
                      list(plot = baseplot,
                           df = mydat)))
}
