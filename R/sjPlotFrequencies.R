# bind global variables
utils::globalVariables(c("frq", "grp", "upper.ci", "lower.ci", "ia", "..density.."))


#' @title Plot frequencies of (count) variables
#' @name sjp.frq
#' 
#' @seealso \itemize{
#'            \item \href{http://www.strengejacke.de/sjPlot/sjp.frq/}{sjPlot manual: sjp.frq}
#'            \item \code{\link{sjt.frq}}
#'          }
#' 
#' @description Plot frequencies of a (count) variable as bar graph, histogram,
#'                box plot etc.
#' 
#' @note This function only works with variables with integer values, i.e. scales / centred variables
#'         with decimales may result in unexpected behaviour.
#' 
#' @param varCount a vector of values (variable) describing the bars which make up the plot.
#' @param weightBy weight factor that will be applied to weight all cases of \code{varCount}.
#'          Must be a vector of same length as \code{varCount}. Default is \code{NULL}, so no weights are used.
#' @param interactionVar an interaction variable which can be used for box plots. Divides the observations in 
#'          \code{varCount} into sub groups indicated by \code{interactionVar}. Only 
#'          applies when \code{type = "boxplots"} or \code{"violins"}.
#' @param sort.frq Determines whether categories (bars) on x-axis should be sorted 
#'          according to the frequencies or not.  Default is \code{"none"}, so 
#'          categories are not sorted by frequency. Use \code{"asc"} or
#'          \code{"desc"} for sorting categories ascending or descending order.
#' @param type Specifies the type of distribution plot that will be plotted.
#'          \describe{
#'            \item{\code{"bars"}}{for simple bars (the default setting)}
#'            \item{\code{"dots"}}{for a dot plot}
#'            \item{\code{"histogram"}}{for a histogram}
#'            \item{\code{"lines"}}{for a line-styled histogram with filled area}
#'            \item{\code{"density"}}{for a density plot}
#'            \item{\code{"boxplots"}}{for box plots}
#'            \item{\code{"violins"}}{for violin plots}
#'            }
#'            You may use initial letter for \code{type} options, except for
#'            \code{type = "dots"} and \code{type = "boxplots"}, which may be abbreviated
#'            with \code{type = "dot"} and \code{type = "box"}
#' @param geom.colors user defined color for geoms, e.g. \code{geom.colors = "#0080ff"}.
#' @param interactionVarLabels a character vector with labels for the x-axis breaks 
#'          when having interaction variables included. These labels replace the 
#'          \code{axisLabels.x}. Only applies, when using box or violin plots
#'          (i.e. \code{type = "boxplots"} or \code{"violins"}) and \code{interactionVar} 
#'          is not \code{NULL}.
#' @param showCI logical, whether or not confidence intervals should be plotted. 
#'          Only applies to \code{type = "dots"} or \code{type = "bars"}.
#' @param error.bar.color color of confidence interval bars (error bars). 
#'          Only applies to \code{type = "bars"}. In case of dot plots, error bars 
#'          will have same colors as dots (see \code{geom.colors}).
#' @param showMeanIntercept logical, if \code{TRUE}, a vertical line in histograms 
#'          is drawn to indicate the mean value of the count variables. Only 
#'          applies to histogram-charts.
#' @param showMeanValue logical, if \code{TRUE} (default), the mean value 
#'          is printed to the vertical line that indicates the mean value
#'          of the count variables. Only applies to histogram-charts.
#' @param showStandardDeviation logical, if \code{TRUE}, the standard deviation 
#'          is annotated as shaded rectangle around the mean intercept
#'          line. Only applies to histogram-charts.
#' @param meanInterceptLineType numeric value, indicating the linetype of the mean 
#'          intercept line. Only applies to histogram-charts and 
#'          when \code{showMeanIntercept = TRUE}.
#' @param meanInterceptLineSize numeric, size of the mean intercept line. Only 
#'          applies to histogram-charts and when \code{showMeanIntercept = TRUE}.
#' @param showNormalCurve logical, if \code{TRUE}, a normal curve, which is adjusted to the data,
#'          is plotted over the histogram or density plot. Default is
#'          \code{FALSE}. Only applies when histograms or density plots are plotted (see \code{type}).
#' @param showStandardNormalCurve logical, if \code{TRUE}, a normal curve, which is not 
#'          adjusted to the data (thus representing a "true" standard normal curve, 
#'          which is, however, still an approximation), is plotted over the histogram 
#'          or density plot. Default is \code{FALSE}. Only applies when 
#'          histograms or density plots are plotted (see \code{type}).
#' @param adjustNormalCurve.x logical, if \code{TRUE} and \code{showStandardNormalCurve} is also \code{TRUE}, the 
#'          x-axis-start of the standard normal curve starts with the x-axis limits of the graph. This
#'          is only necessary, if minimum value of \code{varCount} is larger than 0 or 1.
#' @param normalCurveColor color of the normal curve line. Only
#'          applies if \code{showNormalCurve = TRUE}.
#' @param normalCurveSize numeric, size of the normal curve line. Only
#'          applies if \code{showNormalCurve = TRUE}.
#' @param normalCurveAlpha transparancy level (alpha value) of the normal curve. Only
#'          applies if \code{showNormalCurve = TRUE}.
#' @param axisTitle.x title for the x-axis. By default, the variable name will be 
#'          automatically detected and used as title (see \code{\link[sjmisc]{set_label}}) 
#'          for details).
#' @param axisTitle.y title for the y-axis. By default, this value is \code{NULL},
#'          i.e. no title is printed.
#' @param hist.skipZeros logical, if \code{TRUE}, zero counts (categories with no answer) 
#'          in \code{varCount} are omitted when drawing histrograms, and the mapping 
#'          is changed to \code{\link[ggplot2]{stat_bin}}. Only applies to  histograms. 
#'          Use this argument to get similar results to the default \code{\link[ggplot2]{qplot}} 
#'          or \code{\link[ggplot2]{geom_histogram}} histogram plots of ggplot. You may need
#'          to adjust the \code{geom.size} argument for better visual results 
#'          (which, by ggplot-default, is 1/30 of the x-axis-range).
#' @param autoGroupAt numeric value, indicating at which length of unique values of \code{varCount}, 
#'          automatic grouping into smaller units is done (see \code{\link[sjmisc]{group_var}}).
#'          If \code{varCount} has large numbers of unique values, there may be too many bars 
#'          for the plot. Hence it's practical to group such variables. For example, 
#'          if \code{autoGroupAt = 50} and \code{varCount} has more than 50 unique values,
#'          it will be grouped (using the \code{\link[sjmisc]{group_var}} function). 
#'          Default value for \code{autoGroupAt} is \code{NULL}, i.e. auto-grouping is off.
#'          See \code{\link[sjmisc]{group_var}} for examples on grouping.
#' @param labelPos string, indicating the position of value labels, when \code{coord.flip = TRUE}.
#'          Can be either \code{"inside"} or \code{"outside"} (default). You may specify
#'          initial letter only. If \code{coord.flip = FALSE}, this argument will
#'          be ignored.
#'          
#' @inheritParams sjp.grpfrq
#' 
#' @return (Insisibily) returns the ggplot-object with the complete plot (\code{plot}) as well as the data frame that
#'           was used for setting up the ggplot-object (\code{mydf}).
#' 
#' @examples
#' # ---------------
#' # boxplot
#' # ---------------
#' sjp.frq(ChickWeight$weight, type = "box")
#' 
#' # ---------------
#' # histogram
#' # ---------------
#' sjp.frq(discoveries, type = "hist", showMeanIntercept = TRUE)
#' # histogram with minimal theme and w/o labels
#' sjp.frq(discoveries, type = "hist",
#'         showMeanIntercept = TRUE,
#'         showValueLabels = FALSE)
#'         
#' # ---------------
#' # violin plot
#' # ---------------
#' sjp.frq(ChickWeight$weight, type = "v")
#' 
#' # ---------------
#' # bar plot
#' # ---------------
#' sjp.frq(ChickWeight$Diet)
#' 
#' # ---------------
#' # bar plot with EUROFAMCARE sample dataset
#' # dataset was importet from an SPSS-file, using:
#' # efc <- sjmisc::read_spss("efc.sav", enc = "UTF-8")
#' # ---------------
#' library(sjmisc)
#' data(efc)
#' efc.val <- get_labels(efc)
#' efc.var <- get_label(efc)
#' # you may use sjp.setTheme here to change axis textangle
#' sjp.frq(as.factor(efc$e15relat), 
#'         title = efc.var[['e15relat']],
#'         axisLabels.x = efc.val['e15relat'])
#' 
#' # bar plot with EUROFAMCARE sample dataset
#' # grouped variable
#' ageGrp <- group_var(efc$e17age)
#' ageGrpLab <- group_labels(efc$e17age)
#' sjp.frq(ageGrp,
#'         title = efc.var[['e17age']],
#'         axisLabels.x = ageGrpLab)
#' 
#' # ---------------
#' # box plots with interaction variable
#' # the following example is equal to the function call
#' # sjp.grpfrq(efc$e17age, efc$e16sex, type = "box")
#' # ---------------
#' sjp.frq(efc$e17age,
#'         title = paste(efc.var[['e17age']], 
#'                       "by", 
#'                       efc.var[['e16sex']]),
#'         interactionVar = efc$e16sex,
#'         interactionVarLabels = efc.val['e16sex'],
#'         type = "box")
#' 
#' # -------------------------------------------------
#' # auto-detection of value labels and variable names
#' # -------------------------------------------------
#' # negative impact scale, ranging from 7-28, assuming that
#' # variable scale (lowest value) starts with 1
#' sjp.frq(efc$neg_c_7, startAxisAt = 1)
#' 
#' # negative impact scale, ranging from 7-28, using
#' # automatic detection of start index of x-axis
#' sjp.frq(efc$neg_c_7)
#' 
#' # -------------------------------------------------
#' # plotting confidence intervals
#' # -------------------------------------------------
#' sjp.frq(efc$e15relat,
#'         type = "dots",
#'         showCI = TRUE,
#'         sort.frq = "desc",
#'         coord.flip = TRUE)
#' 
#' # -------------------------------------------------
#' # Simulate ggplot-default histogram, using "hist.skipZeros"
#' # and adjusted "geom.size".
#' # -------------------------------------------------
#' sjp.frq(efc$c160age, 
#'         type = "h", 
#'         hist.skipZeros = TRUE, 
#'         geom.size = 1)
#' 
#'   
#' @import ggplot2
#' @import sjmisc
#' @importFrom stats na.omit sd weighted.mean
#' @export
sjp.frq <- function(varCount,
                    title = "",
                    weightBy = NULL,
                    weightByTitleString = NULL,
                    interactionVar = NULL,
                    sort.frq = "none",
                    type = "bars",
                    geom.size = NULL,
                    geom.colors = NULL,
                    axisLabels.x = NULL,
                    interactionVarLabels = NULL,
                    axisLimits.x = NULL,
                    axisLimits.y = NULL,
                    breakTitleAt = 50,
                    breakLabelsAt = 20,
                    gridBreaksAt = NULL,
                    innerBoxPlotWidth = 0.15,
                    innerBoxPlotDotSize = 3,
                    expand.grid = FALSE,
                    showValueLabels = TRUE,
                    showCountValues = TRUE,
                    showPercentageValues = TRUE,
                    showAxisLabels.x = TRUE,
                    showAxisLabels.y = TRUE,
                    showCI = FALSE,
                    error.bar.color = "darkred",
                    showMeanIntercept = FALSE,
                    showMeanValue = TRUE,
                    showStandardDeviation = TRUE,
                    showNormalCurve = FALSE,
                    showStandardNormalCurve = FALSE,
                    adjustNormalCurve.x = FALSE,
                    meanInterceptLineType = 2,
                    meanInterceptLineSize = 0.5,
                    normalCurveColor = "red",
                    normalCurveSize = 0.8,
                    normalCurveAlpha = 0.4,
                    axisTitle.x = NULL,
                    axisTitle.y = NULL,
                    startAxisAt = "auto",
                    hist.skipZeros = FALSE,
                    autoGroupAt = NULL,
                    coord.flip = FALSE,
                    labelPos = "outside",
                    na.rm = TRUE,
                    printPlot = TRUE) {
  # --------------------------------------------------------
  # try to automatically set labels is not passed as argument
  # --------------------------------------------------------
  if (is.null(axisLabels.x)) axisLabels.x <- sjmisc:::autoSetValueLabels(varCount)
  if (is.null(interactionVarLabels) && !is.null(interactionVar)) interactionVarLabels <- sjmisc:::autoSetValueLabels(interactionVar)
  if (is.null(axisTitle.x)) axisTitle.x <- sjmisc:::autoSetVariableLabels(varCount)
  if (is.null(title)) title <- sjmisc:::autoSetVariableLabels(varCount)
  # --------------------------------------------------------
  # remove titles if empty
  # --------------------------------------------------------
  if (!is.null(axisTitle.x) && axisTitle.x == "") axisTitle.x <- NULL
  if (!is.null(title) && title == "") title <- NULL    
  # --------------------------------------------------------
  # check color argument
  # --------------------------------------------------------
  if (is.null(geom.colors)) {
    geom.colors <- ggplot2::waiver()
  } else if (length(geom.colors) > 1) {
    geom.colors <- geom.colors[1]
  }
  # --------------------------------------------------------
  # save label values. needed later to determine correct
  # amount of categories
  # --------------------------------------------------------
  labelvalues <- sjmisc::get_values(varCount)
  # --------------------------------------------------------
  # count variable may not be a factor!
  # --------------------------------------------------------
  if (is.factor(varCount)) varCount <- sjmisc::to_value(varCount, keep.labels = F)
  # --------------------------------------------------------
  # We have several options to name the plot type
  # Here we will reduce it to a unique value
  # --------------------------------------------------------
  if (type == "b" || type == "bar") type <- "bars"
  if (type == "dot") type <- "dots"
  if (type == "h" || type == "hist") type <- "histogram"
  if (type == "d" || type == "density") type <- "dens"
  if (type == "l" || type == "lines") type <- "line"
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
      geom.size <- 0.7
    else if (type == "dots") 
      geom.size <- 3
    else if (type == "hist") 
      geom.size <- .7
    else if (type == "line") 
      geom.size <- .8
    else if (type == "box") 
      geom.size <- .3
    else if (type == "violin") 
      geom.size <- .3
  }
  #---------------------------------------------------
  # check whether variable should be auto-grouped
  #---------------------------------------------------
  if (!is.null(interactionVar) && type != "boxplots" && type != "violin") {
    warning("'interactionVar' only applies to boxplots and violinplots (see 'type') and will be ignored.", call. = F)
    interactionVar <- NULL
  }
  #---------------------------------------------------
  # check whether variable should be auto-grouped
  #---------------------------------------------------
  if (!is.null(autoGroupAt) && length(unique(varCount)) >= autoGroupAt) {
    message(sprintf("Variable has %i unique values and was grouped...", length(unique(varCount))))
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
    # set label attributes
    varCount <- sjmisc::set_labels(varCount, axisLabels.x)
    # --------------------------------------------------------
    # save label values. needed later to determine correct
    # amount of categories
    # --------------------------------------------------------
    labelvalues <- sjmisc::get_values(varCount)
  }
  # --------------------------------------------------------
  # unlist labels
  # --------------------------------------------------------
  if (!is.null(axisLabels.x) && is.list(axisLabels.x)) axisLabels.x <- unlistlabels(axisLabels.x)
  if (!is.null(interactionVarLabels) && is.list(interactionVarLabels)) interactionVarLabels <- unlistlabels(interactionVarLabels)
  #---------------------------------------------------
  # create frequency data frame
  #---------------------------------------------------
  df.frq <- create.frq.df(varCount, 
                          axisLabels.x, 
                          labelvalues,
                          breakLabelsAt, 
                          sort.frq, 
                          2, 
                          na.rm, 
                          startAxisAt, 
                          weightBy)
  mydat <- df.frq$mydat
  axisLabels.x <- df.frq$labels
  catmin <- df.frq$catmin
  # --------------------------------------------------------
  # add confidence intervals. first,  check whether 
  # we have removed NA's. If so, replace NA's, so
  # we get confidence intervals for CI as well
  # --------------------------------------------------------
  if (!na.rm) 
    varCount.ci <- sjmisc::replace_na(varCount, max(varCount, na.rm = T) + 1)
  else
    varCount.ci <- varCount
  # get frequencies with upper/lower CI
  df.frqci <- sjs.frqci(varCount.ci)$mydat.frq
  # find matching confidence intervals
  ci.match <- match(mydat$var, df.frqci$var)
  # copy ci
  mydat$lower.ci <- round(df.frqci$lower.ci[ci.match])
  mydat$upper.ci <- round(df.frqci$upper.ci[ci.match])
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
  # check length of x-axis title and split longer string at into new lines
  # every 50 chars
  if (!is.null(axisTitle.x)) {
    axisTitle.x <- sjmisc::word_wrap(axisTitle.x, breakTitleAt)    
  }
  # check length of x-axis title and split longer string at into new lines
  # every 50 chars
  if (!is.null(axisTitle.y)) {
    axisTitle.y <- sjmisc::word_wrap(axisTitle.y, breakTitleAt)    
  }
  # check length of x-axis-labels of interaction variable and split 
  # longer strings into new lines
  if (!is.null(interactionVar)) {
    if (!is.null(interactionVarLabels)) {
      interactionVarLabels <- sjmisc::word_wrap(interactionVarLabels, breakLabelsAt)    
    # If interaction-variable-labels were not defined, simply set numbers from 1 to
    # amount of categories instead
    } else {
      iavarLabLength <- length(unique(stats::na.omit(interactionVar)))
      interactionVarLabels <- c(1:iavarLabLength)
    }
  }
  # --------------------------------------------------------
  # If we have a histogram, caluclate means of groups
  # --------------------------------------------------------
  if (is.null(weightBy)) {
    mittelwert <- mean(varCount, na.rm = TRUE)
  } else {
    mittelwert <- stats::weighted.mean(varCount, weightBy, na.rm = TRUE)
  }
  stddev <- sd(varCount, na.rm = TRUE)
  # --------------------------------------------------------
  # If we have boxplots, use different data frame structure
  # --------------------------------------------------------
  if (type == "boxplots" || type == "violin") {
    if (is.null(interactionVar)) {
      mydat <- stats::na.omit(data.frame(cbind(grp = 1, 
                                               frq = varCount, 
                                               var = varCount)))
    } else {
      mydat <- stats::na.omit(data.frame(cbind(grp = 1, 
                                               ia = interactionVar, 
                                               frq = varCount, 
                                               var = varCount)))
      mydat$ia <- as.factor(mydat$ia)
    }
    mydat$grp <- as.factor(mydat$grp)
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
    } else {
      # ... or the amount of max. answers per category
      # add 5% margin to upper limit
      upper_lim <- max(pretty(table(varCount) * 1.05))
    }
  }
  # --------------------------------------------------------
  # define geom
  # --------------------------------------------------------
  if (type == "bars") {
    geob <- geom_bar(stat = "identity", 
                     width = geom.size, 
                     fill = geom.colors)
  } else if (type == "dots") {
    geob <- geom_point(size = geom.size, fill = geom.colors)
  }
  if (!showAxisLabels.x) axisLabels.x <- c("")
  # --------------------------------------------------------
  # Set value labels
  # --------------------------------------------------------
  if (coord.flip) {
    # adjust vertical position for labels, based on whether percentage values
    # are shown or not
    vert <- ggplot2::waiver() # ifelse((showPercentageValues == TRUE && showCountValues == TRUE), 0.5, 0.1)
    if (labelPos == "inside" || labelPos == "i") {
      hort <- 1.1
    } else {
      hort <- -0.1
    }
  } else {
    # adjust vertical position for labels, based on whether percentage values
    # are shown or not
    vert <- ifelse((showPercentageValues == TRUE && showCountValues == TRUE), -0.2, -0.6)
    hort <- ggplot2::waiver()
  }
  # --------------------------------------------------------
  # Set value labels
  # --------------------------------------------------------
  # don't display value labels when we have boxplots or violin plots
  if (type == "boxplots" || type == "violin") showValueLabels <- FALSE
  if (showValueLabels) {
    # here we have counts and percentages
    if (showPercentageValues && showCountValues) {
      if (coord.flip) {
        if (showCI) {
          ggvaluelabels <-  geom_text(label = sprintf("%i (%.01f%%)", mydat$frq, mydat$prz),
                                      hjust = hort,
                                      vjust = vert,
                                      aes(y = upper.ci))
        } else {
          ggvaluelabels <-  geom_text(label = sprintf("%i (%.01f%%)", mydat$frq, mydat$prz),
                                      hjust = hort,
                                      vjust = vert)
        }
      } else {
        if (showCI) {
          ggvaluelabels <-  geom_text(label = sprintf("%i\n(%.01f%%)", mydat$frq, mydat$prz),
                                      hjust = hort,
                                      vjust = vert,
                                      aes(y = upper.ci))
        } else {
          ggvaluelabels <-  geom_text(label = sprintf("%i\n(%.01f%%)", mydat$frq, mydat$prz),
                                      hjust = hort,
                                      vjust = vert)
        }
      }
    } else if (showCountValues) {
      if (showCI) {
        # here we have counts, without percentages
        ggvaluelabels <-  geom_text(label = sprintf("%i", mydat$frq),
                                    hjust = hort,
                                    vjust = vert,
                                    aes(y = upper.ci))
      } else {
        # here we have counts, without percentages
        ggvaluelabels <-  geom_text(label = sprintf("%i", mydat$frq),
                                    hjust = hort,
                                    vjust = vert)
      }
    } else if (showPercentageValues) {
      if (showCI) {
        # here we have counts, without percentages
        ggvaluelabels <-  geom_text(label = sprintf("%.01f%%", mydat$prz),
                                    hjust = hort,
                                    vjust = vert,
                                    aes(y = upper.ci))
      } else {
        # here we have counts, without percentages
        ggvaluelabels <-  geom_text(label = sprintf("%.01f%%", mydat$prz),
                                    hjust = hort,
                                    vjust = vert)
      }
    } else {
      # no labels
      ggvaluelabels <-  geom_text(label = "")
    }
  } else {
    # no labels
    ggvaluelabels <-  geom_text(label = "")
  }
  # --------------------------------------------------------
  # Set up grid breaks
  # --------------------------------------------------------
  maxx <- max(mydat$var) + 1
  if (is.null(gridBreaksAt)) {
    gridbreaks <- ggplot2::waiver()
    histgridbreaks <- ggplot2::waiver()
  } else {
    gridbreaks <- c(seq(lower_lim, upper_lim, by = gridBreaksAt))
    histgridbreaks <- c(seq(lower_lim, maxx, by = gridBreaksAt))
  }
  # ----------------------------------
  # set x-axis limits
  # ----------------------------------
  if (is.null(axisLimits.x)) axisLimits.x <- c(catmin, maxx)
  # ----------------------------------
  # set y scaling and label texts
  # ----------------------------------
  # set Y-axis, depending on the calculated upper y-range.
  # It either corresponds to the maximum amount of cases in the data set
  # (length of var) or to the highest count of var's categories.
  if (showAxisLabels.y) {
    yscale <- scale_y_continuous(limits = c(lower_lim, upper_lim), 
                                 expand = expand.grid, 
                                 breaks = gridbreaks)
  } else {
    yscale <- scale_y_continuous(limits = c(lower_lim, upper_lim), 
                                 expand = expand.grid, 
                                 breaks = gridbreaks, 
                                 labels = NULL)
  }
  # ----------------------------------
  # Print plot
  # ----------------------------------
  # calculate mean and sd for non-adjusted normal curve
  stdmean <- diff(range(varCount, na.rm = TRUE)) / 2
  stdadjust <- min(varCount, na.rm = TRUE)
  stdsd <- stdmean / 4
  stdlen <- length(stats::na.omit(varCount))
  # ----------------------------------
  # Check how many categories we have on the x-axis.
  # If it exceeds the user defined limits, plot
  # histrogram instead of bar chart
  # ----------------------------------
  if (type == "bars" || type == "dots") {
    # mydat is a data frame that only contains one variable (var).
    # Must be declared as factor, so the bars are central aligned to
    # each x-axis-break. 
    baseplot <- ggplot(mydat, aes(x = factor(var), y = frq)) + 
      geob +
      yscale + 
      # remove guide / legend
      guides(fill = FALSE) +
      # show absolute and percentage value of each bar.
      ggvaluelabels +
      # print value labels to the x-axis.
      # If argument "axisLabels.x" is NULL, the category numbers (1 to ...) 
      # appear on the x-axis
      scale_x_discrete(labels = axisLabels.x)
    if (showCI) {
      ebcol <- ifelse(type == "dots", geom.colors, error.bar.color)
      # print confidence intervalls (error bars)
      baseplot <- baseplot + geom_errorbar(aes(ymin = lower.ci, ymax = upper.ci), 
                                           colour = ebcol, 
                                           width = 0)
    }
    # check whether coordinates should be flipped, i.e.
    # swap x and y axis
    if (coord.flip) baseplot <- baseplot + coord_flip()
  } else {
    # --------------------------------------------------
    # Here we start when we have a histogram instead of
    # bar plot.
    # --------------------------------------------------
    # Start density plot here
    # --------------------------------------------------
    if (type == "boxplots" || type == "violin") {
      if (is.null(interactionVar)) {
        baseplot <- ggplot(mydat, aes(x = grp, y = frq))
        scalex <- scale_x_discrete(labels = "")
      } else {
        baseplot <- ggplot(mydat, aes(x = interaction(ia, grp), y = frq))
        scalex <- scale_x_discrete(labels = interactionVarLabels)
      }
      if (type == "boxplots") {
        baseplot <- baseplot + 
          geom_boxplot(width = geom.size, fill = geom.colors)
      } else {
        baseplot <- baseplot + 
          geom_violin(trim = trimViolin, 
                      width = geom.size, 
                      fill = geom.colors) +
          # if we have a violin plot, add an additional boxplot inside to show
          # more information
          geom_boxplot(width = innerBoxPlotWidth, 
                       fill = "white")
      }
      # if we have boxplots or violon plots, also add a point that indicates
      # the mean value
      # different fill colours, because violin boxplots have white background
      fcsp <- ifelse(type == "boxplots", "white", "black")
      baseplot <- baseplot +
        stat_summary(fun.y = "mean", 
                     geom = "point", 
                     shape = 21, 
                     size = innerBoxPlotDotSize, 
                     fill = fcsp)
      # no additional labels for the x- and y-axis, only diagram title
      baseplot <- baseplot + 
        yscale +
        scalex
    # --------------------------------------------------
    # Start density plot here
    # --------------------------------------------------
    } else if (type == "dens") {
      x <- stats::na.omit(varCount)
      densityDat <- data.frame(x)
      # First, plot histogram with density curve
      baseplot <- ggplot(densityDat, aes(x = x)) +
        geom_histogram(aes(y = ..density..), fill = geom.colors) +
        # transparent density curve above bars
        geom_density(aes(y = ..density..), 
                     fill = "cornsilk", 
                     alpha = 0.3) +
        # remove margins from left and right diagram side
        scale_x_continuous(expand = expand.grid, 
                           breaks = histgridbreaks,
                           limits = axisLimits.x)
      # check whether user wants to overlay the histogram
      # with a normal curve
      if (showNormalCurve) {
        baseplot <- baseplot +
          stat_function(fun = dnorm,
                        args = list(mean = mean(densityDat$x),
                                    sd = stats::sd(densityDat$x)),
                        colour = normalCurveColor,
                        size = normalCurveSize,
                        alpha = normalCurveAlpha)
      }
      if (showStandardNormalCurve) {
        baseplot <- baseplot +
          stat_function(fun = dnorm,
                        args = list(mean = stdmean, sd = stdsd),
                        colour = normalCurveColor,
                        size = normalCurveSize,
                        alpha = normalCurveAlpha)
      }
    } else {
      # -----------------------------------------------------------------
      # Since the density curve shows no absolute numbers (counts) on the
      # y-axis, have also the opportunity to plot "real" histrograms with 
      # counts on the y-axis
      # -----------------------------------------------------------------
      # base constructor
      if (hist.skipZeros) {
        x <- stats::na.omit(varCount)
        if (geom.size < round(diff(range(x)) / 50)) message("Using very small binwidth. Consider adjusting \"geom.size\" argument.")
        hist.dat <- data.frame(x)
        baseplot <- ggplot(mydat)
        basehist <- geom_histogram(data = hist.dat, 
                                   aes(x = x),
                                   binwidth = geom.size, 
                                   fill = geom.colors)
      } else {
        baseplot <- ggplot(mydat, aes(x = var, y = frq))
        basehist <- geom_histogram(stat = "identity", 
                                   binwidth = geom.size, 
                                   fill = geom.colors)
      }
      basehistline <- geom_area(alpha = 0.3)
      # check whether user wants line or bar histogram
      if (type == "line") {
        baseplot <- baseplot + basehistline + geom_line(colour = geom.colors)
      } else {
        baseplot <- baseplot + basehist
      }
      # check whether user wants to overlay the histogram
      # with a normal curve
      if (showNormalCurve) {
        baseplot <- baseplot +
          stat_function(fun = function(x, mean, sd, n) { n * dnorm(x = x, mean = mean, sd = sd) },
                        args = with(mydat, c(mean = mittelwert, sd = stddev, n = length(varCount))),
                        colour = normalCurveColor,
                        size = normalCurveSize,
                        alpha = normalCurveAlpha)
      }
      if (showStandardNormalCurve) {
        baseplot <- baseplot +
          stat_function(fun = function(x, mean, sd, n) { 
            if (adjustNormalCurve.x) x <- x - stdadjust
            n * dnorm(x = x, mean = mean, sd = sd)},
            args = with(mydat, c(mean = stdmean, sd = stdsd, n = stdlen)),
            colour = normalCurveColor,
            size = normalCurveSize,
            alpha = normalCurveAlpha)
      }
      # if we have a histogram, add mean-lines
      if (showMeanIntercept) {
        baseplot <- baseplot + 
          # vertical lines indicating the mean
          geom_vline(xintercept = mittelwert, 
                     linetype = meanInterceptLineType, 
                     size = meanInterceptLineSize)
        # check whether meanvalue should be shown.
        if (showMeanValue) {
          baseplot <- baseplot + 
            # use annotation instead of geomtext, because we need mean value only printed once
            annotate("text", 
                     x = mittelwert, 
                     y = upper_lim, 
                     parse = TRUE, 
                     label = paste("italic(bar(x)) == ", "'", c(round(mittelwert, 1)), "'"), 
                     hjust = 1.1, 
                     vjust = 2.2)
        }
        # check whether the user wants to plot standard deviation area
        if (showStandardDeviation) {
          baseplot <- baseplot +
            # first draw shaded rectangle. these are by default in grey colour with very high transparancy
            annotate("rect", 
                     xmin = mittelwert - stddev, 
                     xmax = mittelwert + stddev, 
                     ymin = 0, 
                     ymax = c(upper_lim), 
                     fill = "grey70", 
                     alpha = 0.2) +
            # draw border-lines for shaded rectangle
            geom_vline(xintercept = mittelwert - stddev, 
                       linetype = 3, 
                       size = meanInterceptLineSize, 
                       alpha = 0.7) +
            geom_vline(xintercept = mittelwert + stddev, 
                       linetype = 3, 
                       size = meanInterceptLineSize, 
                       alpha = 0.7)
          # if mean values are plotted, plot standard deviation values as well
          if (showMeanValue) {
            baseplot <- baseplot + 
              # use annotation instead of geomtext, because we need mean value only printed once
              annotate("text", 
                       x = mittelwert, 
                       y = upper_lim, 
                       label = sprintf("italic(s) == %.2f", round(stddev, 1)), 
                       parse = TRUE, 
                       hjust = 1.15, 
                       vjust = 4.2)
          }
        }
      }
      if (!hist.skipZeros) {
        # show absolute and percentage value of each bar.
        baseplot <- baseplot + ggvaluelabels
      }
      baseplot <- baseplot +
        # remove margins from left and right diagram side
        scale_x_continuous(limits = axisLimits.x, 
                           expand = expand.grid, 
                           breaks = histgridbreaks) +
        yscale
    }
  }
  # set axes text and 
  baseplot <- baseplot + labs(title = title, x = axisTitle.x, y = axisTitle.y)
  # ---------------------------------------------------------
  # Check whether ggplot object should be returned or plotted
  # ---------------------------------------------------------
  if (printPlot) plot(baseplot)
  # -------------------------------------
  # return results
  # -------------------------------------
  invisible(structure(class = "sjpfrq",
                      list(plot = baseplot,
                           mydf = mydat)))
}


# usage:
# df<-insertRowToDF(df,5,c(16,0)); # inserting the values (16,0) after the 5th row
insertRowToDF <- function(X, index_after, vector_to_insert){
  stopifnot(length(vector_to_insert) == ncol(X)) # to check valid row to be inserted
  X <- rbind(X[1:index_after,],
             vector_to_insert,
             X[(index_after + 1):nrow(X), ])
  row.names(X) <- 1:nrow(X)
  return(X)
}

