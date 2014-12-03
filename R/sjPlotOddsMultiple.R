# bind global variables
if(getRversion() >= "2.15.1") utils::globalVariables(c("OR", "lower", "upper", "p", "pa", "shape"))

#' @title Plot odds ratios (forest plots) of multiple fitted glm's
#' @name sjp.glmm
#' 
#' @description Plot odds ratios (forest plots) of multiple fitted glm's with confidence intervalls in one plot.
#' 
#' @seealso \itemize{
#'            \item \code{\link{sjp.glm}}
#'            \item \code{\link{sjt.glm}}
#'            \item \code{\link{sjp.glm.ma}}
#'            \item \code{\link{sjp.lmm}}
#'            }
#' 
#' @param ... One or more fitted glm-objects.
#' @param title Diagram's title as string.
#'          Example: \code{title=c("my title")}
#' @param labelDependentVariables Labels of the dependent variables of all fitted models
#'          which have been used as first parameter(s), provided as char vector.
#' @param legendDepVarTitle A character vector used for the title of the dependent variable's legend.
#'          Default is \code{"Dependent Variables"}.
#' @param legendPValTitle A character vector used for the title of the significance level's legend.
#'          Default is \code{"p-level"}. Only applies if \code{usePShapes} is \code{TRUE}.
#' @param stringModel String constant used as legend text for the model names in case no 
#'          labels for the dependent variables are provided (see \code{labelDependentVariables}).
#'          Default is \code{"Model"}.
#' @param axisLabels.y Labels of the predictor variables (independent vars, odds) that are used for labelling the
#'          axis. Passed as vector of strings.
#'          Example: \code{axisLabels.y=c("Label1", "Label2", "Label3")}
#'          Note: If you use the \code{\link{sji.SPSS}} function and the \code{\link{sji.getValueLabels}} function, you receive a
#'          \code{list} object with label strings. The labels may also be passed as list object. They will be unlisted and
#'          converted to character vector automatically.
#' @param showAxisLabels.y Whether odds names (predictor labels) should be shown or not.
#' @param axisTitle.x A label ("title") for the x axis.
#' @param axisLimits Defines the range of the axis where the beta coefficients and their confidence intervalls
#'          are drawn. By default, the limits range from the lowest confidence interval to the highest one, so
#'          the diagram has maximum zoom. Use your own values as 2-value-vector, for instance: \code{limits=c(-0.8,0.8)}.
#' @param breakTitleAt Wordwrap for diagram title. Determines how many chars of the title are displayed in
#'          one line and when a line break is inserted into the title
#' @param breakLabelsAt Wordwrap for diagram labels. Determines how many chars of the category labels are displayed in 
#'          one line and when a line break is inserted
#' @param breakLegendAt Wordwrap for legend, i.e. names of the dependent variables of each fitted model.
#'          See parameter \code{labelDependentVariables}. Determines how many chars of each dependent variable name
#'          is displayed in one line in the legend and when a line break is inserted
#' @param gridBreaksAt Sets the breaks on the y axis, i.e. at every n'th position a major
#'          grid is being printed. Default is 0.5
#' @param transformTicks if \code{TRUE}, the grid bars have exponential distances, i.e. they
#'          visually have the same distance from one grid bar to the next. Default is \code{FALSE} which
#'          means that grids are plotted on every \code{gridBreaksAt}'s position, thus the grid bars
#'          become narrower with higher odds ratio values.
#' @param geom.size The size of the points that indicate the odds ratios. Default is 3.
#' @param geom.spacing Defines the space between the dots and error bars of the plotted fitted models. Default
#'          is 0.3.
#' @param geom.colors A vector with colors for representing the odds ratio values (i.e. points and error bars)
#'          of the different fitted models. Thus, the length of this vector must be equal to
#'          the length of supplied fitted models, so each model is represented by its own color.
#'          You can use:
#'          \itemize{
#'            \item If not specified, the qualitative \code{"Dark2"} color brewer palette will be used.
#'            \item If \code{"gs"}, a greyscale will be used.
#'            \item If \code{geom.colors} is any valid color brewer palette name, the related \href{http://colorbrewer2.org}{color brewer} palette will be used. Use \code{display.brewer.all()} from the \code{RColorBrewer} package to view all available palette names.
#'            }
#'          Else specify your own color values as vector (e.g. \code{geom.colors=c("#f00000", "#00ff00", "#0080ff")}).
#' @param nsAlpha The alpha level (transparancy) of non significant predicors. Points and error bars
#'          are affected by this value and plotted with a slight transparancy. Default is 1.
#' @param usePShapes If \code{TRUE}, significant levels are distinguished by different point shapes and a related
#'          legend is plotted. Default is \code{FALSE}.
#' @param interceptLineType The linetype of the intercept line (zero point). Default is \code{2} (dashed line).
#' @param interceptLineColor The color of the intercept line. Default value is \code{"grey70"}.
#' @param hideLegend Indicates whether legend (guide) should be shown or not.
#' @param coord.flip If \code{TRUE} (default), predictors are plotted on the left y-axis and estimate
#'          values are plotted on the x-axis.
#' @param showIntercept If \code{TRUE}, the intercept of the fitted model is also plotted.
#'          Default is \code{FALSE}. Please note that due to exp-transformation of
#'          estimates, the intercept in some cases can not be calculated, thus the
#'          function call is interrupted and no plot printed.
#' @param showValueLabels Whether the beta and standardized beta values should be plotted 
#'          to each dot or not.
#' @param labelDigits The amount of digits for rounding the estimations (see \code{showValueLabels}).
#'          Default is 2, i.e. estimators have 2 digits after decimal point.
#' @param showPValueLabels Whether the significance levels of each coefficient should be appended
#'          to values or not.
#' @param facet.grid \code{TRUE} when each model should be plotted as single facet instead of 
#'          an integrated single graph.
#' @param printPlot If \code{TRUE} (default), plots the results as graph. Use \code{FALSE} if you don't
#'          want to plot any graphs. In either case, the ggplot-object will be returned as value.
#' @return (Insisibily) returns the ggplot-object with the complete plot (\code{plot}) as well as the data frame that
#'           was used for setting up the ggplot-object (\code{df}).
#'          
#' @examples
#' # prepare dummy variables for binary logistic regression
#' y1 <- ifelse(swiss$Fertility<median(swiss$Fertility), 0, 1)
#' y2 <- ifelse(swiss$Infant.Mortality<median(swiss$Infant.Mortality), 0, 1)
#' y3 <- ifelse(swiss$Agriculture<median(swiss$Agriculture), 0, 1)
#' 
#' # Now fit the models. Note that all models share the same predictors
#' # and only differ in their dependent variable (y1, y2 and y3)
#' fitOR1 <- glm(y1 ~ swiss$Education+swiss$Examination+swiss$Catholic,
#'               family=binomial(link="logit"))
#' fitOR2 <- glm(y2 ~ swiss$Education+swiss$Examination+swiss$Catholic,
#'               family=binomial(link="logit"))
#' fitOR3 <- glm(y3 ~ swiss$Education+swiss$Examination+swiss$Catholic,
#'               family=binomial(link="logit"))
#' 
#' # plot multiple models
#' sjp.glmm(fitOR1, fitOR2, fitOR3, facet.grid=TRUE)
#' 
#' # plot multiple models with legend labels and point shapes instead of value  labels
#' sjp.glmm(fitOR1, fitOR2, fitOR3,
#'          labelDependentVariables=c("Fertility", "Infant Mortality", "Agriculture"),
#'          showValueLabels=FALSE,
#'          showPValueLabels=FALSE,
#'          usePShapes=TRUE,
#'          nsAlpha=0.2)
#' 
#' @import ggplot2
#' @export
sjp.glmm <- function(..., 
                    title=NULL,
                    labelDependentVariables=NULL, 
                    legendDepVarTitle="Dependent Variables",
                    legendPValTitle="p-level",
                    stringModel="Model",
                    axisLabels.y=NULL, 
                    axisTitle.x="Odds Ratios",
                    axisLimits=NULL,
                    breakTitleAt=50, 
                    breakLabelsAt=25,
                    breakLegendAt=20,
                    gridBreaksAt=0.5,
                    transformTicks=FALSE,
                    geom.size=3,
                    geom.spacing=0.4,
                    geom.colors="Dark2",
                    nsAlpha=1,
                    usePShapes=FALSE,
                    interceptLineType=2,
                    interceptLineColor="grey70",
                    coord.flip=TRUE,
                    showIntercept=FALSE,
                    showAxisLabels.y=TRUE,
                    showValueLabels=TRUE, 
                    labelDigits=2,
                    showPValueLabels=TRUE,
                    hideLegend=FALSE,
                    facet.grid=FALSE,
                    printPlot=TRUE) {
  # --------------------------------------------------------
  # retrieve list of fitted models
  # --------------------------------------------------------
  input_list <- list(...)
  # --------------------------------------------------------
  # unlist labels
  # --------------------------------------------------------
  # unlist axis labels (predictors)
  if (!is.null(axisLabels.y) && is.list(axisLabels.y)) {
    axisLabels.y <- unlistlabels(axisLabels.y)
  }
  # unlist labels of dependent variables (legend)
  if (!is.null(labelDependentVariables) && is.list(labelDependentVariables)) {
    labelDependentVariables <- unlistlabels(labelDependentVariables)
  }
  # ----------------------------
  # init final data frame
  # ----------------------------
  finalodds <- c()
  fitlength <- length(input_list)
  # ----------------------------
  # Prepare length of title and labels
  # ----------------------------
  # check length of diagram title and split longer string at into new lines
  if (!is.null(title)) {
    title <- sju.wordwrap(title, breakTitleAt)
  }
  # check length of x-axis title and split longer string at into new lines
  # every 50 chars
  if (!is.null(axisTitle.x)) {
    axisTitle.x <- sju.wordwrap(axisTitle.x, breakTitleAt)
  }
  # check length of dependent variables
  if (!is.null(labelDependentVariables)) {
    labelDependentVariables <- sju.wordwrap(labelDependentVariables, breakLegendAt)
  }
  else {
    # else if we have no labels of dependent variables supplied, use a 
    # default string (Model) for legend
    labelDependentVariables <- c(sprintf("%s %i", stringModel, 1:fitlength))
  }
  # check length of x-axis-labels and split longer strings at into new lines
  if (!is.null(axisLabels.y)) {
    axisLabels.y <- sju.wordwrap(axisLabels.y, breakLabelsAt)
  }
  # ----------------------------
  # iterate all fitted models
  # ----------------------------
  for (fitcnt in 1:fitlength) {
    # retrieve fitted model
    fit <- input_list[[fitcnt]]
    # ----------------------------
    # retrieve odds ratios (glm) 
    # ----------------------------
    # create data frame for ggplot
    odds <- data.frame(cbind(exp(coef(fit)), exp(confint(fit))))
    # ----------------------------
    # print p-values in bar charts
    # ----------------------------
    # retrieve sigificance level of independent variables (p-values)
    pv <- unname(coef(summary(fit))[,4])
    # for better readability, convert p-values to asterisks
    # with:
    # p < 0.001 = ***
    # p < 0.01 = **
    # p < 0.05 = *
    # retrieve odds ratios
    ov <- exp(coef(fit))
    # "ps" holds the p-value of the coefficients, including asterisks, as
    # string vector
    ps <- NULL
    # point shapes indicate different shapes for geom_point, according to
    # the p-level
    pointshapes <- NULL
    # palpha indicates whether a coefficient is significant or not.
    # non-significant values can be drawn with a lesser alpha-level
    # (i.e. are more transparent)
    palpha <- NULL
    for (i in 1:length(pv)) {
      ps[i] <- c("")
      pointshapes[i] <- 1
      palpha[i] <- "s"
    }
    # ----------------------------
    # copy OR-values into data column
    # ----------------------------
    if (showValueLabels) {
      for (i in 1:length(pv)) {
        ps[i] <- sprintf("%.*f", labelDigits, ov[i])
      }
    }
    # ----------------------------
    # copy p-values into data column
    # ----------------------------
    for (i in 1:length(pv)) {
      if (pv[i]>=0.05) {
        pointshapes[i] <- 1
        palpha[i] <- "ns"
      }
      else if (pv[i]>=0.01 && pv[i]<0.05) {
        if (showPValueLabels) {
          ps[i] <- paste(ps[i], "*")
        }
        pointshapes[i] <- 2
      }
      else if (pv[i]>=0.001 && pv[i]<0.01) {
        if (showPValueLabels) {
          ps[i] <- paste(ps[i], "**")
        }
        pointshapes[i] <- 3
      }
      else {
        if (showPValueLabels) {
          ps[i] <- paste(ps[i], "***")
        }
        pointshapes[i] <- 4
      }
    }  
    # ----------------------------
    # check if user defined labels have been supplied
    # if not, use variable names from data frame
    # ----------------------------
    if (is.null(axisLabels.y)) {
      axisLabels.y <- row.names(odds)
      #remove intercept from labels
      if (!showIntercept) {
        axisLabels.y <- axisLabels.y[-1]
      }
    }
    # ----------------------------
    # bind p-values to data frame
    # ----------------------------
    odds <- cbind(odds, ps, palpha, pointshapes, fitcnt)
    # set column names
    names(odds) <- c("OR", "lower", "upper", "p", "pa", "shape", "grp")
    # set x-position
    odds$xpos <- cbind(c(nrow(odds):1))
    odds$xpos <- as.factor(odds$xpos)
    #remove intercept from df
    if (!showIntercept) {
      odds <- odds[-1,]
    }
    # add data frame to final data frame
    finalodds <- rbind(finalodds, odds)
  }
  # convert to factor
  finalodds$xpos <- as.factor(finalodds$xpos)
  finalodds$grp <- as.factor(finalodds$grp)
  # convert to character
  finalodds$shape <- as.character(finalodds$shape)
  # reverse axislabel order, so predictors appear from top to bottom
  # as they appear in the console when typing "summary(fit)"
  axisLabels.y <- rev(axisLabels.y)
  # --------------------------------------------------------
  # Calculate axis limits. The range is from lowest lower-CI
  # to highest upper-CI, or a user defined range
  # --------------------------------------------------------
  if (is.null(axisLimits)) {
    # we have confindence intervals displayed, so
    # the range corresponds to the boundaries given by
    # the CI's
    upper_lim <- (ceiling(10*max(finalodds$upper))) / 10
    lower_lim <- (floor(10*min(finalodds$lower))) / 10
    # if we show p value labels, increase upper
    # limit of x axis, so labels are plotted inside
    # diagram range
    if (showValueLabels || showPValueLabels) {
      upper_lim <- upper_lim + 0.1
    }
    # give warnings when auto-limits are very low/high
    if ((lower_lim < 0.1) || (upper_lim > 100)) {
      warning("Exp. coefficients and/or exp. confidence intervals may be out of printable bounds. Consider using \"axisLimits\" parameter!")
    }
  }
  else {
    # Here we have user defind axis range
    lower_lim <- axisLimits[1]
    upper_lim <- axisLimits[2]
  }
  # --------------------------------------------------------
  # Define axis ticks, i.e. at which position we have grid
  # bars.
  # --------------------------------------------------------
  ticks<-c(seq(lower_lim, upper_lim, by=gridBreaksAt))
  if (!showAxisLabels.y) {
    axisLabels.y <- c("")
  }
  # --------------------------------------------------------
  # body of plot
  # --------------------------------------------------------
  # The order of aesthetics matters in terms of ordering the error bars!
  # Using alpha-aes before colour would order error-bars according to
  # alpha-level instead of colour-aes.
  plotHeader <- ggplot(finalodds, aes(y=OR, x=xpos, colour=grp, alpha=pa))
  # --------------------------------------------------------
  # start with dot-plotting here
  # first check, whether user wants different shapes for
  # different p-levels
  # --------------------------------------------------------
  if (usePShapes) {
    plotHeader <- plotHeader +
      # set shape aesthetic. we have to repeat the other aesthestics as well,
      # because otherwise the order of point shapes differes from the order
      # of error bars.
      # The order of aesthetics matters in terms of ordering the error bars!
      # Using shape before colour would order points according to shapes instead
      # of colour-aes.
      geom_point(aes(y=OR, x=xpos, colour=grp, shape=shape), size=geom.size, position=position_dodge(-geom.spacing)) +
      # and use a shape scale, in order to have a legend
      scale_shape_manual(values=c(1,16,17,15), labels=c("n.s.", "*", "**", "***"))
  }
  else {
    plotHeader <- plotHeader +
      geom_point(size=geom.size, position=position_dodge(-geom.spacing))
  }
  # --------------------------------------------------------
  # continue with errorbars, p-value-label and intercept line
  # --------------------------------------------------------
  plotHeader <- plotHeader +
    # print confidence intervalls (error bars)
    geom_errorbar(aes(ymin=lower, ymax=upper), width=0, position=position_dodge(-geom.spacing)) +
    # print value labels and p-values
    geom_text(aes(label=p, y=upper), position=position_dodge(width=-geom.spacing), hjust=-0.1) +
    # Intercept-line
    geom_hline(yintercept=1, linetype=interceptLineType, color=interceptLineColor) +
    labs(title=title, x=NULL, y=axisTitle.x, shape=legendPValTitle, colour=legendDepVarTitle) +
    scale_x_discrete(labels=axisLabels.y) +
    # use transparancy if requested, but hide legend
    scale_alpha_manual(values=c(nsAlpha,1.0), guide="none")
  # --------------------------------------------------------
  # create pretty breaks for log-scale
  # --------------------------------------------------------
  if (transformTicks) {
    # since the odds are plotted on a log-scale, the grid bars'
    # distance shrinks with higher odds values. to provide a visual
    # proportional distance of the grid bars, we can apply the
    # exponential-function on the tick marks
    plotHeader <- plotHeader +
      scale_y_continuous(trans = "log10",
                         limits = c(lower_lim, upper_lim), 
                         breaks = base_breaks(upper_lim),
                         labels = prettyNum)
  }
  else {
    plotHeader <- plotHeader +
      # logarithmic scale for odds
      # logarithmic scale for odds
      scale_y_log10(limits = c(lower_lim, upper_lim), 
                    breaks = ticks, 
                    labels = ticks)
  }
  # --------------------------------------------------------
  # flip coordinates?
  # --------------------------------------------------------
  if (coord.flip)  {
    plotHeader <- plotHeader +
      coord_flip()
  }
  if (facet.grid) {
    plotHeader <- plotHeader + facet_grid(.~grp)
  }  
  # ---------------------------------------------------------
  # set geom colors
  # ---------------------------------------------------------
  plotHeader <- sj.setGeomColors(plotHeader, geom.colors, length(labelDependentVariables), ifelse(hideLegend==TRUE, FALSE, TRUE), labelDependentVariables)
  # ---------------------------------------------------------
  # Check whether ggplot object should be returned or plotted
  # ---------------------------------------------------------
  if (printPlot) print(plotHeader)
  # -------------------------------------
  # return results
  # -------------------------------------
  invisible (structure(class = "sjpglmm",
                       list(plot = plotHeader,
                            df = finalodds)))
}
