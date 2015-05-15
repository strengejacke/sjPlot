# bind global variables
if (getRversion() >= "2.15.1") utils::globalVariables(c("beta", "lower", "upper", "p", "pa", "shape"))

#' @title Plot beta coefficients of multiple fitted lm's
#' @name sjp.lmm
#' 
#' @description Plot beta coefficients (estimates) with confidence intervalls of multiple fitted linear models
#'                in one plot.
#'                
#' @param ... One or more fitted lm-objects. May also be a \code{\link{list}}-object with 
#'          fitted models, instead of separating each model with comma. See examples.
#' @param title Diagram's title as string.
#'          Example: \code{title = "my title"}
#' @param labelDependentVariables Labels of the dependent variables of all fitted models
#'          which have been used as first parameter(s), provided as char vector.
#' @param legendDepVarTitle A character vector used for the title of the dependent variable's legend.
#'          Default is \code{"Dependent Variables"}.
#' @param legendPValTitle A character vector used for the title of the significance level's legend.
#'          Default is \code{"p-level"}. Only applies if \code{usePShapes} is \code{TRUE}.
#' @param stringModel String constant used as legend text for the model names in case no 
#'          labels for the dependent variables are provided (see \code{labelDependentVariables}).
#'          Default is \code{"Model"}.
#' @param axisLabels.y Labels of the predictor variables (independent vars, betas) that are used for labelling the
#'          axis. Passed as vector of strings.
#'          Example: \code{axisLabels.y = c("Label1", "Label2", "Label3")} \cr
#'          \strong{Note:} If you use the \code{\link[sjmisc]{read_spss}} function and the \code{\link[sjmisc]{get_val_labels}} function, you receive a
#'          \code{list} object with label strings. The labels may also be passed as list object. They will be coerced
#'          to character vector automatically.
#' @param showAxisLabels.y Whether beta names (predictor labels) should be shown or not.
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
#'          grid is being printed. Default is \code{NULL}, so \code{\link{pretty}} gridbeaks will be used.
#' @param geom.size The size of the points that indicate the estimates. Default is 3.
#' @param geom.spacing Defines the space between the dots and error bars of the plotted fitted models. Default
#'          is 0.3.
#' @param geom.colors A vector with colors for representing the estimates (i.e. points and error bars)
#'          of the different fitted models. Thus, the length of this vector must be equal to
#'          the length of supplied fitted models, so each model is represented by its own color.
#'          You can use:
#'          \itemize{
#'            \item If not specified, the qualitative \code{"Dark2"} color brewer palette will be used.
#'            \item If \code{"gs"}, a greyscale will be used.
#'            \item If \code{geom.colors} is any valid color brewer palette name, the related \href{http://colorbrewer2.org}{color brewer} palette will be used. Use \code{display.brewer.all()} from the \code{RColorBrewer} package to view all available palette names.
#'            \item Else specify your own color values as vector (e.g. \code{geom.colors = c("#f00000", "#00ff00", "#0080ff")}).
#'            }
#' @param fade.ns if \code{TRUE}, non significant estimates will be printed in slightly faded colors.
#' @param usePShapes If \code{TRUE}, significant levels are distinguished by different point shapes and a related
#'          legend is plotted. Default is \code{FALSE}.
#' @param interceptLineType The linetype of the intercept line (zero point). Default is \code{2} (dashed line).
#' @param interceptLineColor The color of the intercept line. Default value is \code{"grey70"}.
#' @param hideLegend Indicates whether legend (guide) should be shown or not.
#' @param coord.flip If \code{TRUE} (default), predictors are plotted on the left y-axis and estimate
#'          values are plotted on the x-axis.
#' @param showIntercept If \code{TRUE}, the intercept of the fitted model is also plotted.
#'          Default is \code{FALSE}.
#' @param showValueLabels Whether the beta value labels should be plotted.
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
#' # Now fit the models. Note that all models share the same predictors
#' # and only differ in their dependent variable
#' library(sjmisc)
#' data(efc)
#' 
#' # fit three models
#' fit1 <- lm(barthtot ~ c160age + c12hour + c161sex + c172code, data = efc)
#' fit2 <- lm(neg_c_7 ~ c160age + c12hour + c161sex + c172code, data = efc)
#' fit3 <- lm(tot_sc_e ~ c160age + c12hour + c161sex + c172code, data = efc)
#' 
#' # plot multiple models
#' sjp.lmm(fit1, fit2, fit3, facet.grid = TRUE, fade.ns = FALSE)
#' 
#' # plot multiple models with legend labels and point shapes instead of value  labels
#' sjp.lmm(fit1, fit2, fit3,
#'          axisLabels.y = c("Carer's Age",
#'                           "Hours of Care", 
#'                           "Carer's Sex",
#'                           "Educational Status"),
#'          labelDependentVariables = c("Barthel Index", 
#'                                      "Negative Impact", 
#'                                      "Services used"),
#'          showValueLabels = FALSE,
#'          showPValueLabels = FALSE,
#'          usePShapes = TRUE)
#' 
#' # plot multiple models from nested lists parameter
#' all.models <- list()
#' all.models[[1]] <- fit1
#' all.models[[2]] <- fit2
#' all.models[[3]] <- fit3
#' 
#' sjp.lmm(all.models)
#' 
#' @import ggplot2
#' @import sjmisc
#' @export
sjp.lmm <- function(..., 
                     title=NULL,
                     labelDependentVariables=NULL, 
                     legendDepVarTitle="Dependent Variables",
                     legendPValTitle="p-level",
                     stringModel="Model",
                     axisLabels.y=NULL, 
                     axisTitle.x="Estimates",
                     axisLimits=NULL,
                     breakTitleAt=50, 
                     breakLabelsAt=25,
                     breakLegendAt=20,
                     gridBreaksAt=NULL,
                     geom.size=3,
                     geom.spacing=0.4,
                     geom.colors="Set1",
                     fade.ns=TRUE,
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
  # check length. if we have a list of fitted model, 
  # we need to "unlist" them
  # --------------------------------------------------------
  if (length(input_list) == 1 && class(input_list[[1]]) == "list") input_list <- lapply(input_list[[1]], function(x) x)
  # --------------------------------------------------------
  # unlist labels
  # --------------------------------------------------------
  # unlist axis labels (predictors)
  if (!is.null(axisLabels.y) && is.list(axisLabels.y)) axisLabels.y <- unlistlabels(axisLabels.y)
  # unlist labels of dependent variables (legend)
  if (!is.null(labelDependentVariables) && is.list(labelDependentVariables)) {
    labelDependentVariables <- unlistlabels(labelDependentVariables)
  }
  # ----------------------------
  # init final data frame
  # ----------------------------
  finalbetas <- c()
  fitlength <- length(input_list)
  # ----------------------------
  # Prepare length of title and labels
  # ----------------------------
  # check length of diagram title and split longer string at into new lines
  if (!is.null(title)) title <- sjmisc::word_wrap(title, breakTitleAt)
  # check length of x-axis title and split longer string at into new lines
  # every 50 chars
  if (!is.null(axisTitle.x)) axisTitle.x <- sjmisc::word_wrap(axisTitle.x, breakTitleAt)
  # check length of dependent variables
  if (!is.null(labelDependentVariables)) {
    labelDependentVariables <- sjmisc::word_wrap(labelDependentVariables, breakLegendAt)
  } else {
    # else if we have no labels of dependent variables supplied, use a 
    # default string (Model) for legend
    labelDependentVariables <- c(sprintf("%s %i", stringModel, 1:fitlength))
  }
  # check length of x-axis-labels and split longer strings at into new lines
  if (!is.null(axisLabels.y)) axisLabels.y <- sjmisc::word_wrap(axisLabels.y, breakLabelsAt)
  # ----------------------------
  # iterate all fitted models
  # ----------------------------
  for (fitcnt in 1:fitlength) {
    # retrieve fitted model
    fit <- input_list[[fitcnt]]
    # ----------------------------
    # retrieve beta's (lm)
    # ----------------------------
    betas <- data.frame(coef(fit), confint(fit))
    # ----------------------------
    # print p-values in bar charts
    # ----------------------------
    # retrieve sigificance level of independent variables (p-values)
    pv <- coef(summary(fit))[, 4]
    # for better readability, convert p-values to asterisks
    # with:
    # p < 0.001 = ***
    # p < 0.01 = **
    # p < 0.05 = *
    ov <- coef(fit)
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
    # copy beta-values into data column
    # ----------------------------
    ps <- rep("", length(ov))
    if (showValueLabels) ps <- sprintf("%.*f", labelDigits, ov)
    # ----------------------------
    # copy p-values into data column
    # ----------------------------
    for (i in 1:length(pv)) {
      if (pv[i] >= 0.05) {
        pointshapes[i] <- 1
        palpha[i] <- "ns"
      } else if (pv[i] >= 0.01 && pv[i] < 0.05) {
        if (showPValueLabels) ps[i] <- paste(ps[i], "*")
        pointshapes[i] <- 2
      } else if (pv[i] >= 0.001 && pv[i] < 0.01) {
        if (showPValueLabels) ps[i] <- paste(ps[i], "**")
        pointshapes[i] <- 3
      } else {
        if (showPValueLabels) ps[i] <- paste(ps[i], "***")
        pointshapes[i] <- 4
      }
    }  
    # ----------------------------
    # check if user defined labels have been supplied
    # if not, use variable names from data frame
    # ----------------------------
    if (is.null(axisLabels.y)) {
      axisLabels.y <- row.names(betas)
      #remove intercept from labels
      if (!showIntercept) axisLabels.y <- axisLabels.y[-1]
    }
    # ----------------------------
    # bind p-values to data frame
    # ----------------------------
    betas <- data.frame(betas, ps, palpha, pointshapes, fitcnt)
    # set column names
    colnames(betas) <- c("beta", "lower", "upper", "p", "pa", "shape", "grp")
    # set x-position
    betas$xpos <- c(nrow(betas):1)
    betas$xpos <- as.factor(betas$xpos)
    #remove intercept from df
    if (!showIntercept) betas <- betas[-1, ]
    # add data frame to final data frame
    finalbetas <- rbind(finalbetas, betas)
  }
  # convert to factor
  finalbetas$xpos <- as.factor(finalbetas$xpos)
  finalbetas$grp <- as.factor(finalbetas$grp)
  # convert to character
  finalbetas$shape <- as.character(finalbetas$shape)
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
    upper_lim <- ceiling(10 * max(finalbetas$upper)) / 10
    lower_lim <- floor(10 * min(finalbetas$lower)) / 10
    # if we show p value labels, increase upper
    # limit of x axis, so labels are plotted inside
    # diagram range
    if (showValueLabels || showPValueLabels) upper_lim <- upper_lim + 0.1
  } else {
    # Here we have user defind axis range
    lower_lim <- axisLimits[1]
    upper_lim <- axisLimits[2]
  }
  # --------------------------------------------------------
  # Define axis ticks, i.e. at which position we have grid
  # bars.
  # --------------------------------------------------------
  # determine gridbreaks
  if (is.null(gridBreaksAt)) {
    ticks <- pretty(c(lower_lim, upper_lim))
  } else {
    ticks <- c(seq(lower_lim, upper_lim, by = gridBreaksAt))
  }
  if (!showAxisLabels.y) axisLabels.y <- c("")
  # --------------------------------------------------------
  # body of plot
  # --------------------------------------------------------
  # The order of aesthetics matters in terms of ordering the error bars!
  # Using alpha-aes before colour would order error-bars according to
  # alpha-level instead of colour-aes.
  plotHeader <- ggplot(finalbetas, aes(y = beta, x = xpos, colour = grp, alpha = pa))
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
      geom_point(aes(shape = shape), 
                 size = geom.size, 
                 position = position_dodge(-geom.spacing)) +
      # and use a shape scale, in order to have a legend
      scale_shape_manual(values = c(1, 16, 17, 15), 
                         labels = c("n.s.", "*", "**", "***"))
  } else {
    plotHeader <- plotHeader +
      geom_point(size = geom.size, position = position_dodge(-geom.spacing))
  }
  # --------------------------------------------------------
  # fade non-significant estimates?
  # --------------------------------------------------------
  nsAlpha <- ifelse(fade.ns == TRUE, 0.3, 1.0)
  # --------------------------------------------------------
  # continue with errorbars, p-value-label and intercept line
  # --------------------------------------------------------
  plotHeader <- plotHeader +
    # --------------------------------------------------------
    # print confidence intervalls (error bars)
    # --------------------------------------------------------
    geom_errorbar(aes(ymin = lower, ymax = upper), 
                  position = position_dodge(-geom.spacing), 
                  width = 0) +
    # --------------------------------------------------------
    # print value labels and p-values
    # --------------------------------------------------------
    geom_text(aes(label = p, y = upper), 
              position = position_dodge(width = -geom.spacing), 
              hjust = -0.1) +
    # --------------------------------------------------------
    # Intercept-line
    # --------------------------------------------------------
    geom_hline(yintercept = 0, 
               linetype = interceptLineType, 
               color = interceptLineColor) +
    labs(title = title, 
         x = NULL, 
         y = axisTitle.x, 
         shape = legendPValTitle, 
         colour = legendDepVarTitle) +
    scale_x_discrete(labels = axisLabels.y) +
    scale_y_continuous(limits = c(lower_lim, upper_lim), 
                       breaks = ticks, 
                       labels = ticks) +
    # --------------------------------------------------------
    # use transparancy if requested, but hide legend
    # --------------------------------------------------------
    scale_alpha_manual(values = c(nsAlpha, 1.0), guide = "none")
  # --------------------------------------------------------
  # flip coordinates?
  # --------------------------------------------------------
  if (coord.flip)  plotHeader <- plotHeader + coord_flip()
  if (facet.grid) plotHeader <- plotHeader + facet_grid(. ~ grp)
  # ---------------------------------------------------------
  # set geom colors
  # ---------------------------------------------------------
  plotHeader <- sj.setGeomColors(plotHeader, geom.colors, length(labelDependentVariables), ifelse(hideLegend==TRUE, FALSE, TRUE), labelDependentVariables)  
  # ---------------------------------------------------------
  # Check whether ggplot object should be returned or plotted
  # ---------------------------------------------------------
  if (printPlot) plot(plotHeader)
  # -------------------------------------
  # return results
  # -------------------------------------
  invisible (structure(class = "sjplmm",
                       list(plot = plotHeader,
                            df = finalbetas)))
}
