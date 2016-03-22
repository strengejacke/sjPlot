# bind global variables
utils::globalVariables(c("OR", "lower", "upper", "p", "pa", "shape"))

#' @title Plot odds or incidents ratios (forest plots) of multiple fitted glm(er)'s
#' @name sjp.glmm
#' 
#' @description Plot and compare odds or incidents ratios (forest plots) of multiple fitted 
#'                generalized linear (mixed effects) models with confidence 
#'                intervals in one plot.
#' 
#' @param ... one or more fitted \code{glm}- or \code{glmerMod}-objects. May 
#'          also be a \code{\link{list}}-object with 
#'          fitted models, instead of separating each model with comma. See 'Examples'.
#'          
#' @inheritParams sjp.lmm
#' @inheritParams sjp.glm
#' @inheritParams sjp.lm
#' @inheritParams sjp.grpfrq
#'          
#' @note The fitted models may have differing predictors, but only in a 
#'         "stepwise" sense; i.e., models should share a common set of predictors,
#'         while some models may have additional predictors (e.g. added via
#'         the \code{\link[stats]{update}} function). See 'Examples'.
#'          
#' @return (Insisibily) returns the ggplot-object with the complete plot (\code{plot}) as well as the data frame that
#'           was used for setting up the ggplot-object (\code{data}).
#'          
#' @examples
#' # prepare dummy variables for binary logistic regression
#' y1 <- ifelse(swiss$Fertility < median(swiss$Fertility), 0, 1)
#' y2 <- ifelse(swiss$Infant.Mortality < median(swiss$Infant.Mortality), 0, 1)
#' y3 <- ifelse(swiss$Agriculture<median(swiss$Agriculture), 0, 1)
#' 
#' # Now fit the models. Note that all models share the same predictors
#' # and only differ in their dependent variable (y1, y2 and y3)
#' fitOR1 <- glm(y1 ~ swiss$Education + swiss$Examination + swiss$Catholic,
#'               family = binomial(link = "logit"))
#' fitOR2 <- glm(y2 ~ swiss$Education + swiss$Examination + swiss$Catholic,
#'               family = binomial(link = "logit"))
#' fitOR3 <- glm(y3 ~ swiss$Education + swiss$Examination + swiss$Catholic,
#'               family = binomial(link = "logit"))
#' 
#' # plot multiple models
#' sjp.glmm(fitOR1, fitOR2, fitOR3, facet.grid = TRUE)
#' 
#' # plot multiple models with legend labels and point shapes instead of value  labels
#' sjp.glmm(fitOR1, fitOR2, fitOR3,
#'          labelDependentVariables = c("Fertility", 
#'                                      "Infant Mortality", 
#'                                      "Agriculture"),
#'          showValueLabels = FALSE,
#'          showPValueLabels = FALSE,
#'          fade.ns = TRUE,
#'          usePShapes = TRUE)
#' 
#' # plot multiple models from nested lists argument
#' all.models <- list()
#' all.models[[1]] <- fitOR1
#' all.models[[2]] <- fitOR2
#' all.models[[3]] <- fitOR3
#' 
#' sjp.glmm(all.models)
#' 
#' 
#' # -------------------------------
#' # Predictors for negative impact
#' # of care. Data from the EUROFAMCARE
#' # sample dataset
#' # -------------------------------
#' library(sjmisc)
#' data(efc)
#' 
#' # create binary response
#' y <- ifelse(efc$neg_c_7 < median(na.omit(efc$neg_c_7)), 0, 1)
#' # create dummy variables for educational status
#' edu.mid <- ifelse(efc$c172code == 2, 1, 0)
#' edu.high <- ifelse(efc$c172code == 3, 1, 0)
#' # create data frame for fitted model
#' mydat <- data.frame(y = as.factor(y),
#'                     sex = as.factor(efc$c161sex),
#'                     dep = as.factor(efc$e42dep),
#'                     barthel = as.numeric(efc$barthtot),
#'                     edu.mid = as.factor(edu.mid),
#'                     edu.hi = as.factor(edu.high))
#' 
#' fit1 <- glm(y ~ sex + edu.mid + edu.hi, 
#'             data = mydat, 
#'             family = binomial(link = "logit"))
#' fit2 <- update(fit1, . ~ . + barthel)
#' fit3 <- update(fit2, . ~ . + dep)
#' 
#' sjp.glmm(fit1, fit2, fit3)
#' 
#' @import ggplot2
#' @import sjmisc
#' @importFrom stats na.omit coef confint
#' @export
sjp.glmm <- function(...,
                     title = NULL,
                     labelDependentVariables = NULL,
                     legendDepVarTitle = "Dependent Variables",
                     legendPValTitle = "p-level",
                     axisLabels.y = NULL,
                     axisTitle.x = "Odds Ratios",
                     axisLimits = NULL,
                     breakTitleAt = 50,
                     breakLabelsAt = 25,
                     breakLegendTitleAt = 20,
                     gridBreaksAt = 0.5,
                     transformTicks = TRUE,
                     geom.size = 3,
                     geom.spacing = 0.4,
                     geom.colors = "Set1",
                     fade.ns = FALSE,
                     usePShapes = FALSE,
                     interceptLineType = 2,
                     interceptLineColor = "grey70",
                     remove.estimates = NULL,
                     coord.flip = TRUE,
                     showIntercept = FALSE,
                     showAxisLabels.y = TRUE,
                     showValueLabels = TRUE,
                     labelDigits = 2,
                     showPValueLabels = TRUE,
                     hideLegend = FALSE,
                     facet.grid = FALSE,
                     printPlot = TRUE) {
  # --------------------------------------------------------
  # retrieve list of fitted models
  # --------------------------------------------------------
  input_list <- list(...)
  # --------------------------------------------------------
  # check length. if we have a list of fitted model, 
  # we need to "unlist" them
  # --------------------------------------------------------
  if (length(input_list) == 1 && class(input_list[[1]]) == "list") input_list <- lapply(input_list[[1]], function(x) x)
  # ----------------------------
  # init final data frame
  # ----------------------------
  finalodds <- c()
  fitlength <- length(input_list)
  # ----------------------------
  # Prepare length of title and labels
  # ----------------------------
  # if we have no labels of dependent variables supplied, use a 
  # default string (Model) for legend
  if (is.null(labelDependentVariables)) {
    labelDependentVariables <- c()
    for (i in seq_len(fitlength)) {
      labelDependentVariables <- c(labelDependentVariables, 
                                   get_model_response_label(input_list[[i]]))
    }
  }
  # check length of diagram title and split longer string at into new lines
  if (!is.null(title)) title <- sjmisc::word_wrap(title, breakTitleAt)
  # check length of x-axis title and split longer string at into new lines
  # every 50 chars
  if (!is.null(axisTitle.x)) axisTitle.x <- sjmisc::word_wrap(axisTitle.x, breakTitleAt)
  # check length of dependent variables
  if (!is.null(labelDependentVariables)) labelDependentVariables <- sjmisc::word_wrap(labelDependentVariables, breakLegendTitleAt)
  # check length of x-axis-labels and split longer strings at into new lines
  if (!is.null(axisLabels.y)) axisLabels.y <- sjmisc::word_wrap(axisLabels.y, breakLabelsAt)
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
    if (sjmisc::str_contains(class(fit), "merMod", ignore.case = T))
      odds <- get_cleaned_ciMerMod(fit, "glm")
    else
      odds <- data.frame(exp(stats::coef(fit)), 
                         exp(stats::confint(fit)))
    # ----------------------------
    # print p-values in bar charts
    # ----------------------------
    # retrieve sigificance level of independent variables (p-values)
    if (sjmisc::str_contains(class(fit), "merMod", ignore.case = T))
      pv <- get_lmerMod_pvalues(fit)
    else
      pv <- unname(stats::coef(summary(fit))[, 4])
    # for better readability, convert p-values to asterisks
    # with:
    # p < 0.001 = ***
    # p < 0.01 = **
    # p < 0.05 = *
    # retrieve odds ratios
    ov <- odds[, 1]
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
    # bind p-values to data frame
    # ----------------------------
    odds <- data.frame(odds, ps, palpha, pointshapes, fitcnt, pv)
    # set column names
    colnames(odds) <- c("OR", "lower", "upper", "p", "pa", "shape", "grp", "p.value")
    # add rownames
    odds$term <- row.names(odds)
    #remove intercept from df
    if (!showIntercept) odds <- odds[-1, ]
    # add data frame to final data frame
    finalodds <- rbind(finalodds, odds)
  }
  # ----------------------------
  # check if user defined labels have been supplied
  # if not, use variable names from data frame
  # ----------------------------
  # reverse x-pos, convert to factor
  finalodds$xpos <- sjmisc::to_value(as.factor(finalodds$term), keep.labels = F)
  finalodds$xpos <- as.factor(finalodds$xpos)
  finalodds$grp <- as.factor(finalodds$grp)
  # convert to character
  finalodds$shape <- as.character(finalodds$shape)
  # -------------------------------------------------
  # remove any estimates from the output?
  # -------------------------------------------------
  if (!is.null(remove.estimates)) {
    # get row indices of rows that should be removed
    remrows <- c()
    for (re in 1:length(remove.estimates)) {
      remrows <- c(remrows, which(substr(row.names(finalodds), 
                                         start = 1, 
                                         stop = nchar(remove.estimates[re])) == remove.estimates[re]))
    }
    # remember old rownames
    keepnames <- row.names(finalodds)[-remrows]
    # remove rows
    finalodds <- dplyr::slice(finalodds, c(1:nrow(finalodds))[-remrows])
    # set back rownames
    row.names(finalodds) <- keepnames
  }
  # set axis labels
  if (is.null(axisLabels.y)) {
    axisLabels.y <- unique(finalodds$term)
    axisLabels.y <- axisLabels.y[order(unique(finalodds$xpos))]
  }
  # --------------------------------------------------------
  # Calculate axis limits. The range is from lowest lower-CI
  # to highest upper-CI, or a user defined range
  # --------------------------------------------------------
  if (is.null(axisLimits)) {
    # we have confindence intervals displayed, so
    # the range corresponds to the boundaries given by
    # the CI's
    upper_lim <- ceiling(10 * max(finalodds$upper)) / 10
    lower_lim <- floor(10 * min(finalodds$lower)) / 10
    # avoid zero or NA axis limit!
    if (is.na(upper_lim)) upper_lim <- ceiling(10 * max(stats::na.omit(finalodds$upper))) / 10
    if (lower_lim == 0 || is.na(lower_lim)) lower_lim <- 0.01
    # if we show p value labels, increase upper
    # limit of x axis, so labels are plotted inside
    # diagram range
    if (showValueLabels || showPValueLabels) upper_lim <- upper_lim + 0.1
    # give warnings when auto-limits are very low/high
    if ((lower_lim < 0.1) || (upper_lim > 100)) {
      warning("Exp. coefficients and/or exp. confidence intervals may be out of printable bounds. Consider using \"axisLimits\" argument!")
    }
  } else {
    # Here we have user defind axis range
    lower_lim <- axisLimits[1]
    upper_lim <- axisLimits[2]
  }
  # --------------------------------------------------------
  # Define axis ticks, i.e. at which position we have grid
  # bars.
  # --------------------------------------------------------
  ticks <- seq(lower_lim, upper_lim, by = gridBreaksAt)
  if (!showAxisLabels.y) axisLabels.y <- c("")
  # --------------------------------------------------------
  # prepare star and shape values. we just copy those values
  # that are actually needed, so legend shapes are always 
  # identical, independent whether model have only two 
  # different p-levels or four.
  # --------------------------------------------------------
  shape.values <- c(1, 16, 17, 15)
  star.values <- c("n.s.", "*", "**", "***")
  shape.values <- shape.values[sort(as.numeric(unique(finalodds$shape)))]
  star.values <- star.values[sort(as.numeric(unique(finalodds$shape)))]
  # --------------------------------------------------------
  # body of plot
  # --------------------------------------------------------
  # The order of aesthetics matters in terms of ordering the error bars!
  # Using alpha-aes before colour would order error-bars according to
  # alpha-level instead of colour-aes.
  plotHeader <- ggplot(finalodds, aes(y = OR, 
                                      x = xpos, 
                                      group = grp,
                                      colour = grp, 
                                      alpha = pa))
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
      scale_shape_manual(values = shape.values, 
                         labels = star.values)
  } else {
    plotHeader <- plotHeader +
      geom_point(size = geom.size, 
                 position = position_dodge(-geom.spacing))
  }
  # --------------------------------------------------------
  # fade non-significant estimates?
  # --------------------------------------------------------
  nsAlpha <- ifelse(isTRUE(fade.ns), 0.3, 1.0)
  # --------------------------------------------------------
  # continue with errorbars, p-value-label and intercept line
  # --------------------------------------------------------
  plotHeader <- plotHeader +
    # print confidence intervalls (error bars)
    geom_errorbar(aes(ymin = lower, ymax = upper), 
                  width = 0, 
                  position = position_dodge(-geom.spacing)) +
    # print value labels and p-values
    geom_text(aes(label = p, y = upper), 
              position = position_dodge(width = -geom.spacing), 
              hjust = -0.1,
              show.legend = FALSE) +
    # Intercept-line
    geom_hline(yintercept = 1, 
               linetype = interceptLineType, 
               colour = interceptLineColor) +
    labs(title = title, 
         x = NULL, 
         y = axisTitle.x, 
         shape = legendPValTitle, 
         colour = legendDepVarTitle) +
    scale_x_discrete(labels = axisLabels.y) +
    # use transparancy if requested, but hide legend
    scale_alpha_manual(values = c(nsAlpha, 1.0), guide = "none")
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
  } else {
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
  if (coord.flip) plotHeader <- plotHeader + coord_flip()
  if (facet.grid) plotHeader <- plotHeader + facet_grid(~grp)
  # ---------------------------------------------------------
  # set geom colors
  # ---------------------------------------------------------
  plotHeader <- sj.setGeomColors(plotHeader, 
                                 geom.colors, 
                                 length(labelDependentVariables), 
                                 ifelse(isTRUE(hideLegend), FALSE, TRUE), 
                                 labelDependentVariables)
  # ---------------------------------------------------------
  # Check whether ggplot object should be returned or plotted
  # ---------------------------------------------------------
  if (printPlot) print(plotHeader)
  # -------------------------------------
  # set proper column names
  # -------------------------------------
  colnames(finalodds) <- c("estimate", "conf.low", "conf.high", "p.string", 
                           "p.alpha", "shape", "grp", "p.value", "term", "xpos")
  # -------------------------------------
  # return results
  # -------------------------------------
  invisible(structure(class = c("sjPlot", "sjpglmm"),
                      list(plot = plotHeader,
                           data = finalodds)))
}
