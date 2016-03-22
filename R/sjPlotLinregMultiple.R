# bind global variables
utils::globalVariables(c("beta", "lower", "upper", "p", "pa", "shape"))

#' @title Plot coefficients of multiple fitted lm(er)'s
#' @name sjp.lmm
#' 
#' @description Plot and compare coefficients (estimates) with confidence 
#'                intervals of  multiple fitted linear (mixed effects) models 
#'                in one plot. 
#'                Fitted models may have differing predictors, but only
#'                in a "stepwise" sense.
#'                
#' @param ... one or more fitted \code{lm} or \code{lmerMod}-objects. May also 
#'          be a \code{\link{list}}-object with fitted models, instead of separating 
#'          each model with comma. See 'Examples'.
#' @param type type of plot. Use one of following:
#'          \describe{
#'            \item{\code{"lm"}}{(default) for forest-plot like plot of estimates.}
#'            \item{\code{"std"}}{for forest-plot like plot of standardized beta values.}
#'            \item{\code{"std2"}}{for forest-plot like plot of standardized beta values, however, standardization is done by rescaling estimates by dividing them by two sd (see 'Details' in \code{\link{sjp.lm}}).}
#'          }
#' @param title diagram's title as string.
#' @param legendDepVarTitle character vector used for the legend title.
#'          Default is \code{"Dependent Variables"}.
#' @param legendPValTitle character vector used for the title of the significance level's legend.
#'          Default is \code{"p-level"}. Only applies if \code{usePShapes = TRUE}.
#' @param showAxisLabels.y Whether term names (predictor labels) should be shown or not.
#' @param axisTitle.x string, title for the x axis.
#' @param geom.size size of the points that indicate the estimates. Default is 3.
#' @param geom.spacing spacing between the dots and error bars of the plotted fitted models. Default
#'          is 0.3.
#' @param geom.colors colors for representing the estimates (i.e. points and error bars)
#'          of the different fitted models. Thus, the length of this vector must be equal to
#'          the length of supplied fitted models, so each model is represented by its own color.
#'          See 'Note' in \code{\link{sjp.grpfrq}}.
#' @param fade.ns if \code{TRUE}, non significant estimates will be printed in slightly faded colors.
#' @param usePShapes If \code{TRUE}, significant levels are distinguished by different point shapes and a related
#'          legend is plotted. Default is \code{FALSE}.
#' @param showIntercept If \code{TRUE}, the intercept of the fitted model is also plotted.
#'          Default is \code{FALSE}.
#' @param showPValueLabels Whether the significance levels of each coefficient should be appended
#'          to values or not.
#' @param facet.grid \code{TRUE} when each model should be plotted as single facet instead of 
#'          an integrated single graph.
#'          
#' @inheritParams sjp.lm
#' @inheritParams sjt.lm
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
#' sjp.lmm(fit1, fit2, fit3, facet.grid = TRUE)
#' 
#' # plot multiple models with legend labels and 
#' # point shapes instead of value labels
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
#'          fade.ns = TRUE,
#'          usePShapes = TRUE)
#' 
#' # ------------------------------
#' # plot multiple models from nested lists argument
#' # ------------------------------
#' all.models <- list()
#' all.models[[1]] <- fit1
#' all.models[[2]] <- fit2
#' all.models[[3]] <- fit3
#' 
#' sjp.lmm(all.models)
#' 
#' # ------------------------------
#' # plot multiple models with different
#' # predictors (stepwise inclusion),
#' # standardized estimates
#' # ------------------------------
#' fit1 <- lm(mpg ~ wt + cyl + disp + gear, data = mtcars)
#' fit2 <- update(fit1, . ~ . + hp)
#' fit3 <- update(fit2, . ~ . + am)
#' 
#' sjp.lmm(fit1, fit2, fit3, type = "std2")
#' 
#' @import ggplot2
#' @import sjmisc
#' @importFrom stats coef confint
#' @importFrom dplyr slice
#' @export
sjp.lmm <- function(...,
                    type = "lm",
                    title = NULL,
                    labelDependentVariables = NULL,
                    legendDepVarTitle = "Dependent Variables",
                    legendPValTitle = "p-level",
                    axisLabels.y = NULL,
                    axisTitle.x = "Estimates",
                    axisLimits = NULL,
                    breakTitleAt = 50,
                    breakLabelsAt = 25,
                    breakLegendTitleAt = 20,
                    gridBreaksAt = NULL,
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
  if (length(input_list) == 1 && class(input_list[[1]]) == "list")
    input_list <- lapply(input_list[[1]], function(x) x)
  # ----------------------------
  # init final data frame
  # ----------------------------
  finalbetas <- c()
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
    # retrieve beta's (lm)
    # ----------------------------
    if (type == "std") {
      # retrieve standardized betas
      betas <- data.frame(rbind(data.frame(beta = 0, ci.low = 0, ci.hi = 0),
                                suppressWarnings(sjmisc::std_beta(fit, include.ci = TRUE))))
      # no intercept for std
      showIntercept <- FALSE
      # add "std." to title?
      if (axisTitle.x == "Estimates")
        axisTitle.x <- "Std. Estimates"
    } else if (type == "std2") {
      # retrieve standardized betas
      betas <- data.frame(rbind(data.frame(beta = 0, ci.low = 0, ci.hi = 0),
                                sjmisc::std_beta(fit, include.ci = TRUE, type = "std2")))
      # no intercept for std
      showIntercept <- FALSE
      # add "std." to title?
      if (axisTitle.x == "Estimates")
        axisTitle.x <- "Std. Estimates"
    } else {
      # do we have mermod object?
      if (sjmisc::str_contains(class(fit), "merMod", ignore.case = T))
        betas <- get_cleaned_ciMerMod(fit, "lm")
      else
        # copy estimates to data frame
        betas <- data.frame(stats::coef(fit), stats::confint(fit))
    }
    # ----------------------------
    # give proper column names
    # ----------------------------
    colnames(betas) <- c("beta", "ci.low", "ci.hi")
    # ----------------------------
    # print p-values in bar charts
    # ----------------------------
    # retrieve sigificance level of independent variables (p-values)
    if (sjmisc::str_contains(class(fit), "merMod", ignore.case = T))
      pv <- get_lmerMod_pvalues(fit)
    else
      pv <- get_lm_pvalues(fit)$p
    # for better readability, convert p-values to asterisks
    # with:
    # p < 0.001 = ***
    # p < 0.01 = **
    # p < 0.05 = *
    ov <- betas[, 1]
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
    # bind p-values to data frame
    # ----------------------------
    betas <- data.frame(betas, ps, palpha, pointshapes, fitcnt, pv)
    # set column names
    colnames(betas) <- c("beta", "lower", "upper", "p", "pa", "shape", "grp", "p.value")
    #remove intercept from df
    if (!showIntercept) betas <- betas[-1, ]
    # add rownames
    betas$term <- row.names(betas)
    # add data frame to final data frame
    finalbetas <- rbind(finalbetas, betas)
  }
  # ----------------------------
  # check if user defined labels have been supplied
  # if not, use variable names from data frame
  # ----------------------------
  # reverse x-pos, convert to factor
  finalbetas$xpos <- sjmisc::to_value(as.factor(finalbetas$term), keep.labels = F)
  finalbetas$xpos <- as.factor(finalbetas$xpos)
  finalbetas$grp <- as.factor(finalbetas$grp)
  # convert to character
  finalbetas$shape <- as.character(finalbetas$shape)
  # -------------------------------------------------
  # remove any estimates from the output?
  # -------------------------------------------------
  if (!is.null(remove.estimates)) {
    # get row indices of rows that should be removed
    remrows <- c()
    for (re in 1:length(remove.estimates)) {
      remrows <- c(remrows, which(substr(row.names(finalbetas), 
                                         start = 1, 
                                         stop = nchar(remove.estimates[re])) == remove.estimates[re]))
    }
    # remember old rownames
    keepnames <- row.names(finalbetas)[-remrows]
    # remove rows
    finalbetas <- dplyr::slice(finalbetas, c(1:nrow(finalbetas))[-remrows])
    # set back rownames
    row.names(finalbetas) <- keepnames
  }
  # set axis labels
  if (is.null(axisLabels.y)) {
    axisLabels.y <- unique(finalbetas$term)
    axisLabels.y <- axisLabels.y[order(unique(finalbetas$xpos))]
  }
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
  # prepare star and shape values. we just copy those values
  # that are actually needed, so legend shapes are always 
  # identical, independent whether model have only two 
  # different p-levels or four.
  # --------------------------------------------------------
  shape.values <- c(1, 16, 17, 15)
  star.values <- c("n.s.", "*", "**", "***")
  shape.values <- shape.values[sort(as.numeric(unique(finalbetas$shape)))]
  star.values <- star.values[sort(as.numeric(unique(finalbetas$shape)))]
  # --------------------------------------------------------
  # body of plot
  # --------------------------------------------------------
  # The order of aesthetics matters in terms of ordering the error bars!
  # Using alpha-aes before colour would order error-bars according to
  # alpha-level instead of colour-aes.
  plotHeader <- ggplot(finalbetas, aes(y = beta, 
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
      geom_point(size = geom.size, position = position_dodge(-geom.spacing))
  }
  # --------------------------------------------------------
  # fade non-significant estimates?
  # --------------------------------------------------------
  nsAlpha <- ifelse(isTRUE(fade.ns), 0.3, 1.0)
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
              hjust = -0.1,
              show.legend = FALSE) +
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
  if (printPlot) graphics::plot(plotHeader)
  # -------------------------------------
  # set proper column names
  # -------------------------------------
  colnames(finalbetas) <- c("estimate", "conf.low", "conf.high", "p.string", 
                            "p.alpha", "shape", "grp", "p.value", "term", "xpos")
  # -------------------------------------
  # return results
  # -------------------------------------
  invisible(structure(class = c("sjPlot", "sjplmm"),
                      list(plot = plotHeader,
                           data = finalbetas)))
}
