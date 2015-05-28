# bind global variables
if (getRversion() >= "2.15.1") utils::globalVariables(c("OR", "lower", "upper", "p"))



#' @title Plot odds ratios or predicted probabilities of generalized linear models
#' @name sjp.glm
#'
#' @seealso \href{http://www.strengejacke.de/sjPlot/sjp.glm/}{sjPlot manual: sjp.glm}
#'
#' @description Plot odds ratios (exponentiated coefficients) with confidence intervalls as bar chart or dot plot.
#'                Depending on the \code{type} parameter, this function may also plot model
#'                assumptions for generalized linear models, or predicted probabilities
#'                of coefficients.
#'
#' @note Based on the script from \href{http://www.surefoss.org/dataanalysis/plotting-odds-ratios-aka-a-forrestplot-with-ggplot2/}{surefoss}
#'
#' @param fit fitted generalized linear model (\code{\link{glm}}- or \code{logistf}-object).
#' @param type type of plot. Use one of following:
#'          \describe{
#'            \item{\code{"dots"}}{(or \code{"glm"} or \code{"or"} (default)) for odds ratios (forest plot)}
#'            \item{\code{"bars"}}{for odds ratios as bar plot}
#'            \item{\code{"prob"}}{(or \code{"pc"}) to plot predicted probabilities for each model terms, where all remaining co-variates are set to zero (i.e. ignored). Use \code{facet.grid} to decide whether to plot each coefficient as separate plot or as integrated faceted plot.}
#'            \item{\code{"probc"}}{(or \code{"pcc"}) to plot centered predicted probabilities for each model term (see 'Details'). Use \code{facet.grid} to decide whether to plot each coefficient as separate plot or as integrated faceted plot.}
#'            \item{\code{"y.pc"}}{(or \code{"y.prob"}) to plot predicted probabilities for the response.}
#'            \item{\code{"ma"}}{to check model assumptions. Note that only two parameters are relevant for this option \code{fit} and \code{showOriginalModelOnly}. All other parameters are ignored.}
#'            \item{\code{"vif"}}{to plot Variance Inflation Factors.}
#'          }
#' @param sortOdds If \code{TRUE} (default), the odds ratios are ordered according their values from highest first
#'          to lowest last. Use \code{FALSE} if you don't want to change the order of the predictors.
#' @param title Diagram's title as string. Example: \code{title = "my title"}
#' @param axisLabels.y labels of the predictor variables (independent vars) that are used for labelling the
#'          axis. Must be a character vector of same length as independent variables. \cr
#'          \strong{Note:} If you use the \code{\link[sjmisc]{read_spss}} function and the \code{\link[sjmisc]{get_val_labels}} function, you receive a
#'          \code{list} object with label string. The labels may also be passed as list object. They will be coerced
#'          to character vector automatically.
#' @param showAxisLabels.y Whether labels of independent variables should be shown or not.
#' @param axisTitle.x string; title for the x axis.
#' @param axisLimits Defines the range of the axis where the odds ratios and their confidence intervalls
#'          are drawn. By default, the limits range from the lowest confidence interval to the highest one, so
#'          the diagram has maximum zoom. Use your own values as 2-value-vector, for instance: \code{limits = c(-0.8, 0.8)}.
#' @param breakTitleAt Wordwrap for diagram title. Determines how many chars of the title are displayed in
#'          one line and when a line break is inserted into the title
#' @param breakLabelsAt Wordwrap for diagram labels. Determines how many chars of the category labels are displayed in
#'          one line and when a line break is inserted
#' @param gridBreaksAt Sets the breaks on the y axis, i.e. at every n'th position a major
#'          grid is being printed. Default is 0.5
#' @param transformTicks if \code{TRUE}, the grid bars have exponential distances (equidistant), i.e. they
#'          visually have the same distance from one panel grid to the next. If \code{FALSE}, grids are 
#'          plotted on every \code{gridBreaksAt}'s position, thus the grid bars become narrower with 
#'          higher odds ratio values.
#' @param geom.colors User defined color palette for geoms. Must either be vector with two color values
#'          or a specific color palette code (see below).
#'          \itemize{
#'            \item If not specified, the \code{"Set1"} color brewer palette will be used.
#'            \item If \code{"gs"}, a greyscale will be used.
#'            \item If \code{geom.colors} is any valid color brewer palette name, the related \href{http://colorbrewer2.org}{color brewer} palette will be used. Use \code{display.brewer.all()} from the \code{RColorBrewer} package to view all available palette names.
#'            \item Else specify your own color values as vector, e.g. \code{geom.colors=c("#f00000", "#00ff00")}.
#'          }
#' @param geom.size size resp. width of the geoms (bar width or point size, depending on \code{type} parameter).
#' @param hideErrorBars If \code{TRUE}, the error bars that indicate the confidence intervals of the odds ratios are not
#'          shown. Only applies if parameter \code{type = "bars"}. Default value is \code{FALSE}.
#' @param interceptLineType The linetype of the intercept line (zero point). Default is \code{2} (dashed line).
#' @param interceptLineColor The color of the intercept line. Default value is \code{"grey70"}.
#' @param coord.flip If \code{TRUE} (default), predictors are plotted on the left y-axis and estimate
#'          values are plotted on the x-axis.
#' @param showIntercept If \code{TRUE}, the intercept of the fitted model is also plotted.
#'          Default is \code{FALSE}. Please note that due to exponential transformation of
#'          estimates, the intercept in some cases can not be calculated, thus the
#'          function call is interrupted and no plot printed.
#' @param showValueLabels Whether odds ratio values should be plotted to each dot or not.
#' @param labelDigits The amount of digits for rounding the estimations (see \code{showValueLabels}).
#'          Default is 2, i.e. estimators have 2 digits after decimal point.
#' @param showPValueLabels Whether the significance levels of each coefficient should be appended
#'          to values or not.
#' @param showModelSummary If \code{TRUE} (default), a summary of the regression model with
#'          Intercept, R-square, F-Test and AIC-value is printed to the lower right corner
#'          of the diagram.
#' @param show.se Use \code{TRUE} to plot (depending on \code{type}) the standard
#'          error for probability curves (predicted probabilities).
#' @param facet.grid \code{TRUE} when each plot should be plotted separately instead of
#'          an integrated (faceted) single graph. Only applies, if \code{type = "prob"}.
#' @param showOriginalModelOnly if \code{TRUE} (default) and \code{type = "ma"}, 
#'          only the model assumptions of the fitted model \code{fit} are plotted.
#'          If \code{FALSE}, the model assumptions of an updated model where outliers
#'          are automatically excluded are also plotted.
#' @param printPlot If \code{TRUE} (default), plots the results as graph. Use \code{FALSE} if you don't
#'          want to plot any graphs. In either case, the ggplot-object will be returned as value.
#' @return (Invisibly) returns various objects, depending on 
#'           the \code{type}-parameter:
#'         \describe{
#'          \item{\code{type = "dots" or "bars"}}{
#'            \itemize{
#'              \item \code{mydf} - data frame used for the plot
#'              \item \code{plot} - plot as ggplot-object
#'            }
#'          }
#'          \item{\code{type = "prob" or "probc"}}{
#'            \itemize{
#'              \item \code{mydf.mp} - data frame used for predicted probability plots
#'              \item \code{plot.mp} - predicted probability plots as ggplot-objects
#'              \item \code{mydf.facet} - data frame used for faceted predicted probability plots
#'              \item \code{plot.facet} - facted predicted probability plots as ggplot-objects
#'            }
#'          }
#'          \item{\code{type = "y.pc"}}{
#'            \itemize{
#'              \item \code{mydf} - data frame used for the plot
#'              \item \code{plot} - plot as ggplot-object
#'              \item \code{mean.pp} - mean value of the predicted probabilities for the response
#'            }
#'          }
#'          \item{\code{type = "ma"}}{
#'            \itemize{
#'              \item \code{model} - updated model-fit with removed outliers
#'            }
#'          }
#'          \item{\code{type = "vif"}}{
#'            \itemize{
#'              \item \code{vifval} - a vector with vif-values
#'            }
#'          }
#'         }
#'
#' @details \describe{
#'            \item{\code{type = "prob"}}{(or \code{"pc"}), the predicted probabilities
#'            are based on the intercept's estimate and each specific term's estimate.
#'            All other co-variates are set to zero (i.e. ignored).}
#'            \item{\code{type = "probc"}}{(or \code{"pcc"}), the predicted probabilities
#'            are based on the \code{\link{predict.glm}} method, where predicted values 
#'            are "centered"
#'            (see \href{http://stats.stackexchange.com/questions/35682/contribution-of-each-covariate-to-a-single-prediction-in-a-logistic-regression-m#comment71993_35802}{CrossValidated}).}
#'            \item{\code{type = "y.pc"}}{(or \code{type = "y.prob"}), the predicted values
#'            of the response are computed, based on the \code{\link{predict.glm}}
#'            method.}
#'          }
#'
#' @examples
#' # prepare dichotomous dependent variable
#' y <- ifelse(swiss$Fertility < median(swiss$Fertility), 0, 1)
#'
#' # fit model
#' fitOR <- glm(y ~ swiss$Education + swiss$Examination + swiss$Infant.Mortality + swiss$Catholic,
#'              family = binomial(link = "logit"))
#'
#' # print Odds Ratios as dots
#' sjp.glm(fitOR)
#'
#' # print Odds Ratios as bars
#' sjp.glm(fitOR, type = "bars", geom.size = .3)
#'
#'
#' # -------------------------------
#' # Predictors for negative impact
#' # of care. Data from the EUROFAMCARE
#' # sample dataset
#' # -------------------------------
#' library(sjmisc)
#' data(efc)
#' # retrieve predictor variable labels
#' labs <- get_var_labels(efc)
#' predlab <- c(labs[['c161sex']],
#'              paste0(labs[['e42dep']], " (slightly)"),
#'              paste0(labs[['e42dep']], " (moderate)"),
#'              paste0(labs[['e42dep']], " (severely)"),
#'              labs[['barthtot']],
#'              paste0(labs[['c172code']], " (mid)"),
#'              paste0(labs[['c172code']], " (high)"))
#' # create binary response
#' y <- ifelse(efc$neg_c_7 < median(na.omit(efc$neg_c_7)), 0, 1)
#' # create dummy variables for educational status
#' edu.mid <- ifelse(efc$c172code == 2, 1, 0)
#' edu.high <- ifelse(efc$c172code == 3, 1, 0)
#' # create data frame for fitted model
#' mydf <- data.frame(y = as.factor(y),
#'                    sex = as.factor(efc$c161sex),
#'                    dep = as.factor(efc$e42dep),
#'                    barthel = as.numeric(efc$barthtot),
#'                    edu.mid = as.factor(edu.mid),
#'                    edu.hi = as.factor(edu.high))
#' # fit model
#' fit <- glm(y ~., data = mydf, family = binomial(link = "logit"))
#' # plot odds
#' sjp.glm(fit,
#'         title = labs[['neg_c_7']],
#'         axisLabels.y = predlab)
#'
#' # plot probability curves (predicted probabilities)
#' # of coefficients
#' sjp.glm(fit,
#'         title = labs[['neg_c_7']],
#'         axisLabels.y = predlab,
#'         type = "prob")
#'
#' @import ggplot2
#' @import sjmisc
#' @importFrom car outlierTest influencePlot crPlots durbinWatsonTest leveragePlots ncvTest spreadLevelPlot vif
#' @export
sjp.glm <- function(fit,
                    type="dots",
                    sortOdds=TRUE,
                    title=NULL,
                    axisLabels.y=NULL,
                    axisTitle.x="Odds Ratios",
                    axisLimits=NULL,
                    breakTitleAt=50,
                    breakLabelsAt=25,
                    gridBreaksAt=0.5,
                    transformTicks=TRUE,
                    geom.size=3,
                    geom.colors="Set1",
                    hideErrorBars=FALSE,
                    interceptLineType=2,
                    interceptLineColor="grey70",
                    coord.flip=TRUE,
                    showIntercept=FALSE,
                    showAxisLabels.y=TRUE,
                    showValueLabels=TRUE,
                    labelDigits=2,
                    showPValueLabels=TRUE,
                    showModelSummary=TRUE,
                    facet.grid = TRUE,
                    show.se = FALSE,
                    showOriginalModelOnly=TRUE,
                    printPlot=TRUE) {
  # --------------------------------------------------------
  # check param
  # --------------------------------------------------------
  if (any(class(fit) == "logistf")) {
    # no model summary currently supported for logistf class
    showModelSummary = FALSE
    # create "dummy" variable, to avoid errors
    fit$model <- fit$data
    # no probability curves currently supported
    if (type == "prob" || type == "pc") {
      warning("Predicted probability plots currently not supported for 'logistf' objects.", call. = F)
      type <- "dots"
    }
  }
  # --------------------------------------------------------
  # check type
  # --------------------------------------------------------
  if (type == "prob" || type == "pc") {
    return(invisible(sjp.glm.pc(fit,
                                show.se,
                                type = "prob",
                                facet.grid,
                                printPlot)))
  }
  if (type == "probc" || type == "pcc") {
    return(invisible(sjp.glm.pc(fit,
                                show.se,
                                type = "probc",
                                facet.grid,
                                printPlot)))
  }
  if (type == "y.pc" || type == "y.prob") {
    return(invisible(sjp.glm.response.probcurv(fit,
                                               show.se,
                                               printPlot)))
  }
  if (type == "ma") {
    return(invisible(sjp.glm.ma(fit, showOriginalModelOnly)))
  }
  if (type == "vif") {
    return(invisible(sjp.vif(fit)))
  }
  if (type == "or" || type == "glm") type <- "dots"
  # --------------------------------------------------------
  # unlist labels
  # --------------------------------------------------------
  if (!is.null(axisLabels.y) && is.list(axisLabels.y)) {
    axisLabels.y <- unlistlabels(axisLabels.y)
  }
  # --------------------------------------------------------
  # auto-retrieve value labels
  # --------------------------------------------------------
  if (is.null(axisLabels.y)) {
    axisLabels.y <- suppressWarnings(retrieveModelLabels(list(fit)))
  }
  # ----------------------------
  # Prepare length of title and labels
  # ----------------------------
  # check length of diagram title and split longer string at into new lines
  if (!is.null(title)) title <- sjmisc::word_wrap(title, breakTitleAt)
  # check length of x-axis title and split longer string at into new lines
  # every 50 chars
  if (!is.null(axisTitle.x)) axisTitle.x <- sjmisc::word_wrap(axisTitle.x, breakTitleAt)
  # check length of x-axis-labels and split longer strings at into new lines
  if (!is.null(axisLabels.y)) axisLabels.y <- sjmisc::word_wrap(axisLabels.y, breakLabelsAt)
  # create data frame for ggplot
  tmp <- data.frame(cbind(exp(coef(fit)), exp(confint(fit))))
  # ----------------------------
  # print p-values in bar charts
  # ----------------------------
  # retrieve sigificance level of independent variables (p-values)
  if (any(class(fit) == "logistf")) {
    pv <- fit$prob
  } else {
    pv <- coef(summary(fit))[,4]
  }
  # for better readability, convert p-values to asterisks
  # with:
  # p < 0.001 = ***
  # p < 0.01 = **
  # p < 0.05 = *
  # retrieve odds ratios
  ov <- exp(coef(fit))
  # ----------------------------
  # copy OR-values into data column
  # ----------------------------
  ps <- rep("", length(ov))
  if (showValueLabels) ps <- sprintf("%.*f", labelDigits, ov)
  # ----------------------------
  # copy p-values into data column
  # ----------------------------
  if (showPValueLabels) {
    for (i in 1:length(pv)) {
      if (pv[i] >= 0.01 && pv[i] < 0.05) {
        ps[i] <- paste(ps[i], "*")
      } else if (pv[i] >= 0.001 && pv[i] < 0.01) {
        ps[i] <- paste(ps[i], "**")
      } else if (pv[i] < 0.001) {
        ps[i] <- paste(ps[i], "***")
      }
    }
  }
  # ----------------------------
  # remove intercept
  # ----------------------------
  odds <- cbind(tmp[-1, ])
  # ----------------------------
  # retrieve odds ratios, without intercept. now we can order
  # the predictors according to their OR value, while the intercept
  # is always shown on top
  # ----------------------------
  ov <- exp(coef(fit))[-1]
  # ----------------------------
  # check if user defined labels have been supplied
  # if not, use variable names from data frame
  # ----------------------------
  # auto-retrieving variable labels does not work when we
  # have factors with different levels, which appear as
  # "multiple predictors", but are only one variable
  # --------------------------------------------------------
  if (is.null(axisLabels.y) || length(axisLabels.y) < length(row.names(odds))) {
    axisLabels.y <- row.names(odds)
  }
  # ----------------------------
  # sort labels descending in order of
  # odds ratio values
  # This is necessary because the OR-values are reorderd by size
  # in the ggplot function below
  # ----------------------------
  if (sortOdds) axisLabels.y <- axisLabels.y[order(ov)]
  # ----------------------------
  # bind p-values to data frame
  # ----------------------------
  odds <- cbind(odds, ps[-1])
  # we repeat the whole procedure for our
  # tmp-data frame as well, since this data frame
  # contains the intercepts. We than later just copy the
  # intercept row to our odds-data frame, if needed. The intercept
  # is not included from the beginning, because when sorting the OR values,
  # the intercept should not be sorted, but alway placed on top
  tmp <- cbind(tmp, ps)
  # set column names
  names(odds) <- c("OR", "lower", "upper", "p")
  names(tmp) <- c("OR", "lower", "upper", "p")
  lhj <- ifelse(odds$OR > 1, 1.3, -0.3)
  odds <- cbind(odds, labhjust = lhj)
  lhj <- ifelse(tmp$OR > 1, 1.3, -0.3)
  tmp <- cbind(tmp, labhjust = lhj)
  # ----------------------------
  # Create new variable. Needed for sorting the variables / OR
  # in the graph (see reorder in ggplot-function)
  # ----------------------------
  tmp$vars <- as.factor(c(nrow(tmp)))
  # --------------------------------------------------------
  # Calculate axis limits. The range is from lowest lower-CI
  # to highest upper-CI, or a user defined range
  # --------------------------------------------------------
  if (is.null(axisLimits)) {
    # if intercept is shown, we have to adjuste the axis limits to max/min
    # values of odds ratios AND intercept
    if (showIntercept) {
      rdf <- tmp
    } else {
      # else, we have to adjuste the axis limits to max/min
      # values just of odds ratios
      rdf <- odds
    }
    # check whether we have bar chart and error bars hidden
    # in this case, the upper limit does not correspond to the
    # upper CI, but to the highest OR value
    if (type == "bars" && hideErrorBars) {
      maxval <- max(rdf$OR)
      minval <- min(rdf$OR)
    } else {
      # else we have confindence intervals displayed, so
      # the range corresponds to the boundaries given by
      # the CI's
      maxval <- max(rdf$upper)
      minval <- min(rdf$lower)
    }
    upper_lim <- ceiling(10 * maxval) / 10
    lower_lim <- floor(10 * minval) / 10
    # avoid zero or NA axis limit!
    if (is.na(upper_lim)) upper_lim <- ceiling(10 * max(na.omit(maxval))) / 10
    if (lower_lim == 0 || is.na(lower_lim)) lower_lim <- 0.01
    # give warnings when auto-limits are very low/high
    if ((minval < 0.1) || (maxval > 100)) {
      warning("Exp. coefficients and/or exp. confidence intervals may be out of printable bounds. Consider using \"axisLimits\" parameter!")
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
  ticks <- c(seq(lower_lim, upper_lim, by = gridBreaksAt))
  # ----------------------------
  # create expression with model summarys. used
  # for plotting in the diagram later
  # ----------------------------
  if (showModelSummary) {
    psr <- sjmisc::pseudo_r2(fit)
    modsum <- as.character(as.expression(
      substitute("(Intercept)" == ic * "," ~~ italic(R)[CS]^2 == r2cs * "," ~~ italic(R)[N]^2 == r2n * "," ~~ -2 * lambda == la * "," ~~ chi^2 == c2 * "," ~~ "AIC" == aic,
                 list(ic = sprintf("%.2f", exp(coef(fit)[1])),
                      r2cs = sprintf("%.3f", psr$CoxSnell),
                      r2n = sprintf("%.3f", psr$Nagelkerke),
                      la = sprintf("%.2f", -2 * logLik(fit)),
                      c2 = sprintf("%.2f", Chisquare.glm(fit)),
                      aic = sprintf("%.2f", fit$aic)))))
    cat(sprintf("Intercept = %.2f\nR2[cs] = %.3f\nR2[n] = %.3f\nLambda = %.2f\nChi2 = %.2f\nAIC = %.2f\n",
                exp(coef(fit)[1]),
                psr$CoxSnell,
                psr$Nagelkerke,
                -2 * logLik(fit),
                Chisquare.glm(fit),
                fit$aic))
  } else {
    modsum <- NULL
  }
  # --------------------------------------------------------
  # should axis labels be hidden? if yes, clear labels
  # --------------------------------------------------------
  if (!showAxisLabels.y) axisLabels.y <- c("")
  # --------------------------------------------------------
  # Order odds according to beta-coefficients
  # --------------------------------------------------------
  if (sortOdds) odds <- odds[order(ov), ]
  odds$vars <- cbind(c(1:nrow(odds)))
  odds$vars <- as.factor(odds$vars)
  # --------------------------------------------------------
  # check whether intercept should be shown
  # --------------------------------------------------------
  if (showIntercept) {
    odds <- data.frame(rbind(tmp[1, ], odds))
    axisLabels.y <- c("Intercept", axisLabels.y)
  }
  # --------------------------------------------------------
  # body of plot, i.e. this is the same in both bar and dot plots
  # --------------------------------------------------------
  # plot as bars, fill bars according to
  # OR-value greater / lower than 1
  plotHeader <- ggplot(odds, aes(y = OR, x = vars))
  # --------------------------------------------------------
  # start with dot-plotting here
  # --------------------------------------------------------
  if (type == "dots") {
    plotHeader <- plotHeader +
      # Order odds according to beta-coefficients, colour points and lines according to
      # OR-value greater / lower than 1
      geom_point(size = geom.size, aes(colour = (OR > 1))) +
      # print confidence intervalls (error bars)
      geom_errorbar(aes(ymin = lower, 
                        ymax = upper, 
                        colour = (OR > 1)), 
                    width = 0) +
      # print value labels and p-values
      geom_text(aes(label = p, y = OR), vjust = -0.7)
  # --------------------------------------------------------
  # start with bar plots here
  # --------------------------------------------------------
  } else if (type == "bars") {
    # Order odds according to beta-coefficients, colour points and lines according to
    # OR-value greater / lower than 1
    plotHeader <- plotHeader +
      # stat-parameter indicates statistics
      # stat="bin": y-axis relates to count of variable
      # stat="identity": y-axis relates to value of variable
      geom_bar(aes(fill = (OR > 1)), 
               stat = "identity", 
               position = "identity", 
               width = geom.size) +
      # print value labels and p-values
      geom_text(aes(label = p, y = 1), 
                vjust = -1, 
                hjust = odds$labhjust)
    if (hideErrorBars == FALSE) {
      plotHeader <- plotHeader +
        # print confidence intervalls (error bars)
      geom_errorbar(aes(ymin = lower, 
                        ymax = upper), 
                    colour = "black", 
                    width = 0)
    }
  }
  # ------------------------------------------
  # add annotations with model summary
  # here we print out the log-lik-ratio "lambda" and the chi-square significance of the model
  # compared to the null-model
  # ------------------------------------------
  plotHeader <- print.table.summary(plotHeader, modsum)
  plotHeader <- plotHeader +
    # Intercept-line
    geom_hline(yintercept = 1,
               linetype = interceptLineType,
               color = interceptLineColor) +
    labs(title = title,
         x = NULL,
         y = axisTitle.x) +
    scale_x_discrete(labels = axisLabels.y)
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
      scale_y_log10(limits = c(lower_lim, upper_lim),
                    breaks = ticks,
                    labels = ticks)
  }
  # --------------------------------------------------------
  # flip coordinates?
  # --------------------------------------------------------
  if (coord.flip) plotHeader <- plotHeader + coord_flip()
  # ---------------------------------------------------------
  # set geom colors
  # ---------------------------------------------------------
  plotHeader <- sj.setGeomColors(plotHeader, geom.colors, 2, FALSE)
  # ---------------------------------------------------------
  # Check whether ggplot object should be returned or plotted
  # ---------------------------------------------------------
  if (printPlot) print(plotHeader)
  # -------------------------------------
  # return results
  # -------------------------------------
  invisible(structure(class = "sjpglm",
                      list(plot = plotHeader,
                           mydf = odds)))
}


sjp.glm.pc <- function(fit,
                       show.se,
                       facet.grid,
                       type,
                       printPlot) {
  # ----------------------------
  # prepare additional plots, when metric
  # predictors should also be plotted
  # ----------------------------
  # init lists with all additional data frames and plots
  mydf.metricpred <- list()
  plot.metricpred <- list()
  axisLabels.mp <- c()
  plot.facet <- NULL
  mydf.facet <- NULL
  # retrieve term names, so we find the estimates in the
  # coefficients list
  fit.term.names <- names(attr(fit$terms, "dataClasses"))[-1]
  # ----------------------------
  # loop through all coefficients
  # ----------------------------
  for (i in 1:length(fit.term.names)) {
    # get values from coefficient
    coef.column <- which(colnames(fit$model) == fit.term.names[i])
    # check if we have found the coefficient
    if (length(coef.column) > 0) {
      # get values from numeric term
      values <- fit$model[, coef.column]
      # melt variable
      mydf.vals <- data.frame(values = values)
      # convert factor to numeric
      if (is.factor(mydf.vals$values)) mydf.vals$values <- sjmisc::to_value(mydf.vals$values, 0, keep.labels = F)
      # retrieve names of coefficients
      coef.names <- names(coef(fit))
      # check if we have a factor, then we may have reference levels
      if (is.factor(values)) {
        # add reference level to coefficient name
        ll <- levels(values)
        fit.fac.name <- paste0(fit.term.names[i], ll[length(ll)])
      } else {
        fit.fac.name <- fit.term.names[i]
      }
      # find coef-position
      coef.pos <- which(coef.names == fit.fac.name)
      # ---------------------------------------------
      # Here we go with predicted probabilities
      # for each single term, w/o including remaining
      # co-variates to predict the values. This can be
      # used to investigate a term "isolated"
      # ---------------------------------------------
      if (type == "prob") {
        # calculate x-beta by multiplying original values with estimate of that term
        mydf.vals$xbeta <- mydf.vals$values * coef(fit)[coef.pos]
        # calculate probability (y) via cdf-function
        mydf.vals$y <- plogis(coef(fit)[1] + mydf.vals$xbeta)
      # ---------------------------------------------
      # Here we go with predicted probabilities,
      # with all remaining co-variates set to zero or mean
      # i.e. we are using the 'predict' function to predict
      # fitted values of terms
      # ---------------------------------------------
      } else {
        # get predicted values. Note that the returned matrix
        # does not contain response value
        pred.vals <- predict(fit, type = "terms")
        # retrieve predicted prob. of term. coef.column -1
        # because coef.column relates to fit$model, which has one
        # more column (response)
        mydf.vals$y <- plogis(pred.vals[, coef.column - 1])
        # add title prefix for predicted values
      }
      # assign group
      mydf.vals$grp = coef.names[coef.pos]
      # add mydf to list
      mydf.metricpred[[length(mydf.metricpred) + 1]] <- mydf.vals
      # save predictor name
      axisLabels.mp <- c(axisLabels.mp, fit.term.names[i])
    }
  }
  # ---------------------------------------------------------
  # Prepare metric plots
  # ---------------------------------------------------------
  if (length(mydf.metricpred) > 0) {
    # create mydf for integrated plot
    mydf.ges <- data.frame()
    for (i in 1:length(mydf.metricpred)) {
      # "melt" all single mydf's to one
      mydf.ges <- rbind(mydf.ges, mydf.metricpred[[i]])
      # create single plots for each numeric predictor
      mp <- ggplot(mydf.metricpred[[i]], aes(x = values, y = y)) +
        labs(x = axisLabels.mp[i], 
             y = "Predicted Probability") +
        stat_smooth(method = "glm", 
                    family = "binomial", 
                    se = show.se) +
        coord_cartesian(ylim = c(0, 1))
      # add plot to list
      plot.metricpred[[length(plot.metricpred) + 1]] <- mp
    }
    # if we have more than one numeric var, also create integrated plot
    if (length(mydf.metricpred) > 1) {
      mp <- ggplot(mydf.ges, aes(x = values,
                                 y = y,
                                 colour = grp)) +
        labs(x = NULL,
             y = "Predicted Probability",
             colour = "Term",
             title = "Predicted probabilities of coefficients") +
        scale_colour_manual(values = brewer_pal(palette = "Set1")(length(axisLabels.mp)),
                            labels = axisLabels.mp) +
        stat_smooth(method = "glm", 
                    family = "binomial", 
                    se = show.se) +
        coord_cartesian(ylim = c(0, 1)) +
        facet_wrap(~grp,
                   ncol = round(sqrt(length(mydf.metricpred))),
                   scales = "free_x") +
        guides(colour = FALSE)
      # add integrated plot to plot list
      plot.facet <- mp
      # add integrated data frame to plot list
      mydf.facet <- mydf.ges
    }
  }
  # --------------------------
  # plot plots
  # --------------------------
  if (printPlot) {
    if (facet.grid && !is.null(plot.facet)) {
      print(plot.facet)
    } else {
      for (i in 1:length(plot.metricpred)) {
        print(plot.metricpred[[i]])
      }
    }
  }

  invisible(structure(class = "sjpglm.pc",
                      list(mydf.mp = mydf.metricpred,
                           plot.mp = plot.metricpred,
                           mydf.facet = mydf.facet,
                           plot.facet = plot.facet)))
}


sjp.glm.response.probcurv <- function(fit,
                                      show.se,
                                      printPlot) {
  # ----------------------------
  # get predicted values for response
  # ----------------------------
  pp <- plogis(predict(fit, type = "response"))
  # ----------------------------
  # get predicted probabilities for 
  # response, including random effects
  # ----------------------------
  mydf <- data.frame(x = 1:length(pp), y = sort(pp))
  # ---------------------------------------------------------
  # Prepare plot
  # ---------------------------------------------------------
  mp <- ggplot(mydf, aes(x = x, y = y)) +
    labs(x = NULL, 
         y = "Predicted Probability",
         title = "Predicted Probabilities for model-response") +
    stat_smooth(method = "glm", 
                family = "binomial", 
                se = show.se) +
    # cartesian coord still plots range of se, even
    # when se exceeds plot range.
    coord_cartesian(ylim = c(0, 1))
  # --------------------------
  # plot plots
  # --------------------------
  if (printPlot) print(mp)
  return(structure(class = "sjpglm.ppresp",
                   list(mydf = mydf,
                        plot = mp,
                        mean.pp = mean(pp))))
}


sjp.glm.ma <- function(logreg, showOriginalModelOnly=TRUE) {
  # ---------------------------------
  # remove outliers
  # ---------------------------------
  # copy current model
  model <- logreg
  # get AIC-Value
  aic <- logreg$aic
  # maximum loops
  maxloops <- 10
  maxcnt <- maxloops
  # remember how many cases have been removed
  removedcases <- 0
  loop <- TRUE
  # start loop
  while (loop == TRUE) {
    # get outliers of model
    ol <- outlierTest(model)
    # retrieve variable numbers of outliers
    vars <- as.numeric(attr(ol$p, "names"))
    # update model by removing outliers
    dummymodel <- update(model, subset = -c(vars))
    # retrieve new AIC-value
    dummyaic <- dummymodel$aic
    # decrease maximum loops
    maxcnt <- maxcnt - 1
    # check whether AIC-value of updated model is larger
    # than previous AIC-value or if we have already all loop-steps done,
    # stop loop
    if (dummyaic >= aic || maxcnt < 1) {
      loop <- FALSE
    } else {
      # else copy new model, which is the better one (according to AIC-value)
      model <- dummymodel
      # and get new AIC-value
      aic <- dummyaic
      # count removed cases
      removedcases <- removedcases + length(vars)
    }
  }
  # ---------------------------------
  # print steps from original to updated model
  # ---------------------------------
  message(sprintf(("Removed %i cases during %i step(s).\nAIC-value of original model: %.2f\nAIC-value of updated model: %.2f\n"),
                  removedcases,
                  maxloops - (maxcnt + 1),
                  logreg$aic,
                  model$aic))
  
  modelOptmized <- ifelse(removedcases > 0, TRUE, FALSE)
  if (showOriginalModelOnly) modelOptmized <- FALSE
  # ------------------------------------------------------
  # Overdispersion
  # Sometimes we can get a deviance that is much larger than expected
  # if the model was correct. It can be due to the presence of outliers,
  # sparse data or clustering of data. A half-normal plot of the residuals
  # can help checking for outliers:
  # ------------------------------------------------------
  halfnorm <- function(x, nlab = 2, labs = as.character(1:length(x)), ylab = "Sorted Data", ...) {
    x <- abs(x)
    labord <- order(x)
    x <- sort(x)
    i <- order(x)
    n <- length(x)
    ui <- qnorm((n + 1:n) / (2 * n + 1))
    plot(ui, 
         x[i], 
         xlab = "Half-normal quantiles", 
         ylab = ylab, 
         ylim = c(0, max(x)), 
         type = "n",
         ...)
    if (nlab < n) points(ui[1:(n - nlab)], x[i][1:(n - nlab)])
    text(ui[(n - nlab + 1):n], 
         x[i][(n - nlab + 1):n], 
         labs[labord][(n - nlab + 1):n])
  }
  # show half-normal quantiles for original model
  halfnorm(residuals(logreg), main = "Original model (over-/underdispersion)")
  if (!showOriginalModelOnly) {
    # show half-normal quantiles for updated model
    halfnorm(residuals(model), main = "Updated model (over-/underdispersion)")
  }
  # ------------------------------------------------------
  # Influential and leverage points
  # ------------------------------------------------------
  influencePlot(logreg)
  if (!showOriginalModelOnly) influencePlot(model)
  # ------------------------------------------------------
  # Residual plot
  # ------------------------------------------------------
  res <- residuals(logreg, type = "deviance")
  plot(log(abs(predict(logreg))), 
       res, main = "Residual plot (original model)", 
       xlab = "Log-predicted values", 
       ylab = "Deviance residuals")
  abline(h = 0, lty = 2)
  qqnorm(res)
  qqline(res)
  if (!showOriginalModelOnly) {
    res <- residuals(model, type = "deviance")
    plot(log(abs(predict(model))), 
         res, 
         main = "Residual plot (updated model)", 
         xlab = "Log-predicted values", 
         ylab = "Deviance residuals")
    abline(h = 0, lty = 2)
    qqnorm(res)
    qqline(res)
  }
  # -------------------------------------
  # Anova-Test
  # We can see that all terms were highly significant when they were
  # introduced into the model.
  # -------------------------------------
  message("--------------------\nCheck significance of terms when they entered the model...")
  message("Anova original model:")
  print(anova(logreg, test = "Chisq"))
  if (!showOriginalModelOnly) {
    message(paste("\n\nAnova updated model:\n"))
    print(anova(model, test = "Chisq"))
  }
  # -------------------------------------
  sjp.setTheme(theme = "forestgrey")
  sjp.glm(logreg, title = "Original model")
  if (!showOriginalModelOnly) sjp.glm(model, title = "Updated model")
  # return updated model
  return(model)
}
