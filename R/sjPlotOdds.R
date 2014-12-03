# bind global variables
if(getRversion() >= "2.15.1") utils::globalVariables(c("OR", "lower", "upper", "p"))



#' @title Plot odds ratios (forest plots)
#' @name sjp.glm
#'
#' @seealso \itemize{
#'              \item \href{http://www.strengejacke.de/sjPlot/sjp.glm/}{sjPlot manual: sjp.glm}
#'              \item \code{\link{sjp.glmm}}
#'              \item \code{\link{sjp.glm.ma}}
#'              \item \code{\link{sjt.glm}}
#'              }
#'
#' @description Plot odds ratios (exponentiated coefficients) with confidence intervalls as bar chart or dot plot
#'
#' @note Based on the script from \href{http://www.surefoss.org/dataanalysis/plotting-odds-ratios-aka-a-forrestplot-with-ggplot2/}{surefoss}
#'
#' @param fit The fitted model of a logistic regression (or any other \code{\link{glm}}-object).
#' @param sortOdds If \code{TRUE} (default), the odds ratios are ordered according their OR value from highest first
#'          to lowest last. Use \code{FALSE} if you don't want to change the order of the predictors.
#' @param title Diagram's title as string.
#'          Example: \code{title=c("my title")}
#' @param axisLabels.y Labels of the predictor variables (independent vars, odds) that are used for labelling the
#'          axis. Passed as vector of strings.
#'          Example: \code{axisLabels.y=c("Label1", "Label2", "Label3")}
#'          Note: If you use the \code{\link{sji.SPSS}} function and the \code{\link{sji.getValueLabels}} function, you receive a
#'          \code{list} object with label string. The labels may also be passed as list object. They will be unlisted and
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
#' @param gridBreaksAt Sets the breaks on the y axis, i.e. at every n'th position a major
#'          grid is being printed. Default is 0.5
#' @param transformTicks if \code{TRUE}, the grid bars have exponential distances (equidistant), i.e. they
#'          visually have the same distance from one grid bar to the next. Default is \code{FALSE} which
#'          means that grids are plotted on every \code{gridBreaksAt}'s position, thus the grid bars
#'          become narrower with higher odds ratio values.
#' @param type type of plot. Use one of following:
#'          \itemize{
#'            \item \code{"dots"} or \code{"or"} (default) for odds ratios (forest plot)
#'            \item \code{"bars"} for odds ratios as bar plot
#'            \item \code{"prob"} or \code{"pc"} to plot probability curves of coefficients (model terms). Use \code{facet.grid} to decide whether to plot each coefficient as separate plot or as integrated faceted plot.
#'          }
#' @param geom.colors User defined color palette for geoms. Must either be vector with two color values
#'          or a specific color palette code (see below).
#'          \itemize{
#'            \item If not specified, the diverging \code{"Paired"} color brewer palette will be used.
#'            \item If \code{"gs"}, a greyscale will be used.
#'            \item If \code{geom.colors} is any valid color brewer palette name, the related \href{http://colorbrewer2.org}{color brewer} palette will be used. Use \code{display.brewer.all()} from the \code{RColorBrewer} package to view all available palette names.
#'          }
#'          Else specify your own color values as vector (e.g. \code{geom.colors=c("#f00000", "#00ff00")}).
#' @param geom.size size resp. width of the geoms (bar width or point size, depending on \code{type} parameter).
#' @param hideErrorBars If \code{TRUE}, the error bars that indicate the confidence intervals of the odds ratios are not
#'          shown. Only applies if parameter \code{type} is \code{bars}. Default value is \code{FALSE}.
#' @param interceptLineType The linetype of the intercept line (zero point). Default is \code{2} (dashed line).
#' @param interceptLineColor The color of the intercept line. Default value is \code{"grey70"}.
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
#' @param showModelSummary If \code{TRUE} (default), a summary of the regression model with
#'          Intercept, R-square, F-Test and AIC-value is printed to the lower right corner
#'          of the diagram.
#' @param show.se Use \code{TRUE} to plot (depending on \code{type}) the standard
#'          error for probability curves.
#' @param facet.grid \code{TRUE} when each plot should be plotted separately instead of
#'          an integrated (faceted) single graph. Only applies, if \code{type="prob"}.
#' @param printPlot If \code{TRUE} (default), plots the results as graph. Use \code{FALSE} if you don't
#'          want to plot any graphs. In either case, the ggplot-object will be returned as value.
#' @return (Invisibly) returns a structure with following elements:
#'         \itemize{
#'          \item \code{plot}: ggplot-object with the complete plot
#'          \item \code{mydf}: data frame that was used for setting up the ggplot-object
#'          \item \code{mydf.mp}: a list of data frames with the data for metric predictors (terms of type \code{numeric}), which will be plotted if \code{showContPredPlots} is \code{TRUE}
#'          \item \code{plot.mp}: a list of ggplot-objects with plots of metric predictors (terms of type \code{numeric}), which will be plotted if \code{showContPredPlots} is \code{TRUE}
#'         }
#'
#' @examples
#' # prepare dichotomous dependent variable
#' y <- ifelse(swiss$Fertility<median(swiss$Fertility), 0, 1)
#'
#' # fit model
#' fitOR <- glm(y ~ swiss$Education + swiss$Examination + swiss$Infant.Mortality + swiss$Catholic,
#'              family=binomial(link="logit"))
#'
#' # print Odds Ratios as dots
#' sjp.glm(fitOR)
#'
#' # print Odds Ratios as bars
#' sjp.glm(fitOR, type="bars")
#'
#'
#' # -------------------------------
#' # Predictors for negative impact
#' # of care. Data from the EUROFAMCARE
#' # sample dataset
#' # -------------------------------
#' data(efc)
#' # retrieve predictor variable labels
#' labs <- sji.getVariableLabels(efc)
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
#' mydf <- na.omit(data.frame(y = as.factor(y),
#'                            sex = as.factor(efc$c161sex),
#'                            dep = as.factor(efc$e42dep),
#'                            barthel = as.numeric(efc$barthtot),
#'                            edu.mid = as.factor(edu.mid),
#'                            edu.hi = as.factor(edu.high)))
#' # fit model
#' fit <- glm(y ~., data = mydf, family = binomial(link = "logit"))
#' # plot odds
#' sjp.glm(fit,
#'         title = labs[['neg_c_7']],
#'         axisLabels.y = predlab)
#'
#' # plot probability curves of coefficients
#' sjp.glm(fit,
#'         title = labs[['neg_c_7']],
#'         axisLabels.y = predlab,
#'         type = "prob")
#'
#' @import ggplot2
#' @importFrom reshape2 melt
#' @export
sjp.glm <- function(fit,
                    sortOdds=TRUE,
                    title=NULL,
                    axisLabels.y=NULL,
                    axisTitle.x="Odds Ratios",
                    axisLimits=NULL,
                    breakTitleAt=50,
                    breakLabelsAt=25,
                    gridBreaksAt=0.5,
                    transformTicks=FALSE,
                    type="dots",
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
                    printPlot=TRUE) {
  # --------------------------------------------------------
  # check type
  # --------------------------------------------------------
  if (type == "prob" || type == "pc") {
    return(invisible(sjp.glm.pc(fit,
                                show.se,
                                facet.grid,
                                printPlot)))
  }
  if (type == "or") type <- "dots"
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
    axisLabels.y <- c()
    # iterate coefficients (1 is intercept or response)
    for (i in 2 : ncol(fit$model)) {
      # check if we hav label
      lab <- autoSetVariableLabels(fit$model[, i])
      # if not, use coefficient name
      if (is.null(lab)) {
        lab <- attr(fit$coefficients[i], "names")
      }
      axisLabels.y <- c(axisLabels.y, lab)
    }
  }
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
  # check length of x-axis-labels and split longer strings at into new lines
  if (!is.null(axisLabels.y)) {
    axisLabels.y <- sju.wordwrap(axisLabels.y, breakLabelsAt)
  }
  # create data frame for ggplot
  tmp <- data.frame(cbind(exp(coef(fit)), exp(confint(fit))))
  # ----------------------------
  # print p-values in bar charts
  # ----------------------------
  # retrieve sigificance level of independent variables (p-values)
  pv <- coef(summary(fit))[,4]
  # for better readability, convert p-values to asterisks
  # with:
  # p < 0.001 = ***
  # p < 0.01 = **
  # p < 0.05 = *
  # retrieve odds ratios
  ov <- exp(coef(fit))
  # init data column for p-values
  ps <- NULL
  for (i in 1:length(pv)) {
    ps[i] <- c("")
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
  if (showPValueLabels) {
    for (i in 1:length(pv)) {
      if (pv[i]>=0.05) {
      }
      else if (pv[i]>=0.01 && pv[i]<0.05) {
        ps[i] <- paste(ps[i], "*")
      }
      else if (pv[i]>=0.001 && pv[i]<0.01) {
        ps[i] <- paste(ps[i], "**")
      }
      else {
        ps[i] <- paste(ps[i], "***")
      }
    }
  }
  # ----------------------------
  # remove intercept
  # ----------------------------
  odds <- cbind(tmp[-1,])
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
  if (sortOdds) {
    axisLabels.y <- axisLabels.y[order(ov)]
  }
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
  lhj <- ifelse(odds$OR>1, 1.3, -0.3)
  odds <- cbind(odds, labhjust=lhj)
  lhj <- ifelse(tmp$OR>1, 1.3, -0.3)
  tmp <- cbind(tmp, labhjust=lhj)
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
    }
    # else, we have to adjuste the axis limits to max/min
    # values just of odds ratios
    else {
      rdf <- odds
    }
    # check whether we have bar chart and error bars hidden
    # in this case, the upper limit does not correspond to the
    # upper CI, but to the highest OR value
    if (type=="bars" && hideErrorBars) {
      maxval <- max(rdf$OR)
      minval <- min(rdf$OR)
    }
    else {
      # else we have confindence intervals displayed, so
      # the range corresponds to the boundaries given by
      # the CI's
      maxval <- max(rdf$upper)
      minval <- min(rdf$lower)
    }
    upper_lim <- (ceiling(10*maxval)) / 10
    lower_lim <- (floor(10*minval)) / 10
    # give warnings when auto-limits are very low/high
    if ((minval < 0.1) || (maxval > 100)) {
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
  ticks <- c(seq(lower_lim, upper_lim, by=gridBreaksAt))
  # ----------------------------
  # create expression with model summarys. used
  # for plotting in the diagram later
  # ----------------------------
  if (showModelSummary) {
    psr <- PseudoR2(fit)
    modsum <- as.character(as.expression(
      substitute("(Intercept)" == ic * "," ~~ italic(R)[CS]^2 == r2cs * "," ~~ italic(R)[N]^2 == r2n * "," ~~ -2 * lambda == la * "," ~~ chi^2 == c2 * "," ~~ "AIC" == aic,
                 list(ic=sprintf("%.2f", exp(coef(fit)[1])),
                      r2cs=sprintf("%.3f", psr[2]),
                      r2n=sprintf("%.3f", psr[3]),
                      la=sprintf("%.2f", -2*logLik(fit)),
                      c2=sprintf("%.2f", Chisquare.glm(fit)),
                      aic=sprintf("%.2f", fit$aic)))))
    cat(sprintf("Intercept = %.2f\nR2[cs] = %.3f\nR2[n] = %.3f\nLambda = %.2f\nChi2 = %.2f\nAIC = %.2f",
            exp(coef(fit)[1]),
            psr[2],
            psr[3],
            -2*logLik(fit),
            Chisquare.glm(fit),
            fit$aic))
  }
  else {
    modsum <- NULL
  }
  if (!showAxisLabels.y) {
    axisLabels.y <- c("")
  }
  # --------------------------------------------------------
  # Order odds according to beta-coefficients
  # --------------------------------------------------------
  if (sortOdds) {
    odds <- odds[order(ov),]
  }
  odds$vars <- cbind(c(1:nrow(odds)))
  odds$vars <- as.factor(odds$vars)
  # --------------------------------------------------------
  # check whether intercept should be shown
  # --------------------------------------------------------
  if (showIntercept) {
    odds <- data.frame(rbind(tmp[1,], odds))
    axisLabels.y <- c("Intercept", axisLabels.y)
  }
  # --------------------------------------------------------
  # body of plot, i.e. this is the same in both bar and dot plots
  # --------------------------------------------------------
  # plot as bars, fill bars according to
  # OR-value greater / lower than 1
  plotHeader <- ggplot(odds, aes(y=OR, x=vars))
  # --------------------------------------------------------
  # start with dot-plotting here
  # --------------------------------------------------------
  if (type=="dots") {
    plotHeader <- plotHeader +
      # Order odds according to beta-coefficients, colour points and lines according to
      # OR-value greater / lower than 1
      geom_point(size=geom.size, aes(colour=(OR>1))) +
      # print confidence intervalls (error bars)
      geom_errorbar(aes(ymin=lower, ymax=upper, colour=(OR>1)), width=0) +
      # print value labels and p-values
      geom_text(aes(label=p, y=OR), vjust=-0.7)
  }
  # --------------------------------------------------------
  # start with bar plots here
  # --------------------------------------------------------
  else if (type=="bars") {
    # Order odds according to beta-coefficients, colour points and lines according to
    # OR-value greater / lower than 1
    plotHeader <- plotHeader +
      # stat-parameter indicates statistics
      # stat="bin": y-axis relates to count of variable
      # stat="identity": y-axis relates to value of variable
      geom_bar(aes(fill=(OR>1)), stat="identity", position="identity", width=geom.size) +
      # print value labels and p-values
      geom_text(aes(label=p, y=1), vjust=-1, hjust=odds$labhjust)
    if (hideErrorBars==FALSE) {
      plotHeader <- plotHeader +
        # print confidence intervalls (error bars)
      geom_errorbar(aes(ymin=lower, ymax=upper), colour="black", width=0)
    }
  }
  # ------------------------------------------
  # add annotations with model summary
  # here we print out the log-lik-ratio "lambda" and the chi-square significance of the model
  # compared to the null-model
  # ------------------------------------------
  plotHeader <- print.table.summary(plotHeader,
                                    modsum)
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
  }
  else {
    plotHeader <- plotHeader +
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
  # ---------------------------------------------------------
  # set geom colors
  # ---------------------------------------------------------
  plotHeader <- sj.setGeomColors(plotHeader, geom.colors, 2, FALSE)
  # ---------------------------------------------------------
  # Check whether ggplot object should be returned or plotted
  # ---------------------------------------------------------
  if (printPlot) {
    # print base plot
    print(plotHeader)
  }
  # -------------------------------------
  # return results
  # -------------------------------------
  invisible (structure(class = "sjpglm",
                       list(plot = plotHeader,
                            mydf = odds)))
}


sjp.glm.pc <- function(fit,
                       show.se,
                       facet.grid,
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
  # retrieve data classes of model terms to check whether
  # we have any numeric terms in fitted model
  fit.data.classes <- unname(attr(fit$terms, "dataClasses"))
  # retrieve term names, so we find the estimates in the
  # coefficients list
  fit.term.names <- names(attr(fit$terms, "dataClasses"))[-1]
  # ----------------------------
  # loop through all coefficients
  # ----------------------------
  for (i in 1 : length(fit.term.names)) {
    # get values from coefficient
    coef.column <- which(colnames(fit$model) == fit.term.names[i])
    # check if we have found the coefficient
    if (length(coef.column) > 0) {
      # get values from numeric term
      vals <- fit$model[, coef.column]
      # sort values, for x axis
      vals.unique <- sort(vals)
      # melt variable
      mydf.vals <- data.frame(melt(vals.unique))
      # set colnames
      colnames(mydf.vals) <- c("value")
      # convert factor to numeric
      if (is.factor(mydf.vals$value)) mydf.vals$value <- sji.convertToValue(mydf.vals$value, 0)
      # retrieve names of coefficients
      coef.names <- names(coef(fit))
      # check if we have a factor, then we may have reference levels
      if (is.factor(vals)) {
        # add reference level to coefficient name
        ll <- levels(vals)
        fit.fac.name <- paste0(fit.term.names[i], ll[length(ll)])
      }
      else {
        fit.fac.name <- fit.term.names[i]
      }
      # find coef-position
      coef.pos <- which(coef.names == fit.fac.name)
      # calculate x-beta by multiplying original values with estimate of that term
      mydf.vals$xbeta <- mydf.vals$value * (coef(fit)[coef.pos])
      # calculate probability (y) via cdf-function
      mydf.vals$y <- odds.to.prob(coef(fit)[1] + mydf.vals$xbeta)
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
  if (length(mydf.metricpred)>0) {
    # create mydf for integrated plot
    mydf.ges <- data.frame()
    for (i in 1:length(mydf.metricpred)) {
      # "melt" all single mydf's to one
      mydf.ges <- rbind(mydf.ges, mydf.metricpred[[i]])
      # create single plots for each numeric predictor
      mp <- ggplot(mydf.metricpred[[i]], aes(x = value, y = y)) +
        labs(x = axisLabels.mp[i], y = "Probability") +
        stat_smooth(method = "glm", family = "binomial", se = show.se) +
        coord_cartesian(ylim = c(0, 1))
      # add plot to list
      plot.metricpred[[length(plot.metricpred)+1]] <- mp
    }
    # if we have more than one numeric var, also create integrated plot
    if (length(mydf.metricpred) > 1) {
      mp <- ggplot(mydf.ges, aes(x = value,
                                 y = y,
                                 colour = grp)) +
        labs(x = NULL,
             y = "Probability",
             colour = "Term",
             title = "Probability of coefficients") +
        scale_colour_manual(values = brewer_pal(palette = "Set1")(length(axisLabels.mp)),
                            labels = axisLabels.mp) +
        stat_smooth(method = "glm", family = "binomial", se = show.se) +
        coord_cartesian(ylim = c(0, 1)) +
        facet_wrap( ~ grp,
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
    }
    else {
      for (i in 1 : length(plot.metricpred)) {
        print(plot.metricpred[[i]])
      }
    }
  }

  invisible (structure(class = "sjpglm.pc",
                       list(mydf.mp = mydf.metricpred,
                            plot.mp = plot.metricpred,
                            mydf.facet = mydf.facet,
                            plot.facet = plot.facet)))
}


#' @title Plot model assumptions of glm's
#' @name sjp.glm.ma
#'
#' @description Plots model assumptions of generalized linear models
#'              to verify if generalized linear regression is applicable
#'
#' @seealso \code{\link{sjp.glm}}

#' @param logreg a fitted \code{\link{glm}}-model
#' @param showOriginalModelOnly if \code{TRUE} (default), only the model assumptions of the fitted model
#'   \code{logreg} are plotted. if \code{FALSE}, the model assumptions of an updated model where outliers
#'   are automatically excluded are also plotted.
#' @return an updated fitted generalized linear model where outliers are dropped out.
#'
#' @examples
#' # prepare dichotomous dependent variable
#' y <- ifelse(swiss$Fertility<median(swiss$Fertility), 0, 1)
#'
#' # fit model
#' fitOR <- glm(y ~ swiss$Education + swiss$Examination + swiss$Infant.Mortality + swiss$Catholic,
#'              family=binomial(link="logit"))
#'
#' # plot model assumptions
#' sjp.glm.ma(fitOR)
#'
#' @importFrom car outlierTest influencePlot
#' @export
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
  while(loop==TRUE) {
    # get outliers of model
    ol <- outlierTest(model)
    # retrieve variable numbers of outliers
    vars <- as.numeric(attr(ol$p, "names"))
    # update model by removing outliers
    dummymodel <- update(model, subset=-c(vars))
    # retrieve new AIC-value
    dummyaic <- dummymodel$aic
    # decrease maximum loops
    maxcnt <- maxcnt -1
    # check whether AIC-value of updated model is larger
    # than previous AIC-value or if we have already all loop-steps done,
    # stop loop
    if(dummyaic >= aic || maxcnt<1) {
      loop <- FALSE
    }
    else {
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
  cat(sprintf(("\nRemoved %i cases during %i step(s).\nAIC-value of original model: %.2f\nAIC-value of updated model: %.2f\n\n"),
              removedcases,
              maxloops-(maxcnt+1),
              logreg$aic,
              model$aic))

  modelOptmized <- ifelse(removedcases>0, TRUE, FALSE)
  if (showOriginalModelOnly) modelOptmized <- FALSE
  # ---------------------------------
  # show VIF-Values
  # ---------------------------------
  sjp.vif(logreg)
  if (modelOptmized) sjp.vif(model)
  # ------------------------------------------------------
  # Overdispersion
  # Sometimes we can get a deviance that is much larger than expected
  # if the model was correct. It can be due to the presence of outliers,
  # sparse data or clustering of data. A half-normal plot of the residuals
  # can help checking for outliers:
  # ------------------------------------------------------
  halfnorm <- function (x, nlab=2, labs=as.character(1:length(x)), ylab="Sorted Data", ...) {
    x <- abs(x)
    labord <- order(x)
    x <- sort(x)
    i <- order(x)
    n <- length(x)
    ui <- qnorm((n + 1:n)/(2 * n + 1))
    plot(ui, x[i], xlab="Half-normal quantiles", ylab=ylab, ylim=c(0,max(x)), type="n", ...)
    if(nlab < n) {
      points(ui[1:(n - nlab)], x[i][1:(n - nlab)])
    }
    text(ui[(n - nlab + 1):n], x[i][(n - nlab + 1):n], labs[labord][(n - nlab + 1):n])
  }
  # show half-normal quantiles for original model
  halfnorm(residuals(logreg), main="Original model (over-/underdispersion)")
  if (!showOriginalModelOnly) {
    # show half-normal quantiles for updated model
    halfnorm(residuals(model), main="Updated model (over-/underdispersion)")
  }
  # ------------------------------------------------------
  # Influential and leverage points
  # ------------------------------------------------------
  influencePlot(logreg)
  if (!showOriginalModelOnly) {
    influencePlot(model)
  }
  # ------------------------------------------------------
  # Residual plot
  # ------------------------------------------------------
  res <- residuals(logreg, type="deviance")
  plot(log(abs(predict(logreg))), res, main="Residual plot (original model)", xlab="Log-predicted values", ylab="Deviance residuals")
  abline(h=0, lty=2)
  qqnorm(res)
  qqline(res)
  if (!showOriginalModelOnly) {
    res <- residuals(model, type="deviance")
    plot(log(abs(predict(model))), res, main="Residual plot (updated model)", xlab="Log-predicted values", ylab="Deviance residuals")
    abline(h=0, lty=2)
    qqnorm(res)
    qqline(res)
  }
  # -------------------------------------
  # Anova-Test
  # We can see that all terms were highly significant when they were
  # introduced into the model.
  # -------------------------------------
  cat(paste("\n--------------------\nCheck significance of terms when they entered the model...\n"))
  cat(paste("\nAnova original model:\n"))
  print(anova(logreg,test="Chisq"))
  if (!showOriginalModelOnly) {
    cat(paste("\n\n\nAnova updated model:\n"))
    print(anova(model,test="Chisq"))
  }
  # -------------------------------------
  sjp.glm(logreg, title="Original model")
  if (!showOriginalModelOnly) {
    sjp.glm(model, title="Updated model")
  }
  # return updated model
  return(model)
}
