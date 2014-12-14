# bind global variables
if(getRversion() >= "2.15.1") utils::globalVariables(c("vars", "Beta", "xv", "lower", "upper", "stdbeta", "p", "x", "ydiff", "y", "grp", ".stdresid", ".resid", ".fitted", "V1", "V2"))


#' @title Plot beta coefficients of lm
#' @name sjp.lm
#' 
#' @seealso \itemize{
#'              \item \href{http://www.strengejacke.de/sjPlot/sjp.lm}{sjPlot manual: sjp.lm}
#'              \item \code{\link{sjp.lm.ma}}
#'              \item \code{\link{sjp.reglin}}
#'              \item \code{\link{sjp.int}}
#'              \item \code{\link{sjp.scatter}}
#'              \item  \code{\link{sjs.stdb}}
#'             }
#' 
#' @description Plot beta coefficients (estimates) of linear regressions with confidence intervalls as dot plot
#'                (forest plot). Additionally, the standardized beta values are plotted
#'                as red dots.
#'                
#' @param fit The model of the linear regression (lm-Object).
#' @param title Diagram's title as string.
#'          Example: \code{title=c("my title")}
#' @param sort Determines whether the predictors are sorted by beta-values (default, or use \code{"beta"} as
#'          parameter) or by standardized beta values (use \code{"std"}).
#' @param axisLabels.y Labels of the predictor variables (independent vars) that are used for labelling the
#'          axis. Passed as vector of strings.
#'          Example: \code{axisLabels.y=c("Label1", "Label2", "Label3")}.
#'          Note: If you use the \code{\link{sji.SPSS}} function and the \code{\link{sji.getValueLabels}} function, you receive a
#'          list object with label string. The labels may also be passed as list object. They will be unlisted and
#'          converted to character vector automatically.
#' @param showAxisLabels.y Whether x axis text (category names, predictor labels) should be shown (use \code{TRUE})
#'          or not. Default is \code{TRUE}
#' @param axisTitle.x A label for the x axis. Default is \code{"Estimates"}.
#' @param axisLimits Defines the range of the axis where the beta coefficients and their confidence intervalls
#'          are drawn. By default, the limits range from the lowest confidence interval to the highest one, so
#'          the diagram has maximum zoom. Use your own values as 2-value-vector, for instance: \code{limits=c(-0.8,0.8)}.
#' @param geom.colors User defined color palette for geoms. Must either be vector with two color values 
#'          or a specific color palette code (see below).
#'          \itemize{
#'            \item If not specified, the diverging \code{"Paired"} color brewer palette will be used.
#'            \item If \code{"gs"}, a greyscale will be used.
#'            \item If \code{geom.colors} is any valid color brewer palette name, the related \href{http://colorbrewer2.org}{color brewer} palette will be used. Use \code{display.brewer.all()} from the \code{RColorBrewer} package to view all available palette names.
#'          }
#'          Else specify your own color values as vector (e.g. \code{geom.colors=c("#f00000", "#00ff00")}).
#' @param geom.size size resp. width of the geoms (bar width or point size, depending on \code{type} parameter).
#' @param stdBetaLineType The standardized beta-value dots are connected by a thin line
#'          for a better overview. With this parameter you can specify the line type.
#' @param stdBetaLineAlpha The alpha-value for the line that connects the
#'          standardized beta-value dots.
#' @param interceptLineType The linetype of the intercept line (zero point). Default is \code{2} (dashed line).
#' @param interceptLineColor The color of the intercept line. Default value is \code{"grey70"}.
#' @param breakTitleAt Wordwrap for diagram title. Determines how many chars of the title are displayed in
#'          one line and when a line break is inserted into the title
#' @param breakLabelsAt Wordwrap for diagram labels. Determines how many chars of the category labels are displayed in 
#'          one line and when a line break is inserted
#' @param gridBreaksAt Sets the breaks on the y axis, i.e. at every n'th position a major
#'          grid is being printed. Default is \code{NULL}, so \code{\link{pretty}} gridbeaks will be used.
#' @param coord.flip If \code{TRUE} (default), predictors are plotted on the left y-axis and estimate
#'          values are plotted on the x-axis.
#' @param showValueLabels Whether the beta and standardized beta values should be plotted 
#'          to each dot or not.
#' @param labelDigits The amount of digits for rounding the estimations (see \code{showValueLabels}).
#'          Default is 2, i.e. estimators have 2 digits after decimal point.
#' @param showPValueLabels Whether the significance levels of each coefficient should be appended
#'          to values or not
#' @param showModelSummary If \code{TRUE} (default), a summary of the regression model with 
#'          Intercept, R-square, F-Test and AIC-value is printed to the lower right corner
#'          of the diagram.
#' @param showStandardBeta Whether or not the dots for the standardized beta values 
#'          should be plotted to the diagram.
#' @param showStandardBetaLine Whether or not the connecting line for the standardized beta values 
#'          should be plotted to the diagram. Default is \code{FALSE}.
#' @param printPlot If \code{TRUE} (default), plots the results as graph. Use \code{FALSE} if you don't
#'          want to plot any graphs. In either case, the ggplot-object will be returned as value.
#' @return (Insisibily) returns the ggplot-object with the complete plot (\code{plot}) as well as the data frame that
#'           was used for setting up the ggplot-object (\code{df}).
#' 
#' @examples
#' # fit linear model
#' fit <- lm(airquality$Ozone ~ airquality$Wind + airquality$Temp + airquality$Solar.R)
#' 
#' # plot estimates with CI and standardized beta-values
#' sjp.lm(fit, gridBreaksAt=2)
#' 
#' # plot estimates with CI without standardized beta-values
#' # and with narrower tick marks (because "gridBreaksAt" was not specified)
#' sjp.lm(fit, showStandardBeta=FALSE)
#' 
#' @import ggplot2
#' @export
sjp.lm <- function(fit,
                    sort="beta",
                    title=NULL,
                    axisLabels.y=NULL, 
                    showAxisLabels.y=TRUE,
                    axisTitle.x="Estimates",
                    axisLimits=NULL,
                    geom.colors="Set1",
                    geom.size=3,
                    stdBetaLineType=2,
                    stdBetaLineAlpha=0.3,
                    interceptLineType=2,
                    interceptLineColor="grey70",
                    breakTitleAt=50, 
                    breakLabelsAt=25, 
                    gridBreaksAt=NULL,
                    coord.flip=TRUE,
                    showValueLabels=TRUE, 
                    labelDigits=2,
                    showPValueLabels=TRUE,
                    showModelSummary=TRUE,
                    showStandardBeta=FALSE,
                    showStandardBetaLine=FALSE,
                    printPlot=TRUE) {
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
  # check length of diagram title and split longer string at into new lines
  # every 50 chars
  if (!is.null(title)) {
    title <- sju.wordwrap(title, breakTitleAt)
  }
  # check length of x-axis title and split longer string at into new lines
  # every 50 chars
  if (!is.null(axisTitle.x)) {
    axisTitle.x <- sju.wordwrap(axisTitle.x, breakTitleAt)
  }
  # check length of x-axis-labels and split longer strings at into new lines
  # every 10 chars, so labels don't overlap
  if (!is.null(axisLabels.y)) {
    axisLabels.y <- sju.wordwrap(axisLabels.y, breakLabelsAt)
  }
  # ----------------------------
  # create expression with model summarys. used
  # for plotting in the diagram later
  # ----------------------------
  if (showModelSummary) {
    modsum <- sju.modsum.lm(fit)
  }
  else {
    modsum <- NULL
  }
  # ----------------------------
  # print beta- and p-values in bar charts
  # ----------------------------
  # retrieve sigificance level of independent variables (p-values)
  pv <- coef(summary(fit))[-1,4]
  # for better readability, convert p-values to asterisks
  # with:
  # p < 0.001 = ***
  # p < 0.01 = **
  # p < 0.05 = *
  # retrieve betas, leave out intercept ([-1])
  bv <- coef(fit)[-1]
  # retrieve standardized betas
  stdbv <- sjs.stdb(fit)
  # init data column for p-values
  ps <- sprintf("%.*f", labelDigits, bv)
  pstdbv <- sprintf("%.*f", labelDigits, stdbv)
  # if no values should be shown, clear
  # vector now
  if (!showValueLabels) {
    ps <- rep(c(""), length(ps))
    pstdbv <- rep(c(""), length(pstdbv))
  }
  # --------------------------------------------------------
  # copy p-values into data column
  # --------------------------------------------------------
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
  # --------------------------------------------------------
  # create new data.frame, since ggplot requires data.frame as parameter
  # The data frame contains betas, CI and p-values
  # --------------------------------------------------------
  # if we have only one independent variable, cbind does not
  # work, since it duplicates the coefficients. so we simply
  # concatenate here
  if (1==length(coefficients(fit)[-1])) {
    tmp <- data.frame(
      # Append beta coefficients, [-1] means that the first
      # row (Intercept) will be removed / ignored
      coefficients(fit)[-1],
      # append CI
      confint(fit, level=0.95)[-1,1],
      confint(fit, level=0.95)[-1,2])
  }
  else {
    tmp <- data.frame(cbind(
      # Append beta coefficients, [-1] means that the first
      # row (Intercept) will be removed / ignored
      coefficients(fit)[-1],
      # append CI
      confint(fit, level=0.95)[-1,]))
  }
  # append p-values and standardized beta coefficients
  # further more, we take the stand. beta as string, because in
  # case no values are drawn, we simply use an empty string.
  # finally, we need the p-values of the coefficients, because the value
  # labels may have different colours according to their significance level
  betas <- cbind(tmp, c(ps), sjs.stdb(fit), c(pstdbv), pv)
  # --------------------------------------------------------
  # check if user defined labels have been supplied
  # if not, use variable names from data frame
  # --------------------------------------------------------
  # auto-retrieving variable labels does not work when we
  # have factors with different levels, which appear as 
  # "multiple predictors", but are only one variable
  # --------------------------------------------------------
  if (is.null(axisLabels.y) || length(axisLabels.y) < length(row.names(betas))) {
    axisLabels.y <- row.names(betas)
  }
  # --------------------------------------------------------
  # define sorting criteria. the values on the x-axis are being sorted
  # either by beta-values (sort="beta") or by standardized
  # beta values (sort = anything else)
  # --------------------------------------------------------
  # sort labels descending in order of (std.) beta values
  # --------------------------------------------------------
  if (sort=="beta") {
    axisLabels.y <- axisLabels.y[order(bv)]
  }
  else {
    axisLabels.y <- axisLabels.y[order(stdbv)]
  }
  # --------------------------------------------------------
  # sort rows of data frame descending in order of (std.) beta values
  # --------------------------------------------------------
  if (sort=="beta") {
    betas <- betas[order(bv),]
  }
  else {
    betas <- betas[order(stdbv),]
  }
  betas <- cbind(c(seq(1:nrow(betas))), betas)
  # give columns names
  names(betas)<-c("xv", "Beta", "lower", "upper", "p", "stdbeta", "pstdbv", "pv")
  betas$p <- as.character(betas$p)
  # --------------------------------------------------------
  # Calculate axis limits. The range is from lowest lower-CI
  # to highest upper-CI, or a user-defined range (if "axisLimits"
  # is not NULL)
  # --------------------------------------------------------
  if (is.null(axisLimits)) {
    upper_lim <- (ceiling(10*max(betas$upper))) / 10
    lower_lim <- (floor(10*min(betas$lower))) / 10
  }
  else {
    lower_lim <- axisLimits[1]
    upper_lim <- axisLimits[2]
  }
  # determine gridbreaks
  if (is.null(gridBreaksAt)) {
    ticks <- pretty(c(lower_lim, upper_lim))
  }
  else {
    ticks <- c(seq(lower_lim, upper_lim, by=gridBreaksAt))
  }
  if (!showAxisLabels.y) {
    axisLabels.y <- c("")
  }
  # --------------------------------------------------------
  # Start plot here!
  # --------------------------------------------------------
  # show points for standard beta values
  if (showStandardBeta) {
    betaplot <- ggplot(betas, aes(y=stdbeta, x=xv, colour=Beta>=0)) +
      # Print std.beta-values. With vertical adjustment, so they don't overlap with the errorbars
      geom_text(aes(label=pstdbv, y=stdbeta), vjust=1.8, show_guide=FALSE)
      # to better distinguish betas and stand. betas, the stand. beta points can be connected with a line
    if (showStandardBetaLine) {
      # print line for standardized beta values
      betaplot <- betaplot +
        geom_line()
    }
  }
  else {
    betaplot <- ggplot(betas, aes(y=Beta, x=xv, colour=Beta>=0)) +
      # and error bar
      geom_errorbar(aes(ymin=lower, ymax=upper), width=0) +
      # Print p-values. With vertical adjustment, so they don't overlap with the errorbars
      geom_text(aes(label=p, y=Beta), vjust=-0.8, show_guide=FALSE)
  }
  betaplot <- betaplot +
    # print point
    geom_point(size=geom.size) +
    # Intercept-line
    geom_hline(yintercept=0, linetype=interceptLineType, color=interceptLineColor) +
    # set y-scale-limits, breaks and tick labels
    scale_y_continuous(limits=c(lower_lim,upper_lim), breaks=ticks, labels=ticks) +
    # set value labels to x-axis
    scale_x_discrete(labels=axisLabels.y, limits=c(1:nrow(betas))) +
    labs(title=title, x=NULL, y=axisTitle.x)
  # --------------------------------------------------------
  # flip coordinates?
  # --------------------------------------------------------
  if (coord.flip)  {
    betaplot <- betaplot +
      coord_flip()
  }
  # ------------------------------------------
  # check whether table summary should be printed
  # ------------------------------------------
  betaplot <- print.table.summary(betaplot,
                                  modsum)
  # ---------------------------------------------------------
  # set geom colors
  # ---------------------------------------------------------
  betaplot <- sj.setGeomColors(betaplot, geom.colors, 2, FALSE)
  # ---------------------------------------------------------
  # Check whether ggplot object should be returned or plotted
  # ---------------------------------------------------------
  if (printPlot) print(betaplot)
  # -------------------------------------
  # return results
  # -------------------------------------
  invisible (structure(class = "sjplm",
                       list(plot = betaplot,
                            df = betas)))
}


#' @title Plot regression lines for each predictor
#' @name sjp.reglin
#' 
#' @description Plot regression lines with confidence intervals for each single predictor of
#'                a fitted model. This method extracts all predictors of a fitted model and fits 
#'                each of them against the response variable. \cr \cr
#'                This function plots two lines: The resulting linear regression line
#'                including confidence interval (in blue) and a loess-smoothed line without
#'                confidence interval (in red). The better the linear relationship
#'                of predictor and response is, the more both lines should overlap
#'                (i.e. the red loess-smoothed line is almost linear). \cr \cr
#'                Furthermore, a scatter plot of response and predictor values
#'                is plotted.
#'                
#' @seealso \itemize{
#'              \item \href{http://www.strengejacke.de/sjPlot/sjp.lm}{sjPlot manual: sjp.lm}
#'              \item \code{\link{sjp.lm}}
#'              \item \code{\link{sjp.lm.ma}}
#'              \item \code{\link{sjp.int}}
#'              \item \code{\link{sjp.scatter}}
#'             }
#'          
#' @param fit The model of the linear regression (lm-Object).
#' @param title Diagram's title as string. By default \code{NULL}, i.e. no
#'          title is shown. Example: \code{title=c("my title")}
#' @param breakTitleAt Wordwrap for diagram title. Determines how many chars of the title are displayed in
#'          one line and when a line break is inserted into the title
#' @param lineColor The color of the regression line. Default is \code{"blue"}.
#' @param showCI If \code{TRUE} (default), a confidence region for the regression line
#'          will be plotted. Use \code{ciLevel} to specifiy the confidence level.
#' @param ciLevel The confidence level of the confidence region. Only applies when
#'          \code{showCI} is \code{TRUE}. Default is 0.95.
#' @param pointAlpha The alpha values of the scatter plot's point-geoms.
#'          Default is 0.2.
#' @param pointColor The color of the scatter plot's point-geoms. Only applies when \code{showScatterPlot}
#'          is \code{TRUE}. Default is \code{"black"}.
#' @param showScatterPlot If \code{TRUE} (default), a scatter plot of response and predictor values
#'          for each predictor of the fitted model \code{fit} is plotted.
#' @param showLoess If \code{TRUE} (default), an additional loess-smoothed line is plotted.
#' @param loessLineColor The color of the loess-smoothed line. Default is \code{"red"}. Only applies, if
#'          \code{showLoess} is \code{TRUE}.
#' @param showLoessCI If \code{TRUE}, a confidence region for the loess-smoothed line
#'          will be plotted. Default is \code{FALSE}. Use \code{loessCiLevel} to specifiy the confidence level.
#'          Only applies, if \code{showLoess} is \code{TRUE}.
#' @param loessCiLevel The confidence level of the loess-line's confidence region.
#'          Only applies, if \code{showLoessCI} is \code{TRUE}. Default is 0.95.
#' @param useResiduals If \code{TRUE}, the residuals (instead of response) are plotted 
#'          against each predictor. May be used for model diagnostics
#'          (see \url{https://www.otexts.org/fpp/5/4}).
#' @param printPlot If \code{TRUE} (default), plots the results as graph. Use \code{FALSE} if you don't
#'          want to plot any graphs. In either case, the ggplot-object will be returned as value.
#' @return (Insisibily) returns the ggplot-objects with the complete plot-list (\code{plot.list}) 
#'           as well as the data frame that were used for setting up the ggplot-objects (\code{df.list}).
#' 
#' @examples
#' data(efc)
#' fit <- lm(tot_sc_e ~ c12hour + e17age + e42dep, data=efc)
#' 
#' # reression line and scatter plot
#' sjp.reglin(fit)
#'            
#' # reression line w/o scatter plot
#' sjp.reglin(fit, showScatterPlot=FALSE)
#' 
#' # reression line w/o CI
#' sjp.reglin(fit, showCI=FALSE)
#' 
#' @import ggplot2
#' @export
sjp.reglin <- function(fit,
                       title=NULL,
                       breakTitleAt=50,
                       lineColor="blue",
                       showCI=TRUE,
                       ciLevel=0.95,
                       pointAlpha=0.2,
                       pointColor="black",
                       showScatterPlot=TRUE,
                       showLoess=TRUE,
                       loessLineColor="red",
                       showLoessCI=FALSE,
                       loessCiLevel=0.95,
                       useResiduals=FALSE,
                       printPlot=TRUE) {
  # -----------------------------------------------------------
  # retrieve amount of predictor variables
  # -----------------------------------------------------------
  listpv <- attr(fit$terms,"predvars")
  predvars <- attr(attr(fit$terms, "dataClasses"), "names")[-1]
  depvar.label <- attr(attr(fit$terms, "dataClasses"), "names")[1]
  # remember length of predictor variables
  predvars.length <- length(predvars)
  # -----------------------------------------------------------
  # retrieve name of dependent variable
  # -----------------------------------------------------------
  response <- ifelse(useResiduals==TRUE, "residuals", depvar.label)
  # -----------------------------------------------------------
  # retrieve column names of dataset so we can identify in which
  # column the data for each predictor is.
  # -----------------------------------------------------------
  cn <- colnames(fit$model)
  # init return var
  plotlist <- list()
  dflist <- list()
  # -----------------------------------------------------------
  # check length of diagram title and split longer string at into new lines
  # every 50 chars
  # -----------------------------------------------------------
  if (!is.null(title)) {
    title <- sju.wordwrap(title, breakTitleAt)
  }
  # -----------------------------------------------------------
  # iterate all predictors
  # -----------------------------------------------------------
  for (j in 1:predvars.length) {
    # -----------------------------------------------------------
    # retrieve each single predictor
    # -----------------------------------------------------------
    xval <- predvars[j]
    # -----------------------------------------------------------
    # create dummy-data frame with response and predictor
    # as data columns, used for the ggplot
    # -----------------------------------------------------------
    if (useResiduals) {
      mydat <- as.data.frame(cbind(fit$model[,which(cn==xval)],
                                   fit$residuals))
    }
    else {
      mydat <- as.data.frame(cbind(fit$model[,which(cn==xval)],
                                   fit$model[,which(cn==response)]))
    }
    # -----------------------------------------------------------
    # plot regression line and confidence intervall
    # -----------------------------------------------------------
    reglinplot <- ggplot(mydat, aes(x=V1, y=V2)) +
      stat_smooth(method="lm", se=showCI, level=ciLevel, colour=lineColor)
    # -----------------------------------------------------------
    # plot jittered values if requested
    # -----------------------------------------------------------
    if (showScatterPlot) {
      reglinplot <- reglinplot + geom_jitter(alpha=pointAlpha, colour=pointColor)
    }
    # -----------------------------------------------------------
    # check whether additional loess-line should be plotted
    # -----------------------------------------------------------
    if (showLoess) {
      reglinplot <- reglinplot + 
        stat_smooth(method="loess", se=showLoessCI, level=loessCiLevel, colour=loessLineColor)
    }
    # -----------------------------------------------------------
    # set plot labs
    # -----------------------------------------------------------
    reglinplot <- reglinplot + 
      labs(title=title, x=xval, y=response)
    # ---------------------------------------------------------
    # Check whether ggplot object should be returned or plotted
    # ---------------------------------------------------------
    # concatenate plot object
    plotlist[[length(plotlist)+1]] <- reglinplot
    dflist[[length(dflist)+1]] <- mydat
    # print plot
    if (printPlot) print(reglinplot)
  }
  # -------------------------------------
  # return results
  # -------------------------------------
  invisible (structure(class = "sjpreglin",
                       list(plot.list = plotlist,
                            df.list = dflist)))
}


#' @title Plot model assumptions of lm's
#' @name sjp.lm.ma
#' 
#' @description Plots model assumptions of linear models to verify if linear regression is applicable
#' 
#' @references \url{https://www.otexts.org/fpp/5/4}
#' 
#' @seealso \itemize{
#'              \item \href{http://www.strengejacke.de/sjPlot/sjp.lm}{sjPlot manual: sjp.lm}
#'              \item \code{\link{sjp.lm}}
#'              \item \code{\link{sjp.reglin}}
#'              \item \code{\link{sjp.int}}
#'             }
#'          
#' @param linreg a fitted lm-model
#' @param showOriginalModelOnly if \code{TRUE} (default), only the model assumptions of the fitted model
#'   \code{linreg} are plotted. if \code{FALSE}, the model assumptions of an updated model where outliers
#'   are automatically excluded are also plotted.
#' @param completeDiagnostic if \code{TRUE}, additional tests are performed. Default is \code{FALSE}
#' @return an updated fitted linear model where outliers are dropped out.
#' 
#' @examples
#' \dontrun{
#' # fit linear model
#' fit <- lm(airquality$Ozone ~ airquality$Wind + airquality$Temp + airquality$Solar.R)
#' fit.updated <- sjp.lm.ma(fit)
#' 
#' data(efc)
#' fit <- lm(tot_sc_e ~ c12hour + e17age + e42dep, data=efc)
#' fit.updated <- sjp.lm.ma(fit, completeDiagnostic = TRUE)}
#' 
#' @importFrom car outlierTest crPlots durbinWatsonTest leveragePlots ncvTest spreadLevelPlot
#' @export
sjp.lm.ma <- function(linreg, showOriginalModelOnly=TRUE, completeDiagnostic=FALSE) {
  # ------------------------
  # check if suggested package is available
  # ------------------------
  if (!requireNamespace("lmtest", quietly = TRUE)) {
    stop("Package 'lmtest' needed for this function to work. Please install it.", call. = FALSE)
  }
  # ---------------------------------
  # remove outliers
  # ---------------------------------
  # copy current model
  model <- linreg
  # get r2
  rs <- summary(model)$r.squared
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
    # retrieve new r2
    dummyrs <- summary(dummymodel)$r.squared
    # decrease maximum loops
    maxcnt <- maxcnt -1
    # check whether r2 of updated model is lower
    # than previous r2 or if we have already all loop-steps done,
    # stop loop
    if(dummyrs<rs || maxcnt<1) {
      loop <- FALSE
    }
    else {
      # else copy new model, which is the better one (according to r2)
      model <- dummymodel
      # and get new r2
      rs <- dummyrs
      # count removed cases
      removedcases <- removedcases + length(vars)
    }
  }
  # ---------------------------------
  # print steps from original to updated model
  # ---------------------------------
  message(sprintf(("Removed %i cases during %i step(s).\nR-square/adj. R-square of original model: %f / %f\nR-square/adj. R-square of updated model: %f / %f\n"), 
              removedcases,
              maxloops-(maxcnt+1), 
              summary(linreg)$r.squared, 
              summary(linreg)$adj.r.squared,
              summary(model)$r.squared, 
              summary(model)$adj.r.squared))
  modelOptmized <- ifelse(removedcases>0, TRUE, FALSE)
  if (showOriginalModelOnly) modelOptmized <- FALSE
  # ---------------------------------
  # show VIF-Values
  # ---------------------------------
  sjp.vif(linreg)
  if (modelOptmized) sjp.vif(model)
  # ---------------------------------
  # Print non-normality of residuals and outliers both of original and updated model
  # dots should be plotted along the line, this the dots should follow a linear direction
  # ---------------------------------
  mydf <- data.frame(x = sort(linreg$fitted.values), y = sort(linreg$residuals))
  print(ggplot(mydf, aes(x = x, y = y)) + 
          geom_point() + 
          stat_smooth(method = "lm", se = FALSE) +
          ggtitle("Non-normality of residuals and outliers (original model)\n(Dots should be plotted along the line)"))
  if (modelOptmized) {
    mydf <- data.frame(x = sort(model$fitted.values), y = sort(model$residuals))
    print(ggplot(mydf, aes(x = x, y = y)) + 
            geom_point() + 
            stat_smooth(method = "lm", se = FALSE) +
            ggtitle("Non-normality of residuals and outliers (updated model)\n(Dots should be plotted along the line)"))
  }
  # ---------------------------------
  # Print non-normality of residuals both of original and updated model
  # Distribution should look like normal curve
  # ---------------------------------
  print(ggplot(linreg, aes(x=.resid)) + 
          geom_histogram(aes(y=..density..), binwidth=0.2, fill="grey60", colour="grey30") +
          geom_density(aes(y=..density..), fill="#4080cc", alpha=0.2) +
          stat_function(fun=dnorm, args=list(mean=mean(unname(linreg$residuals), na.rm=TRUE), sd=sd(unname(linreg$residuals), na.rm=TRUE)), colour="FireBrick", size=0.8) +
          ggtitle("Non-normality of residuals (original model)\n(Distribution should look like normal curve)"))
  if (modelOptmized) {
    print(ggplot(model, aes(x=.resid)) + 
            geom_histogram(aes(y=..density..), binwidth=0.2, fill="grey60", colour="grey30") +
            geom_density(aes(y=..density..), fill="#4080cc", alpha=0.2) +
            stat_function(fun=dnorm, args=list(mean=mean(unname(model$residuals), na.rm=TRUE), sd=sd(unname(model$residuals), na.rm=TRUE)), colour="FireBrick", size=0.8) +
            ggtitle("Non-normality of residuals (updated model)\n(Distribution should look like normal curve)"))
  }
  # ---------------------------------
  # Non-constant residuals
  # ---------------------------------
  # Frage: Können hohe Werte auf der X-Achse genauso gut hervorgesagt
  # werden wie niedrige Werte auf der X-Achse? Das Muster muss sich ähneln
  # über den Verlauf der X-Achse
  #
  # The linearity assumption is supported to the extent that the amount 
  # of points scattered above and below the line is equal.
  #
  # A linear trend would mean that the error of the model (the difference between observed and fitted values) 
  # is in some way systematic. If, for instance, lower fitted values have residuals that are more towards the 0 line. 
  # Higher fitted values are consistently more off, so the model is more wrong with larger values. So, ideally what 
  # you want is something that is akin towards a horizontal line. In such case, the data is somehow not homogenous 
  # maybe because one part of the data is more variable than another. If that is the case, you might need to transform 
  # the data in order to make it meet the assumptions that are necessary for linear models.  
  print(ggplot(linreg, aes(x=.fitted, y=.resid)) +
          geom_hline(yintercept=0, alpha=0.7) +
          geom_point() +
          geom_smooth(method="loess", se=FALSE) +
          ggtitle("Homoscedasticity (homogeneity of variance,\nrandomly distributed residuals, original model)\n(Amount and distance of points scattered above/below line is equal)"))
  
  if (modelOptmized) {
    print(ggplot(model, aes(x=.fitted, y=.resid)) +
            geom_hline(yintercept=0, alpha=0.7) +
            geom_point() +
            geom_smooth(method="loess", se=FALSE) +
            ggtitle("Homoscedasticity (homogeneity of variance,\nrandomly distributed residuals, updated model)\n(Amount and distance of points scattered above/below line is equal)"))
  }
  # ---------------------------------
  # summarize old and new model
  # ---------------------------------
  sjp.lm(linreg, title="Original model")
  if (modelOptmized) sjp.lm(model, title="Updated model")
  if (completeDiagnostic) {
    # ---------------------------------
    # Plot residuals against predictors
    # ---------------------------------
    sjp.reglin(linreg, title="Relationship of residuals against predictors (original model) (if scatterplots show a pattern, relationship may be nonlinear and model needs to be modified accordingly", breakTitleAt=60, useResiduals = T)
    if (modelOptmized) sjp.reglin(model, title="Relationship of residuals against predictors (updated model) (if scatterplots show a pattern, relationship may be nonlinear and model needs to be modified accordingly", breakTitleAt=60, useResiduals = T)
    # ---------------------------------
    # Non-linearity
    # ---------------------------------
    plot(crPlots(linreg))
    # ---------------------------------
    # non-independence of residuals
    # ---------------------------------
    print(durbinWatsonTest(linreg))
    # ---------------------------------
    # Print leverage plots
    # ---------------------------------
    plot(leveragePlots(linreg))
    # ---------------------------------
    # Non-constant residuals
    # ---------------------------------
    print(ncvTest(linreg))
    print(lmtest::bptest(linreg))
    print(spreadLevelPlot(linreg))
  }
  # return updated model
  return(model)
}


#' @title Plot regression line of fitted lm
#' @name sjp.lm1
#' 
#' @description Plot a regression line with confidence interval for a fitted model with only
#'                one predictor (i.e. \code{lm(y~x)}).
#'                This function may plot two lines: The resulting linear regression line
#'                including confidence interval (in blue) by default, and a loess-smoothed line without
#'                confidence interval (in red) if parameter \code{showLoess} is \code{TRUE}.
#'                The better the linear relationship of predictor and response is, the more both lines should overlap
#'                (i.e. the red loess-smoothed line is almost linear). \cr \cr
#'                Furthermore, a scatter plot of response and predictor values
#'                is plotted.
#'                
#' @seealso \itemize{
#'              \item \href{http://www.strengejacke.de/sjPlot/sjp.lm}{sjPlot manual: sjp.lm}
#'              \item \code{\link{sjp.lm}}
#'              \item \code{\link{sjp.reglin}}
#'              \item \code{\link{sjp.scatter}}
#'             }
#'          
#' @param fit The model of the linear regression (lm-Object).
#' @param title Diagram's title as string.
#'          Example: \code{title=c("my title")}
#' @param breakTitleAt Wordwrap for diagram title. Determines how many chars of the title are displayed in
#'          one line and when a line break is inserted into the title
#' @param axisLabel.x Labels of the predictor (independent variable) that is used for labelling the
#'          axis. Passed as string.
#'          Example: \code{axisLabel.x=c("My Predictor Var")}.
#'          Note: If you use the \code{\link{sji.SPSS}} function and the \code{\link{sji.getVariableLabels}} function, you receive a
#'          character vector with variable label strings. You can use it like so:
#'          \code{axisLabel.x=sji.getVariableLabels(efc)['quol_5']}
#' @param axisLabel.y Labels of the response (dependent variable) that is used for labelling the
#'          axis. Passed as string.
#'          Example: \code{axisLabel.y=c("My Depdendent Var")}.
#'          Note: If you use the \code{\link{sji.SPSS}} function and the \code{\link{sji.getVariableLabels}} function, you receive a
#'          character vector with variable label strings. You can use it like so:
#'          \code{axisLabel.y=sji.getVariableLabels(efc)['neg_c_7']}
#' @param breakLabelsAt Wordwrap for axis labels. Determines how many chars of the category labels are displayed in 
#'          one line and when a line break is inserted
#' @param lineColor The color of the regression line. Default is \code{"blue"}.
#' @param showCI If \code{TRUE} (default), a confidence region for the regression line
#'          will be plotted. Use \code{ciLevel} to specifiy the confidence level.
#' @param ciLevel The confidence level of the confidence region. Only applies when
#'          \code{showCI} is \code{TRUE}. Default is 0.95.
#' @param pointAlpha The alpha values of the scatter plot's point-geoms.
#'          Default is 0.2.
#' @param pointColor The color of the scatter plot's point-geoms. Only applies when \code{showScatterPlot}
#'          is \code{TRUE}. Default is \code{"black"}.
#' @param showScatterPlot If \code{TRUE} (default), a scatter plot of response and predictor values
#'          for each predictor of the fitted model \code{fit} is plotted.
#' @param showLoess If \code{TRUE}, an additional loess-smoothed line is plotted.
#' @param loessLineColor The color of the loess-smoothed line. Default is \code{"red"}. Only applies, if
#'          \code{showLoess} is \code{TRUE}.
#' @param showLoessCI If \code{TRUE}, a confidence region for the loess-smoothed line
#'          will be plotted. Default is \code{FALSE}. Use \code{loessCiLevel} to specifiy the confidence level.
#'          Only applies, if \code{showLoess} is \code{TRUE}.
#' @param loessCiLevel The confidence level of the loess-line's confidence region.
#'          Only applies, if \code{showLoessCI} is \code{TRUE}. Default is 0.95.
#' @param showModelSummary If \code{TRUE} (default), a summary of the regression model with 
#'          Intercept, R-square, F-Test and AIC-value is printed to the lower right corner
#'          of the diagram.
#' @param useResiduals If \code{TRUE}, the residuals (instead of response) are plotted 
#'          against the predictor. May be used for model diagnostics
#'          (see \url{https://www.otexts.org/fpp/5/4}).
#' @param printPlot If \code{TRUE} (default), plots the results as graph. Use \code{FALSE} if you don't
#'          want to plot any graphs. In either case, the ggplot-object will be returned as value.
#' @return (Insisibily) returns the ggplot-object with the complete plot (\code{plot}) as well as the data frame that
#'           was used for setting up the ggplot-object (\code{df}).
#' 
#' @examples
#' # load sample data
#' data(efc)
#' # fit model
#' fit <- lm(neg_c_7 ~ quol_5, data=efc, na.action=na.omit)
#' # plot regression line
#' sjp.lm1(fit)
#' # plot regression line with label strings
#' sjp.lm1(fit,
#'         axisLabel.x = sji.getVariableLabels(efc$quol_5),
#'         axisLabel.y = sji.getVariableLabels(efc$neg_c_7),
#'         showLoess = TRUE)
#' 
#' @import ggplot2
#' @export
sjp.lm1 <- function(fit,
                   title=NULL,
                   breakTitleAt=50, 
                   axisLabel.x=NULL,
                   axisLabel.y=NULL,
                   breakLabelsAt=20,
                   lineColor="blue",
                   showCI=TRUE,
                   ciLevel=0.95,
                   pointAlpha=0.2,
                   pointColor="black",
                   showScatterPlot=TRUE,
                   showLoess=FALSE,
                   loessLineColor="red",
                   showLoessCI=FALSE,
                   loessCiLevel=0.95,
                   showModelSummary=TRUE,
                   useResiduals=FALSE,
                   printPlot=TRUE) {
  # -----------------------------------------------------------
  # check length of diagram title and split longer string at into new lines
  # every 50 chars
  # -----------------------------------------------------------
  if (!is.null(title)) {
    title <- sju.wordwrap(title, breakTitleAt)    
  }
  # -----------------------------------------------------------
  # remember length of predictor variables
  # -----------------------------------------------------------
  predvars.length <- length(fit$coefficients)
  # -----------------------------------------------------------
  # this function requires a fitted model with only one predictor,
  # so check whether only one predictor was used
  # -----------------------------------------------------------
  if (predvars.length>2) {
    stop("Only one predictor is allowed in fitted model. Formula y=b*x is plotted.", call.=FALSE)
  }
  # -----------------------------------------------------------
  # retrieve column names of dataset so we can identify in which
  # column the data for each predictor is.
  # -----------------------------------------------------------
  cn <- colnames(fit$model)
  # -----------------------------------------------------------
  # retrieve name of predictor and response
  # -----------------------------------------------------------
  response <- ifelse(useResiduals == TRUE, "residuals", cn[1])
  xval <- cn[2]
  # -----------------------------------------------------------
  # create dummy-data frame with response and predictor
  # as data columns, used for the ggplot
  # -----------------------------------------------------------
  if (useResiduals) {
    mydat <- as.data.frame(cbind(x = fit$model[,2],
                                 y = fit$residuals))
  }
  else {
    mydat <- as.data.frame(cbind(x = fit$model[,2],
                                 y = fit$model[,1]))
  }
  # ----------------------------
  # create expression with model summarys. used
  # for plotting in the diagram later
  # ----------------------------
  if (showModelSummary) {
    modsum <- sju.modsum.lm(fit)
  }
  else {
    modsum <- NULL
  }
  # ----------------------------
  # prepare axis labels
  # ----------------------------
  if (is.null(axisLabel.x)) {
    axisLabel.x <- xval
  }
  if (is.null(axisLabel.y)) {
    axisLabel.y <- response
  }
  # check length of axis-labels and split longer strings at into new lines
  # every 10 chars, so labels don't overlap
  axisLabel.x <- sju.wordwrap(axisLabel.x, breakLabelsAt)    
  axisLabel.y <- sju.wordwrap(axisLabel.y, breakLabelsAt)    
  # -----------------------------------------------------------
  # plot regression line and confidence intervall
  # -----------------------------------------------------------
  reglinplot <- ggplot(mydat, 
                       aes(x = x, y = y)) +
    stat_smooth(method = "lm", 
                se = showCI, 
                level = ciLevel, 
                colour = lineColor)
  # -----------------------------------------------------------
  # plot jittered values if requested
  # -----------------------------------------------------------
  if (showScatterPlot) {
    reglinplot <- reglinplot + geom_jitter(alpha = pointAlpha, 
                                           colour = pointColor)
  }
  # -----------------------------------------------------------
  # check whether additional loess-line should be plotted
  # -----------------------------------------------------------
  if (showLoess) {
    reglinplot <- reglinplot + 
      stat_smooth(method = "loess", 
                  se = showLoessCI, 
                  level = loessCiLevel, 
                  colour = loessLineColor)
  }
  # -----------------------------------------------------------
  # set plot labs
  # -----------------------------------------------------------
  reglinplot <- reglinplot + 
    labs(title = title, x = axisLabel.x, y = axisLabel.y)
  # ------------------------------------------
  # check whether table summary should be printed
  # ------------------------------------------
  reglinplot <- print.table.summary(reglinplot,
                                    modsum)
  # ---------------------------------------------------------
  # Check whether ggplot object should be returned or plotted
  # ---------------------------------------------------------
  if (printPlot) print(reglinplot)
  # -------------------------------------
  # return results
  # -------------------------------------
  invisible (structure(class = "sjplm1",
                       list(plot = reglinplot,
                            df = mydat)))
}
