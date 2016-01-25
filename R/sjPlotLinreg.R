# bind global variables
utils::globalVariables(c("vars", "Beta", "xv", "lower", "upper", "stdbeta", "p", "x", "ydiff", "y", "grp", ".stdresid", ".resid", ".fitted", "V1", "V2", "grp.est"))


#' @title Plot estimates or predicted values of linear models
#' @name sjp.lm
#'
#' @seealso \href{http://www.strengejacke.de/sjPlot/sjp.lm}{sjPlot manual: sjp.lm} for
#'            more details and examples of this function; use \code{\link{sjp.poly}}
#'            to see which polynomial degree fits best for possible polynomial terms.
#'
#' @description Depending on the \code{type}, this function plots coefficients (estimates)
#'                of linear regressions (including panel models fitted with the \code{plm}-function
#'                from the \pkg{plm}-package) with confidence intervals as dot plot (forest plot),
#'                model assumptions for linear models or slopes and scatter plots for each single
#'                coefficient. See \code{type} for details.
#'
#' @details \describe{
#'            \item{\code{type = "lm"}}{if fitted model only has one predictor, no forest plot is shown. Instead, a regression line with confidence interval (in blue) is plotted by default, and a loess-smoothed line without confidence interval (in red) can be added if argument \code{showLoess} is \code{TRUE}.}
#'            \item{\code{type = "std2"}}{plots standardized beta values, however, standardization follows \href{http://www.stat.columbia.edu/~gelman/research/published/standardizing7.pdf}{Gelman's (2008)} suggestion, rescaling the estimates by dividing them by two standard deviations instead of just one. Resulting coefficients are then directly comparable for untransformed binary predictors. This standardization uses the \code{\link[arm]{standardize}}-function.}
#'            \item{\code{type = "pred"}}{regression lines (slopes) with confidence intervals for each single predictor of the fitted model are plotted, i.e. all predictors of the fitted model are extracted and for each of them, the linear relationship is plotted against the response variable.}
#'            \item{\code{type = "resid"}}{is similar to the \code{type = "pred"} option, however, each predictor is plotted against the residuals (instead of response).}
#'            \item{\code{type = "resp"}}{the predicted values of the response for each observation is plotted, which mostly results in a single linear line.}
#'            \item{\code{type = "eff"}}{computes the marginal effects for all predictors, using the \code{\link[effects]{allEffects}} function. I.e. for each predictor, the predicted values towards the response are plotted, with all remaining co-variates set to the mean. Due to possible different scales of predictors, a faceted plot is printed (instead of plotting all lines in one plot). This function accepts following argument: \code{fit}, \code{title}, \code{geom.size}, \code{remove.estimates}, \code{showCI} and \code{printPlot}.}
#'            \item{\code{type = "poly"}}{plots the marginal effects of polynomial terms in \code{fit}, using the \code{\link[effects]{effect}} function, but only for a selected polynomial term, which is specified with \code{poly.term}. This function helps undertanding the effect of polynomial terms by plotting the curvilinear relationships of response and quadratic, cubic etc. terms. This function accepts following argument: \code{fit}, \code{poly.term}, \code{geom.colors}, \code{geom.size}, \code{axisTitle.x}, \code{showCI} and \code{printPlot}.}
#'            \item{\code{type = "ma"}}{checks model assumptions. Please note that only three arguments are relevant: \code{fit}, \code{completeDiagnostic} and \code{showOriginalModelOnly}. All other arguments are ignored.}
#'            \item{\code{type = "vif"}}{Variance Inflation Factors (check for multicollinearity) are plotted. As a rule of thumb, values below 5 are considered as good and indicate no multicollinearity, values between 5 and 10 may be tolerable. Values greater than 10 are not acceptable and indicate multicollinearity between model's predictors.}
#'            }
#'
#' @param fit fitted linear regression model (\code{\link{lm}}- or \code{plm}-object).
#' @param type type of plot. Use one of following:
#'          \describe{
#'            \item{\code{"lm"}}{(default) for forest-plot like plot of estimates. If the fitted model only contains one predictor, intercept and slope are plotted.}
#'            \item{\code{"std"}}{for forest-plot like plot of standardized beta values. If the fitted model only contains one predictor, intercept and slope are plotted.}
#'            \item{\code{"std2"}}{for forest-plot like plot of standardized beta values, however, standardization is done by dividing by two sd (see 'Details'). If the fitted model only contains one predictor, intercept and slope are plotted.}
#'            \item{\code{"pred"}}{to plot regression lines for each single predictor of the fitted model, against the response (linear relationship between each model term and response).}
#'            \item{\code{"resid"}}{to plot regression lines for each single predictor of the fitted model, against the residuals (linear relationship between each model term and residuals). May be used for model diagnostics (see \url{https://www.otexts.org/fpp/5/4}).}
#'            \item{\code{"resp"}}{to plot predicted values for the response. Use \code{showCI} argument to plot standard errors as well.}
#'            \item{\code{"eff"}}{to plot marginal effects of all terms in \code{fit}. Note that interaction terms are excluded from this plot; use \code{\link{sjp.int}} to plot effects of interaction terms.}
#'            \item{\code{"poly"}}{to plot predicted values (marginal effects) of polynomial terms in \code{fit}. Use \code{poly.term} to specify the polynomial term in the fitted model (see 'Examples').}
#'            \item{\code{"ma"}}{to check model assumptions. Note that only three arguments are relevant for this option \code{fit}, \code{completeDiagnostic} and \code{showOriginalModelOnly}. All other arguments are ignored.}
#'            \item{\code{"vif"}}{to plot Variance Inflation Factors.}
#'          }
#' @param sort.est logical, determines whether estimates should be sorted according to their values.
#'          If \code{group.estimates} is \emph{not} \code{NULL}, estimates are sorted
#'          according to their group assignment.
#' @param axisLabels.x name of predictor (independent variable) as string. Two things to consider:
#'          \itemize{
#'            \item Only used if fitted model has only one predictor and \code{type = "lm"}.
#'            \item If you use the \code{\link[sjmisc]{read_spss}} and \code{\link[sjmisc]{get_label}} functions, you receive a character vector with variable label strings. You can use it like this: \code{axisLabels.x = get_label(efc)['quol_5']}
#'          }
#' @param axisLabels.y labels or names of the predictor variables (independent vars). Must
#'          be a character vector of same length as independent variables. The labels
#'          may also be passed as list object; they will be coerced to character vector automatically.
#' @param showAxisLabels.y logical, whether labels of independent variables should be shown or not.
#' @param axisTitle.x title for the x-axis. Default is \code{"Estimates"}.
#' @param geom.colors user defined color palette for geoms. If \code{group.estimates}
#'          is \emph{not} specified, must either be vector with two color values or a specific
#'          color palette code (see 'Note' in \code{\link{sjp.grpfrq}}). Else, if
#'          \code{group.estimates} is specified, \code{geom.colors} must be a vector
#'          of same length as groups. See 'Examples'.
#' @param geom.size size resp. width of the geoms (bar width or point size, depending on \code{type} argument).
#' @param interceptLineType linetype of the intercept line (zero point). Default is \code{2} (dashed line).
#' @param interceptLineColor color of the intercept line. Default value is \code{"grey70"}.
#' @param group.estimates numeric or character vector, indicating a group identifier for
#'          each estimate. Dots and confidence intervals of estimates are coloured
#'          according to their group association. See 'Examples'.
#' @param remove.estimates character vector with coefficient names that indicate
#'          which estimates should be removed from the plot.
#'          \code{remove.estimates = "est_name"} would remove the estimate \emph{est_name}. Default
#'          is \code{NULL}, i.e. all estimates are printed.
#' @param coord.flip logical, if \code{TRUE} (default), predictors are plotted along the y-axis and estimate
#'          values are plotted on the x-axis.
#' @param showValueLabels logical, whether value labels should be plotted to each dot or not.
#' @param labelDigits amount of digits for rounding the estimates (see \code{showValueLabels}).
#'          Default is 2, i.e. estimates have 2 digits after decimal point.
#' @param showPValueLabels logical, whether the significance level of each coefficient
#'          should be appended to values or not.
#' @param showModelSummary logical, if \code{TRUE}, a summary of the regression model with
#'          Intercept, R-squared, F-Test and AIC-value is printed to the lower right corner
#'          of the plot.
#' @param showCI logical, if \code{TRUE} (default), a confidence region for the regression line
#'          will be plotted. Only applies if \code{type = "lm"} and fitted model has
#'          only one predictor, or if \code{type = "pred"} or \code{type = "resid"}.
#' @param showScatterPlot logical, if \code{TRUE} (default), a scatter plot of
#'          response and predictor values for each predictor of \code{fit} is plotted.
#'          Only applies if \code{type = "lm"} and fitted model has only one predictor,
#'          or if \code{type = "pred"} or \code{type = "resid"}.
#' @param showOriginalModelOnly logical, if \code{TRUE} (default), only model assumptions of
#'          \code{fit} are plotted. if \code{FALSE}, model assumptions of an updated
#'          model where outliers are automatically excluded are also plotted.
#'          Only applies if \code{type = "ma"}.
#' @param completeDiagnostic logical, if \code{TRUE}, additional tests are performed. Default is \code{FALSE}
#'          Only applies if \code{type = "ma"}.
#'
#' @inheritParams sjp.grpfrq
#' @inheritParams sjp.lmer
#' @inheritParams sjp.aov1
#'
#' @references Gelman A (2008) "Scaling regression inputs by dividing by two standard deviations." \emph{Statistics in Medicine 27: 2865–2873.} \url{http://www.stat.columbia.edu/~gelman/research/published/standardizing7.pdf}
#'
#' @return Depending on the \code{type}, in most cases (insisibily)
#'           returns the ggplot-object with the complete plot (\code{plot})
#'           as well as the data frame that was used for setting up the
#'           ggplot-object (\code{df}). For \code{type = "ma"}, an updated model
#'           with removed outliers is returned.
#'
#' @examples
#' # --------------------------------------------------
#' # plotting estimates of linear models as forest plot
#' # --------------------------------------------------
#' # fit linear model
#' fit <- lm(airquality$Ozone ~ airquality$Wind + airquality$Temp + airquality$Solar.R)
#'
#' # plot estimates with CI
#' sjp.lm(fit, gridBreaksAt = 2)
#'
#' # plot estimates with CI
#' # and with narrower tick marks
#' # (because "gridBreaksAt" was not specified)
#' sjp.lm(fit)
#'
#' # ---------------------------------------------------
#' # plotting regression line of linear model (done
#' # automatically if fitted model has only 1 predictor)
#' # ---------------------------------------------------
#' library(sjmisc)
#' data(efc)
#' # fit model
#' fit <- lm(neg_c_7 ~ quol_5, data=efc)
#' # plot regression line with label strings
#' sjp.lm(fit,
#'        axisLabels.x = "Quality of life",
#'        axisLabels.y = "Burden of care",
#'        showLoess = TRUE)
#'
#' # --------------------------------------------------
#' # plotting regression lines of each single predictor
#' # of a fitted model
#' # --------------------------------------------------
#' library(sjmisc)
#' data(efc)
#' # fit model
#' fit <- lm(tot_sc_e ~ c12hour + e17age + e42dep, data=efc)
#'
#' # reression line and scatter plot
#' sjp.lm(fit, type = "pred")
#'
#' # reression line w/o scatter plot
#' sjp.lm(fit,
#'        type = "pred",
#'        showScatterPlot = FALSE)
#'
#' # --------------------------
#' # plotting model assumptions
#' # --------------------------
#' sjp.lm(fit, type = "ma")
#'
#' \dontrun{
#' # --------------------------
#' # grouping estimates
#' # --------------------------
#' library(sjmisc)
#' data(efc)
#' fit <- lm(barthtot ~ c160age + e17age + c12hour + e16sex + c161sex + c172code,
#'           data = efc)
#'
#' # order estimates according to coefficient's order
#' sjp.lm(fit,
#'        group.estimates = c(1, 1, 2, 3, 3, 4),
#'        geom.colors = c("green", "red", "blue", "grey"),
#'        sort.est = FALSE)
#'
#' fit <- lm(barthtot ~ c160age + c12hour + e17age+ c161sex + c172code + e16sex,
#'           data = efc)
#'
#' # force order of estimates according to group assignment
#' sjp.lm(fit,
#'        group.estimates = c(1, 2, 1, 3, 4, 3),
#'        geom.colors = c("green", "red", "blue", "grey"),
#'        sort.est = TRUE)
#'
#'
#' # --------------------------
#' # plotting polynomial terms
#' # --------------------------
#' library(sjmisc)
#' data(efc)
#' # fit sample model
#' fit <- lm(tot_sc_e ~ c12hour + e17age + e42dep, data = efc)
#' # "e17age" does not seem to be linear correlated to response
#' # try to find appropiate polynomial. Grey line (loess smoothed)
#' # indicates best fit. Looks like x^3 has a good fit.
#' # (not checked for significance yet).
#' sjp.poly(fit, "e17age", 2:4, showScatterPlot = FALSE)
#' # fit new model
#' fit <- lm(tot_sc_e ~ c12hour + e42dep +
#'           e17age + I(e17age^2) + I(e17age^3),
#'           data = efc)
#' # plot marginal effects of polynomial term
#' sjp.lm(fit, type = "poly", poly.term = "e17age")
#'
#'
#' library(splines)
#' # fit new model with "splines"-package, "bs"
#' fit <- lm(tot_sc_e ~ c12hour + e42dep + bs(e17age, 3), data = efc)
#' # plot marginal effects of polynomial term, same call as above
#' sjp.lm(fit, type = "poly", poly.term = "e17age")}
#'
#' @import ggplot2
#' @import sjmisc
#' @importFrom car outlierTest crPlots durbinWatsonTest leveragePlots ncvTest spreadLevelPlot vif
#' @importFrom stats model.matrix confint coef
#' @importFrom dplyr slice
#' @export
sjp.lm <- function(fit,
                   type = "lm",
                   sort.est = TRUE,
                   title = NULL,
                   axisLabels.x = NULL,
                   axisLabels.y = NULL,
                   legendTitle = NULL,
                   showAxisLabels.y = TRUE,
                   axisTitle.x = "Estimates",
                   axisLimits = NULL,
                   geom.colors = "Set1",
                   geom.size = NULL,
                   interceptLineType = 2,
                   interceptLineColor = "grey70",
                   group.estimates = NULL,
                   remove.estimates = NULL,
                   breakTitleAt = 50,
                   breakLabelsAt = 25,
                   gridBreaksAt = NULL,
                   coord.flip = TRUE,
                   showValueLabels = TRUE,
                   labelDigits = 2,
                   showPValueLabels = TRUE,
                   showModelSummary = FALSE,
                   showCI = TRUE,
                   pointAlpha = 0.2,
                   showScatterPlot = TRUE,
                   showLoess = FALSE,
                   showLoessCI = FALSE,
                   show.legend = FALSE,
                   poly.term = NULL,
                   showOriginalModelOnly = TRUE,
                   completeDiagnostic = FALSE,
                   printPlot = TRUE) {
  # -----------------------------------------------------------
  # remember length of predictor variables
  # -----------------------------------------------------------
  predvars.length <- length(fit$coefficients)
  # -----------------------------------------------------------
  # check argument. No model-summary supported for plm-objects
  # -----------------------------------------------------------
  if (any(class(fit) == "plm") || any(class(fit) == "pggls")) {
    showModelSummary <- FALSE
    # -----------------------------------------------------------
    # check package availability if fit is plm-object
    # -----------------------------------------------------------
    if (!"package:plm" %in% search()) {
      stop("Package 'plm' needs to be loaded for this function to work... Use 'library(plm)' and call this function again.", call. = FALSE)
    }
  }
  # -----------------------------------------------------------
  # this function requires a fitted model with only one predictor,
  # so check whether only one predictor was used
  # -----------------------------------------------------------
  if ((type == "lm" || type == "resid") && predvars.length <= 2) {
    # reset default color setting, does not look that good.
    if (geom.colors == "Set1") geom.colors <- NULL
    return(invisible(sjp.lm1(fit,
                             title,
                             breakTitleAt,
                             axisLabels.x,
                             axisLabels.y,
                             breakLabelsAt,
                             geom.colors,
                             showCI,
                             pointAlpha,
                             showScatterPlot,
                             showLoess,
                             showLoessCI,
                             showModelSummary,
                             useResiduals = ifelse(type == "lm", FALSE, TRUE),
                             printPlot)))
  }
  if (type == "pred" || type == "resid") {
    # reset default color setting, does not look that good.
    if (geom.colors == "Set1") geom.colors <- NULL
    return(invisible(sjp.reglin(fit,
                                title,
                                breakTitleAt,
                                geom.colors,
                                showCI,
                                pointAlpha,
                                showScatterPlot,
                                showLoess,
                                showLoessCI,
                                useResiduals = ifelse(type == "pred", FALSE, TRUE),
                                printPlot)))
  }
  if (type == "resp") {
    return(invisible(sjp.lm.response.pred(fit,
                                          geom.colors,
                                          showCI,
                                          showLoess,
                                          showLoessCI,
                                          printPlot)))
  }
  if (type == "poly") {
    return(invisible(sjp.lm.poly(fit,
                                 poly.term,
                                 geom.colors,
                                 geom.size,
                                 axisTitle.x,
                                 showCI,
                                 printPlot)))
  }
  if (type == "eff") {
    return(invisible(sjp.lm.eff(fit,
                                title,
                                geom.size,
                                remove.estimates,
                                showCI,
                                printPlot)))
  }
  if (type == "ma") {
    return(invisible(sjp.lm.ma(fit,
                               showOriginalModelOnly,
                               completeDiagnostic)))
  }
  if (type == "vif") {
    return(invisible(sjp.vif(fit)))
  }
  # check size argument
  if (is.null(geom.size)) geom.size <- 3
  # --------------------------------------------------------
  # unlist labels
  # --------------------------------------------------------
  if (!is.null(axisLabels.y) && is.list(axisLabels.y)) axisLabels.y <- unlistlabels(axisLabels.y)
  # --------------------------------------------------------
  # auto-retrieve value labels
  # --------------------------------------------------------
  if (is.null(axisLabels.y)) axisLabels.y <- suppressWarnings(retrieveModelLabels(list(fit)))
  # check length of diagram title and split longer string at into new lines
  # every 50 chars
  if (!is.null(title)) title <- sjmisc::word_wrap(title, breakTitleAt)
  # check length of x-axis title and split longer string at into new lines
  # every 50 chars
  if (!is.null(axisTitle.x)) axisTitle.x <- sjmisc::word_wrap(axisTitle.x, breakTitleAt)
  # check length of x-axis-labels and split longer strings at into new lines
  # every 10 chars, so labels don't overlap
  if (!is.null(axisLabels.y)) axisLabels.y <- sjmisc::word_wrap(axisLabels.y, breakLabelsAt)
  # ----------------------------
  # create expression with model summarys. used
  # for plotting in the diagram later
  # ----------------------------
  if (showModelSummary) {
    modsum <- sju.modsum.lm(fit)
  } else {
    modsum <- NULL
  }
  # ----------------------------
  # print beta- and p-values in bar charts
  # ----------------------------
  # retrieve sigificance level of independent variables (p-values)
  if (any(class(fit) == "pggls")) {
    pv <- summary(fit)$CoefTable[-1, 4]
  } else {
    pv <- stats::coef(summary(fit))[-1, 4]
  }
  # -------------------------------------------------
  # for better readability, convert p-values to asterisks
  # with:
  # p < 0.001 = ***
  # p < 0.01 = **
  # p < 0.05 = *
  # -------------------------------------------------
  # retrieve betas, leave out intercept ([-1])
  # -------------------------------------------------
  if (type == "std" || type == "std2") {
    # retrieve standardized betas
    tmp <- suppressWarnings(sjmisc::std_beta(fit, include.ci = TRUE, type = type))
    # add "std." to title?
    if (axisTitle.x == "Estimates")
      axisTitle.x <- "Std. Estimates"
  } else {
    bv <- stats::coef(fit)[-1]
    if (1 == length(bv)) {
      # --------------------------------------------------------
      # if we have only one independent variable, cbind does not
      # work, since it duplicates the coefficients. so we simply
      # concatenate here
      # --------------------------------------------------------
      tmp <- data.frame(
        bv,
        stats::confint(fit, level = 0.95)[-1, 1],
        stats::confint(fit, level = 0.95)[-1, 2])
    } else {
      tmp <- data.frame(cbind(
        bv,
        stats::confint(fit, level = 0.95)[-1, ]))
    }
  }
  colnames(tmp) <- c("beta", "low.ci", "hi.ci")
  tmp$grp.est <- NA
  # -------------------------------------------------
  # group estimates?
  # -------------------------------------------------
  if (!is.null(group.estimates)) {
    # check for correct length
    if (length(group.estimates) != nrow(tmp)) {
      warning("Length of `group.estimates` does not equal number of model coefficients. Ignoring this argument.", call. = F)
      group.estimates = NULL
      show.legend <- FALSE
      legendTitle <- NULL
    } else {
      tmp$grp.est <- as.character(group.estimates)
    }
  } else {
    show.legend <- FALSE
    legendTitle <- NULL
  }
  # -------------------------------------------------
  # remove any estimates from the output?
  # -------------------------------------------------
  if (!is.null(remove.estimates)) {
    # get row indices of rows that should be removed
    remrows <- match(remove.estimates, row.names(tmp))
    # remember old rownames
    keepnames <- row.names(tmp)[-remrows]
    # remove rows
    tmp <- dplyr::slice(tmp, c(1:nrow(tmp))[-remrows])
    # set back rownames
    row.names(tmp) <- keepnames
    # remove labels?
    if (!is.null(axisLabels.y) && length(axisLabels.y) > nrow(tmp))
      axisLabels.y <- axisLabels.y[-remrows]
    # remove p-values
    pv <- pv[remrows]
  }
  # -------------------------------------------------
  # init data column for p-values
  # -------------------------------------------------
  ps <- sprintf("%.*f", labelDigits, tmp$beta)
  # if no values should be shown, clear
  # vector now
  if (!showValueLabels) ps <- rep("", length(ps))
  # --------------------------------------------------------
  # copy p-values into data column
  # --------------------------------------------------------
  if (showPValueLabels) {
    for (i in 1:length(pv)) {
      ps[i] <- sjmisc::trim(paste(ps[i], get_p_stars(pv[i])))
    }
  }
  # --------------------------------------------------------
  # append p-values and standardized beta coefficients
  # further more, we take the stand. beta as string, because in
  # case no values are drawn, we simply use an empty string.
  # finally, we need the p-values of the coefficients, because the value
  # labels may have different colours according to their significance level
  betas <- cbind(tmp[, !colnames(tmp) == "grp.est"], ps, pv, tmp$grp.est)
  # --------------------------------------------------------
  # check if user defined labels have been supplied
  # if not, use variable names from data frame
  # --------------------------------------------------------
  # auto-retrieving variable labels does not work when we
  # have factors with different levels, which appear as
  # "multiple predictors", but are only one variable
  # --------------------------------------------------------
  if (is.null(axisLabels.y) || length(axisLabels.y) < length(row.names(betas)))
    axisLabels.y <- row.names(betas)
  # --------------------------------------------------------
  # define sorting criteria. the values on the x-axis are being sorted
  # either by beta-values (sort="beta") or by standardized
  # beta values (sort = anything else)
  # --------------------------------------------------------
  # sort labels descending in order of (std.) beta values
  # sort rows of data frame descending in order of (std.) beta values
  # --------------------------------------------------------
  if (sort.est) {
    # order according to group assignment?
    if (!is.null(group.estimates)) {
      axisLabels.y <- rev(axisLabels.y[order(tmp$grp.est, tmp$beta)])
      betas <- betas[rev(order(tmp$grp.est, tmp$beta)), ]
    } else {
      axisLabels.y <- axisLabels.y[order(tmp$beta)]
      betas <- betas[order(tmp$beta), ]
    }
  } else {
    axisLabels.y <- rev(axisLabels.y)
    betas <- betas[nrow(betas):1, ]
  }
  betas <- cbind(c(1:nrow(betas)), betas)
  # give columns names
  colnames(betas) <- c("xv", "Beta", "lower", "upper", "p", "pv", "grp.est")
  betas$p <- as.character(betas$p)
  # --------------------------------------------------------
  # Calculate axis limits. The range is from lowest lower-CI
  # to highest upper-CI, or a user-defined range (if "axisLimits"
  # is not NULL)
  # --------------------------------------------------------
  if (is.null(axisLimits)) {
    upper_lim <- (ceiling(10 * max(betas$upper))) / 10
    lower_lim <- (floor(10 * min(betas$lower))) / 10
  } else {
    lower_lim <- axisLimits[1]
    upper_lim <- axisLimits[2]
  }
  # determine gridbreaks
  if (is.null(gridBreaksAt)) {
    ticks <- pretty(c(lower_lim, upper_lim))
  } else {
    ticks <- c(seq(lower_lim, upper_lim, by = gridBreaksAt))
  }
  if (!showAxisLabels.y) axisLabels.y <- c("")
  # --------------------------------------------------------
  # Start plot here! First check how to colour geoms
  # (whether grouped or not)
  # --------------------------------------------------------
  if (!is.null(group.estimates)) {
    betaplot <- ggplot(betas, aes(y = Beta, x = xv, colour = grp.est))
    pal.len <- length(unique(group.estimates))
    legend.labels <- unique(betas$grp.est)
  } else {
    betaplot <- ggplot(betas, aes(y = Beta, x = xv, colour = (Beta >= 0)))
    pal.len <- 2
    legend.labels <- NULL
  }
  # --------------------------------------------------------
  # Continue with plot
  # --------------------------------------------------------
  betaplot <- betaplot +
    # and error bar
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0) +
    # Print p-values. With vertical adjustment, so they don't overlap with the errorbars
    geom_text(aes(label = p, y = Beta),
              vjust = -0.8,
              show.legend = FALSE) +
    # print point
    geom_point(size = geom.size) +
    # Intercept-line
    geom_hline(yintercept = 0,
               linetype = interceptLineType,
               color = interceptLineColor) +
    # set y-scale-limits, breaks and tick labels
    scale_y_continuous(limits = c(lower_lim, upper_lim),
                       breaks = ticks,
                       labels = ticks) +
    # set value labels to x-axis
    scale_x_discrete(labels = axisLabels.y,
                     limits = c(1:nrow(betas))) +
    labs(title = title, x = NULL, y = axisTitle.x, colour = legendTitle)
  # --------------------------------------------------------
  # flip coordinates?
  # --------------------------------------------------------
  if (coord.flip) betaplot <- betaplot + coord_flip()
  # ------------------------------------------
  # check whether table summary should be printed
  # ------------------------------------------
  betaplot <- print.table.summary(betaplot, modsum)
  # ---------------------------------------------------------
  # set geom colors
  # ---------------------------------------------------------
  betaplot <- sj.setGeomColors(betaplot, geom.colors, pal.len, show.legend, legend.labels)
  # ---------------------------------------------------------
  # Check whether ggplot object should be returned or plotted
  # ---------------------------------------------------------
  if (printPlot) print(betaplot)
  # -------------------------------------
  # return results
  # -------------------------------------
  invisible(structure(class = "sjplm",
                      list(plot = betaplot,
                           df = betas)))
}


#' @importFrom stats predict
sjp.lm.response.pred <- function(fit,
                                 geom.colors,
                                 show.se,
                                 showLoess,
                                 showLoessCI,
                                 printPlot) {
  # -----------------------------------------------------------
  # check argument
  # -----------------------------------------------------------
  geom.colors <- col_check(geom.colors, showLoess)
  # -----------------------------------------------------------
  # set color defaults
  # -----------------------------------------------------------
  lineColor <- geom.colors[1]
  loessLineColor <- geom.colors[2]
  # ----------------------------
  # get predicted values for response
  # ----------------------------
  pp <- stats::predict(fit, type = "response")
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
         y = "Predicted values",
         title = "Predicted value for model-response") +
    stat_smooth(method = "lm",
                se = show.se,
                colour = lineColor)
  # ---------------------------------------------------------
  # Add Loess-Line
  # ---------------------------------------------------------
  if (showLoess) mp <- mp + stat_smooth(method = "loess",
                                        se = showLoessCI,
                                        colour = loessLineColor)
  # --------------------------
  # plot plots
  # --------------------------
  if (printPlot) print(mp)
  return(structure(class = "sjplm.pvresp",
                   list(df = mydf,
                        plot = mp,
                        mean.pp = mean(pp))))
}


sjp.reglin <- function(fit,
                       title = NULL,
                       breakTitleAt = 50,
                       geom.colors = NULL,
                       showCI = TRUE,
                       pointAlpha = 0.2,
                       showScatterPlot = TRUE,
                       showLoess = TRUE,
                       showLoessCI = FALSE,
                       useResiduals = FALSE,
                       printPlot = TRUE) {
  # -----------------------------------------------------------
  # check argument
  # -----------------------------------------------------------
  geom.colors <- col_check(geom.colors, showLoess)
  # -----------------------------------------------------------
  # set color defaults
  # -----------------------------------------------------------
  if (showLoess) {
    lineColor <- geom.colors[1]
    loessLineColor <- geom.colors[2]
    pointColor <- geom.colors[3]
  } else {
    lineColor <- geom.colors[1]
    pointColor <- geom.colors[2]
  }
  # -----------------------------------------------------------
  # retrieve amount of predictor variables and
  # retrieve column names of dataset so we can identify in which
  # column the data for each predictor is.
  # -----------------------------------------------------------
  if (any(class(fit) == "plm")) {
    # plm objects have different structure than (g)lm
    fit_x <- data.frame(cbind(as.vector(fit$model[, 1]), stats::model.matrix(fit)))
    depvar.label <- attr(attr(attr(fit$model, "terms"), "dataClasses"), "names")[1]
    # retrieve response vector
    resp <- as.vector(fit$model[, 1])
  } else if (any(class(fit) == "pggls")) {
    # plm objects have different structure than (g)lm
    fit_x <- data.frame(fit$model)
    depvar.label <- attr(attr(attr(fit$model, "terms"), "dataClasses"), "names")[1]
    # retrieve response vector
    resp <- as.vector(fit$model[, 1])
  } else if (any(class(fit) == "lmerMod") || any(class(fit) == "merModLmerTest")) {
    fit_x <- data.frame(stats::model.matrix(fit))
    # retrieve response vector
    resp <- lme4::getME(fit, "y")
    depvar.label <- attr(attr(attr(fit@frame, "terms"), "dataClasses"), "names")[1]
  } else {
    fit_x <- data.frame(stats::model.matrix(fit))
    depvar.label <- attr(attr(fit$terms, "dataClasses"), "names")[1]
    # retrieve response vector
    resp <- as.vector(fit$model[, 1])
  }
  predvars <- colnames(fit_x)[-1]
  cn <- predvars
  # remember length of predictor variables
  predvars.length <- length(predvars)
  # -----------------------------------------------------------
  # retrieve name of dependent variable
  # -----------------------------------------------------------
  response <- ifelse(useResiduals == TRUE, "residuals", depvar.label)
  # init return var
  plotlist <- list()
  dflist <- list()
  # -----------------------------------------------------------
  # check length of diagram title and split longer string at into new lines
  # every 50 chars
  # -----------------------------------------------------------
  if (!is.null(title)) title <- sjmisc::word_wrap(title, breakTitleAt)
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
      mydat <- data.frame(x = fit_x[, which(cn == xval) + 1], y = residuals(fit))
    } else {
      mydat <- data.frame(x = fit_x[, which(cn == xval) + 1], y = resp)
    }
    # -----------------------------------------------------------
    # plot regression line and confidence intervall
    # -----------------------------------------------------------
    reglinplot <- ggplot(mydat, aes(x = x, y = y)) +
      stat_smooth(method = "lm",
                  se = showCI,
                  colour = lineColor)
    # -----------------------------------------------------------
    # plot jittered values if requested
    # -----------------------------------------------------------
    if (showScatterPlot) reglinplot <- reglinplot + geom_jitter(alpha = pointAlpha,
                                                                colour = pointColor)
    # -----------------------------------------------------------
    # check whether additional loess-line should be plotted
    # -----------------------------------------------------------
    if (showLoess) {
      reglinplot <- reglinplot +
        stat_smooth(method = "loess",
                    se = showLoessCI,
                    colour = loessLineColor)
    }
    # -----------------------------------------------------------
    # set plot labs
    # -----------------------------------------------------------
    reglinplot <- reglinplot +
      labs(title = title,
           x = xval,
           y = response)
    # ---------------------------------------------------------
    # Check whether ggplot object should be returned or plotted
    # ---------------------------------------------------------
    # concatenate plot object
    plotlist[[length(plotlist) + 1]] <- reglinplot
    dflist[[length(dflist) + 1]] <- mydat
    # print plot
    if (printPlot) print(reglinplot)
  }
  # -------------------------------------
  # return results
  # -------------------------------------
  invisible(structure(class = "sjpreglin",
                      list(plot.list = plotlist,
                           df.list = dflist)))
}


col_check <- function(geom.colors, showLoess) {
  # define required length of color palette
  collen <- ifelse(showLoess == TRUE, 3, 2)
  if (is.null(geom.colors)) {
    if (collen == 2)
      geom.colors <- c("#1f78b4", "#404040")
    else
      geom.colors <- c("#1f78b4", "#e41a1c", "#404040")
  } else if (is.brewer.pal(geom.colors[1])) {
    geom.colors <- scales::brewer_pal(palette = geom.colors[1])(collen)
  } else if (geom.colors[1] == "gs") {
    geom.colors <- scales::grey_pal()(collen)
  } else {
    # do we have correct amount of colours?
    if (length(geom.colors) != collen) {
      # warn user abount wrong color palette
      warning(sprintf("Insufficient length of color palette provided. %i color values needed.", collen), call. = F)
      # set default
      if (collen == 2)
        geom.colors <- c("#1f78b4", "#404040")
      else
        geom.colors <- c("#1f78b4", "#e41a1c", "#404040")
    }
  }
  return(geom.colors)
}


col_check2 <- function(geom.colors, collen) {
  # --------------------------------------------
  # check color argument
  # --------------------------------------------
  # check for corrct color argument
  if (!is.null(geom.colors)) {
    # check for color brewer palette
    if (is.brewer.pal(geom.colors[1])) {
      geom.colors <- scales::brewer_pal(palette = geom.colors[1])(collen)
    } else if (geom.colors[1] == "gs") {
      geom.colors <- scales::grey_pal()(collen)
      # do we have correct amount of colours?
    } else if (length(geom.colors) != collen) {
      # warn user abount wrong color palette
      warning(sprintf("Insufficient length of color palette provided. %i color values needed.", collen), call. = F)
      # set default palette
      geom.colors <- scales::brewer_pal(palette = "Set1")(collen)
    }
  } else {
    geom.colors <- scales::brewer_pal(palette = "Set1")(collen)
  }
  return(geom.colors)
}


#' @importFrom stats fitted rstudent
sjp.lm.ma <- function(linreg, showOriginalModelOnly=TRUE, completeDiagnostic=FALSE) {
  # ------------------------
  # prepare plot list
  # ------------------------
  plot.list <- list()
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
  maxloops <- 5
  maxcnt <- maxloops
  # remember how many cases have been removed
  removedcases <- 0
  outlier <- c()
  loop <- TRUE
  # start loop
  while (loop == TRUE) {
    # get outliers of model
    # ol <- car::outlierTest(model)
    # vars <- as.numeric(names(ol$p))
    vars <- as.numeric(names(which(car::outlierTest(model, cutoff = Inf, n.max = Inf)$bonf.p < 1)))
    # do we have any outliers?
    if (sjmisc::is_empty(vars)) {
      loop <- FALSE
    } else {
      # retrieve variable numbers of outliers
      # update model by removing outliers
      dummymodel <- update(model, subset = -c(vars))
      # retrieve new r2
      dummyrs <- summary(dummymodel)$r.squared
      # decrease maximum loops
      maxcnt <- maxcnt - 1
      # check whether r2 of updated model is lower
      # than previous r2 or if we have already all loop-steps done,
      # stop loop
      if (dummyrs < rs || maxcnt < 1) {
        loop <- FALSE
      } else {
        # else copy new model, which is the better one (according to r2)
        model <- dummymodel
        # and get new r2
        rs <- dummyrs
        # count removed cases
        removedcases <- removedcases + length(vars)
        # add outliers to final return value
        outlier <- c(outlier, vars)
      }
    }
  }
  # ---------------------------------
  # print steps from original to updated model
  # ---------------------------------
  message(sprintf("Removed %i cases during %i step(s).\nR^2 / adj. R^2 of original model: %f / %f\nR^2 / adj. R^2 of updated model:  %f / %f\nAIC of original model: %f \nAIC of updated model:  %f\n",
              removedcases,
              maxloops - (maxcnt + 1),
              summary(linreg)$r.squared,
              summary(linreg)$adj.r.squared,
              summary(model)$r.squared,
              summary(model)$adj.r.squared,
              AIC(linreg),
              AIC(model)))
  modelOptmized <- ifelse(removedcases > 0, TRUE, FALSE)
  if (showOriginalModelOnly) modelOptmized <- FALSE
  # ---------------------------------
  # show VIF-Values
  # ---------------------------------
  sjp.setTheme(theme = "539w")
  sjp.vif(linreg)
  if (modelOptmized) sjp.vif(model)
  # ---------------------------------
  # Print non-normality of residuals and outliers both of original and updated model
  # dots should be plotted along the line, this the dots should follow a linear direction
  # ---------------------------------
  ggqqp <- function(fit, title.suffix = " (original model)") {
    mydf <- data.frame(x = sort(stats::fitted(fit)),
                       y = sort(stats::rstudent(fit)))
    return(ggplot(mydf, aes(x = x, y = y)) +
             geom_point() +
             stat_smooth(method = "lm", se = FALSE) +
             labs(title = sprintf("Non-normality of residuals and outliers%s\n(Dots should be plotted along the line)", title.suffix),
                  y = "Studentized Residuals",
                  x = "Theoretical quantiles"))
  }
  sjp.setTheme(theme = "scatterw")
  # qq-plot of studentized residuals for base model
  p1 <- ggqqp(linreg)
  # save plot
  plot.list[[length(plot.list) + 1]] <- p1
  # print plot
  print(p1)
  # qq-plot of studentized residuals for updated model
  if (modelOptmized) {
    p1 <- ggqqp(model, " (updated model)")
    # save plot
    plot.list[[length(plot.list) + 1]] <- p1
    # print plot
    print(p1)
  }
  # ---------------------------------
  # Print non-normality of residuals both of original and updated model
  # Distribution should look like normal curve
  # ---------------------------------
  gghist <- function(fit, title.suffix = " (original model)") {
    return(ggplot(fit, aes(x = .resid)) +
             geom_histogram(aes(y = ..density..),
                            binwidth = 0.2,
                            fill = "grey60",
                            colour = "grey30") +
             geom_density(aes(y = ..density..),
                          fill = "#4080cc",
                          alpha = 0.2) +
             stat_function(fun = dnorm,
                           args = list(mean = mean(unname(residuals(fit)), na.rm = TRUE),
                                       sd = sd(unname(residuals(fit)), na.rm = TRUE)),
                           colour = "FireBrick",
                           size = 0.8) +
             labs(x = "Residuals",
                  y = "Density",
                  title = sprintf("Non-normality of residuals%s\n(Distribution should look like normal curve)", title.suffix)))
  }
  sjp.setTheme(theme = "539w")
  # residuals histrogram for base model
  p1 <- gghist(linreg)
  # save plot
  plot.list[[length(plot.list) + 1]] <- p1
  # print plot
  print(p1)
  # residuals histrogram for updated model
  if (modelOptmized) {
    p1 <- gghist(model, " (updated model)")
    # save plot
    plot.list[[length(plot.list) + 1]] <- p1
    # print plot
    print(p1)
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
  ggsced <- function(fit, title.suffix = ", original model") {
    return(ggplot(fit, aes(x = .fitted, y = .resid)) +
             geom_hline(yintercept = 0, alpha = 0.7) +
             geom_point() +
             geom_smooth(method = "loess", se = FALSE) +
             labs(x = "Fitted values",
                  y = "Residuals",
                  title = sprintf("Homoscedasticity (homogeneity of variance,\nrandomly distributed residuals%s)\n(Amount and distance of points scattered above/below line is equal)", title.suffix)))
  }
  sjp.setTheme(theme = "scatterw")
  # homoscedascity for base model
  p1 <- ggsced(linreg)
  # save plot
  plot.list[[length(plot.list) + 1]] <- p1
  # print plot
  print(p1)
  # homoscedascity for base model
  if (modelOptmized) {
    p1 <- ggsced(model, ", updated model")
    # save plot
    plot.list[[length(plot.list) + 1]] <- p1
    # print plot
    print(p1)
  }
  # ---------------------------------
  # summarize old and new model
  # ---------------------------------
  sjp.setTheme(theme = "forestw")
  p1 <- sjp.lm(linreg, title = "Original model", printPlot = FALSE)$plot
  # save plot
  plot.list[[length(plot.list) + 1]] <- p1
  # print plot
  print(p1)
  if (modelOptmized) {
    p1 <- sjp.lm(model, title = "Updated model", printPlot = FALSE)$plot
    # save plot
    plot.list[[length(plot.list) + 1]] <- p1
    # print plot
    print(p1)
  }
  if (completeDiagnostic) {
    # ---------------------------------
    # Plot residuals against predictors
    # ---------------------------------
    sjp.setTheme(theme = "scatterw")
    p1 <- sjp.reglin(linreg,
                     title = "Relationship of residuals against predictors (original model) (if scatterplots show a pattern, relationship may be nonlinear and model needs to be modified accordingly",
                     breakTitleAt = 60,
                     useResiduals = T)$plot.list
    # save plot
    plot.list <- c(plot.list, p1)
    if (modelOptmized) {
      p1 <- sjp.reglin(model,
                       title = "Relationship of residuals against predictors (updated model) (if scatterplots show a pattern, relationship may be nonlinear and model needs to be modified accordingly",
                       breakTitleAt = 60,
                       useResiduals = T)
      # save plot
      plot.list <- c(plot.list, p1)
      # print plot
      print(p1)
    }
    # ---------------------------------
    # Non-linearity
    # ---------------------------------
    plot(car::crPlots(linreg))
    # ---------------------------------
    # non-independence of residuals
    # ---------------------------------
    print(car::durbinWatsonTest(linreg))
    # ---------------------------------
    # Print leverage plots
    # ---------------------------------
    plot(car::leveragePlots(linreg))
    # ---------------------------------
    # Non-constant residuals
    # ---------------------------------
    print(car::ncvTest(linreg))
    print(lmtest::bptest(linreg))
    print(car::spreadLevelPlot(linreg))
  }
  # return updated model
  invisible(structure(list(class = "sjp.lm.ma",
                           model = model,
                           plot.list = plot.list,
                           outlier = outlier)))
}


sjp.lm1 <- function(fit,
                   title=NULL,
                   breakTitleAt=50,
                   axisLabel.x=NULL,
                   axisLabel.y=NULL,
                   breakLabelsAt=20,
                   geom.colors = NULL,
                   showCI=TRUE,
                   pointAlpha=0.2,
                   showScatterPlot=TRUE,
                   showLoess=FALSE,
                   showLoessCI=FALSE,
                   showModelSummary=TRUE,
                   useResiduals=FALSE,
                   printPlot=TRUE) {
  # -----------------------------------------------------------
  # check argument
  # -----------------------------------------------------------
  geom.colors <- col_check(geom.colors, showLoess)
  # -----------------------------------------------------------
  # set color defaults
  # -----------------------------------------------------------
  if (showLoess) {
    lineColor <- geom.colors[1]
    loessLineColor <- geom.colors[2]
    pointColor <- geom.colors[3]
  } else {
    lineColor <- geom.colors[1]
    pointColor <- geom.colors[2]
  }
  # -----------------------------------------------------------
  # check length of diagram title and split longer string at into new lines
  # every 50 chars
  # -----------------------------------------------------------
  if (!is.null(title)) title <- sjmisc::word_wrap(title, breakTitleAt)
  # -----------------------------------------------------------
  # remember length of predictor variables
  # -----------------------------------------------------------
  predvars.length <- length(fit$coefficients)
  # -----------------------------------------------------------
  # this function requires a fitted model with only one predictor,
  # so check whether only one predictor was used
  # -----------------------------------------------------------
  if (predvars.length > 2) {
    stop("Only one predictor is allowed in fitted model. Formula y=b*x is plotted.", call. = FALSE)
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
    mydat <- as.data.frame(cbind(x = as.vector(fit$model[, 2]),
                                 y = fit$residuals))
  } else {
    # use as.vector, to make function work with plm-objects
    mydat <- as.data.frame(cbind(x = as.vector(fit$model[, 2]),
                                 y = as.vector(fit$model[, 1])))
  }
  # ----------------------------
  # create expression with model summarys. used
  # for plotting in the diagram later
  # ----------------------------
  if (showModelSummary) {
    modsum <- sju.modsum.lm(fit)
  } else {
    modsum <- NULL
  }
  # ----------------------------
  # prepare axis labels
  # ----------------------------
  if (is.null(axisLabel.x)) axisLabel.x <- xval
  if (is.null(axisLabel.y)) axisLabel.y <- response
  # check length of axis-labels and split longer strings at into new lines
  # every 10 chars, so labels don't overlap
  axisLabel.x <- sjmisc::word_wrap(axisLabel.x, breakLabelsAt)
  axisLabel.y <- sjmisc::word_wrap(axisLabel.y, breakLabelsAt)
  # -----------------------------------------------------------
  # plot regression line and confidence intervall
  # -----------------------------------------------------------
  reglinplot <- ggplot(mydat,
                       aes(x = x, y = y)) +
    stat_smooth(method = "lm",
                se = showCI,
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
  invisible(structure(class = "sjplm1",
                      list(plot = reglinplot,
                           df = mydat)))
}


sjp.lm.poly <- function(fit,
                        poly.term,
                        geom.colors,
                        geom.size,
                        axisTitle.x,
                        showCI,
                        printPlot) {
  # check size argument
  if (is.null(geom.size)) geom.size <- .8
  # ------------------------
  # check if suggested package is available
  # ------------------------
  if (!requireNamespace("effects", quietly = TRUE)) {
    stop("Package 'effects' needed for this function to work. Please install it.", call. = FALSE)
  }
  # -------------------------------------
  # retrieve model matrix
  # -------------------------------------
  mm <- stats::model.matrix(fit)
  # get model data column names
  cn <- colnames(mm)
  xl <- NULL
  # -------------------------------------
  # argument check: poly.term required and
  # polynomial must be found in model
  # -------------------------------------
  if (!is.null(poly.term)) {
    # check for simple poly term, using I(x^2) + I(x^3) etc.
    poly.found <- any(colnames(mm) == poly.term)
    # found poly? If yes, get range
    if (poly.found) {
      xl <- list(x = sort(unique(stats::na.omit(mm[, poly.term]))))
    } else {
      # not found? than check for poly term, using poly(x, degree = 3)
      if (!poly.found) {
        # find term names
        pt <- unique(sub("^poly\\(([^,)]*).*", "\\1", cn))
        # found poly-term?
        poly.found <- any(pt == poly.term)
      }
      # not found? last try, looking for splines
      if (!poly.found) {
        # find term names
        pt <- unique(sub("^bs\\(([^,)]*).*", "\\1", cn))
        # found poly-term?
        poly.found <- any(pt == poly.term)
      }
    }
    # no polynomial term found...
    if (!poly.found) {
      stop("'poly.term' not given, or not found in model. Please check name of polynomial term.", call. = FALSE)
      # xl already defined? If not, do it now!
    } else if (is.null(xl)) {
      # "dummy computation" of effects to get range of poly term
      eff <- suppressMessages(effects::effect(poly.term, fit, KR = FALSE))
      # get range
      pora <- range(eff$x[[poly.term]])
      # create levels paramater for effect method
      xl <- list(x = seq(pora[1], pora[2]))
    }
  } else {
    stop("'poly.term' must be specified.", call. = FALSE)
  }
  # ------------------------
  # check for color brewer palette
  # ------------------------
  if (is.brewer.pal(geom.colors[1])) {
    geom.colors <- scales::brewer_pal(palette = geom.colors[1])(2)
  } else if (geom.colors[1] == "gs") {
    geom.colors <- scales::grey_pal()(2)
  }
  # --------------------------------------------
  # retrieve labels
  # --------------------------------------------
  if (is.null(axisTitle.x) || axisTitle.x == "Estimates") axisTitle.x <- "Polynomial term"
  # ------------------------
  # compute marginal effects of polynomial
  # ------------------------
  names(xl) <- poly.term
  eff <- effects::effect(poly.term, fit, xlevels = xl, KR = FALSE)
  # ------------------------
  # build data frame, with raw values
  # from polynomial term, predicted response
  # and lower/upper ci
  # ------------------------
  mydat <- data.frame(x = eff$x[[poly.term]],
                      y = eff$fit,
                      lower = eff$lower,
                      upper = eff$upper)
  # base plot
  polyplot <- ggplot(mydat, aes(x = x, y = y))
  # show confidence region?
  if (showCI) polyplot <- polyplot + geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .15)
  # plot predicted effect of polynomial term
  polyplot <- polyplot +
    geom_line(colour = geom.colors[1], size = geom.size) +
    labs(x = axisTitle.x, y = "Response")
  # print plot
  if (printPlot) print(polyplot)
  # return result
  invisible(structure(class = "sjplmpoly",
                      list(plot = polyplot,
                           df = mydat)))
}


sjp.lm.eff <- function(fit,
                       title,
                       geom.size,
                       remove.estimates,
                       showCI,
                       printPlot) {
  # ------------------------
  # check if suggested package is available
  # ------------------------
  if (!requireNamespace("effects", quietly = TRUE)) {
    stop("Package 'effects' needed for this function to work. Please install it.", call. = FALSE)
  }
  if ((any(class(fit) == "lmerMod" || any(class(fit) == "merModLmerTest"))) && !requireNamespace("lme4", quietly = TRUE)) {
    stop("Package 'lme4' needed for this function to work. Please install it.", call. = FALSE)
  }
  # ------------------------
  # Retrieve response for automatic title
  # ------------------------
  if (any(class(fit) == "lmerMod") || any(class(fit) == "merModLmerTest")) {
    # retrieve response vector
    resp <- lme4::getME(fit, "y")
    resp.col <- colnames(fit@frame)[1]
  } else if (any(class(fit) == "lm")) {
    # retrieve response vector
    resp <- fit$model[[1]]
    resp.col <- colnames(fit$model)[1]
  }
  # --------------------------------------------
  # retrieve labels
  # --------------------------------------------
  axisTitle.y <- sjmisc::get_label(resp,
                                   def.value = get_var_name(deparse(substitute(resp))))
  # no labels found? set default then
  if (is.null(axisTitle.y)) axisTitle.y <- resp.col
  # which title?
  if (is.null(title)) title <- "Marginal effects of model predictors"
  # ------------------------
  # retrieve model matrix and all terms,
  # excluding intercept
  # ------------------------
  mm <- stats::model.matrix(fit)
  all.terms <- colnames(stats::model.matrix(fit))[-1]
  # ------------------------
  # remove setimates?
  # ------------------------
  if (!is.null(remove.estimates)) {
    remcols <- match(remove.estimates, all.terms)
    # remember old rownames
    if (!sjmisc::is_empty(remcols))
      all.terms <- all.terms[-remcols]
  }
  # ------------------------
  # prepare getting unique values of predictors,
  # which are passed to the allEffects-function
  # ------------------------
  xl <- list()
  for (t in all.terms) {
    # get unique values
    dummy <- list(x = sort(unique(stats::na.omit(mm[, t]))))
    # name list, needed for effect-function
    names(dummy) <- t
    # create list for "xlevels" argument of allEffects fucntion
    xl <- c(xl, dummy)
  }
  # ------------------------
  # compute marginal effects for each model term
  # ------------------------
  eff <- effects::allEffects(fit, xlevels = xl, KR = FALSE)
  # init final df
  mydat <- data.frame()
  # interaction term found?
  int.found <- FALSE
  # iterate all effects
  for (i in 1:length(eff)) {
    # get term, for which effects were calculated
    t <- eff[[i]]$term
    # check if we have interaction term
    # these are ignored in this case.
    if (length(grep(":", t, fixed = T)) == 0 && length(grep("*", t, fixed = T)) == 0) {
      # ------------------------
      # build data frame, with raw values
      # from polynomial term, predicted response
      # and lower/upper ci
      # ------------------------
      tmp <- data.frame(x = eff[[i]]$x[[t]],
                        y = eff[[i]]$fit,
                        lower = eff[[i]]$lower,
                        upper = eff[[i]]$upper,
                        grp = t)
      # make sure x is numeric
      tmp$x <- sjmisc::to_value(tmp$x, keep.labels = F)
      # do we already have data?
      if (nrow(mydat) > 0)
        mydat <- rbind(mydat, tmp)
      else
        # else init data frame
        mydat <- tmp
    } else {
      int.found <- TRUE
    }
  }
  # continuous numbering of row names
  rownames(mydat) <- c(1:nrow(mydat))
  # ------------------------
  # tell user that interaction terms are ignored
  # ------------------------
  if (int.found) {
    message("Interaction terms in model have been ignored. Call 'sjp.int' to plot effects of interaction terms.")
  }
  # ------------------------
  # how many different groups?
  # ------------------------
  grp.cnt <- length(unique(mydat$grp))
  # check size argument
  if (is.null(geom.size)) geom.size <- .8
  # ------------------------
  # create plot
  # ------------------------
  eff.plot <- ggplot(mydat, aes(x = x, y = y))
  # show confidence region?
  if (showCI) eff.plot <- eff.plot + geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .15)
  eff.plot <- eff.plot +
    geom_line(size = geom.size) +
    facet_wrap(~grp, ncol = round(sqrt(grp.cnt)), scales = "free_x") +
    labs(x = NULL, y = axisTitle.y, title = title)
  # ------------------------
  # print plot?
  # ------------------------
  if (printPlot) print(eff.plot)
  # return result
  invisible(structure(class = "sjplmeff",
                      list(plot = eff.plot,
                           df = mydat)))
}
