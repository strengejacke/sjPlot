# bind global variables
utils::globalVariables(c("fit", "vars", "Beta", "xv", "lower", "upper", "stdbeta", "p", "x", "ydiff", "y", "grp", ".stdresid", ".resid", ".fitted", "V1", "V2", "grp.est"))


#' @title Plot estimates, predictions or effects of linear models
#' @name sjp.lm
#'
#' @seealso \href{http://www.strengejacke.de/sjPlot/sjp.lm}{sjPlot manual: sjp.lm} for
#'            more details and examples of this function; use \code{\link{sjp.poly}}
#'            to see which polynomial degree fits best for possible polynomial terms.
#'
#' @description Depending on the \code{type}, this function plots coefficients (estimates)
#'                of linear regressions (including panel models fitted with the \code{plm}-function
#'                from the \pkg{plm}-package and generalized least squares models fitted with
#'                the \code{gls}-function from the \pkg{nlme}-package) with confidence 
#'                intervals as dot plot (forest plot),
#'                model assumptions for linear models or slopes and scatter plots for each single
#'                coefficient. See \code{type} for details.
#'
#' @details \describe{
#'            \item{\code{type = "lm"}}{if fitted model only has one predictor, no 
#'                  forest plot is shown. Instead, a regression line with confidence 
#'                  interval (in blue) is plotted by default, and a loess-smoothed 
#'                  line without confidence interval (in red) can be added if argument 
#'                  \code{show.loess = TRUE}.}
#'            \item{\code{type = "std2"}}{plots standardized beta values, however, 
#'                  standardization follows Gelman's (2008) suggestion, rescaling the 
#'                  estimates by dividing them by two standard deviations instead of 
#'                  just one. Resulting coefficients are then directly comparable for 
#'                  untransformed binary predictors. This standardization uses the 
#'                  \code{\link[arm]{standardize}}-function.}
#'            \item{\code{type = "slope"}}{regression lines (slopes) with confidence 
#'                  intervals for each single predictor of the fitted model are plotted, 
#'                  i.e. all predictors of the fitted model are extracted and for each of
#'                  them, the linear relationship is plotted against the response variable.
#'                  Other predictors are omitted, so this plot type is intended to check
#'                  the linear relationship between a predictor and the response.}
#'            \item{\code{type = "resid"}}{is similar to the \code{type = "slope"} option, 
#'                  however, each predictor is plotted against the residuals
#'                  (instead of response).}
#'            \item{\code{type = "pred"}}{plots predicted values of the response, related
#'                  to specific model predictors. This plot type calls 
#'                  \code{\link[stats]{predict}(fit, newdata = model.frame, type = "response")} 
#'                  and requires the \code{vars} argument to select specific terms that should be 
#'                  used for the x-axis and - optional - as grouping factor. Hence, 
#'                  \code{vars} must be a character vector with the names of one or 
#'                  two model predictors. See 'Examples'.}
#'            \item{\code{type = "eff"}}{computes the marginal effects for all predictors, 
#'                  using the \code{\link[effects]{allEffects}} function. I.e. for each 
#'                  predictor, the predicted values towards the response are plotted, with 
#'                  all remaining co-variates set to the mean. Due to possible different 
#'                  scales of predictors, a faceted plot is printed (instead of plotting 
#'                  all lines in one plot).}
#'            \item{\code{type = "poly"}}{plots the marginal effects of polynomial terms 
#'                  in \code{fit}, using the \code{\link[effects]{effect}} function, but 
#'                  only for a selected polynomial term, which is specified with \code{poly.term}.
#'                  This function helps undertanding the effect of polynomial terms by 
#'                  plotting the curvilinear relationships of response and quadratic, cubic etc. 
#'                  terms. This function accepts following argument.}
#'            \item{\code{type = "ma"}}{checks model assumptions. Please note that only 
#'                  three arguments are relevant: \code{fit}, \code{completeDiagnostic} 
#'                  and \code{showOriginalModelOnly}. All other arguments are ignored.}
#'            \item{\code{type = "vif"}}{Variance Inflation Factors (check for multicollinearity) 
#'                  are plotted. As a rule of thumb, values below 5 are considered as good 
#'                  and indicate no multicollinearity, values between 5 and 10 may be tolerable.
#'                  Values greater than 10 are not acceptable and indicate multicollinearity
#'                  between model's predictors.}
#'            }
#'
#' @param fit fitted linear regression model (of class \code{\link{lm}}, \code{\link[nlme]{gls}} or \code{plm}).
#' @param type type of plot. Use one of following:
#'          \describe{
#'            \item{\code{"lm"}}{(default) for forest-plot like plot of estimates. If the fitted model only contains one predictor, intercept and slope are plotted.}
#'            \item{\code{"std"}}{for forest-plot like plot of standardized beta values. If the fitted model only contains one predictor, intercept and slope are plotted.}
#'            \item{\code{"std2"}}{for forest-plot like plot of standardized beta values, however, standardization is done by dividing by two sd (see 'Details'). If the fitted model only contains one predictor, intercept and slope are plotted.}
#'            \item{\code{"slope"}}{to plot regression lines for each single predictor of the fitted model, against the response (linear relationship between each model term and response).}
#'            \item{\code{"resid"}}{to plot regression lines for each single predictor of the fitted model, against the residuals (linear relationship between each model term and residuals). May be used for model diagnostics (see \url{https://www.otexts.org/fpp/5/4}).}
#'            \item{\code{"pred"}}{to plot predicted values for the response, related to specific predictors. See 'Details'.}
#'            \item{\code{"eff"}}{to plot marginal effects of all terms in \code{fit}. Note that interaction terms are excluded from this plot; use \code{\link{sjp.int}} to plot effects of interaction terms.}
#'            \item{\code{"poly"}}{to plot predicted values (marginal effects) of polynomial terms in \code{fit}. Use \code{poly.term} to specify the polynomial term in the fitted model (see 'Examples').}
#'            \item{\code{"ma"}}{to check model assumptions. Note that only three arguments are relevant for this option \code{fit}, \code{completeDiagnostic} and \code{showOriginalModelOnly}. All other arguments are ignored.}
#'            \item{\code{"vif"}}{to plot Variance Inflation Factors.}
#'          }
#' @param sort.est logical, determines whether estimates should be sorted according to their values.
#'          If \code{group.estimates} is \emph{not} \code{NULL}, estimates are sorted
#'          according to their group assignment.
#' @param resp.label name of dependent variable, as string. Only 
#'          used if fitted model has only one predictor and \code{type = "lm"}.
#' @param axisTitle.x title for the x-axis. Default is \code{"Estimates"}.
#' @param geom.colors user defined color palette for geoms. If \code{group.estimates}
#'          is \emph{not} specified, must either be vector with two color values or a specific
#'          color palette code (see 'Note' in \code{\link{sjp.grpfrq}}). Else, if
#'          \code{group.estimates} is specified, \code{geom.colors} must be a vector
#'          of same length as groups. See 'Examples'.
#' @param geom.size size resp. width of the geoms (bar width, point size or line thickness, depending on \code{type} argument).
#' @param group.estimates numeric or character vector, indicating a group identifier for
#'          each estimate. Dots and confidence intervals of estimates are coloured
#'          according to their group association. See 'Examples'.
#' @param remove.estimates character vector with coefficient names that indicate
#'          which estimates should be removed from the plot.
#'          \code{remove.estimates = "est_name"} would remove the estimate \emph{est_name}. Default
#'          is \code{NULL}, i.e. all estimates are printed.
#' @param coord.flip logical, if \code{TRUE} (default), predictors are plotted along the y-axis and estimate
#'          values are plotted on the x-axis.
#' @param labelDigits amount of digits for rounding the estimates (see \code{show.values}).
#'          Default is 2, i.e. estimates have 2 digits after decimal point.
#' @param showPValueLabels logical, whether the significance level of each coefficient
#'          should be appended to values or not.
#' @param showModelSummary logical, if \code{TRUE}, a summary of the regression model with
#'          Intercept, R-squared, F-Test and AIC-value is printed to the lower right corner
#'          of the plot.
#' @param show.ci logical, if \code{TRUE}, depending on \code{type}, a condifence
#'          region is added to the plot.
#' @param showScatterPlot logical, if \code{TRUE} (default), a scatter plot of
#'          response and predictor values for each predictor of \code{fit} is plotted.
#'          Only applies if \code{type = "lm"} and fitted model has only one predictor,
#'          or if \code{type = "slope"} or \code{type = "resid"}.
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
#' @inheritParams sjp.glmer
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
#'        resp.label = "Burden of care",
#'        var.labels = "Quality of life",
#'        show.loess = TRUE)
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
#' sjp.lm(fit, type = "slope")
#'
#' # reression line w/o scatter plot
#' sjp.lm(fit,
#'        type = "slope",
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
#' # predicted values for response
#' # --------------------------
#' library(sjmisc)
#' data(efc)
#' efc$education <- to_label(to_factor(efc$c172code))
#' fit <- lm(barthtot ~ c160age + c12hour + e17age+ education,
#'           data = efc)
#'
#' sjp.lm(fit, type = "pred", vars = "c160age")
#'
#' # with loess
#' sjp.lm(fit, type = "pred", vars = "e17age", show.loess = TRUE)
#'
#' # grouped
#' sjp.lm(fit, type = "pred", vars = c("c12hour", "education"))
#' 
#' # grouped, non-facet
#' sjp.lm(fit, type = "pred", vars = c("c12hour", "education"),
#'        facet.grid = FALSE)
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
#' @importFrom stats model.matrix confint coef residuals sd
#' @importFrom dplyr slice
#' @importFrom nlme getData getResponse getCovariateFormula
#' @export
sjp.lm <- function(fit,
                   type = "lm",
                   sort.est = TRUE,
                   title = NULL,
                   resp.label = NULL,
                   var.labels = NULL,
                   legendTitle = NULL,
                   axisTitle.x = "Estimates",
                   axisLimits = NULL,
                   geom.colors = "Set1",
                   geom.size = NULL,
                   vline.type = 2,
                   vline.color = "grey70",
                   group.estimates = NULL,
                   remove.estimates = NULL,
                   vars = NULL,
                   breakTitleAt = 50,
                   breakLabelsAt = 25,
                   gridBreaksAt = NULL,
                   coord.flip = TRUE,
                   facet.grid = TRUE,
                   show.values = TRUE,
                   labelDigits = 2,
                   showPValueLabels = TRUE,
                   showModelSummary = FALSE,
                   show.ci = TRUE,
                   pointAlpha = 0.2,
                   showScatterPlot = TRUE,
                   show.loess = FALSE,
                   show.loess.ci = FALSE,
                   show.legend = FALSE,
                   y.offset = .15,
                   poly.term = NULL,
                   showOriginalModelOnly = TRUE,
                   completeDiagnostic = FALSE,
                   printPlot = TRUE) {
  # -----------------------------------------------------------
  # remember length of predictor variables
  # -----------------------------------------------------------
  predvars.length <- length(stats::coef(fit))
  # -----------------------------------------------------------
  # check argument. No model-summary supported for plm-objects
  # -----------------------------------------------------------
  if (any(class(fit) == "plm") || any(class(fit) == "pggls")) {
    showModelSummary <- FALSE
    # -----------------------------------------------------------
    # check package availability if fit is plm-object
    # -----------------------------------------------------------
    if (!"package:plm" %in% search()) {
      stop("Package `plm` needs to be loaded for this function to work... Use `library(plm)` and call this function again.", call. = FALSE)
    }
  }
  if (any(class(fit) == "gls")) {
    if (!requireNamespace("nlme", quietly = TRUE))
      stop("Package `nlme` needed for this function to work. Please install it.", call. = FALSE)
    showModelSummary <- FALSE
  }
  # -----------------------------------------------------------
  # set default title
  # -----------------------------------------------------------
  if (is.null(title) && type != "eff") title <- get_model_response_label(fit)
  # -----------------------------------------------------------
  # this function requires a fitted model with only one predictor,
  # so check whether only one predictor was used
  # -----------------------------------------------------------
  if ((type == "lm" || type == "resid") && predvars.length <= 2) {
    # reset default color setting, does not look that good.
    if (geom.colors == "Set1") geom.colors <- NULL
    return(invisible(sjp.lm1(fit, title, breakTitleAt, var.labels, resp.label,
                             breakLabelsAt, geom.colors, show.ci, pointAlpha,
                             showScatterPlot, show.loess, show.loess.ci, showModelSummary,
                             useResiduals = ifelse(type == "lm", FALSE, TRUE),
                             printPlot)))
  }
  if (type == "slope" || type == "resid") {
    # reset default color setting, does not look that good.
    if (geom.colors == "Set1") geom.colors <- NULL
    return(invisible(sjp.reglin(fit, title, breakTitleAt, geom.colors, show.ci,
                                pointAlpha, showScatterPlot, show.loess, show.loess.ci,
                                useResiduals = ifelse(type == "slope", FALSE, TRUE),
                                remove.estimates, vars, printPlot)))
  }
  if (type == "pred") {
    return(invisible(sjp.glm.predy(fit, vars, t.title = title, l.title = legendTitle,
                                   show.ci, geom.size, axisLimits.y = axisLimits,
                                   facet.grid, type = "fe", show.loess, printPlot)))
  }
  if (type == "poly") {
    return(invisible(sjp.lm.poly(fit, poly.term, geom.colors, geom.size, axisTitle.x,
                                 NULL, show.ci, printPlot)))
  }
  if (type == "eff") {
    return(invisible(sjp.glm.eff(fit, title, geom.size, remove.estimates, vars,
                                 show.ci, axisLimits.y = NULL, facet.grid,
                                 fun = "lm", printPlot)))
  }
  if (type == "ma") {
    return(invisible(sjp.lm.ma(fit, showOriginalModelOnly, completeDiagnostic)))
  }
  if (type == "vif") {
    return(invisible(sjp.vif(fit)))
  }
  # check size argument
  if (is.null(geom.size)) geom.size <- 3
  # --------------------------------------------------------
  # auto-retrieve value labels
  # --------------------------------------------------------
  if (is.null(var.labels) && all(class(fit) != "plm")) {
    var.labels <- suppressWarnings(retrieveModelLabels(list(fit), group.pred = FALSE))
  }
  # check length of diagram title and split longer string at into new lines
  # every 50 chars
  if (!is.null(title)) title <- sjmisc::word_wrap(title, breakTitleAt)
  # check length of x-axis title and split longer string at into new lines
  # every 50 chars
  if (!is.null(axisTitle.x)) axisTitle.x <- sjmisc::word_wrap(axisTitle.x, breakTitleAt)
  # check length of x-axis-labels and split longer strings at into new lines
  # every 10 chars, so labels don't overlap
  if (!is.null(var.labels)) var.labels <- sjmisc::word_wrap(var.labels, breakLabelsAt)
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
  pv <- get_lm_pvalues(fit, include.intercept = F)$p
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
    if (!is.null(var.labels) && length(var.labels) > nrow(tmp))
      var.labels <- var.labels[-remrows]
    # remove p-values
    pv <- pv[-remrows]
  }
  # -------------------------------------------------
  # init data column for p-values
  # -------------------------------------------------
  ps <- sprintf("%.*f", labelDigits, tmp$beta)
  # if no values should be shown, clear
  # vector now
  if (!show.values) ps <- rep("", length(ps))
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
  if (is.null(var.labels) || length(var.labels) < length(row.names(betas)))
    var.labels <- row.names(betas)
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
      var.labels <- rev(var.labels[order(tmp$grp.est, tmp$beta)])
      betas <- betas[rev(order(tmp$grp.est, tmp$beta)), ]
    } else {
      var.labels <- var.labels[order(tmp$beta)]
      betas <- betas[order(tmp$beta), ]
    }
  } else {
    var.labels <- rev(var.labels)
    betas <- betas[nrow(betas):1, ]
  }
  betas <- cbind(1:nrow(betas), betas)
  # give columns names
  colnames(betas) <- c("xv", "Beta", "lower", "upper", "p", "pv", "grp.est")
  betas$p <- as.character(betas$p)
  betas$xv <- as.factor(betas$xv)
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
    ticks <- seq(lower_lim, upper_lim, by = gridBreaksAt)
  }
  # --------------------------------------------------------
  # Start plot here! First check how to colour geoms
  # (whether grouped or not)
  # --------------------------------------------------------
  if (!is.null(group.estimates)) {
    betaplot <- ggplot(betas, aes(x = xv, y = Beta, colour = grp.est))
    pal.len <- length(unique(group.estimates))
    legend.labels <- unique(betas$grp.est)
  } else {
    betaplot <- ggplot(betas, aes(x = xv, y = Beta, colour = (Beta >= 0)))
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
    geom_text(aes(label = p),
              nudge_x = y.offset,
              show.legend = FALSE) +
    # print point
    geom_point(size = geom.size) +
    # Intercept-line
    geom_hline(yintercept = 0,
               linetype = vline.type,
               color = vline.color) +
    # set y-scale-limits, breaks and tick labels
    scale_y_continuous(limits = c(lower_lim, upper_lim),
                       breaks = ticks,
                       labels = ticks) +
    # set value labels to x-axis
    scale_x_discrete(labels = var.labels,
                     limits = 1:length(var.labels)) +
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
  # set proper column names
  # -------------------------------------
  betas <- dplyr::add_rownames(betas)
  colnames(betas) <- c("term", "xpos", "estimate", "conf.low", 
                       "conf.high", "p.string", "p.value", "group")
  # -------------------------------------
  # return results
  # -------------------------------------
  invisible(structure(class = c("sjplot", "sjplm"),
                      list(plot = betaplot,
                           data = betas)))
}


sjp.reglin <- function(fit, 
                       title = NULL, 
                       breakTitleAt = 50, 
                       geom.colors = NULL,
                       show.ci = TRUE,
                       pointAlpha = 0.2,
                       showScatterPlot = TRUE,
                       show.loess = TRUE,
                       show.loess.ci = FALSE,
                       useResiduals = FALSE,
                       remove.estimates = NULL,
                       vars = NULL,
                       printPlot = TRUE) {
  # -----------------------------------------------------------
  # check argument
  # -----------------------------------------------------------
  geom.colors <- col_check(geom.colors, show.loess)
  # -----------------------------------------------------------
  # set color defaults
  # -----------------------------------------------------------
  if (show.loess) {
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
  model_data <- stats::model.frame(fit)
  depvar.label <- sjmisc::get_label(model_data[[1]], def.value = colnames(model_data)[1])
  resp <- model_data[[1]]
  # gls needs extra handling
  if (any(class(fit) == "gls")) 
    predvars <- all.vars(nlme::getCovariateFormula(fit))
  else
    predvars <- colnames(model_data)[-1]
  # ------------------------
  # remove estimates?
  # ------------------------
  if (!is.null(remove.estimates)) {
    remcols <- match(remove.estimates, predvars)
    # remember old rownames
    if (!sjmisc::is_empty(remcols))
      predvars <- predvars[-remcols]
  }
  # ------------------------
  # select specific setimates?
  # ------------------------
  if (!is.null(vars)) {
    remcols <- match(vars, predvars)
    # remember old rownames
    if (!sjmisc::is_empty(remcols))
      predvars <- predvars[remcols]
  }
  # -----------------------------------------------------------
  # retrieve name of dependent variable
  # -----------------------------------------------------------
  response <- ifelse(isTRUE(useResiduals), "residuals", depvar.label)
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
  for (p_v in predvars) {
    # -----------------------------------------------------------
    # create dummy-data frame with response and predictor
    # as data columns, used for the ggplot
    # -----------------------------------------------------------
    if (useResiduals) {
      mydat <- data.frame(x = model_data[[p_v]], y = stats::residuals(fit))
    } else {
      mydat <- data.frame(x = model_data[[p_v]], y = resp)
    }
    # -----------------------------------------------------------
    # plot regression line and confidence intervall
    # -----------------------------------------------------------
    reglinplot <- ggplot(mydat, aes(x = x, y = y)) +
      stat_smooth(method = "lm",
                  se = show.ci,
                  colour = lineColor)
    # -----------------------------------------------------------
    # plot jittered values if requested
    # -----------------------------------------------------------
    if (showScatterPlot) reglinplot <- reglinplot + geom_jitter(alpha = pointAlpha,
                                                                colour = pointColor)
    # -----------------------------------------------------------
    # check whether additional loess-line should be plotted
    # -----------------------------------------------------------
    if (show.loess) {
      reglinplot <- reglinplot +
        stat_smooth(method = "loess",
                    se = show.loess.ci,
                    colour = loessLineColor)
    }
    # -----------------------------------------------------------
    # set plot labs
    # -----------------------------------------------------------
    reglinplot <- reglinplot +
      labs(title = title,
           x = sjmisc::get_label(model_data[[p_v]], def.value = p_v),
           y = response)
    # ---------------------------------------------------------
    # Check whether ggplot object should be returned or plotted
    # ---------------------------------------------------------
    # concatenate plot object
    plotlist[[length(plotlist) + 1]] <- reglinplot
    dflist[[length(dflist) + 1]] <- mydat
    # print plot
    if (printPlot) graphics::plot(reglinplot)
  }
  # -------------------------------------
  # return results
  # -------------------------------------
  invisible(structure(class = "sjpreglin",
                      list(plot.list = plotlist,
                           df.list = dflist)))
}


col_check <- function(geom.colors, show.loess) {
  # define required length of color palette
  collen <- ifelse(isTRUE(show.loess), 3, 2)
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


#' @importFrom stats fitted rstudent residuals sd median
#' @importFrom dplyr add_rownames
sjp.lm.ma <- function(linreg, showOriginalModelOnly = TRUE, completeDiagnostic = FALSE) {
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
  # remove outliers, only non-mixed models
  # ---------------------------------
  if (any(class(linreg) == "lm")) {
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
    while (isTRUE(loop)) {
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
  } else {
    # we have no updated model w/o outliers for
    # other model classes than "lm"
    showOriginalModelOnly <- TRUE
    modelOptmized <- FALSE
    model <- linreg
    outlier <- NULL
  }
  # ---------------------------------
  # Print non-normality of residuals and outliers both of original and updated model
  # dots should be plotted along the line, this the dots should follow a linear direction
  # ---------------------------------
  ggqqp <- function(fit, title.suffix = " (original model)") {
    # mixed model model?
    if (any(class(fit) == "lme") || any(class(fit) == "lmerMod")) {
      res_ <- sort(stats::residuals(fit), na.last = NA)
      y_lab <- "Residuals"
    } else {
      # else, normal model
      res_ <- sort(stats::rstudent(fit), na.last = NA)
      y_lab <- "Studentized Residuals"
    }
    fitted_ <- sort(stats::fitted(fit), na.last = NA)
    # create data frame
    mydf <- na.omit(data.frame(x = fitted_, y = res_))
    # try to estimate outlier
    mydf <- cbind(quot = mydf$x / mydf$y, mydf)
    mydf$case.nr <- names(res_)
    mydf$ratio <- mydf$y / mydf$quot
    # something like a ratio of maximum distance from residuals
    quot.md <- stats::median(mydf$ratio)
    quot.sd <- stats::sd(mydf$ratio)
    quot.rng <- c(quot.md - quot.sd, quot.md + quot.sd)
    # label outliers with case number
    mydf$label <- NA
    outl <- which(mydf$ratio < (2 * quot.rng[1]))
    mydf$label[outl] <- mydf$case.nr[outl]
    outl <- which(mydf$ratio > (2 * quot.rng[2]))
    mydf$label[outl] <- mydf$case.nr[outl]
    # ggrepel installed?
    if (!requireNamespace("ggrepel", quietly = TRUE)) {
      text_label <- geom_text(hjust = "bottom", vjust = "bottom")
    } else {
      text_label <- ggrepel::geom_label_repel()
    }
    # plot it
    return(ggplot(mydf, aes(x = x, y = y, label = label)) +
             geom_point() +
             text_label +
             stat_smooth(method = "lm", se = FALSE) +
             labs(title = sprintf("Non-normality of residuals and outliers%s\n(Dots should be plotted along the line)", title.suffix),
                  y = y_lab,
                  x = "Theoretical quantiles"))
  }
  sjp.setTheme(theme = "scatterw")
  # qq-plot of studentized residuals for base model
  p1 <- ggqqp(linreg)
  # save plot
  plot.list[[length(plot.list) + 1]] <- p1
  # print plot
  suppressWarnings(print(p1))
  # qq-plot of studentized residuals for updated model
  if (modelOptmized) {
    p1 <- ggqqp(model, " (updated model)")
    # save plot
    plot.list[[length(plot.list) + 1]] <- p1
    # print plot
    suppressWarnings(print(p1))
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
                           args = list(mean = mean(unname(stats::residuals(fit)), na.rm = TRUE),
                                       sd = stats::sd(unname(stats::residuals(fit)), na.rm = TRUE)),
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
  if (any(class(linreg) == "lm")) {
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
      graphics::plot(car::crPlots(linreg))
      # ---------------------------------
      # non-independence of residuals
      # ---------------------------------
      print(car::durbinWatsonTest(linreg))
      # ---------------------------------
      # Print leverage plots
      # ---------------------------------
      graphics::plot(car::leveragePlots(linreg))
      # ---------------------------------
      # Non-constant residuals
      # ---------------------------------
      print(car::ncvTest(linreg))
      print(lmtest::bptest(linreg))
      print(car::spreadLevelPlot(linreg))
    }
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
                   var.labels=NULL,
                   resp.label=NULL,
                   breakLabelsAt=20,
                   geom.colors = NULL,
                   show.ci=TRUE,
                   pointAlpha=0.2,
                   showScatterPlot=TRUE,
                   show.loess=FALSE,
                   show.loess.ci=FALSE,
                   showModelSummary=TRUE,
                   useResiduals=FALSE,
                   printPlot=TRUE) {
  # -----------------------------------------------------------
  # check argument
  # -----------------------------------------------------------
  geom.colors <- col_check(geom.colors, show.loess)
  # -----------------------------------------------------------
  # set color defaults
  # -----------------------------------------------------------
  if (show.loess) {
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
  mf <- stats::model.frame(fit)
  cn <- colnames(mf)
  # -----------------------------------------------------------
  # retrieve name of predictor and response
  # -----------------------------------------------------------
  if (useResiduals)
    response <- "residuals"
  else
    response <- sjmisc::get_label(mf[[1]], def.value = cn[1])
  xval <- sjmisc::get_label(mf[[2]], def.value = cn[2])
  # -----------------------------------------------------------
  # create dummy-data frame with response and predictor
  # as data columns, used for the ggplot
  # -----------------------------------------------------------
  if (useResiduals) {
    mydat <- as.data.frame(cbind(x = as.vector(mf[, 2]),
                                 y = fit$residuals))
  } else {
    # use as.vector, to make function work with plm-objects
    mydat <- as.data.frame(cbind(x = as.vector(mf[, 2]),
                                 y = as.vector(mf[, 1])))
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
  if (is.null(var.labels)) var.labels <- xval
  if (is.null(resp.label)) resp.label <- response
  # check length of axis-labels and split longer strings at into new lines
  # every 10 chars, so labels don't overlap
  var.labels <- sjmisc::word_wrap(var.labels, breakLabelsAt)
  resp.label <- sjmisc::word_wrap(resp.label, breakLabelsAt)
  # -----------------------------------------------------------
  # plot regression line and confidence intervall
  # -----------------------------------------------------------
  reglinplot <- ggplot(mydat,
                       aes(x = x, y = y)) +
    stat_smooth(method = "lm",
                se = show.ci,
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
  if (show.loess) {
    reglinplot <- reglinplot +
      stat_smooth(method = "loess",
                  se = show.loess.ci,
                  colour = loessLineColor)
  }
  # -----------------------------------------------------------
  # set plot labs
  # -----------------------------------------------------------
  reglinplot <- reglinplot +
    labs(title = title, x = var.labels, y = resp.label)
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
                        axisTitle.y,
                        show.ci,
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
  mf <- stats::model.frame(fit)
  # get model data column names
  cn <- colnames(mf)
  xl <- NULL
  # any axis title?
  if (is.null(axisTitle.y)) {
    # find response name
    resp.name <- "Response"
    resp.name <- get_var_name(cn[1])
  } else {
    resp.name <- axisTitle.y
  }
  # -------------------------------------
  # argument check: poly.term required and
  # polynomial must be found in model
  # -------------------------------------
  if (!is.null(poly.term)) {
    # check for simple poly term, using I(x^2) + I(x^3) etc.
    poly.found <- any(cn == poly.term)
    # found poly? If yes, get range
    if (poly.found) {
      xl <- list(x = sort(unique(stats::na.omit(mf[, poly.term]))))
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
  if (is.null(axisTitle.x) || axisTitle.x == "Estimates") axisTitle.x <- poly.term
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
  if (show.ci) polyplot <- polyplot + geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .15)
  # plot predicted effect of polynomial term
  polyplot <- polyplot +
    geom_line(colour = geom.colors[1], size = geom.size) +
    labs(x = axisTitle.x, y = resp.name)
  # print plot
  if (printPlot) print(polyplot)
  # return result
  invisible(structure(class = "sjplmpoly",
                      list(plot = polyplot,
                           data = mydat)))
}


get_lm_pvalues <- function(fit, include.intercept = TRUE) {
  # retrieve sigificance level of independent variables (p-values)
  if (any(class(fit) == "pggls")) {
    p <- summary(fit)$CoefTable[, 4]
    se <- summary(fit)$CoefTable[, 2]
  } else if (any(class(fit) == "gls")) {
    p <- summary(fit)$tTable[, 4]
    se <- summary(fit)$tTable[, 2]
  } else {
    p <- stats::coef(summary(fit))[, 4]
    se <- stats::coef(summary(fit))[, 2]
  }
  # remove intercept?
  if (!include.intercept) {
    p <- p[-1]
    se <- se[-1]
  }
  return(list(p = p, se = se))
}