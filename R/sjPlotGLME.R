# bind global variables
if(getRversion() >= "2.15.1") utils::globalVariables(c("nQQ", "ci", "fixef", "fade", "lower.CI", "upper.CI", "pred", "prob", "p"))


#' @title Plot odds ratios (forest plots) of generalized linear mixed effects models
#' @name sjp.glmer
#'
#' @seealso \itemize{
#'            \item \href{http://www.strengejacke.de/sjPlot/sjp.glmer/}{sjPlot manual: sjp.glmer}
#'            \item \code{\link{sjp.lmer}}
#'            \item \code{\link{sjp.glm}}
#'            \item \code{\link{sjp.glmm}}
#'            \item \code{\link{sjt.glm}}
#'          }
#'
#' @description Plot odds ratios (exponentiated coefficients) with confidence intervalls of either
#'                fixed effects or random effects of generalized linear mixed effects models
#'                (that have been fitted with \code{glmer} of the
#'                \code{lme4} package.
#'
#' @param fit a fitted \code{glmer} object.
#' @param type type of plot. Use one of following:
#'          \itemize{
#'            \item \code{"re"} (default) for odds ratios of random effects
#'            \item \code{"fe"} for odds ratios of fixed effects
#'            \item \code{"fe.cor"} for correlation matrix of fixed effects
#'            \item \code{"re.qq"} for a QQ-plot of random effects (random effects quantiles against standard normal quantiles)
#'            \item \code{"fe.pc"} or \code{"fe.prob"} to plot probability curves (predicted probabilities) of all fixed effects coefficients. Use \code{facet.grid} to decide whether to plot each coefficient as separate plot or as integrated faceted plot.
#'            \item \code{"ri.pc"} or \code{"ri.prob"} to plot probability curves (predicted probabilities) of random intercept variances for all fixed effects coefficients. Use \code{facet.grid} to decide whether to plot each coefficient as separate plot or as integrated faceted plot.
#'          }
#' @param vars a numeric vector with column indices of selected variables or a character vector with
#'          variable names of selected variables from the fitted model, which should be used to plot probability
#'          curves. This parameter only applies if \code{type} is either \code{"fe.pc"} (resp. \code{"fe.prob"})
#'          or \code{"fe.pc"} (resp. \code{"re.pc"}). In this case, only probability curves (predicted probabilities) for the selected
#'          variables specified in \code{"vars"} will be plotted.
#' @param ri.nr Numeric value. If \code{type = "re"} and fitted model has more than one random
#'          intercept, \code{ri.nr} indicates which random effects of which random intercept (or:
#'          which list element of \code{lme4::ranef}) will be plotted. Default is \code{1},
#'          so the first (or only) random intercept will be plotted.
#' @param show.se Use \code{TRUE} to plot (depending on \code{type}) the standard
#'          error for probability curves (predicted probabilities).
#' @param title a character vector with one or more labels that are used as plot title. If
#'          \code{type = "re"}, use the predictors' variable labels as titles.
#' @param geom.colors User defined color palette for geoms. Must either be vector with two color values
#'          or a specific color palette code (see below).
#'          \itemize{
#'            \item If not specified, the diverging \code{"Set1"} color brewer palette will be used.
#'            \item If \code{"gs"}, a greyscale will be used.
#'            \item If \code{geom.colors} is any valid color brewer palette name, the related \href{http://colorbrewer2.org}{color brewer} palette will be used. Use \code{display.brewer.all()} from the \code{RColorBrewer} package to view all available palette names.
#'          }
#'          Else specify your own color values as vector (e.g. \code{geom.colors=c("#f00000", "#00ff00")}).
#' @param geom.size size of geoms (point size).
#' @param hideErrorBars If \code{TRUE}, the error bars that indicate the confidence intervals of the odds ratios are not
#'          shown.
#' @param showIntercept if \code{TRUE}, the intercept is included when plotting random or fixed effects.
#' @param stringIntercept string of intercept estimate on the y axis. Only applies, if \code{showIntercept}
#'          is \code{TRUE} and \code{pred.labels} is not \code{NULL}.
#' @param sort.coef indicates which coefficient should be used for sorting odds ratios
#'          \itemize{
#'            \item If \code{NULL} (default), no sorting is done and odds ratios are sorted in order of model coefficients.
#'            \item If \code{sort.coef = "sort.all"}, odds ratios are re-sorted for each coefficient (only applies if \code{type = "re"} and \code{facet.grid = FALSE}), i.e. the odds ratios of the random effects for each predictor are sorted and plotted to an own plot.
#'            \item If \code{type = "fe"}, \code{TRUE} value will sort the odds ratios
#'            \item Else, specify a predictor's / coefficient's name to sort odds ratios according to this coefficient.
#'            }
#'            See examples for details.
#' @param fade.ns if \code{TRUE}, non significant odds ratios will be printed in slightly fading colors.
#' @param pred.labels a character vector with labels for the predictors / covariates / groups. Should either be vector
#'          of fixed effects variable labels (if \code{type = "fe"}) or a vector of group (value)
#'          labels from the random intercept's categories (if \code{type = "re"}).
#' @param axisTitle.x A label (title) for the x axis. If not specified, a default labelling depending
#'          on the plot type is chosen.
#' @param axisTitle.y A label (title) for the y axis. If not specified, a default labelling depending
#'          on the plot type is chosen.
#' @param interceptLineType The linetype of the intercept line (zero point). Default is \code{2} (dashed line).
#' @param interceptLineColor The color of the intercept line. Default value is \code{"grey70"}.
#' @param showValueLabels Whether odds ratio values should be plotted to each dot or not.
#' @param labelDigits The amount of digits for rounding the estimations (see \code{showValueLabels}).
#'          Default is 2, i.e. estimators have 2 digits after decimal point.
#' @param showPValueLabels Whether the significance levels of each coefficient should be appended
#'          to values or not.
#' @param facet.grid \code{TRUE} when each plot should be plotted separately instead of
#'          an integrated (faceted) single graph.
#' @param free.scale If \code{TRUE} and \code{facet.grid=TRUE}, each facet grid gets its own fitted scale. If
#'          \code{free.scale=FALSE}, each facet in the grid has the same scale range.
#' @param printPlot If \code{TRUE} (default), plots the results as graph. Use \code{FALSE} if you don't
#'          want to plot any graphs. In either case, the ggplot-object will be returned as value.
#' @return (Insisibily) returns
#'          \itemize{
#'            \item the ggplot-object (\code{plot}), if \code{type = "fe"} or if \code{type = "re"} and \code{facet.grid = TRUE}). Multiple plots (\code{type = "re"} and if \code{facet.grid = FALSE}) are returned in the object \code{plot.list}.
#'            \item a list of ggplot-objects (\code{plot.list}). see \code{plot} for details.
#'            \item a data frame \code{mydf} with the data used to build the ggplot-object(s).
#'            }
#'
#' @note Thanks to Robert Reijntjes from Leiden University Medical Center for sharing
#'         R code that is used to compute fixed effects correlation matrices and
#'         qq-plots of random effects.
#'
#' @examples
#' \dontrun{
#' library(lme4)
#' # create binary response
#' sleepstudy$Reaction.dicho <- dicho(sleepstudy$Reaction, dichBy = "md")
#' # fit model
#' fit <- glmer(Reaction.dicho ~ Days + (Days | Subject),
#'              sleepstudy,
#'              family = binomial("logit"))
#'
#' # simple plot
#' sjp.glmer(fit)
#'
#' # sort by predictor Days
#' sjp.glmer(fit, sort.coef = "Days")
#'
#' # plot each predictor as own plot and sort them
#' sjp.glmer(fit,
#'           facet.grid = FALSE,
#'           sort.coef = "sort.all")
#'
#' library(lme4)
#' data(efc)
#' # create binary response
#' efc$hi_qol <- dicho(efc$quol_5)
#' # prepare group variable
#' efc$grp = as.factor(efc$e15relat)
#' levels(x = efc$grp) <- get_val_labels(efc$e15relat)
#' # data frame for fitted model
#' mydf <- na.omit(data.frame(hi_qol = as.factor(efc$hi_qol),
#'                            sex = as.factor(efc$c161sex),
#'                            c12hour = as.numeric(efc$c12hour),
#'                            neg_c_7 = as.numeric(efc$neg_c_7),
#'                            grp = efc$grp))
#' # fit glmer
#' fit <- glmer(hi_qol ~ sex + c12hour + neg_c_7 + (1|grp),
#'              data = mydf,
#'              family = binomial("logit"))
#'
#' # plot random effects
#' sjp.glmer(fit)
#'
#' # plot fixed effects
#' sjp.glmer(fit, type = "fe")
#'
#' # plot and sort fixed effects
#' sjp.glmer(fit,
#'           type = "fe",
#'           sort.coef = TRUE)
#'
#' # plot fixed effects correlations
#' sjp.glmer(fit, type = "fe.cor")
#'
#' # qq-plot of random effects
#' sjp.glmer(fit, type = "re.qq")
#'
#' # plot probability curves (predicted probabilities) 
#' # for each covariate, grouped by random intercepts
#' sjp.glmer(fit,
#'           type = "ri.pc",
#'           show.se = TRUE)
#'
#' # plot probability curves (predicted probabilities)
#' # for each covariate, grouped by random intercepts 
#' # in integrated plots
#' sjp.glmer(fit,
#'           type = "ri.pc",
#'           facet.grid = FALSE)
#'
#' # plot probability curve (predicted probabilities)
#' # of fixed effect, only for coefficient "neg_c_7"
#' sjp.glmer(fit, 
#'           type = "fe.pc", 
#'           vars = "neg_c_7")}
#'
#' @import ggplot2
#' @export
sjp.glmer <- function(fit,
                      type = "re",
                      vars = NULL,
                      ri.nr = 1,
                      title = NULL,
                      geom.size = 3,
                      geom.colors = "Set1",
                      hideErrorBars = FALSE,
                      showIntercept = TRUE,
                      stringIntercept = "(Intercept)",
                      sort.coef = NULL,
                      pred.labels = NULL,
                      axisTitle.x = NULL,
                      axisTitle.y = NULL,
                      facet.grid = TRUE,
                      free.scale = FALSE,
                      interceptLineType = 2,
                      interceptLineColor = "grey70",
                      showValueLabels = TRUE,
                      labelDigits = 2,
                      showPValueLabels = TRUE,
                      fade.ns = FALSE,
                      show.se = FALSE,
                      printPlot = TRUE) {

  if (type == "fe.prob") type <- "fe.pc"
  if (type == "ri.prob") type <- "ri.pc"

  sjp.lme4(fit,
           type,
           vars,
           ri.nr,
           title,
           geom.size,
           geom.colors,
           hideErrorBars,
           showIntercept,
           stringIntercept,
           sort.coef,
           pred.labels,
           axisTitle.x,
           axisTitle.y,
           interceptLineType,
           interceptLineColor,
           showValueLabels,
           labelDigits,
           showPValueLabels,
           facet.grid,
           free.scale,
           fade.ns,
           show.se,
           printPlot,
           fun = "glm")
}


#' @title Plot estimates (forest plots) of linear mixed effects models
#' @name sjp.lmer
#'
#' @seealso \itemize{
#'            \item \href{http://www.strengejacke.de/sjPlot/sjp.lmer/}{sjPlot manual: sjp.lmer}
#'            \item \code{\link{sjp.glmer}}
#'            \item \code{\link{sjp.lm}}
#'            \item \code{\link{sjp.lmm}}
#'            \item \code{\link{sjt.lm}}
#'          }
#'
#' @description Plot estimates (coefficients) with confidence intervalls of either
#'                fixed effects or random effects of linear mixed effects models
#'                (that have been fitted with \code{lmer} of the
#'                \code{lme4} package.
#'
#' @param fit a fitted \code{lmer} object.
#' @param type type of plot. Use one of following:
#'          \itemize{
#'            \item \code{"re"} (default) for estimates of random effects
#'            \item \code{"fe"} for estimates of fixed effects
#'            \item \code{"fe.std"} for standardized estimates of fixed effects
#'            \item \code{"fe.cor"} for correlation matrix of fixed effects
#'            \item \code{"re.qq"} for a QQ-plot of random effects (random effects quantiles against standard normal quantiles)
#'            \item \code{"fe.ri"} for fixed effects slopes depending on the random intercept.
#'          }
#' @param vars a numeric vector with column indices of selected variables or a character vector with
#'          variable names of selected variables from the fitted model, which should be used to plot probability
#'          curves (predicted probabilities). This parameter only applies if \code{type = "fe.ri"}. In this case, only 
#'          probability curves for the selected variables specified in \code{"vars"} will be plotted.
#' @param ri.nr Numeric value. If \code{type = "re"} and fitted model has more than one random
#'          intercept, \code{ri.nr} indicates which random effects of which random intercept (or:
#'          which list element of \code{lme4::ranef}) will be plotted. Default is \code{1},
#'          so the first (or only) random intercept will be plotted.
#' @param title a character vector with one or more labels that are used as plot title. If
#'          \code{type = "re"}, use the predictors' variable labels as titles.
#' @param geom.colors User defined color palette for geoms. Must either be vector with two color values
#'          or a specific color palette code (see below).
#'          \itemize{
#'            \item If not specified, the diverging \code{"Set1"} color brewer palette will be used.
#'            \item If \code{"gs"}, a greyscale will be used.
#'            \item If \code{geom.colors} is any valid color brewer palette name, the related \href{http://colorbrewer2.org}{color brewer} palette will be used. Use \code{display.brewer.all()} from the \code{RColorBrewer} package to view all available palette names.
#'          }
#'          Else specify your own color values as vector (e.g. \code{geom.colors=c("#f00000", "#00ff00")}).
#' @param geom.size size of geoms (point size).
#' @param hideErrorBars If \code{TRUE}, the error bars that indicate the confidence intervals of the estimates are not
#'          shown.
#' @param showIntercept if \code{TRUE}, the intercept is included when plotting random or fixed effects.
#' @param stringIntercept string of intercept estimate on the y axis. Only applies, if \code{showIntercept}
#'          is \code{TRUE} and \code{pred.labels} is not \code{NULL}.
#' @param sort.coef indicates which coefficient should be used for sorting estimates.
#'          \itemize{
#'            \item If \code{NULL} (default), no sorting is done and estimates are sorted in order of model coefficients.
#'            \item If \code{sort.coef = "sort.all"}, estimates are re-sorted for each coefficient (only applies if \code{type = "re"} and \code{facet.grid = FALSE}), i.e. the estimates of the random effects for each predictor are sorted and plotted to an own plot.
#'            \item If \code{type = "fe"} or \code{type = "fe.std"}, \code{TRUE} value will sort the estimates.
#'            \item Else, specify a predictor's / coefficient's name to sort estimators according to this coefficient.
#'            }
#'            See examples for details.
#' @param fade.ns if \code{TRUE}, non significant estimates will be printed in slightly fading colors.
#' @param pred.labels a character vector with labels for the predictors / covariates / groups. Should either be vector
#'          of fixed effects variable labels (if \code{type = "fe"} or \code{type = "fe.std"}) or a vector of group (value)
#'          labels from the random intercept's categories (if \code{type = "re"}).
#' @param axisTitle.x A label (title) for the x axis. If not specified, a default labelling depending
#'          on the plot type is chosen.
#' @param axisTitle.y A label (title) for the y axis. If not specified, a default labelling depending
#'          on the plot type is chosen.
#' @param interceptLineType The linetype of the intercept line (zero point). Default is \code{2} (dashed line).
#' @param interceptLineColor The color of the intercept line. Default value is \code{"grey70"}.
#' @param showValueLabels Whether the beta and standardized beta values should be plotted
#'          to each dot or not.
#' @param labelDigits The amount of digits for rounding the estimations (see \code{showValueLabels}).
#'          Default is 2, i.e. estimators have 2 digits after decimal point.
#' @param showPValueLabels Whether the significance levels of each coefficient should be appended
#'          to values or not
#' @param facet.grid \code{TRUE} when each plot should be plotted separately instead of
#'          an integrated (faceted) single graph.
#' @param free.scale If \code{TRUE} and \code{facet.grid=TRUE}, each facet grid gets its own fitted scale. If
#'          \code{free.scale=FALSE}, each facet in the grid has the same scale range.
#' @param printPlot If \code{TRUE} (default), plots the results as graph. Use \code{FALSE} if you don't
#'          want to plot any graphs. In either case, the ggplot-object will be returned as value.
#' @return (Insisibily) returns
#'          \itemize{
#'            \item the ggplot-object (\code{plot}), if \code{type = "fe"} or if \code{type = "re"} and \code{facet.grid = TRUE}). Multiple plots (\code{type = "re"} and if \code{facet.grid = FALSE}) are returned in the object \code{plot.list}.
#'            \item a list of ggplot-objects (\code{plot.list}). see \code{plot} for details.
#'            \item a data frame \code{mydf} with the data used to build the ggplot-object(s).
#'            }
#'
#' @examples
#' \dontrun{
#' # fit model
#' library(lme4)
#' fit <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
#'
#' # simple plot
#' sjp.lmer(fit)
#'
#' # plot fixed effects
#' sjp.lmer(fit, type = "fe")
#'
#' # sort by predictor Days
#' sjp.lmer(fit, sort.coef = "Days")
#'
#' # plot each predictor as own plot
#' # sort each plot
#' sjp.lmer(fit,
#'          facet.grid = FALSE,
#'          sort.coef = "sort.all")
#'
#' # plot and sort fixed effects
#' sjp.lmer(fit,
#'          type = "fe",
#'          sort.coef = TRUE)
#'
#'
#' library(lme4)
#' data(efc)
#' # prepare group variable
#' efc$grp = as.factor(efc$e15relat)
#' levels(x = efc$grp) <- get_val_labels(efc$e15relat)
#' # data frame for fitted model
#' mydf <- na.omit(data.frame(neg_c_7 = as.numeric(efc$neg_c_7),
#'                            sex = as.factor(efc$c161sex),
#'                            c12hour = as.numeric(efc$c12hour),
#'                            barthel = as.numeric(efc$barthtot),
#'                            grp = efc$grp))
#' # fit glmer
#' fit <- lmer(neg_c_7 ~ sex + c12hour + barthel + (1|grp),
#'             data = mydf)
#'
#' # plot random effects
#' sjp.lmer(fit)
#'
#' # plot fixed effects
#' sjp.lmer(fit, type = "fe")
#'
# plot and sort standardized fixed effects
#' sjp.lmer(fit,
#'          type = "fe.std",
#'          sort.coef = TRUE)
#'
#' # plot fixed effects slopes for
#' # each random intercept, but only for
#' # coefficient "c12hour"
#' sjp.lmer(fit, 
#'          type = "fe.ri", 
#'          vars = "c12hour")
#' 
#' # plot fixed effects correlations
#' sjp.lmer(fit, type = "fe.cor")
#'
#' # qq-plot of random effects
#' sjp.lmer(fit, type = "re.qq")}
#'
#' @import ggplot2
#' @export
sjp.lmer <- function(fit,
                     type = "re",
                     vars = NULL,
                     ri.nr = 1,
                     title = NULL,
                     geom.size = 3,
                     geom.colors = "Set1",
                     hideErrorBars = FALSE,
                     showIntercept = TRUE,
                     stringIntercept = "(Intercept)",
                     sort.coef = NULL,
                     pred.labels = NULL,
                     axisTitle.x = NULL,
                     axisTitle.y = NULL,
                     interceptLineType = 2,
                     interceptLineColor = "grey70",
                     showValueLabels=TRUE,
                     labelDigits=2,
                     showPValueLabels=TRUE,
                     facet.grid = TRUE,
                     free.scale = FALSE,
                     fade.ns = FALSE,
                     printPlot = TRUE) {

  if (type == "fe.prob") type <- "fe.pc"
  if (type == "ri.prob") type <- "ri.pc"
  
  sjp.lme4(fit,
           type,
           vars,
           ri.nr,
           title,
           geom.size,
           geom.colors,
           hideErrorBars,
           showIntercept,
           stringIntercept,
           sort.coef,
           pred.labels,
           axisTitle.x,
           axisTitle.y,
           interceptLineType,
           interceptLineColor,
           showValueLabels,
           labelDigits,
           showPValueLabels,
           facet.grid,
           free.scale,
           fade.ns,
           FALSE,
           printPlot,
           fun = "lm")
}

sjp.lme4  <- function(fit,
                      type,
                      vars,
                      ri.nr,
                      title,
                      geom.size,
                      geom.colors,
                      hideErrorBars,
                      showIntercept,
                      stringIntercept,
                      sort.coef,
                      pred.labels,
                      axisTitle.x,
                      axisTitle.y,
                      interceptLineType,
                      interceptLineColor,
                      showValueLabels,
                      labelDigits,
                      showPValueLabels,
                      facet.grid,
                      free.scale,
                      fade.ns,
                      show.se,
                      printPlot,
                      fun) {
  # -------------------------------------
  # check type
  # -------------------------------------
  if (type != "re" && type != "fe" && type != "fe.std" && type != "fe.cor" &&
      type != "re.qq" && type != "fe.pc" && type != "ri.pc" &&
      type != "fe.prob" && type != "ri.prob" && type != "fe.ri") {
    warning("'type' must be one of 're', 'fe', 'fe.cor', 're.qq', 'fe.ri', 'fe.pc', 'ri.pc', 'fe.std', 'fe.prob' or 'ri.prob'. Defaulting to 'fe' now.")
    type  <- "fe"
  }
  # -------------------------------------
  # check if required package is available
  # -------------------------------------
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package 'lme4' needed for this function to work. Please install it.", call. = FALSE)
  }
  if (!requireNamespace("Matrix", quietly = TRUE)) {
    stop("Package 'Matrix' needed for this function to work. Please install it.", call. = FALSE)
  }
  if (!requireNamespace("arm", quietly = TRUE)) {
    stop("Package 'arm' needed for this function to work. Please install it.", call. = FALSE)
  }
  # ---------------------------------------
  # for standardized coefficients, intercept
  # is always 0, so no need to be shown
  # ---------------------------------------
  if (type == "fe.std") showIntercept <- FALSE
  # ---------------------------------------
  # empty mydf
  # ---------------------------------------
  mydf <- data.frame()
  # ---------------------------------------
  # check whether random or fixed effects
  # should be plotted
  # ---------------------------------------
  if (type == "re") {
    # ---------------------------------------
    # check amounnt of random intercepts
    # ---------------------------------------
    if (length(lme4::ranef(fit)) > ri.nr) {
      message("Index specified in 'ri.nr' was larger than amount of random intercepts in model. Paramater 'ri.nr' was set to 1.")
      ri.nr <- 1
    }
    # ---------------------------------------
    # copy estimates of random effects
    # ---------------------------------------
    mydf.ef <- as.data.frame(lme4::ranef(fit)[[ri.nr]])
    # ---------------------------------------
    # copy rownames as axis labels, if not set
    # ---------------------------------------
    if (is.null(pred.labels)) {
      # use rownames, if pred.labels not available
      pred.labels <- rownames(mydf.ef)
      # check if intercept should be removed?
      if (!showIntercept) pred.labels <- pred.labels[-1]
    }
    else {
      # check if intercept should be added, in case
      # pred.labels are passed
      if (showIntercept) pred.labels <- c(stringIntercept, pred.labels)
    }
    # ---------------------------------------
    # show intercept?
    # ---------------------------------------
    startAt <- ifelse (showIntercept == TRUE, 1, 2)
    # ---------------------------------------
    # select random effects for each coefficient
    # ---------------------------------------
    for (i in startAt:ncol(mydf.ef)) {
      # ---------------------------------------
      # retrieve standard errors, for ci
      # ---------------------------------------
      se.fit <- arm::se.ranef(fit)[[ri.nr]]
      # ---------------------------------------
      # create data frame
      # 1. col: odds ratios /estimates of re-estimates
      # 2. col.: lower conf int
      # 3. col: upper conf int
      # ---------------------------------------
      if (fun == "glm") {
        tmp <- as.data.frame(cbind(OR = exp(mydf.ef[, i]),
                                   lower.CI = exp(mydf.ef[, i] - (1.96 * se.fit[, i])),
                                   upper.CI = exp(mydf.ef[, i] + (1.96 * se.fit[, i]))))
      }
      else {
        tmp <- as.data.frame(cbind(OR = mydf.ef[, i],
                                   lower.CI = mydf.ef[, i] - (1.96 * se.fit[, i]),
                                   upper.CI = mydf.ef[, i] + (1.96 * se.fit[, i])))
      }
      # ---------------------------------------
      # set column names (variable / coefficient name)
      # as group indicator
      # ---------------------------------------
      tmp$grp <- colnames(mydf.ef)[i]
      # ---------------------------------------
      # sort data frame. init order
      # ---------------------------------------
      reihe <- c(1 : nrow(tmp))
      # ---------------------------------------
      # sorting requested?
      # ---------------------------------------
      if (!is.null(sort.coef)) {
        # ---------------------------------------
        # should all plots be sorted? works only
        # when faceting is FALSE
        # ---------------------------------------
        if (sort.coef == "sort.all") {
          if (facet.grid) {
            # ---------------------------------------
            # no sorting with facet.grids, because y-axis-labels
            # (group levels / labels) have to be re-sorted for
            # each coefficient, which is not possible with facet.grids
            # ---------------------------------------
            message("Sorting each group of random intercept ('sort.all') is not possible when 'facet.grid = TRUE'.")
          }
          else {
            # ---------------------------------------
            # sort odds ratios of random effects
            # for current coefficient
            # ---------------------------------------
            reihe <- order(mydf.ef[, i])
          }
        }
        else {
          # ---------------------------------------
          # else, just sort a specific coefficient
          # this also works with facet.grid
          # ---------------------------------------
          reihe <- order(mydf.ef[, sort.coef])
        }
        # ---------------------------------------
        # sort data frame
        # ---------------------------------------
        tmp <- tmp[reihe,]
      }
      # ---------------------------------------
      # save sorting order to data frame, so
      # axis labels can be sorted accordingly later
      # ---------------------------------------
      tmp$sorting <- reihe
      # no p-values for random effects
      tmp$p <- ""
      # ---------------------------------------
      # add to final data frame
      # ---------------------------------------
      mydf <- data.frame(rbind(mydf, tmp))
    }
  }
  # ---------------------------------------
  # fixed effects, odds ratios
  # ---------------------------------------
  else if (type == "fe" || type == "fe.std") {
    # ---------------------------------------
    # retrieve odds ratios and conf int of
    # fixed effects
    # ---------------------------------------
    if (fun == "glm") {
      if (type == "fe.std") {
        warning("'type = fe.std' only works for linear models.", call. = F)
      }
      mydf <- as.data.frame(exp(cbind(OR = lme4::fixef(fit),
                                      lme4::confint.merMod(fit, method = "Wald"))))
    }
    else {
      if (type == "fe.std") {
        tmpdf <- std_beta(fit)
        mydf <- as.data.frame(cbind(OR = tmpdf$stdcoef,
                                    lower.CI = tmpdf$stdcoef - (1.96 * tmpdf$stdse),
                                    upper.CI = tmpdf$stdcoef + (1.96 * tmpdf$stdse)))
        # set default row names
        rownames(mydf) <- names(lme4::fixef(fit))
      }
      else {
        mydf <- as.data.frame(cbind(OR = lme4::fixef(fit),
                                    lme4::confint.merMod(fit, method = "Wald")))
      }
    }
    # ----------------------------
    # print p-values in bar charts
    # ----------------------------
    # retrieve sigificance level of independent variables (p-values)
    cs <- coef(summary(fit))
    # check if we have p-values in summary
    if (ncol(cs) >= 4) {
      pv <- cs[, 4]
    }
    else {
      # if we don't have p-values in summary,
      # don't show them
      pv <- rep(1, nrow(cs))
      showPValueLabels <- FALSE
    }
    # for better readability, convert p-values to asterisks
    # with:
    # p < 0.001 = ***
    # p < 0.01 = **
    # p < 0.05 = *
    # retrieve odds ratios
    if (fun == "glm") {
      ov <- exp(lme4::fixef(fit))
    }
    else {
      if (type == "fe.std") {
        ov <- std_beta(fit)$stdcoef
      }
      else {
        ov <- lme4::fixef(fit)
      }
    }
    # init data column for p-values
    ps <- rep("", length(ov))
    # ----------------------------
    # copy OR-values into data column
    # ----------------------------
    if (showValueLabels) {
      for (i in 1:length(ov)) {
        ps[i] <- sprintf("%.*f", labelDigits, ov[i])
      }
    }
    # ----------------------------
    # copy p-values into data column
    # ----------------------------
    if (showPValueLabels) {
      for (i in 1:length(pv)) {
        if (pv[i] >= 0.05) {
        }
        else if (pv[i] >= 0.01 && pv[i] < 0.05) {
          ps[i] <- paste(ps[i], "*")
        }
        else if (pv[i] >= 0.001 && pv[i] < 0.01) {
          ps[i] <- paste(ps[i], "**")
        }
        else {
          ps[i] <- paste(ps[i], "***")
        }
      }
    }
    # bind p-values
    mydf$p <- ps
    # ---------------------------------------
    # set proper column names
    # ---------------------------------------
    colnames(mydf) <- c("OR", "lower.CI", "upper.CI", "p")
    # ---------------------------------------
    # just one group, so no faceting needed
    # ---------------------------------------
    mydf$grp <- c("1")
    facet.grid <- FALSE
    if (is.null(title)) title <- "Fixed effects"
    # ---------------------------------------
    # show intercept?
    # ---------------------------------------
    if (!showIntercept) mydf <- mydf[-1, ]
    # ---------------------------------------
    # copy rownames as axis labels, if not set
    # ---------------------------------------
    if (is.null(pred.labels)) {
      pred.labels <- rownames(mydf)
    }
    else {
      # check if intercept should be added, in case
      # pred.labels are passed
      if (showIntercept) pred.labels <- c(stringIntercept, pred.labels)
    }
    # ---------------------------------------
    # sort data frame. init order
    # ---------------------------------------
    reihe <- c(1 : nrow(mydf))
    # ---------------------------------------
    # just one sorting option, simply sort odds ratios
    # ---------------------------------------
    if (!is.null(sort.coef)) {
      reihe <- order(mydf$OR)
      mydf <- mydf[reihe,]
    }
    mydf$sorting <- reihe
  }
  else if (type == "fe.cor") {
    return (invisible(sjp.lme.fecor(fit,
                                    pred.labels,
                                    sort.coef,
                                    fun,
                                    printPlot)))
  }
  else if (type == "fe.ri") {
    if (fun == "lm") {
      return (invisible(sjp.lme.feri(fit,
                                     ri.nr,
                                     vars,
                                     printPlot)))
      }
    else {
      warning("Fixed effects plots by random intercept effects (grouping levels) only works for function 'sjp.lmer'.", call. = FALSE)
      return
    }
  }
  else if (type == "re.qq") {
    return (invisible(sjp.lme.reqq(fit,
                                   geom.colors,
                                   geom.size,
                                   hideErrorBars,
                                   interceptLineType,
                                   interceptLineColor,
                                   fun,
                                   printPlot)))
  }
  else if (type == "fe.pc") {
    if (fun == "glm") {
      return(invisible(sjp.lme.feprobcurv(fit,
                                          show.se,
                                          facet.grid,
                                          vars,
                                          printPlot)))
    }
    else {
      warning("Probability plots of fixed effects only works for function 'sjp.glmer'.", call. = FALSE)
      return
    }
  }
  else if (type == "ri.pc") {
    if (fun == "glm") {
      return (invisible(sjp.lme.reprobcurve(fit,
                                            show.se,
                                            facet.grid,
                                            ri.nr,
                                            vars,
                                            printPlot)))
    }
    else {
      warning("Probability plots of random intercept effects only works for function 'sjp.glmer'.", call. = FALSE)
      return
    }
  }
  # ---------------------------------------
  # discrete x position, needed for ggplot
  # ---------------------------------------
  mydf$x <- as.factor(1 : length(pred.labels))
  # ---------------------------------------
  # set indicator whether or not non significant
  # odds ratios should be faded.
  # ---------------------------------------
  if (fade.ns == TRUE) {
    interc <- ifelse (fun == "glm", 1, 0)
    mydf$fade <- (mydf$lower.CI < interc & mydf$upper.CI > interc)
  }
  else {
    mydf$fade <- FALSE
  }
  # ---------------------------------------
  # dummy function to plot the odds ratios
  # ---------------------------------------
  plot.effe <- function(mydf,
                        title,
                        facet.grid,
                        hideErrorBars,
                        interceptLineType,
                        interceptLineColor,
                        fun,
                        type,
                        free.scale) {
    # ---------------------------------------
    # ggplot-objekt
    # ---------------------------------------
    interc <- ifelse (fun == "glm", 1, 0)
    mydf$interc <- interc
    gp <- ggplot(mydf, aes(x = x,
                           y = OR,
                           colour = (OR > interc),
                           alpha = fade)) +
      # Intercept-line
      geom_hline(yintercept = interc,
                 linetype = interceptLineType,
                 color = interceptLineColor) +
      geom_point(size = geom.size) +
      # print value labels and p-values
      geom_text(aes(label = p, y = OR),
                vjust = -0.7) +
      # ---------------------------------------
      # labels in sorted order
      # ---------------------------------------
      scale_x_discrete(labels = pred.labels[mydf$sorting]) +
      # ---------------------------------------
      # fade non significant OR
      # ---------------------------------------
      scale_alpha_manual(values = c(1, .3), guide = FALSE)
    # ---------------------------------------
    # coord flip only works with non-free scales
    # ---------------------------------------
    if (!free.scale) gp <- gp + coord_flip()
    # ---------------------------------------
    # continious scale with log-transform is better
    # to get pretty grid breaks for log scales
    # ---------------------------------------
    if (fun == "glm") {
      gp <- gp + scale_y_continuous(trans = "log10",
                                    breaks = base_breaks(ceiling(max(mydf$upper.CI))),
                                    labels = prettyNum)
    }
    # ---------------------------------------
    # hide error bars (conf int)?
    # ---------------------------------------
    if (!hideErrorBars) {
      gp <- gp +
        geom_errorbar(aes(ymin = lower.CI, ymax = upper.CI), width = 0)
    }
    # ---------------------------------------
    # axis titles
    # ---------------------------------------
    if (type == "fe" || type == "fe.std") {
      if (is.null(axisTitle.x)) axisTitle.x <- ""
      if (is.null(axisTitle.y)) axisTitle.y <- ""
    }
    else if (type == "re") {
      if (is.null(axisTitle.x)) axisTitle.x <- "Group levels"
      if (is.null(axisTitle.y)) axisTitle.y <- ""
    }
    # ---------------------------------------
    # add facet grid here, faceting by group
    # (level) of random intercept
    # ---------------------------------------
    if (facet.grid) {
      gp <- gp +
        labs(x = axisTitle.x, y = axisTitle.y)
      # check if user wants free scale for each facet
      if (free.scale)
        gp  <- gp + facet_wrap( ~ grp, scales = "free_y")
      else
        gp  <- gp + facet_grid( ~ grp)
    }
    else {
      gp <- gp +
        labs(x = axisTitle.x, y = axisTitle.y, title = title)
    }
    return (gp)
  }
  # ---------------------------------------
  # facet grid means, just one plot
  # ---------------------------------------
  if (facet.grid) {
    # ---------------------------------------
    # for random effects, no title is displayed in facet. so
    # tell user via message that random effects are plotted
    # ---------------------------------------
    if (type == "re") message("Plotting random effects...")
    me.plot <- plot.effe(mydf,
                         title,
                         facet.grid,
                         hideErrorBars,
                         interceptLineType,
                         interceptLineColor,
                         fun,
                         type,
                         free.scale)
    # ---------------------------------------------------------
    # set geom colors
    # ---------------------------------------------------------
    me.plot <- sj.setGeomColors(me.plot, geom.colors, 2, FALSE, NULL)
    # ---------------------------------------------------------
    # Check whether ggplot object should be returned or plotted
    # ---------------------------------------------------------
    if (printPlot) print(me.plot)
    me.plot.list <- NULL
  }
  else {
    # ---------------------------------------
    # single plots means, each coefficient is
    # plotted to an own figure
    # ---------------------------------------
    groups <- unique(mydf$grp)
    # ---------------------------------------
    # set title for plots (coefficient label)
    # ---------------------------------------
    if (is.null(title)) title <- paste("Random effects of ", as.character(groups))
    me.plot.list <- list()
    # ---------------------------------------
    # iterate coefficients
    # ---------------------------------------
    for (j in 1 : length(groups)) {
      me.plot <- plot.effe(mydf[mydf$grp == groups[j], ],
                           title[j],
                           facet.grid,
                           hideErrorBars,
                           interceptLineType,
                           interceptLineColor,
                           fun,
                           type,
                           free.scale)
      # ---------------------------------------------------------
      # set geom colors
      # ---------------------------------------------------------
      me.plot <- sj.setGeomColors(me.plot, geom.colors, 2, FALSE, NULL)
      me.plot.list[[length(me.plot.list)+1]]  <- me.plot
      # ---------------------------------------------------------
      # Check whether ggplot object should be returned or plotted
      # ---------------------------------------------------------
      if (printPlot) print(me.plot)
      me.plot <- NULL
    }
  }
  # -------------------------------------
  # return results
  # -------------------------------------
  invisible (structure(class = ifelse (fun == "glm", "sjpglmer", "sjplmer"),
                       list(plot = me.plot,
                            plot.list <- me.plot.list,
                            mydf = mydf)))
}


lme.p <- function(fit) {
  # retrieve sigificance level of independent variables (p-values)
  cs <- coef(summary(fit))
  # check if we have p-values in summary
  if (ncol(cs) >= 4) {
    pv <- cs[, 4]
  }
  else {
    ps <- NULL
  }
  return (ps)
}


sjp.lme.feprobcurv <- function(fit,
                               show.se,
                               facet.grid,
                               vars,
                               printPlot) {
  # ----------------------------
  # prepare additional plots, when metric
  # predictors should also be plotted
  # ----------------------------
  # init lists with all additional data frames and plots
  mydf.metricpred <- list()
  plot.metricpred <- list()
  mydf.facet <- NULL
  plot.facet <- NULL
  axisLabels.mp <- c()
  # ----------------------------
  # retrieve data frame of model to check whether
  # we have any numeric terms in fitted model
  # ----------------------------
  fit.df <- fit@frame
  # ----------------------------
  # retrieve term names, so we find the estimates in the
  # coefficients list
  # ----------------------------
  fit.term.length <- length(names(lme4::fixef(fit))[-1])
  fit.term.names <- na.omit(attr(attr(fit.df, "terms"), "term.labels")[1 : fit.term.length])
  response.name <- attr(attr(attr(fit.df, "terms"), "dataClasses"), "names")[1]
  fi <- unname(lme4::fixef(fit))[1]
  # ----------------------------
  # filter vars?
  # ----------------------------
  if (!is.null(vars)) {
    if (is.character(vars)) {
      fit.term.names <- fit.term.names[!is.na(match(fit.term.names, vars))]
    }
    else {
      fit.term.names <- fit.term.names[vars]
    }
  }
  # ----------------------------
  # plot all terms
  # ----------------------------
  for (i in 1 : length(fit.term.names)) {
    # get values from coefficient
    coef.column <- which(colnames(fit.df) == fit.term.names[i])
    # check if we have found the coefficient
    if (length(coef.column) > 0) {
      vals <- fit.df[, coef.column]
      # find unique values, for x axis
      vals.unique <- sort(vals)
      # melt variable
      mydf.vals <- data.frame(value = vals.unique)
      # convert factor to numeric
      if (is.factor(mydf.vals$value)) mydf.vals$value <- to_value(mydf.vals$value, 0)
      # retrieve names of coefficients
      coef.names <- names(lme4::fixef(fit))
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
      # check if we have found the coefficient
      if (length(coef.pos) > 0) {
        # calculate x-beta by multiplying original values with estimate of that term
        mydf.vals$xbeta <- mydf.vals$value * (lme4::fixef(fit)[coef.pos])
        # calculate probability (y) via cdf-function
        mydf.vals$y <- odds.to.prob(fi + mydf.vals$xbeta)
        # save predictor name
        pred.name <- fit.term.names[i]
        axisLabels.mp <- c(axisLabels.mp, pred.name)
        # assign group
        mydf.vals$grp <- pred.name
        # add mydf to list
        mydf.metricpred[[length(mydf.metricpred) + 1]] <- mydf.vals
      }
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
        labs(x = axisLabels.mp[i], y = "Predicted Probability") +
        stat_smooth(method = "glm", family = "binomial", se = show.se) +
        # cartesian coord still plots range of se, even
        # when se exceeds plot range.
        coord_cartesian(ylim = c(0, 1))
      # add plot to list
      plot.metricpred[[length(plot.metricpred)+1]] <- mp
    }
    # -------------------------------------
    # if we have more than one numeric var, also create integrated plot
    # -------------------------------------
    if (length(mydf.metricpred) > 1) {
      mp <- ggplot(mydf.ges, aes(x = value,
                                 y = y,
                                 colour = grp)) +
        labs(x = NULL,
             y = "Predicted Probability",
             title = "Predicted Probabilities of coefficients") +
#         scale_colour_manual(values = brewer_pal(palette = "Set1")(length(axisLabels.mp)),
#                             labels = axisLabels.mp) +
        stat_smooth(method = "glm", family = "binomial", se = show.se) +
        # cartesian coord still plots range of se, even
        # when se exceeds plot range.
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
  return (structure(class = "sjpglmer.fecc",
          list(mydf.mp = mydf.metricpred,
               plot.mp = plot.metricpred,
               mydf.facet = mydf.facet,
               plot.facet = plot.facet)))
}


sjp.lme.reprobcurve <- function(fit,
                                show.se,
                                facet.grid,
                                ri.nr,
                                vars,
                                printPlot) {
  # ----------------------------
  # retrieve data frame of model to check whether
  # we have any numeric terms in fitted model
  # ----------------------------
  fit.df <- fit@frame
  # ----------------------------
  # retrieve term names, so we find the estimates in the
  # coefficients list
  # ----------------------------
  plot.prob <- list()
  mydf.prob <- list()
  fit.term.length <- length(names(lme4::fixef(fit))[-1])
  fit.term.names <- na.omit(attr(attr(fit.df, "terms"), "term.labels")[1 : fit.term.length])
  response.name <- attr(attr(attr(fit.df, "terms"), "dataClasses"), "names")[1]
  fi <- unname(lme4::fixef(fit))[1]
  # ---------------------------------------
  # check amounnt of random intercepts
  # ---------------------------------------
  if (length(lme4::ranef(fit)) > ri.nr) {
    message("Index specified in 'ri.nr' was larger than amount of random intercepts in model. Paramater 'ri.nr' was set to 1.")
    ri.nr <- 1
  }
  # retrieve random effects
  rand.ef <- lme4::ranef(fit)[[ri.nr]]
  # ----------------------------
  # filter vars?
  # ----------------------------
  if (!is.null(vars)) {
    if (is.character(vars)) {
      fit.term.names <- fit.term.names[!is.na(match(fit.term.names, vars))]
    }
    else {
      fit.term.names <- fit.term.names[vars]
    }
  }
  # ----------------------------
  # loop through all coefficients
  # ----------------------------
  for (i in 1 : length(fit.term.names)) {
    # init lists with all additional data frames and plots
    final.df <- data.frame()
    final.grp <- c()
    # get values from coefficient
    coef.column <- which(colnames(fit.df) == fit.term.names[i])
    # check if we have found the coefficient
    if (length(coef.column) > 0) {
      # get values from each coefficient
      vals <- fit.df[, coef.column]
      # sort values, for x axis
      vals.unique <- sort(vals)
      # melt variable
      mydf.vals <- data.frame(value = vals.unique)
      # convert factor to numeric
      if (is.factor(mydf.vals$value)) mydf.vals$value <- to_value(mydf.vals$value, 0)
      # retrieve names of coefficients
      coef.names <- names(lme4::fixef(fit))
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
      # check if we have found the coefficient
      if (length(coef.pos) > 0) {
        # calculate x-beta by multiplying original values with estimate of that term
        mydf.vals$xbeta <- mydf.vals$value * (lme4::fixef(fit)[coef.pos])
        # save predictor name
        pred.name <- fit.term.names[i]
        # do this for each random intercept group
        for (j in 1 : nrow(rand.ef)) {
          # calculate probability for each random effect group
          mydf.vals$y <- odds.to.prob(fi + rand.ef[j, 1] + mydf.vals$xbeta)
          # add to final data frame
          final.df <- rbind(final.df, cbind(pred = mydf.vals$value,
                                            prob = mydf.vals$y))
          # need to add grp vector later to data frame,
          # else "x" and "prob" would be coerced to factors
          final.grp <- c(final.grp,
                         rep(row.names(rand.ef)[j],
                             times = length(mydf.vals$value)))
        }
        # add grp vector
        final.df$grp <- final.grp
        # ---------------------------------------------------------
        # plot
        # ---------------------------------------------------------
        mp <- ggplot(final.df, aes(x = pred, y = prob, colour = grp)) +
          stat_smooth(method = "glm", family = "binomial", se = show.se) +
          # cartesian coord still plots range of se, even
          # when se exceeds plot range.
          coord_cartesian(ylim = c(0, 1)) +
          labs(x = NULL,
               y = "Predicted Probability",
               title = sprintf("Predicted Probability of %s on %s", pred.name, response.name))
        # wrap to facets
        if (facet.grid) {
          mp <- mp + facet_wrap( ~ grp,
                                 ncol = round(sqrt(nrow(rand.ef))),
                                 scales = "free_x") +
            # no legend
            guides(colour = FALSE)
        }
        # -------------------------------------
        # add to plot and df list
        # -------------------------------------
        plot.prob[[length(plot.prob)+1]] <- mp
        mydf.prob[[length(mydf.prob)+1]] <- final.df
        # -------------------------------------
        # check if metric plots should be plotted
        # -------------------------------------
        if (printPlot) {
          print(mp)
        }
      }
    }
  }
  invisible(structure(class = "sjpglmer.ripc",
                      list(mydf = mydf.prob,
                           plot = plot.prob)))
}


sjp.lme.feri <- function(fit,
                         ri.nr,
                         vars,
                         printPlot) {
  # ----------------------------
  # retrieve term names, so we find the estimates in the
  # coefficients list
  # ----------------------------
  plot.fe <- list()
  mydf.fe <- list()
  all.term.names <- colnames(fit@frame)
  response.name <- all.term.names[1]
  fit.term.names <- names(lme4::fixef(fit))[-1]
  estimates <- unname(lme4::fixef(fit))[-1]
  fi <- unname(lme4::fixef(fit))[1]
  # ---------------------------------------
  # check amounnt of random intercepts
  # ---------------------------------------
  if (length(lme4::ranef(fit)) > ri.nr) {
    message("Index specified in 'ri.nr' was larger than amount of random intercepts in model. Paramater 'ri.nr' was set to 1.")
    ri.nr <- 1
  }
  # retrieve random effects
  rand.ef <- lme4::ranef(fit)[[ri.nr]]
  # ----------------------------
  # filter vars?
  # ----------------------------
  if (!is.null(vars)) {
    if (is.character(vars)) {
      removers <- !is.na(match(fit.term.names, vars))
    }
    else {
      removers <- vars
    }
    fit.term.names <- fit.term.names[removers]
    estimates <- estimates[removers]
  }
  # ----------------------------
  # loop through all coefficients
  # ----------------------------
  # slopes for all fixed effects
  for (j in 1 : length(estimates)) {
    # reset data frame
    final.df <- data.frame()
    # slopes for each random intercept
    for (i in 1 : nrow(rand.ef)) {
      # retrieve intercept
      ri <- rand.ef[i, 1]
      xpos <- NULL
      # find original values for estimates
      for (k in 1 : length(all.term.names)) {
        # check if estimate's name matches any column
        # in the data frame of the fitted model
        pos <- grep(all.term.names[k], fit.term.names[j], fixed = T)
        # found?
        if (length(pos) > 0) {
          xpos <- sort(unique(fit@frame[, k]))
          break
        }
      }
      # check if we found any values...
      if (!is.null(xpos)) {
        final.df <- rbind(final.df,
                          cbind(x = as.numeric(as.character(xpos)),
                                y = fi + ri + (as.numeric(as.character(xpos)) * estimates[j]),
                                grp = i))
      }
    }
    # comvert grouping level to factor
    final.df$grp <- as.factor(final.df$grp)
    # retrieve group level label
    levels(final.df$grp)  <- row.names(rand.ef)
    gp <- ggplot(final.df, aes(x = x, y = y, colour = grp)) + 
      geom_line() +
      xlab(fit.term.names[j]) +
      ylab(response.name)
    # -------------------------------------
    # add to plot and df list
    # -------------------------------------
    plot.fe[[length(plot.fe)+1]] <- gp
    mydf.fe[[length(mydf.fe)+1]] <- final.df
    # -------------------------------------
    # check if metric plots should be plotted
    # -------------------------------------
    if (printPlot) {
      print(gp)
    }
  }
  invisible(structure(class = "sjplmer.feri",
                      list(mydf = mydf.fe,
                           plot = plot.fe)))
}


# ---------------------------------------
# Thanks to Robert Reijntjes from
# Leiden University Medical Center
# for providing the core code snipptes,
# which are used in this function
# ---------------------------------------
sjp.lme.reqq <- function(fit,
                         geom.colors,
                         geom.size,
                         hideErrorBars,
                         interceptLineType,
                         interceptLineColor,
                         fun,
                         printPlot) {
  re   <- lme4::ranef(fit, condVar = T)[[1]]
  pv   <- attr(re, "postVar")
  cols <- 1 : (dim(pv)[1])
  se   <- unlist(lapply(cols, function(i) sqrt(pv[i, i, ])))
  ord  <- unlist(lapply(re, order)) + rep((0 : (ncol(re) - 1)) * nrow(re), each=nrow(re))
  pDf  <- data.frame(y = unlist(re)[ord],
                     ci = 1.96 * se[ord],
                     nQQ = rep(qnorm(ppoints(nrow(re))), ncol(re)),
                     ID = factor(rep(rownames(re), ncol(re))[ord], levels = rownames(re)[ord]),
                     ind = gl(ncol(re), nrow(re), labels = names(re)),
                     grp = "1")
  gp <- ggplot(pDf, aes(nQQ, y, colour = grp)) +
    facet_wrap(~ ind, scales = "free") +
    xlab("Standard normal quantiles") +
    ylab("Random effect quantiles") +
    # Intercept-line
    geom_hline(yintercept = 0,
               linetype = interceptLineType,
               color = interceptLineColor)
  # ---------------------------------------
  # hide error bars (conf int)?
  # ---------------------------------------
  if (!hideErrorBars) {
    gp <- gp +
      geom_errorbar(aes(ymin = y - ci, ymax = y + ci), width = 0, colour = "black")
  }
  # ---------------------------------------
  # plot points and interceot
  # ---------------------------------------
  gp <- gp +
    stat_smooth(method = "lm") +
    geom_point(size = geom.size)
  # ---------------------------------------------------------
  # set geom colors
  # ---------------------------------------------------------
  gp <- sj.setGeomColors(gp, geom.colors, 1, FALSE, NULL)
  # ---------------------------------------------------------
  # print plot?
  # ---------------------------------------------------------
  if (printPlot) {
    message("Testing for normal distribution. Dots should be plotted along the line.")
    print(gp)
  }
  # -------------------------------------
  # return results
  # -------------------------------------
  return (invisible(structure(class = ifelse (fun == "glm", "sjpglmer.qq", "sjplmer.qq"),
                              list(plot = gp,
                                   mydf = pDf))))
}


# ---------------------------------------
# Thanks to Robert Reijntjes from
# Leiden University Medical Center
# for providing the core code snipptes,
# which are used in this function
# ---------------------------------------
sjp.lme.fecor <- function(fit,
                          pred.labels,
                          sort.coef,
                          fun,
                          printPlot) {
  # ---------------------------------------
  # copy rownames as axis labels, if not set
  # ---------------------------------------
  if (is.null(pred.labels)) {
    pred.labels <- names(lme4::fixef(fit))
  }
  else {
    pred.labels <- c("(Intercept)", pred.labels)
  }
  # ---------------------------------------
  so <- summary(fit)
  mydf <- tryCatch(
    mydf <- as.matrix(cov2cor(as.matrix(vcov(fit)))),
    error = function(cond) { mydf <- as.matrix(so$vcov@factors$correlation) }
  )
  rownames(mydf) <- pred.labels
  colnames(mydf) <- pred.labels
  # fix sort-parameter
  if (!is.null(sort.coef) && sort.coef != TRUE)
    sort.coef <- FALSE
  else
    sort.coef <- TRUE
  # ---------------------------------------
  # return correlation plot
  # ---------------------------------------
  corret <- sjp.corr(as.matrix(mydf),
                     sortCorrelations = sort.coef,
                     axisLabels = pred.labels)
  return (invisible(structure(class = ifelse (fun == "glm", "sjpglmer.cor", "sjplmer.cor"),
                              list(plot = corret$plot,
                                   mydf = corret$df,
                                   corr.matrix = corret$corr.matrix))))
}



sjp.lme.fecondpred.onlynumeric <- function(fit,
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
  mydf.facet <- NULL
  plot.facet <- NULL
  axisLabels.mp <- c()
  # ----------------------------
  # retrieve data frame of model to check whether
  # we have any numeric terms in fitted model
  # ----------------------------
  fit.df<- fit@frame
  # ----------------------------
  # retrieve term names, so we find the estimates in the
  # coefficients list
  # ----------------------------
  fit.term.names <- names(lme4::fixef(fit))[-1]
  # ----------------------------
  # find amount of numeric terms
  # ----------------------------
  findnumeric <- c()
  for (i in 1 : length(fit.term.names)) {
    if (class(fit.df[, fit.term.names[i]]) == "numeric")
      findnumeric <- c(findnumeric, which(colnames(fit.df) == fit.term.names[i]))
  }
  # ----------------------------
  # check if we have any numeric predictors
  # ----------------------------
  if (length(findnumeric > 0)) {
    # loop through all numeric termns
    for (i in 1 : length(findnumeric)) {
      # get values from numeric term
      vals <- fit.df[, findnumeric[i]]
      # find unique values, for x axis
      vals.unique <- sort(unique(vals))
      # melt variable
      mydf.vals <- data.frame(value = vals.unique)
      mydf.vals$x <- seq(from = 1, to = nrow(mydf.vals), by = 1)
      # retrieve names of coefficients
      coef.names <- names(lme4::fixef(fit))
      # find coef-position
      coef.pos <- which(coef.names==fit.term.names[i])
      # calculate x-beta by multiplying original values with estimate of that term
      mydf.vals$xbeta <- mydf.vals$value * (lme4::fixef(fit)[coef.pos])
      # calculate probability (y) via cdf-function
      mydf.vals$y <- odds.to.prob(lme4::fixef(fit)[1] + mydf.vals$xbeta)
      # save predictor name
      pred.name <- colnames(fit.df)[findnumeric[i]]
      axisLabels.mp <- c(axisLabels.mp, pred.name)
      # assign group
      mydf.vals$grp <- pred.name
      # add mydf to list
      mydf.metricpred[[length(mydf.metricpred) + 1]] <- mydf.vals
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
        geom_point() +
        labs(x = axisLabels.mp[i], y = "Probability") +
        stat_smooth(method = "glm", family = "binomial", se = show.se) +
        # cartesian coord still plots range of se, even
        # when se exceeds plot range.
        coord_cartesian(ylim = c(0, 1))
      # add plot to list
      plot.metricpred[[length(plot.metricpred)+1]] <- mp
    }
    # -------------------------------------
    # if we have more than one numeric var, also create integrated plot
    # -------------------------------------
    if (length(mydf.metricpred) > 1) {
      mp <- ggplot(mydf.ges, aes(x = value,
                                 y = y,
                                 colour = grp)) +
        geom_point() +
        labs(x = NULL,
             y = "Probability",
             title = "Probability of coefficients") +
        #         scale_colour_manual(values = brewer_pal(palette = "Set1")(length(axisLabels.mp)),
        #                             labels = axisLabels.mp) +
        stat_smooth(method = "glm", family = "binomial", se = show.se) +
        # cartesian coord still plots range of se, even
        # when se exceeds plot range.
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
  return (structure(class = "sjpglmer.fecc",
                    list(mydf.mp = mydf.metricpred,
                         plot.mp = plot.metricpred,
                         mydf.facet = mydf.facet,
                         plot.facet = plot.facet)))
}
