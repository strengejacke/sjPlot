#' @title Plot interaction effects of (generalized) linear (mixed) models
#' @name sjp.int
#'
#' @references \itemize{
#'              \item Aiken and West (1991). Multiple Regression: Testing and Interpreting Interactions.
#'              \item Brambor T, Clark WR and Golder M (2006) Understanding Interaction Models: Improving Empirical Analyses. Political Analysis 14: 63-82 \href{https://files.nyu.edu/mrg217/public/pa_final.pdf}{download}
#'              \item Esarey J, Sumner JL (2015) Marginal Effects in Interaction Models: Determining and Controlling the False Positive Rate. \href{http://jee3.web.rice.edu/interaction-overconfidence.pdf}{download}
#'              \item Fox J (2003) Effect displays in R for generalised linear models. Journal of Statistical Software 8:15, 1–27, \href{http://www.jstatsoft.org/v08/i15/}{<http://www.jstatsoft.org/v08/i15/>}
#'              \item Hayes AF (2012) PROCESS: A versatile computational tool for observed variable mediation, moderation, and conditional process modeling [White paper] \href{http://imaging.mrc-cbu.cam.ac.uk/statswiki/FAQ/SobelTest?action=AttachFile&do=get&target=process.pdf}{download}
#'              \item \href{http://www.theanalysisfactor.com/interpreting-interactions-in-regression/}{Grace-Martin K: Interpreting Interactions in Regression}
#'              \item \href{http://www.theanalysisfactor.com/clarifications-on-interpreting-interactions-in-regression/}{Grace-Martin K: Clarifications on Interpreting Interactions in Regression}
#'              \item \href{http://www.theanalysisfactor.com/3-tips-interpreting-moderation/}{Grace-Martin K: 3 Tips to Make Interpreting Moderation Effects Easier}
#'              \item \href{http://www.theanalysisfactor.com/using-adjusted-means-to-interpret-moderators-in-analysis-of-covariance/}{Grace-Martin K: Using Adjusted Means to Interpret Moderators in Analysis of Covariance.}
#'              }
#'
#' @seealso \href{http://www.strengejacke.de/sjPlot/sjp.int/}{sjPlot manual: sjp.int}
#'
#' @description Plot regression (predicted values) or probability lines (predicted probabilities) of 
#'                significant interaction terms to better understand effects
#'                of moderations in regression models. This function accepts following fitted model classes:
#'                \itemize{
#'                  \item linear models (\code{\link{lm}})
#'                  \item generalized linear models (\code{\link{glm}})
#'                  \item linear mixed effects models (\code{\link[lme4]{lmer}})
#'                  \item generalized linear mixed effects models (\code{\link[lme4]{glmer}})
#'                  \item non-linear mixed effects models (\code{\link[lme4]{nlmer}})
#'                  \item linear mixed effects models (\code{\link[nlme]{lme}}, but only for \code{type = "eff"})
#'                  \item generalized least squares models (\code{\link[nlme]{gls}}, but only for \code{type = "eff"})
#'                  \item panel data estimators (\code{plm})
#'                }
#'                Note that beside interaction terms, also the single predictors of each interaction (main effects)
#'                must be included in the fitted model as well. Thus, \code{lm(dep ~ pred1 * pred2)} will work, 
#'                but \code{lm(dep ~ pred1:pred2)} won't!
#'
#' @param fit the fitted (generalized) linear (mixed) model object, including interaction terms. Accepted model
#'          classes are
#'          \itemize{
#'            \item linear models (\code{\link{lm}})
#'            \item generalized linear models (\code{\link{glm}})
#'            \item linear mixed effects models (\code{\link[lme4]{lmer}})
#'            \item generalized linear mixed effects models (\code{\link[lme4]{glmer}})
#'            \item non-linear mixed effects models (\code{\link[lme4]{nlmer}})
#'            \item linear mixed effects models (\code{\link[nlme]{lme}}, but only for \code{type = "eff"})
#'            \item generalized least squares models (\code{\link[nlme]{gls}}, but only for \code{type = "eff"})
#'            \item panel data estimators (\code{plm})
#'          }
#' @param type interaction plot type. Use one of following values:
#'          \describe{
#'            \item{\code{type = "cond"}}{(default) plots the mere change of the moderating effect on the response value (conditional effect). See 'Details'.}
#'            \item{\code{type = "eff"}}{plots the overall moderation effect on the response value. See 'Details'.}
#'            \item{\code{type = "emm"}}{plots the estimated marginal means (least square means). If this type is chosen, not all function arguments are applicable. See 'Details'.}
#'          }
#' @param int.term select interaction term of \code{fit} (as character), which should be plotted
#'          when using \code{type = "eff"}. By default, this argument can be ignored
#'          (i.e. \code{int.term = NULL}). See 'Details'.
#' @param int.plot.index numeric vector with index numbers that indicate which 
#'          interaction terms should be plotted in case the \code{fit} has more than
#'          one interaction. By default, this values is \code{NULL}, hence all interactions
#'          are plotted.
#' @param diff if \code{FALSE} (default), the minimum and maximum interaction effects of the moderating variable
#'          is shown (one line each). if \code{TRUE}, only the difference between minimum and maximum interaction effect
#'          is shown (single line). Only applies to \code{type = "cond"}.
#' @param moderatorValues indicates which values of the moderator variable should be used when plotting the effects of the
#'          independent variable on the dependent variable.
#'          \describe{
#'            \item{\code{"minmax"}}{(default) minimum and maximum values (lower and upper bounds) of the moderator are used to plot the interaction between independent variable and moderator.}
#'            \item{\code{"meansd"}}{uses the mean value of the moderator as well as one standard deviation below and above mean value to plot the effect of the moderator on the independent variable (following the convention suggested by Cohen and Cohen and popularized by Aiken and West, i.e. using the mean, the value one standard deviation above, and the value one standard deviation below the mean as values of the moderator, see \href{http://www.theanalysisfactor.com/3-tips-interpreting-moderation/}{Grace-Martin K: 3 Tips to Make Interpreting Moderation Effects Easier}).}
#'            \item{\code{"zeromax"}}{is similar to the \code{"minmax"} option, however, \code{0} is always used as minimum value for the moderator. This may be useful for predictors that don't have an empirical zero-value, but absence of moderation should be simulated by using 0 as minimum.}
#'            \item{\code{"quart"}}{calculates and uses the quartiles (lower, median and upper) of the moderator value.}
#'          }
#' @param swapPredictors if \code{TRUE}, the predictor on the x-axis and the moderator value in an interaction are
#'          swapped. For \code{type = "eff"}, the first interaction term is used as moderator and the second term
#'          is plotted at the x-axis. For \code{type = "cond"}, the interaction's predictor with less unique values is 
#'          printed along the x-axis. Default is \code{FALSE}, so the second predictor in an interaction, respectively 
#'          the predictor with more unique values is printed along the x-axis.
#' @param plevel indicates at which p-value an interaction term is considered as \emph{significant},
#'          i.e. at which p-level an interaction term will be considered for plotting. Default is
#'          0.05 (5 percent), hence, non-significant interactions are excluded by default. This
#'          argument does not apply to \code{type = "eff"}.
#' @param title a default title used for the plots. Should be a character vector
#'          of same length as interaction plots to be plotted. Default value is \code{NULL}, which means that each plot's title
#'          includes the dependent variable as well as the names of the interaction terms.
#' @param fillColor fill color of the shaded area between the minimum and maximum lines. Default is \code{"grey"}.
#'          Either set \code{fillColor} to \code{NULL} or use 0 for \code{fillAlpha} if you want to hide the shaded area.
#' @param fillAlpha alpha value (transparancy) of the shaded area between the minimum and maximum lines. Default is 0.4.
#'          Use either 0 or set \code{fillColor} to \code{NULL} if you want to hide the shaded area.
#' @param geom.colors vector of color values. First value is the color of the line indicating the lower bound of
#'          the interaction term (moderator value). Second value is the color of the line indicating the upper bound of
#'          the interaction term (moderator value). Third value, if applicable, is the color of the line indicating the
#'          mean value of the interaction term (moderator value). Third value is only used when 
#'          \code{moderatorValues = "meansd"}. Or, if \code{diff = TRUE}, only one color value for the 
#'          line indicating the upper difference between lower and upper bound of interaction terms.
#' @param axisTitle.x a default title used for the x-axis. Should be a character vector
#'          of same length as interaction plots to be plotted. Default value is \code{NULL},
#'          which means that each plot's x-axis uses the predictor's name as title.
#' @param axisTitle.y a default title used for the y-axis. Default value is \code{NULL},
#'          which means that each plot's y-axis uses the dependent variable's name as title.
#' @param axisLabels.x character vector with value labels of the repeated measure variable
#'          that are used for labelling the x-axis.
#' @param legendTitle title of the diagram's legend. A character vector of same length as 
#'          amount of interaction plots to be plotted (i.e. one vector element for each
#'          plot's legend title).
#' @param legendLabels labels for the guide/legend. Either a character vector of same length as
#'          amount of legend labels of the plot, or a \code{list} of character vectors, if more than one
#'          interaction plot is plotted (i.e. one vector of legend labels for each interaction plot).
#'          Default is \code{NULL}, so the name of the predictor with min/max-effect is used 
#'          as legend label.
#' @param showValueLabels if \code{TRUE}, value labels are plotted along the lines. Default is \code{FALSE}.
#' @param breakAnnotationLabelsAt Wordwrap for diagram annotation labels. Determines how many chars of the legend labels are
#'          displayed in one line and when a line break is inserted. Default is \code{50}.
#'          Only applies if \code{showInterceptLine} is \code{TRUE}.
#' @param showInterceptLines If \code{TRUE}, the intercept and the estimate of the predictor
#'          (reference category of predictor in case interaction is not present) are plotted.
#' @param showInterceptLabels If \code{TRUE} (default), the intercept lines are labelled. Only
#'          applies if \code{showInterceptLines} is \code{TRUE}.
#' @param showCI If \code{TRUE}, a confidence region will be plotted. Onyl applies
#'          to \code{type = "emm"} or \code{type = "eff"}.
#' @param valueLabel.digits the amount of digits of the displayed value labels. Defaults to 2.
#' @param interceptLineColor color of the model's intercept line. Only applies, if
#'          \code{showInterceptLines} is \code{TRUE}.
#' @param estLineColor color of the model's predictor's estimate line. Only applies, if
#'          \code{showInterceptLines} is \code{TRUE}.
#' @param lineLabelSize size of the intercept line annotations inside the plot. Only applies
#'          if \code{showInterceptLines} is \code{TRUE}. Default is 3.7.
#' @param lineLabelColor color of the intercept line annotations inside the plot. Only applies
#'          if \code{showInterceptLines} is \code{TRUE}. Default is \code{"black"}.
#' @param lineLabelString string for the intercept lines that is appended to the predictor
#'          variable name. By default, this string is \code{"(no interaction)"}.
#' @param facet.grid \code{TRUE} for faceted plots instead of an integrated single plot.
#' 
#' @inheritParams sjp.grpfrq
#' 
#' @return (Insisibily) returns the ggplot-objects with the complete plot-list (\code{plot.list})
#'           as well as the data frame that were used for setting up the ggplot-objects (\code{df.list}).
#'
#' @details \describe{
#'            \item{\code{type = "cond"}}{plots the effective \emph{change} or \emph{impact} 
#'              (conditional effect) on a dependent variable of a moderation effect, as 
#'              described in \href{http://www.theanalysisfactor.com/clarifications-on-interpreting-interactions-in-regression/}{Grace-Martin},
#'              i.e. the difference of the moderation effect on the dependent variable in \emph{presence}
#'              and \emph{absence} of the moderating effect (\emph{simple slope} plot or 
#'              \emph{conditional effect}, see \href{http://imaging.mrc-cbu.cam.ac.uk/statswiki/FAQ/SobelTest?action=AttachFile&do=get&target=process.pdf}{Hayes 2012}).
#'              Hence, this plot type may be used especially for \emph{binary or dummy coded} 
#'              moderator values (see also \href{http://jee3.web.rice.edu/interaction-overconfidence.pdf}{Esarey and Summer 2015}).
#'              This type \emph{does not} show the overall effect of interactions on the result of Y. Use 
#'              \code{type = "eff"} for effect displays similar to the \code{\link[effects]{effect}}-function 
#'              from the \pkg{effects}-package.
#'            }
#'            \item{\code{type = "eff"}}{plots the overall effects (marginal effects) of the interaction, with all remaining
#'              covariates set to the mean. Effects are calculated using the \code{\link[effects]{effect}}-
#'              function from the \pkg{effects}-package. \cr \cr
#'              Following arguments \emph{do not} apply to this function: \code{diff}, \code{axisLabels.x}
#'              \code{interceptLineColor}, \code{estLineColor}, \code{lineLabelSize}, \code{lineLabelColor} 
#'              and \code{lineLabelString}.
#'            }
#'            \item{\code{type = "emm"}}{plots the estimated marginal means of repeated measures designs,
#'              like two-way repeated measures AN(C)OVA. In detail, this type plots estimated marginal means 
#'              (also called \emph{least square means} or \emph{marginal means}) of (significant) interaction terms.
#'              The fitted models may be linear (mixed effects) 
#'              models of class \code{\link{lm}} or \code{\link[lme4]{merMod}}. This function may be used, for example,
#'              to plot differences in interventions between control and treatment groups over multiple time points.
#'              \itemize{
#'                \item Following paramters apply to this plot type: \code{showCI}, \code{valueLabel.digits} and \code{axisLabels.x}.
#'                \item Following arguments \emph{do not} apply to this function: \code{int.term}, \code{int.plot.index}, \code{diff}, \code{moderatorValues}, \code{fillColor}, \code{fillAlpha}, \code{interceptLineColor}, \code{estLineColor}, \code{lineLabelSize}, \code{lineLabelColor} and \code{lineLabelString}.
#'              }
#'            }
#'          }
#'          The argument \code{int.term} only applies to \code{type = "eff"} and can be used
#'          to select a specific interaction term of the model that should be plotted. The function
#'          then calls \code{effect(int.term, fit)} to compute effects for this specific interaction
#'          term only. This approach is recommended, when the fitted model contains many observations
#'          and/or variables, which may slow down the effect-computation dramatically. In such cases,
#'          consider computing effects for selected interaction terms only with \code{int.terms}.
#'          See 'Examples'.
#'
#' @note Note that beside interaction terms, also the single predictors of each interaction (main effects)
#'        must be included in the fitted model as well. Thus, \code{lm(dep ~ pred1 * pred2)} will work, 
#'        but \code{lm(dep ~ pred1:pred2)} won't! \cr \cr
#'        For \code{type = "emm"}, all interaction terms have to be \code{\link{factor}}s!
#'        Furthermore, for \code{type = "eff"}, predictors of interactions that are introduced first into the model
#'        are used as grouping variable, while the latter predictor is printed along the x-axis
#'        (i.e. lm(y~a+b+a:b) means that "a" is used as grouping variable and "b" is plotted along the x-axis).
#'
#' @examples
#' # Note that the data sets used in this example may not be perfectly suitable for
#' # fitting linear models. I just used them because they are part of the R-software.
#'
#' # fit "dummy" model. Note that moderator should enter
#' # first the model, followed by predictor. Else, use
#' # argument "swapPredictors" to change predictor on 
#' # x-axis with moderator
#' fit <- lm(weight ~ Diet * Time, data = ChickWeight)
#'
#' # show summary to see significant interactions
#' summary(fit)
#'
#' # plot regression line of interaction terms, including value labels
#' sjp.int(fit, type = "eff", showValueLabels = TRUE)
#'
#'
#' # load sample data set
#' library(sjmisc)
#' data(efc)
#' # create data frame with variables that should be included
#' # in the model
#' mydf <- data.frame(usage = efc$tot_sc_e,
#'                    sex = efc$c161sex,
#'                    education = efc$c172code,
#'                    burden = efc$neg_c_7,
#'                    dependency = efc$e42dep)
#' # convert gender predictor to factor
#' mydf$sex <- relevel(factor(mydf$sex), ref = "2")
#' # fit "dummy" model
#' fit <- lm(usage ~ .*., data = mydf)
#' summary(fit)
#'
#' # plot interactions. note that type = "cond" only considers 
#' # significant interactions by default. use "plevel" to 
#' # adjust p-level sensivity
#' sjp.int(fit, type = "cond")
#' 
#' # plot only selected interaction term for
#' # type = "eff"
#' sjp.int(fit, type = "eff", int.term = "sex*education")
#'
#' # plot interactions, using mean and sd as moderator
#' # values to calculate interaction effect
#' sjp.int(fit, type = "eff", moderatorValues = "meansd")
#' sjp.int(fit, type = "cond", moderatorValues = "meansd")
#'
#' # plot interactions, including those with p-value up to 0.1
#' sjp.int(fit,
#'         type = "cond",
#'         plevel = 0.1,
#'         showInterceptLines = TRUE)
#'
#' # -------------------------------
#' # Predictors for negative impact of care.
#' # Data from the EUROFAMCARE sample dataset
#' # -------------------------------
#' library(sjmisc)
#' data(efc)
#' # create binary response
#' y <- ifelse(efc$neg_c_7 < median(stats::na.omit(efc$neg_c_7)), 0, 1)
#' # create data frame for fitted model
#' mydf <- data.frame(y = as.factor(y),
#'                    sex = as.factor(efc$c161sex),
#'                    barthel = as.numeric(efc$barthtot))
#' # fit model
#' fit <- glm(y ~ sex * barthel,
#'            data = mydf,
#'            family = binomial(link = "logit"))
#' # plot interaction, increase p-level sensivity
#' sjp.int(fit,
#'         type = "eff",
#'         legendLabels = get_labels(efc$c161sex),
#'         plevel = 0.1)
#'
#' sjp.int(fit,
#'         type = "cond",
#'         legendLabels = get_labels(efc$c161sex),
#'         plevel = 0.1)
#'         
#' \dontrun{
#' # -------------------------------
#' # Plot estimated marginal means
#' # -------------------------------
#' # load sample data set
#' library(sjmisc)
#' data(efc)
#' # create data frame with variables that should be included
#' # in the model
#' mydf <- data.frame(burden = efc$neg_c_7,
#'                    sex = efc$c161sex,
#'                    education = efc$c172code)
#' # convert gender predictor to factor
#' mydf$sex <- factor(mydf$sex)
#' mydf$education <- factor(mydf$education)
#' # name factor levels and dependent variable
#' levels(mydf$sex) <- c("female", "male")
#' levels(mydf$education) <- c("low", "mid", "high")
#' mydf$burden <- set_label(mydf$burden, "care burden")
#' # fit "dummy" model
#' fit <- lm(burden ~ .*., data = mydf)
#' summary(fit)
#'
#' # plot marginal means of interactions, no interaction found
#' sjp.int(fit, type = "emm")
#' # plot marginal means of interactions, including those with p-value up to 1
#' sjp.int(fit, type = "emm", plevel = 1)
#' # swap predictors
#' sjp.int(fit, 
#'         type = "emm",
#'         plevel = 1, 
#'         swapPredictors = TRUE)
#'
#' # -------------------------------
#' # Plot effects
#' # -------------------------------
#' # add continuous variable
#' mydf$barthel <- efc$barthtot
#' # re-fit model with continuous variable
#' fit <- lm(burden ~ .*., data = mydf)
#' 
#' # plot effects
#' sjp.int(fit, type = "eff", showCI = TRUE)
#'
#' # plot effects, faceted
#' sjp.int(fit, 
#'         type = "eff", 
#'         int.plot.index = 3,
#'         showCI = TRUE,
#'         facet.grid = TRUE)}
#'
#' @import ggplot2
#' @import sjmisc
#' @export
sjp.int <- function(fit,
                    type = "cond",
                    int.term = NULL,
                    int.plot.index = NULL,
                    diff = FALSE,
                    moderatorValues = "minmax",
                    swapPredictors = FALSE,
                    plevel = 0.05,
                    title = NULL,
                    fillColor = "grey",
                    fillAlpha = 0.3,
                    geom.colors = "Set1",
                    axisTitle.x = NULL,
                    axisTitle.y = NULL,
                    axisLabels.x = NULL,
                    legendTitle = NULL,
                    legendLabels = NULL,
                    showValueLabels = FALSE,
                    breakTitleAt = 50,
                    breakLegendLabelsAt = 20,
                    breakLegendTitleAt = 20,
                    breakAnnotationLabelsAt = 50,
                    axisLimits.x = NULL,
                    axisLimits.y = NULL,
                    gridBreaksAt = NULL,
                    showInterceptLines = FALSE,
                    showInterceptLabels = TRUE,
                    showCI = FALSE,
                    valueLabel.digits = 2,
                    interceptLineColor = "darkseagreen4",
                    estLineColor = "darkslategray4",
                    lineLabelSize = 3.7,
                    lineLabelColor = "black",
                    lineLabelString = "(no interaction)",
                    facet.grid = FALSE,
                    printPlot = TRUE) {
  # -----------------------------------------------------------
  # check class of fitted model
  # -----------------------------------------------------------
  c.f <- class(fit)
  fun <- "lm"
  stat.fun <- "lm"
  if (any(c.f == "glm")) {
    fun <- "glm"
    stat.fun <- "glm"
  } else if (any(c.f == "lm")) {
    fun <- "lm"
    stat.fun <- "lm"
  } else if (any(c.f == "plm")) {
    fun <- "plm"
    stat.fun <- "lm"
  } else if (any(c.f == "glmerMod")) {
    fun <- "glmer"
    stat.fun <- "glm"
  } else if (any(c.f == "nlmerMod")) {
    fun <- "nlmer"
    stat.fun <- "nlm"
  } else if (any(c.f == "lmerMod") || any(c.f == "merModLmerTest")) {
    fun <- "lmer"
    stat.fun <- "lm"
  } else if (any(c.f == "lme")) {
    fun <- "lme"
    stat.fun <- "lm"
    if (type != "eff") {
      message("Only 'type = \"eff\"' supports objects of class 'nlme::lme'. Defaulting type to \"eff\".")
      type <- "eff"
    }
  } else if (any(c.f == "gls")) {
    fun <- "gls"
    stat.fun <- "lm"
    if (type != "eff") {
      message("Only 'type = \"eff\"' supports objects of class 'nlme::gls'. Defaulting type to \"eff\".")
      type <- "eff"
    }
  }
  # ------------------------
  # check if suggested package is available
  # ------------------------
  if ((fun == "lmer" || fun == "glmer" || fun == "nlmer") && !requireNamespace("lme4", quietly = TRUE)) {
    stop("Package 'lme4' needed for this function to work. Please install it.", call. = FALSE)
  }
  if (fun == "plm" && !"package:plm" %in% search()) {
    stop("Package 'plm' needs to be loaded for this function to work... Use 'library(plm)' and call this function again.", call. = FALSE)
  }
  # -----------------------------------------------------------
  # argument check
  # -----------------------------------------------------------
  if (is.null(fillColor)) {
    fillColor <- "white"
    fillAlpha <- 0
  }
  # gridbreaks
  if (is.null(gridBreaksAt)) gridbreaks.x <- gridbreaks.y <- ggplot2::waiver()
  # moderator value
  if (moderatorValues != "minmax" && moderatorValues != "zeromax" && moderatorValues != "meansd" && moderatorValues != "quart") {
    message("'moderatorValues' has to be one of 'minmax', 'zeromax', 'quart' or 'meansd'. Defaulting to 'minmax'...")
    moderatorValues <- "minmax"
  }
  # check plot type
  if (type != "cond" && type != "emm" && type != "eff") {
    message("'type' has to be one of 'cond', 'eff' or 'emm'. Defaulting to 'cond'...")
    type <- "cond"
  }
  # --------------------------------------------------------
  # plot estimated marginal means?
  # --------------------------------------------------------
  if (type == "emm") {
    return(sjp.emm(fit, swapPredictors, plevel, title, geom.colors,
                   axisTitle.x, axisTitle.y, axisLabels.x, legendTitle, legendLabels,
                   showValueLabels, valueLabel.digits, showCI, breakTitleAt,
                   breakLegendTitleAt, breakLegendLabelsAt, axisLimits.y, 
                   gridBreaksAt, facet.grid, printPlot))
  }
  # --------------------------------------------------------
  # list labels
  # --------------------------------------------------------
  if (!is.null(legendLabels) && !is.list(legendLabels)) legendLabels <- list(legendLabels)
  if (!is.null(legendTitle) && is.list(legendTitle)) legendTitle <- unlist(legendTitle)
  # --------------------------------------------------------
  # plot moderation effeczs?
  # --------------------------------------------------------
  if (type == "eff") {
    return(sjp.eff.int(fit, int.term, int.plot.index, moderatorValues, swapPredictors, plevel,
                       title, fillAlpha, geom.colors, axisTitle.x,
                       axisTitle.y, legendTitle, legendLabels,
                       showValueLabels, breakTitleAt, breakLegendLabelsAt, 
                       breakLegendTitleAt, breakAnnotationLabelsAt, axisLimits.x,
                       axisLimits.y, gridBreaksAt, showCI, facet.grid, printPlot, fun))
  }
  # -----------------------------------------------------------
  # set axis title
  # -----------------------------------------------------------
  if ((fun == "glm" || fun == "glmer") && is.null(axisTitle.y)) axisTitle.y <- "Change in Predicted Probability"
  # -----------------------------------------------------------
  # get all (significant) interaction terms from model
  # the function "getInteractionTerms" checks if a fitted
  # model contains any interaction terms that are significant
  # at the level specified by "plevel". returns NULL, if model
  # contains no interaction terms or no significant interaction term.
  # else, information on model and interaction terms is returned
  # -----------------------------------------------------------
  git <- getInteractionTerms(fit, fun, plevel)
  # check return value
  if (is.null(git)) return(invisible(NULL))
  # -----------------------------------------------------------
  # init variables from return values
  # -----------------------------------------------------------
  b0 <- git[["b0"]]
  estimates.names <- git[["estimates.names"]]
  estimates <- git[["estimates"]]
  fitdat <- git[["fitdat"]]
  # init vector that saves ggplot objects
  plotlist <- list()
  dflist <- list()
  # -----------------------------------------------------------
  # when we have linear mixed effects models and both interaction 
  # terms are factors, we may have the same interaction term names
  # multiples times - thus, remove redundant duplicates
  # -----------------------------------------------------------
  intnames <- unique(git[["intnames"]])
  # check if we have selected plots only, and remove any plots
  # that should not be plotted. but be careful for out of bound index!
  if (!is.null(int.plot.index) && !any(int.plot.index > length(intnames))) intnames <- intnames[int.plot.index]
  # -----------------------------------------------------------
  # Now iterate all significant interaction terms
  # and manually calculate the linear regression by inserting
  # the estimates of each term and the associated interaction term,
  # i.e.: y = b0 + (b1 * pred1) + (b2 * pred2) + (b3 * pred1 * pred2)
  # -----------------------------------------------------------
  for (cnt in 1:length(intnames)) {
    # -----------------------------------------------------------
    # first, retrieve and split interaction term so we know
    # the two predictor variables of the interaction term
    # -----------------------------------------------------------
    interactionterms <- unlist(strsplit(intnames[cnt], ":"))
    labx <- c()
    # Label on y-axis is name of dependent variable
    laby <- paste0("Change in ", git[["depvar.label"]])
    # -----------------------------------------------------------
    # find estimates (beta values) for each single predictor of
    # the interaction as well as of the interaction term
    # -----------------------------------------------------------
    b1 <- as.numeric(estimates[match(interactionterms[1], estimates.names)])
    b2 <- as.numeric(estimates[match(interactionterms[2], estimates.names)])
    b3 <- as.numeric(estimates[match(intnames[cnt], estimates.names)])
    # -----------------------------------------------------------
    # check whether each predictor was included in the model
    # as single term as well
    # -----------------------------------------------------------
    if (is.na(b1) || is.na(b2) || is.na(b3)) {
      stop("Predictors of interaction terms (main effects) must be included as single term as well. See Note in ?sjp.int", call. = FALSE)
    }
    # -----------------------------------------------------------
    # retrieve number of unique values in each predictor variable.
    # depending on the amount of values the variable for the x-axis
    # is chosen. In this case, we use the predictor with the higher
    # number of unique values on the x-axis.
    # -----------------------------------------------------------
    # retrieve values as data frame
    df_pred1uniquevals <- unique(stats::na.omit(fitdat[, interactionterms[1]]))
    df_pred2uniquevals <- unique(stats::na.omit(fitdat[, interactionterms[2]]))
    # convert data frame to numeric vector
    pred1uniquevals <- pred2uniquevals <- as.numeric(c())
    pred1uniquevals <- sort(as.numeric(sapply(df_pred1uniquevals, as.numeric)))
    pred2uniquevals <- sort(as.numeric(sapply(df_pred2uniquevals, as.numeric)))
    # init data frame
    intdf <- c()
    # -----------------------------------------------------------
    # choose x-value according to higher number of unique values
    # choose minimum and maximum value from predictor that has
    # a "smaller range" (i.e. less unique values)
    # or swap predictors on axes if requested
    # -----------------------------------------------------------
    if (swapPredictors) {
      useFirstPredOnY <- ifelse(length(pred1uniquevals) > length(pred2uniquevals), F, T)
    } else {
      useFirstPredOnY <- ifelse(length(pred1uniquevals) > length(pred2uniquevals), T, F)
    }
    # -----------------------------------------------------------
    # calculate regression line
    # -----------------------------------------------------------
    if (useFirstPredOnY) {
      labx <- interactionterms[1]
      predy <- interactionterms[2]
      # -----------------------------------------------------------
      # define predictor and moderator values
      # -----------------------------------------------------------
      pred.value <- pred1uniquevals
      mod.value <- pred2uniquevals
      # -----------------------------------------------------------
      # define predictor beta
      # -----------------------------------------------------------
      b.pred <- b1
    } else {
      labx <- interactionterms[2]
      predy <- interactionterms[1]
      # -----------------------------------------------------------
      # define predictor and moderator values
      # -----------------------------------------------------------
      pred.value <- pred2uniquevals
      mod.value <- pred1uniquevals
      # -----------------------------------------------------------
      # define predictor beta
      # -----------------------------------------------------------
      b.pred <- b2
    }
    # -----------------------------------------------------------
    # Check whether moderator value has enough unique values
    # for quartiles
    # -----------------------------------------------------------
    moderatorValues <- mv_check(moderatorValues, mod.value)
    # -----------------------------------------------------------
    # check which values of moderator should be plotted, i.e. if
    # lower/upper bound (min-max) or mean and standard-deviation
    # should be used as valus for the moderator.
    # see http://www.theanalysisfactor.com/3-tips-interpreting-moderation/
    # -----------------------------------------------------------
    if (moderatorValues == "minmax") {
      mw <- NA
      ymin <- min(mod.value, na.rm = T)
      ymax <- max(mod.value, na.rm = T)
    } else if (moderatorValues == "meansd") {
      mw <- mean(mod.value, na.rm = T)
      ymin <- mw - sd(mod.value, na.rm = T)
      ymax <- mw + sd(mod.value, na.rm = T)
    } else if (moderatorValues == "zeromax") {
      mw <- NA
      ymin <- 0
      ymax <- max(mod.value, na.rm = T)
    } else if (moderatorValues == "quart") {
      qu <- as.vector(quantile(mod.value, na.rm = T))
      mw <- qu[3]
      ymin <- qu[2]
      ymax <- qu[4]
    }
    # intercept of predictor's reference category
    est_b <- b.pred + b0
    # -----------------------------------------------------------
    # Create data frame for plotting the interactions by
    # manually calculating the linear regression by inserting
    # the estimates of each term and the associated interaction term,
    # i.e.: y = b0 + (b1 * pred1) + (b2 * pred2) + (b3 * pred1 * pred2)
    # -----------------------------------------------------------
    # We now calculate the conditional effect of predictor 1 under absence 
    # (or lowest impact) of predictor 2 on the dependent variable. Thus, 
    # the slope for predictor 2 is not calculated. see
    # http://www.theanalysisfactor.com/interpreting-interactions-in-regression/
    # http://www.theanalysisfactor.com/clarifications-on-interpreting-interactions-in-regression/
    # ------------------------------
    miny <- (b0 + (b.pred * pred.value) + (b3 * pred.value * ymin))
    # ------------------------------
    # here we calculate the conditional effect of predictor 1 under presence
    # (or strongest impact) of predictor 2 on the dependent variable. Thus, 
    # the slope for predictor 2 only is not needed. see references above
    # ------------------------------
    maxy <- (b0 + (b.pred * pred.value) + (b3 * pred.value * ymax))
    # store in df
    tmp <- data.frame(x = pred.value, 
                      y = miny, 
                      ymin = miny, 
                      ymax = maxy, 
                      grp = "min")
    intdf <- as.data.frame(rbind(intdf, tmp))
    # store in df
    tmp <- data.frame(x = pred.value, 
                      y = maxy, 
                      ymin = miny, 
                      ymax = maxy, 
                      grp = "max")
    intdf <- as.data.frame(rbind(intdf, tmp))
    # store in df
    if (moderatorValues == "meansd" || moderatorValues == "quart") {
      # ------------------------------
      # here we calculate the effect of predictor 1 under presence
      # of mean of predictor 2 on the dependent variable. Thus, the slope for
      # predictor 2 only is not needed. see references above
      # ------------------------------
      mittelwert <- (b0 + (b.pred * pred.value) + (b3 * pred.value * mw))
      tmp <- data.frame(x = pred.value, 
                        y = mittelwert, 
                        ymin = miny, 
                        ymax = maxy, 
                        grp = "mean")
      intdf <- as.data.frame(rbind(intdf, tmp))
    }
    # -----------------------------------------------------------
    # convert df-values to numeric
    # -----------------------------------------------------------
    if (fun == "lm" || fun == "lmer") {
      intdf$x <- sjmisc::to_value(intdf$x, keep.labels = F)
      intdf$y <- sjmisc::to_value(intdf$y, keep.labels = F)
      intdf$ymin <- sjmisc::to_value(intdf$ymin, keep.labels = F)
      intdf$ymax <- sjmisc::to_value(intdf$ymax, keep.labels = F)
      intdf$ydiff <- intdf$ymax - intdf$ymin
      # -----------------------------------------------------------
      # retrieve lowest and highest x and y position to determine
      # the scale limits
      # -----------------------------------------------------------
      if (is.null(axisLimits.y)) {
        if (diff) {
          lowerLim.y <- floor(min(intdf$ydiff, na.rm = T))
          upperLim.y <- ceiling(max(intdf$ydiff, na.rm = T))
        } else {
          lowerLim.y <- floor(min(intdf$y, na.rm = T))
          upperLim.y <- ceiling(max(intdf$y, na.rm = T))
        }
      } else {
        lowerLim.y <- axisLimits.y[1]
        upperLim.y <- axisLimits.y[2]
      }
    } else {
      intdf$x <- sjmisc::to_value(intdf$x, keep.labels = F)
      intdf$y <- plogis(sjmisc::to_value(intdf$y, keep.labels = F))
      intdf$ymin <- plogis(sjmisc::to_value(intdf$ymin, keep.labels = F))
      intdf$ymax <- plogis(sjmisc::to_value(intdf$ymax, keep.labels = F))
      intdf$ydiff <- plogis(intdf$ymax - intdf$ymin)
      # -----------------------------------------------------------
      # retrieve lowest and highest x and y position to determine
      # the scale limits
      # -----------------------------------------------------------
      if (is.null(axisLimits.y)) {
        lowerLim.y <- 0
        upperLim.y <- 1
      } else {
        lowerLim.y <- axisLimits.y[1]
        upperLim.y <- axisLimits.y[2]
      }
    }
    # -----------------------------------------------------------
    # check x-axis limits
    # -----------------------------------------------------------
    if (is.null(axisLimits.x)) {
      lowerLim.x <- axisLimits.x[1]
      upperLim.x <- axisLimits.x[2]
    } else {
      lowerLim.x <- floor(min(intdf$x, na.rm = T))
      upperLim.x <- ceiling(max(intdf$x, na.rm = T))
    }
    # -----------------------------------------------------------
    # check whether we have to modify axis limits in case intercept
    # lines are also plotted
    # -----------------------------------------------------------
    if (showInterceptLines) {
      # retrieve intercept bounds
      ilmin <- min(b0, est_b)
      ilmax <- max(b0, est_b)
      # adjust lower lim if necessary
      if (ilmin < lowerLim.y) lowerLim.y <- floor(ilmin)
      # adjust upper lim if necessary
      if (ilmax > upperLim.y) upperLim.y <- ceiling(max(ilmax))
    }
    # -----------------------------------------------------------
    # check whether user defined grid breaks / tick marks are used
    # -----------------------------------------------------------
    if (!is.null(gridBreaksAt)) {
      gridbreaks.x <- c(seq(lowerLim.x, upperLim.x, by = gridBreaksAt))
      gridbreaks.y <- c(seq(lowerLim.y, upperLim.y, by = gridBreaksAt))
    }
    # -----------------------------------------------------------
    # prepare plot title and axis titles
    # -----------------------------------------------------------
    if (is.null(title)) {
      labtitle <- paste0("Conditional effect of ",
                         interactionterms[ifelse(useFirstPredOnY == TRUE, 1, 2)],
                         " (by ",
                         interactionterms[ifelse(useFirstPredOnY == TRUE, 2, 1)],
                         ") on ", git[["depvar.label"]])
    } else {
      # copy plot counter 
      l_nr <- cnt
      # check if we have enough labels. if not, use last labels
      if (l_nr > length(title)) l_nr <- length(title)
      # set legend labels for plot
      labtitle <- title[l_nr]
    }
    # -----------------------------------------------------------
    # legend labels
    # -----------------------------------------------------------
    if (is.null(legendLabels)) {
      if (moderatorValues == "minmax") {
        lLabels <- c(paste0("lower bound of ", predy), paste0("upper bound of ", predy))
      } else if (moderatorValues == "meansd") {
        lLabels <- c(paste0("lower sd of ", predy), paste0("upper sd of ", predy), paste0("mean of ", predy))
      } else if (moderatorValues == "quart") {
        lLabels <- c(paste0("lower quartile of ", predy), paste0("upper quartile of ", predy), paste0("median of ", predy))
      } else {
        lLabels <- c(paste0("0 for ", predy), paste0("upper bound of ", predy))
      }
    } else {
      # copy plot counter 
      l_nr <- cnt
      # check if we have enough labels. if not, use last labels
      if (l_nr > length(legendLabels)) l_nr <- length(legendLabels)
      # set legend labels for plot
      lLabels <- legendLabels[[l_nr]]
    }
    # -----------------------------------------------------------
    # legend titles
    # -----------------------------------------------------------
    if (is.null(legendTitle)) {
      lTitle <- predy
    } else {
      # copy plot counter 
      l_nr <- cnt
      # check if we have enough legend titles, if not, use last legend title
      if (l_nr > length(legendTitle)) l_nr <- length(legendTitle)
      # set legend title for plot
      lTitle <- legendTitle[l_nr]
    }
    # -----------------------------------------------------------
    # x axis titles
    # -----------------------------------------------------------
    if (!is.null(axisTitle.x)) {
      # copy plot counter 
      l_nr <- cnt
      # check if we have enough axis titles, if not, use last legend title
      if (l_nr > length(axisTitle.x)) l_nr <- length(axisTitle.x)
      # set axis title
      labx <- axisTitle.x[l_nr]
    }
    if (!is.null(axisTitle.y)) laby <- axisTitle.y
    # -----------------------------------------------------------
    # prepare annotation labels
    # -----------------------------------------------------------
    annoLabels <- paste(lLabels[1], lineLabelString)
    annoLabels <- c(annoLabels, paste(lLabels[2], lineLabelString))
    # wrap title
    labtitle <- sjmisc::word_wrap(labtitle, breakTitleAt)
    # wrap legend labels
    lLabels <- sjmisc::word_wrap(lLabels, breakLegendLabelsAt)
    # wrap legend title
    lTitle <- sjmisc::word_wrap(lTitle, breakLegendTitleAt)
    # wrap annotation labels
    annoLabels <- sjmisc::word_wrap(annoLabels, breakAnnotationLabelsAt)
    # -----------------------------------------------------------
    # prepare base plot of interactions
    # -----------------------------------------------------------
    if (diff) {
      baseplot <- ggplot(intdf, aes(x = x, y = ydiff)) +
        # -----------------------------------------------------------
        # add a shaded region between minimun
        # and maximum curve of interactions
        # -----------------------------------------------------------
        geom_ribbon(aes(ymin = 0, ymax = ydiff),
                    fill = fillColor,
                    alpha = fillAlpha) +
        geom_line()
      # -----------------------------------------------------------
      # show value labels
      # -----------------------------------------------------------
      if (showValueLabels) {
        baseplot <- baseplot +
          geom_text(aes(label = round(ydiff, 1)),
                    vjust = 1.5,
                    show_guide = FALSE)
      }
    } else {
      baseplot <- ggplot(intdf, aes(x = x, y = y, colour = grp))
      # the shaded area between line only if plots are not faceted
      if (!facet.grid) {
        baseplot <- baseplot +
          # add a shaded region between minimun and maximum curve of interactions
          geom_ribbon(aes(ymin = ymin, ymax = ymax, colour = NULL),
                      fill = fillColor,
                      alpha = fillAlpha,
                      show_guide = FALSE)
      }
      # add line
      baseplot <- baseplot + geom_line()
      # ------------------------------------------------------------
      # plot value labels
      # ------------------------------------------------------------
      if (showValueLabels) {
        baseplot <- baseplot +
          geom_point() +
          geom_text(aes(label = round(y, 1)),
                    vjust = 1.5,
                    show_guide = FALSE)
      }
      # ------------------------------------------------------------
      # plot intercept line and estimate line (i.e. reference category
      # of predictor, in case interaction is not present)
      # ------------------------------------------------------------
      if (showInterceptLines) {
        baseplot <- baseplot +
          geom_abline(intercept = b0,
                      slope = 0,
                      colour = interceptLineColor) +
          geom_abline(intercept = est_b,
                      slope = 0,
                      colour = estLineColor)
        if (showInterceptLabels) {
          baseplot <- baseplot +
            annotate("text",
                     label = annoLabels[1],
                     x = -Inf,
                     hjust = -0.05,
                     vjust = -0.5,
                     colour = lineLabelColor,
                     size = lineLabelSize,
                     y = b0) +
            annotate("text",
                     label = annoLabels[2],
                     x = -Inf,
                     hjust = -0.05,
                     vjust = -0.5,
                     colour = lineLabelColor,
                     size = lineLabelSize,
                     y = est_b)
        }
      }
    }
    # ------------------------------------------------------------------------------------
    # check whether only diff-line is shown or upper and lower boundaries. in the latter
    # case, show legend, else hide legend
    # ------------------------------------------------------------------------------------
    if (diff) {
      col.len <- 1
      lLabels <- NULL
      lTitle <- NULL
    } else {
      if (moderatorValues == "minmax" || moderatorValues == "zeromax") {
        col.len <- 2
      } else {
        col.len <- 3
      }
    }
    # ------------------------------------------------------------------------------------
    # build plot object with theme and labels
    # ------------------------------------------------------------------------------------
    baseplot <- baseplot +
      # set plot and axis titles
      labs(title = labtitle, x = labx, y = laby, colour = lTitle) +
      # set axis scale breaks
      scale_x_continuous(limits = c(lowerLim.x, upperLim.x), breaks = gridbreaks.x) +
      scale_y_continuous(limits = c(lowerLim.y, upperLim.y), breaks = gridbreaks.y)
    # ---------------------------------------------------------
    # facet grids?
    # ---------------------------------------------------------
    if (facet.grid && !diff) baseplot <- baseplot + facet_grid(~grp)
    # ---------------------------------------------------------
    # set geom colors
    # ---------------------------------------------------------
    baseplot <- sj.setGeomColors(baseplot, geom.colors, col.len, !is.null(lLabels), lLabels)
    # ---------------------------------------------------------
    # Check whether ggplot object should be returned or plotted
    # ---------------------------------------------------------
    if (printPlot) print(baseplot)
    # concatenate plot object
    plotlist[[length(plotlist) + 1]] <- baseplot
    dflist[[length(dflist) + 1]] <- intdf
  }
  # -------------------------------------
  # return results
  # -------------------------------------
  invisible(structure(class = "sjpint",
                      list(plot.list = plotlist,
                           df.list = dflist)))
}


#' @importFrom stats plogis na.omit
sjp.eff.int <- function(fit,
                        int.term = NULL,
                        int.plot.index = NULL,
                        moderatorValues = "minmax",
                        swapPredictors = FALSE,
                        plevel = 0.05,
                        title = NULL,
                        fillAlpha = 0.3,
                        geom.colors = "Set1",
                        axisTitle.x = NULL,
                        axisTitle.y = NULL,
                        legendTitle = NULL,
                        legendLabels = NULL,
                        showValueLabels = FALSE,
                        breakTitleAt = 50,
                        breakLegendLabelsAt = 20,
                        breakLegendTitleAt = 20,
                        breakAnnotationLabelsAt = 50,
                        axisLimits.x = NULL,
                        axisLimits.y = NULL,
                        gridBreaksAt = NULL,
                        showCI = FALSE,
                        facet.grid = FALSE,
                        printPlot = TRUE,
                        fun) {
  # ------------------------
  # check if suggested package is available
  # ------------------------
  if (!requireNamespace("effects", quietly = TRUE)) {
    stop("Package 'effects' needed for this function to work. Please install it.", call. = FALSE)
  }
  # gridbreaks
  if (is.null(gridBreaksAt)) gridbreaks.x <- gridbreaks.y <- ggplot2::waiver()
  # ------------------------
  # calculate effects of higher order terms and
  # check if fitted model contains any interaction terms
  # allEffects returns a list, with all interaction effects 
  # (or higher order terms) as separate list element. each list
  # element contains the higher-order-term of the fitted model,
  # where the 'term' attribute of interaction terms have a "*". 
  # So we just need to look at each 'term' attribute of each
  # list element and see if there is a "*"...
  # ------------------------
  if (is.null(int.term)) {
    eff <- effects::allEffects(fit, KR = F)
    int <- unlist(lapply(eff, function(x) grep("*", x['term'], fixed = T)))
  } else {
    eff <- effects::effect(int.term, fit, KR = F)
    int <- grep("*", eff$term, fixed = T)
  }
  if (length(int) == 0) {
    warning("No interaction term found in fitted model...", call. = FALSE)
    return(invisible(NULL))
  }
  # ------------------------
  # retrieve position of interaction terms in effects-object
  # ------------------------
  if (is.null(int.term)) {
    intpos <- which(as.vector(sapply(eff, function(x) length(grep("*", x['term'], fixed = T)) > 0)) == T)
  } else {
    intpos <- 1
  }
  # select only specific plots
  if (!is.null(int.plot.index) && !any(int.plot.index > length(intpos))) intpos <- intpos[int.plot.index]  
  # init vector that saves ggplot objects
  plotlist <- list()
  dflist <- list()
  # -----------------------------------------------------------
  # iterate all interaction terms
  # -----------------------------------------------------------
  for (i in 1:length(intpos)) {
    # -----------------------------------------------------------
    # copy "eff" object, so we don't confuse with effect-return-
    # value from single term and multiple terms
    # -----------------------------------------------------------
    if (is.null(int.term)) {
      dummy.eff <- eff[[intpos[i]]]
    } else {
      dummy.eff <- eff
    }
    # -----------------------------------------------------------
    # retrieve data frame
    # -----------------------------------------------------------
    intdf <- data.frame(dummy.eff)
    # -----------------------------------------------------------
    # save response, predictor and moderator names
    # -----------------------------------------------------------
    pred_x.name <- colnames(intdf)[ifelse(swapPredictors == TRUE, 1, 2)]
    moderator.name <- colnames(intdf)[ifelse(swapPredictors == TRUE, 2, 1)]
    response.name <- dummy.eff$response
    # prepare axis titles
    labx <- pred_x.name
    # check whether x-axis-predictor is a factor or not
    x_is_factor <- is.factor(intdf[[pred_x.name]])
    # -----------------------------------------------------------
    # check for moderator values, but only, if moderator 
    # is no factor value. In this case, we can choose
    # the values for continuous moderator intentionally,
    # e.g. only min/max, or mean and sd. We don't need these
    # values for categorical moderator values.
    # -----------------------------------------------------------
    if (!is.factor(intdf[[moderator.name]])) {
      # retrieve moderator value
      modval <- dummy.eff$data[[moderator.name]]
      # retrieve predictor value
      predval <- dummy.eff$data[[pred_x.name]]
      # -----------------------------------------------------------
      # Check whether moderator value has enough unique values
      # for quartiles
      # -----------------------------------------------------------
      moderatorValues <- mv_check(moderatorValues, modval)
      # we have more than two values, so re-calculate effects, just using
      # min and max value of moderator. 
      if (moderatorValues == "minmax" && length(unique(intdf[[moderator.name]])) > 2) {
        # retrieve min and max values
        mv.min <- min(modval, na.rm = T)
        mv.max <- max(modval, na.rm = T)
        # re-compute effects, prepare xlevels
        xl1 <- list(x = c(mv.min, mv.max))
      # we have more than two values, so re-calculate effects, just using
      # 0 and max value of moderator.
      } else if (moderatorValues == "zeromax" && length(unique(intdf[[moderator.name]])) > 2) {
        # retrieve max values
        mv.max <- max(modval, na.rm = T)
        # re-compute effects, prepare xlevels
        xl1 <- list(x = c(0, mv.max))
      # compute mean +/- sd
      } else if (moderatorValues == "meansd") {
        # retrieve mean and sd
        mv.mean <- round(mean(modval, na.rm = T), 2)
        mv.sd <- round(sd(modval, na.rm = T), 2)
        # re-compute effects, prepare xlevels
        xl1 <- list(x = c(mv.mean - mv.sd, mv.mean, mv.mean + mv.sd))
      } else if (moderatorValues == "quart") {
        # re-compute effects, prepare xlevels
        xl1 <- list(x = as.vector(quantile(modval, na.rm = T)))
      }
      # change list name to moderator value name
      names(xl1) <- moderator.name
      # add values of interaction term
      # first, get all unqiue values
      prvl <- sort(unique(stats::na.omit(predval)))
      # add them to list as well
      xl2 <- list(y = prvl)
      # change list name
      names(xl2) <- pred_x.name
      # combine lists
      if (is.null(int.term)) {
        # re-compute effects
        eff.tmp <- effects::allEffects(fit, xlevels = c(xl1, xl2), KR = F)
        # reset data frame
        intdf <- data.frame(eff.tmp[[intpos[i]]])
      } else {
        # re-compute effects
        eff.tmp <- effects::effect(int.term, fit, xlevels = c(xl1, xl2), KR = F)
        # reset data frame
        intdf <- data.frame(eff.tmp)
      }
      # -----------------------------------------------------------
      # check for predictor values on x-axis. if it 
      # is no factor, select whole range of possible
      # values.
      # -----------------------------------------------------------
    } else if (!is.factor(intdf[[pred_x.name]])) {
      # retrieve predictor value
      predval <- dummy.eff$data[[pred_x.name]]
      # add values of interaction term
      # first, get all unqiue values
      prvl <- sort(unique(stats::na.omit(predval)))
      # add them to list as well
      xl <- list(x = prvl)
      # change list name
      names(xl) <- pred_x.name
      # combine lists
      if (is.null(int.term)) {
        # re-compute effects
        eff.tmp <- effects::allEffects(fit, xlevels = xl, KR = F)
        # reset data frame
        intdf <- data.frame(eff.tmp[[intpos[i]]])
      } else {
        # re-compute effects
        eff.tmp <- effects::effect(int.term, fit, xlevels = xl, KR = F)
        # reset data frame
        intdf <- data.frame(eff.tmp)
      }
    }
    # -----------------------------------------------------------
    # change column names
    # -----------------------------------------------------------
    if (swapPredictors) {
      colnames(intdf) <- c("x", "grp", "y", "se", "lower", "upper")
    } else {
      colnames(intdf) <- c("grp", "x", "y", "se", "lower", "upper")
    }
    # -----------------------------------------------------------
    # effects-package creates "NA" factor levels, which
    # need to be removed
    # -----------------------------------------------------------
    intdf <- droplevels(intdf)
    # group as factor
    intdf$grp <- as.factor(intdf$grp)
    # make sure x is numeric
    intdf$x <- sjmisc::to_value(intdf$x, keep.labels = F)
    # -----------------------------------------------------------
    # convert df-values to numeric
    # -----------------------------------------------------------
    if (fun == "lm" || fun == "lmer" || fun == "lme" || fun == "gls") {
      # Label on y-axis is name of dependent variable
      laby <- response.name
      # -----------------------------------------------------------
      # retrieve lowest and highest x and y position to determine
      # the scale limits
      # -----------------------------------------------------------
      if (is.null(axisLimits.y)) {
        if (showCI) {
          lowerLim.y <- floor(min(intdf$lower, na.rm = T))
          upperLim.y <- ceiling(max(intdf$upper, na.rm = T))
        } else {
          lowerLim.y <- floor(min(intdf$y, na.rm = T))
          upperLim.y <- ceiling(max(intdf$y, na.rm = T))
        }
      } else {
        lowerLim.y <- axisLimits.y[1]
        upperLim.y <- axisLimits.y[2]
      }
    } else {
      # Label on y-axis is fixed
      if (is.null(axisTitle.y)) axisTitle.y <- "Predicted Probability"
      # convert log-odds to probabilities
      intdf$y <- stats::plogis(intdf$y)
      intdf$lower <- stats::plogis(intdf$lower)
      intdf$upper <- stats::plogis(intdf$upper)
      intdf$se <- stats::plogis(intdf$se)
      # -----------------------------------------------------------
      # retrieve lowest and highest x and y position to determine
      # the scale limits
      # -----------------------------------------------------------
      if (is.null(axisLimits.y)) {
        lowerLim.y <- 0
        upperLim.y <- 1
      } else {
        lowerLim.y <- axisLimits.y[1]
        upperLim.y <- axisLimits.y[2]
      }
    }
    # -----------------------------------------------------------
    # check x-axis limits
    # -----------------------------------------------------------
    if (is.null(axisLimits.x)) {
      lowerLim.x <- axisLimits.x[1]
      upperLim.x <- axisLimits.x[2]
    } else {
      lowerLim.x <- floor(min(intdf$x, na.rm = T))
      upperLim.x <- ceiling(max(intdf$x, na.rm = T))
    }
    # -----------------------------------------------------------
    # check whether user defined grid breaks / tick marks are used
    # -----------------------------------------------------------
    if (!is.null(gridBreaksAt)) {
      gridbreaks.x <- c(seq(lowerLim.x, upperLim.x, by = gridBreaksAt))
      gridbreaks.y <- c(seq(lowerLim.y, upperLim.y, by = gridBreaksAt))
    }
    # -----------------------------------------------------------
    # prepare plot title and axis titles
    # -----------------------------------------------------------
    if (is.null(title)) {
      labtitle <- paste0("Interaction effect of ",
                         moderator.name,
                         " and ",
                         pred_x.name,
                         " on ", 
                         response.name)
    } else {
      # copy plot counter 
      l_nr <- i
      # check if we have enough labels. if not, use last labels
      if (l_nr > length(title)) l_nr <- length(title)
      # set legend labels for plot
      labtitle <- title[l_nr]
    }
    # -----------------------------------------------------------
    # legend labels
    # -----------------------------------------------------------
    if (is.null(legendLabels)) {
      lLabels <- levels(intdf$grp)
    } else {
      # copy plot counter 
      l_nr <- i
      # check if we have enough labels. if not, use last labels
      if (l_nr > length(legendLabels)) l_nr <- length(legendLabels)
      # set legend labels for plot
      lLabels <- legendLabels[[l_nr]]
    }
    # -----------------------------------------------------------
    # legend titles
    # -----------------------------------------------------------
    if (is.null(legendTitle)) {
      lTitle <- moderator.name
    } else {
      # copy plot counter 
      l_nr <- i
      # check if we have enough legend titles, if not, use last legend title
      if (l_nr > length(legendTitle)) l_nr <- length(legendTitle)
      # set legend title for plot
      lTitle <- legendTitle[l_nr]
    }
    # -----------------------------------------------------------
    # x axis titles
    # -----------------------------------------------------------
    if (!is.null(axisTitle.x)) {
      # copy plot counter 
      l_nr <- i
      # check if we have enough axis titles, if not, use last legend title
      if (l_nr > length(axisTitle.x)) l_nr <- length(axisTitle.x)
      # set axis title
      labx <- axisTitle.x[l_nr]
    }
    if (!is.null(axisTitle.y)) laby <- axisTitle.y
    # -----------------------------------------------------------
    # wrap titles
    # -----------------------------------------------------------
    labtitle <- sjmisc::word_wrap(labtitle, breakTitleAt)
    # wrap legend labels
    lLabels <- sjmisc::word_wrap(lLabels, breakLegendLabelsAt)
    # wrap legend title
    lTitle <- sjmisc::word_wrap(lTitle, breakLegendTitleAt)
    # ------------------------------------------------------------
    # start plot
    # ------------------------------------------------------------
    baseplot <- ggplot(intdf, aes(x = x, y = y, colour = grp))
    # ------------------------------------------------------------
    # confidence interval?
    # ------------------------------------------------------------
    if (showCI) {
      if (x_is_factor) {
        # -------------------------------------------------
        # for factors, we add error bars instead of
        # continuous confidence region
        # -------------------------------------------------
        baseplot <- baseplot +
          geom_errorbar(aes(ymin = lower, ymax = upper, colour = grp),
                        width = 0,
                        show_guide = FALSE) +
          geom_point()
      } else {
        # -------------------------------------------------
        # for continuous variables, we add  continuous 
        # confidence region instead of error bars 
        # -------------------------------------------------
        baseplot <- baseplot +
          geom_ribbon(aes(ymin = lower, ymax = upper, colour = NULL, fill = grp),
                      alpha = fillAlpha,
                      show_guide = FALSE)
      }
    }
    baseplot <- baseplot + geom_line()
    # ------------------------------------------------------------
    # plot value labels
    # ------------------------------------------------------------
    if (showValueLabels) {
      # don't need geom_point, because point-layer already 
      # added with x_is_factor
      if (!x_is_factor) baseplot <- baseplot + geom_point()
      # add value label text
      baseplot <- baseplot +
        geom_text(aes(label = round(y, 1)),
                  vjust = 1.5,
                  show_guide = FALSE)
    }
    # ------------------------------------------------------------------------------------
    # build plot object with theme and labels
    # ------------------------------------------------------------------------------------
    baseplot <- baseplot +
      # set plot and axis titles
      labs(title = labtitle, x = labx, y = laby, colour = lTitle) +
      # set axis scale breaks
      scale_x_continuous(limits = c(lowerLim.x, upperLim.x), breaks = gridbreaks.x) +
      scale_y_continuous(limits = c(lowerLim.y, upperLim.y), breaks = gridbreaks.y)
    # ---------------------------------------------------------
    # facet grids?
    # ---------------------------------------------------------
    if (facet.grid) baseplot <- baseplot + facet_grid(~grp)
    # ---------------------------------------------------------
    # set geom colors
    # ---------------------------------------------------------
    baseplot <- sj.setGeomColors(baseplot, 
                                 geom.colors, 
                                 length(unique(stats::na.omit(intdf$grp))), 
                                 !is.null(lLabels), 
                                 lLabels)
    # ---------------------------------------------------------
    # Check whether ggplot object should be returned or plotted
    # ---------------------------------------------------------
    if (printPlot) print(baseplot)
    # concatenate plot object
    plotlist[[length(plotlist) + 1]] <- baseplot
    dflist[[length(dflist) + 1]] <- intdf
  }
  # -------------------------------------
  # return results
  # -------------------------------------
  invisible(structure(class = "sjpint",
                      list(plot.list = plotlist,
                           df.list = dflist)))
}


#' @importFrom stats quantile
mv_check <- function(moderatorValues, x) {
  mvc <- length(unique(as.vector(stats::quantile(x, na.rm = T))))
  if (moderatorValues == "quart" && mvc < 3) {
    # tell user that quart won't work
    message("Could not compute quartiles, too small range of moderator variable. Defaulting 'moderatorValues' to 'minmax'.")
    moderatorValues <- "minmax"
  }
  return(moderatorValues)
}


# get all (significant) interaction terms from model
# the function "getInteractionTerms" checks if a fitted
# model contains any interaction terms that are significant
# at the level specified by "plevel". returns NULL, if model
# contains no interaction terms or no significant interaction term.
# else, information on model and interaction terms is returned
#' @importFrom stats model.matrix
getInteractionTerms <- function(fit, fun, plevel) {
  # -----------------------------------------------------------
  # retrieve coefficients
  # -----------------------------------------------------------
  coef.tab <- summary(fit)$coefficients
  pval <- rep(0, times = nrow(coef.tab) - 1)
  # -----------------------------------------------------------
  # Help-function that removes AsIS I from formulas.
  # If someone know regular expressions better than me,
  # please provide a one-liner solution for the 3 sub commands.
  # -----------------------------------------------------------
  remove_I <- function(xnames) {
    fpos <- grep("I(", xnames, fixed = T)
    if (length(fpos) > 0 && fpos > 0) {
      xnames <- sub("I(", "", xnames, fixed = T)
      xnames <- sub(")", "", xnames, fixed = T)
      xnames <- sub(" * ", ":", xnames, fixed = T)
    }
    return(xnames)
  }
  # -----------------------------------------------------------
  # prepare values for (generalized) linear models
  # -----------------------------------------------------------
  if (fun == "lm" || fun == "glm" || fun == "plm" || fun == "lme" || fun == "gls") {
    # -----------------------------------------------------------
    # retrieve amount and names of predictor variables and
    # of dependent variable
    # -----------------------------------------------------------
    if (fun == "plm") {
      # plm objects have different structure than (g)lm
      depvar.label <- attr(attr(attr(fit$model, "terms"), "dataClasses"), "names")[1]
      # retrieve model matrix
      fitdat <- data.frame(cbind(as.vector(fit$model[, 1]), stats::model.matrix(fit)))
    } else {
      depvar.label <- attr(attr(fit$terms, "dataClasses"), "names")[1]
      # retrieve model matrix
      fitdat <- data.frame(stats::model.matrix(fit))
    }
    # -----------------------------------------------------------
    # retrieve p-values, without intercept
    # -----------------------------------------------------------
    if (ncol(coef.tab) > 3) pval <- coef.tab[-1, 4]
    # -----------------------------------------------------------
    # retrieve estimates, without intercept
    # -----------------------------------------------------------
    estimates <- coef.tab[-1, 1]
    # -----------------------------------------------------------
    # need to remove "I(...)"?
    # -----------------------------------------------------------
    estimates.names <- names(estimates)
    estimates.names <- remove_I(estimates.names)
    it <- estimates.names
    # -----------------------------------------------------------
    # retrieve estimate of intercept
    # -----------------------------------------------------------
    b0 <- coef.tab[1, 1]
    # -----------------------------------------------------------
    # prepare values for (generalized) linear mixed effecrs models
    # -----------------------------------------------------------
  } else if (fun == "lmer" || fun == "glmer" || fun == "nlmer") {
    # -----------------------------------------------------------
    # retrieve amount and names of predictor variables and
    # of dependent variable
    # -----------------------------------------------------------
    depvar.label <- colnames(fit@frame)[1]
    # -----------------------------------------------------------
    # retrieve p-values, without intercept
    # -----------------------------------------------------------
    pval <- get_lmerMod_pvalues(fit)[-1]
    # -----------------------------------------------------------
    # retrieve estimates, without intercept
    # -----------------------------------------------------------
    estimates <- unname(lme4::fixef(fit)[-1])
    estimates.names <- names(lme4::fixef(fit)[-1])
    # -----------------------------------------------------------
    # retrieve model matrix with all relevant predictors
    # -----------------------------------------------------------
    fitdat <- stats::model.matrix(fit)
    # -----------------------------------------------------------
    # need to remove "I(...)"?
    # -----------------------------------------------------------
    estimates.names <- remove_I(estimates.names)
    it <- estimates.names
    # -----------------------------------------------------------
    # retrieve estimate of intercept
    # -----------------------------------------------------------
    b0 <- unname(lme4::fixef(fit)[1])
  } else {
    stop("Unsupported model-class. This type of regression is not yet supported by 'sjp.int'.", call. = F)
  }
  # -----------------------------------------------------------
  # find all significant interactions
  # we start looking for significant p-values beginning
  # with the first interaction, not the first single term!
  # thus, the starting point is first position after all single
  # predictor variables
  # -----------------------------------------------------------
  # find interaction terms, which contains a colon, in row names
  firstit <- grep(":", it, fixed = TRUE)[1]
  # check whether we have any interaction terms included at all
  if (is.null(firstit) || is.na(firstit) || firstit == 0) {
    warning("No interaction term found in fitted model...", call. = FALSE)
    return(invisible(NULL))
  }
  # save names of interaction predictor variables into this object
  intnames <- c()
  for (i in firstit:length(pval)) {
    if (pval[i] < plevel) intnames <- c(intnames, it[i])
  }
  # check for any signigicant interactions, stop if nothing found
  if (is.null(intnames)) {
    warning("No significant interactions found... Try to adjust 'plevel' argument.", call. = FALSE)
    return(invisible(NULL))
  }
  return(list(intnames = intnames,
              estimates = estimates,
              estimates.names = estimates.names,
              b0 = b0,
              fitdat = fitdat,
              depvar.label = depvar.label))
}