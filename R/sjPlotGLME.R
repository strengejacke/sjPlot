# bind global variables
utils::globalVariables(c("estimate", "nQQ", "ci", "fixef", "fade", "conf.low", "conf.high", "pred", "prob", "p.string", "CSS", "useViewer", "no.output"))


#' @title Plot estimates or effects of generalized linear mixed effects models
#' @name sjp.glmer
#'
#' @seealso \href{http://www.strengejacke.de/sjPlot/sjp.glmer/}{sjPlot manual: sjp.glmer}
#'
#' @description By default, this function plots odds or incidents ratios (exponentiated coefficients)
#'                with confidence intervalls of either fixed effects or random effects of
#'                generalized linear mixed effects models (that have been fitted with the
#'                \code{\link[lme4]{glmer}}-function of the \pkg{lme4}-package).
#'                Furthermore, this function also plots predicted probabilities /
#'                incidents or diagnostic plots.
#'
#' @param fit a fitted model as returned by the \code{\link[lme4]{glmer}}-function.
#' @param type type of plot. Use one of following:
#'          \describe{
#'            \item{\code{"re"}}{(default) for odds or incidents ratios of random effects}
#'            \item{\code{"fe"}}{for odds or incidents ratios of fixed effects}
#'            \item{\code{"fe.cor"}}{for correlation matrix of fixed effects}
#'            \item{\code{"re.qq"}}{for a QQ-plot of random effects (random effects quantiles against standard normal quantiles)}
#'            \item{\code{"fe.pc"}}{or \code{"fe.prob"} to plot probability curves (predicted probabilities) of all fixed effects coefficients. Use \code{facet.grid} to decide whether to plot each coefficient as separate plot or as integrated faceted plot. See 'Details'.}
#'            \item{\code{"ri.pc"}}{or \code{"ri.prob"} to plot probability curves (predicted probabilities) of random intercept variances for all fixed effects coefficients. Use \code{facet.grid} to decide whether to plot each coefficient as separate plot or as integrated faceted plot. See 'Details'.}
#'            \item{\code{"rs.ri"}}{for fitted probability curves (predicted probabilities) indicating the random slope-intercept pairs. Use this to visualize the random parts of random slope-intercept (or repeated measure) models. When having too many groups, use \code{sample.n} argument.}
#'            \item{\code{"eff"}}{to plot marginal effects of predicted probabilities or incidents for each fixed term, where remaining co-variates are set to the mean. Use \code{facet.grid} to decide whether to plot each coefficient as separate plot or as integrated faceted plot. See 'Details'.}
#'            \item{\code{"y.pc"}}{or \code{"y.prob"} to plot predicted probabilities for the response, with and without random effects. Use \code{facet.grid} to decide whether to plot with and w/o random effect plots as separate plot or as integrated faceted plot. See 'Details'.}
#'            \item{\code{"ma"}}{to check model assumptions. Note that only argument \code{fit} applies to this plot type. All other arguments are ignored.}
#'          }
#' @param vars numeric vector with column indices of selected variables or a character vector with
#'          variable names of selected variables from the fitted model, which should be used to plot
#'          estimates, fixed effects slopes (for \code{\link[lme4]{lmer}}) or probability curves
#'          (for \code{\link[lme4]{glmer}}) of random intercepts. This argument
#'          applies if \code{type} is \code{"fe"}, \code{"fe.std"}, \code{"re"},
#'          \code{"fe.pc"}, \code{"ri.pc"} or \code{"fe.ri"}.
#'          In this case, only those terms specified in \code{"vars"} will be plotted.
#' @param ri.nr numeric vector. If \code{type = "re"}, \code{type = "ri.pc"} or \code{type = "fe.ri"},
#'          and fitted model has more than one random intercept, \code{ri.nr} indicates
#'          which random effects of which random intercept (or: which list elements
#'          of \code{\link[lme4]{ranef}}) will be plotted. Default is \code{NULL},
#'          so all random effects will be plotted.
#' @param emph.grp numeric vector with index numbers of grouping levels (from random effect).
#'          If \code{type = "ri.pc"} or \code{type = "fe.ri"}, and \code{facet.grid = FALSE},
#'          an integrated plot of predicted probabilities of fixed effects resp. fixed
#'          effects slopes for each grouping level is plotted. To better find
#'          certain groups, use this argument to emphasize these groups in the plot.
#'          See 'Examples'.
#' @param show.ci logical, use \code{TRUE} to plot confidence intervals for 
#'          predicted values or for slope lines.
#' @param title character vector with one or more labels that are used as plot title. If
#'          \code{type = "re"}, use the predictors' variable labels as titles.
#' @param geom.colors user defined color palette for geoms. Must either be vector with two color values
#'          or a specific color palette code. See 'Note' in \code{\link{sjp.grpfrq}}.
#' @param geom.size size of geoms (point size or line size, depending on \code{type}-argument).
#' @param hideErrorBars logical, if \code{TRUE}, the error bars that indicate the confidence intervals of the odds ratios are not
#'          shown.
#' @param showIntercept logical, if \code{TRUE}, the intercept is included when plotting random or fixed effects.
#' @param stringIntercept string, label of intercept estimate on the y axis. Only applies, if \code{showIntercept}
#'          is \code{TRUE} and \code{pred.labels} is not \code{NULL}.
#' @param sort.coef indicates which coefficient should be used for sorting odds ratios
#'          \itemize{
#'            \item If \code{NULL} (default), no sorting is done and odds ratios are sorted in order of model coefficients.
#'            \item If \code{sort.coef = "sort.all"}, odds ratios are re-sorted for each coefficient (only applies if \code{type = "re"} and \code{facet.grid = FALSE}), i.e. the odds ratios of the random effects for each predictor are sorted and plotted to an own plot.
#'            \item If \code{type = "fe"} or \code{type = "fe.std"}, \code{TRUE} will sort estimates
#'            \item Else, specify a predictor's / coefficient's name to sort odds ratios according to this coefficient.
#'            }
#'            See 'Examples'.
#' @param fade.ns if \code{TRUE}, non significant estimates will be printed in slightly faded colors.
#' @param pred.labels character vector with labels for the predictors / covariates / groups. Should either be vector
#'          of fixed effects variable labels (if \code{type = "fe"} or \code{type = "fe.std"}) or a vector of group (value)
#'          labels from the random intercept's categories (if \code{type = "re"}).
#' @param axisTitle.x title for the x axis. If not specified, a default labelling depending
#'          on the plot type is chosen.
#' @param axisTitle.y title for the y axis. If not specified, a default labelling depending
#'          on the plot type is chosen.
#' @param interceptLineType linetype of the intercept line (zero point). Default is \code{2} (dashed line).
#' @param interceptLineColor color of the intercept line. Default value is \code{"grey70"}.
#' @param showValueLabels logical, whether estimate values should be plotted to each dot or not.
#' @param labelDigits numeric, amount of digits for rounding the estimates (see \code{showValueLabels}).
#'          Default is 2, i.e. estimates have 2 digits after decimal point.
#' @param showPValueLabels logical, whether the significance levels of each coefficient should be appended
#'          to values or not.
#' @param facet.grid \code{TRUE} when each plot should be plotted separately instead of
#'          an integrated (faceted) single graph.
#' @param free.scale if \code{TRUE} and \code{facet.grid = TRUE}, each facet grid
#'          gets its own fitted scale. If \code{free.scale = FALSE}, each facet in
#'          the grid has the same scale range.
#' @param sample.n numeric vector. only applies, if \code{type = "rs.ri"}. If 
#'          plot has many random intercepts (grouping levels), overplotting of 
#'          regression lines may occur. In this case, consider random sampling of 
#'          grouping levels. If \code{sample.n} is of length 1, a random sample
#'          of \code{sample.n} observation is selected to plot random intercepts.
#'          If \code{sample.n} is of length > 1, random effects indicated by
#'          the values in \code{sample.n} are selected to plot random effects.
#'          Use the latter option to always select a fixed, identical set of
#'          random effects for plotting (useful when ecomparing multiple models).
#' @param show.legend logical, if \code{TRUE}, for mixed models 
#'          \code{type = "rs.ri"}, a legend for group levels of 
#'          the random intercept is shown. For \code{lm} and \code{glm}, 
#'          a legend for grouped estimates is shown.
#'
#' @inheritParams sjp.grpfrq
#' @inheritParams sjp.lm
#' @inheritParams sjp.glm
#'
#' @return (Insisibily) returns
#'          \itemize{
#'            \item the ggplot-object (\code{plot}), if \code{type = "fe"} or if \code{type = "re"} and \code{facet.grid = TRUE}). Multiple plots (\code{type = "re"} and if \code{facet.grid = FALSE}) are returned in the object \code{plot.list}.
#'            \item a list of ggplot-objects (\code{plot.list}). See \code{plot} for details.
#'            \item a data frame \code{data} with the data used to build the ggplot-object(s).
#'            }
#'
#' @note \itemize{
#'          \item{Computation of p-values (if necessary) are based on 
#'                Wald chi-square tests from the \code{Anova}-function of the \pkg{car}-package.}
#'          \item{Most plot types work for binomial outcomes only (see 'Details'),
#'                however, some plot types like \code{type = "fe"} or \code{type = "eff"} also
#'                work for count reponses.}
#'          \item{Thanks go to Robert Reijntjes from 
#'                Leiden University Medical Center for sharing R code that is used 
#'                to compute fixed effects correlation matrices and qq-plots of 
#'                random effects.}
#'        }
#'
#' @details \describe{
#'            \item{\code{type = "fe.pc"}}{(or \code{"fe.prob"}), the predicted probabilities
#'            are based on the fixed effects intercept's estimate and each specific
#'            fixed term's estimate. All other fixed effects are set to zero (i.e. ignored),
#'            which corresponds to \code{\link{plogis}(b0 + bi * xi)} (where \code{xi}
#'            is the logit-estimate of fixed effects and \code{b0} is the intercept of
#'            the fixed effects).}
#'            \item{\code{type = "eff"}}{unlike \code{type = "fe.pc"}, the predicted
#'            probabilities or incidents computed by \code{type = "eff"} have all co-variates
#'            set to the mean, as returned by the \code{\link[effects]{allEffects}} function.}
#'            \item{\code{type = "ri.pc"}}{(or \code{"ri.prob"}), the predicted probabilities
#'            are based on the fixed effects intercept, plus each random intercept and
#'            each specific  fixed term's estimate. All other fixed effects are set to zero (i.e. ignored),
#'            which corresponds to \code{\link{plogis}(b0 + b0[r1-rn] + bi * xi)} (where \code{xi}
#'            is the logit-estimate of fixed effects, \code{b0} is the intercept of
#'            the fixed effects and \code{b0[r1-rn]} are all random intercepts).}
#'            \item{\code{type = "rs.ri"}}{the predicted probabilities are based 
#'            on the fixed effects intercept, plus each random intercept and
#'            random slope. This plot type is intended to plot the random part, i.e.
#'            the predicted probabilities of each random slope for each random intercept.
#'            Since the random intercept specifies the deviance from the gloabl
#'            intercept, the global intercept is always included. In case of overplotting,
#'            use the \code{sample.n} argument to randomly sample a limited amount
#'            of groups.}
#'            \item{\code{type = "coef"}}{forest plot of joint fixed and random
#'            effect coefficients, as retrieved by \code{\link[lme4]{coef.merMod}},
#'            it's simply \code{\link[lme4]{ranef} + \link[lme4]{fixef}}.}
#'            \item{\code{type = "y.pc"}}{(or \code{"y.prob"}) predicted probabilities
#'            against response, only fixed effects and 
#'            conditional on random intercept. It's calling
#'            \code{predict(fit, type = "response", re.form = NA)} resp.
#'            \code{predict(fit, type = "response", re.form = NULL)} to
#'            compute the values.}
#'          }
#'
#' @examples
#' library(lme4)
#' library(sjmisc)
#' # create binary response
#' sleepstudy$Reaction.dicho <- dicho(sleepstudy$Reaction, dich.by = "md")
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
#' data(efc)
#' # create binary response
#' efc$hi_qol <- dicho(efc$quol_5)
#' # prepare group variable
#' efc$grp = as.factor(efc$e15relat)
#' levels(x = efc$grp) <- get_labels(efc$e15relat)
#' # data frame for fitted model
#' mydf <- data.frame(hi_qol = as.factor(efc$hi_qol),
#'                    sex = as.factor(efc$c161sex),
#'                    c12hour = as.numeric(efc$c12hour),
#'                    neg_c_7 = as.numeric(efc$neg_c_7),
#'                    grp = efc$grp)
#' # fit glmer
#' fit <- glmer(hi_qol ~ sex + c12hour + neg_c_7 + (1|grp),
#'              data = mydf,
#'              family = binomial("logit"))
#'
#' # plot and sort fixed effects
#' sjp.glmer(fit,
#'           type = "fe",
#'           sort.coef = TRUE)
#'
#' # plot probability curves (predicted probabilities)
#' # for each covariate, grouped by random intercepts
#' # in integrated plots, emphasizing groups 1 and 4
#' sjp.glmer(fit,
#'           type = "ri.pc",
#'           emph.grp = c(1, 4),
#'           facet.grid = FALSE)
#'
#' # plot probability curve (predicted probabilities)
#' # of fixed effect, only for coefficient "neg_c_7"
#' sjp.glmer(fit,
#'           type = "fe.pc",
#'           vars = "neg_c_7")
#'
#' @import ggplot2
#' @importFrom dplyr slice add_rownames sample_n
#' @importFrom lme4 fixef ranef confint.merMod getME
#' @export
sjp.glmer <- function(fit,
                      type = "re",
                      vars = NULL,
                      ri.nr = NULL,
                      emph.grp = NULL,
                      title = NULL,
                      geom.size = NULL,
                      geom.colors = "Set1",
                      hideErrorBars = FALSE,
                      showIntercept = TRUE,
                      stringIntercept = "(Intercept)",
                      sort.coef = NULL,
                      pred.labels = NULL,
                      axisTitle.x = NULL,
                      axisTitle.y = NULL,
                      axisLimits.y = NULL,
                      facet.grid = TRUE,
                      free.scale = FALSE,
                      interceptLineType = 2,
                      interceptLineColor = "grey70",
                      remove.estimates = NULL,
                      showValueLabels = TRUE,
                      labelDigits = 2,
                      y.offset = .1,
                      showPValueLabels = TRUE,
                      fade.ns = FALSE,
                      show.ci = FALSE,
                      sample.n = NULL,
                      show.legend = FALSE,
                      printPlot = TRUE) {
  if (type == "fe.prob") type <- "fe.pc"
  if (type == "ri.prob") type <- "ri.pc"
  if (type == "y.prob") type <- "y.pc"

  sjp.lme4(fit,
           type,
           vars,
           ri.nr,
           emph.grp,
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
           axisLimits.y,
           interceptLineType,
           interceptLineColor,
           remove.estimates,
           showValueLabels,
           labelDigits,
           y.offset,
           showPValueLabels,
           facet.grid,
           free.scale,
           fade.ns,
           show.ci,
           printPlot,
           fun = "glm",
           0.2,
           TRUE,
           FALSE,
           FALSE,
           NULL,
           sample.n,
           show.legend)
}


#' @title Plot estimates or predicted values of linear mixed effects models
#' @name sjp.lmer
#'
#' @seealso \href{http://www.strengejacke.de/sjPlot/sjp.lmer/}{sjPlot manual: sjp.lmer}
#'
#' @description By default, this function plots estimates (coefficients) with confidence
#'                intervalls of either fixed effects or random effects of linear mixed
#'                effects models (that have been fitted with the \code{\link[lme4]{lmer}}-function
#'                of the \pkg{lme4}-package). Furhermore, this function also plot
#'                predicted values or diagnostic plots.
#'
#' @details \describe{
#'            \item{\code{type = "fe.pred"}}{plots the linear relationship between
#'            each fixed effect and the response. The regression lines are \emph{not}
#'            based on the fitted model's fixed effects estimates (though they may
#'            be similar). This plot type just computes a simple linear model for
#'            each fixed effect and response. Hence, it's intended for checking
#'            model assumptions, i.e. if predictor and respone are in a linear relationship.
#'            You may use the \code{showLoess} argument to see whether the linear
#'            line differs from the best fitting line.}
#'            \item{\code{type = "fe.resid"}}{Similar to \code{type = "fe.pred"},
#'            this this type is intended for checking model assumptions. However,
#'            fitted values are plotted against the residuals instead of response.}
#'            \item{\code{type = "eff"}}{plots the adjusted (marginal) effects
#'            for each fixed effect, with all co-variates set to the mean, as 
#'            returned by the \code{\link[effects]{allEffects}} function.}
#'            \item{\code{type = "rs.ri"}}{plots regression lines for the random
#'            parts of the model, i.e. all random slopes for each random intercept.
#'            As the random intercepts describe the deviation from the global intercept,
#'            the regression lines are computed as global intercept + random intercept +
#'            random slope. In case of overplotting,
#'            use the \code{sample.n} argument to randomly sample a limited amount
#'            of groups.}
#'            \item{\code{type = "fe.ri"}}{plots regression lines for each fixed
#'            effect (slopes) within each random intercept. The lines in these plots
#'            have random intercept values as intercept. For each fixed effect,
#'            a new figure is plotted, where the same slope (of the fixed effect)
#'            is used for each plot.}
#'            \item{\code{type = "coef"}}{forest plot of joint fixed and random
#'            effect coefficients, as retrieved by \code{\link[lme4]{coef.merMod}},
#'            it's simply \code{\link[lme4]{ranef} + \link[lme4]{fixef}}.}
#'            \item{\code{type = "resp"}}{regression line for
#'            predicted values against response, only fixed effects and 
#'            conditional on random intercept. It's calling
#'            \code{predict(fit, type = "response", re.form = NA)} resp.
#'            \code{predict(fit, type = "response", re.form = NULL)} to
#'            compute the values.}
#'          }
#'
#' @param fit a fitted model as returned by the \code{\link[lme4]{lmer}}-function.
#' @param type type of plot. Use one of following:
#'          \describe{
#'            \item{\code{"re"}}{(default) for estimates of random effects as forest plot}
#'            \item{\code{"fe"}}{for estimates of fixed effects as forest plot}
#'            \item{\code{"fe.std"}}{for standardized estimates of fixed effects as forest plot}
#'            \item{\code{"fe.pred"}}{to plot regression lines (slopes) with confidence intervals for each single fixed effect, i.e. all fixed terms are extracted and each is plotted against the response variable (linear relationship between each fixed term and response)}
#'            \item{\code{"fe.resid"}}{to plot regression lines (slopes) with confidence intervals for each single fixed effect (against residuals), i.e. all fixed terms are extracted and each is plotted against the model residuals (linear relationship between each fixed term and residuals)}
#'            \item{\code{"fe.cor"}}{for correlation matrix of fixed effects}
#'            \item{\code{"re.qq"}}{for a QQ-plot of random effects (random effects quantiles against standard normal quantiles)}
#'            \item{\code{"fe.ri"}}{for fixed effects slopes depending on the random intercept.}
#'            \item{\code{"rs.ri"}}{for fitted regression lines indicating the random slope-intercept pairs. Use this to visualize the random parts of random slope-intercept (or repeated measure) models. When having too many groups, use \code{sample.n} argument.}
#'            \item{\code{"coef"}}{for joint (sum of) random and fixed effects coefficients for each explanatory variable for each level of each grouping factor as forest plot.}
#'            \item{\code{"resp"}}{to plot predicted values for the response, with and without random effects. Use \code{facet.grid} to decide whether to plot with and w/o random effect plots as separate plot or as integrated faceted plot.}
#'            \item{\code{"eff"}}{to plot marginal effects of all fixed terms in \code{fit}. Note that interaction terms are excluded from this plot; use \code{\link{sjp.int}} to plot effects of interaction terms. See also 'Details' of \code{\link{sjp.lm}}.}
#'            \item{\code{"poly"}}{to plot predicted values (marginal effects) of polynomial terms in \code{fit}. Use \code{poly.term} to specify the polynomial term in the fitted model (see 'Examples' here and 'Details' of \code{\link{sjp.lm}}).}
#'            \item{\code{"ma"}}{to check model assumptions. Note that no further arguments except \code{fit} are relevant for this option. All other arguments are ignored.}
#'          }
#' @param pointAlpha alpha value of point-geoms in the scatter plots.
#'          Default is 0.2.
#' @param showScatterPlot logical, if \code{TRUE} (default), a scatter plot of
#'          response and predictor values for each predictor of the fitted
#'          model \code{fit} is plotted. Only applies if \code{type = "fe.pred"}
#'          or \code{type = "fe.resid"}.
#' @param showLoess logical, if \code{TRUE}, an additional loess-smoothed line is plotted.
#'          For \code{\link{sjp.lmer}}, only applies \code{type = "fe.pred"} or
#'          \code{type = "fe.resid"}.
#' @param showLoessCI logical, if \code{TRUE}, a confidence region for the loess-smoothed line
#'          will be plotted. Default is \code{FALSE}. Only applies, if \code{showLoess = TRUE}
#'          (and for \code{\link{sjp.lmer}}, only applies if \code{type = "fe.pred"}
#'          or \code{type = "fe.resid"}).
#' @param poly.term name of a polynomial term in \code{fit} as string. Needs to be
#'          specified, if \code{type = "poly"}, in order to plot marginal effects
#'          for polynomial terms. See 'Examples'.
#'
#' @inheritParams sjp.glmer
#' @inheritParams sjp.grpfrq
#' @inheritParams sjp.lm
#'
#' @return (Insisibily) returns
#'          \itemize{
#'            \item the ggplot-object (\code{plot}), if \code{type = "fe"} or if \code{type = "re"} and \code{facet.grid = TRUE}). Multiple plots (\code{type = "re"} and if \code{facet.grid = FALSE}) are returned in the object \code{plot.list}.
#'            \item a list of ggplot-objects (\code{plot.list}). see \code{plot} for details.
#'            \item a data frame \code{data} with the data used to build the ggplot-object(s).
#'            }
#'
#' @note Computation of p-values (if necessary) are based on conditional F-tests
#'         with Kenward-Roger approximation for the df, using the \pkg{pbkrtest}-package.
#'         If \pkg{pbkrtest} is not available, Wald chi-squared tests from the 
#'         \code{Anova}-function of the \pkg{car}-package are computed.
#'
#' @examples
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
#'
#' library(sjmisc)
#' data(efc)
#' # prepare group variable
#' efc$grp = as.factor(efc$e15relat)
#' levels(x = efc$grp) <- get_labels(efc$e15relat)
#' # data frame for fitted model
#' mydf <- data.frame(neg_c_7 = as.numeric(efc$neg_c_7),
#'                    sex = as.factor(efc$c161sex),
#'                    c12hour = as.numeric(efc$c12hour),
#'                    barthel = as.numeric(efc$barthtot),
#'                    grp = efc$grp)
#' # fit lmer
#' fit <- lmer(neg_c_7 ~ sex + c12hour + barthel + (1|grp),
#'             data = mydf)
#'
#' # plot fixed effects
#' sjp.lmer(fit, type = "fe")
#'
#  # plot and sort standardized fixed effects
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
#' # highlight specific grouping levels
#' # in this case we compare spouses, children
#' # and children-in-law
#' sjp.lmer(fit,
#'          type = "fe.ri",
#'          emph.grp = c(1, 2, 4),
#'          vars = "c12hour")
#'
#' \dontrun{
#' # --------------------------
#' # plotting polynomial terms
#' # --------------------------
#' # check linear relation between predictors and response
#' sjp.lmer(fit, type = "fe.pred")
#'
#' # "barthel" does not seem to be linear correlated to response
#' # try to find appropiate polynomial. Grey line (loess smoothed)
#' # indicates best fit. Looks like x^4 has the best fit,
#' # however, x^2 seems to be suitable according to p-values.
#' sjp.poly(fit, "barthel", 2:4, showScatterPlot = FALSE)
#'
#' # fit new model
#' fit <- lmer(neg_c_7 ~ sex + c12hour + barthel +
#'             I(barthel^2) + (1|grp), data = mydf)
#'
#' # plot marginal effects of polynomial term
#' sjp.lmer(fit, type = "poly", poly.term = "barthel")
#'
#'
#' # lme4 complaints about scale of polynomial term, so
#' # try centering this predictor
#' mydf$barthel_s <- scale(mydf$barthel, center = TRUE, scale = TRUE)
#'
#' # re-fit model
#' fit_s <- lmer(neg_c_7 ~ sex + c12hour + barthel_s +
#'               I(barthel_s^2) + (1|grp), data = mydf)
#'
#' # plot marginal effects of centered, scaled polynomial term
#' sjp.lmer(fit_s, type = "poly", poly.term = "barthel_s")
#'
#' # scaling also improved p-values
#' sjt.lmer(fit, fit_s)}
#'
#' @import ggplot2
#' @importFrom car Anova
#' @importFrom dplyr sample_n add_rownames slice
#' @export
sjp.lmer <- function(fit,
                     type = "re",
                     vars = NULL,
                     ri.nr = NULL,
                     emph.grp = NULL,
                     title = NULL,
                     geom.size = NULL,
                     geom.colors = "Set1",
                     hideErrorBars = FALSE,
                     showIntercept = TRUE,
                     stringIntercept = "(Intercept)",
                     sort.coef = NULL,
                     pred.labels = NULL,
                     axisTitle.x = NULL,
                     axisTitle.y = NULL,
                     axisLimits.y = NULL,
                     interceptLineType = 2,
                     interceptLineColor = "grey70",
                     remove.estimates = NULL,
                     showValueLabels = TRUE,
                     labelDigits = 2,
                     y.offset = .1,
                     showPValueLabels = TRUE,
                     facet.grid = TRUE,
                     free.scale = FALSE,
                     fade.ns = FALSE,
                     show.ci = TRUE,
                     pointAlpha = 0.2,
                     showScatterPlot = TRUE,
                     showLoess = FALSE,
                     showLoessCI=FALSE,
                     poly.term = NULL,
                     sample.n = NULL,
                     show.legend = FALSE,
                     printPlot = TRUE) {
  if (type == "fe.prob") type <- "fe.pc"
  if (type == "ri.prob") type <- "ri.pc"
  if (type == "resp") type <- "y.pc"

  sjp.lme4(fit,
           type,
           vars,
           ri.nr,
           emph.grp,
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
           axisLimits.y,
           interceptLineType,
           interceptLineColor,
           remove.estimates,
           showValueLabels,
           labelDigits,
           y.offset,
           showPValueLabels,
           facet.grid,
           free.scale,
           fade.ns,
           show.ci,
           printPlot,
           fun = "lm",
           pointAlpha,
           showScatterPlot,
           showLoess,
           showLoessCI,
           poly.term,
           sample.n,
           show.legend)
}

sjp.lme4  <- function(fit,
                      type,
                      vars,
                      ri.nr,
                      emph.grp,
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
                      axisLimits.y,
                      interceptLineType,
                      interceptLineColor,
                      remove.estimates,
                      showValueLabels,
                      labelDigits,
                      y.offset,
                      showPValueLabels,
                      facet.grid,
                      free.scale,
                      fade.ns,
                      show.ci,
                      printPlot,
                      fun,
                      pointAlpha = 0.2,
                      showScatterPlot = TRUE,
                      showLoess = FALSE,
                      showLoessCI = FALSE,
                      poly.term = NULL,
                      sample.n = NULL,
                      show.legend = FALSE) {
  # ------------------------
  # check if suggested package is available
  # ------------------------
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package `lme4` needed for this function to work. Please install it.", call. = FALSE)
  }
  if (!requireNamespace("arm", quietly = TRUE)) {
    stop("Package `arm` needed for this function to work. Please install it.", call. = FALSE)
  }
  # -------------------------------------
  # check type
  # -------------------------------------
  if (type != "re" && type != "fe" && type != "fe.std" && type != "fe.cor" &&
      type != "re.qq" && type != "fe.pc" && type != "ri.pc" && type != "fe.pred" &&
      type != "fe.prob" && type != "ri.prob" && type != "fe.ri" && type != "y.pc" &&
      type != "fe.resid" && type != "poly" && type != "eff" && type != "coef" && 
      type != "rs.ri" && type != "ma") {
    warning("Invalid option for `type` argument. Defaulting to `type = \"fe\"` now.")
    type  <- "fe"
  }
  # ---------------------------------------
  # remember whether predictor labels
  # are empty
  # ---------------------------------------
  empty.pred.labels <- is.null(pred.labels)
  # ---------------------------------------
  # for standardized coefficients, intercept
  # is always 0, so no need to be shown
  # ---------------------------------------
  if (type == "fe.std") showIntercept <- FALSE
  # ---------------------------------------
  # get amount of random intercepts
  # ---------------------------------------
  ri.cnt <- length(lme4::ranef(fit))
  # ---------------------------------------
  # how many plots? if we have random effects plots,
  # determine number of random effects and iterate
  # all effects
  # ---------------------------------------
  loops <- 1
  if (type == "re" || type == "fe.ri" || type == "rs.ri" || type == "ri.pc" || type == "coef") {
    # ---------------------------------------
    # do we have a specific random intercept
    # specified? If yes, check valid index
    # ---------------------------------------
    if (!is.null(ri.nr)) {
      # ---------------------------------------
      # check amounnt of random intercepts
      # ---------------------------------------
      out.of.bounds <- which(ri.nr > ri.cnt)
      if (length(out.of.bounds) > 0) {
        # ---------------------------------------
        # remove out of bound indices
        # ---------------------------------------
        ri.nr <- ri.nr[-out.of.bounds]
        # ---------------------------------------
        # any valid indices left?
        # ---------------------------------------
        if (length(ri.nr) == 0) {
          warning("All indices specified in `ri.nr` were greater than amount of random intercepts in model. Please use valid range for `ri.nr`.", call. = F)
          return(invisible(NULL))
        } else {
          message("One or more indices specified in `ri.nr` were greater than amount of random intercepts in model. These indices have been removed from `ri.nr`.")
        }
      }
      # ---------------------------------------
      # our looping counter contains all rand. int.
      # indices
      # ---------------------------------------
      loops <- ri.nr
    } else {
      # ---------------------------------------
      # else, if ri.nr was NULL, plot all random
      # intercepts, i.e. looping counter contains
      # all index numbers
      # ---------------------------------------
      loops <- ri.nr <- seq(ri.cnt)
    }
    # ---------------------------------------
    # Check valid index of highlighted group levels
    # ---------------------------------------
    if (!is.null(emph.grp)) {
      # ---------------------------------------
      # emphasizing groups does only work if
      # plot is not faceted!
      # ---------------------------------------
      if (facet.grid) {
        message("Emphasizing groups only works in non-faceted plots. Use `facet.grid = FALSE` to enable group emphasizing. `emph.grp` will now be ignored.")
        emph.grp <- NULL
      } else {
        # ---------------------------------------
        # get random effects
        # ---------------------------------------
        rand.ef <- lme4::ranef(fit)[[1]]
        # ---------------------------------------
        # if "emph.grp" is numeric, check
        # correct index values
        # ---------------------------------------
        if (is.numeric(emph.grp)) {
          out.of.bounds <- which(emph.grp > nrow(rand.ef))
        } else {
          # find matching groupin levels, and automatically
          # convert character to numeric indices
          emph.grp <- match(emph.grp, row.names(rand.ef))
          out.of.bounds <- which(is.na(emph.grp))
        }
        if (length(out.of.bounds) > 0) {
          # ---------------------------------------
          # remove out of bound indices
          # ---------------------------------------
          emph.grp <- emph.grp[-out.of.bounds]
          # ---------------------------------------
          # any valid indices left?
          # ---------------------------------------
          if (length(emph.grp) == 0) {
            warning("No index value in `emph.grp` matches any grouping level. Please use valid values for `emph.grp`.", call. = F)
            return(invisible(NULL))
          }
        }
      }
    }
  }
  # ---------------------------------------
  # plot correlation matrix of fixed effects,
  # to inspect multicollinearity
  # ---------------------------------------
  if (type == "ma") {
    if (fun == "lm")
      return(invisible(sjp.lm.ma(fit)))
    else
      return(invisible(sjp.glmer.ma(fit)))
  } else if (type == "fe.cor") {
    # ---------------------------------------
    # plot correlation matrix of fixed effects,
    # to inspect multicollinearity
    # ---------------------------------------
    return(invisible(sjp.lme.fecor(fit,
                                   pred.labels,
                                   sort.coef,
                                   fun,
                                   printPlot)))
  } else if (type == "fe.pred" || type == "fe.resid") {
    # ---------------------------------------
    # plot slopes for each fixed coefficient
    # ---------------------------------------
    if (fun == "lm") {
      # reset default color setting, does not look that good.
      if (geom.colors == "Set1") geom.colors <- NULL
      return(invisible(sjp.reglin(fit = fit,
                                  title = title,
                                  geom.colors = geom.colors,
                                  showCI = show.ci,
                                  pointAlpha = pointAlpha,
                                  showScatterPlot = showScatterPlot,
                                  showLoess = showLoess,
                                  showLoessCI = showLoessCI,
                                  useResiduals = ifelse(type == "fe.pred", FALSE, TRUE),
                                  printPlot = printPlot)))
    } else {
      warning("Plotting slopes of fixed effects only works for function `sjp.lmer`.", call. = FALSE)
      return(invisible(NULL))
    }
  } else if (type == "poly") {
    # ---------------------------------------
    # plot marginal effects for polynimial terms
    # ---------------------------------------
    if (fun == "lm") {
      return(invisible(sjp.lm.poly(fit,
                                   poly.term,
                                   geom.colors,
                                   geom.size,
                                   axisTitle.x,
                                   axisTitle.y,
                                   showCI = show.ci,
                                   printPlot)))
    } else {
      warning("Plotting polynomial terms only works for function `sjp.lmer`.", call. = FALSE)
      return(invisible(NULL))
    }
  } else if (type == "eff") {
    # ---------------------------------------
    # plot marginal effects of fixed terms
    # ---------------------------------------
    if (fun == "lm") {
      return(invisible(sjp.lm.eff(fit,
                                  title,
                                  geom.size,
                                  remove.estimates,
                                  showCI = show.ci,
                                  printPlot)))
    } else {
      return(invisible(sjp.glm.eff(fit,
                                   title,
                                   geom.size,
                                   remove.estimates,
                                   showCI = show.ci,
                                   axisLimits.y = axisLimits.y,
                                   printPlot)))
    }
  } else if (type == "fe.ri") {
    # ---------------------------------------
    # plot slopes for each fixex coefficient
    # depending on random intercept levels
    # ---------------------------------------
    if (fun == "lm") {
      return(invisible(sjp.lme.feri(fit,
                                    ri.nr,
                                    vars,
                                    emph.grp,
                                    geom.size,
                                    printPlot)))
    } else {
      warning("Fixed effects plots by random intercept effects (grouping levels) only works for function `sjp.lmer`.", call. = FALSE)
      return(invisible(NULL))
    }
  } else if (type == "rs.ri") {
    return(invisible(sjp.lme.reri(fit,
                                  title,
                                  axisTitle.x,
                                  axisTitle.y,
                                  ri.nr,
                                  emph.grp,
                                  geom.colors,
                                  geom.size,
                                  sample.n,
                                  show.legend,
                                  axisLimits.y,
                                  printPlot,
                                  fun)))
  } else if (type == "re.qq") {
    # ---------------------------------------
    # plot qq-plots for random effects to
    # inspect normality
    # ---------------------------------------
    return(invisible(sjp.lme.reqq(fit,
                                  geom.colors,
                                  geom.size,
                                  hideErrorBars,
                                  interceptLineType,
                                  interceptLineColor,
                                  fun,
                                  printPlot)))
  } else if (type == "fe.pc") {
    # ---------------------------------------
    # plot predicted probabilities of
    # fixed effects
    # ---------------------------------------
    if (fun == "glm") {
      return(invisible(sjp.lme.feprobcurv(fit,
                                          show.ci,
                                          facet.grid,
                                          vars,
                                          geom.size,
                                          axisLimits.y,
                                          printPlot)))
    } else {
      warning("Probability plots of fixed effects only works for function `sjp.glmer`.", call. = FALSE)
      return(invisible(NULL))
    }
  } else if (type == "ri.pc") {
    # ---------------------------------------
    # plot predicted probabilities of
    # random intercepts
    # ---------------------------------------
    if (fun == "glm") {
      return(invisible(sjp.lme.reprobcurve(fit,
                                           show.ci,
                                           facet.grid,
                                           ri.nr,
                                           vars,
                                           emph.grp,
                                           axisLimits.y,
                                           printPlot)))
    } else {
      warning("Probability plots of random intercept effects only works for function `sjp.glmer`.", call. = FALSE)
      return(invisible(NULL))
    }
  } else if (type == "y.pc") {
    # ---------------------------------------
    # plot predicted probabilities / values of
    # response value
    # ---------------------------------------
    return(invisible(sjp.lme.response.probcurv(fit,
                                               show.ci,
                                               facet.grid,
                                               axisLimits.y,
                                               fun,
                                               printPlot)))
  }
  # ---------------------------------------
  # check geom size
  # ---------------------------------------
  if (is.null(geom.size)) geom.size <- 3
  # ---------------------------------------
  # init plot list
  # ---------------------------------------
  me.plot.list <- list()
  # ---------------------------------------
  # start plotting here. for fixed effects,
  # only one plot. For random effect, we loop
  # the plotting for each specifief random
  # intercept (see above)
  # ---------------------------------------
  for (lcnt in loops) {
    # ---------------------------------------
    # clear mydf
    # ---------------------------------------
    mydf <- data.frame()
    # ---------------------------------------
    # check whether random or fixed effects
    # should be plotted
    # ---------------------------------------
    if (type == "re" || type == "coef") {
      # ---------------------------------------
      # if we have only one random intercept, and facet.grid
      # not specified, default it to false
      # ---------------------------------------
      if (1 == ri.cnt && missing(facet.grid)) facet.grid <- FALSE
      # ---------------------------------------
      # copy estimates of random effects
      # ---------------------------------------
      if (type == "coef") {
        mydf.ef <- as.data.frame(coef(fit)[[lcnt]])
      } else {
        mydf.ef <- as.data.frame(lme4::ranef(fit)[[lcnt]])
      }
      # ---------------------------------------
      # copy rownames as axis labels, if not set
      # ---------------------------------------
      if (empty.pred.labels) {
        # use rownames, if pred.labels not available
        pred.labels <- rownames(mydf.ef)
      }
      # ---------------------------------------
      # retrieve standard errors, for ci
      # ---------------------------------------
      if (type == "coef") {
        se.fit <- data.frame(t(sjmisc::se(fit)[[lcnt]]))
      } else {
        se.fit <- arm::se.ranef(fit)[[lcnt]]
      }
      # ---------------------------------------
      # select random effects for each coefficient
      # ---------------------------------------
      for (i in 1:ncol(mydf.ef)) {
        # ---------------------------------------
        # create data frame
        # 1. col: odds ratios /estimates of re-estimates
        # 2. col.: lower conf int
        # 3. col: upper conf int
        # ---------------------------------------
        if (fun == "glm") {
          tmp <- data.frame(estimate = exp(mydf.ef[, i]),
                            conf.low = exp(mydf.ef[, i] - (1.96 * se.fit[, i])),
                            conf.high = exp(mydf.ef[, i] + (1.96 * se.fit[, i])))
        } else {
          tmp <- data.frame(estimate = mydf.ef[, i],
                            conf.low = mydf.ef[, i] - (1.96 * se.fit[, i]),
                            conf.high = mydf.ef[, i] + (1.96 * se.fit[, i]))
        }
        # ---------------------------------------
        # set column names (variable / coefficient name)
        # as group indicator
        # ---------------------------------------
        tmp$grp <- colnames(mydf.ef)[i]
        # ---------------------------------------
        # sort data frame. init order
        # ---------------------------------------
        reihe <- c(1:nrow(tmp))
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
            } else {
              # ---------------------------------------
              # sort odds ratios of random effects
              # for current coefficient
              # ---------------------------------------
              reihe <- order(mydf.ef[, i])
            }
          } else {
            # ---------------------------------------
            # else, just sort a specific coefficient
            # this also works with facet.grid
            # ---------------------------------------
            reihe <- order(mydf.ef[, sort.coef])
          }
          # ---------------------------------------
          # sort data frame
          # ---------------------------------------
          tmp <- tmp[reihe, ]
        }
        # ---------------------------------------
        # save sorting order to data frame, so
        # axis labels can be sorted accordingly later
        # ---------------------------------------
        tmp$sorting <- reihe
        # no p-values for random effects,
        # but value labels
        ps <- rep("", nrow(tmp))
        if (showValueLabels) ps <- sprintf("%.*f", labelDigits, tmp$estimate)
        tmp$p.string <- ps
        tmp$p.value <- NA
        # ---------------------------------------
        # add to final data frame
        # ---------------------------------------
        mydf <- data.frame(rbind(mydf, tmp))
      }
      # ---------------------------------------
      # fixed effects, odds ratios
      # ---------------------------------------
    } else if (type == "fe" || type == "fe.std") {
      # ---------------------------------------
      # retrieve odds ratios and conf int of
      # fixed effects
      # ---------------------------------------
      if (fun == "glm") {
        if (type == "fe.std") {
          warning("'type = fe.std' only works for linear models.", call. = F)
        }
        # get odds ratios and cleaned CI
        mydf <- get_cleaned_ciMerMod(fit, fun)
      } else {
        if (type == "fe.std") {
          tmpdf <- sjmisc::std_beta(fit)
          mydf <- data.frame(estimate = tmpdf$stdcoef,
                             conf.low = tmpdf$stdcoef - (1.96 * tmpdf$stdse),
                             conf.high = tmpdf$stdcoef + (1.96 * tmpdf$stdse))
          # set default row names
          rownames(mydf) <- names(lme4::fixef(fit))
        } else {
          # get odds ratios and cleaned CI
          mydf <- get_cleaned_ciMerMod(fit, fun)
        }
      }
      # ----------------------------
      # retrieve sigificance level of independent variables (p-values)
      # ----------------------------
      pv <- get_lmerMod_pvalues(fit)
      # ----------------------------
      # retrieve odds ratios resp.
      # betas or standardized betas
      # ----------------------------
      if (fun == "glm") {
        ov <- exp(lme4::fixef(fit))
      } else {
        if (type == "fe.std") {
          ov <- sjmisc::std_beta(fit)$stdcoef
        } else {
          ov <- lme4::fixef(fit)
        }
      }
      # init data column for p-values
      ps <- rep("", length(ov))
      # ----------------------------
      # copy estimate-values into data column
      # ----------------------------
      if (showValueLabels) ps <- sprintf("%.*f", labelDigits, ov)
      # ----------------------------
      # copy p-values into data column
      # for better readability, convert p-values to asterisks
      # with:
      # p < 0.001 = ***
      # p < 0.01 = **
      # p < 0.05 = *
      # ----------------------------
      if (showPValueLabels) {
        for (i in 1:length(pv)) {
          ps[i] <- sjmisc::trim(paste(ps[i], get_p_stars(pv[i])))
        }
      }
      # bind p-values
      mydf$p.string <- ps
      mydf$p.value <- pv
      # ---------------------------------------
      # set proper column names
      # ---------------------------------------
      colnames(mydf) <- c("estimate", "conf.low", "conf.high", "p.string", "p.value")
      # ---------------------------------------
      # just one group, so no faceting needed
      # ---------------------------------------
      mydf$grp <- c("1")
      facet.grid <- FALSE
      if (is.null(title)) title <- ifelse(type == "fe.std", "Standardized fixed effects", "Fixed effects")
      # ---------------------------------------
      # show intercept?
      # ---------------------------------------
      if (!showIntercept) mydf <- mydf[-1, ]
      # -------------------------------------------------
      # remove any estimates from the output?
      # -------------------------------------------------
      if (!is.null(remove.estimates)) {
        # get row indices of rows that should be removed
        remrows <- match(remove.estimates, row.names(mydf))
        # remember old rownames
        keepnames <- row.names(mydf)[-remrows]
        # remove rows
        mydf <- dplyr::slice(mydf, c(1:nrow(mydf))[-remrows])
        # set back rownames
        row.names(mydf) <- keepnames
        # remove labels?
        if (!empty.pred.labels && length(pred.labels) > nrow(mydf))
          pred.labels <- pred.labels[-remrows]
      }
      # ---------------------------------------
      # copy rownames as axis labels, if not set
      # ---------------------------------------
      if (empty.pred.labels) {
        pred.labels <- rownames(mydf)
      } else {
        # check if intercept should be added, in case
        # pred.labels are passed
        if (showIntercept) pred.labels <- c(stringIntercept, pred.labels)
      }
      # ---------------------------------------
      # sort data frame. init order
      # ---------------------------------------
      reihe <- c(1:nrow(mydf))
      # ---------------------------------------
      # just one sorting option, simply sort odds ratios
      # ---------------------------------------
      if (!is.null(sort.coef)) {
        reihe <- order(mydf$estimate)
        mydf <- mydf[reihe, ]
      }
      mydf$sorting <- reihe
    }
    # ---------------------------------------
    # remove specific estimates?
    # ---------------------------------------
    if (!is.null(vars)) {
      # find estimates that should be removed
      remes <- which(!is.na(match(rownames(mydf), vars)))
      # remove data rows for these estimates
      mydf <- mydf[remes, ]
      # also remove predictor labels
      pred.labels <- pred.labels[remes]
      # re-arrange sorting
      mydf$sorting <- order(mydf$sorting)
    }
    # ---------------------------------------
    # check length labels
    # ---------------------------------------
    if (length(pred.labels) != nrow(mydf) && 
        (length(pred.labels) != (nrow(mydf) / length(unique(mydf$grp))))) {
      warning("`pred.labels` has insufficient length. Using row names.", call. = F)
      pred.labels <- row.names(mydf)
    }
    # ---------------------------------------
    # discrete x position, needed for ggplot
    # ---------------------------------------
    mydf$x <- as.factor(1:length(pred.labels))
    # ---------------------------------------
    # set indicator whether or not non significant
    # odds ratios should be faded.
    # ---------------------------------------
    if (isTRUE(fade.ns)) {
      interc <- ifelse(fun == "glm", 1, 0)
      mydf$fade <- (mydf$conf.low < interc & mydf$conf.high > interc)
    } else {
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
      interc <- ifelse(fun == "glm", 1, 0)
      mydf$interc <- interc
      gp <- ggplot(mydf, aes(x = x,
                             y = estimate,
                             colour = (estimate > interc),
                             alpha = fade)) +
        # Intercept-line
        geom_hline(yintercept = interc,
                   linetype = interceptLineType,
                   color = interceptLineColor) +
        geom_point(size = geom.size) +
        # print value labels and p-values
        geom_text(aes(label = p.string, y = estimate), nudge_x = y.offset) +
        # ---------------------------------------
      # labels in sorted order
      # ---------------------------------------
      scale_x_discrete(labels = pred.labels[mydf$sorting]) +
        # ---------------------------------------
      # fade non significant estimate
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
                                      breaks = base_breaks(ceiling(max(mydf$conf.high, na.rm = T))),
                                      labels = prettyNum)
      }
      # ---------------------------------------
      # hide error bars (conf int)?
      # ---------------------------------------
      if (!hideErrorBars)  gp <- gp +
          geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0)
      # ---------------------------------------
      # axis titles
      # ---------------------------------------
      if (type == "fe" || type == "fe.std") {
        if (is.null(axisTitle.x)) axisTitle.x <- ""
        if (is.null(axisTitle.y)) axisTitle.y <- ""
      } else if (type == "re") {
        if (is.null(axisTitle.x)) axisTitle.x <- "Group levels"
        if (is.null(axisTitle.y)) axisTitle.y <- ""
      }
      # ---------------------------------------
      # add facet grid here, faceting by group
      # (level) of random intercept
      # ---------------------------------------
      if (facet.grid) {
        gp <- gp + labs(x = axisTitle.x, y = axisTitle.y)
        # check if user wants free scale for each facet
        if (free.scale)
          gp  <- gp + facet_wrap(~grp, scales = "free_y")
        else
          gp  <- gp + facet_grid(~grp)
      } else {
        gp <- gp +
          labs(x = axisTitle.x, y = axisTitle.y, title = title)
      }
      return(gp)
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
      me.plot.list[[length(me.plot.list) + 1]]  <- me.plot
      # ---------------------------------------------------------
      # Check whether ggplot object should be returned or plotted
      # ---------------------------------------------------------
      if (printPlot) print(me.plot)
      me.plot <- NULL
    } else {
      # ---------------------------------------
      # single plots means, each coefficient is
      # plotted to an own figure
      # ---------------------------------------
      groups <- unique(mydf$grp)
      # ---------------------------------------
      # set title for plots (coefficient label)
      # ---------------------------------------
      if (is.null(title)) title <- paste("Random effects of ", as.character(groups))
      # ---------------------------------------
      # iterate coefficients
      # ---------------------------------------
      for (j in 1:length(groups)) {
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
        me.plot.list[[length(me.plot.list) + 1]]  <- me.plot
        # ---------------------------------------------------------
        # Check whether ggplot object should be returned or plotted
        # ---------------------------------------------------------
        if (printPlot) print(me.plot)
        me.plot <- NULL
      }
    }
  }
  # me plot contains first of all plots...
  me.plot <- me.plot.list[[1]]
  # -------------------------------------
  # add term names
  # -------------------------------------
  if (type == "fe" || type == "fe.std") {
    mydf <- dplyr::add_rownames(mydf, var = "term")
  }
  # -------------------------------------
  # return results
  # -------------------------------------
  invisible(structure(class = ifelse(fun == "glm", c("sjPlot", "sjpglmer"), c("sjPlot", "sjplmer")),
                      list(plot = me.plot,
                           plot.list = me.plot.list,
                           data = mydf)))
}


sjp.lme.feprobcurv <- function(fit,
                               show.ci,
                               facet.grid,
                               vars,
                               geom.size,
                               axisLimits.y,
                               printPlot) {
  # check size argument
  if (is.null(geom.size)) geom.size <- .7
  # check axis limits
  if (is.null(axisLimits.y)) axisLimits.y <- c(0, 1)
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
  fit.df <- stats::model.frame(fit)
  # ----------------------------
  # retrieve term names, so we find the estimates in the
  # coefficients list
  # ----------------------------
  fit.term.length <- length(names(lme4::fixef(fit))[-1])
  fit.term.names <- stats::na.omit(attr(attr(fit.df, "terms"), "term.labels")[1:fit.term.length])
  fi <- unname(lme4::fixef(fit))[1]
  # ----------------------------
  # filter vars?
  # ----------------------------
  if (!is.null(vars)) {
    if (is.character(vars)) {
      fit.term.names <- fit.term.names[!is.na(match(fit.term.names, vars))]
    } else {
      fit.term.names <- fit.term.names[vars]
    }
  }
  # ----------------------------
  # plot all terms
  # ----------------------------
  for (i in 1:length(fit.term.names)) {
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
      if (is.factor(mydf.vals$value)) mydf.vals$value <- sjmisc::to_value(mydf.vals$value, 0, keep.labels = F)
      # retrieve names of coefficients
      coef.names <- names(lme4::fixef(fit))
      # check if we have a factor, then we may have reference levels
      if (is.factor(vals)) {
        # add reference level to coefficient name
        ll <- levels(vals)
        fit.fac.name <- paste0(fit.term.names[i], ll[length(ll)])
      } else {
        fit.fac.name <- fit.term.names[i]
      }
      # find coef-position
      coef.pos <- which(coef.names == fit.fac.name)
      # check if we have found the coefficient
      if (length(coef.pos) > 0) {
        # calculate x-beta by multiplying original values with estimate of that term
        mydf.vals$xbeta <- mydf.vals$value * lme4::fixef(fit)[coef.pos]
        # calculate probability (y) via cdf-function
        mydf.vals$y <- plogis(fi + mydf.vals$xbeta)
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
  if (length(mydf.metricpred) > 0) {
    # create mydf for integrated plot
    mydf.ges <- data.frame()
    for (i in 1:length(mydf.metricpred)) {
      # "melt" all single mydf's to one
      mydf.ges <- rbind(mydf.ges, mydf.metricpred[[i]])
      # create single plots for each numeric predictor
      mp <- ggplot(mydf.metricpred[[i]],
                   aes(x = value, y = y)) +
        labs(x = axisLabels.mp[i],
             y = "Predicted Probability") +
        stat_smooth(method = "glm",
                    method.args = list(family = "binomial"),
                    se = show.ci) +
        # cartesian coord still plots range of se, even
        # when se exceeds plot range.
        coord_cartesian(ylim = axisLimits.y)
      # add plot to list
      plot.metricpred[[length(plot.metricpred) + 1]] <- mp
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
        stat_smooth(method = "glm",
                    method.args = list(family = "binomial"),
                    se = show.ci) +
        # cartesian coord still plots range of se, even
        # when se exceeds plot range.
        coord_cartesian(ylim = axisLimits.y) +
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
  return(structure(class = "sjpglmer.fecc",
                   list(mydf.mp = mydf.metricpred,
                        plot.mp = plot.metricpred,
                        mydf.facet = mydf.facet,
                        plot.facet = plot.facet)))
}


#' @importFrom stats model.frame
sjp.lme.reprobcurve <- function(fit,
                                show.ci,
                                facet.grid,
                                ri.nr,
                                vars,
                                emph.grp,
                                axisLimits.y,
                                printPlot) {
  # ----------------------------
  # check axis limits
  # ----------------------------
  if (is.null(axisLimits.y)) axisLimits.y <- c(0, 1)
  # ----------------------------
  # retrieve data frame of model to check whether
  # we have any numeric terms in fitted model
  # ----------------------------
  fit.df <- stats::model.frame(fit)
  # ----------------------------
  # retrieve term names, so we find the estimates in the
  # coefficients list
  # ----------------------------
  plot.prob <- list()
  mydf.prob <- list()
  fit.term.length <- length(names(lme4::fixef(fit))[-1])
  fit.term.names <- stats::na.omit(attr(attr(fit.df, "terms"), "term.labels")[1:fit.term.length])
  response.name <- attr(attr(attr(fit.df, "terms"), "dataClasses"), "names")[1]
  fi <- unname(lme4::fixef(fit))[1]
  # ----------------------------
  # filter vars?
  # ----------------------------
  if (!is.null(vars)) {
    if (is.character(vars)) {
      fit.term.names <- fit.term.names[!is.na(match(fit.term.names, vars))]
    } else {
      fit.term.names <- fit.term.names[vars]
    }
  }
  # ---------------------------------------
  # iterate all random effects
  # ---------------------------------------
  for (ri.count in ri.nr) {
    # retrieve random effects
    rand.ef <- lme4::ranef(fit)[[ri.count]]
    # ------------------------------
    # set geom highlight colors
    # to highlight specific grouping levels
    # ------------------------------
    geom.colors <- NULL
    if (!is.null(emph.grp)) {
      # create color palette
      grp.col <- scales::brewer_pal(palette = "Set1")(length(emph.grp))
      # now set only colors for highlighted groups
      geom.colors <- rep("#999999", length(row.names(rand.ef)))
      geom.colors[emph.grp] <- grp.col
    }
    # ----------------------------
    # loop through all coefficients
    # ----------------------------
    for (i in 1:length(fit.term.names)) {
      # init lists with all additional data frames and plots
      final.df <- data.frame()
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
        if (is.factor(mydf.vals$value)) mydf.vals$value <- sjmisc::to_value(mydf.vals$value, 0, keep.labels = F)
        # retrieve names of coefficients
        coef.names <- names(lme4::fixef(fit))
        # check if we have a factor, then we may have reference levels
        if (is.factor(vals)) {
          # add reference level to coefficient name
          ll <- levels(vals)
          fit.fac.name <- paste0(fit.term.names[i], ll[length(ll)])
        } else {
          fit.fac.name <- fit.term.names[i]
        }
        # find coef-position
        coef.pos <- which(coef.names == fit.fac.name)
        # check if we have found the coefficient
        if (length(coef.pos) > 0) {
          # calculate x-beta by multiplying original values with estimate of that term
          mydf.vals$xbeta <- mydf.vals$value * lme4::fixef(fit)[coef.pos]
          # save predictor name
          pred.name <- fit.term.names[i]
          # do this for each random intercept group
          for (j in 1:nrow(rand.ef)) {
            # calculate probability for each random effect group
            mydf.vals$y <- plogis(fi + rand.ef[j, 1] + mydf.vals$xbeta)
            # add to final data frame
            final.df <- rbind(final.df,
                              cbind(pred = mydf.vals$value,
                                    prob = mydf.vals$y,
                                    grp = j))
          }
          # convert grouping level to factor
          final.df$grp <- as.factor(final.df$grp)
          # retrieve group level label
          levels(final.df$grp) <- row.names(rand.ef)
          # ---------------------------------------------------------
          # prepare base plot
          # ---------------------------------------------------------
          mp <- ggplot(final.df,
                       aes(x = pred, y = prob, colour = grp)) +
            stat_smooth(method = "glm",
                        method.args = list(family = "binomial"),
                        se = show.ci) +
            # cartesian coord still plots range of se, even
            # when se exceeds plot range.
            coord_cartesian(ylim = axisLimits.y) +
            labs(x = NULL,
                 y = "Predicted Probability",
                 title = sprintf("Predicted Probability of %s on %s", pred.name, response.name))
          # ---------------------------------------------------------
          # wrap to facets
          # ---------------------------------------------------------
          if (facet.grid) {
            mp <- mp + facet_wrap(~grp,
                                  ncol = round(sqrt(nrow(rand.ef))),
                                  scales = "free_x") +
              # no legend
              guides(colour = FALSE)
          } else if (!is.null(geom.colors)) {
            # ------------------------------
            # highlight specific groups?
            # ------------------------------
            # set grouping levels as legend labels
            legendLabels <- row.names(rand.ef)
            # set new color scale
            mp <- sj.setGeomColors(mp,
                                   geom.colors,
                                   length(geom.colors),
                                   T,
                                   legendLabels)
          }
          # -------------------------------------
          # add to plot and df list
          # -------------------------------------
          plot.prob[[length(plot.prob) + 1]] <- mp
          mydf.prob[[length(mydf.prob) + 1]] <- final.df
          # -------------------------------------
          # check if metric plots should be plotted
          # -------------------------------------
          if (printPlot) print(mp)
        }
      }
    }
  }
  invisible(structure(class = "sjpglmer.ripc",
                      list(data = mydf.prob,
                           plot = plot.prob)))
}


sjp.lme.response.probcurv <- function(fit,
                                      show.ci,
                                      facet.grid,
                                      axisLimits.y,
                                      fun,
                                      printPlot) {
  # ----------------------------
  # get predicted values for response with and
  # without random effects
  # ----------------------------
  pp.fe <- stats::predict(fit, type = "response", re.form = NA)
  pp.re <- stats::predict(fit, type = "response", re.form = NULL)
  # ----------------------------
  # for glm, get probabilities
  # ----------------------------
  #   if (fun == "glm") {
  #     pp.fe <- plogis(pp.fe)
  #     pp.re <- plogis(pp.re)
  #   }
  # ----------------------------
  # get predicted probabilities for
  # response, including random effects
  # ----------------------------
  mydf <- data.frame(x = 1:length(pp.re),
                     y = sort(pp.re),
                     grp = "Including random effects")
  # ----------------------------
  # get predicted probabilities for
  # response, only fixed effects
  # ----------------------------
  tmp <- data.frame(x = 1:length(pp.fe),
                    y = sort(pp.fe),
                    grp = "Including fixed effects only")
  # bind rows
  mydf <- rbind(mydf, tmp)
  # ------------------------------
  # check axis limits
  # ------------------------------
  if (is.null(axisLimits.y)) {
    axisLimits.y <- c(as.integer(floor(10 * min(mydf$y, na.rm = T) * .9)) / 10,
                      as.integer(ceiling(10 * max(mydf$y, na.rm = T) * 1.1)) / 10)
  }
  # ---------------------------------------------------------
  # Prepare plot
  # ---------------------------------------------------------
  # create single plots for each numeric predictor
  mp <- ggplot(mydf, aes(x = x, y = y, colour = grp))
  if (fun == "glm") {
    mp <- mp +
      labs(x = NULL,
           y = "Predicted Probability",
           title = "Predicted Probabilities for model, conditioned on random and fixed effects only") +
      stat_smooth(method = "glm",
                  method.args = list(family = "binomial"),
                  se = show.ci) +
      # cartesian coord still plots range of se, even
      # when se exceeds plot range.
      coord_cartesian(ylim = axisLimits.y)
  } else {
    mp <- mp +
      labs(x = NULL,
           y = "Predicted values",
           title = "Predicted values for model, conditioned on random and fixed effects only") +
      stat_smooth(method = "lm",
                  se = show.ci)
  }
  if (facet.grid) {
    mp <- mp +
      facet_wrap(~grp, scales = "free_x") +
      guides(colour = FALSE)
  }
  # --------------------------
  # plot plots
  # --------------------------
  if (printPlot) print(mp)
  return(structure(class = "sjpglmer.ppall",
                    list(data = mydf,
                         plot = mp,
                         mean.re = mean(pp.re),
                         mean.fe = mean(pp.fe))))
}


sjp.lme.feri <- function(fit,
                         ri.nr,
                         vars,
                         emph.grp,
                         geom.size,
                         printPlot) {
  # check size argument
  if (is.null(geom.size)) geom.size <- .7
  # -----------------------------------------------------------
  # get model frame
  # -----------------------------------------------------------
  m_f <- stats::model.frame(fit)
  # ----------------------------
  # retrieve term names, so we find the estimates in the
  # coefficients list
  # ----------------------------
  plot.fe <- list()
  mydf.fe <- list()
  all.term.names <- colnames(m_f)
  response.name <- all.term.names[1]
  fit.term.names <- names(lme4::fixef(fit))[-1]
  estimates <- unname(lme4::fixef(fit))[-1]
  fi <- unname(lme4::fixef(fit))[1]
  # ---------------------------------------
  # iterate all random intercept
  # ---------------------------------------
  for (ri.count in ri.nr) {
    # retrieve random effects
    rand.ef <- lme4::ranef(fit)[[ri.count]]
    # and list name
    ri.name <- names(lme4::ranef(fit)[ri.count])
    # ------------------------------
    # set geom highlight colors
    # to highlight specific grouping levels
    # ------------------------------
    geom.colors <- NULL
    if (!is.null(emph.grp)) {
      # create color palette
      grp.col <- scales::brewer_pal(palette = "Set1")(length(emph.grp))
      # now set only colors for highlighted groups
      geom.colors <- rep("#999999", length(row.names(rand.ef)))
      geom.colors[emph.grp] <- grp.col
    }
    # ----------------------------
    # filter vars?
    # ----------------------------
    if (!is.null(vars)) {
      if (is.character(vars)) {
        removers <- !is.na(match(fit.term.names, vars))
      } else {
        removers <- vars
      }
      fit.term.names <- fit.term.names[removers]
      estimates <- estimates[removers]
    }
    # ----------------------------
    # loop through all coefficients
    # ----------------------------
    # slopes for all fixed effects
    for (j in 1:length(estimates)) {
      # reset data frame
      final.df <- data.frame()
      # slopes for each random intercept
      for (i in 1:nrow(rand.ef)) {
        # retrieve intercept
        ri <- rand.ef[i, 1]
        xpos <- NULL
        # find original values for estimates
        for (k in 1:length(all.term.names)) {
          # check if estimate's name matches any column
          # in the data frame of the fitted model
          pos <- grep(all.term.names[k], fit.term.names[j], fixed = T)
          # found?
          if (length(pos) > 0) {
            xpos <- sort(unique(m_f[, k]))
            break
          }
        }
        # check if we found any values...
        if (!is.null(xpos)) {
          final.df <- rbind(final.df,
                            cbind(x = sjmisc::to_value(xpos, keep.labels = F),
                                  y = fi + ri + sjmisc::to_value(xpos, keep.labels = F) * estimates[j],
                                  grp = i))
        }
      }
      # comvert grouping level to factor
      final.df$grp <- as.factor(final.df$grp)
      # retrieve group level label
      levels(final.df$grp)  <- row.names(rand.ef)
      # ------------------------------
      # prepare base plot
      # ------------------------------
      gp <- ggplot(final.df, aes(x = x, y = y, colour = grp)) +
        geom_line(size = geom.size) +
        labs(title = sprintf("Random effect \"%s\"", ri.name),
             x = fit.term.names[j],
             y = response.name)
      # ------------------------------
      # highlight specific groups?
      # ------------------------------
      if (!is.null(geom.colors)) {
        # set grouping levels as legend labels
        legendLabels <- row.names(rand.ef)
        # set new color scale
        gp <- sj.setGeomColors(gp,
                               geom.colors,
                               length(geom.colors),
                               T,
                               legendLabels)
      }
      # -------------------------------------
      # add to plot and df list
      # -------------------------------------
      plot.fe[[length(plot.fe) + 1]] <- gp
      mydf.fe[[length(mydf.fe) + 1]] <- final.df
      # -------------------------------------
      # check if metric plots should be plotted
      # -------------------------------------
      if (printPlot) print(gp)
    }
  }
  invisible(structure(class = "sjplmer.feri",
                      list(data = mydf.fe,
                           plot = plot.fe)))
}


sjp.lme.reri <- function(fit,
                         title,
                         axisTitle.x,
                         axisTitle.y,
                         ri.nr,
                         emph.grp,
                         geom.colors,
                         geom.size,
                         sample.n,
                         show.legend,
                         axisLimits.y,
                         printPlot,
                         fun) {
  # check size argument
  if (is.null(geom.size)) geom.size <- .7
  # -----------------------------------------------------------
  # get model frame
  # -----------------------------------------------------------
  m_f <- stats::model.frame(fit)
  # get predictor names
  pred.values <- colnames(m_f)
  # ----------------------------
  # retrieve term names, so we find the estimates in the
  # coefficients list
  # ----------------------------
  plot.fe <- list()
  mydf.fe <- list()
  # ---------------------------------------
  # global intercept and values from random slope predictor
  # ---------------------------------------
  global.intercept <- as.vector(lme4::fixef(fit))[1]
  # we need to check bounds
  remove_ri <- c()
  for (h in seq_len(length(ri.nr))) {
    # get each random part
    re_tmp <- lme4::ranef(fit)[[ri.nr[h]]]
    # has slopes?
    if (ncol(re_tmp) < 2) 
      remove_ri <- c(remove_ri, h)
  }
  # found any random parts withou slopes? if yes, remove them from index
  if (!sjmisc::is_empty(remove_ri)) {
    ri.nr <- ri.nr[-remove_ri]
  }
  # nothing found?
  if (sjmisc::is_empty(ri.nr)) {
    warning("No random parts with random-slope-intercept parameters found.", call. = F)
    return(NULL)
  }
  # ---------------------------------------
  # iterate all random intercept
  # ---------------------------------------
  for (ri.count in ri.nr) {
    # ------------------------------
    # find random slopes
    # ------------------------------
    rnd.part <- lme4::ranef(fit)[[ri.count]]
    rnd.slope.name <- colnames(rnd.part[2])
    # do predictor name and rnd. slope name equal?
    # if not, might be a factor, so no exact matching possible
    if (!any(pred.values == rnd.slope.name)) {
      # try to find predictor name in random slope name
      for (ef in pred.values) {
        pos <- grep(ef, rnd.slope.name, fixed = T)
        if (length(pos) > 0 && 1 == pos) {
          rnd.slope.name <- ef
          break
        }
      }
    }
    # get all values of predictor that was used as random slope
    eff.range <- unique(sort(m_f[[rnd.slope.name]], na.last = NA))
    # if it a factor?
    if (is.factor(eff.range)) eff.range <- sjmisc::to_value(eff.range)
    # ------------------------------
    # retrieve random effects
    # ------------------------------
    rand.ef <- dplyr::add_rownames(rnd.part)
    # ------------------------------
    # sample random rows?
    # good to have when we have many random intercepts
    # ------------------------------
    if (!is.null(sample.n) && is.numeric(sample.n)) {
      if (length(sample.n) == 1)
        rand.ef <- dplyr::sample_n(rand.ef, sample.n)
      else
        rand.ef <- dplyr::slice(rand.ef, sample.n)
    }
    # ------------------------------
    # set geom highlight colors
    # to highlight specific grouping levels
    # ------------------------------
    if (!is.null(emph.grp)) {
      # create color palette
      grp.col <- col_check2(geom.colors, length(emph.grp))
      # now set only colors for highlighted groups
      geom.colors <- rep("#999999", length(row.names(rand.ef)))
      geom.colors[emph.grp] <- grp.col
    } else if (!is.null(geom.colors) && (geom.colors[1] == "gs" || nrow(rand.ef) < 10)) {
      geom.colors <- col_check2(geom.colors, nrow(rand.ef))
    } else {
      geom.colors <- NULL
    }
    # we may have multiple random slope values, e.g.
    # if random slope is a factor
    for (j in 3:ncol(rand.ef)) {
      # reset data frame
      final.df <- data.frame()
      # slopes for each random intercept
      for (i in 1:nrow(rand.ef)) {
        # retrieve intercept
        ri <- rand.ef[[2]][i]
        # retrieve random slope
        rs <- rand.ef[[j]][i]
        # compute x and y posistion, i.e. the coordinate for the regression line
        # of random slope / intercept
        final.df <- rbind(final.df,
                          cbind(x = eff.range,
                                y = global.intercept + ri + rs * eff.range,
                                grp = rand.ef[[1]][i]))
      }
      # convert grouping level to factor
      final.df$grp <- as.factor(final.df$grp)
      final.df$x <- sjmisc::to_value(final.df$x, keep.labels = F)
      final.df$y <- sjmisc::to_value(final.df$y, keep.labels = F)
      # logistic regression?
      if (fun == "glm") final.df$y <- plogis(final.df$y)
      # ------------------------------
      # check axis limits
      # ------------------------------
      if (is.null(axisLimits.y)) {
        axisLimits.y <- c(as.integer(floor(10 * min(final.df$y, na.rm = T) * .9)) / 10,
                          as.integer(ceiling(10 * max(final.df$y, na.rm = T) * 1.1)) / 10)
      }
      # get random intercept name
      ri.name <- names(lme4::ranef(fit)[ri.count])
      # ------------------------------
      # title and axis title
      # ------------------------------
      if (is.null(title)) 
        p_title <- sprintf("Random slopes within \"%s\"", ri.name)
      else
        p_title <- title
      if (is.null(axisTitle.x)) 
        p_axisTitle.x <- rnd.slope.name
      else
        p_axisTitle.x <- axisTitle.x
      # ------------------------------
      # prepare base plot
      # ------------------------------
      if (fun == "lm") {
        if (is.null(axisTitle.y)) 
          p_axisTitle.y <- colnames(m_f)[1]
        else
          p_axisTitle.y <- axisTitle.y
        gp <- ggplot(final.df, aes(x = x, y = y, colour = grp)) +
          geom_line(size = geom.size)
      } else {
        if (is.null(axisTitle.y)) 
          p_axisTitle.y <- sprintf("Predicted Probability of %s", colnames(m_f)[1])
        else
          p_axisTitle.y <- axisTitle.y
        gp <- ggplot(final.df, aes(x = x, y = y, colour = grp)) +
          stat_smooth(method = "glm", se = F,
                      method.args = list(family = "binomial"))
      }
      gp <- gp +
        scale_y_continuous(limits = axisLimits.y) +
        labs(title = p_title, y = p_axisTitle.y, x = p_axisTitle.x)
      # ------------------------------
      # highlight specific groups?
      # ------------------------------
      if (!is.null(geom.colors)) {
        # set grouping levels as legend labels
        legendLabels <- rand.ef[, 1]
        # set new color scale
        gp <- sj.setGeomColors(gp,
                               geom.colors,
                               length(geom.colors),
                               show.legend,
                               legendLabels)
      } else if (!show.legend) {
        gp <- gp + guides(colour = FALSE)
      }
      # -------------------------------------
      # add to plot and df list
      # -------------------------------------
      plot.fe[[length(plot.fe) + 1]] <- gp
      mydf.fe[[length(mydf.fe) + 1]] <- final.df
      # -------------------------------------
      # check if metric plots should be plotted
      # -------------------------------------
      if (printPlot) print(gp)
    }
  }
  invisible(structure(class = "sjplmer.reri",
                      list(data = mydf.fe,
                           plot = plot.fe)))
}


# ---------------------------------------
# Thanks to Robert Reijntjes from
# Leiden University Medical Center
# for providing the core code snipptes,
# which are used in this function
# ---------------------------------------
#' @importFrom stats ppoints qnorm
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
  cols <- 1:(dim(pv)[1])
  se   <- unlist(lapply(cols, function(i) sqrt(pv[i, i, ])))
  ord  <- unlist(lapply(re, order)) + rep((0:(ncol(re) - 1)) * nrow(re), each = nrow(re))
  pDf  <- data.frame(y = unlist(re)[ord],
                     ci = 1.96 * se[ord],
                     nQQ = rep(stats::qnorm(stats::ppoints(nrow(re))), ncol(re)),
                     ID = factor(rep(rownames(re), ncol(re))[ord], levels = rownames(re)[ord]),
                     ind = gl(ncol(re), nrow(re), labels = names(re)),
                     grp = "1")
  # check size argument
  if (is.null(geom.size)) geom.size <- 3
  gp <- ggplot(pDf, aes(nQQ, y, colour = grp)) +
    facet_wrap(~ind, scales = "free") +
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
      geom_errorbar(aes(ymin = y - ci, ymax = y + ci),
                    width = 0,
                    colour = "black")
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
  return(invisible(structure(class = ifelse(fun == "glm", "sjpglmer.qq", "sjplmer.qq"),
                             list(plot = gp,
                                  data = pDf))))
}


# ---------------------------------------
# Thanks to Robert Reijntjes from
# Leiden University Medical Center
# for providing the core code snipptes,
# which are used in this function
# ---------------------------------------
#' @importFrom stats cov2cor vcov
sjp.lme.fecor <- function(fit,
                          pred.labels,
                          sort.coef,
                          fun,
                          printPlot,
                          fcall = "sjp",
                          CSS = NULL,
                          useViewer = TRUE,
                          no.output = TRUE) {
  # ---------------------------------------
  # copy rownames as axis labels, if not set
  # ---------------------------------------
  if (is.null(pred.labels)) {
    pred.labels <- names(lme4::fixef(fit))
  } else {
    pred.labels <- c("(Intercept)", pred.labels)
  }
  # ---------------------------------------
  so <- summary(fit)
  mydf <- tryCatch(
    mydf <- as.matrix(stats::cov2cor(as.matrix(stats::vcov(fit)))),
    error = function(cond) { mydf <- as.matrix(so$vcov@factors$correlation) }
  )
  rownames(mydf) <- pred.labels
  colnames(mydf) <- pred.labels
  # fix sort-argument
  if (!is.null(sort.coef) && sort.coef != TRUE)
    sort.coef <- FALSE
  else
    sort.coef <- TRUE
  # ---------------------------------------
  # return correlation plot
  # ---------------------------------------
  if (fcall == "sjp") {
    corret <- sjp.corr(as.matrix(mydf),
                       sortCorrelations = sort.coef,
                       axisLabels = pred.labels,
                       printPlot = printPlot)
  } else {
    corret <- sjt.corr(as.matrix(mydf),
                       triangle = "l",
                       CSS = CSS,
                       useViewer = useViewer,
                       no.output = no.output)
  }
  return(invisible(structure(class = ifelse(fun == "glm", "sjpglmer.cor", "sjplmer.cor"),
                             list(plot = corret$plot,
                                  data = corret$df,
                                  corr.matrix = corret$corr.matrix))))
}


#' @importFrom stats model.frame
sjp.lme.fecondpred.onlynumeric <- function(fit,
                                           show.ci,
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
  fit.df <- stats::model.frame(fit)
  # ----------------------------
  # retrieve term names, so we find the estimates in the
  # coefficients list
  # ----------------------------
  fit.term.names <- names(lme4::fixef(fit))[-1]
  # ----------------------------
  # find amount of numeric terms
  # ----------------------------
  findnumeric <- c()
  for (i in 1:length(fit.term.names)) {
    if (class(fit.df[, fit.term.names[i]]) == "numeric")
      findnumeric <- c(findnumeric, which(colnames(fit.df) == fit.term.names[i]))
  }
  # ----------------------------
  # check if we have any numeric predictors
  # ----------------------------
  if (length(findnumeric > 0)) {
    # loop through all numeric termns
    for (i in 1:length(findnumeric)) {
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
      coef.pos <- which(coef.names == fit.term.names[i])
      # calculate x-beta by multiplying original values with estimate of that term
      mydf.vals$xbeta <- mydf.vals$value * (lme4::fixef(fit)[coef.pos])
      # calculate probability (y) via cdf-function
      mydf.vals$y <- plogis(lme4::fixef(fit)[1] + mydf.vals$xbeta)
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
  if (length(mydf.metricpred) > 0) {
    # create mydf for integrated plot
    mydf.ges <- data.frame()
    for (i in 1:length(mydf.metricpred)) {
      # "melt" all single mydf's to one
      mydf.ges <- rbind(mydf.ges, mydf.metricpred[[i]])
      # create single plots for each numeric predictor
      mp <- ggplot(mydf.metricpred[[i]], aes(x = value, y = y)) +
        geom_point() +
        labs(x = axisLabels.mp[i], y = "Probability") +
        stat_smooth(method = "glm", method.args = list(family = "binomial"), se = show.ci) +
        # cartesian coord still plots range of se, even
        # when se exceeds plot range.
        coord_cartesian(ylim = c(0, 1))
      # add plot to list
      plot.metricpred[[length(plot.metricpred) + 1]] <- mp
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
        stat_smooth(method = "glm", method.args = list(family = "binomial"), se = show.ci) +
        # cartesian coord still plots range of se, even
        # when se exceeds plot range.
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
    }
    else {
      for (i in 1:length(plot.metricpred)) {
        print(plot.metricpred[[i]])
      }
    }
  }
  return(structure(class = "sjpglmer.fecc",
                   list(mydf.mp = mydf.metricpred,
                        plot.mp = plot.metricpred,
                        mydf.facet = mydf.facet,
                        plot.facet = plot.facet)))
}


#' @importFrom stats coef
#' @importFrom car Anova
get_lmerMod_pvalues <- function(fitmod) {
  # retrieve sigificance level of independent variables (p-values)
  if (any(class(fitmod) == "merModLmerTest") && requireNamespace("lmerTest", quietly = TRUE)) {
    cs <- suppressWarnings(stats::coef(lmerTest::summary(fitmod)))
  } else {
    cs <- stats::coef(summary(fitmod))
  }
  # check if we have p-values in summary
  if (ncol(cs) >= 4) {
    # do we have a p-value column?
    pvcn <- which(colnames(cs) == "Pr(>|t|)")
    # if not, default to 4
    if (length(pvcn) == 0) pvcn <- 4
    pv <- cs[, pvcn]
  } else if (any(class(fitmod) == "lmerMod") && requireNamespace("pbkrtest", quietly = TRUE)) {
    # compute Kenward-Roger-DF for p-statistic. Code snippet adapted from
    # http://mindingthebrain.blogspot.de/2014/02/three-ways-to-get-parameter-specific-p.html
    message("Computing p-values via Kenward-Roger approximation...")
    #first coefficients need to be data frame
    cs <- as.data.frame(cs)
    # get KR DF
    df.kr <- pbkrtest::get_Lb_ddf(fitmod, lme4::fixef(fitmod))
    # compute p-values, assuming an approximate t-dist
    pv <- 2 * (1 - pt(abs(cs$`t value`), df.kr))
  } else {
    # if we don't have p-values in summary, try to get them via anova
    # we use type 3 here to include intercept
    message("Computing approximate p-values via Wald chi-squared test...")
    pia <- suppressMessages(car::Anova(fitmod, type = "III"))
    # factors may have multiple levels, however, p-value
    # is not calculated for each factor level. Drop these p-values.
    # pia$`Pr(>Chisq)`[which(pia$Df > 1)] <- NA
    pv <- c()
    # to get matching rows between model coefficient and p-values
    # calculated by anova, we "repeat" rows of factors - these factors
    # appear multiple times in the coefficient table (one for each factor
    # level), however, only once in the anova table. Factor levels,
    # i.e. times to repeat, is indicated by the Df.
    pv <- c(pv, rep(pia$`Pr(>Chisq)`, pia$Df))
  }
  return(pv)
}


#' @importFrom stats family model.frame model.matrix na.omit
sjp.glm.eff <- function(fit,
                        title,
                        geom.size,
                        remove.estimates,
                        showCI,
                        axisLimits.y,
                        printPlot) {
  # check axis range
  if (is.null(axisLimits.y)) axisLimits.y <- c(0, 1)
  # ------------------------
  # check if suggested package is available
  # ------------------------
  if (!requireNamespace("effects", quietly = TRUE)) {
    stop("Package 'effects' needed for this function to work. Please install it.", call. = FALSE)
  }
  # ------------------------
  # Get link family and model frame
  # ------------------------
  fitfram <- stats::model.frame(fit)
  fitfam <- get_glm_family(fit)
  # --------------------------------------------------------
  # create logical for family
  # --------------------------------------------------------
  poisson_fam <- fitfam$is_pois
  binom_fam <- fitfam$is_bin
  # ------------------------
  # Retrieve response for automatic title
  # ------------------------
  # retrieve response vector
  if (isTRUE(binom_fam))
    axisTitle.y <- paste("Predicted probabilities of", colnames(fitfram)[1])
  else if (isTRUE(poisson_fam))
    axisTitle.y <- paste("Predicted incidents of", colnames(fitfram)[1])
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
    if (sjmisc::str_contains(t, pattern = c(":", "*"), logic = "not")) {
      # ------------------------
      # build data frame, with raw values
      # predicted response and lower/upper ci
      # ------------------------
      if (isTRUE(binom_fam)) {
        tmp <- data.frame(x = eff[[i]]$x[[t]],
                          y = plogis(eff[[i]]$fit),
                          lower = plogis(eff[[i]]$lower),
                          upper = plogis(eff[[i]]$upper),
                          grp = t)
      } else {
        tmp <- data.frame(x = eff[[i]]$x[[t]],
                          y = exp(eff[[i]]$fit),
                          lower = exp(eff[[i]]$lower),
                          upper = exp(eff[[i]]$upper),
                          grp = t)
      }
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
    message("Interaction terms in model have been ignored. Call `sjp.int` to plot effects of interaction terms.")
  }
  # ------------------------
  # how many different groups?
  # ------------------------
  grp.cnt <- length(unique(mydat$grp))
  # check size argument
  if (is.null(geom.size)) geom.size <- .7
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
  # for logistic regression, use 
  # 0 to 1 scale limits and percentage scale
  # ------------------------
  if (isTRUE(binom_fam)) {
    eff.plot <- eff.plot + 
      coord_cartesian(ylim = axisLimits.y) +
      scale_y_continuous(labels = scales::percent)
  }
  # ------------------------
  # print plot?
  # ------------------------
  if (printPlot) print(eff.plot)
  # return result
  invisible(structure(class = "sjpglmerff",
                      list(plot = eff.plot,
                           data = mydat)))
}

#' @importFrom stats residuals model.frame predict
#' @importFrom graphics plot
sjp.glmer.ma <- function(fit) {
  m_f <- stats::model.frame(fit)
  sjp.setTheme("scatterw")
  gp <- ggplot(data.frame(x = stats::predict(fit), 
                          y = stats::residuals(fit),
                          grp = as.factor(lme4::getME(fit, "y"))),
               aes(x, y)) + 
    geom_point(aes(colour = grp), show.legend = F) + 
    geom_hline(yintercept = 0) +
    stat_smooth(method = "loess", se = T) +
    labs(title = "Residual plot (original model)",
         x = "Log-predicted values",
         y = "Deviance residuals")
  graphics::plot(gp)
  
  preds <- colnames(m_f)[-1]
  for (pr in preds) {
    if (length(unique(m_f[[pr]])) > 4) {
      mydat <- data.frame(x = m_f[[pr]], 
                          y = stats::residuals(fit),
                          grp = as.factor(lme4::getME(fit, "y")))
      gp <- ggplot(mydat, aes(x, y)) + 
        geom_point(aes(colour = grp), show.legend = F) + 
        geom_hline(yintercept = 0) +
        stat_smooth(method = "loess", se = T) +
        labs(x = pr, y = "Residuals",
             title = "Linear relationship between predictor and residuals")
      graphics::plot(gp)
    }
  }
}


#' @importFrom lme4 fixef confint.merMod 
get_cleaned_ciMerMod <- function(fit, fun, ci.only = FALSE) {
  # get odds ratios of fixed effects
  estimate <- lme4::fixef(fit)
  # get condifence intervals, cleaned (variance CI removed via NA)
  CI <- lme4::confint.merMod(fit, method = "Wald", parm = "beta_")
  # create data frame
  if (fun == "lm")
    mydf <- data.frame(cbind(estimate, CI))
  else
    mydf <- data.frame(exp(cbind(estimate, CI)))
  # only return ci?
  if (ci.only)
    return(as.data.frame(CI))
  else
    # return df
    return(mydf)
}
