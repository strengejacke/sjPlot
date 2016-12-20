# bind global variables
utils::globalVariables(c("estimate", "nQQ", "ci", "fixef", "fade", "conf.low", "conf.high", "pred", "prob", "p.string", "CSS", "no.output"))


#' @title Plot estimates, predictions or effects of generalized linear mixed effects models
#' @name sjp.glmer
#'
#' @seealso \href{http://www.strengejacke.de/sjPlot/sjp.glmer/}{sjPlot manual: sjp.glmer}
#'
#' @description By default, this function plots estimates (odds, risk or incidents
#'                ratios, i.e. exponentiated coefficients, depending on family and
#'                link function)
#'                with confidence intervals of either fixed effects or random effects of
#'                generalized linear mixed effects models (that have been fitted with the
#'                \code{\link[lme4]{glmer}}-function of the \pkg{lme4}-package).
#'                Furthermore, this function also plots predicted probabilities /
#'                incidents or diagnostic plots.
#'
#' @param fit a fitted model as returned by the \code{\link[lme4]{glmer}}-function.
#' @param type type of plot. Use one of following:
#'          \describe{
#'            \item{\code{"re"}}{(default) for conditional modes (odds or incidents ratios) of random effects}
#'            \item{\code{"fe"}}{for odds or incidents ratios of fixed effects}
#'            \item{\code{"fe.cor"}}{for correlation matrix of fixed effects}
#'            \item{\code{"re.qq"}}{for a QQ-plot of random effects (random effects quantiles against standard normal quantiles)}
#'            \item{\code{"fe.slope"}}{to plot probability or incidents curves (predicted probabilities or incidents) of all fixed effects coefficients. Use \code{facet.grid} to decide whether to plot each coefficient as separate plot or as integrated faceted plot. See 'Details'.}
#'            \item{\code{"ri.slope"}}{to plot probability or incidents curves (predicted probabilities or incidents) of random intercept variances for all fixed effects coefficients. Use \code{facet.grid} to decide whether to plot each coefficient as separate plot or as integrated faceted plot. See 'Details'.}
#'            \item{\code{"rs.ri"}}{for fitted probability curves (predicted probabilities) indicating the random slope-intercept pairs. Use this to visualize the random parts of random slope-intercept (or repeated measure) models. When having too many groups, use \code{sample.n} argument.}
#'            \item{\code{"eff"}}{to plot marginal effects of predicted probabilities or incidents for each fixed term, where remaining co-variates are set to the mean. Use \code{facet.grid} to decide whether to plot each coefficient as separate plot or as integrated faceted plot. See 'Details'.}
#'            \item{\code{"pred"}}{to plot predicted probabilities or incidents for the response, related to specific model predictors and conditioned on random effects. See 'Details'.}
#'            \item{\code{"pred.fe"}}{to plot predicted probabilities or incidents for the response, related to specific model predictors, only for fixed effects. See 'Details'.}
#'            \item{\code{"ma"}}{to check model assumptions. Note that only argument \code{fit} applies to this plot type. All other arguments are ignored.}
#'          }
#' @param vars numeric vector with column indices of selected variables or a character vector with
#'          variable names of selected variables from the fitted model, which should be used to plot
#'          - depending on \code{type} - estimates, fixed effects slopes or predicted values 
#'          (mean, probabilities, incidents rates, ...). See 'Examples'.
#' @param ri.nr numeric vector. If \code{type = "re"} or \code{type = "ri.slope"},
#'          and fitted model has more than one random intercept, \code{ri.nr} indicates
#'          which random effects of which random intercept (or: which list elements
#'          of \code{\link[lme4]{ranef}}) will be plotted. Default is \code{NULL},
#'          so all random effects will be plotted.
#' @param emph.grp numeric vector with index numbers of grouping levels (from random effect).
#'          If \code{type = "ri.slope"} and \code{facet.grid = FALSE},
#'          an integrated plot of predicted probabilities of fixed effects resp. fixed
#'          effects slopes for each grouping level is plotted. To better find
#'          certain groups, use this argument to emphasize these groups in the plot.
#'          See 'Examples'.
#' @param title character vector with one or more labels that are used as plot title.
#' @param string.interc string, axis label of intercept estimate. Only applies, 
#'          if \code{show.intercept = TRUE} and \code{axis.labels} is not \code{NULL}.
#' @param point.alpha alpha value of point-geoms in the scatter plots. Only applies,
#'          if \code{show.scatter = TRUE}.
#' @param point.color color of of point-geoms in the scatter plots. Only applies,
#'          if \code{show.scatter = TRUE}.
#' @param sort.est determines in which way estimates are sorted in the plot:
#'          \itemize{
#'            \item If \code{NULL} (default), no sorting is done and estimates are sorted in order of model coefficients.
#'            \item If \code{sort.est = "sort.all"}, estimates are re-sorted for each coefficient (only applies if \code{type = "re"} and \code{facet.grid = FALSE}), i.e. the estimates of the random effects for each predictor are sorted and plotted to an own plot.
#'            \item If \code{type = "fe"} or \code{type = "fe.std"}, \code{TRUE} will sort estimates
#'            \item If \code{type = "re"}, specify a predictor's / coefficient's name to sort estimates according to this coefficient.
#'            }
#'            See 'Examples'.
#' @param fade.ns if \code{TRUE}, non significant estimates will be printed in slightly faded colors.
#' @param axis.labels character vector with labels for the model terms, used as axis labels.
#'          For mixed models, should either be vector of fixed effects variable labels 
#'          (if \code{type = "fe"} or \code{type = "fe.std"}) or a vector of group (value)
#'          labels from the random intercept's categories (if \code{type = "re"}).
#' @param axis.title character vector of length one or two (depending on
#'          the plot function and type), used as title(s) for the x and y axis. 
#'          If not specified, a default labelling  is chosen.
#' @param vline.type linetype of the vertical "zero point" line. Default is \code{2} (dashed line).
#' @param vline.color color of the vertical "zero point" line. Default value is \code{"grey70"}.
#' @param digits numeric, amount of digits after decimal point when rounding estimates and values.
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
#' @param ... other arguments passed down to further functions. Currently, following
#'          arguments are supported:
#'          \describe{
#'            \item{\code{?effects::effect}}{
#'              Any arguments accepted by the \code{\link[effects]{effect}} resp. 
#'              \code{\link[effects]{allEffects}} function, for \code{type = "eff"}.
#'            }
#'            \item{\code{width}}{The \code{width}-argument for error bars.}
#'            \item{\code{alpha}}{The \code{alpha}-argument for confidence bands.}
#'            \item{\code{level}}{The \code{level}-argument confidence bands.}
#'          }
#'
#' @inheritParams sjp.lm
#' @inheritParams sjp.glm
#' @inheritParams sjp.grpfrq
#' @inheritParams sjp.gpt
#' @inheritParams sjp.int
#'
#' @return (Insisibily) returns, depending on the plot type
#'          \itemize{
#'            \item The ggplot-object (\code{plot}). For multiple plots and if \code{facet.grid = FALSE}) a \code{plot.list} is returned.
#'            \item A data frame \code{data} with the data used to build the ggplot-object(s), or a list of data frames (\code{data.list}).
#'            }
#'
#' @note \itemize{
#'          \item{Computation of p-values (if necessary) is based on normal-
#'                distribution assumption, treating the t-statistics as Wald
#'                z-statistics.}
#'          \item{Plot types use the inverse link-function to calculate predicted
#'                probabilites or incidents rates. Thus, this function should work
#'                with different model families and link functions; however, the
#'                plot or axis title may not use the exact terminology regarding
#'                model family or link function.}
#'          \item{Thanks go to Robert Reijntjes from
#'                Leiden University Medical Center for sharing R code that is used
#'                to compute fixed effects correlation matrices and qq-plots of
#'                random effects.}
#'        }
#'
#' @details \describe{
#'            \item{\code{type = "re"}}{plots the conditional modes of the random
#'            effects, inclduing predicion intervals. It basically does the same
#'            as \code{dotplot(exp(ranef(fit, condVar = TRUE)[[i]])}, where \code{i}
#'            denotes the random effect index.}
#'            \item{\code{type = "fe.slope"}}{the predicted values
#'            are based on the fixed effects intercept's estimate and each specific
#'            fixed term's estimate. All other fixed effects are set to zero (i.e. ignored),
#'            which corresponds to \code{family(fit)$linkinv(eta = b0 + bi * xi)} (where \code{xi}
#'            is the estimate of fixed effects and \code{b0} is the intercept of
#'            the fixed effects; the inverse link-function is used). This plot type 
#'            may give similar results as \code{type = "pred"}, however, \code{type = "fe.slope"} 
#'            does not adjust for other predictors.}
#'            \item{\code{type = "eff"}}{plots the marginal effects of model predictors.
#'            Unlike \code{type = "fe.slope"}, the predicted values computed by 
#'            \code{type = "eff"} are adjusted for all co-variates, which are
#'            set to the mean, as returned by the \code{\link[effects]{allEffects}} function.
#'            You can pass further arguments down to \code{allEffects} for flexible
#'            function call via the \code{...}-argument.}
#'            \item{\code{type = "ri.slope"}}{the predicted values
#'            are based on the fixed effects intercept, plus each random intercept and
#'            each specific  fixed term's estimate. All other fixed effects are set to zero (i.e. ignored),
#'            which corresponds to \code{family(fit)$linkinv(eta = b0 + b0[r1-rn] + bi * xi)}
#'            (where \code{xi} is the estimate of fixed effects, \code{b0} is the intercept of
#'            the fixed effects and \code{b0[r1-rn]} are all random intercepts).}
#'            \item{\code{type = "rs.ri"}}{the predicted values are based
#'            on the fixed effects intercept, plus each random intercept and
#'            random slope. This plot type is intended to plot the random part, i.e.
#'            the predicted probabilities or incident rates of each random slope
#'            for each random intercept.
#'            Since the random intercept specifies the deviance from the gloabl
#'            intercept, the global intercept is always included. In case of overplotting,
#'            use the \code{sample.n} argument to randomly sample a limited amount
#'            of groups.}
#'            \item{\code{type = "coef"}}{forest plot of joint fixed and random
#'            effect coefficients, as retrieved by \code{\link[lme4]{coef.merMod}},
#'            it's simply \code{\link[lme4]{ranef} + \link[lme4]{fixef}}.}
#'            \item{\code{type = "pred"} or \code{type = "pred.fe"}}{predicted 
#'            values against response, only fixed effects or
#'            conditional on random intercept. It's calling
#'            \code{predict(fit, type = "response", re.form = NA)} resp.
#'            \code{predict(fit, type = "response", re.form = NULL)} to
#'            compute the values.
#'            This plot type requires the \code{vars} argument to select specific terms
#'            that should be used for the x-axis and - optional - as grouping factor. 
#'            Hence, \code{vars} must be a character vector with the names of
#'            one or two model predictors. See 'Examples'.}
#'          }
#'
#' @examples
#' library(lme4)
#' library(sjmisc)
#' # create binary response
#' sleepstudy$Reaction.dicho <- dicho(sleepstudy$Reaction, dich.by = "median")
#' # fit model
#' fit <- glmer(Reaction.dicho ~ Days + (Days | Subject),
#'              data = sleepstudy, family = binomial("logit"))
#'
#' # simple plot
#' sjp.glmer(fit)
#'
#' # sort by predictor Days
#' sjp.glmer(fit, sort.est = "Days")
#'
#' data(efc)
#' # create binary response
#' efc$hi_qol <- dicho(efc$quol_5)
#' # prepare group variable
#' efc$grp = as.factor(efc$e15relat)
#' levels(x = efc$grp) <- get_labels(efc$e15relat)
#' # data frame for fitted model
#' mydf <- data.frame(hi_qol = to_factor(efc$hi_qol),
#'                    sex = to_factor(efc$c161sex),
#'                    education = to_factor(efc$c172code),
#'                    c12hour = efc$c12hour,
#'                    neg_c_7 = efc$neg_c_7,
#'                    grp = efc$grp)
#' # fit glmer
#' fit <- glmer(hi_qol ~ sex + c12hour + neg_c_7 + (1|grp),
#'              data = mydf, family = binomial("logit"))
#'
#' # plot and sort fixed effects
#' sjp.glmer(fit, type = "fe", sort.est = TRUE)
#'
#' # fit glmer, with categorical predictor with more than 2 levels
#' fit <- glmer(hi_qol ~ sex + education + c12hour + neg_c_7 + (1|grp),
#'              data = mydf, family = binomial("logit"))
#'
#' # plot and sort fixed effects, axis labels automatically retrieved
#' sjp.glmer(fit, type = "fe", sort.est = TRUE)
#' 
#' # plot probability curves (predicted probabilities)
#' # for each covariate, grouped by random intercepts
#' # in integrated plots, emphasizing groups 1 and 4
#' sjp.glmer(fit, type = "ri.slope", emph.grp = c(1, 4), facet.grid = FALSE)
#'
#' # plot probability curve (predicted probabilities)
#' # of fixed effect, only for coefficient "neg_c_7"
#' sjp.glmer(fit, type = "fe.slope", vars = "neg_c_7")
#'
#' # plot predicted probabilities for response,
#' # related to model predictor, conditioned on random effects
#' sjp.glmer(fit, type = "pred", vars = "neg_c_7")
#'
#' # plot predicted probabilities for response,
#' # related to model predictor, grouped
#' sjp.glmer(fit, type = "pred.fe", vars = c("neg_c_7", "sex"))
#'           
#' # non faceted, with ci           
#' sjp.glmer(fit, type = "pred.fe", vars = c("neg_c_7", "education"), 
#'           show.ci = TRUE, facet.grid = FALSE)
#'
#' # predictions by gender and education
#' sjp.glmer(fit, type = "pred.fe", vars = c("neg_c_7", "sex", "education"))
#'                      
#' @import ggplot2
#' @importFrom dplyr slice sample_n
#' @importFrom lme4 fixef ranef confint.merMod getME
#' @export
sjp.glmer <- function(fit,
                      type = "re",
                      vars = NULL,
                      ri.nr = NULL,
                      group.estimates = NULL,
                      remove.estimates = NULL,
                      emph.grp = NULL,
                      sample.n = NULL,
                      sort.est = NULL,
                      title = NULL,
                      legend.title = NULL,
                      axis.labels = NULL,
                      axis.title = NULL,
                      geom.colors = "Set1",
                      geom.size = NULL,
                      show.values = TRUE,
                      show.p = TRUE,
                      show.ci = FALSE,
                      show.legend = FALSE,
                      show.intercept = FALSE,
                      string.interc = "(Intercept)",
                      show.scatter = TRUE,
                      point.alpha = 0.2,
                      point.color = NULL,
                      jitter.ci = FALSE,                     
                      fade.ns = FALSE,
                      axis.lim = NULL,
                      digits = 2,
                      vline.type = 2,
                      vline.color = "grey70",
                      facet.grid = TRUE,
                      free.scale = FALSE,
                      y.offset = .1,
                      prnt.plot = TRUE,
                      ...) {
  # -------------------------------------
  # check for deprecated argument values
  # -------------------------------------
  if (type == "fe.prob" || type == "fe.pc") type <- "fe.slope"
  if (type == "ri.prob" || type == "ri.pc" || type == "fe.ri") type <- "ri.slope"
  if (type == "y.prob" || type == "y.pc") type <- "pred"
  # -------------------------------------
  # switch default value for "show.ci" for certain plot types
  # -------------------------------------
  if (type %in% c("re.qq", "fe", "re", "fe.std", "coef") && missing(show.ci)) show.ci <- TRUE
  
  sjp.lme4(fit,
           type,
           vars,
           ri.nr,
           emph.grp,
           title,
           legend.title,
           geom.size,
           geom.colors,
           show.intercept,
           string.interc,
           sort.est,
           axis.labels,
           axis.title,
           axis.lim,
           vline.type,
           vline.color,
           group.estimates,
           remove.estimates,
           show.values,
           digits,
           y.offset,
           show.p,
           facet.grid,
           free.scale,
           fade.ns,
           show.ci,
           jitter.ci,
           FALSE,
           prnt.plot,
           fun = "glm",
           show.scatter,
           point.alpha,
           point.color,
           FALSE,
           FALSE,
           NULL,
           sample.n,
           show.legend,
           ...)
}


#' @title Plot estimates, predictions or effects of linear mixed effects models
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
#'            \item{\code{type = "re"}}{plots the conditional modes of the random
#'            effects, inclduing predicion intervals. It basically does the same
#'            as \code{dotplot(ranef(fit, condVar = TRUE)[[i]])}, where \code{i}
#'            denotes the random effect index.}
#'            \item{\code{type = "fe.slope"}}{plots the linear relationship between
#'            each fixed effect and the response. The regression lines are \emph{not}
#'            based on the fitted model's fixed effects estimates (though they may
#'            be similar). This plot type just computes a simple linear model for
#'            each fixed effect and response. Hence, it's intended for checking
#'            model assumptions, i.e. if predictor and respone are in a linear relationship.
#'            You may use the \code{show.loess} argument to see whether the linear
#'            line differs from the best fitting line.}
#'            \item{\code{type = "fe.resid"}}{Similar to \code{type = "fe.slope"},
#'            this this type is intended for checking model assumptions. However,
#'            fitted values are plotted against the residuals instead of response.}
#'            \item{\code{type = "eff"}}{plots the adjusted (marginal) effects
#'            for each fixed effect, with all co-variates set to the mean, as
#'            returned by the \code{\link[effects]{allEffects}} function.
#'            You can pass further arguments down to \code{allEffects} for flexible
#'            function call via the \code{...}-argument.}
#'            \item{\code{type = "eff.ri"}}{plots the adjusted (marginal) effects
#'            for each fixed effect, with all co-variates set to the mean, varying
#'            by the random intercepts. This plot type basically does the same
#'            as \code{type = "ri.slope"}, except that the co-variates are not
#'            set to zero, but adjusted for. This plot type differs from \code{type = "ri.slope"}
#'            only in the adjusted y-axis-scale}
#'            \item{\code{type = "rs.ri"}}{plots regression lines for the random
#'            parts of the model, i.e. all random slopes for each random intercept.
#'            As the random intercepts describe the deviation from the global intercept,
#'            the regression lines are computed as global intercept + random intercept +
#'            random slope. In case of overplotting,
#'            use the \code{sample.n} argument to randomly sample a limited amount
#'            of groups.}
#'            \item{\code{type = "ri.slope"}}{plots regression lines for each fixed
#'            effect (slopes) within each random intercept. Lines are based on 
#'            the fixed effects intercept, plus each random intercept and
#'            each specific fixed term's estimate. All other fixed effects are 
#'            set to zero (i.e. ignored),
#'            which corresponds to \code{b0 + b0[r1-rn] + bi * xi)}
#'            (where \code{xi} is the estimate of fixed effects, \code{b0} is the intercept of
#'            the fixed effects and \code{b0[r1-rn]} are all random intercepts).}
#'            \item{\code{type = "coef"}}{forest plot of joint fixed and random
#'            effect coefficients, as retrieved by \code{\link[lme4]{coef.merMod}},
#'            it's simply \code{\link[lme4]{ranef} + \link[lme4]{fixef}}.}
#'            \item{\code{type = "pred"} or \code{type = "pred.fe"}}{predicted
#'            values for response, conditional on fixed effects only or on random 
#'            intercept. It's calling
#'            \code{predict(fit, type = "response", re.form = NA)} resp.
#'            \code{predict(fit, type = "response", re.form = NULL)} to
#'            compute the values.
#'            This plot type requires the \code{vars} argument to select specific terms
#'            that should be used for the x-axis and - optional - as grouping factor. 
#'            Hence, \code{vars} must be a character vector with the names of
#'            one or two model predictors. See 'Examples'.}
#'          }
#'
#' @param fit a fitted model as returned by the \code{\link[lme4]{lmer}}-function.
#' @param type type of plot. Use one of following:
#'          \describe{
#'            \item{\code{"re"}}{(default) for conditional modes of random effects as forest plot}
#'            \item{\code{"fe"}}{for estimates of fixed effects as forest plot}
#'            \item{\code{"fe.std"}}{for standardized estimates of fixed effects as forest plot}
#'            \item{\code{"fe.slope"}}{to plot regression lines (slopes) with confidence intervals for each single fixed effect, i.e. all fixed terms are extracted and each is plotted against the response variable (linear relationship between each fixed term and response)}
#'            \item{\code{"fe.resid"}}{to plot regression lines (slopes) with confidence intervals for each single fixed effect (against residuals), i.e. all fixed terms are extracted and each is plotted against the model residuals (linear relationship between each fixed term and residuals)}
#'            \item{\code{"fe.cor"}}{for correlation matrix of fixed effects}
#'            \item{\code{"re.qq"}}{for a QQ-plot of random effects (random effects quantiles against standard normal quantiles)}
#'            \item{\code{"ri.slope"}}{for fixed effects slopes depending on the random intercept.}
#'            \item{\code{"rs.ri"}}{for fitted regression lines indicating the random slope-intercept pairs. Use this to visualize the random parts of random slope-intercept (or repeated measure) models. When having too many groups, use \code{sample.n} argument.}
#'            \item{\code{"coef"}}{for joint (sum of) random and fixed effects coefficients for each explanatory variable for each level of each grouping factor as forest plot.}
#'            \item{\code{"pred"}}{to plot predicted values for the response, related to specific model predictors and conditioned on random effects. See 'Details'.}
#'            \item{\code{"pred.fe"}}{to plot predicted values for the response, related to specific model predictors and conditioned on fixed effects only. See 'Details'.}
#'            \item{\code{"eff"}}{to plot marginal effects of all fixed terms in \code{fit}. Note that interaction terms are excluded from this plot; use \code{\link{sjp.int}} to plot effects of interaction terms. See also 'Details' of \code{\link{sjp.lm}}.}
#'            \item{\code{"eff.ri"}}{to plot marginal effects of all fixed terms in \code{fit}, varying by the random intercepts.}
#'            \item{\code{"poly"}}{to plot predicted values (marginal effects) of polynomial terms in \code{fit}. Use \code{poly.term} to specify the polynomial term in the fitted model (see 'Examples' here and 'Details' of \code{\link{sjp.lm}}).}
#'            \item{\code{"ma"}}{to check model assumptions. Note that no further arguments except \code{fit} are relevant for this option. All other arguments are ignored.}
#'          }
#' @param show.loess logical, if \code{TRUE}, and depending on \code{type}, an 
#'          additional loess-smoothed line is plotted.
#' @param show.loess.ci logical, if \code{TRUE}, a confidence region for the loess-smoothed line
#'          will be plotted. Default is \code{FALSE}. Only applies, if \code{show.loess = TRUE}
#'          (and for \code{\link{sjp.lmer}}, only applies if \code{type = "fe.slope"}
#'          or \code{type = "fe.resid"}).
#' @param poly.term name of a polynomial term in \code{fit} as string. Needs to be
#'          specified, if \code{type = "poly"}, in order to plot marginal effects
#'          for polynomial terms. See 'Examples'.
#' @param p.kr logical, if \code{TRUE}, p-value estimation is based on conditional 
#'          F-tests with Kenward-Roger approximation for the df. Caution: Computation
#'          may take very long time for large samples!
#'
#' @inheritParams sjp.glmer
#' @inheritParams sjp.grpfrq
#' @inheritParams sjp.lm
#' @inheritParams sjp.gpt
#' @inheritParams sjp.int
#'
#' @return (Insisibily) returns
#'          \itemize{
#'            \item the ggplot-object (\code{plot}), if \code{type = "fe"} or if \code{type = "re"} and \code{facet.grid = TRUE}). Multiple plots (\code{type = "re"} and if \code{facet.grid = FALSE}) are returned in the object \code{plot.list}.
#'            \item a list of ggplot-objects (\code{plot.list}). see \code{plot} for details.
#'            \item a data frame \code{data} with the data used to build the ggplot-object(s).
#'            }
#'
#' @note Computation of p-values (if necessary and if \code{p.kr = TRUE}) are based 
#'         on conditional F-tests with Kenward-Roger approximation for the df, using 
#'         the \pkg{pbkrtest}-package. If \pkg{pbkrtest} is not available or
#'         \code{p.kr = FALSE}, computation of p-values is based 
#'         on normal-distribution assumption, treating the t-statistics as Wald
#'         z-statistics. See 'Details' in \code{\link[sjstats]{get_model_pval}}.
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
#' sjp.lmer(fit, sort.est = "Days")
#'
#' # plot each predictor as own plot
#' # sort each plot
#' sjp.lmer(fit, facet.grid = FALSE, sort.est = "sort.all")
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
#' fit <- lmer(neg_c_7 ~ sex + c12hour + barthel + (1|grp), data = mydf)
#'
#' # plot fixed effects
#' sjp.lmer(fit, type = "fe")
#'
#  # plot and sort standardized fixed effects
#' sjp.lmer(fit, type = "fe.std", sort.est = TRUE)
#'
#' # plot fixed effects slopes for each random intercept, 
#' # but only for coefficient "c12hour"
#' sjp.lmer(fit, type = "ri.slope", vars = "c12hour")
#'
#' # highlight specific grouping levels, in this case we compare 
#' # spouses, children and children-in-law
#' sjp.lmer(fit, type = "ri.slope", emph.grp = c(1, 2, 4), vars = "c12hour")
#'
#' \dontrun{
#' # plotting polynomial terms
#' # check linear relation between predictors and response
#' sjp.lmer(fit, type = "fe.slope", show.loess = TRUE)
#'
#' # "barthel" does not seem to be linear correlated to response
#' # try to find appropiate polynomial. Grey line (loess smoothed)
#' # indicates best fit. Looks like x^4 has the best fit,
#' # however, x^2 seems to be suitable according to p-values.
#' sjp.poly(fit, "barthel", 2:4, show.scatter = FALSE)
#'
#' # fit new model
#' fit <- lmer(neg_c_7 ~ sex + c12hour + barthel +
#'             I(barthel^2) + (1|grp), data = mydf)
#'
#' # plot marginal effects of polynomial term
#' sjp.lmer(fit, type = "poly", poly.term = "barthel")
#'
#' # lme4 complaints about scale of polynomial term, so
#' # try centering this predictor
#' mydf$barthel_s <- sjstats::std(mydf$barthel)
#'
#' # re-fit model
#' fit_s <- lmer(neg_c_7 ~ sex + c12hour + barthel_s +
#'               I(barthel_s^2) + (1|grp), data = mydf)
#'
#' # plot marginal effects of centered, scaled polynomial term
#' sjp.lmer(fit_s, type = "poly", poly.term = "barthel_s")
#'
#' # scaling also improved p-values
#' sjt.lmer(fit, fit_s)
#' 
#' # plotting predicted values for response
#' # conditioned on random effects
#' sjp.lmer(fit, type = "pred", vars = "c12hour")
#' 
#' # grouped, for fixed effects only
#' sjp.lmer(fit, type = "pred.fe", vars = c("c12hour", "sex"))
#' 
#' # grouped, for fixed effects only, non-facted
#' sjp.lmer(fit, type = "pred.fe", vars = c("c12hour", "sex"), 
#'          facet.grid = FALSE, show.ci = FALSE)}
#'
#' @import ggplot2
#' @importFrom sjstats se std_beta get_model_pval
#' @importFrom dplyr sample_n slice
#' @export
sjp.lmer <- function(fit,
                     type = "re",
                     vars = NULL,
                     ri.nr = NULL,
                     group.estimates = NULL,
                     remove.estimates = NULL,
                     emph.grp = NULL,
                     sample.n = NULL,
                     poly.term = NULL,
                     sort.est = NULL,
                     title = NULL,
                     legend.title = NULL,
                     axis.labels = NULL,
                     axis.title = NULL,
                     geom.size = NULL,
                     geom.colors = "Set1",
                     show.values = TRUE,
                     show.p = TRUE,
                     show.ci = FALSE,
                     show.legend = FALSE,
                     show.loess = FALSE,
                     show.loess.ci = FALSE,
                     show.intercept = FALSE,
                     string.interc = "(Intercept)",
                     p.kr = TRUE,
                     show.scatter = TRUE,
                     point.alpha = 0.2,
                     point.color = NULL,
                     jitter.ci = FALSE,                     
                     fade.ns = FALSE,
                     axis.lim = NULL,
                     digits = 2,
                     vline.type = 2,
                     vline.color = "grey70",
                     facet.grid = TRUE,
                     free.scale = FALSE,
                     y.offset = .1,
                     prnt.plot = TRUE,
                     ...) {
  # -------------------------------------
  # check for deprecated argument values
  # -------------------------------------
  if (type == "fe.prob" || type == "fe.pc") type <- "fe.slope"
  if (type == "fe.ri") type <- "ri.slope"
  # -------------------------------------
  # switch default value for "show.ci" for certain plot types
  # -------------------------------------
  if (type %in% c("re.qq", "fe", "re", "fe.std", "coef") && missing(show.ci)) show.ci <- TRUE
  
  sjp.lme4(fit,
           type,
           vars,
           ri.nr,
           emph.grp,
           title,
           legend.title,
           geom.size,
           geom.colors,
           show.intercept,
           string.interc,
           sort.est,
           axis.labels,
           axis.title,
           axis.lim,
           vline.type,
           vline.color,
           group.estimates,
           remove.estimates,
           show.values,
           digits,
           y.offset,
           show.p,
           facet.grid,
           free.scale,
           fade.ns,
           show.ci,
           jitter.ci, 
           p.kr,
           prnt.plot,
           fun = "lm",
           show.scatter,
           point.alpha,
           point.color,
           show.loess,
           show.loess.ci,
           poly.term,
           sample.n,
           show.legend,
           ...)
}

sjp.lme4  <- function(fit,
                      type,
                      vars,
                      ri.nr,
                      emph.grp,
                      title,
                      legend.title,
                      geom.size,
                      geom.colors,
                      show.intercept,
                      string.interc,
                      sort.est,
                      axis.labels,
                      axis.title,
                      axis.lim,
                      vline.type,
                      vline.color,
                      group.estimates,
                      remove.estimates,
                      show.values,
                      digits,
                      y.offset,
                      show.p,
                      facet.grid,
                      free.scale,
                      fade.ns,
                      show.ci,
                      jitter.ci,
                      p.kr,
                      prnt.plot,
                      fun,
                      show.scatter = TRUE,
                      point.alpha = 0.2,
                      point.color = NULL,
                      show.loess = FALSE,
                      show.loess.ci = FALSE,
                      poly.term = NULL,
                      sample.n = NULL,
                      show.legend = FALSE,
                      ...) {
  # -------------------------------------
  # check type
  # -------------------------------------
  if (!(type %in% c("re", "fe", "fe.std", "fe.slope", "fe.resid", "fe.cor", "re.qq",
                    "ri.slope", "rs.ri", "coef", "pred", "pred.fe", "poly", "eff", 
                    "eff.ri", "ma"))) {
    warning("Invalid option for `type` argument. Defaulting to `type = \"fe\"` now.")
    type  <- "fe"
  }
  # ---------------------------------------
  # remember whether predictor labels
  # are empty
  # ---------------------------------------
  empty.axis.labels <- is.null(axis.labels)
  # ---------------------------------------
  # for standardized coefficients, intercept
  # is always 0, so no need to be shown
  # ---------------------------------------
  if (type == "fe.std") show.intercept <- FALSE
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
  if (type %in% c("re", "ri.slope", "eff.ri", "rs.ri", "coef")) {
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
      # check default for facet grid. if not specified, set to false
      # ---------------------------------------
      if (missing(facet.grid)) facet.grid <- FALSE
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
    return(invisible(sjp.lme.fecor(fit, axis.labels, sort.est, fun, prnt.plot)))
  } else if (type == "fe.slope" || type == "fe.resid") {
    # ---------------------------------------
    # plot slopes for each fixed coefficient
    # ---------------------------------------
    if (geom.colors == "Set1") geom.colors <- "black"
    if (fun == "lm") {
      # reset default color setting, does not look that good.
      return(invisible(sjp.reglin(fit, title, 50, geom.colors, show.ci, point.alpha,
                                  show.scatter, show.loess, show.loess.ci, 
                                  useResiduals = ifelse(type == "fe.slope", FALSE, TRUE),
                                  remove.estimates, vars, ylim = axis.lim, 
                                  prnt.plot, ...)))
    } else {
      return(invisible(sjp.glm.slope(fit, title, geom.size, geom.colors, remove.estimates, vars,
                                     ylim = axis.lim, show.ci, facet.grid, show.scatter,
                                     point.alpha, prnt.plot)))
    }
  } else if (type == "poly") {
    # ---------------------------------------
    # plot marginal effects for polynimial terms
    # ---------------------------------------
    if (fun == "lm") {
      return(invisible(sjp.lm.poly(fit, poly.term, geom.colors, geom.size, axis.title,
                                   show.ci, prnt.plot)))
    } else {
      warning("Plotting polynomial terms only works for function `sjp.lmer`.", call. = FALSE)
      return(invisible(NULL))
    }
  } else if (type == "eff") {
    # ---------------------------------------
    # plot marginal effects of fixed terms
    # ---------------------------------------
    return(invisible(sjp.glm.eff(fit, title, axis.title, geom.size, 
                                 remove.estimates, vars, show.ci, ylim = axis.lim, 
                                 facet.grid, fun = fun, prnt.plot, ...)))
  } else if (type == "ri.slope" || type == "eff.ri") {
    # ---------------------------------------
    # plot slopes for each fixex coefficient
    # depending on random intercept levels
    # ---------------------------------------
    if (fun == "lm") {
      return(invisible(sjp.lmer.ri.slope(fit, ri.nr, vars, emph.grp, ylim = axis.lim, 
                                         geom.size, prnt.plot, type)))
    } else {
      return(invisible(sjp.glmer.ri.slope(fit, show.ci, facet.grid, ri.nr, vars,
                                          emph.grp, ylim = axis.lim, prnt.plot,
                                          ...)))
    }
  } else if (type == "rs.ri") {
    return(invisible(sjp.lme.rsri(fit, title, axis.title, ri.nr, emph.grp, 
                                  geom.colors, geom.size, sample.n, show.legend, 
                                  ylim = axis.lim, prnt.plot, fun)))
  } else if (type == "re.qq") {
    # ---------------------------------------
    # plot qq-plots for random effects to
    # inspect normality
    # ---------------------------------------
    return(invisible(sjp.lme.reqq(fit, geom.colors, geom.size, show.ci,
                                  vline.type, vline.color, fun,
                                  prnt.plot)))
  } else if (type == "pred") {
    # fix color
    if (geom.colors == "Set1" && length(vars) == 1) geom.colors <- "black"
    # ---------------------------------------
    # plot predicted probabilities / values of
    # response value
    # ---------------------------------------
    return(invisible(sjp.glm.predy(fit, vars, t.title = title, l.title = legend.title,
                                   a.title = axis.title,
                                   geom.colors, show.ci, jitter.ci, geom.size, ylim = axis.lim, facet.grid, 
                                   type = "re", show.scatter, point.alpha, point.color,
                                   show.loess = F, prnt.plot, ...)))
  } else if (type == "pred.fe") {
    # fix color
    if (geom.colors == "Set1" && length(vars) == 1) geom.colors <- "black"
    # ---------------------------------------
    # plot predicted probabilities / values of
    # response value
    # ---------------------------------------
    return(invisible(sjp.glm.predy(fit, vars, t.title = title, l.title = legend.title,
                                   a.title = axis.title,
                                   geom.colors, show.ci, jitter.ci, geom.size, ylim = axis.lim, facet.grid, 
                                   type = "fe", show.scatter, point.alpha, 
                                   point.color, show.loess = F, prnt.plot, ...)))
  }
  # ------------------------
  # check if suggested package is available
  # ------------------------
  if (!requireNamespace("arm", quietly = TRUE)) {
    stop("Package `arm` needed for this function to work. Please install it.", call. = FALSE)
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
  # grouping estimates only for fixed effects
  # ---------------------------------------
  if ((type == "re" || type == "coef") && !is.null(group.estimates)) {
    warning("`group.estimates` not supported for random effects.", call. = F)
    group.estimates <- NULL
  }
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
      if (empty.axis.labels) {
        # use rownames, if axis.labels not available
        axis.labels <- rownames(mydf.ef)
      }
      # ---------------------------------------
      # retrieve standard errors, for ci
      # ---------------------------------------
      if (show.ci) {
        if (type == "coef") {
          se.fit <- data.frame(t(sjstats::se(fit)[[lcnt]]))
        } else {
          se.fit <- arm::se.ranef(fit)[[lcnt]]
        }
      }
      # ---------------------------------------
      # select random effects for each coefficient
      # ---------------------------------------
      for (i in seq_len(ncol(mydf.ef))) {
        # ---------------------------------------
        # create data frame
        # 1. col: odds ratios /estimates of re-estimates
        # 2. col.: lower conf int
        # 3. col: upper conf int
        # ---------------------------------------
        if (show.ci) {
          if (fun == "glm") {
            tmp <- data.frame(estimate = exp(mydf.ef[, i]),
                              conf.low = exp(mydf.ef[, i] - (1.96 * se.fit[, i])),
                              conf.high = exp(mydf.ef[, i] + (1.96 * se.fit[, i])))
          } else {
            tmp <- data.frame(estimate = mydf.ef[, i],
                              conf.low = mydf.ef[, i] - (1.96 * se.fit[, i]),
                              conf.high = mydf.ef[, i] + (1.96 * se.fit[, i]))
          }
        } else {
          if (fun == "glm") {
            tmp <- data.frame(estimate = exp(mydf.ef[, i]), conf.low = NA, conf.high = NA)
          } else {
            tmp <- data.frame(estimate = mydf.ef[, i], conf.low = NA, conf.high = NA)
          }
        }
        # ---------------------------------------
        # set column names (variable / coefficient name)
        # as group indicator
        # ---------------------------------------
        tmp$grp <- colnames(mydf.ef)[i]
        # ---------------------------------------
        # sort data frame. init order
        # ---------------------------------------
        reihe <- seq_len(nrow(tmp))
        # ---------------------------------------
        # sorting requested?
        # ---------------------------------------
        if (!is.null(sort.est)) {
          # ---------------------------------------
          # should all plots be sorted? works only
          # when faceting is FALSE
          # ---------------------------------------
          if (sort.est == "sort.all") {
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
            reihe <- order(mydf.ef[, sort.est])
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
        if (show.values) ps <- sprintf("%.*f", digits, tmp$estimate)
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
          warning("`type = \"fe.std\"` only works for linear models.", call. = F)
        }
        # get odds ratios and cleaned CI
        mydf <- get_cleaned_ciMerMod(fit, fun)
      } else {
        if (type == "fe.std") {
          mydf <- sjstats::std_beta(fit) %>% 
            dplyr::select_("term", "std.estimate", "conf.low", "conf.high")
        } else {
          # get odds ratios and cleaned CI
          mydf <- get_cleaned_ciMerMod(fit, fun)
        }
      }
      # ----------------------------
      # retrieve sigificance level of independent variables (p-values)
      # ----------------------------
      pv <- sjstats::get_model_pval(fit, p.kr)[["p.value"]]
      # ----------------------------
      # retrieve odds ratios resp.
      # betas or standardized betas
      # ----------------------------
      if (fun == "glm") {
        ov <- exp(lme4::fixef(fit))
      } else {
        if (type == "fe.std") {
          ov <- mydf$std.estimate
        } else {
          ov <- lme4::fixef(fit)
        }
      }
      # init data column for p-values
      ps <- rep("", length(ov))
      # ----------------------------
      # copy estimate-values into data column
      # ----------------------------
      if (show.values) ps <- sprintf("%.*f", digits, ov)
      # ----------------------------
      # copy p-values into data column
      # for better readability, convert p-values to asterisks
      # with:
      # p < 0.001 = ***
      # p < 0.01 = **
      # p < 0.05 = *
      # ----------------------------
      if (show.p) {
        for (i in seq_len(length(pv))) {
          ps[i] <- sjmisc::trim(paste(ps[i], get_p_stars(pv[i])))
        }
      }
      # bind p-values
      mydf$p.string <- ps
      mydf$p.value <- pv
      # ---------------------------------------
      # set proper column names
      # ---------------------------------------
      colnames(mydf) <- c("term", "estimate", "conf.low", "conf.high", "p.string", "p.value")
      # ---------------------------------------
      # just one group, so no faceting needed
      # ---------------------------------------
      mydf$grp <- "1"
      facet.grid <- FALSE
      if (is.null(title)) title <- ifelse(type == "fe.std", "Standardized fixed effects", "Fixed effects")
      # ---------------------------------------
      # show intercept?
      # ---------------------------------------
      if (!show.intercept) mydf <- mydf[-1, ]
      # -------------------------------------------------
      # group estimates?
      # -------------------------------------------------
      if (!is.null(group.estimates)) {
        # check for correct length
        if (length(group.estimates) != nrow(mydf)) {
          warning("Length of `group.estimates` does not equal number of model coefficients. Ignoring this argument.", call. = F)
          group.estimates = NULL
          show.legend <- FALSE
          legend.title <- NULL
        } else {
          mydf$grp <- as.character(group.estimates)
          # by default, legend should be visible
          if (missing(show.legend)) show.legend <- TRUE
        }
      } else {
        show.legend <- FALSE
        legend.title <- NULL
      }
      # -------------------------------------------------
      # remove any estimates from the output?
      # -------------------------------------------------
      if (!is.null(remove.estimates)) {
        # get row indices of rows that should be removed
        remrows <- match(remove.estimates, mydf$term)
        # remove rows
        mydf <- dplyr::slice(mydf, seq_len(nrow(mydf))[-remrows])
        # remove labels?
        if (!empty.axis.labels && length(axis.labels) > nrow(mydf))
          axis.labels <- axis.labels[-remrows]
      }
      # ---------------------------------------
      # copy rownames as axis labels, if not set
      # ---------------------------------------
      if (empty.axis.labels) {
        # use rownames, if axis.labels not available
        axis.labels <- suppressWarnings(retrieveModelLabels(list(fit), group.pred = FALSE))
        if (show.intercept) axis.labels <- c(string.interc, axis.labels)
        # check for correct length
        if (length(axis.labels) != nrow(mydf)) axis.labels <- mydf$term
      } else {
        # check if intercept should be added, in case
        # axis.labels are passed
        if (show.intercept) axis.labels <- c(string.interc, axis.labels)
      }
      # ---------------------------------------
      # sort data frame. init order
      # ---------------------------------------
      reihe <- seq_len(nrow(mydf))
      # ---------------------------------------
      # just one sorting option, simply sort estimates
      # ---------------------------------------
      if (!is.null(sort.est)) {
        # sort according to group assignment?
        if (!is.null(group.estimates))
          reihe <- order(mydf$grp, mydf$estimate)
        else
          reihe <- order(mydf$estimate)
        # sort data frame
        mydf <- mydf[reihe, ]
      }
      mydf$sorting <- reihe
    }
    # ---------------------------------------
    # remove specific estimates?
    # ---------------------------------------
    if (!is.null(vars)) {
      # find estimates that should be removed
      remes <- which(!is.na(match(mydf$term, vars)))
      # remove data rows for these estimates
      mydf <- mydf[remes, ]
      # also remove predictor labels
      axis.labels <- axis.labels[remes]
      # re-arrange sorting
      mydf$sorting <- order(mydf$sorting)
    }
    # ---------------------------------------
    # check length labels
    # ---------------------------------------
    if (length(axis.labels) != nrow(mydf) &&
        (length(axis.labels) != (nrow(mydf) / length(unique(mydf$grp))))) {
      warning("`axis.labels` has insufficient length. Using row names.", call. = F)
      axis.labels <- mydf$term[order(mydf$sorting)]
    }
    # ---------------------------------------
    # discrete x position, needed for ggplot
    # ---------------------------------------
    mydf$x <- as.factor(seq_len(length(axis.labels)))
    # ---------------------------------------
    # set indicator whether or not non significant
    # odds ratios should be faded.
    # ---------------------------------------
    if (fade.ns) {
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
                          legend.title,
                          facet.grid,
                          group.estimates,
                          show.ci,
                          vline.type,
                          vline.color,
                          fun,
                          type,
                          free.scale) {
      # ---------------------------------------
      # ggplot-objekt
      # ---------------------------------------
      interc <- ifelse(fun == "glm", 1, 0)
      mydf$interc <- interc
      # --------------------------------------------------------
      # Start plot here! First check how to colour geoms
      # (whether grouped or not)
      # --------------------------------------------------------
      if (!is.null(group.estimates)) {
        gp <- ggplot(mydf, aes_string(x = "x", y = "estimate",
                                      colour = "grp", alpha = "fade"))
      } else {
        gp <- ggplot(mydf, aes(x = x, y = estimate, 
                               colour = (estimate > interc), alpha = fade))
      }
      gp <- gp +
        # Intercept-line
        geom_hline(yintercept = interc,
                   linetype = vline.type,
                   color = vline.color) +
        geom_point(size = geom.size) +
        # print value labels and p-values
        geom_text(aes_string(label = "p.string", y = "estimate"), nudge_x = y.offset) +
        # ---------------------------------------
      # labels in sorted order
      # ---------------------------------------
      scale_x_discrete(labels = axis.labels[mydf$sorting]) +
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
      # do we have an axis limit?
      if (!is.null(axis.lim)) gp <- gp + ylim(axis.lim)
      # ---------------------------------------
      # hide error bars (conf int)?
      # ---------------------------------------
      if (show.ci)  gp <- gp +
          geom_errorbar(aes_string(ymin = "conf.low", ymax = "conf.high"), width = 0)
      # ---------------------------------------
      # axis titles
      # ---------------------------------------
      if (type == "fe" || type == "fe.std") {
        if (is.null(axis.title)) axis.title <- c("", "")
      } else if (type == "re") {
        if (is.null(axis.title)) axis.title <- c("Group levels", "BLUP")
      }
      # check if we have required length of axis titles
      if (length(axis.title) == 1) {
        if (type == "fe" || type == "fe.std")
          axis.title <- c("", axis.title)
        else
          axis.title <- c(axis.title, "")
      }
      # ---------------------------------------
      # add facet grid here, faceting by group
      # (level) of random intercept
      # ---------------------------------------
      if (facet.grid) {
        # no title for facets
        title <- NULL
        # check if user wants free scale for each facet
        if (free.scale)
          gp  <- gp + facet_wrap(~grp, scales = "free_y")
        else
          gp  <- gp + facet_grid(~grp)
      }
      gp <- gp + labs(x = axis.title[1], y = axis.title[2], title = title, colour = legend.title)
      return(gp)
    }
    # ---------------------------------------
    # facet grid means, just one plot
    # ---------------------------------------
    if (facet.grid || !is.null(group.estimates)) {
      # ---------------------------------------
      # for random effects, no title is displayed in facet. so
      # tell user via message that random effects are plotted
      # ---------------------------------------
      if (type == "re") message("Plotting random effects...")
      me.plot <- plot.effe(mydf,
                           title,
                           legend.title,
                           facet.grid,
                           group.estimates,
                           show.ci,
                           vline.type,
                           vline.color,
                           fun,
                           type,
                           free.scale)
      # ---------------------------------------------------------
      # grouped estimates should be shown in legend
      # ---------------------------------------------------------
      if (!is.null(group.estimates)) {
        pal.len <- length(unique(group.estimates))
        legend.labels <- unique(mydf$grp)
      } else {
        pal.len <- 2
        show.legend <- FALSE
        legend.labels <- NULL
      }
      # ---------------------------------------------------------
      # set geom colors
      # ---------------------------------------------------------
      me.plot <- sj.setGeomColors(me.plot, geom.colors, pal.len, show.legend, legend.labels)
      me.plot.list[[length(me.plot.list) + 1]]  <- me.plot
      # ---------------------------------------------------------
      # Check whether ggplot object should be returned or plotted
      # ---------------------------------------------------------
      if (prnt.plot) graphics::plot(me.plot)
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
      for (j in seq_len(length(groups))) {
        me.plot <- plot.effe(mydf[mydf$grp == groups[j], ],
                             title[j],
                             NULL,
                             facet.grid,
                             group.estimates,
                             show.ci,
                             vline.type,
                             vline.color,
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
        if (prnt.plot) graphics::plot(me.plot)
        me.plot <- NULL
      }
    }
  }
  # me plot contains first of all plots...
  me.plot <- me.plot.list[[1]]
  # -------------------------------------
  # return results
  # -------------------------------------
  invisible(structure(class = ifelse(fun == "glm", c("sjPlot", "sjpglmer"), c("sjPlot", "sjplmer")),
                      list(plot = me.plot,
                           plot.list = me.plot.list,
                           data = mydf)))
}


#' @importFrom stats model.frame family na.omit
sjp.glmer.ri.slope <- function(fit, show.ci, facet.grid, ri.nr, vars, emph.grp,
                               ylim, prnt.plot, ...) {
  # ----------------------------
  # retrieve data frame of model to check whether
  # we have any numeric terms in fitted model; and
  # get model family, for link-inverse function
  # ----------------------------
  fit.df <- stats::model.frame(fit, fixed.only = TRUE)
  fitfam <- stats::family(fit)
  faminfo <- get_glm_family(fit)
  # --------------------------------------------------------
  # create logical for family
  # --------------------------------------------------------
  poisson_fam <- faminfo$is_pois
  binom_fam <- faminfo$is_bin
  # ---------------------------------------
  # get ...-argument
  # ---------------------------------------
  dot.args <- get_dot_args(match.call(expand.dots = FALSE)$`...`)
  # ----------------------------
  # retrieve term names, so we find the estimates in the
  # coefficients list
  # ----------------------------
  plot.prob <- list()
  mydf.prob <- list()
  fit.term.names <- colnames(fit.df)[-1]
  response.name <- colnames(fit.df)[1]
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
  # ---------------------------------------------------------
  # axis title, depending on model family
  # ---------------------------------------------------------
  if (binom_fam)
    y.title <- paste("Predicted probabilities")
  else if (poisson_fam)
    y.title <- paste("Predicted incidents")
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
    for (i in seq_len(length(fit.term.names))) {
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
        if (is.factor(mydf.vals$value)) mydf.vals$value <- sjmisc::to_value(mydf.vals$value, start.at = 0, keep.labels = F)
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
          for (j in seq_len(nrow(rand.ef))) {
            # calculate probability for each random effect group
            mydf.vals$y <- fitfam$linkinv(eta = (fi + rand.ef[j, 1] + mydf.vals$xbeta))
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
          mp <- ggplot(final.df, aes_string(x = "pred", y = "prob", colour = "grp", fill = "grp"))
          # special handling for negativ binomial
          if (sjmisc::str_contains(fitfam$family, "negative binomial", ignore.case = T)) {
            mp <- mp +
              stat_smooth(method = "glm.nb", se = show.ci, alpha = dot.args[["ci.alpha"]])
          } else {
            mp <- mp +
              stat_smooth(method = "glm",
                          method.args = list(family = fitfam$family),
                          se = show.ci,
                          alpha = dot.args[["ci.alpha"]])
          }
          # continue with plot setup
          mp <- mp +
            labs(x = NULL, y = y.title,
                 title = sprintf("%s of %s on %s", y.title, pred.name, response.name))
          # ------------------------------
          # prepare default y-axis limits
          # ------------------------------
          y.limits <- c(as.integer(floor(10 * min(final.df$prob, na.rm = T) * .9)) / 10,
                        as.integer(ceiling(10 * max(final.df$prob, na.rm = T) * 1.1)) / 10)
          # ------------------------------
          # check axis limits, if we have user defined values
          # ------------------------------
          if (!is.null(ylim)) {
            # if we have one axis limits range for all plots, use this here
            if (!is.list(ylim) && length(ylim) == 2) {
              y.limits <- ylim
            } else if (is.list(ylim) && length(ylim) >= i) {
              # we may have multiple axis-limits-values here, one pair for
              # each plot. so check for correct length here
              y.limits <- ylim[[i]]
            }
          } 
          # ---------------------------------------------------------
          # cartesian coord still plots range of se, even
          # when se exceeds plot range.
          # ---------------------------------------------------------
          mp <- mp + coord_cartesian(ylim = y.limits)
          # ---------------------------------------------------------
          # wrap to facets
          # ---------------------------------------------------------
          if (facet.grid) {
            mp <- mp + facet_wrap(~grp,
                                  ncol = round(sqrt(nrow(rand.ef))),
                                  scales = "free_x") +
              # no legend
              guides(colour = FALSE, fill = FALSE)
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
          if (prnt.plot) suppressWarnings(graphics::plot(mp))
        }
      }
    }
  }
  invisible(structure(class = "sjpglmer.ripc",
                      list(data = mydf.prob,
                           plot = plot.prob)))
}


#' @importFrom stats formula
#' @importFrom tibble as_tibble rownames_to_column
sjp.lmer.ri.slope <- function(fit, ri.nr, vars, emph.grp, ylim, geom.size, prnt.plot, type) {
  # check size argument
  if (is.null(geom.size)) geom.size <- .7
  # -----------------------------------------------------------
  # get model frame and model matrix
  # -----------------------------------------------------------
  m_m <- as.data.frame(stats::model.matrix(fit))
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
    ri.tmp <- lme4::ranef(fit)
    # retrieve random effects
    rand.ef <- tibble::as_tibble(tibble::rownames_to_column(ri.tmp[[ri.count]]))
    ri.name <- names(ri.tmp[ri.count])
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
    # slopes for all fixed effects
    for (j in seq_len(length(estimates))) {
      # reset data frame
      final.df <- data.frame()
      # slopes for each random intercept
      for (i in seq_len(nrow(rand.ef))) {
        # retrieve intercept
        ri <- rand.ef[[2]][i]
        # 
        if (type == "eff.ri") {
          # retrieve unique values of estimates
          est.values <- as.vector(na.omit(unique(m_m[[fit.term.names[j]]])))
          # retrieve all other estimates, which should be set to mean
          # we need the switch-argument for interaction terms, to find the correct
          # data in the model matrix
          other.est <- 
            m_m[, c(FALSE, !sjmisc::str_contains(fit.term.names[j], 
                                                 colnames(m_m),
                                                 switch = !sjmisc::is_empty(grep(":", fit.term.names[j], fixed = T)))[-1])]
          # compute mean effects
          est.effect <- as.vector(estimates[-j] * colMeans(sjmisc::to_value(other.est), na.rm = T))
          final.df <- rbind(final.df,
                            cbind(x = sjmisc::to_value(est.values, keep.labels = F),
                                  y = fi + ri + sum(est.effect) + sjmisc::to_value(est.values, keep.labels = F) * estimates[j],
                                  grp = i))
        } else {
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
      }
      # comvert grouping level to factor
      final.df$grp <- as.factor(final.df$grp)
      # retrieve group level label
      levels(final.df$grp) <- rand.ef[[1]]
      # ------------------------------
      # prepare plot title
      # ------------------------------
      if (type == "eff.ri")
        plot.title <- sprintf("Marginal effect of %s, conditioned on %s", fit.term.names[j], ri.name)
      else
        plot.title <- sprintf("Random effect \"%s\"", ri.name)
      # ------------------------------
      # prepare base plot
      # ------------------------------
      gp <- ggplot(final.df, aes(x = x, y = y, colour = grp)) +
        geom_line(size = geom.size) +
        labs(title = plot.title, x = fit.term.names[j], y = response.name)
      # ------------------------------
      # check axis limits, if we have user defined values
      # ------------------------------
      if (!is.null(ylim)) {
        # if we have one axis limits range for all plots, use this here
        if (!is.list(ylim) && length(ylim) == 2) {
          gp <- gp + ylim(ylim)
        } else if (is.list(ylim) && length(ylim) >= j) {
          # we may have multiple axis-limits-values here, one pair for
          # each plot. so check for correct length here
          gp <- gp + ylim(ylim[[j]])
        }
      } 
      # ------------------------------
      # highlight specific groups?
      # ------------------------------
      if (!is.null(geom.colors)) {
        # set grouping levels as legend labels
        legendLabels <- rand.ef[[1]]
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
      if (prnt.plot) graphics::plot(gp)
    }
  }
  invisible(structure(class = "sjplmer.feri",
                      list(data = mydf.fe,
                           plot = plot.fe)))
}


sjp.lme.rsri <- function(fit,
                         title,
                         axis.title,
                         ri.nr,
                         emph.grp,
                         geom.colors,
                         geom.size,
                         sample.n,
                         show.legend,
                         ylim,
                         prnt.plot,
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
  # get model family, for link-inverse function
  # ----------------------------
  fitfam <- stats::family(fit)
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
    # is it a factor?
    if (is.factor(eff.range)) eff.range <- sjmisc::to_value(eff.range)
    # ------------------------------
    # retrieve random effects
    # ------------------------------
    rand.ef <- tibble::rownames_to_column(rnd.part)
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
      for (i in seq_len(nrow(rand.ef))) {
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
      if (fun == "glm") final.df$y <- fitfam$linkinv(eta = final.df$y)
      # ------------------------------
      # check axis limits
      # ------------------------------
      if (is.null(ylim)) {
        ylim <- c(as.integer(floor(10 * min(final.df$y, na.rm = T) * .9)) / 10,
                          as.integer(ceiling(10 * max(final.df$y, na.rm = T) * 1.1)) / 10)
      }
      # get random intercept name
      ri.name <- names(lme4::ranef(fit)[ri.count])
      # ------------------------------
      # plot title
      # ------------------------------
      if (is.null(title))
        p_title <- sprintf("Random slopes within \"%s\"", ri.name)
      else
        p_title <- title
      # ------------------------------
      # axis-x title
      # ------------------------------
      if (is.null(axis.title))
        p_axisTitle.x <- sjmisc::get_label(m_f[[rnd.slope.name]], def.value = rnd.slope.name)
      else
        p_axisTitle.x <- axis.title
      # ------------------------------
      # prepare base response title
      # ------------------------------
      p_axisTitle.y <- sjmisc::get_label(m_f[[1]], def.value = colnames(m_f)[1])
      # ------------------------------
      # prepare base plot
      # ------------------------------
      gp <- ggplot(final.df, aes_string(x = "x", y = "y", colour = "grp"))
      if (fun == "lm") {
        gp <- gp + geom_line(size = geom.size)
      } else {
        # special handling for negativ binomial
        if (sjmisc::str_contains(fitfam$family, "negative binomial", ignore.case = T)) {
          gp <- gp +
            stat_smooth(method = "glm.nb", se = F)
        } else {
          gp <- gp +
            stat_smooth(method = "glm", se = F,
                        method.args = list(family = fitfam$family))
        }
      }
      gp <- gp +
        scale_y_continuous(limits = ylim) +
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
      if (prnt.plot) graphics::plot(gp)
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
                         show.ci,
                         vline.type,
                         vline.color,
                         fun,
                         prnt.plot,
                         ...) {
  re   <- lme4::ranef(fit, condVar = T)[[1]]
  pv   <- attr(re, "postVar")
  cols <- seq_len(dim(pv)[1])
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
  
  # get ...-arguments
  dot.args <- get_dot_args(match.call(expand.dots = FALSE)$`...`)

  gp <- ggplot(pDf, aes_string(x = "nQQ", y = "y", colour = "grp", fill = "grp")) +
    facet_wrap(~ind, scales = "free") +
    xlab("Standard normal quantiles") +
    ylab("Random effect quantiles") +
    # Intercept-line
    geom_hline(yintercept = 0,
               linetype = vline.type,
               color = vline.color)
  # ---------------------------------------
  # hide error bars (conf int)?
  # ---------------------------------------
  if (show.ci) {
    gp <- gp +
      geom_errorbar(aes(ymin = y - ci, ymax = y + ci),
                    width = 0,
                    colour = "black")
  }
  # ---------------------------------------
  # plot points and interceot
  # ---------------------------------------
  gp <- gp +
    stat_smooth(method = "lm", alpha = dot.args[["ci.alpha"]]) +
    geom_point(size = geom.size)
  # ---------------------------------------------------------
  # set geom colors
  # ---------------------------------------------------------
  gp <- sj.setGeomColors(gp, geom.colors, 1, FALSE, NULL)
  # ---------------------------------------------------------
  # print plot?
  # ---------------------------------------------------------
  if (prnt.plot) {
    message("Testing for normal distribution. Dots should be plotted along the line.")
    graphics::plot(gp)
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
                          axis.labels,
                          sort.est,
                          fun,
                          prnt.plot,
                          fcall = "sjp",
                          CSS = NULL,
                          use.viewer = TRUE,
                          no.output = TRUE) {
  # ---------------------------------------
  # copy rownames as axis labels, if not set
  # ---------------------------------------
  if (is.null(axis.labels)) {
    axis.labels <- names(lme4::fixef(fit))
  } else {
    axis.labels <- c("(Intercept)", axis.labels)
  }
  # ---------------------------------------
  so <- summary(fit)
  mydf <- tryCatch(
    mydf <- as.matrix(stats::cov2cor(as.matrix(stats::vcov(fit)))),
    error = function(cond) { mydf <- as.matrix(so$vcov@factors$correlation) }
  )
  rownames(mydf) <- axis.labels
  colnames(mydf) <- axis.labels
  # fix sort-argument
  if (!is.null(sort.est) && sort.est != TRUE)
    sort.est <- FALSE
  else
    sort.est <- TRUE
  # ---------------------------------------
  # return correlation plot
  # ---------------------------------------
  if (fcall == "sjp") {
    corret <- sjp.corr(as.matrix(mydf), sort.corr = sort.est,
                       axis.labels = axis.labels, prnt.plot = prnt.plot)
  } else {
    corret <- sjt.corr(as.matrix(mydf),
                       triangle = "l",
                       CSS = CSS,
                       use.viewer = use.viewer,
                       no.output = no.output)
  }
  return(invisible(structure(class = ifelse(fun == "glm", "sjpglmer.cor", "sjplmer.cor"),
                             list(plot = corret$plot,
                                  data = corret$df,
                                  corr.matrix = corret$corr.matrix))))
}


#' @importFrom stats family model.frame na.omit
#' @importFrom dplyr filter
#' @importFrom sjstats pred_vars
#' @importFrom sjmisc is_empty
sjp.glm.eff <- function(fit,
                        title,
                        axis.title,
                        geom.size,
                        remove.estimates,
                        vars,
                        show.ci,
                        ylim,
                        facet.grid,
                        fun,
                        prnt.plot,
                        ...) {
  # ------------------------
  # check if suggested package is available
  # ------------------------
  if (!requireNamespace("effects", quietly = TRUE)) {
    stop("Package `effects` needed for this function to work. Please install it.", call. = FALSE)
  }
  
  # additional arguments for 'effects()'-function?
  add.args <- lapply(match.call(expand.dots = F)$`...`, function(x) x)
  # check whether we have a "transformation" argument
  t.add <- which(names(add.args) == "transformation")
  # if we have a "transformation" argument, and it's NULL, 
  # no transformation of scale
  no.transform <- !sjmisc::is_empty(t.add) && is.null(eval(add.args[[t.add]]))
  
  # ---------------------------------------
  # get ...-arguments
  # ---------------------------------------
  dot.args <- get_dot_args(match.call(expand.dots = FALSE)$`...`)
  
  # ---------------------------------------
  # init plot list
  # ---------------------------------------
  eff.plot.list <- list()
  # ------------------------
  # Get link family and model frame
  # ------------------------
  if (is_merMod(fit))
    fitfram <- stats::model.frame(fit, fixed.only = TRUE)
  else
    fitfram <- stats::model.frame(fit)
  fitfam <- get_glm_family(fit)
  # --------------------------------------------------------
  # create logical for family
  # --------------------------------------------------------
  poisson_fam <- fitfam$is_pois
  binom_fam <- fitfam$is_bin
  # ------------------------
  # retrieve all terms and term name, excluding intercept,
  # both as they appear as column name and as real variable name
  # ------------------------
  all.terms <- colnames(fitfram)[-1]
  all.pred.names <- sjstats::pred_vars(fit)[seq_len(length(all.terms))]
  # ------------------------
  # Retrieve response for automatic title
  # ------------------------
  resp <- fitfram[[1]]
  resp.col <- colnames(fitfram)[1]
  # ------------------------
  # Retrieve response for automatic title
  # ------------------------
  if (!is.null(axis.title) && sjmisc::is_empty(axis.title)) {
    axisTitle.y <- axis.title
  }
  else {
    if (fun == "glm") {
      # check for family, and set appropriate scale-title
      # if we have transformation through effects-package,
      # check if data is on original or transformed scale
      if (binom_fam)
        ysc <- dplyr::if_else(isTRUE(no.transform), 
                              true = "log-odds", 
                              false = "probabilities", 
                              missing = "values")
      else if (poisson_fam)
        ysc <- dplyr::if_else(isTRUE(no.transform), 
                              true = "log-mean", 
                              false = "incidents", 
                              missing = "values")
      else
        ysc <- "values"
      
      # set y-axis-title
      axisTitle.y <- paste(sprintf("Predicted %s for", ysc),
                           sjmisc::get_label(resp, def.value = resp.col))
      
    } else {
      axisTitle.y <- sjmisc::get_label(resp, def.value = resp.col)
    }
  }
  # ------------------------
  # remove setimates?
  # ------------------------
  if (!is.null(remove.estimates)) {
    remcols <- match(remove.estimates, all.pred.names)
    # remember old rownames
    if (!sjmisc::is_empty(remcols)) {
      all.terms <- all.terms[-remcols]
      all.pred.names <- all.pred.names[-remcols]
    }
  }
  # ------------------------
  # select specific setimates?
  # ------------------------
  if (!is.null(vars)) {
    remcols <- match(vars, all.pred.names)
    # remember old rownames
    if (!sjmisc::is_empty(remcols)) {
      all.terms <- all.terms[remcols]
      all.pred.names <- all.pred.names[remcols]
    }
  }
  # ------------------------
  # prepare getting unique values of predictors,
  # which are passed to the allEffects-function
  # ------------------------
  xl <- list()
  for (t in all.terms) {
    # get unique values
    dummy <- list(x = sort(unique(stats::na.omit(fitfram[, t]))))
    # name list, needed for effect-function
    names(dummy) <- t
    # create list for "xlevels" argument of allEffects fucntion
    xl <- c(xl, dummy)
  }
  # ------------------------
  # compute marginal effects for each model term
  # ------------------------
  eff <- effects::allEffects(fit, xlevels = xl, KR = FALSE, ...)
  # remove spaces from model terms, required, because 'effects()' removes
  # them, too, else we don't match the model term ("log(term + 1)" => "log(term+1)")
  all.terms <- gsub(" ", "", all.terms, fixed = TRUE)
  # select specific terms only
  eff <- eff[which(names(eff) %in% all.terms | names(eff) %in% all.pred.names)]
  # init final df
  mydat <- data.frame()
  # interaction term found?
  int.found <- FALSE
  # iterate all effects
  for (i in seq_len(length(eff))) {
    # get term, for which effects were calculated
    t <- eff[[i]]$term
    # check if we have interaction term
    # these are ignored in this case.
    if (sjmisc::str_contains(t, pattern = c(":", "*"), logic = "not")) {
      # ------------------------
      # build data frame, with raw values
      # predicted response and lower/upper ci
      # ------------------------
      if (fun == "glm" && !no.transform) {
        tmp <- data.frame(x = eff[[i]]$x[[t]],
                          y = eff[[i]]$transformation$inverse(eta = eff[[i]]$fit),
                          lower = eff[[i]]$transformation$inverse(eta = eff[[i]]$lower),
                          upper = eff[[i]]$transformation$inverse(eta = eff[[i]]$upper),
                          grp = t)
      } else {
        tmp <- data.frame(x = eff[[i]]$x[[t]],
                          y = eff[[i]]$fit,
                          lower = eff[[i]]$lower,
                          upper = eff[[i]]$upper,
                          grp = t)
      }
      # -------------------------
      # get possible labels
      # -------------------------
      if (t %in% colnames(fitfram))
        tmp_lab <- sjmisc::get_labels(fitfram[[t]])
      else
        tmp_lab <- NULL
      # -------------------------
      # check if we have correct amount of labels
      # -------------------------
      if (length(tmp_lab) != nrow(tmp)) tmp_lab <- NULL
      # -------------------------
      # copy labels to data frame
      # -------------------------
      if (is.null(tmp_lab))
        tmp$label <- as.character(suppressWarnings(sjmisc::to_label(tmp$x, add.non.labelled = T)))
      else
        tmp$label <- tmp_lab
      # -------------------------
      # make sure x is numeric
      # -------------------------
      tmp$x <- sjmisc::to_value(tmp$x, keep.labels = F)
      # sort rows. we may need to do this if we have factors
      # tmp <- tmp[order(tmp$x), ]
      tmp$x <- sort(tmp$x)
      # get possible variable labels
      tmp$var.label <- sjmisc::get_label(fitfram[[t]], def.value = t)
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
  # check if we have only moderation and no single
  # higher order terms
  if (sjmisc::is_empty(mydat)) {
    warning("Model has no higher order terms (except for possible interaction terms). There are no effects that can be plotted. Consider using `sjp.int` if model has interaction terms.", call. = F)
    return(list(p = NULL, se = NULL))
  }
  # continuous numbering of row names
  rownames(mydat) <- seq_len(nrow(mydat))
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
  if (facet.grid) {
    eff.plot <- ggplot(mydat, aes_string(x = "x", y = "y"))
    # show confidence region?
    if (show.ci) eff.plot <- eff.plot + 
      geom_ribbon(aes_string(ymin = "lower", ymax = "upper"), alpha = dot.args[["ci.alpha"]])
    
    # which title?
    if (is.null(title)) title <- "Marginal effects of model predictors"

    eff.plot <- eff.plot +
      geom_line(size = geom.size) +
      facet_wrap(~var.label, ncol = round(sqrt(grp.cnt)), scales = "free_x") +
      labs(x = NULL, y = axisTitle.y, title = title)
    # ------------------------
    # for logistic regression, use percentage scale
    # ------------------------
    if (fun == "glm" && binom_fam && !no.transform)
      eff.plot <- eff.plot + scale_y_continuous(labels = scales::percent)
    # do we have axis limits?
    if (!is.null(ylim)) {
      if (fun == "glm" && binom_fam)
        eff.plot <- eff.plot + coord_cartesian(ylim = ylim)
      else
        eff.plot <- eff.plot + ylim(ylim)
    }
    # ------------------------
    # print plot?
    # ------------------------
    if (prnt.plot) graphics::plot(eff.plot)
  } else {
    # separate plot for each group
    for (i in unique(mydat$grp)) {
      # select subset
      mydat_sub <- dplyr::filter(mydat, grp == i)
      # check if is factor
      x_is_factor <- anyNA(suppressWarnings(as.numeric(mydat_sub$label)))
      # start plot
      eff.plot <- ggplot(mydat_sub, aes_string(x = "x", y = "y"))
      # show confidence region?
      if (show.ci) {
        if (x_is_factor)
          eff.plot <- eff.plot + 
            geom_errorbar(aes_string(ymin = "lower", ymax = "upper"), width = dot.args[["eb.width"]]) +
            geom_point()
        else
          eff.plot <- eff.plot + geom_ribbon(aes_string(ymin = "lower", ymax = "upper"), alpha = dot.args[["ci.alpha"]])
      }
      
      # do we have a title?
      if (!is.null(title) && length(title) >= i)
        ptitle <- title[i]
      else if (!is.null(title) && sjmisc::is_empty(title))
        ptitle <- ""
      else
        ptitle <- sprintf("Marginal effects of %s", mydat_sub$var.label[1])
                          
      eff.plot <- eff.plot +
        geom_line(size = geom.size) +
        labs(x = NULL, y = axisTitle.y, title = ptitle)
      # ------------------------
      # for logistic regression, use percentage scale
      # ------------------------
      if (fun == "glm" && binom_fam)
        eff.plot <- eff.plot + scale_y_continuous(labels = scales::percent)
      # ------------------------
      # do we have axis limits?
      # ------------------------
      y.limits <- NULL
      if (!is.null(ylim)) {
        # find current loop index
        loopcnt <- which(i == unique(mydat$grp))
        # if we have one axis limits range for all plots, use this here
        if (!is.list(ylim) && length(ylim) == 2) {
          y.limits <- ylim
        } else if (is.list(ylim) && length(ylim) >= loopcnt) {
          # we may have multiple axis-limits-values here, one pair for
          # each plot. so check for correct length here
          y.limits <- ylim[[loopcnt]]
        }
      }
      # ---------------------------------------------------------
      # cartesian coord still plots range of se, even
      # when se exceeds plot range. only for binomial models
      # ---------------------------------------------------------
      if (!is.null(y.limits)) {
        if (fun == "glm" && binom_fam)
          eff.plot <- eff.plot + coord_cartesian(ylim = y.limits)
        else
          eff.plot <- eff.plot + ylim(y.limits)
      }
      # ------------------------
      # continuous or discrete scale?
      # ------------------------
      if (x_is_factor) {
        eff.plot <- eff.plot +
          scale_x_continuous(labels = mydat_sub$label, breaks = mydat_sub$x)
      }
      # ------------------------
      # print plot?
      # ------------------------
      if (prnt.plot) graphics::plot(eff.plot)
      # add plot to plot list
      eff.plot.list[[length(eff.plot.list) + 1]] <- eff.plot
    }
  }
  # -------------------------------------
  # set proper column names
  # -------------------------------------
  colnames(mydat) <- c("x", "y", "conf.low", "conf.high", "term", "labels", "label")
  # return result
  invisible(structure(class = c("sjPlot", "sjpglmeff"),
                      list(plot = eff.plot,
                           plot.list = eff.plot.list,
                           data = mydat)))
}

#' @importFrom stats residuals model.frame predict
#' @importFrom graphics plot
sjp.glmer.ma <- function(fit) {
  m_f <- stats::model.frame(fit)
  set_theme("scatterw")
  gp <- ggplot(data.frame(x = stats::predict(fit),
                          y = stats::residuals(fit),
                          grp = as.factor(lme4::getME(fit, "y"))),
               aes_string(x = "x", y = "y")) +
    geom_point(aes_string(colour = "grp"), show.legend = F) +
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
      gp <- ggplot(mydat, aes_string(x = "x", y = "y")) +
        geom_point(aes_string(colour = "grp"), show.legend = F) +
        geom_hline(yintercept = 0) +
        stat_smooth(method = "loess", se = T) +
        labs(x = pr, y = "Residuals",
             title = "Linear relationship between predictor and residuals")
      graphics::plot(gp)
    }
  }
}


#' @importFrom lme4 fixef confint.merMod
#' @importFrom tibble rownames_to_column
#' @importFrom dplyr select_ rename_
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
  # add rownames
  mydf <- 
    tibble::rownames_to_column(mydf, var = "term") %>% 
    dplyr::rename_(.dots = list("conf.low" = "X2.5..", "conf.high" = "X97.5.."))
  # only return ci?
  if (ci.only) mydf <- mydf %>% dplyr::select_("-estimate")
  # return df
  return(mydf)
}
