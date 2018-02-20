#' @title Plot regression models
#' @name plot_model
#'
#' @description
#'   \code{plot_model()} creates plots from regression models, either
#'   estimates (as so-called forest or dot whisker plots) or marginal effects.
#'
#' @param model A regression model object. Depending on the \code{type}, many
#'   kinds of models are supported, e.g. from packages like \pkg{stats},
#'   \pkg{lme4}, \pkg{nlme}, \pkg{rstanarm}, \pkg{survey}, \pkg{glmmTMB},
#'   \pkg{MASS}, \pkg{brms} etc.
#' @param type Type of plot. There are three groups of plot-types: \cr \cr
#'   \emph{Coefficients} (\href{../doc/plot_model_estimates.html}{related vignette})
#'   \describe{
#'     \item{\code{type = "est"}}{Forest-plot of estimates. If the fitted model
#'     only contains one predictor, slope-line is plotted.}
#'     \item{\code{type = "re"}}{For mixed effects models, plots the random
#'     effects.}
#'     \item{\code{type = "std"}}{Forest-plot of standardized beta values.}
#'     \item{\code{type = "std2"}}{Forest-plot of standardized beta values,
#'     however, standardization is done by dividing by two sd (see 'Details').}
#'   }
#'   \emph{Marginal Effects}  (\href{../doc/plot_marginal_effects.html}{related vignette})
#'   \describe{
#'     \item{\code{type = "pred"}}{Predicted values (marginal effects) for
#'     specific model terms. See \code{\link[ggeffects]{ggpredict}} for details.}
#'     \item{\code{type = "eff"}}{Similar to \code{type = "pred"}, however,
#'     discrete predictors are held constant at their proportions (not reference
#'     level). See \code{\link[ggeffects]{ggeffect}} for details.}
#'     \item{\code{type = "int"}}{Marginal effects of interaction terms in
#'     \code{model}.}
#'   }
#'   \emph{Model diagnostics}
#'   \describe{
#'     \item{\code{type = "slope"}}{Slope of coefficients for each single
#'     predictor, against the response (linear relationship between each model
#'     term and response).}
#'     \item{\code{type = "resid"}}{Slope of coefficients for each single
#'     predictor, against the residuals (linear relationship between each model
#'     term and residuals).}
#'     \item{\code{type = "diag"}}{Check model assumptions.}
#'   }
#'   \strong{Note:} For mixed models, the diagnostic plots like linear relationship
#'   or check for Homoscedasticity, do \strong{not} take the uncertainty of
#'   random effects into account, but is only based on the fixed effects part
#'   of the model.
#' @param transform A character vector, naming a function that will be applied
#'   on estimates and confidence intervals. By default, \code{transform} will
#'   automatically use \code{"exp"} as transformation for applicable classes of
#'   \code{model} (e.g. logistic or poisson regression). Estimates of linear
#'   models remain untransformed. Use \code{NULL} if you want the raw,
#'   non-transformed estimates.
#' @param terms Character vector with the names of those terms from \code{model}
#'   that should be plotted. This argument depends on the plot-type: \describe{
#'   \item{\emph{Coefficients}}{ Select terms that should be plotted. All other
#'   term are removed from the output. } \item{\emph{Marginal Effects}}{ Here
#'   \code{terms} indicates for which terms marginal effects should be
#'   displayed. At least one term is required to calculate effects, maximum
#'   length is three terms, where the second and third term indicate the groups,
#'   i.e. predictions of first term are grouped by the levels of the second (and
#'   third) term. \code{terms} may also indicate higher order terms (e.g.
#'   interaction terms). Indicating levels in square brackets allows for
#'   selecting only specific groups. Term name and levels in brackets must be
#'   separated by a whitespace character, e.g. \code{terms = c("age", "education
#'   [1,3]")}. For more details, see \code{\link[ggeffects]{ggpredict}}. } }
#' @param sort.est Determines in which way estimates are sorted in the plot:
#'   \itemize{ \item If \code{NULL} (default), no sorting is done and estimates
#'   are sorted in the same order as they appear in the model formula. \item If
#'   \code{TRUE}, estimates are sorted in descending order, with highedt
#'   estimate at the top. \item If \code{sort.est = "sort.all"}, estimates are
#'   re-sorted for each coefficient (only applies if \code{type = "re"} and
#'   \code{grid = FALSE}), i.e. the estimates of the random effects for each
#'   predictor are sorted and plotted to an own plot. \item If \code{type =
#'   "re"}, specify a predictor's / coefficient's name to sort estimates
#'   according to this random effect. }
#' @param rm.terms Character vector with names that indicate which terms should
#'   be removed from the plot. Counterpart to \code{terms}. \code{rm.terms =
#'   "t_name"} would remove the term \emph{t_name}. Default is \code{NULL}, i.e.
#'   all terms are used. Note that this argument does not apply to
#'   \emph{Marginal Effects} plots.
#' @param group.terms Numeric vector with group indices, to group coefficients.
#'   Each group of coefficients gets its own color (see 'Examples').
#' @param order.terms Numeric vector, indicating in which order the coefficients
#'   should be plotted. See examples in
#'   \href{../doc/plot_model_estimates.html}{this package-vignette}.
#' @param pred.type Character, only applies for \emph{Marginal Effects} plots
#'   with mixed effects models. Indicates whether predicted values should be
#'   conditioned on random effects (\code{pred.type = "re"}) or fixed effects
#'   only (\code{pred.type = "fe"}, the default).
#' @param mdrt.values Indicates which values of the moderator variable should be
#'   used when plotting interaction terms (i.e. \code{type = "int"}). \describe{
#'   \item{\code{"minmax"}}{(default) minimum and maximum values (lower and
#'   upper bounds) of the moderator are used to plot the interaction between
#'   independent variable and moderator(s).} \item{\code{"meansd"}}{uses the
#'   mean value of the moderator as well as one standard deviation below and
#'   above mean value to plot the effect of the moderator on the independent
#'   variable (following the convention suggested by Cohen and Cohen and
#'   popularized by Aiken and West (1991), i.e. using the mean, the value one
#'   standard deviation above, and the value one standard deviation below the
#'   mean as values of the moderator, see
#'   \href{http://www.theanalysisfactor.com/3-tips-interpreting-moderation/}{Grace-Martin
#'   K: 3 Tips to Make Interpreting Moderation Effects Easier}).}
#'   \item{\code{"zeromax"}}{is similar to the \code{"minmax"} option, however,
#'   \code{0} is always used as minimum value for the moderator. This may be
#'   useful for predictors that don't have an empirical zero-value, but absence
#'   of moderation should be simulated by using 0 as minimum.}
#'   \item{\code{"quart"}}{calculates and uses the quartiles (lower, median and
#'   upper) of the moderator value.} \item{\code{"all"}}{uses all values of the
#'   moderator variable.} }
#' @param ri.nr Numeric vector. If \code{type = "re"} and fitted model has more
#'   than one random intercept, \code{ri.nr} indicates which random effects of
#'   which random intercept (or: which list elements of
#'   \code{\link[lme4]{ranef}}) will be plotted. Default is \code{NULL}, so all
#'   random effects will be plotted.
#' @param title Character vector, used as plot title. By default,
#'   \code{\link[sjlabelled]{get_dv_labels}} is called to retrieve the label of
#'   the dependent variable, which will be used as title. Use \code{title = ""}
#'   to remove title.
#' @param axis.title Character vector of length one or two (depending on the
#'   plot function and type), used as title(s) for the x and y axis. If not
#'   specified, a default labelling  is chosen. \strong{Note:} Some plot types
#'   may not support this argument sufficiently. In such cases, use the returned
#'   ggplot-object and add axis titles manually with
#'   \code{\link[ggplot2]{labs}}. Use \code{axis.title = ""} to remove axis
#'   titles.
#' @param axis.labels Character vector with labels for the model terms, used as
#'   axis labels. By default, \code{\link[sjlabelled]{get_term_labels}} is
#'   called to retrieve the labels of the coefficients, which will be used as
#'   axis labels. Use \code{axis.labels = ""} or \code{auto.label = FALSE} to
#'   use the variable names as labels instead.
#' @param axis.lim Numeric vector of length 2, defining the range of the plot
#'   axis. Depending on plot-type, may effect either x- or y-axis. For
#'   \emph{Marginal Effects} plots, \code{axis.lim} may also be a list of two
#'   vectors of length 2, defining axis limits for both the x and y axis.
#' @param grid.breaks Numeric value or vector; if \code{grid.breaks} is a
#'   single value, sets the distance between breaks for the axis at every
#'   \code{grid.breaks}'th position, where a major grid line is plotted. If
#'   \code{grid.breaks} is a vector, values will be used to define the
#'   axis positions of the major grid lines.
#' @param ci.lvl Numeric, the level of the confidence intervals (error bars).
#'   Use \code{ci.lvl = NA} to remove error bars. For \code{stanreg}-models,
#'   \code{ci.lvl} defines the (outer) probability for the
#'   \code{\link[sjstats]{hdi}} (High Density Interval) that is plotted. By
#'   default, \code{stanreg}-models are printed with two intervals: the "inner"
#'   interval, which defaults to the 50\%-HDI; and the "outer" interval, which
#'   defaults to the 89\%-HDI. \code{ci.lvl} affects only the outer interval in
#'   such cases. See \code{prob.inner} and \code{prob.outer} under the
#'   \code{...}-argument for more details.
#' @param se Either a logical, and if \code{TRUE}, error bars indicate standard
#'   errors, not confidence intervals. Or a character vector with a specification
#'   of the covariance matrix to compute robust standard errors (see argument
#'   \code{vcov} of \code{link[sjstats]{robust}} for valid values; robust standard
#'   errors are only supported for models that work with \code{\link[lmtest]{coeftest}}).
#'   \code{se} overrides \code{ci.lvl}: if not \code{NULL}, arguments \code{ci.lvl}
#'   and \code{transform} will be ignored. Currently, \code{se} only applies
#'   to \emph{Coefficients} plots.
#' @param show.intercept Logical, if \code{TRUE}, the intercept of the fitted
#'   model is also plotted. Default is \code{FALSE}. If \code{transform =
#'   "exp"}, please note that due to exponential transformation of estimates,
#'   the intercept in some cases is non-finite and the plot can not be created.
#' @param show.values Logical, whether values should be plotted or not.
#' @param show.p Logical, adds asterisks that indicate the significance level of
#'   estimates to the value labels.
#' @param show.data Logical, for \emph{Marginal Effects} plots, also plots the
#'   raw data points.
#' @param show.legend For \emph{Marginal Effects} plots, shows or hides the
#'   legend.
#' @param value.offset Numeric, offset for text labels to adjust their position
#'   relative to the dots or lines.
#' @param dot.size Numeric, size of the dots that indicate the point estimates.
#' @param line.size Numeric, size of the lines that indicate the error bars.
#' @param colors May be a character vector of color values in hex-format, valid
#'   color value names (see \code{demo("colors")}) or a name of a pre-defined
#'   color palette. Following options are valid for the \code{colors} argument:
#'   \itemize{
#'     \item If not specified, a default color brewer palette will be used, which is suitable for the plot style.
#'     \item If \code{"gs"}, a greyscale will be used.
#'     \item If \code{"bw"}, and plot-type is a line-plot, the plot is black/white and uses different line types to distinguish groups (see \href{../doc/blackwhitefigures.html}{this package-vignette}).
#'     \item If \code{colors} is any valid color brewer palette name, the related palette will be used. Use \code{\link[RColorBrewer]{display.brewer.all}} to view all available palette names.
#'     \item If \pkg{wesanderson} is installed, you may also specify a name of a palette from that package.
#'     \item If \pkg{viridis} is installed, use \code{colors = "v"} to get the viridis color palette.
#'     \item There are some pre-defined color palettes in this package: \code{"aqua", "warm", "dust", "blambus", "simply", "us"}.
#'     \item Else specify own color values or names as vector (e.g. \code{colors = "#00ff00"} or \code{colors = c("firebrick", "blue")}).
#'   }
#' @param grid Logical, if \code{TRUE}, multiple plots are plotted as grid
#'   layout.
#' @param wrap.title Numeric, determines how many chars of the plot title are
#'   displayed in one line and when a line break is inserted.
#' @param wrap.labels Numeric, determines how many chars of the value, variable
#'   or axis labels are displayed in one line and when a line break is inserted.
#' @param case Desired target case. Labels will automatically converted into the
#'   specified character case. See \code{\link[snakecase]{to_any_case}} for more
#'   details on this argument.
#' @param auto.label Logical, if \code{TRUE} (the default), plot-labels are
#'   based on value and variable labels, if the data is labelled. See
#'   \code{\link[sjlabelled]{get_label}} and
#'   \code{\link[sjlabelled]{get_term_labels}} for details. If \code{FALSE},
#'   original variable names and value labels (factor levels) are used.
#' @param digits Numeric, amount of digits after decimal point when rounding
#'   estimates or values.
#' @param value.size Numeric, indicates the size of value labels. Can be used
#'   for all plot types where the argument \code{show.values} is applicable,
#'   e.g. \code{value.size = 4}.
#' @param vline.color Color of the vertical "zero effect" line. Default color is
#'   inherited from the current theme.
#' @param bpe For \strong{Stan}-models (fitted with the \pkg{rstanarm}- or
#'   \pkg{brms}-package), the Bayesian point estimate is, by default, the median
#'   of the posterior distribution. Use \code{bpe} to define other functions to
#'   calculate the Bayesion point estimate. \code{bpe} needs to be a character
#'   naming the specific function, which is passed to the \code{fun}-argument in
#'   \code{\link[sjstats]{typical_value}}. So, \code{bpe = "mean"} would
#'   calculate the mean value of the posterior distribution.
#' @param bpe.style For \strong{Stan}-models (fitted with the \pkg{rstanarm}- or
#'   \pkg{brms}-package), the Bayesian point estimate is indicated as a small,
#'   vertical line by default. Use \code{bpe.style = "dot"} to plot a dot
#'   instead of a line for the point estimate.
#' @param ... Other arguments, passed down to various functions. Here is a list
#'   of supported arguments and their description in detail.
#'   \describe{
#'     \item{\code{prob.inner} and \code{prob.outer}}{For \strong{Stan}-models
#'       (fitted with the \pkg{rstanarm}- or \pkg{brms}-package) and coefficients
#'       plot-types, you can specify numeric values between 0 and 1 for
#'       \code{prob.inner} and \code{prob.outer}, which will then be used as inner
#'       and outer probabilities for the uncertainty intervals (HDI). By default,
#'       the inner probability is 0.5 and the outer probability is 0.89 (unless
#'       \code{ci.lvl} is specified - in this case, \code{ci.lvl} is used as outer
#'       probability).
#'     }
#'     \item{\code{size.inner}}{For \strong{Stan}-models and \emph{Coefficients}
#'       plot-types, you can specify the width of the bar for the inner
#'       probabilities. Default is \code{0.1}.
#'     }
#'     \item{\code{width}, \code{alpha} and \code{scale}}{Passed down to
#'       \code{geom_errorbar()} or \code{geom_density_ridges()}, for forest or
#'       diagnostic plots; or passed down to \code{\link[ggeffects]{plot.ggeffects}}
#'       for \emph{Marginal Effects} plots.
#'     }
#'     \item{\code{show.loess}}{Logical, for diagnostic plot-types \code{"slope"}
#'       and \code{"resid"}, adds (or hides) a loess-smoothed line to the plot.
#'     }
#'     \item{\emph{Marginal Effects} plot-types}{When plotting marginal effects,
#'       arguments are also passed down to \code{\link[ggeffects]{ggpredict}},
#'       \code{\link[ggeffects]{ggeffect}} or \code{\link[ggeffects]{plot.ggeffects}}.
#'     }
#'     \item{Case conversion of labels}{For case conversion of labels (see argument
#'       \code{case}), arguments \code{sep_in} and \code{sep_out} will be passed
#'       down to \code{\link[snakecase]{to_any_case}}. This only
#'       applies to automatically retrieved term labels, \emph{not} if
#'       term labels are provided by the \code{axis.labels}-argument.
#'     }
#'   }
#'
#' @return
#'   Depending on the plot-type, \code{plot_model()} returns a
#'   \code{ggplot}-object or a list of such objects. \code{get_model_data}
#'   returns the associated data with the plot-object as tidy data frame, or
#'   (depending on the plot-type) a list of such data frames.
#'
#' @details
#'   \code{get_model_data} simply calls \code{plot_model()} and returns
#'   the data from the ggplot-object. Hence, it is rather inefficient and should
#'   be used as alternative to \pkg{brooms} \code{tidy()}-function only in
#'   specific situations. \cr \cr Some notes on the different plot-types:
#'   \describe{
#'     \item{\code{type = "std2"}}{Plots standardized beta values,
#'     however, standardization follows Gelman's (2008) suggestion, rescaling the
#'     estimates by dividing them by two standard deviations instead of just one.
#'     Resulting coefficients are then directly comparable for untransformed
#'     binary predictors. This standardization uses the
#'     \code{\link[arm]{standardize}}-function from the \pkg{arm}-package.
#'   }
#'   \item{\code{type = "pred"}}{Plots marginal effects. Simply wraps
#'     \code{\link[ggeffects]{ggpredict}}.
#'   }
#'   \item{\code{type = "eff"}}{Plots marginal effects. Simply wraps
#'     \code{\link[ggeffects]{ggeffect}}.
#'   }
#'   \item{\code{type = "int"}}{A shortcut for marginal effects plots, where
#'     interaction terms are automatically detected and used as
#'     \code{terms}-argument. Furthermore, if the moderator variable (the second
#'     - and third - term in an interaction) is continuous, \code{type = "int"}
#'     automatically chooses useful values based on the \code{mdrt.values}-argument,
#'     which are passed to \code{terms}. Then, \code{\link[ggeffects]{ggpredict}}
#'     is called. \code{type = "int"} plots the interaction term that appears
#'     first in the formula along the x-axis, while the second (and possibly
#'     third) variable in an interaction is used as grouping factor(s)
#'     (moderating variable). Use \code{type = "pred"} or \code{type = "eff"}
#'     and specify a certain order in the \code{terms}-argument to indicate
#'     which variable(s) should be used as moderator.}
#'   }
#'
#' @note
#'   \code{plot_model()} replaces the functions \code{sjp.lm},
#'   \code{sjp.glm}, \code{sjp.lmer}, \code{sjp.glmer} and \code{sjp.int}. These
#'   are becoming softly deprecated and will be removed in a future update.
#'
#' @references
#'   Gelman A (2008) "Scaling regression inputs by dividing by two
#'   standard deviations." \emph{Statistics in Medicine 27: 2865–2873.}
#'   \url{http://www.stat.columbia.edu/~gelman/research/published/standardizing7.pdf}
#'   \cr \cr
#'   Aiken and West (1991). Multiple Regression: Testing and Interpreting Interactions.
#'
#' @examples
#' # prepare data
#' library(sjmisc)
#' data(efc)
#' efc <- to_factor(efc, c161sex, e42dep, c172code)
#' m <- lm(neg_c_7 ~ pos_v_4 + c12hour + e42dep + c172code, data = efc)
#'
#' # simple forest plot
#' plot_model(m)
#'
#' # grouped coefficients
#' plot_model(m, group.terms = c(1, 2, 3, 3, 3, 4, 4))
#'
#' # multiple plots, as returned from "diagnostic"-plot type,
#' # can be arranged with 'plot_grid()'
#' \dontrun{
#' p <- plot_model(m, type = "diag")
#' plot_grid(p)}
#'
#' # plot random effects
#' library(lme4)
#' m <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
#' plot_model(m, type = "re")
#'
#' # plot marginal effects
#' plot_model(m, type = "eff", terms = "Days")
#'
#' # plot interactions
#' \dontrun{
#' m <- glm(
#'   tot_sc_e ~ c161sex + c172code * neg_c_7,
#'   data = efc,
#'   family = poisson()
#' )
#' # type = "int" automatically selects groups for continuous moderator
#' # variables - see argument 'mdrt.values'. The following function call is
#' # identical to:
#' # plot_model(m, type = "pred", terms = c("c172code", "neg_c_7 [7,28]"))
#' plot_model(m, type = "int")
#'
#' # switch moderator
#' plot_model(m, type = "pred", terms = c("neg_c_7", "c172code"))
#' # same as
#' # ggeffects::ggpredict(m, terms = c("neg_c_7", "c172code"))}
#'
#' # plot Stan-model
#' \dontrun{
#' if (require("rstanarm")) {
#'   data(mtcars)
#'   m <- stan_glm(mpg ~ wt + am + cyl + gear, data = mtcars, chains = 1)
#'   plot_model(m, bpe.style = "dot")
#' }}
#'
#' @importFrom sjstats pred_vars std_beta p_value
#' @importFrom sjmisc word_wrap str_contains
#' @importFrom sjlabelled get_dv_labels get_term_labels
#' @importFrom dplyr if_else n_distinct
#' @importFrom graphics plot
#' @importFrom ggeffects ggpredict ggeffect
#' @importFrom stats terms
#' @importFrom tibble add_column
#'
#' @export
plot_model <- function(model,
                       type = c("est", "re", "eff", "pred", "int", "std", "std2", "slope", "resid", "diag"),
                       transform,
                       terms = NULL,
                       sort.est = NULL,
                       rm.terms = NULL,
                       group.terms = NULL,
                       order.terms = NULL,
                       pred.type = c("fe", "re"),
                       mdrt.values = c("minmax", "meansd", "zeromax", "quart", "all"),
                       ri.nr = NULL,
                       title = NULL,
                       axis.title = NULL,
                       axis.labels = NULL,
                       wrap.title = 50,
                       wrap.labels = 25,
                       axis.lim = NULL,
                       grid.breaks = NULL,
                       ci.lvl = NULL,
                       se = NULL,
                       colors = "Set1",
                       show.intercept = FALSE,
                       show.values = FALSE,
                       show.p = TRUE,
                       show.data = FALSE,
                       show.legend = TRUE,
                       value.offset = NULL,
                       value.size,
                       digits = 2,
                       dot.size = NULL,
                       line.size = NULL,
                       vline.color = NULL,
                       grid,
                       case = "parsed",
                       auto.label = TRUE,
                       bpe = "median",
                       bpe.style = "line",
                       ...
                       ) {

  type <- match.arg(type)
  pred.type <- match.arg(pred.type)
  mdrt.values <- match.arg(mdrt.values)


  # check se-argument
  se <- check_se_argument(se = se, type = type)


  # get info on model family
  fam.info <- get_glm_family(model)


  # check whether estimates should be transformed or not

  if (missing(transform)) {
    if (fam.info$is_linear)
      transform <- NULL
    else
      transform <- "exp"
  }


  # get titles and labels for axis ----

  # this is not appropriate when plotting random effects,
  # so retrieve labels only for other plot types

  if (type %in% c("est", "std", "std2") && isTRUE(auto.label)) {

    # get labels of dependent variables, and wrap them if too long
    if (is.null(title)) title <- sjlabelled::get_dv_labels(model, case = case, ...)
    title <- sjmisc::word_wrap(title, wrap = wrap.title)

    # labels for axis with term names
    if (is.null(axis.labels)) axis.labels <- sjlabelled::get_term_labels(model, case = case, ...)
    axis.labels <- sjmisc::word_wrap(axis.labels, wrap = wrap.labels)

    # title for axis with estimate values
    if (is.null(axis.title)) axis.title <- sjmisc::word_wrap(get_estimate_axis_title(model, axis.title, type, transform), wrap = wrap.title)
    axis.title <- sjmisc::word_wrap(axis.title, wrap = wrap.labels)

  }


  # check nr of terms. if only one, plot slope
  if (type == "est" && length(sjstats::pred_vars(model)) == 1) type <- "slope"


  # set some default options for stan-models, which are not
  # available or appropriate for these

  if (is.stan(model)) {
    # no p-values
    show.p <- FALSE
    # no standardized coefficients
    if (type %in% c("std", "std2", "slope")) type <- "est"
  }


  # set defaults for arguments, depending on model ----

  if (is.null(ci.lvl)) ci.lvl <- dplyr::if_else(is.stan(model), .89, .95)
  if (is.null(dot.size)) dot.size <- dplyr::if_else(is.stan(model), 1, 2.5)
  if (is.null(line.size)) line.size <- dplyr::if_else(is.stan(model), .5, .5)
  if (is.null(value.offset)) value.offset <- dplyr::if_else(is.stan(model), .25, .15)


  # check if plot-type is applicable

  if (type == "slope" && !fam.info$is_linear) {
    type <- "est"
    message("Plot-type \"slope\" only available for linear models. Using `type = \"est\"` now.")
  }


  if (type %in% c("est", "std", "std2") || (is.stan(model) && type == "re")) {

    # plot estimates ----

    p <- plot_type_est(
      type = type,
      ci.lvl = ci.lvl,
      se = se,
      tf = transform,
      model = model,
      terms = terms,
      group.terms = group.terms,
      rm.terms = rm.terms,
      sort.est = sort.est,
      title = title,
      axis.title = axis.title,
      axis.labels = axis.labels,
      axis.lim = axis.lim,
      grid.breaks = grid.breaks,
      show.intercept = show.intercept,
      show.values = show.values,
      show.p = show.p,
      value.offset = value.offset,
      digits = digits,
      geom.colors = colors,
      geom.size = dot.size,
      line.size = line.size,
      order.terms = order.terms,
      vline.color = vline.color,
      value.size = value.size,
      bpe = bpe,
      bpe.style = bpe.style,
      facets = grid,
      ...
    )

  } else if (type == "re") {

    # plot random effects ----

    p <- plot_type_ranef(
      model = model,
      ri.nr = ri.nr,
      ci.lvl = ci.lvl,
      se = se,
      tf = transform,
      sort.est = sort.est,
      title = title,
      axis.labels = axis.labels,
      axis.lim = axis.lim,
      grid.breaks = grid.breaks,
      show.values = show.values,
      value.offset = value.offset,
      digits = digits,
      facets = grid,
      geom.colors = colors,
      geom.size = dot.size,
      line.size = line.size,
      vline.color = vline.color,
      value.size = value.size,
      ...
    )

  } else if (type %in% c("pred", "eff")) {

    # plot marginal effects ----

    p <- plot_type_eff(
      type = type,
      model = model,
      terms = terms,
      ci.lvl = ci.lvl,
      pred.type = pred.type,
      facets = grid,
      show.data = show.data,
      geom.colors = colors,
      axis.title = axis.title,
      title = title,
      axis.lim = axis.lim,
      case = case,
      show.legend = show.legend,
      ...
    )

  } else if (type == "int") {

    # plot interaction terms ----

    p <- plot_type_int(
      model = model,
      mdrt.values = mdrt.values,
      ci.lvl = ci.lvl,
      pred.type = pred.type,
      facets = grid,
      show.data = show.data,
      geom.colors = colors,
      axis.title = axis.title,
      title = title,
      axis.lim = axis.lim,
      case = case,
      show.legend = show.legend,
      ...
    )


  } else if (type %in% c("slope", "resid")) {

    # plot slopes of estimates ----

    p <- plot_type_slope(
      model = model,
      terms = terms,
      rm.terms = rm.terms,
      ci.lvl = ci.lvl,
      colors = colors,
      title = title,
      show.data = show.data,
      facets = grid,
      axis.title = axis.title,
      case = case,
      useResiduals = type == "resid",
      ...
    )

  } else if (type == "diag") {

    # plot diagnostic plots ----

    if (is.stan(model)) {

      p <- plot_diag_stan(
        model = model,
        geom.colors = colors,
        facets = grid,
        ...
      )

    } else if (fam.info$is_linear) {

      p <- plot_diag_linear(
        model = model,
        geom.colors = colors,
        dot.size = dot.size,
        ...
      )

    } else {

      p <- plot_diag_glm(
        model = model,
        geom.colors = colors,
        dot.size = dot.size,
        ...
      )

    }

  }

  p
}


#' @importFrom purrr map
#' @rdname plot_model
#' @export
get_model_data <- function(model,
                       type = c("est", "re", "eff", "pred", "int", "std", "std2", "slope", "resid", "diag"),
                       transform,
                       terms = NULL,
                       sort.est = NULL,
                       rm.terms = NULL,
                       group.terms = NULL,
                       order.terms = NULL,
                       pred.type = c("fe", "re"),
                       ri.nr = NULL,
                       ci.lvl = NULL,
                       colors = "Set1",
                       grid,
                       case = "parsed",
                       digits = 2,
                       ...) {
  p <- plot_model(
    model = model,
    type = type,
    transform = transform,
    terms = terms,
    sort.est = sort.est,
    rm.terms = rm.terms,
    group.terms = group.terms,
    order.terms = order.terms,
    pred.type = pred.type,
    ri.nr = ri.nr,
    ci.lvl = ci.lvl,
    colors = colors,
    grid = grid,
    case = case,
    digits = digits,
    auto.label = FALSE,
    ...
  )


  if (inherits(p, "list"))
    purrr::map(p, ~ .x$data)
  else
    p$data
}
