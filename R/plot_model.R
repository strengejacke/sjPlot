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
#'   \emph{Coefficients} (\href{https://strengejacke.github.io/sjPlot/articles/plot_model_estimates.html}{related vignette})
#'   \describe{
#'     \item{\code{type = "est"}}{Forest-plot of estimates. If the fitted model
#'     only contains one predictor, slope-line is plotted.}
#'     \item{\code{type = "re"}}{For mixed effects models, plots the random
#'     effects.}
#'     \item{\code{type = "std"}}{Forest-plot of standardized coefficients.}
#'     \item{\code{type = "std2"}}{Forest-plot of standardized coefficients,
#'     however, standardization is done by dividing by two SD (see 'Details').}
#'   }
#'   \emph{Marginal Effects}  (\href{https://strengejacke.github.io/sjPlot/articles/plot_marginal_effects.html}{related vignette})
#'   \describe{
#'     \item{\code{type = "pred"}}{Predicted values (marginal effects) for
#'     specific model terms. See \code{\link[ggeffects]{ggpredict}} for details.}
#'     \item{\code{type = "eff"}}{Similar to \code{type = "pred"}, however,
#'     discrete predictors are held constant at their proportions (not reference
#'     level). See \code{\link[ggeffects]{ggeffect}} for details.}
#'     \item{\code{type = "emm"}}{Similar to \code{type = "eff"}, see
#'     \code{\link[ggeffects]{ggemmeans}} for details.}
#'     \item{\code{type = "int"}}{Marginal effects of interaction terms in
#'     \code{model}.}
#'   }
#'   \emph{Model diagnostics}
#'   \describe{
#'     \item{\code{type = "slope"}}{Slope of coefficients for each single
#'     predictor, against the response (linear relationship between each model
#'     term and response). See 'Details'.}
#'     \item{\code{type = "resid"}}{Slope of coefficients for each single
#'     predictor, against the residuals (linear relationship between each model
#'     term and residuals). See 'Details'.}
#'     \item{\code{type = "diag"}}{Check model assumptions. See 'Details'.}
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
#'   that should be plotted. This argument depends on the plot-type:
#'   \describe{
#'     \item{\emph{Coefficients}}{Select terms that should be plotted. All other
#'       term are removed from the output. Note that the term names must match
#'       the names of the model's coefficients. For factors, this means that
#'       the variable name is suffixed with the related factor level, and each
#'       category counts as one term. E.g. \code{rm.terms = "t_name [2,3]"}
#'       would remove the terms \code{"t_name2"} and \code{"t_name3"} (assuming
#'       that the variable \code{t_name} is categorical and has at least
#'       the factor levels \code{2} and \code{3}). Another example for the
#'       \emph{iris}-dataset: \code{terms = "Species"} would not work, instead
#'       you would write \code{terms = "Species [versicolor,virginica]"} to
#'       remove these two levels, or \code{terms = "Speciesversicolor"} if you
#'       just want to remove the level \emph{versicolor} from the plot.}
#'     \item{\emph{Marginal Effects}}{Here \code{terms} indicates for which
#'       terms marginal effects should be displayed. At least one term is
#'       required to calculate effects, maximum length is three terms, where
#'       the second and third term indicate the groups, i.e. predictions of
#'       first term are grouped by the levels of the second (and third) term.
#'       \code{terms} may also indicate higher order terms (e.g. interaction
#'       terms). Indicating levels in square brackets allows for selecting only
#'       specific groups. Term name and levels in brackets must be separated by
#'       a whitespace character, e.g. \code{terms = c("age", "education [1,3]")}.
#'       It is also possible to specify a range of numeric values for the
#'       predictions with a colon, for instance \code{terms = c("education [1,3]",
#'       "age [30:50]")}. Furthermore, it is possible to specify a function name.
#'       Values for predictions will then be transformed, e.g.
#'       \code{terms = "income [exp]"}. This is useful when model predictors were
#'       transformed for fitting the model and should be back-transformed to the
#'       original scale for predictions. Finally, numeric vectors for which no
#'       specific values are given, a "pretty range" is calculated, to avoid
#'       memory allocation problems for vectors with many unique values. If a
#'       numeric vector is specified as second or third term (i.e. if this vector
#'       represents a grouping structure), representative values (see
#'       \code{\link[ggeffects]{values_at}}) are chosen. If all values for a
#'       numeric vector should be used to compute predictions, you may use
#'       e.g. terms = "age [all]". For more details, see
#'       \code{\link[ggeffects]{ggpredict}}.}
#'  }
#' @param sort.est Determines in which way estimates are sorted in the plot:
#'   \itemize{
#'     \item If \code{NULL} (default), no sorting is done and estimates are sorted in the same order as they appear in the model formula.
#'     \item If \code{TRUE}, estimates are sorted in descending order, with highest estimate at the top.
#'     \item If \code{sort.est = "sort.all"}, estimates are re-sorted for each coefficient (only applies if \code{type = "re"} and \code{grid = FALSE}), i.e. the estimates of the random effects for each predictor are sorted and plotted to an own plot.
#'     \item If \code{type = "re"}, specify a predictor's / coefficient's name to sort estimates according to this random effect.
#'   }
#' @param rm.terms Character vector with names that indicate which terms should
#'   be removed from the plot. Counterpart to \code{terms}. \code{rm.terms =
#'   "t_name"} would remove the term \emph{t_name}. Default is \code{NULL}, i.e.
#'   all terms are used. For factors, levels that should be removed from the plot
#'   need to be explicitely indicated in square brackets, and match the model's
#'   coefficient names, e.g. \code{rm.terms = "t_name [2,3]"} would remove the terms
#'   \code{"t_name2"} and \code{"t_name3"} (assuming that the variable \code{t_name}
#'   was categorical and has at least the factor levels \code{2} and \code{3}).
#'   Another example for the \emph{iris} dataset would be
#'   \code{rm.terms = "Species [versicolor,virginica]"}. Note that the
#'   \code{rm.terms}-argument does not apply to \emph{Marginal Effects} plots.
#' @param group.terms Numeric vector with group indices, to group coefficients.
#'   Each group of coefficients gets its own color (see 'Examples').
#' @param order.terms Numeric vector, indicating in which order the coefficients
#'   should be plotted. See examples in
#'   \href{https://strengejacke.github.io/sjPlot/articles/plot_model_estimates.html}{this package-vignette}.
#' @param std.response Logical, whether the response variable will also be
#'   standardized if standardized coefficients are requested. Setting both
#'   \code{std.response = TRUE} and \code{show.std = TRUE} will behave as if
#'   the complete data was standardized before fitting the model.
#' @param pred.type Character, only applies for \emph{Marginal Effects} plots
#'   with mixed effects models. Indicates whether predicted values should be
#'   conditioned on random effects (\code{pred.type = "re"}) or fixed effects
#'   only (\code{pred.type = "fe"}, the default). For details, see documentation
#'   of the \code{type}-argument in \code{\link[ggeffects]{ggpredict}}.
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
#'   \href{https://www.theanalysisfactor.com/3-tips-interpreting-moderation/}{Grace-Martin
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
#'   \code{\link[sjlabelled]{response_labels}} is called to retrieve the label of
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
#'   axis labels. By default, \code{\link[sjlabelled]{term_labels}} is
#'   called to retrieve the labels of the coefficients, which will be used as
#'   axis labels. Use \code{axis.labels = ""} or \code{auto.label = FALSE} to
#'   use the variable names as labels instead. If \code{axis.labels} is a named
#'   vector, axis labels (by default, the names of the model's coefficients)
#'   will be matched with the names of \code{axis.label}. This ensures that
#'   labels always match the related axis value, no matter in which way
#'   axis labels are sorted.
#' @param axis.lim Numeric vector of length 2, defining the range of the plot
#'   axis. Depending on plot-type, may effect either x- or y-axis. For
#'   \emph{Marginal Effects} plots, \code{axis.lim} may also be a list of two
#'   vectors of length 2, defining axis limits for both the x and y axis.
#' @param legend.title Character vector, used as legend title for plots that
#'   have a legend.
#' @param grid.breaks Numeric value or vector; if \code{grid.breaks} is a
#'   single value, sets the distance between breaks for the axis at every
#'   \code{grid.breaks}'th position, where a major grid line is plotted. If
#'   \code{grid.breaks} is a vector, values will be used to define the
#'   axis positions of the major grid lines.
#' @param ci.lvl Numeric, the level of the confidence intervals (error bars).
#'   Use \code{ci.lvl = NA} to remove error bars. For \code{stanreg}-models,
#'   \code{ci.lvl} defines the (outer) probability for the \emph{credible interval}
#'   that is plotted (see \code{\link[bayestestR]{ci}}). By
#'   default, \code{stanreg}-models are printed with two intervals: the "inner"
#'   interval, which defaults to the 50\%-CI; and the "outer" interval, which
#'   defaults to the 89\%-CI. \code{ci.lvl} affects only the outer interval in
#'   such cases. See \code{prob.inner} and \code{prob.outer} under the
#'   \code{...}-argument for more details.
#' @param se Logical, if \code{TRUE}, the standard errors are
#'   also printed. If robust standard errors are required, use arguments
#'   \code{vcov.fun}, \code{vcov.type} and \code{vcov.args} (see
#'   \code{\link[parameters]{standard_error}} for details), or use argument
#'   \code{robust} as shortcut. \code{se} overrides
#'   \code{ci.lvl}: if not \code{NULL}, arguments \code{ci.lvl} and \code{transform}
#'   will be ignored. Currently, \code{se} only applies to \emph{Coefficients} plots.
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
#' @param show.zeroinf Logical, if \code{TRUE}, shows the zero-inflation part of
#'   hurdle- or zero-inflated models.
#' @param robust Deprecated. Please use \code{vcov.fun} directly to specify
#'   the estimation of the variance-covariance matrix.
#' @param vcov.fun Variance-covariance matrix used to compute uncertainty
#'   estimates (e.g., for robust standard errors). This argument accepts a
#'   covariance matrix, a function which returns a covariance matrix, or a
#'   string which identifies the function to be used to compute the covariance
#'   matrix. See \code{\link[parameters:model_parameters]{model_parameters()}}.
#' @param vcov.type Deprecated. The \code{type}-argument is now included in
#'   \code{vcov.args}.
#' @param vcov.args List of arguments to be passed to the function identified by
#'   the \code{vcov.fun} argument. This function is typically supplied by the
#'   \pkg{sandwich} or \pkg{clubSandwich} packages. Please refer to their
#'   documentation (e.g., \code{?sandwich::vcovHAC}) to see the list of
#'   available arguments.
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
#'     \item If \code{"bw"}, and plot-type is a line-plot, the plot is black/white and uses different line types to distinguish groups (see \href{https://strengejacke.github.io/sjPlot/articles/blackwhitefigures.html}{this package-vignette}).
#'     \item If \code{colors} is any valid color brewer palette name, the related palette will be used. Use \code{RColorBrewer::display.brewer.all()} to view all available palette names.
#'     \item There are some pre-defined color palettes in this package, see \code{\link{sjPlot-themes}} for details.
#'     \item Else specify own color values or names as vector (e.g. \code{colors = "#00ff00"} or \code{colors = c("firebrick", "blue")}).
#'   }
#' @param grid Logical, if \code{TRUE}, multiple plots are plotted as grid
#'   layout.
#' @param p.threshold Numeric vector of length 3, indicating the treshold for
#'   annotating p-values with asterisks. Only applies if
#'   \code{p.style = "asterisk"}.
#' @param p.val Character specifying method to be used to calculate p-values.
#'   Defaults to "profile" for glm/polr models, otherwise "wald".
#' @param wrap.title Numeric, determines how many chars of the plot title are
#'   displayed in one line and when a line break is inserted.
#' @param wrap.labels Numeric, determines how many chars of the value, variable
#'   or axis labels are displayed in one line and when a line break is inserted.
#' @param case Desired target case. Labels will automatically converted into the
#'   specified character case. See \code{snakecase::to_any_case()} for more
#'   details on this argument. By default, if \code{case} is not specified,
#'   it will be set to \code{"parsed"}, unless \code{prefix.labels} is not
#'   \code{"none"}. If \code{prefix.labels} is either \code{"label"} (or
#'   \code{"l"}) or \code{"varname"} (or \code{"v"}) and \code{case} is not
#'   specified, it will be set to \code{NULL} - this is a more convenient
#'   default when prefixing labels.
#' @param auto.label Logical, if \code{TRUE} (the default),
#'    and \href{https://strengejacke.github.io/sjlabelled/articles/intro_sjlabelled.html}{data is labelled},
#'    \code{\link[sjlabelled]{term_labels}} is called to retrieve the labels
#'    of the coefficients, which will be used as predictor labels. If data is
#'    not labelled, \href{https://easystats.github.io/parameters/reference/format_parameters.html}{format_parameters()}
#'    is used to create pretty labels. If \code{auto.label = FALSE},
#'   original variable names and value labels (factor levels) are used.
#' @param prefix.labels Indicates whether the value labels of categorical variables
#'   should be prefixed, e.g. with the variable name or variable label. See
#'   argument \code{prefix} in \code{\link[sjlabelled]{term_labels}} for
#'   details.
#' @param jitter Numeric, between 0 and 1. If \code{show.data = TRUE}, you can
#'   add a small amount of random variation to the location of each data point.
#'   \code{jitter} then indicates the width, i.e. how much of a bin's width
#'   will be occupied by the jittered values.
#' @param digits Numeric, amount of digits after decimal point when rounding
#'   estimates or values.
#' @param p.adjust Character vector, if not \code{NULL}, indicates the method
#'   to adjust p-values. See \code{\link[stats]{p.adjust}} for details.
#' @param value.size Numeric, indicates the size of value labels. Can be used
#'   for all plot types where the argument \code{show.values} is applicable,
#'   e.g. \code{value.size = 4}.
#' @param vline.color Color of the vertical "zero effect" line. Default color is
#'   inherited from the current theme.
#' @param bpe For \strong{Stan}-models (fitted with the \pkg{rstanarm}- or
#'   \pkg{brms}-package), the Bayesian point estimate is, by default, the median
#'   of the posterior distribution. Use \code{bpe} to define other functions to
#'   calculate the Bayesian point estimate. \code{bpe} needs to be a character
#'   naming the specific function, which is passed to the \code{fun}-argument in
#'   \code{\link[sjmisc]{typical_value}}. So, \code{bpe = "mean"} would
#'   calculate the mean value of the posterior distribution.
#' @param bpe.style For \strong{Stan}-models (fitted with the \pkg{rstanarm}- or
#'   \pkg{brms}-package), the Bayesian point estimate is indicated as a small,
#'   vertical line by default. Use \code{bpe.style = "dot"} to plot a dot
#'   instead of a line for the point estimate.
#' @param bpe.color Character vector, indicating the color of the Bayesian
#'   point estimate. Setting \code{bpe.color = NULL} will inherit the color
#'   from the mapped aesthetic to match it with the geom's color.
#' @param ci.style Character vector, defining whether inner and outer intervals
#'   for Bayesion models are shown in boxplot-style (\code{"whisker"}) or in
#'   bars with different alpha-levels (\code{"bar"}).
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
#'       probabilities. Default is \code{0.1}. Setting \code{size.inner = 0}
#'       removes the inner probability regions.
#'     }
#'     \item{\code{width}, \code{alpha}, and \code{scale}}{Passed
#'       down to \code{geom_errorbar()} or \code{geom_density_ridges()}, for
#'       forest or diagnostic plots.
#'     }
#'     \item{\code{width}, \code{alpha}, \code{dot.alpha}, \code{dodge} and \code{log.y}}{Passed
#'       down to \code{\link[ggeffects]{plot.ggeffects}} for \emph{Marginal Effects}
#'       plots.
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
#'       down to \code{snakecase::to_any_case()}. This only
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
#' \subsection{Different Plot Types}{
#'   \describe{
#'   \item{\code{type = "std"}}{Plots standardized estimates. See details below.}
#'   \item{\code{type = "std2"}}{Plots standardized estimates, however,
#'     standardization follows Gelman's (2008) suggestion, rescaling the
#'     estimates by dividing them by two standard deviations instead of just one.
#'     Resulting coefficients are then directly comparable for untransformed
#'     binary predictors.
#'   }
#'   \item{\code{type = "pred"}}{Plots estimated marginal means (or marginal effects).
#'     Simply wraps \code{\link[ggeffects]{ggpredict}}. See also
#'     \href{https://strengejacke.github.io/sjPlot/articles/plot_marginal_effects.html}{this package-vignette}.
#'   }
#'   \item{\code{type = "eff"}}{Plots estimated marginal means (or marginal effects).
#'     Simply wraps \code{\link[ggeffects]{ggeffect}}. See also
#'     \href{https://strengejacke.github.io/sjPlot/articles/plot_marginal_effects.html}{this package-vignette}.
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
#'     which variable(s) should be used as moderator. See also
#'     \href{https://strengejacke.github.io/sjPlot/articles/plot_interactions.html}{this package-vignette}.
#'   }
#'   \item{\code{type = "slope"} and \code{type = "resid"}}{Simple diagnostic-plots,
#'   where a linear model for each single predictor is plotted against the
#'   response variable, or the model's residuals. Additionally, a loess-smoothed
#'   line is added to the plot. The main purpose of these plots is to check whether
#'   the relationship between outcome (or residuals) and a predictor is roughly
#'   linear or not. Since the plots are based on a simple linear regression with
#'   only one model predictor at the moment, the slopes (i.e. coefficients) may
#'   differ from the coefficients of the complete model.
#'   }
#'   \item{\code{type = "diag"}}{For \strong{Stan-models}, plots the prior versus
#'   posterior samples. For \strong{linear (mixed) models}, plots for
#'   multicollinearity-check (Variance Inflation Factors), QQ-plots,
#'   checks for normal distribution of residuals and homoscedasticity
#'   (constant variance of residuals) are shown. For \strong{generalized
#'   linear mixed models}, returns the QQ-plot for random effects.
#'   }
#'   }
#' }
#' \subsection{Standardized Estimates}{
#'   Default standardization is done by completely refitting the model on the
#'   standardized data. Hence, this approach is equal to standardizing the
#'   variables before fitting the model, which is particularly recommended for
#'   complex models that include interactions or transformations (e.g., polynomial
#'   or spline terms). When \code{type = "std2"}, standardization of estimates
#'   follows \href{http://www.stat.columbia.edu/~gelman/research/published/standardizing7.pdf}{Gelman's (2008)}
#'   suggestion, rescaling the estimates by dividing them by two standard deviations
#'   instead of just one. Resulting coefficients are then directly comparable for
#'   untransformed binary predictors.
#' }
#'
#' @references
#'   Gelman A (2008) "Scaling regression inputs by dividing by two
#'   standard deviations." \emph{Statistics in Medicine 27: 2865-2873.}
#'   \url{http://www.stat.columbia.edu/~gelman/research/published/standardizing7.pdf}
#'   \cr \cr
#'   Aiken and West (1991). Multiple Regression: Testing and Interpreting Interactions.
#'
#' @examples
#' # prepare data
#' if (requireNamespace("haven")) {
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
#' # keep only selected terms in the model: pos_v_4, the
#' # levels 3 and 4 of factor e42dep and levels 2 and 3 for c172code
#' plot_model(m, terms = c("pos_v_4", "e42dep [3,4]", "c172code [2,3]"))
#' }
#'
#' # multiple plots, as returned from "diagnostic"-plot type,
#' # can be arranged with 'plot_grid()'
#' \dontrun{
#' p <- plot_model(m, type = "diag")
#' plot_grid(p)}
#'
#' # plot random effects
#' if (require("lme4") && require("glmmTMB")) {
#'   m <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
#'   plot_model(m, type = "re")
#'
#'   # plot marginal effects
#'   plot_model(m, type = "pred", terms = "Days")
#' }
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
#' @export
plot_model <- function(model,
                       type = c("est", "re", "eff", "emm", "pred", "int", "std", "std2", "slope", "resid", "diag"),
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
                       legend.title = NULL,
                       wrap.title = 50,
                       wrap.labels = 25,
                       axis.lim = NULL,
                       grid.breaks = NULL,
                       ci.lvl = NULL,
                       se = NULL,
                       robust = FALSE,
                       vcov.fun = NULL,
                       vcov.type = NULL,
                       vcov.args = NULL,
                       colors = "Set1",
                       show.intercept = FALSE,
                       show.values = FALSE,
                       show.p = TRUE,
                       show.data = FALSE,
                       show.legend = TRUE,
                       show.zeroinf = TRUE,
                       value.offset = NULL,
                       value.size,
                       jitter = NULL,
                       digits = 2,
                       dot.size = NULL,
                       line.size = NULL,
                       vline.color = NULL,
                       p.threshold = c(0.05, 0.01, 0.001),
                       p.val = NULL,
                       p.adjust = NULL,
                       grid,
                       case,
                       auto.label = TRUE,
                       prefix.labels = c("none", "varname", "label"),
                       bpe = "median",
                       bpe.style = "line",
                       bpe.color = "white",
                       ci.style = c("whisker", "bar"),
                       std.response = TRUE,
                       ...
                       ) {

  type <- match.arg(type)
  pred.type <- match.arg(pred.type)
  mdrt.values <- match.arg(mdrt.values)
  prefix.labels <- match.arg(prefix.labels)
  ci.style <- match.arg(ci.style)

  # if we prefix labels, use different default for case conversion,
  # else the separating white spaces after colon are removed.
  if (missing(case)) {
    if (prefix.labels == "none")
      case <- "parsed"
    else
      case <- NULL
  }

  if (isTRUE(robust)) {
    vcov.fun <- "HC3"
  }

  # get info on model family
  fam.info <- insight::model_info(model)

  if (insight::is_multivariate(model))
    fam.info <- fam.info[[1]]

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
    if (is.null(title)) title <- sjlabelled::response_labels(model, case = case, mv = fam.info$is_multivariate, ...)
    title <- sjmisc::word_wrap(title, wrap = wrap.title)

    # labels for axis with term names
    if (is.null(axis.labels)) {
      term_labels <- sjlabelled::term_labels(model, case = case, prefix = prefix.labels, ...)
      if (.labelled_model_data(model) || is.stan(model)) axis.labels <- term_labels
    }
    axis.labels <- sjmisc::word_wrap(axis.labels, wrap = wrap.labels)

    # title for axis with estimate values
    if (is.null(axis.title)) axis.title <- sjmisc::word_wrap(estimate_axis_title(fit = model, axis.title = axis.title, type = type, transform = transform, include.zeroinf = TRUE), wrap = wrap.title)
    axis.title <- sjmisc::word_wrap(axis.title, wrap = wrap.labels)

  }


  # check nr of estimates. if only one, plot slope
  if (type == "est" &&
      length(insight::find_predictors(model, component = "conditional", flatten = TRUE)) == 1 &&
      length(insight::find_predictors(model, component = "instruments", flatten = TRUE)) == 0 &&
      fam.info$is_linear && one_par(model)) type <- "slope"


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
  if (is.null(line.size)) line.size <- dplyr::if_else(is.stan(model), .7, .7)
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
      bpe.color = bpe.color,
      facets = grid,
      show.zeroinf = show.zeroinf,
      p.threshold = p.threshold,
      p.val = p.val,
      vcov.fun = vcov.fun,
      vcov.type = vcov.type,
      vcov.args = vcov.args,
      ci.style = ci.style,
      p_adjust = p.adjust,
      std.response = std.response,
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
      bpe.color = bpe.color,
      ci.style = ci.style,
      ...
    )

  } else if (type %in% c("pred", "eff", "emm")) {

    # plot marginal effects ----

    p <- plot_type_eff(
      type = type,
      model = model,
      terms = terms,
      ci.lvl = ci.lvl,
      pred.type = pred.type,
      facets = grid,
      show.data = show.data,
      jitter = jitter,
      geom.colors = colors,
      axis.title = axis.title,
      title = title,
      legend.title = legend.title,
      axis.lim = axis.lim,
      case = case,
      show.legend = show.legend,
      dot.size = dot.size,
      line.size = line.size,
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
      jitter = jitter,
      geom.colors = colors,
      axis.title = axis.title,
      title = title,
      legend.title = legend.title,
      axis.lim = axis.lim,
      case = case,
      show.legend = show.legend,
      dot.size = dot.size,
      line.size = line.size,
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
      jitter = jitter,
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
        axis.lim = axis.lim,
        facets = grid,
        axis.labels = axis.labels,
        ...
      )

    } else if (fam.info$is_linear) {

      p <- plot_diag_linear(
        model = model,
        geom.colors = colors,
        dot.size = dot.size,
        line.size = line.size,
        ...
      )

    } else {

      p <- plot_diag_glm(
        model = model,
        geom.colors = colors,
        dot.size = dot.size,
        line.size = line.size,
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


#' @importFrom insight has_intercept
one_par <- function(model) {
  tryCatch(
    {
      length(stats::coef(model)) < 2 & !insight::has_intercept(model)
    },
    error = function(x) { FALSE }
  )
}
