#' @title Various plot types for regression models
#' @name plot_model
#'
#' @description To Do...
#'
#' @param type Type of plot. Use one of following:
#'          \describe{
#'            \item{\code{"est"}}{(default) for forest-plot of estimates. If the fitted model only contains one predictor, slope-line is plotted.}
#'            \item{\code{"pred"}}{to plot predicted values (marginal effects) for specific model terms. See 'Details'.}
#'            \item{\code{"eff"}}{to plot marginal effects of all terms in \code{fit}. Note that interaction terms are excluded from this plot.}
#'            \item{\code{"int"}}{to plot marginal effects of interaction terms in \code{fit}.}
#'            \item{\code{"std"}}{for forest-plot of standardized beta values.}
#'            \item{\code{"std2"}}{for forest-plot of standardized beta values, however, standardization is done by dividing by two sd (see 'Details').}
#'            \item{\code{"slope"}}{to plot regression lines for each single predictor of the fitted model, against the response (linear relationship between each model term and response).}
#'            \item{\code{"resid"}}{to plot regression lines for each single predictor of the fitted model, against the residuals (linear relationship between each model term and residuals). May be used for model diagnostics.}
#'            \item{\code{"ma"}}{to check model assumptions.}
#'            \item{\code{"vif"}}{to plot Variance Inflation Factors.}
#'          }
#' @param exponentiate Logical, if \code{TRUE} and models inherit from generalized
#'          linear models, estimates will be exponentiated (e.g., log-odds will
#'          be displayed as odds ratios). By default, \code{exponentiate} will
#'          automatically be set to \code{FALSE} or \code{TRUE}, depending on
#'          the class of \code{fit}.
#' @param terms Character vector with the names of those from \code{fit}, which
#'          should be used to plot for. This argument depends on the plot-type;
#'          for \code{type = "pred"} or \code{type = "eff"}, \code{terms} indicates
#'          for which terms marginal effects should be displayed. At least one term
#'          is required to calculate effects, maximum length is three terms,
#'          where the second and third term indicate the groups, i.e. predictions
#'          of first term are grouped by the levels of the second (and third)
#'          term. Indicating levels in square brackets allows for selecting
#'          only specific groups. Term name and levels in brackets must be
#'          separated by a whitespace character, e.g.
#'          \code{terms = c("age", "education [1,3]")}. For more details, see
#'          \code{\link[ggeffects]{ggpredict}}.
#' @param rm.terms Character vector with names that indicate which terms should
#'          be removed from the plot. \code{rm.terms = "t_name"} would remove the
#'          term \emph{t_name}. Default is \code{NULL}, i.e. all terms are
#'          used.
#' @param show.intercept Logical, if \code{TRUE}, the intercept of the fitted
#'          model is also plotted. Default is \code{FALSE}. If \code{exponentiate = TRUE},
#'          please note that due to exponential transformation of estimates, the
#'          intercept in some cases is non-finite and the plot can not be created.
#' @param show.p Logical, adds significance levels to values, or value and
#'          variable labels.
#'
#' @importFrom sjstats pred_vars std_beta
#' @importFrom sjmisc word_wrap
#' @importFrom sjlabelled get_dv_labels get_term_labels
#' @importFrom broom tidy
#' @export
plot_model <- function(fit,
                       type = "est",
                       exponentiate,
                       terms = NULL,
                       sort.est = FALSE,
                       rm.terms = NULL,
                       group.terms = NULL,
                       title = NULL,
                       axis.title = NULL,
                       axis.labels = NULL,
                       axis.lim = NULL,
                       grid.breaks = NULL,
                       show.intercept = FALSE,
                       show.values = FALSE,
                       show.p = FALSE,
                       geom.size = NULL,
                       geom.colors = "Set1",
                       facets,
                       wrap.title = 50,
                       wrap.labels = 25,
                       digits = 2,
                       vline.type = 2,
                       vline.color = "grey70",
                       ...
                       ) {
  # check whether estimates should be exponentiated or not
  if (missing(exponentiate))
    exponentiate <- inherits(fit, c("glm", "glmerMod", "glmmTMB"))

  # get labels of dependent variables, and wrap them if too long
  if (is.null(title)) title <- sjlabelled::get_dv_labels(fit)
  title <- sjmisc::word_wrap(title, wrap = wrap.title)

  # labels for axis with term names
  if (is.null(axis.labels)) axis.labels <- sjlabelled::get_term_labels(fit)
  axis.labels <- sjmisc::word_wrap(axis.labels, wrap = wrap.labels)

  # title for axis with estimate values
  if (is.null(axis.title)) axis.title <- sjmisc::word_wrap(get_estimate_axis_title(fit, axis.title), wrap = wrap.title)
  axis.title <- sjmisc::word_wrap(axis.title, wrap = wrap.labels)

  # check nr of terms. if only one, plot slope
  if (type == "est" && length(sjstats::pred_vars(fit)) == 1) type <- "slope"


  if (type == "est") {
    ## TODO provide own tidier for not-supported models
    # get tidy output of summary
    dat <- broom::tidy(fit, conf.int = TRUE, effects = "fixed")

    p <- plot_model_estimates(
      fit = fit,
      dat = dat,
      exponentiate = exponentiate,
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
      digits = digits,
      geom.colors = geom.colors,
      geom.size = geom.size,
      vline.type = vline.type,
      vline.color = vline.color
    )

    return(p)

  } else if (type %in% c("std", "std2")) {
    # get tidy output of summary
    dat <- sjstats::std_beta(fit, type = type)

    p <- plot_model_estimates(
      fit = fit,
      dat = dat,
      exponentiate = exponentiate,
      terms = terms,
      group.terms = group.terms,
      rm.terms = rm.terms,
      sort.est = sort.est,
      title = title,
      axis.title = axis.title,
      axis.labels = axis.labels,
      axis.lim = axis.lim,
      grid.breaks = grid.breaks,
      show.intercept = FALSE,
      show.values = show.values,
      show.p = show.p,
      digits = digits,
      geom.colors = geom.colors,
      geom.size = geom.size,
      vline.type = vline.type,
      vline.color = vline.color
    )

    return(p)
  }

}
