#' @title Various plot types for regression models
#' @name plot_model
#'
#' @description To Do...
#'
#' @param type Type of plot. Use one of following:
#'          \describe{
#'            \item{\code{"est"}}{(default) for forest-plot of estimates. If the fitted model only contains one predictor, slope-line is plotted.}
#'            \item{\code{"re"}}{for mixed effects models, plots the random effects.}
#'            \item{\code{"pred"}}{to plot predicted values (marginal effects) for specific model terms. \code{\link[ggeffects]{ggpredict}}.}
#'            \item{\code{"eff"}}{similar to \code{type = "pred"}, however, discrete predictors are held constant at their proportions (not reference level). See \code{\link[ggeffects]{ggeffect}} for details.}
#'            \item{\code{"int"}}{to plot marginal effects of interaction terms in \code{model}.}
#'            \item{\code{"std"}}{for forest-plot of standardized beta values.}
#'            \item{\code{"std2"}}{for forest-plot of standardized beta values, however, standardization is done by dividing by two sd (see 'Details').}
#'            \item{\code{"slope"}}{to plot regression lines for each single predictor of the fitted model, against the response (linear relationship between each model term and response).}
#'            \item{\code{"resid"}}{to plot regression lines for each single predictor of the fitted model, against the residuals (linear relationship between each model term and residuals). May be used for model diagnostics.}
#'            \item{\code{"diag"}}{to check model assumptions.}
#'            \item{\code{"vif"}}{to plot Variance Inflation Factors.}
#'          }
#' @param terms Character vector with the names of those terms from \code{model}
#'          that should be plotted. This argument depends on the plot-type:
#'          \describe{
#'            \item{\code{type = "est"}}{
#'              Select terms that should be plotted. All other term are removed
#'              from the output.
#'            }
#'            \item{\code{type = "pred"} or \code{type = "eff"}}{
#'              Here \code{terms} indicates for which terms marginal effects
#'              should be displayed. At least one term is required to calculate
#'              effects, maximum length is three terms, where the second and
#'              third term indicate the groups, i.e. predictions of first term
#'              are grouped by the levels of the second (and third) term.
#'              Indicating levels in square brackets allows for selecting only
#'              specific groups. Term name and levels in brackets must be separated
#'              by a whitespace character, e.g. \code{terms = c("age", "education [1,3]")}.
#'              For more details, see \code{\link[ggeffects]{ggpredict}}.
#'            }
#'          }
#' @param ... Other arguments, passed down to various functions. Here is the
#'        description of these arguments in detail.
#'        \describe{
#'          \item{\code{prob.inner} and \code{prob.outer}}{
#'            For \code{stanreg}-models (fitted with the \pkg{rstanarm}-package)
#'            and plot-type \code{type = "est"}, you can specify numeric values
#'            between 0 and 1 for \code{prob.inner} and \code{prob.outer}, which
#'            will then be used as inner and outer probabilities for the uncertainty
#'            intervals (HDI). By default, the inner probability is 0.5 and the
#'            outer probability is 0.89 (unless \code{ci.lvl} is specified - in
#'            this case, \code{ci.lvl} is used as outer probability).
#'          }
#'          \item{\code{bpe}}{
#'            Also for \code{stanreg}-models (fitted with the \pkg{rstanarm}-package),
#'            the Bayesian point estimate is, by default, the median of the posterior
#'            distribution. Use \code{bpe} to define other functions to calculate
#'            the Bayesion point estimate. \code{bpe} needs to be a character naming
#'            the specific function, which is passed to the \code{fun}-argument
#'            in \code{\link[sjstats]{typical_value}}. So, \code{bpe = "mean"}
#'            would calculate the mean value of the posterior distribution.
#'          }
#'          \item{\code{bpe.style}}{
#'            Again for \code{stanreg}-models (fitted with the \pkg{rstanarm}-package),
#'            the Bayesian point estimate is indicated as a small, vertical line
#'            by default. Use \code{bpe.style = "dot"} to plot a dot instead
#'            of a line for the point estimate.
#'          }
#'        }
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
                       exponentiate,
                       terms = NULL,
                       sort.est = FALSE,
                       rm.terms = NULL,
                       group.terms = NULL,
                       order.terms = NULL,
                       pred.type = c("fe", "re"),
                       title = NULL,
                       axis.title = NULL,
                       axis.labels = NULL,
                       axis.lim = NULL,
                       grid.breaks = NULL,
                       ci.lvl = NULL,
                       show.intercept = FALSE,
                       show.values = FALSE,
                       show.p = TRUE,
                       show.data = FALSE,
                       value.offset = NULL,
                       dot.size = NULL,
                       line.size = NULL,
                       colors = "Set1",
                       facets,
                       wrap.title = 50,
                       wrap.labels = 25,
                       case = NULL,
                       digits = 2,
                       vline.color = NULL,
                       ...
                       ) {

  type <- match.arg(type)
  pred.type <- match.arg(pred.type)

  # get titles and labels for axis ----

  # get labels of dependent variables, and wrap them if too long
  if (is.null(title)) title <- sjlabelled::get_dv_labels(model, case = case)
  title <- sjmisc::word_wrap(title, wrap = wrap.title)

  # labels for axis with term names
  if (is.null(axis.labels)) axis.labels <- sjlabelled::get_term_labels(model, case = case)
  axis.labels <- sjmisc::word_wrap(axis.labels, wrap = wrap.labels)

  # title for axis with estimate values
  if (is.null(axis.title)) axis.title <- sjmisc::word_wrap(get_estimate_axis_title(model, axis.title, type), wrap = wrap.title)
  axis.title <- sjmisc::word_wrap(axis.title, wrap = wrap.labels)

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


  # check whether estimates should be exponentiated or not
  if (missing(exponentiate))
    exponentiate <- !get_glm_family(model)[["is_linear"]]


  if (type %in% c("est", "std", "std2")) {

    # plot estimates ----

    p <- plot_type_est(
      type = type,
      ci.lvl = ci.lvl,
      exponentiate = exponentiate,
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
      ...
    )
  } else if (type == "re") {

    # plot random effects ----

    p <- plot_type_ranef(

    )

  } else if (type %in% c("pred", "eff")) {

    # plot marginal effects ----

    p <- plot_type_eff(
      type = type,
      model = model,
      terms = terms,
      ci.lvl = ci.lvl,
      pred.type = pred.type,
      facets = facets,
      show.data = show.data,
      geom.colors = colors,
      axis.title = axis.title,
      title = title,
      axis.lim = axis.lim,
      case = case,
      ...
    )
  } else if (type == "int") {

    # plot interaction terms ----




  } else if (type == "diag") {

    # plot diagnostic plots ----

    if (is.stan(model)) {
      p <- plot_diag_stan(
        model = model,
        geom.colors = colors,
        facets = facets,
        ...
      )

    } else {

    }

  }

  p
}
