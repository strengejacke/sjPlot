#' @param type Type of plot. Use one of following:
#'          \describe{
#'            \item{\code{"est"}}{(default) for forest-plot like plot of estimates. If the fitted model only contains one predictor, intercept and slope are plotted.}
#'            \item{\code{"pred"}}{to plot predicted values for the response, related to specific predictors. See 'Details'.}
#'            \item{\code{"eff"}}{to plot marginal effects of all terms in \code{fit}. Note that interaction terms are excluded from this plot.}
#'            \item{\code{"int"}}{to plot marginal effects of interaction terms in \code{fit}.}
#'            \item{\code{"std"}}{for forest-plot like plot of standardized beta values. If the fitted model only contains one predictor, intercept and slope are plotted.}
#'            \item{\code{"std2"}}{for forest-plot like plot of standardized beta values, however, standardization is done by dividing by two sd (see 'Details'). If the fitted model only contains one predictor, intercept and slope are plotted.}
#'            \item{\code{"slope"}}{to plot regression lines for each single predictor of the fitted model, against the response (linear relationship between each model term and response).}
#'            \item{\code{"resid"}}{to plot regression lines for each single predictor of the fitted model, against the residuals (linear relationship between each model term and residuals). May be used for model diagnostics.}
#'            \item{\code{"poly"}}{to plot predicted values (marginal effects) of polynomial terms in \code{fit}. Use \code{poly.term} to specify the polynomial term in the fitted model (see 'Examples').}
#'            \item{\code{"ma"}}{to check model assumptions. Note that only three arguments are relevant for this option \code{fit} and \code{complete.dgns}. All other arguments are ignored.}
#'            \item{\code{"vif"}}{to plot Variance Inflation Factors.}
#'          }
#' @param exponentiate Logical, if \code{TRUE} and models inherit from generalized
#'          linear models, estimates will be exponentiated (e.g., log-odds will
#'          be displayed as odds ratios). By default, \code{exponentiate} will
#'          automatically be set to \code{FALSE} or \code{TRUE}, depending on
#'          the class of \code{fit}.
#' @param rm.term Character vector with names that indicate which terms should
#'          be removed from the plot. \code{rm.term = "t_name"} would remove the
#'          term \emph{t_name}. Default is \code{NULL}, i.e. all terms are
#'          used.
#' @param show.intercept Logical, if \code{TRUE}, the intercept of the fitted
#'          model is also plotted. Default is \code{FALSE}. If \code{exponentiate = TRUE},
#'          please note that due to exponential transformation of estimates, the
#'          intercept in some cases is non-finite and the plot can not be created.
#'
#' @export
plot_model <- function(fit,
                       type = "est",
                       exponentiate,
                       sort.est = FALSE,
                       rm.term = NULL,
                       group.est = NULL,
                       show.intercept = FALSE,
                       show.p = TRUE,
                       title = NULL,
                       axis.labels = NULL,
                       wrap.title = 50,
                       wrap.labels = 25,
                       digits = 2,
                       ...
                       ) {
  # check whether estimates should be exponentiated or not
  if (missing(exponentiate))
    exponentiate <- inherits(fit, c("glm", "glmerMod", "glmmTMB"))

  # get labels of dependent variables, and wrap them if too long
  if (is.null(title)) title <- sjlabelled::get_dv_labels(fit)
  title <- sjmisc::word_wrap(title, wrap = wrap.title)

  if (is.null(axis.labels)) axis.labels <- sjlabelled::get_term_labels(fit)
  axis.labels <- sjmisc::word_wrap(axis.labels, wrap = wrap.labels)

  if (type == "est")
    plot_model_estimates(
      fit = fit,
      exponentiate = exponentiate,
      group.est = group.est,
      rm.term = rm.term,
      sort.est = sort.est,
      title = title,
      show.intercept = show.intercept,
      show.p = show.p,
      digits = digits
    )

}
