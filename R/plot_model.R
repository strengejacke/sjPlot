#' @param rm.est Character vector with coefficient names that indicate
#'          which estimates should be removed from the plot.
#'          \code{remove.estimates = "est_name"} would remove the estimate \emph{est_name}. Default
#'          is \code{NULL}, i.e. all estimates are printed.
#'
#' @export
plot_model <- function(fit,
                       type = "est",
                       exponentiate,
                       show.intercept,
                       rm.est,
                       title = NULL,
                       axis.labels = NULL,
                       wrap.title = 50,
                       wrap.labels = 25,
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
    plot_model_estimates(fit, exponentiate, show.intercept, rm.est, title)

}


plot_model_estimates <-
  function(fit,
           exponentiate,
           rm.est,
           title,
           show.intercept,
           show.p,
           digits) {
  # get tidy output of summary
  dat <- broom::tidy(fit, conf.int = TRUE, effects = "fixed")

  # remove intercept from output
  if (!show.intercept) dat <- dplyr::slice(dat, -1)

  # exponentiation from broom::tidy does not work with merMod-objecs,
  # so we do it manually for all model classes
  if (exponentiate) {
    dat[["estimate"]] <- exp(dat[["estimate"]])
    dat[["conf.low"]] <- exp(dat[["conf.low"]])
    dat[["conf.high"]] <- exp(dat[["conf.high"]])
  }

  # remove further estimates
  if (!is.null(rm.est)) dat <- dplyr::filter(dat$term %in% rm.est)

  # add p-asterisks to data
  dat$p.stars <- get_p_stars(dat$p.value)
  dat$p.label <- sprintf("%.*f", digits, dat$estimate)
  if (show.p) dat$p.label <- sprintf("%s %s", dat$p.label, dat$p.stars)

}
