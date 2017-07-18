plot_model_estimates <- function(fit,
                                 exponentiate,
                                 group.term,
                                 rm.term,
                                 sort.est,
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
  if (!is.null(rm.term)) dat <- dplyr::filter(dat$term %in% rm.term)

  # add p-asterisks to data
  dat$p.stars <- get_p_stars(dat$p.value)
  dat$p.label <- sprintf("%.*f", digits, dat$estimate)

  if (show.p) dat$p.label <- sprintf("%s %s", dat$p.label, dat$p.stars)

  # group estimates?
  if (!is.null(group.term) && length(group.term) == nrow(dat)) {
    dat$group <- as.character(group.term)
  } else {
    warning("Length of `group.term` does not equal number of model coefficients. Ignoring this argument.", call. = F)
    group.term <- NULL
  }

  # sort estimates by effect size
  if (sort.est) {
    if (!is.null(group.term)) {
      axis.labels <- rev(axis.labels[order(dat$group, dat$estimate)])
      dat <- dat[rev(order(dat$group, dat$estimate)), ]
    } else {
      axis.labels <- axis.labels[order(dat$estimate)]
      dat <- dat[order(dat$estimate), ]
    }
  } else {
    axis.labels <- rev(axis.labels)
    dat <- dat[nrow(dat):1, ]
  }

  plot_point_estimates(model = fit, dat, geom.size, vline.type, vline.color)
}
