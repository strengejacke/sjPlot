plot_model_estimates <- function(fit,
                                 exponentiate,
                                 terms,
                                 group.terms,
                                 rm.terms,
                                 sort.est,
                                 title,
                                 show.intercept,
                                 show.values,
                                 show.p,
                                 digits,
                                 geom.colors,
                                 geom.size,
                                 vline.type,
                                 vline.color) {

  ## TODO provide own tidier for not-supported models

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
  if (!is.null(rm.terms)) dat <- dplyr::filter(dat$term %in% rm.terms)

  # or select further estimates
  if (!is.null(terms)) dat <- dplyr::filter(!(dat$term %in% terms))

  # add p-asterisks to data
  dat$p.stars <- get_p_stars(dat$p.value)
  dat$p.label <- sprintf("%.*f", digits, dat$estimate)

  if (show.p) dat$p.label <- sprintf("%s %s", dat$p.label, dat$p.stars)

  # group estimates?
  if (!is.null(group.terms) && length(group.terms) == nrow(dat)) {
    dat$group <- as.character(group.terms)
  } else {
    warning("Length of `group.terms` does not equal number of model coefficients. Ignoring this argument.", call. = F)
    group.terms <- NULL
  }

  # sort estimates by effect size
  if (sort.est) {
    if (!is.null(group.terms)) {
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

  plot_point_estimates(
    model = fit,
    dat = dat,
    exponentiate = exponentiate,
    dv.labels = dv.labels,
    axis.labels = axis.labels,
    axis.title = axis.title,
    wrap.labels = wrap.labels,
    wrap.title = wrap.title,
    axis.lim = axis.lim,
    grid.breaks = grid.breaks,
    show.values = show.values,
    geom.size = geom.size,
    geom.colors = geom.colors,
    vline.type = vline.type,
    vline.color = vline.color
  )
}

