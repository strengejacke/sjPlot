plot_point_estimates <- function(model,
                                 dat,
                                 tf,
                                 title,
                                 axis.labels,
                                 axis.title,
                                 axis.lim,
                                 grid.breaks,
                                 show.values,
                                 value.offset,
                                 geom.size,
                                 line.size,
                                 geom.colors,
                                 bpe.style,
                                 bpe.color,
                                 vline.color,
                                 value.size,
                                 facets,
                                 ci.style,
                                 ...) {

  # some defaults...

  size.inner <- .07
  spacing <- .4
  width <- if (is.stan(model)) .06 else 0

  # check additional arguments, for stan-geoms

  add.args <- lapply(match.call(expand.dots = F)$`...`, function(x) x)
  if ("size.inner" %in% names(add.args)) size.inner <- eval(add.args[["size.inner"]])
  if ("width" %in% names(add.args)) width <- eval(add.args[["width"]])
  if ("spacing" %in% names(add.args)) spacing <- eval(add.args[["spacing"]])


  # need some additional data, for stan-geoms

  dat$xpos <- sjlabelled::as_numeric(dat$term, start.at = 1)
  dat$xmin <- dat$xpos - (geom.size * size.inner)
  dat$xmax <- dat$xpos + (geom.size * size.inner)


  # set default for empty titles/labels

  if (sjmisc::is_empty(title)) title <- NULL
  if (sjmisc::is_empty(axis.labels)) axis.labels <- attributes(dat)$pretty_names
  if (sjmisc::is_empty(axis.title)) axis.title <- NULL


  # if we have non-estimable coefficients (i.e. missings)
  # remove them here

  no_coefficient <- which(is.na(dat$estimate))
  if (length(no_coefficient) > 0) {
    dat <- dat[-no_coefficient, ]
  }

  # axis limits and tick breaks for y-axis

  axis.scaling <- axis_limits_and_ticks(
    axis.lim = axis.lim,
    min.val = min(dat$conf.low),
    max.val = max(dat$conf.high),
    grid.breaks = grid.breaks,
    exponentiate = isTRUE(tf == "exp"),
    min.est = min(dat$estimate),
    max.est = max(dat$estimate)
  )


  # based on current ggplot theme, highlights vertical default line

  yintercept = dplyr::if_else(isTRUE(tf == "exp"), 1, 0)
  layer_vertical_line <- geom_intercept_line(yintercept, axis.scaling, vline.color)

  # check whether we have a multinomial log. reg. model
  multinomial <- obj_has_name(dat, "response.level")

  # basis aes mapping

  if (multinomial)
    p <- ggplot(dat, aes_string(x = "term", y = "estimate", colour = "response.level", fill = "response.level"))
  else
    p <- ggplot(dat, aes_string(x = "term", y = "estimate", colour = "group", fill = "group"))

  if (is.stan(model)) {

    if (ci.style == "whisker") {
      hdi_alpha <- 1
      dot.fac <- 1.2
    } else {
      hdi_alpha <- .5
      dot.fac <- 3
    }

    # special setup for rstan-models
    p <- p + layer_vertical_line

    if (ci.style == "whisker")
      p <- p + geom_errorbar(aes_string(ymin = "conf.low", ymax = "conf.high"), size = line.size, width = width)
    else
      p <- p + geom_rect(aes_string(ymin = "conf.low", ymax = "conf.high", xmin = "xmin", xmax = "xmax"), alpha = hdi_alpha, colour = "white", size = .5)


    # only add inner region if requested
    if (size.inner > 0) {
      p <- p +
        geom_rect(aes_string(ymin = "conf.low50", ymax = "conf.high50", xmin = "xmin", xmax = "xmax"), alpha = hdi_alpha, colour = "white", size = .5)
    }

    # define style for Bayesian point estimate
    if (bpe.style == "line") {
      if (is.null(bpe.color)) {
        p <- p +
          geom_segment(aes_string(x = "xmin", xend = "xmax", y = "estimate", yend = "estimate"), size = geom.size * .9)
      } else {
        p <- p +
          geom_segment(aes_string(x = "xmin", xend = "xmax", y = "estimate", yend = "estimate"), colour = bpe.color, size = geom.size * .9)
      }
    } else if (is.null(bpe.color)) {
        p <- p +
          geom_point(aes_string(y = "estimate"), fill = "white", size = geom.size * dot.fac)
    } else {
      p <- p +
        geom_point(aes_string(y = "estimate"), fill = "white", colour = bpe.color, size = geom.size * dot.fac)
    }

  } else {

    # setup base plot
    p <- p + layer_vertical_line

    if (multinomial) {
      p <- p +
        geom_point(size = geom.size, position = position_dodge(width = spacing)) +
        geom_errorbar(aes_string(ymin = "conf.low", ymax = "conf.high"), position = position_dodge(width = spacing), width = width, size = line.size)
    } else {
      p <- p +
        geom_point(size = geom.size) +
        geom_errorbar(aes_string(ymin = "conf.low", ymax = "conf.high"), width = width, size = line.size)
    }

  }


  # set up base aes, either with or w/o groups

  p <- p + coord_flip()

  if (multinomial) {
    col.len <- dplyr::n_distinct(dat$response.level)
    # remove legend
    p <- p + guides(fill = "none")
  } else {
    col.len <- dplyr::n_distinct(dat$group)
    # remove legend
    p <- p + guides(colour = "none", fill = "none")
  }


  # add value labels

  if (show.values) p <- p +
      geom_text(
        aes_string(label = "p.label"),
        nudge_x = value.offset,
        show.legend = FALSE,
        size = value.size
      )


  # set axis labels

  if (!is.null(axis.labels))
    p <- p + scale_x_discrete(labels = axis.labels)


  # we need transformed scale for exponentiated estimates

  has_zeroinf <- (obj_has_name(dat, "wrap.facet") && dplyr::n_distinct(dat$wrap.facet, na.rm = TRUE) > 1)

  if (isTRUE(tf == "exp")) {

    if (has_zeroinf) {
      p <- p + scale_y_continuous(trans = "log10")
    } else {
      p <- p + scale_y_continuous(
        trans = "log10",
        limits = axis.scaling$axis.lim,
        breaks = axis.scaling$ticks,
        labels = prettyNum
      )
    }

  } else {

    if (has_zeroinf) {

    } else {
      p <- p + scale_y_continuous(
        limits = axis.scaling$axis.lim,
        breaks = axis.scaling$ticks,
        labels = axis.scaling$ticks
      )
    }

  }


  # set colors

  p <- p +
    scale_colour_manual(values = col_check2(geom.colors, col.len)) +
    scale_fill_manual(values = col_check2(geom.colors, col.len))


  # facets?

  if (obj_has_name(dat, "facet") && dplyr::n_distinct(dat$facet, na.rm = TRUE) > 1)
    p <- p +
      facet_grid(~facet)
  else if (has_zeroinf)
    p <- p +
      facet_wrap(~wrap.facet, ncol = 1, scales = "free")


  # set axis and plot titles

  if (length(axis.title) > 1) axis.title <- axis.title[1]

  p <-
    p + labs(
      x = NULL,
      y = axis.title,
      title = title
    )

  # for multinomial models, set response variable name as name for legend
  if (multinomial) p <- p + labs(colour = insight::find_response(model))

  p
}
