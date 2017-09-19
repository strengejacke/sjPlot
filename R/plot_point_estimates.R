#' @importFrom tibble has_name
#' @importFrom dplyr n_distinct
#' @importFrom sjmisc to_value
plot_point_estimates <- function(model,
                                 dat,
                                 exponentiate,
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
                                 vline.type,
                                 vline.color,
                                 bpe.style) {

  # need some additional data, for stan-geoms

  dat$xpos <- sjmisc::to_value(dat$term, start.at = 1)
  dat$xmin <- dat$xpos - (geom.size * .1)
  dat$xmax <- dat$xpos + (geom.size * .1)


  # basis aes mapping
  p <- ggplot(dat, aes_string(x = "term", y = "estimate", colour = "group", fill = "group"))

  if (is.stan(model)) {
    # special setup for rstan-models
    p <- p +
      geom_hline(yintercept = ifelse(exponentiate, 1, 0), linetype = vline.type, color = vline.color) +
      geom_errorbar(aes_string(ymin = "conf.low", ymax = "conf.high"), size = line.size, width = .06) +
      geom_rect(aes_string(ymin = "conf.low50", ymax = "conf.high50", xmin = "xmin", xmax = "xmax"), colour = "white", size = .5)

    # define style for Bayesian point estimate
    if (bpe.style == "line")
      p <- p +
        geom_segment(aes_string(x = "xmin", xend = "xmax", y = "estimate", yend = "estimate"), colour = "white", size = geom.size * .9)
    else
      p <- p +
        geom_point(aes_string(y = "estimate"), fill = "white", colour = "white", size = geom.size * 1.2)
  } else {
    # setup base plot
    p <- p +
      geom_hline(yintercept = ifelse(exponentiate, 1, 0), linetype = vline.type, color = vline.color) +
      geom_point(size = geom.size) +
      geom_errorbar(aes_string(ymin = "conf.low", ymax = "conf.high"), width = 0, size = line.size)
  }


  # set up base aes, either with or w/o groups

  col.len <- dplyr::n_distinct(dat$group)


  # flip plot, and remove legend

  p <- p +
    coord_flip() +
    guides(colour = "none", fill = "none")


  # add value labels

  if (show.values) p <- p +
      geom_text(
        aes_string(label = "p.label"),
        nudge_x = value.offset,
        show.legend = FALSE
      )


  # set axis labels

  p <- p + scale_x_discrete(labels = axis.labels)


  # axis limits and tick breaks for y-axis

  axis.scaling <- get_axis_limits_and_ticks(
    axis.lim = axis.lim,
    min.val = min(dat$conf.low),
    max.val = max(dat$conf.high),
    grid.breaks = grid.breaks,
    exponentiate = exponentiate
  )


  # we need transformed scale for exponentiated estimates

  if (exponentiate) {
    p <- p + scale_y_continuous(
      trans = "log10",
      limits = axis.scaling$axis.lim,
      breaks = axis.scaling$ticks,
      labels = prettyNum
    )
  } else {
    p <- p + scale_y_continuous(
      limits = axis.scaling$axis.lim,
      breaks = axis.scaling$ticks,
      labels = axis.scaling$ticks
    )
  }


  # set colors

  p <- p +
    scale_colour_manual(values = col_check2(geom.colors, col.len)) +
    scale_fill_manual(values = col_check2(geom.colors, col.len))


  # set axis and plot titles

  p <-
    p + labs(
      x = NULL,
      y = axis.title,
      title = title
    )

  p
}
