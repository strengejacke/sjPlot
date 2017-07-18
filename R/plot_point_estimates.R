plot_point_estimates <- function(model, dat, geom.size, vline.type, vline.color) {
  # set up base plot
  p <- ggplot(dat, aes_string(x = "term", y = "estimate", colour = "group")) +
    geom_hline(yintercept = 0, linetype = vline.type, color = vline.color) +
    geom_point(size = geom.size) +
    geom_errorbar(aes_string(ymin = "conf.low", ymax = "conf.high"), width = 0) +
    coord_flip() +
    guides(colour = "none")


  # add value labels
  if (show.values) p <- p +
      geom_text(
        aes_string(label = "p.label"),
        hjust = -.1,
        show.legend = FALSE
      )

  # check axis labels
  if (is.null(axis.labels)) axis.labels <- sjlabelled::get_term_labels(model)

  # set axis labels
  p <- p + scale_x_discrete(labels = sjmisc::word_wrap(axis.labels, wrap = wrap.labels))


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
  p <- p + scale_colour_manual(values = col_check2(geom.colors, length(dv.labels)))

  # set axis and plot titles
  p <-
    p + labs(
      x = NULL,
      y = sjmisc::word_wrap(get_estimate_axis_title(model, axis.title), wrap = wrap.title),
      title = sjmisc::word_wrap(title, wrap = wrap.title),
    )

  p
}
