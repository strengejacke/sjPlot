#' @importFrom tibble has_name
#' @importFrom dplyr n_distinct
plot_point_estimates <- function(model,
                                 dat,
                                 exponentiate,
                                 title,
                                 axis.labels,
                                 axis.title,
                                 axis.lim,
                                 grid.breaks,
                                 show.values,
                                 geom.size,
                                 geom.colors,
                                 vline.type,
                                 vline.color) {

  # set up base aes, either with or w/o groups
  if (tibble::has_name(dat, "group")) {
    p <- ggplot(dat, aes_string(x = "term", y = "estimate", colour = "group"))
    col.len <- dplyr::n_distinct(dat$group)
  } else {
    p <- ggplot(dat, aes_string(x = "term", y = "estimate"))
    col.len <- 1
  }


  # setup base plot
  p <- p +
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
  p <- p + scale_colour_manual(values = col_check2(geom.colors, col.len))

  # set axis and plot titles
  p <-
    p + labs(
      x = NULL,
      y = axis.title,
      title = title
    )

  p
}
