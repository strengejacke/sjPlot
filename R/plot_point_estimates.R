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

  if (inherits(model, c("stanreg", "stanfit"))) {
    # special setup for rstan-models
    p <- plot_rstan_estimates(
      model,
      dat,
      exponentiate,
      geom.size,
      geom.colors,
      vline.type,
      vline.color
    )

    # only one color
    col.len <- 1
  } else {
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
      geom_hline(yintercept = ifelse(exponentiate, 1, 0), linetype = vline.type, color = vline.color) +
      geom_point(size = geom.size) +
      geom_errorbar(aes_string(ymin = "conf.low", ymax = "conf.high"), width = 0)
  }

  # flip plot, and remove legend
  p <- p +
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



#' @importFrom sjmisc to_value
plot_rstan_estimates <-
  function(model,
           dat,
           exponentiate,
           geom.size,
           geom.colors,
           vline.type,
           vline.color) {

  dat$xpos <- sjmisc::to_value(dat$term, start.at = 1)
  dat$xmin <- dat$xpos - (geom.size * .1)
  dat$xmax <- dat$xpos + (geom.size * .1)

  p <- ggplot(dat, aes_string(x = "term")) +
    geom_hline(yintercept = ifelse(exponentiate, 1, 0), linetype = vline.type, color = vline.color) +
    geom_errorbar(
      aes_string(ymin = "conf.low", ymax = "conf.high"),
      width = .05
    ) +
    geom_rect(
      aes_string(ymin = "conf.low50", ymax = "conf.high50", xmin = "xmin", xmax = "xmax"),
      colour = "white",
      size = .5
    ) +
    geom_point(
      aes_string(y = "estimate"),
      fill = "white",
      colour = "white",
      size = geom.size * 1.2
    )

  p
}
