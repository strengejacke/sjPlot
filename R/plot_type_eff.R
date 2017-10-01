#' @importFrom ggeffects ggpredict ggeffect
plot_type_eff <- function(type,
                          model,
                          terms,
                          ci.lvl,
                          pred.type,
                          facets,
                          show.data,
                          geom.colors,
                          axis.title,
                          title,
                          axis.lim,
                          case,
                          ...) {
  if (type == "pred") {
    dat <- ggeffects::ggpredict(
      model = model,
      terms = terms,
      ci.lvl = ci.lvl,
      type = pred.type,
      full.data = FALSE,
      ...
    )
  } else {
    dat <- ggeffects::ggeffect(
      model = model,
      terms = terms,
      ci.lvl = ci.lvl,
      ...
    )
  }

  p <- graphics::plot(
    dat,
    ci = !is.na(ci.lvl),
    facets = facets,
    rawdata = show.data,
    colors = geom.colors,
    use.theme = FALSE,
    case = case,
    ...
  )

  # set axis and plot titles
  if (!is.null(axis.title)) {
    if (length(axis.title) > 1) {
      p <- p + labs(x = axis.title[1],
                    y = axis.title[2])
    } else {
      p <- p + labs(y = axis.title)
    }
  }

  # set axis and plot titles
  if (!is.null(title))
    p <- p + ggtitle(title)

  # set axis limits
  if (!is.null(axis.lim))
    p <- p + ylim(axis.lim)

  p
}
