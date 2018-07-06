#' @importFrom ggeffects ggpredict ggeffect
plot_type_eff <- function(type,
                          model,
                          terms,
                          ci.lvl,
                          pred.type,
                          facets,
                          show.data,
                          jitter,
                          geom.colors,
                          axis.title,
                          title,
                          legend.title,
                          axis.lim,
                          case,
                          show.legend,
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

  # evaluate dots-arguments
  alpha <- .15
  dodge <- .1
  dot.alpha <- .5
  log.y <- FALSE

  add.args <- lapply(match.call(expand.dots = F)$`...`, function(x) x)
  if ("alpha" %in% names(add.args)) alpha <- eval(add.args[["alpha"]])
  if ("dodge" %in% names(add.args)) dodge <- eval(add.args[["dodge"]])
  if ("dot.alpha" %in% names(add.args)) dot.alpha <- eval(add.args[["dot.alpha"]])
  if ("log.y" %in% names(add.args)) log.y <- eval(add.args[["log.y"]])


  # select color palette
  if (geom.colors[1] != "bw")
    geom.colors <- col_check2(geom.colors, dplyr::n_distinct(dat$group))

  p <- graphics::plot(
    dat,
    ci = !is.na(ci.lvl),
    facets = facets,
    rawdata = show.data,
    colors = geom.colors,
    use.theme = FALSE,
    jitter = jitter,
    case = case,
    show.legend = show.legend,
    dot.alpha = dot.alpha,
    alpha = alpha,
    dodge = dodge,
    log.y = log.y
  )

  # set axis and plot titles
  if (!is.null(axis.title)) {
    if (length(axis.title) > 1) {
      p <- p + labs(x = axis.title[1], y = axis.title[2])
    } else {
      p <- p + labs(y = axis.title)
    }
  }

  # set axis and plot titles
  if (!is.null(title))
    p <- p + ggtitle(title)

  # set axis and plot titles
  if (!is.null(legend.title))
    p <- p + labs(colour = legend.title)

  # set axis limits
  if (!is.null(axis.lim)) {
    if (is.list(axis.lim))
      p <- p + xlim(axis.lim[[1]]) + ylim(axis.lim[[2]])
    else
      p <- p + ylim(axis.lim)
  }


  p
}
