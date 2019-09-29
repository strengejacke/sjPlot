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
                          dot.size,
                          line.size,
                          ...) {

  if (missing(facets) || is.null(facets)) facets <- FALSE

  if (type == "pred") {
    dat <- ggeffects::ggpredict(
      model = model,
      terms = terms,
      ci.lvl = ci.lvl,
      type = pred.type,
      ...
    )
  } else if (type == "emm") {
    dat <- ggeffects::ggemmeans(
      model = model,
      terms = terms,
      ci.lvl = ci.lvl,
      type = pred.type,
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


  if (is.null(dat)) return(NULL)

  # evaluate dots-arguments
  alpha <- .15
  dodge <- .1
  dot.alpha <- .5
  log.y <- FALSE

  # save number of terms, needed later
  n.terms <- length(insight::find_predictors(model, component = "conditional", flatten = TRUE))

  add.args <- lapply(match.call(expand.dots = F)$`...`, function(x) x)
  if ("alpha" %in% names(add.args)) alpha <- eval(add.args[["alpha"]])
  if ("dodge" %in% names(add.args)) dodge <- eval(add.args[["dodge"]])
  if ("dot.alpha" %in% names(add.args)) dot.alpha <- eval(add.args[["dot.alpha"]])
  if ("log.y" %in% names(add.args)) log.y <- eval(add.args[["log.y"]])


  # select color palette
  if (geom.colors[1] != "bw") {
    if (is.null(terms)) {
      if (facets) {
        geom.colors <- "bw"
        .ngrp <- n.terms
      } else {
        .ngrp <- 1
      }
    } else {
      .ngrp <- dplyr::n_distinct(dat$group)
    }
    geom.colors <- col_check2(geom.colors, .ngrp)
  }


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
    log.y = log.y,
    dot.size = dot.size,
    line.size = line.size
  )


  # set axis and plot titles
  if (!is.null(axis.title) && !is.null(terms)) {
    if (length(axis.title) > 1) {
      p <- p + labs(x = axis.title[1], y = axis.title[2])
    } else {
      p <- p + labs(y = axis.title)
    }
  } else if (!is.null(axis.title) && is.null(terms)) {
    if (length(axis.title) > 1) {
      p <- purrr::map(p, ~ .x + labs(x = axis.title[1], y = axis.title[2]))
    } else {
      p <- purrr::map(p, ~ .x + labs(y = axis.title))
    }
  }

  # set axis and plot titles
  if (!is.null(title) && !is.null(terms))
    p <- p + ggtitle(title)
  else if (!is.null(title) && is.null(terms))
    p <- purrr::map(p, ~ .x + ggtitle(title))

  # set axis and plot titles
  if (!is.null(legend.title)) {
    if (geom.colors[1] == "bw") {
      p <- p +
        labs(linetype = legend.title) +
        guides(colour = "none")
    } else {
      p <- p + labs(colour = legend.title)
    }
  }


  # set axis limits
  if (!is.null(axis.lim)) {
    if (is.list(axis.lim))
      p <- p + xlim(axis.lim[[1]]) + ylim(axis.lim[[2]])
    else
      p <- p + ylim(axis.lim)
  }


  p
}
