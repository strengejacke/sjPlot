#' @importFrom tibble has_name
#' @importFrom broom tidy
plot_type_est <- function(type,
                          ci.lvl,
                          tf,
                          model,
                          terms,
                          group.terms,
                          rm.terms,
                          sort.est,
                          title,
                          axis.title,
                          axis.labels,
                          axis.lim,
                          grid.breaks,
                          show.intercept,
                          show.values,
                          show.p,
                          value.offset,
                          digits,
                          geom.colors,
                          geom.size,
                          line.size,
                          order.terms,
                          vline.color,
                          value.size,
                          bpe,
                          bpe.style,
                          ...) {

  # get tidy output of summary ----

  if (type == "est" || type == "re") {
    dat <- tidy_model(model, ci.lvl, tf, type, bpe, ...)
  } else {
    dat <- model %>%
      sjstats::std_beta(type = type, ci.lvl = ci.lvl) %>%
      tibble::add_column(p.value = sjstats::p_value(model)[["p.value"]][-1]) %>%
      sjmisc::var_rename(std.estimate = "estimate")

    show.intercept <- FALSE
  }


  # for stan-models, we can define the style of the Bayesian point estimate,
  # which may be a line or a dot.

  if (missing(bpe.style) || is.null(bpe.style)) bpe.style <- "line"
  if (missing(value.size) || is.null(value.size)) value.size <- 4


  plot_model_estimates(
    model = model,
    dat = dat,
    tf = tf,
    terms = terms,
    group.terms = group.terms,
    rm.terms = rm.terms,
    sort.est = sort.est,
    title = title,
    axis.title = axis.title,
    axis.labels = axis.labels,
    axis.lim = axis.lim,
    grid.breaks = grid.breaks,
    show.intercept = show.intercept,
    show.values = show.values,
    show.p = show.p,
    value.offset = value.offset,
    digits = digits,
    geom.colors = geom.colors,
    geom.size = geom.size,
    line.size = line.size,
    bpe.style = bpe.style,
    term.order = order.terms,
    vline.color = vline.color,
    value.size = value.size,
    ...
  )
}
