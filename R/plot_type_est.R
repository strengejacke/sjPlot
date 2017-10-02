#' @importFrom tibble has_name
#' @importFrom broom tidy
plot_type_est <- function(type,
                          ci.lvl,
                          exponentiate,
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
                          ...) {

  # get tidy output of summary ----

  if (type == "est") {
    ## TODO provide own tidier for not-supported models
    dat <- tidy_model(model, ci.lvl, exponentiate, type, ...)
  } else {
    dat <- model %>%
      sjstats::std_beta(type = type, ci.lvl = ci.lvl) %>%
      tibble::add_column(p.value = sjstats::p_value(model)[["p.value"]][-1]) %>%
      sjmisc::var_rename(std.estimate = "estimate")

    show.intercept <- FALSE
  }


  # for stan-models, we can define the style of the Bayesian point estimate,
  # which may be a line or a dot.

  bpe.style <- "line"

  # additional arguments for 'effects()'-function?

  add.args <- lapply(match.call(expand.dots = F)$`...`, function(x) x)
  if ("bpe.style" %in% names(add.args)) bpe.style <- add.args[["bpe.style"]]


  plot_model_estimates(
    fit = model,
    dat = dat,
    exponentiate = exponentiate,
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
    vline.color = vline.color
  )
}
