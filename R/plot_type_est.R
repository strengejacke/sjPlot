#' @importFrom sjmisc add_variables
plot_type_est <- function(type,
                          ci.lvl,
                          se,
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
                          bpe.color,
                          facets,
                          show.zeroinf,
                          p.threshold,
                          p.val,
                          vcov.fun,
                          vcov.type,
                          vcov.args,
                          ci.style,
                          p_adjust,
                          std.response,
                          ...) {

  if (missing(facets)) facets <- TRUE

  # get tidy output of summary ----

  if (type == "std" || type == "std2") {
    std_method <- switch(type, "std" = "refit", "std2" = "2sd", "refit")
  } else {
    std_method <- FALSE
  }

  dat <-
    tidy_model(
      model = model,
      ci.lvl = ci.lvl,
      tf = tf,
      type = type,
      bpe = bpe,
      robust = list(vcov.fun = vcov.fun, vcov.type = vcov.type, vcov.args = vcov.args),
      facets = facets,
      show.zeroinf = show.zeroinf,
      p.val = p.val,
      standardize = std_method,
      bootstrap = FALSE,
      iterations = 1000,
      seed = NULL,
      p_adjust = p_adjust,
      std.response = std.response,
      ...
    )

  # fix brms coefficient names

  if (inherits(model, "brmsfit")) {
    dat$term <- gsub("^b_", "", dat$term)
  }


  # check if facet groups need to be replaced with title

  if (length(title) > 1) {

    tnames <- names(title)

    if (obj_has_name(dat, "facet") && !is.null(tnames)) {
      if (all(tnames %in% dat$facet)) {
        for (i in tnames) {
          dat$facet[which(dat$facet == i)] <- title[i]
        }
        title <- ""
      }
    }

    if (obj_has_name(dat, "response.level") && !is.null(tnames)) {
      if (all(tnames %in% dat$response.level)) {
        for (i in tnames) {
          dat$response.level[which(dat$response.level == i)] <- title[i]
        }
        title <- ""
      }
    }

  }


  # se needs to be logical from here on
  if (!is.null(se) && !is.logical(se)) se <- TRUE

  # for stan-models, we can define the style of the Bayesian point estimate,
  # which may be a line or a dot.

  if (missing(bpe.style) || is.null(bpe.style)) bpe.style <- "line"
  if (missing(value.size) || is.null(value.size)) value.size <- 4


  plot_model_estimates(
    model = model,
    dat = dat,
    tf = tf,
    se = se,
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
    bpe.color = bpe.color,
    term.order = order.terms,
    vline.color = vline.color,
    value.size = value.size,
    facets = facets,
    p.threshold = p.threshold,
    ci.style = ci.style,
    ...
  )
}
