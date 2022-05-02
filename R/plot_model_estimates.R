plot_model_estimates <- function(model,
                                 dat,
                                 tf,
                                 se,
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
                                 bpe.style,
                                 bpe.color,
                                 term.order,
                                 vline.color,
                                 value.size,
                                 facets,
                                 p.threshold,
                                 ci.style,
                                 ...) {

  # remove intercept(s) from output

  if (!show.intercept) {
    ints1 <- string_contains("(Intercept", x = dat$term)
    ints2 <- string_contains("b_Intercept", x = dat$term)
    ints3 <- string_contains("b_zi_Intercept", x = dat$term)
    ints4 <- which(dat$term %in% "Intercept")

    ints <- c(ints1, ints2, ints3, ints4)

    if (!sjmisc::is_empty(ints))
      dat <- dplyr::slice(dat, !! -ints)
  }


  # remove non-coefficients

  noncoef <- string_contains("Log(theta)", x = dat$term)
  if (!sjmisc::is_empty(noncoef)) dat <- dplyr::slice(dat, !! -noncoef)


  # exponentiation

  if (!is.null(tf) && !is.stan(model)) {

    # no transformation if standard errors should be reported
    # instead of conf. int.

    if (isTRUE(se)) {
      message("If standard errors are requested, no transformation is applied to estimates.")
      tf <- NULL
    } else {
      funtrans <- match.fun(tf)
      dat[["estimate"]] <- funtrans(dat[["estimate"]])
      dat[["conf.low"]] <- funtrans(dat[["conf.low"]])
      dat[["conf.high"]] <- funtrans(dat[["conf.high"]])
    }

  }


  # use standard error instead of ci's?

  if (isTRUE(se)) {
    dat[["conf.low"]] <- dat[["estimate"]] - dat[["std.error"]]
    dat[["conf.high"]] <- dat[["estimate"]] + dat[["std.error"]]
  }


  # remove further estimates

  terms <- parse_terms(terms)
  filter.remove <- dat$term %in% terms
  if (!is.null(terms)) dat <- dplyr::filter(dat, !! filter.remove)


  # or select further estimates

  rm.terms <- parse_terms(rm.terms)
  filter.remove <- !(dat$term %in% rm.terms)
  if (!is.null(rm.terms)) dat <- dplyr::filter(dat, !! filter.remove)


  # add p-asterisks to data

  dat$p.stars <- get_p_stars(dat$p.value, p.threshold)
  dat$p.label <- sprintf("%.*f", digits, dat$estimate)

  if (show.p) dat$p.label <- sprintf("%s %s", dat$p.label, dat$p.stars)


  # create default grouping, depending on the effect:
  # split positive and negative associations with outcome
  # into different groups

  treshold <- dplyr::if_else(isTRUE(tf == "exp"), 1, 0)
  dat$group <- dplyr::if_else(dat$estimate > treshold, "pos", "neg")


  # group estimates?

  if (!is.null(group.terms)) {
    if (length(group.terms) == nrow(dat)) {
      dat$group <- as.character(group.terms)
    } else {
      warning("Length of `group.terms` does not equal number of model coefficients. Ignoring this argument.", call. = F)
      group.terms <- NULL
    }
  }


  # make term name categorical, for axis labelling
  dat$term <- as.factor(dat$term)


  # does user want a specific order for terms?

  ordered.terms <- FALSE
  if (!is.null(term.order)) {
    if (length(term.order) == nrow(dat)) {
      dat$term <- factor(dat$term, levels = unique(dat$term)[rev(term.order)])
      sort.est <- FALSE
      ordered.terms <- TRUE
    } else {
      message("Number of values in `order.terms` does not match number of terms. Terms are not sorted.")
    }
  }


  # sort estimates by effect size

  if (isTRUE(sort.est)) {
    if (!is.null(group.terms))
      dat$term <- factor(dat$term, levels = unique(dat$term[order(dat$group)]))
    else
      dat$term <- factor(dat$term, levels = unique(dat$term[order(dat$estimate)]))
  } else if (!ordered.terms) {
    dat$term <- factor(dat$term, levels = rev(unique(dat$term)))
  }


  # set default colors. for grouped predictors we need more color values

  if (is.null(geom.colors)) geom.colors <- dplyr::if_else(is.null(group.terms), "grey30", "Set1")


  # for brms multilevel with multiple random intercepts, we need
  # special handling

  if (is.stan(model) && stan.has.multiranef(dat)) {
    # split data, create data frame for each random intercept
    dat <- purrr::map(split(dat, f = dat$facet), ~ sjmisc::remove_var(.x, "facet"))

    # random intercept names are default titles
    ri.titles <- names(dat)

    # create plots
    purrr::map2(
      dat,
      1:length(dat),
      function(x, y) {

        # now we need a named vector, in order
        # to match labels and term order at axis

        labs <- as.character(x$term)
        names(labs) <- labs

        # sort terms

        if (!is.null(sort.est)) {
          reihe <- order(x$estimate)
        } else {
          reihe <- 1:nrow(x)
        }
        x$reihe <- order(reihe)

        x$term <- factor(x$term, levels = unique(x$term[order(x$reihe)]))

        # plot title

        if (sjmisc::is_empty(title)) {
          ptitle <- ri.titles[y]
        } else {
          if (length(title) >= y)
            ptitle <- title[y]
          else
            ptitle <- title
        }


        # plot random effects

        plot_point_estimates(
          model = model,
          dat = x,
          tf = tf,
          title = ptitle,
          axis.labels = labs,
          axis.title = NULL,
          axis.lim = axis.lim,
          grid.breaks = grid.breaks,
          show.values = show.values,
          value.offset = value.offset,
          geom.size = geom.size,
          line.size = line.size,
          geom.colors = geom.colors,
          vline.color = vline.color,
          value.size = value.size,
          facets = facets,
          bpe.style = bpe.style,
          bpe.color = bpe.color,
          ci.style = ci.style,
          ...
        )
      }
    )

  } else {

    if (obj_has_name(dat, "wrap.facet") && dplyr::n_distinct(dat$wrap.facet, na.rm = TRUE) > 1 && !facets) {

      dat <- purrr::map(split(dat, f = dat$wrap.facet), ~ sjmisc::remove_var(.x, "wrap.facet"))

      if (length(axis.title) == 1) axis.title <- c(axis.title, "Odds Ratios")

      purrr::pmap(list(dat, axis.title, names(dat)), function(.x, .y, .z) {

        plot_point_estimates(
          model = model,
          dat = .x,
          tf = tf,
          title = paste0(title, " (", .z, ")"),
          axis.labels = axis.labels,
          axis.title = .y,
          axis.lim = NULL,
          grid.breaks = NULL,
          show.values = show.values,
          value.offset = value.offset,
          geom.size = geom.size,
          line.size = line.size,
          geom.colors = geom.colors,
          bpe.style = bpe.style,
          bpe.color = bpe.color,
          vline.color = vline.color,
          value.size = value.size,
          facets = facets,
          ci.style = ci.style,
          ...
        )

      })

    } else {

      plot_point_estimates(
        model = model,
        dat = dat,
        tf = tf,
        title = title,
        axis.labels = axis.labels,
        axis.title = axis.title,
        axis.lim = axis.lim,
        grid.breaks = grid.breaks,
        show.values = show.values,
        value.offset = value.offset,
        geom.size = geom.size,
        line.size = line.size,
        geom.colors = geom.colors,
        bpe.style = bpe.style,
        bpe.color = bpe.color,
        vline.color = vline.color,
        value.size = value.size,
        facets = facets,
        ci.style = ci.style,
        ...
      )

    }
  }
}

