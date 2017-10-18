#' @importFrom lme4 ranef
#' @importFrom tibble rownames_to_column
#' @importFrom purrr map map_df
#' @importFrom arm se.ranef
#' @importFrom stats qnorm
#' @importFrom forcats fct_reorder
#' @importFrom dplyr if_else
#' @importFrom sjmisc remove_var
plot_type_ranef <- function(model,
                            dat,
                            ri.nr,
                            ci.lvl,
                            tf,
                            sort.est,
                            axis.labels,
                            axis.lim,
                            grid.breaks,
                            show.values,
                            value.offset,
                            digits,
                            facets,
                            geom.colors,
                            geom.size,
                            line.size,
                            vline.color,
                            value.size,
                            ...) {

  # get tidy output of summary ----

  rand.ef <- lme4::ranef(model)
  rand.se <- arm::se.ranef(model)


  # get some initial values

  ri.cnt <- length(rand.ef)
  ran.names <- names(rand.ef)


  # set some initial values

  loops <- 1
  p <- list()
  if (missing(value.size) || is.null(value.size)) value.size <- 4


  # do we have a specific random intercept
  # specified? If yes, check valid index

  if (!missing(ri.nr) && !is.null(ri.nr)) {

    out.of.bounds <- which(ri.nr > ri.cnt)

    # remove out of bound indices
    if (length(out.of.bounds) > 0) {
      ri.nr <- ri.nr[-out.of.bounds]
      # any valid indices left?
      if (length(ri.nr) == 0) {
        stop("All indices specified in `ri.nr` were greater than amount of random intercepts in model. Please use valid range for `ri.nr`.", call. = F)
      } else {
        message("One or more indices specified in `ri.nr` were greater than amount of random intercepts in model. These indices have been removed from `ri.nr`.")
      }
    }

    # our looping counter contains all rand. int. indices
    loops <- ri.nr
  } else {
    # else, if ri.nr was NULL, plot all random intercepts, i.e.
    # looping counter contains all index numbers
    loops <- ri.nr <- seq_len(ri.cnt)
  }


  # convert to list of data frames, keep only needed random effects

  rand.ef <- purrr::map(
    loops,
    ~ rand.ef[[.x]] %>% tibble::rownames_to_column()
  )


  # same for standard errors...

  rand.se <- purrr::map(
    loops,
    ~ rand.se[.x] %>% as.data.frame() %>% tibble::rownames_to_column()
  )


  # if we have only one random intercept, and facet.grid
  # not specified, default it to false

  if (missing(facets)) facets <- any(purrr::map_lgl(rand.ef, ~ length(.x) > 1)) || length(ri.nr) > 1


  # set default sorting. if "sort.est" is logical, set
  # default string value for sorting

  if (!is.null(sort.est)) {
    if (isTRUE(sort.est))
      sort.est <- "sort.all"
    else if (is.logical(sort.est))
      sort.est <- NULL
  }


  # compute ci, two-ways

  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- 1 - ((1 - ci.lvl) / 2)
  else
    ci <- NA


  # iterate all random effects

  for (lcnt in loops) {

    mydf.ef <- as.data.frame(rand.ef[[lcnt]])
    se.fit <- rand.se[[lcnt]]

    grp.names <- colnames(mydf.ef)
    grp.names[2] <- paste(ran.names[lcnt], grp.names[2])


    # use rownames, if axis.labels not available

    if (is.null(axis.labels))
      alabels <- mydf.ef[["rowname"]]
    else
      alabels <- axis.labels


    # select random effects for each coefficient

    mydf <- purrr::map_df(2:ncol(mydf.ef), function(i) {

      tmp <- data.frame(estimate = mydf.ef[[i]])

      if (!is.na(ci.lvl)) {
        tmp$conf.low = mydf.ef[[i]] - (stats::qnorm(ci) * se.fit[[i]])
        tmp$conf.high = mydf.ef[[i]] + (stats::qnorm(ci) * se.fit[[i]])
      } else {
        tmp$conf.low = NA
        tmp$conf.high = NA
      }

      if (!is.null(tf)) {
        funtrans <- match.fun(tf)
        tmp$estimate <- funtrans(tmp$estimate)
        tmp$conf.low <- funtrans(tmp$conf.low)
        tmp$conf.high <- funtrans(tmp$conf.high)
      }

      # set column names (variable / coefficient name)
      # as group indicator, and save axis labels and title in variable

      tmp$facet <- grp.names[i]
      tmp$term <- factor(alabels)
      tmp$title <-
        dplyr::if_else(facets, "", sprintf("Random effects of %s", grp.names[i]))


      # sort data frame, initial order
      reihe <- seq_len(nrow(tmp))


      # sorting requested?
      if (!is.null(sort.est)) {

        # should all plots be sorted? works only
        # when faceting is FALSE

        if (sort.est == "sort.all") {
          if (facets) {
            # no sorting with facet.grids, because y-axis-labels
            # (group levels / labels) have to be re-sorted for
            # each coefficient, which is not possible with facet.grids
            message("Sorting each group of random effects ('sort.all') is not possible when 'facets = TRUE'.")
          } else {
            # sort odds ratios of random effects
            # for current coefficient
            reihe <- order(mydf.ef[[i]])
          }
        } else {
          # else, just sort a specific coefficient
          # this also works with facet.grid
          reihe <- order(mydf.ef[[sort.est]])
        }

      }


      # sort axis labels
      tmp$reihe <- order(reihe)


      # create default grouping, depending on the effect:
      # split positive and negative associations with outcome
      # into different groups

      treshold <- dplyr::if_else(isTRUE(tf == "exp"), 1, 0)
      tmp$group <- dplyr::if_else(tmp$estimate > treshold, "pos", "neg")


      # no p-values for random effects,
      # but value labels

      ps <- rep("", nrow(tmp))
      if (show.values) ps <- sprintf("%.*f", digits, tmp$estimate)
      tmp$p.label <- ps

      tmp
    })


    # if user doesn't want facets, split data frame at each facet-group
    # and call plot-function for each sub-data frame. we need to remove
    # the facet variable, else the plotting function would try to plot facets

    if (!facets)
      mydf <- purrr::map(split(mydf, f = mydf$facet), ~ sjmisc::remove_var(.x, "facet"))
    else
      mydf <- list(mydf)


    pl <- purrr::map(
      mydf,
      function(x) {

        # sort terms
        x$term <- forcats::fct_reorder(x$term, x$reihe)


        # now we need a named vector, in order
        # to match labels and term order at axis

        labs <- as.character(x$term)
        names(labs) <- labs


        # plot random effects

        plot_point_estimates(
          model = model,
          dat = x,
          tf = tf,
          title = x[["title"]],
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
          ...
        )
      }
    )


    # add plot result to final return value

    if (length(loops) == 1 && length(mydf) == 1)
      p <- pl[[1]]
    else {
      for (i in seq_len(length(pl)))
        p[[length(p) + 1]] <- pl[[i]]
    }
  }

  p
}
