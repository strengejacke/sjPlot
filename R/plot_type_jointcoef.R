plot_type_jointcoef <- function(fit, ri.nr) {

  ri.cnt <- length(lme4::ranef(fit))

  # how many plots? if we have random effects plots,
  # determine number of random effects and iterate
  # all effects
  loops <- 1

  # do we have a specific random intercept
  # specified? If yes, check valid index

  if (!is.null(ri.nr)) {

    # remove out of bound indices
    out.of.bounds <- which(ri.nr > ri.cnt)

    if (length(out.of.bounds) > 0) {
      ri.nr <- ri.nr[-out.of.bounds]
      if (length(ri.nr) == 0) {
        warning("All indices specified in `ri.nr` were greater than amount of random intercepts in model. Please use valid range for `ri.nr`.", call. = F)
        return(invisible(NULL))
      } else {
        message("One or more indices specified in `ri.nr` were greater than amount of random intercepts in model. These indices have been removed from `ri.nr`.")
      }
    }

    loops <- ri.nr
  } else {
    loops <- ri.nr <- seq(ri.cnt)
  }



  if (missing(facets)) facets <- TRUE
  if (missing(value.size) || is.null(value.size)) value.size <- 4

  # if user doesn't want facets, split data frame at each facet-group
  # and call plot-function for each sub-data frame. we need to remove
  # the facet variable, else the plotting function would try to plot facets

  if (!facets)
    mydf <- purrr::map(split(mydf, f = mydf$facet), ~ sjmisc::remove_var(.x, "facet"))
  else
    mydf <- list(mydf)


  pl <- purrr::map2(
    mydf,
    1:length(mydf),
    function(x, y) {

      # sort terms
      x$term <- forcats::fct_reorder(x$term, x$reihe)


      # now we need a named vector, in order
      # to match labels and term order at axis

      labs <- as.character(x$term)
      names(labs) <- labs


      # plot title

      if (sjmisc::is_empty(title)) {
        ptitle <- x[["title"]]
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
        bpe.color = bpe.color,
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

  p
}
