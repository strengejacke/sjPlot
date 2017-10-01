#' @importFrom lme4 ranef
#' @importFrom tibble rownames_to_column
#' @importFrom purrr map
#' @importFrom arm se.ranef
plot_type_ranef <- function(type,
                            ri.nr,
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
                            facets,
                            geom.colors,
                            geom.size,
                            line.size,
                            vline.type,
                            vline.color) {

  # check some defaults
  if (missing(facets)) facets <- TRUE

  # get tidy output of summary ----

  rand.ef <- lme4::ranef(model)
  rand.se <- arm::se.ranef(model)

  ri.cnt <- length(ranef)


  # how many plots? if we have random effects plots,
  # determine number of random effects and iterate
  # all effects

  loops <- 1


  # do we have a specific random intercept
  # specified? If yes, check valid index

  if (!missing(ri.nr) && is.null(ri.nr)) {

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
    loops <- ri.nr <- seq(ri.cnt)
  }


  # convert to list of data frames, keep only needed random effects

  rand.ef <- purrr::map(
    loops,
    ~ rand.ef[.x] %>% as.data.frame() %>% tibble::rownames_to_column()
  )


  # same for standard errors...

  rand.se <- purrr::map(
    loops,
    ~ rand.se[.x] %>% as.data.frame() %>% tibble::rownames_to_column()
  )

  # tidyr::gather(tmp, key = "group", value = "estimate", 2:ncol(tmp))

}
