#' @importFrom sjstats get_model_frame
plot_type_int <- function(type,
                          model,
                          mdrt.values,
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

  # find right hand side of formula, to extract interaction terms
  rhs <- unlist(strsplit(as.character(stats::formula(model))[3], "+", fixed = TRUE))

  # interaction terms are separated with ":"
  int.terms <- purrr::map_lgl(rhs, ~ sjmisc::str_contains(.x, "*"))


  # stop if no interaction found

  if (!any(int.terms))
    stop("No interaction term found in model.", call. = F)


  # get interaction terms and model frame

  ia.terms <- purrr::map(rhs[int.terms], ~ sjmisc::trim(unlist(strsplit(.x, "*", fixed = TRUE))))
  mf <- sjstats::get_model_frame(model)


  # intertate interaction terms

  for (i in 1:length(ia.terms)) {

    ia <- ia.terms[[i]]
    find.fac <- purrr::map_lgl(ia, ~ is_categorical(mf[[.x]]))


    # find all non-categorical variables, except first
    # term, which is considered as being along the x-axis

    check_cont <- ia[-1][!find.fac[2:length(find.fac)]]


    # for quartiles used as moderator values, make sure
    # that the variable's range is large enough to compute
    # quartiles

    mdrt.val <- mv_check(mdrt.values = mdrt.values, dplyr::select(mf, !! check_cont))




    dat <- ggeffects::ggpredict(
      model = model,
      terms = terms,
      ci.lvl = ci.lvl,
      type = pred.type,
      full.data = FALSE,
      ...
    )

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
    if (!is.null(axis.lim)) {
      if (is.list(axis.lim))
        p <- p + xlim(axis.lim[[1]]) + + ylim(axis.lim[[2]])
      else
        p <- p + ylim(axis.lim)
    }


    p

  }




  p
}


is_categorical <- function(x) {
  is.factor(x) || (length(unique(na.omit(x))) < 3)
}


#' @importFrom stats quantile
#' @importFrom purrr map_dbl
mv_check <- function(mdrt.values, x) {

  # for quartiles used as moderator values, make sure
  # that the variable's range is large enough to compute
  # quartiles

  if (mdrt.values == "quart") {

    if (!is.data.frame(x)) x <- as.data.frame(x)

    mvc <- purrr::map_dbl(x, ~ length(unique(as.vector(stats::quantile(.x, na.rm = T)))))

    if (any(mvc < 3)) {
      # tell user that quart won't work
      message("Could not compute quartiles, too small range of moderator variable. Defaulting `mdrt.values` to `minmax`.")
      mdrt.values <- "minmax"
    }

  }

  mdrt.values
}
