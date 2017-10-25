#' @importFrom sjstats model_frame
#' @importFrom stats formula sd quantile
#' @importFrom purrr map map_lgl map_chr
#' @importFrom sjmisc trim is_empty str_contains
#' @importFrom dplyr select n_distinct
#' @importFrom ggeffects ggpredict
#' @importFrom graphics plot
plot_type_int <- function(model,
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
                          show.legend,
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
  mf <- sjstats::model_frame(model)

  pl <- list()

  # intertate interaction terms

  for (i in 1:length(ia.terms)) {

    ia <- ia.terms[[i]]
    find.fac <- purrr::map_lgl(ia, ~ is_categorical(mf[[.x]]))


    # find all non-categorical variables, except first
    # term, which is considered as being along the x-axis

    check_cont <- ia[-1][!find.fac[2:length(find.fac)]]


    # if we have just categorical as interaction terms,
    # we plot all category values

    if (!sjmisc::is_empty(check_cont)) {

      # get data from continuous interaction terms. we
      # need this to compute the specific values that
      # should be used as group characteristic for the plot

      cont_terms <- dplyr::select(mf, !! check_cont)


      # for quartiles used as moderator values, make sure
      # that the variable's range is large enough to compute
      # quartiles

      mdrt.val <- mv_check(mdrt.values = mdrt.values, cont_terms)

      # prepare terms for ggpredict()-call. terms is a character-vector
      # with term name and values to plot in square brackets

      terms <- purrr::map_chr(check_cont, function(x) {
        if (mdrt.val == "minmax") {
          sprintf("%s [%i,%i]",
                  x,
                  min(cont_terms[[x]], na.rm = TRUE),
                  max(cont_terms[[x]], na.rm = TRUE))
        } else if (mdrt.val == "meansd") {
          mw <- mean(cont_terms[[x]], na.rm = TRUE)
          sabw <- stats::sd(cont_terms[[x]], na.rm = TRUE)
          sprintf("%s [%.2f,%.2f,%.2f]", x, mw, mw - sabw, mw + sabw)
        } else if (mdrt.val == "zeromax") {
          sprintf("%s [0,%i]", x, max(cont_terms[[x]], na.rm = TRUE))
        } else if (mdrt.val == "quart") {
          qu <- as.vector(stats::quantile(cont_terms[[x]], na.rm = T))
          sprintf("%s [%.2f,%.2f,%.2f]", x, qu[3], qu[2], qu[4])
        } else {
          x
        }
      })

      ia[match(check_cont, ia)] <- terms
    }


    # compute marginal effects for interaction terms

    dat <- ggeffects::ggpredict(
      model = model,
      terms = ia,
      ci.lvl = ci.lvl,
      type = pred.type,
      full.data = FALSE,
      ...
    )


    # select color palette

    geom.colors <- col_check2(geom.colors, dplyr::n_distinct(dat$group))


    # save plot of marginal effects for interaction terms

    p <- graphics::plot(
      dat,
      ci = !is.na(ci.lvl),
      facets = facets,
      rawdata = show.data,
      colors = geom.colors,
      use.theme = FALSE,
      case = case,
      show.legend = show.legend,
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
        p <- p + xlim(axis.lim[[1]]) + ylim(axis.lim[[2]])
      else
        p <- p + ylim(axis.lim)
    }


    # add plot result to final return value

    if (length(ia.terms) == 1)
      pl <- p
    else
      pl[[length(pl) + 1]] <- p
  }

  pl
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
