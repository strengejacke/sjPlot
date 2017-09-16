#' @importFrom grDevices axisTicks
get_axis_limits_and_ticks <- function(axis.lim, min.val, max.val, grid.breaks, exponentiate) {
  # axis limits
  if (is.null(axis.lim)) {
    lower_lim <- min.val * .95
    upper_lim <- max.val * 1.05
  } else {
    lower_lim <- axis.lim[1]
    upper_lim <- axis.lim[2]
  }

  # determine gridbreaks
  if (is.null(grid.breaks)) {
    if (exponentiate) {
      # use pretty distances for log-scale
      ticks <- grDevices::axisTicks(log(c(lower_lim, upper_lim)), log = TRUE)
      # make sure that scale is not too wide
      ticks <- ticks[1:which(ticks > max.val)[1]]
    } else {
      ticks <- pretty(c(lower_lim, upper_lim))
    }
  } else {
    ticks <- seq(lower_lim, upper_lim, by = grid.breaks)
  }

  # save proper axis limits
  list(axis.lim = c(min(ticks), max(ticks)), ticks = ticks)
}


get_estimate_axis_title <- function(fit, axis.title) {
  # check default label and fit family
  if (is.null(axis.title)) {
    fitfam <- get_glm_family(fit)

    axis.title <-  dplyr::case_when(
      fitfam$is_pois ~ "Incident Rate Ratios",
      fitfam$is_bin && !fitfam$is_logit ~ "Risk Ratios",
      fitfam$is_bin ~ "Odds Ratios",
      TRUE ~ "Estimates"
    )
  }

  axis.title
}


#' @importFrom dplyr case_when
get_p_stars <- function(pval) {
  dplyr::case_when(
    is.na(pval) ~ "",
    pval < 0.001 ~ "***",
    pval < 0.01 ~ "**",
    pval < 0.05 ~ "*",
    TRUE ~ ""
  )
}


#' @importFrom scales brewer_pal grey_pal
col_check2 <- function(geom.colors, collen) {
  # --------------------------------------------
  # check color argument
  # --------------------------------------------
  # check for corrct color argument
  if (!is.null(geom.colors)) {
    # check for color brewer palette
    if (is.brewer.pal(geom.colors[1])) {
      geom.colors <- scales::brewer_pal(palette = geom.colors[1])(collen)
    } else if (geom.colors[1] == "gs") {
      geom.colors <- scales::grey_pal()(collen)
      # do we have correct amount of colours?
    } else if (geom.colors[1] == "bw") {
      geom.colors <- rep("black", times = collen)
      # do we have correct amount of colours?
    } else if (length(geom.colors) > collen) {
      # shorten palette
      geom.colors <- geom.colors[1:collen]
    } else if (length(geom.colors) < collen) {
      # repeat color palette
      geom.colors <- rep(geom.colors, times = collen)
      # shorten to required length
      geom.colors <- geom.colors[1:collen]
    }
  } else {
    geom.colors <- scales::brewer_pal(palette = "Set1")(collen)
  }

  geom.colors
}


#' @importFrom sjmisc str_contains
#' @importFrom stats family
get_glm_family <- function(fit) {
  # do we have glm? if so, get link family. make exceptions
  # for specific models that don't have family function
  if (inherits(fit, c("lme", "plm", "gls", "truncreg"))) {
    fitfam <- "gaussian"
    logit_link <- FALSE
    link.fun <- "identity"
  } else if (inherits(fit, c("vgam", "vglm"))) {
    faminfo <- fit@family
    fitfam <- faminfo@vfamily
    logit_link <- sjmisc::str_contains(faminfo@blurb, "logit")
    link.fun <- faminfo@blurb[3]
  } else if (inherits(fit, c("zeroinfl", "hurdle"))) {
    fitfam <- "negative binomial"
    logit_link <- FALSE
    link.fun <- NULL
  } else if (inherits(fit, "betareg")) {
    fitfam <- "beta"
    logit_link <- fit$link$mean$name == "logit"
    link.fun <- fit$link$mean$linkfun
  } else if (inherits(fit, "coxph")) {
    fitfam <- "survival"
    logit_link <- TRUE
    link.fun <- NULL
  } else {
    # "lrm"-object from pkg "rms" have no family method
    # so we construct a logistic-regression-family-object
    if (inherits(fit, c("lrm", "polr")))
      faminfo <- stats::binomial(link = "logit")
    else
      # get family info
      faminfo <- stats::family(fit)

    fitfam <- faminfo$family
    logit_link <- faminfo$link == "logit"
    link.fun <- faminfo$link
  }

  # create logical for family
  binom_fam <- fitfam %in% c("binomial", "quasibinomial", "binomialff")
  poisson_fam <- fitfam %in% c("poisson", "quasipoisson")
  neg_bin_fam <- sjmisc::str_contains(fitfam, "negative binomial", ignore.case = T)
  linear_model <- !binom_fam & !poisson_fam & !neg_bin_fam & !logit_link

  return(
    list(
      is_bin = binom_fam,
      is_pois = poisson_fam | neg_bin_fam,
      is_negbin = neg_bin_fam,
      is_logit = logit_link,
      is_linear = linear_model,
      link.fun = link.fun,
      family = fitfam
    )
  )
}
