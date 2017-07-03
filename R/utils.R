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
  # check if we have a linear model
  is.linear.model <- any(inherits(fit, c("lm", "lmerMod", "lme"), which = TRUE) == 1)

  if (!is.linear.model) {
    # get information of glm
    fitfam <- get_glm_family(fit)

    # create logical for family
    poisson_fam <- fitfam$is_pois
    binom_fam <- fitfam$is_bin
    logit_link <- fitfam$is_logit
  }

  # check default label and fit family
  if (is.null(axis.title)) {
    if (is.linear.model)
      axis.title <- "Estimates"
    else if (poisson_fam)
      axis.title <- "Incident Rate Ratios"
    else if (binom_fam && !logit_link)
      axis.title <- "Risk Ratios"
    else
      axis.title <- "Odds Ratios"
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
      # warn user abount wrong color palette
      warning(
        sprintf(
          "Insufficient length of color palette provided. %i color values needed.",
          collen
        ),
        call. = F
      )
      # set default palette
      geom.colors <- scales::brewer_pal(palette = "Set1")(collen)
    }
  } else {
    geom.colors <- scales::brewer_pal(palette = "Set1")(collen)
  }

  geom.colors
}
