#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`


# do we have a stan-model?
is.stan <- function(x) inherits(x, c("stanreg", "stanfit", "brmsfit"))


has_value_labels <- function(x) {
  !(is.null(attr(x, "labels", exact = T)) && is.null(attr(x, "value.labels", exact = T)))
}


#' @importFrom grDevices axisTicks
#' @importFrom dplyr if_else
#' @importFrom sjmisc is_empty
get_axis_limits_and_ticks <- function(axis.lim, min.val, max.val, grid.breaks, exponentiate, min.est, max.est) {

  # factor to multiply the axis limits. for exponentiated scales,
  # these need to be large enough to find appropriate pretty numbers

  fac.ll <- dplyr::if_else(exponentiate, .3, .95)
  fac.ul <- dplyr::if_else(exponentiate, 3.3, 1.05)


  # check for correct boundaries

  if (is.infinite(min.val) || is.na(min.val)) min.val <- min.est
  if (is.infinite(max.val) || is.na(max.val)) max.val <- max.est


  # for negative signes, need to change multiplier

  if (min.val < 0) fac.ll <- 1 / fac.ll
  if (max.val < 0) fac.ul <- 1 / fac.ul


  # axis limits

  if (is.null(axis.lim)) {
    lower_lim <- min.val * fac.ll
    upper_lim <- max.val * fac.ul
  } else {
    lower_lim <- axis.lim[1]
    upper_lim <- axis.lim[2]
  }


  # determine gridbreaks

  if (is.null(grid.breaks)) {
    if (exponentiate) {

      # use pretty distances for log-scale
      ticks <- grDevices::axisTicks(log10(c(lower_lim, upper_lim)), log = TRUE)

      # truncate ticks to highest value below lower lim and
      # lowest value above upper lim

      ll <- which(ticks < lower_lim)
      if (!sjmisc::is_empty(ll) && length(ll) > 1) ticks <- ticks[ll[length(ll)]:length(ticks)]

      ul <- which(ticks > upper_lim)
      if (!sjmisc::is_empty(ul) && length(ul) > 1) ticks <- ticks[1:ul[1]]

      } else {
      ticks <- pretty(c(lower_lim, upper_lim))
    }
  } else {
    ticks <- seq(lower_lim, upper_lim, by = grid.breaks)
  }

  # save proper axis limits
  list(axis.lim = c(min(ticks), max(ticks)), ticks = ticks)
}


get_estimate_axis_title <- function(fit, axis.title, type, transform = NULL) {

  # no automatic title for effect-plots
  if (type %in% c("eff", "pred", "int")) return(axis.title)

  # check default label and fit family
  if (is.null(axis.title)) {
    fitfam <- get_glm_family(fit)

    axis.title <-  dplyr::case_when(
      !is.null(transform) && transform == "plogis" ~ "Probabilities",
      is.null(transform) && fitfam$is_bin ~ "Log-Odds",
      fitfam$is_pois ~ "Incidence Rate Ratios",
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


is_merMod <- function(fit) {
  inherits(fit, c("lmerMod", "glmerMod", "nlmerMod", "merModLmerTest"))
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
    if (inherits(fit, c("lrm", "polr", "logistf", "clm", "multinom", "Zelig-relogit")))
      faminfo <- stats::binomial(link = "logit")
    else
      # get family info
      faminfo <- stats::family(fit)

    fitfam <- faminfo$family
    logit_link <- faminfo$link == "logit"
    link.fun <- faminfo$link
  }

  # create logical for family
  binom_fam <-
    fitfam %in% c("binomial", "quasibinomial", "binomialff") |
    sjmisc::str_contains(fitfam, "binomial", ignore.case = TRUE)

  poisson_fam <-
    fitfam %in% c("poisson", "quasipoisson") |
    sjmisc::str_contains(fitfam, "poisson", ignore.case = TRUE)

  neg_bin_fam <-
    sjmisc::str_contains(fitfam, "negative binomial", ignore.case = T) |
    sjmisc::str_contains(fitfam, "nbinom", ignore.case = TRUE) |
    sjmisc::str_contains(fitfam, "neg_binomial", ignore.case = TRUE)

  linear_model <- !binom_fam & !poisson_fam & !neg_bin_fam & !logit_link


  list(
    is_bin = binom_fam & !neg_bin_fam,
    is_pois = poisson_fam | neg_bin_fam,
    is_negbin = neg_bin_fam,
    is_logit = logit_link,
    is_linear = linear_model,
    link.fun = link.fun,
    family = fitfam
  )
}


nulldef <- function(x, y, z = NULL) {
  if (is.null(x)) {
    if (is.null(y))
      z
    else
      y
  } else
    x
}


geom_intercept_line <- function(yintercept, axis.scaling, vline.color) {
  if (yintercept > axis.scaling$axis.lim[1] && yintercept < axis.scaling$axis.lim[2]) {
    t <- theme_get()
    color <- nulldef(vline.color, t$panel.grid.major$colour, "grey90")
    minor_size <- nulldef(t$panel.grid.minor$size, .125)
    major_size <- nulldef(t$panel.grid.major$size, minor_size * 2)
    size <- major_size * 2
    geom_hline(yintercept = yintercept, color = color, size = size)
  } else {
    NULL
  }
}

# same as above, but no check if intercept is within boundaries or not
geom_intercept_line2 <- function(yintercept, vline.color) {
  t <- theme_get()
  color <- nulldef(vline.color, t$panel.grid.major$colour, "grey90")
  minor_size <- nulldef(t$panel.grid.minor$size, .125)
  major_size <- nulldef(t$panel.grid.major$size, minor_size * 2)
  size <- major_size * 2
  geom_hline(yintercept = yintercept, color = color, size = size)
}


check_se_argument <- function(se, type = NULL) {
  if (!is.null(se) && !is.logical(se) && !is.null(type) && type %in% c("std", "std2")) {
    warning("No robust standard errors for `type = \"std\"` or `type = \"std2\"`.")
    se <- TRUE
  }

  if (!is.null(se) && !is.logical(se)) {
    # check for valid values, if robust standard errors are requested
    if (!(se %in% c("HC3", "const", "HC", "HC0", "HC1", "HC2", "HC4", "HC4m", "HC5"))) {
      warning("`se` must be one of \"HC3\", \"const\", \"HC\", \"HC0\", \"HC1\", \"HC2\", \"HC4\", \"HC4m\" or \"HC5\" for robust standard errors, or `TRUE` for normal standard errors.")
      se <- NULL
    }

    # no robust s.e. for random effetcs
    if (type == "re") {
      warning("No robust standard errors for `type = \"re\"`.")
      se <- TRUE
    }
  }

  se
}
