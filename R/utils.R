data_frame <- function(...) {
  x <- data.frame(..., stringsAsFactors = FALSE)
  rownames(x) <- NULL
  x
}

# do we have a stan-model?
is.stan <- function(x) inherits(x, c("stanreg", "stanfit", "brmsfit"))


#' @importFrom sjmisc is_empty
#' @importFrom dplyr n_distinct
stan.has.multiranef <- function(x) {
  if (obj_has_name(x, "facet")) {
    ri <- string_starts_with("(Intercept", x = x$facet)
    if (!sjmisc::is_empty(ri)) {
      return(dplyr::n_distinct(x$facet[ri]) > 1)
    }
  }
  FALSE
}

has_value_labels <- function(x) {
  !(is.null(attr(x, "labels", exact = T)) && is.null(attr(x, "value.labels", exact = T)))
}


#' @importFrom grDevices axisTicks
#' @importFrom dplyr if_else
#' @importFrom sjmisc is_empty
axis_limits_and_ticks <- function(axis.lim, min.val, max.val, grid.breaks, exponentiate, min.est, max.est) {

  # factor to multiply the axis limits. for exponentiated scales,
  # these need to be large enough to find appropriate pretty numbers

  fac.ll <- dplyr::if_else(exponentiate, .3, .95)
  fac.ul <- dplyr::if_else(exponentiate, 3.3, 1.05)


  # check for correct boundaries

  if (is.infinite(min.val) || is.na(min.val)) min.val <- min.est
  if (is.infinite(max.val) || is.na(max.val)) max.val <- max.est


  # for negative signs, need to change multiplier

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

      # make sure we have nice x-positions for breaks
      lower_lim <- round(lower_lim, 2)
      upper_lim <- round(upper_lim, 2)

      # for *very* small values, lower_lim might be zero, so
      # correct value here. else we have Inf as limit
      if (lower_lim == 0) lower_lim <- min.val * fac.ll / 10

      # use pretty distances for log-scale
      ls <- log10(c(lower_lim, upper_lim))
      ticks <- grDevices::axisTicks(c(floor(ls[1]), ceiling(ls[2])), log = TRUE)

      # truncate ticks to highest value below lower lim and
      # lowest value above upper lim

      ll <- which(ticks < lower_lim)
      if (!sjmisc::is_empty(ll) && length(ll) > 1) ticks <- ticks[ll[length(ll)]:length(ticks)]

      ul <- which(ticks > upper_lim)
      if (!sjmisc::is_empty(ul) && length(ul) > 1) ticks <- ticks[1:ul[1]]

    } else {
      ticks <- pretty(c(floor(lower_lim), ceiling(upper_lim)))
    }
  } else {
    if (length(grid.breaks) == 1)
      ticks <- seq(floor(lower_lim), ceiling(upper_lim), by = grid.breaks)
    else
      ticks <- grid.breaks
  }

  # save proper axis limits
  list(axis.lim = c(min(ticks), max(ticks)), ticks = ticks)
}


estimate_axis_title <- function(fit, axis.title, type, transform = NULL, multi.resp = NULL, include.zeroinf = FALSE) {

  # no automatic title for effect-plots
  if (type %in% c("eff", "pred", "int")) return(axis.title)

  # check default label and fit family
  if (is.null(axis.title)) {

    fitfam <- insight::model_info(fit)

    if (!is.null(multi.resp))
      fitfam <- fitfam[[multi.resp]]
    else if (insight::is_multivariate(fit))
      fitfam <- fitfam[[1]]

    axis.title <- dplyr::case_when(
      !is.null(transform) && transform == "plogis" ~ "Probabilities",
      is.null(transform) && fitfam$is_binomial ~ "Log-Odds",
      is.null(transform) && fitfam$is_ordinal ~ "Log-Odds",
      is.null(transform) && fitfam$is_multinomial ~ "Log-Odds",
      is.null(transform) && fitfam$is_categorical ~ "Log-Odds",
      is.null(transform) && fitfam$is_count ~ "Log-Mean",
      fitfam$is_count ~ "Incidence Rate Ratios",
      fitfam$is_ordinal ~ "Odds Ratios",
      fitfam$is_multinomial ~ "Odds Ratios",
      fitfam$is_categorical ~ "Odds Ratios",
      fitfam$is_binomial && !fitfam$is_logit ~ "Risk Ratios",
      fitfam$is_binomial ~ "Odds Ratios",
      TRUE ~ "Estimates"
    )

    if (fitfam$is_zero_inflated && isTRUE(include.zeroinf)) {
      if (is.null(transform))
        axis.title <- c(axis.title, "Log-Odds")
      else
        axis.title <- c(axis.title, "Odds Ratios")
    }

  }

  axis.title
}


#' @importFrom dplyr case_when
get_p_stars <- function(pval, thresholds = NULL) {

  if (is.null(thresholds)) thresholds <- c(.05, .01, .001)

  dplyr::case_when(
    is.na(pval) ~ "",
    pval < thresholds[3] ~ "***",
    pval < thresholds[2] ~ "**",
    pval < thresholds[1] ~ "*",
    TRUE ~ ""
  )
}


is_merMod <- function(fit) {
  inherits(fit, c("lmerMod", "glmerMod", "nlmerMod", "merModLmerTest"))
}


is_brms_mixed <- function(fit) {
  inherits(fit, "brmsfit") && !sjmisc::is_empty(fit$ranef)
}


# short checker so we know if we need more summary statistics like ICC
#' @importFrom insight model_info is_multivariate
is_mixed_model <- function(fit) {
  mi <- insight::model_info(fit)
  if (is.null(mi)) {
    return(FALSE)
  }
  if (insight::is_multivariate(fit))
    mi[[1]]$is_mixed
  else
    mi$is_mixed
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
    if (is.null(t$panel.grid.major)) t$panel.grid.major <- t$panel.grid
    color <- nulldef(vline.color, t$panel.grid.major$colour, "grey90")
    minor_size <- nulldef(t$panel.grid.minor$size, .125)
    major_size <- nulldef(t$panel.grid.major$size, minor_size * 1.5)
    size <- major_size * 1.5
    geom_hline(yintercept = yintercept, color = color, size = size)
  } else {
    NULL
  }
}

# same as above, but no check if intercept is within boundaries or not
geom_intercept_line2 <- function(yintercept, vline.color) {
  t <- theme_get()
  if (is.null(t$panel.grid.major)) t$panel.grid.major <- t$panel.grid
  color <- nulldef(vline.color, t$panel.grid.major$colour, "grey90")
  minor_size <- nulldef(t$panel.grid.minor$size, .125)
  major_size <- nulldef(t$panel.grid.major$size, minor_size * 1.5)
  size <- major_size * 1.5
  geom_hline(yintercept = yintercept, color = color, size = size)
}


check_se_argument <- function(se, type = NULL) {
  if (!is.null(se) && !is.null(type) && type %in% c("std", "std2")) {
    warning("No robust standard errors for `type = \"std\"` or `type = \"std2\"`.")
    se <- NULL
  }

  if (!is.null(se) && !is.null(type) && type == "re") {
    warning("No robust standard errors for `type = \"re\"`.")
    se <- NULL
  }

  se
}


list.depth <- function(this, thisdepth = 0) {
  # http://stackoverflow.com/a/13433689/1270695
  if (!is.list(this)) {
    return(thisdepth)
  } else {
    return(max(unlist(lapply(this, list.depth, thisdepth = thisdepth + 1))))
  }
}


#' @importFrom purrr map flatten_chr
#' @importFrom sjmisc is_empty trim
parse_terms <- function(x) {
  if (sjmisc::is_empty(x)) return(x)

  # get variable with suffix
  vars.pos <-
    which(as.vector(regexpr(
      pattern = " ([^\\]]*)\\]",
      text = x,
      perl = T
    )) != -1)

  # is empty?
  if (sjmisc::is_empty(vars.pos)) return(x)

  # get variable names. needed later to set as
  # names attributes
  vars.names <- clear_terms(x)[vars.pos]

  # get levels inside brackets
  tmp <- unlist(regmatches(
    x,
    gregexpr(
      pattern = " ([^\\]]*)\\]",
      text = x,
      perl = T
    )
  ))

  # remove brackets
  tmp <- gsub("(\\[*)(\\]*)", "", tmp)

  # see if we have multiple values, split at comma
  tmp <- sjmisc::trim(strsplit(tmp, ",", fixed = T))

  parsed.terms <- seq_len(length(tmp)) %>%
    purrr::map(~ sprintf("%s%s", vars.names[.x], tmp[[.x]])) %>%
    purrr::flatten_chr()

  c(x[-vars.pos], parsed.terms)
}


#' @importFrom sjmisc trim
clear_terms <- function(x) {
  # get positions of variable names and see if we have
  # a suffix for certain values
  cleaned.pos <- regexpr(pattern = "\\s", x)

  # position "-1" means we only had variable name, no suffix
  replacers <- which(cleaned.pos == -1)
  # replace -1 with number of chars
  cleaned.pos[replacers] <- nchar(x)[replacers]

  # get variable names only
  sjmisc::trim(substr(x, 0, cleaned.pos))
}


#' @importFrom purrr map_lgl
#' @importFrom sjmisc is_empty
is_empty_list <- function(x) {
  all(purrr::map_lgl(x, sjmisc::is_empty))
}


model_deviance <- function(x) {
  tryCatch(
    {
      m_deviance(x)
    },
    error = function(x) { NULL }
  )
}


#' @importFrom performance performance_aic
model_aic <- function(x) {
  performance::performance_aic(x)
}


#' @importFrom performance performance_aicc
model_aicc <- function(x) {
  tryCatch(
    {
      performance::performance_aicc(x)
    },
    error = function(x) { NULL }
  )
}


#' @importFrom stats logLik
model_loglik <- function(x) {
  tryCatch(
    {
      stats::logLik(x)
    },
    error = function(x) { NULL }
  )
}


#' @importFrom stats deviance
m_deviance <- function(x) {
  if (is_merMod(x)) {
    if (!requireNamespace("lme4", quietly = TRUE)) {
      stop("Package 'lme4' required for this function to work, please install it.")
    }
    d <- lme4::getME(x, "devcomp")$cmp["dev"]
    if (is.na(d)) d <- stats::deviance(x, REML = FALSE)
  } else {
    d <- stats::deviance(x)
  }

  d
}


#' @importFrom purrr map as_vector
tidy_label <- function(labs, sep = ".") {
  # create table, and check if any value label is duplicated
  duped.val <- names(which(table(labs) > 1))

  # find position of duplicated labels
  dupes <- duped.val %>%
    purrr::map(~which(labs == .x)) %>%
    purrr::as_vector(.type = "double")

  # prefix labels with value
  labs[dupes] <- sprintf("%s%s%s", labs[dupes], sep, dupes)

  labs
}


se_ranef <- function(object) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package 'lme4' required for this function to work, please install it.")
  }
  if (inherits(object, "MixMod")) {
    se.bygroup <- lme4::ranef(object, post_vars = TRUE)
    vars.m <- attr(se.bygroup, "post_vars")

    if (dim(vars.m[[1]])[1] == 1)
      se.bygroup <- sqrt(unlist(vars.m))
    else {
      se.bygroup <- do.call(
        rbind,
        purrr::map_df(vars.m, ~ t(as.data.frame(sqrt(diag(.x)))))
      )

      dimnames(se.bygroup)[[2]] <- dimnames(vars.m[[1]])[[1]]
      se.bygroup <- list(se.bygroup)
      names(se.bygroup) <- insight::find_random(object, flatten = TRUE)
    }
  } else {
    se.bygroup <- lme4::ranef(object, condVar = TRUE)
    n.groupings <- length(se.bygroup)

    for (m in 1:n.groupings) {

      vars.m <- attr(se.bygroup[[m]], "postVar")

      K <- dim(vars.m)[1]
      J <- dim(vars.m)[3]

      names.full <- dimnames(se.bygroup[[m]])
      se.bygroup[[m]] <- array(NA, c(J, K))

      for (j in 1:J) {
        se.bygroup[[m]][j, ] <- sqrt(diag(as.matrix(vars.m[, , j])))
      }
      dimnames(se.bygroup[[m]]) <- list(names.full[[1]], names.full[[2]])
    }
  }

  se.bygroup
}


get_observations <- function(model) {
  tryCatch(
    {
      insight::n_obs(model)
    },
    error = function(x) { NULL }
  )
}


.labelled_model_data <- function(models) {
  # to be generic, make sure argument is a list
  if (!inherits(models, "list")) models <- list(models)

  # get model terms and model frame
  mf <- try(lapply(models, function(.x) insight::get_data(.x, verbose = FALSE)[, -1, drop = FALSE]), silent = TRUE)

  # return NULL on error
  if (inherits(mf, "try-error")) {
    return(FALSE)
  }


  # get all variable labels for predictors
  lbs <- unlist(lapply(mf, function(x) {
    any(sapply(x, function(i) !is.null(attributes(i)$label)))
  }))

  any(lbs)
}
