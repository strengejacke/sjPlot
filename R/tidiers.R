tidy_model <- function(model, ci.lvl, tf, type, bpe, se, facets, show.zeroinf, p.val, ...) {
  dat <- get_tidy_data(model, ci.lvl, tf, type, bpe, facets, show.zeroinf, p.val, ...)

  # get robust standard errors, if requestes, and replace former s.e.
  if (!is.null(se) && !is.logical(se)) {
    std.err <- sjstats::robust(model, se)
    dat[["std.error"]] <- std.err[["std.error"]]
  }

  dat
}


get_tidy_data <- function(model, ci.lvl, tf, type, bpe, facets, show.zeroinf, p.val, ...) {
  if (is.stan(model))
    tidy_stan_model(model, ci.lvl, tf, type, bpe, show.zeroinf, facets, ...)
  else if (inherits(model, "lme"))
    tidy_lme_model(model, ci.lvl)
  else if (inherits(model, "gls"))
    tidy_gls_model(model, ci.lvl)
  else if (inherits(model, "coxph"))
    tidy_cox_model(model, ci.lvl)
  else if (inherits(model, "svyglm.nb"))
    tidy_svynb_model(model, ci.lvl)
  else if (inherits(model, "glmmTMB"))
    tidy_glmmTMB_model(model, ci.lvl, show.zeroinf)
  else if (inherits(model, c("hurdle", "zeroinfl")))
    tidy_hurdle_model(model, ci.lvl)
  else if (inherits(model, "logistf"))
    tidy_logistf_model(model, ci.lvl)
  else if (inherits(model, c("clm", "clmm")))
    tidy_clm_model(model, ci.lvl)
  else if (inherits(model, "polr"))
    tidy_polr_model(model, ci.lvl)
  else if (inherits(model, "multinom"))
    tidy_multinom_model(model, ci.lvl, facets)
  else if (inherits(model, "gam"))
    tidy_gam_model(model, ci.lvl)
  else if (inherits(model, "Zelig-relogit"))
    tidy_zelig_model(model, ci.lvl)
  else
    tidy_generic(model, ci.lvl, facets, p.val)
}


#' @importFrom broom tidy
#' @importFrom tibble has_name
#' @importFrom sjstats p_value
#' @importFrom stats coef qnorm
#' @importFrom dplyr mutate
tidy_generic <- function(model, ci.lvl, facets, p.val) {

  # check for multiple reponse levels

  if (inherits(stats::coef(summary(model)), "listof")) {

    # compute ci, two-ways

    if (!is.null(ci.lvl) && !is.na(ci.lvl))
      ci <- 1 - ((1 - ci.lvl) / 2)
    else
      ci <- .975

    # get estimates, as data frame
    dat <- broom::tidy(model, conf.int = FALSE, exponentiate = FALSE)

    # add conf. int.

    dat <- dat %>%
      dplyr::mutate(
        conf.low = .data$estimate - stats::qnorm(ci) * .data$std.error,
        conf.high = .data$estimate + stats::qnorm(ci) * .data$std.error
      )

    # check whether each category should be printed in facets, or
    # in a single graph (with dodged geoms)

    if (isTRUE(facets))
      colnames(dat)[1] <- "facet"
    else
      colnames(dat)[1] <- "response.level"

  } else {

    # tidy the model

    if (inherits(model, "lmerModLmerTest")) {
      dat <- tidy_lmerModLmerTest(model, ci.lvl)
    } else {
      dat <- broom::tidy(model, conf.int = TRUE, conf.level = ci.lvl, effects = "fixed")
    }


    if (is_merMod(model) && !is.null(p.val) && p.val == "kr") {
      pv <- tryCatch(
        suppressMessages(sjstats::p_value(model, p.kr = TRUE)),
        error = function(x) { NULL }
      )

      if (is.null(pv)) {
        dat$p.value <- NA
      } else {
        dat$p.value <- pv$p.value
        dat$std.error <- attr(pv, "se.kr", exact = TRUE)
        dat$statistic <- attr(pv, "t.kr", exact = TRUE)
        dat$df <- round(attr(pv, "df.kr", exact = TRUE))
      }

      dat$conf.low <- dat$estimate - stats::qnorm(ci.lvl) * dat$std.error
      dat$conf.high <- dat$estimate + stats::qnorm(ci.lvl) * dat$std.error

    } else {

      # see if we have p-values. if not, add them
      if (!tibble::has_name(dat, "p.value"))
        dat$p.value <- tryCatch(
          sjstats::p_value(model, p.kr = FALSE)[["p.value"]],
          error = function(x) { NA }
        )
    }
  }

  dat
}


#' @importFrom tibble rownames_to_column
#' @importFrom stats qnorm
#' @importFrom dplyr select
tidy_lmerModLmerTest <- function(model, ci.lvl) {
  dat <- summary(model)$coef %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "term") %>%
    dplyr::select(1, 2, 3, 5)

  colnames(dat) <- c("term", "estimate", "std.error", "statistic")

  dat$conf.low <- dat$estimate - stats::qnorm(ci.lvl) * dat$std.error
  dat$conf.high <- dat$estimate + stats::qnorm(ci.lvl) * dat$std.error

  dat
}


#' @importFrom stats coef vcov qnorm pnorm
#' @importFrom tibble tibble
tidy_svynb_model <- function(model, ci.lvl) {
  if (!isNamespaceLoaded("survey"))
    requireNamespace("survey", quietly = TRUE)


  # compute ci, two-ways

  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- 1 - ((1 - ci.lvl) / 2)
  else
    ci <- .975


  # keep original value, not rounded

  est <- stats::coef(model)
  se <- sqrt(diag(stats::vcov(model, stderr = "robust")))


  tibble::tibble(
    term = gsub("\\beta\\.", "", names(est), fixed = FALSE),
    estimate = est,
    std.error = se,
    conf.low = est - stats::qnorm(ci) * se,
    conf.high = est + stats::qnorm(ci) * se,
    p.value = 2 * stats::pnorm(abs(est / se), lower.tail = FALSE)
  )
}


#' @importFrom broom tidy
#' @importFrom tibble has_name
#' @importFrom sjstats p_value
tidy_cox_model <- function(model, ci.lvl) {
  # tidy the model
  dat <- broom::tidy(model, conf.int = ci.lvl)

  # see if we have p-values. if not, add them
  if (!tibble::has_name(dat, "p.value"))
    dat$p.value <- sjstats::p_value(model)[["p.value"]]

  dat
}


## TODO replace with sjstats::tidy_stan() in the future?

#' @importFrom stats mad formula
#' @importFrom sjstats hdi typical_value model_family
#' @importFrom sjmisc var_rename add_columns is_empty
#' @importFrom dplyr select filter slice inner_join n_distinct
#' @importFrom tibble add_column tibble
#' @importFrom purrr map_dbl
#' @importFrom rlang .data
#' @importFrom tidyselect starts_with ends_with
tidy_stan_model <- function(model, ci.lvl, tf, type, bpe, show.zeroinf, facets, ...) {

  # set defaults

  p.inner <- .5
  p.outer <- ci.lvl

  # get model information
  modfam <- sjstats::model_family(model)

  # additional arguments for 'effects()'-function?
  add.args <- lapply(match.call(expand.dots = F)$`...`, function(x) x)

  # check whether we have "prob.inner" and "prob.outer" argument
  # and if so, use these for HDI and Bayesian point estimate

  if ("prob.inner" %in% names(add.args)) p.inner <- eval(add.args[["prob.inner"]])
  if ("prob.outer" %in% names(add.args)) p.outer <- eval(add.args[["prob.outer"]])


  # get two HDI-intervals

  if (type == "re")
    ty <- "random"
  else
    ty <- "fixed"

  d1 <- sjstats::hdi(model, prob = p.outer, trans = tf, type = ty)
  d2 <- sjstats::hdi(model, prob = p.inner, trans = tf, type = ty)


  # bind columns, so we have inner and outer hdi interval

  dat <- d2 %>%
    dplyr::select(.data$hdi.low, .data$hdi.high) %>%
    sjmisc::var_rename(hdi.low = "conf.low50", hdi.high = "conf.high50") %>%
    sjmisc::add_columns(d1) %>%
    sjmisc::var_rename(hdi.low = "conf.low", hdi.high = "conf.high")


  # for brmsfit models, we need to remove some columns here to
  # match data rows later

  mod.dat <- as.data.frame(model)

  if (inherits(model, "brmsfit")) {
    re.sd <- tidyselect::starts_with("sd_", vars = colnames(mod.dat))
    re.cor <- tidyselect::starts_with("cor_", vars = colnames(mod.dat))
    lp <- tidyselect::starts_with("lp__", vars = colnames(mod.dat))
    resp.cor <- tidyselect::starts_with("rescor__", vars = colnames(mod.dat))
    priors <- tidyselect::starts_with("prior_", vars = colnames(mod.dat))

    brmsfit.removers <- unique(c(re.sd, re.cor, lp, resp.cor, priors))

    if (!sjmisc::is_empty(brmsfit.removers))
      mod.dat <- dplyr::select(mod.dat, !! -brmsfit.removers)

    # also clean prepared data frame
    resp.cor <- tidyselect::starts_with("rescor__", vars = dat$term)

    if (!sjmisc::is_empty(resp.cor))
      dat <- dplyr::slice(dat, !! -resp.cor)
  }


  # add bayesian point estimate

  est <- purrr::map_dbl(mod.dat, ~ sjstats::typical_value(.x, fun = bpe))

  dat <- tibble::tibble(
    term = names(est),
    estimate = est,
    p.value = 0,
    std.error = purrr::map_dbl(mod.dat, stats::mad)
  ) %>%
    dplyr::inner_join(
      dat,
      by = "term"
    )

  # sort columns, for tab_model()
  dat <- dat[, c(1, 2, 4:8, 3)]

  # remove some of the information not needed for plotting

  if ("sigma" %in% dat$term) dat <- dplyr::filter(dat, .data$term != "sigma")
  if ("lp__" %in% dat$term) dat <- dplyr::filter(dat, .data$term != "lp__")
  if ("shape" %in% dat$term) dat <- dplyr::filter(dat, .data$term != "shape")


  # remove sd_c and cor_ row

  re <- tidyselect::starts_with("sd_", vars = dat$term)
  if (!sjmisc::is_empty(re)) dat <- dplyr::slice(dat, !! -re)

  re <- tidyselect::starts_with("cor_", vars = dat$term)
  if (!sjmisc::is_empty(re)) dat <- dplyr::slice(dat, !! -re)


  # check if we need to keep or remove random effects

  re <- tidyselect::starts_with("b[", vars = dat$term)
  re.s <- tidyselect::starts_with("Sigma[", vars = dat$term)
  re.i <- intersect(
    tidyselect::starts_with("r_", vars = dat$term),
    tidyselect::ends_with(".", vars = dat$term)
  )

  # and all random effect error terms
  if (!sjmisc::is_empty(re.s)) dat <- dplyr::slice(dat, !! -re.s)


  if (type == "est") {

    # remove all random effect intercepts
    if (!sjmisc::is_empty(re)) dat <- dplyr::slice(dat, !! -re)

    # remove random effects from brmsfit-models
    if (!sjmisc::is_empty(re.i)) dat <- dplyr::slice(dat, !! -re.i)

  } else if (type == "re") {

    # remove all random effect intercepts
    if (!sjmisc::is_empty(re)) dat <- dplyr::slice(dat, !! re)

    # remove random effects from brmsfit-models
    if (!sjmisc::is_empty(re.i)) dat <- dplyr::slice(dat, !! re.i)

  }


  # for plot-type random effects, make sure that the random effects
  # are plotted as facet grid, grouped by groups

  if (type == "re") {

    dat$facet <- "re"

    # find random intercepts

    ri <- grep("b\\[\\(Intercept\\) (.*)\\]", dat$term)

    if (!sjmisc::is_empty(ri)) {
      dat$facet[ri] <- "(Intercept)"
      dat$term[ri] <- gsub("b\\[\\(Intercept\\) (.*)\\]", "\\1", dat$term[ri])
    }


    # find random intercepts

    ri1 <- grep("r_(.*)\\.(.*)\\.", dat$term)
    ri2 <- which(gsub("r_(.*)\\.(.*)\\.", "\\2", dat$term) == "Intercept")

    if (!sjmisc::is_empty(ri1)) {
      ri <- intersect(ri1, ri2)
      dat$facet[ri] <- "(Intercept)"
      dat$term[ri] <- gsub("r_(.*)\\.(.*)\\.", "\\1", dat$term[ri])
    }


    # fix multiple random intercepts

    if (inherits(model, "brmsfit")) {
      pattern <- "(.*)\\.(.*)"
    } else {
      pattern <- "(.*)\\:(.*)"
    }

    interc <- which(dat$facet == "(Intercept)")

    if (!sjmisc::is_empty(interc)) {
      interc.grps <- gsub(pattern, "\\1", dat$term[interc])
      resp.lvl <- gsub(pattern, "\\2", dat$term[interc])

      if (!sjmisc::is_empty(interc.grps) && dplyr::n_distinct(interc.grps) > 1) {
        dat$facet[interc] <- sprintf("(Intercept: %s)", interc.grps)
        dat$term[interc] <- resp.lvl
      }
    }


    # find random slopes

    rs1 <- grep("b\\[(.*) (.*)\\]", dat$term)
    rs2 <- which(gsub("b\\[(.*) (.*)\\]", "\\1", dat$term) != "(Intercept)")

    if (!sjmisc::is_empty(rs1)) {
      rs <- intersect(rs1, rs2)
      rs.string <- gsub("b\\[(.*) (.*)\\]", "\\1", dat$term[rs])
      dat$facet[rs] <- rs.string
      dat$term[rs] <- gsub("b\\[(.*) (.*)\\]", "\\2", dat$term[rs])
    }


    # find random slopes

    rs1 <- grep("r_(.*)\\.(.*)\\.", dat$term)
    rs2 <- which(gsub("r_(.*)\\.(.*)\\.", "\\2", dat$term) != "Intercept")

    if (!sjmisc::is_empty(rs1)) {
      rs <- intersect(rs1, rs2)
      rs.string <- gsub("r_(.*)\\.(.*)\\.", "\\2", dat$term[rs])
      dat$facet[rs] <- rs.string
      dat$term[rs] <- gsub("r_(.*)\\.(.*)\\.", "\\1", dat$term[rs])
    }

  }


  # categorical model?

  if (inherits(model, "brmsfit") && modfam$is_categorical) {

    # terms of categorical models are prefixed with "mu"

    if (length(tidyselect::starts_with("b_mu", vars = dat$term)) == nrow(dat)) {
      dat$term <- substr(dat$term, 5, max(nchar(dat$term)))
      # create "response-level" variable
      dat <- tibble::add_column(dat, response.level = "", .before = 1)
      dat$response.level <- gsub("(.*)\\_(.*)", "\\1", dat$term)
      dat$term <- gsub("(.*)\\_(.*)", "\\2", dat$term)
    }
  }


  # multivariate-response model?

  if (inherits(model, "brmsfit") && modfam$is_multivariate) {

    # get response variables

    responses <- stats::formula(model)$responses

    # also clean prepared data frame
    resp.sigma1 <- tidyselect::starts_with("sigma_", vars = dat$term)
    resp.sigma2 <- tidyselect::starts_with("b_sigma_", vars = dat$term)

    resp.sigma <- c(resp.sigma1, resp.sigma2)

    if (!sjmisc::is_empty(resp.sigma))
      dat <- dplyr::slice(dat, !! -resp.sigma)


    # create "response-level" variable

    dat <- tibble::add_column(dat, response.level = "", .before = 1)

    # copy name of response into new character variable
    # and remove response name from term name

    for (i in responses) {
      m <- tidyselect::contains(i, vars = dat$term)
      dat$response.level[intersect(which(dat$response.level == ""), m)] <- i
      dat$term <- gsub(sprintf("b_%s_", i), "", dat$term, fixed = TRUE)
      dat$term <- gsub(sprintf("s_%s_", i), "", dat$term, fixed = TRUE)
    }


    # check whether each category should be printed in facets, or
    # in a single graph (with dodged geoms)

    if (isTRUE(facets))
      colnames(dat)[1] <- "facet"
    else
      colnames(dat)[1] <- "response.level"
  }


  # do we have a zero-inflation model?

  if (modfam$is_zeroinf || sjmisc::str_contains(dat$term, "b_zi_", ignore.case = T)) {
    dat$wrap.facet <- "Conditional Model"

    # zero-inflated part
    zi <- tidyselect::starts_with("b_zi_", vars = dat$term)

    # check if zero-inflated part should be shown or removed
    if (show.zeroinf) {
      dat$wrap.facet[zi] <- "Zero-Inflated Model"
      dat$term[zi] <- sub(pattern = "b_zi_", replacement = "b_", x = dat$term[zi], fixed = T)
    } else {
      if (!sjmisc::is_empty(zi)) dat <- dplyr::slice(dat, !! -zi)
    }
  }


  # need to transform point estimate as well
  if (!is.null(tf)) {
    funtrans <- match.fun(tf)
    dat$estimate <- funtrans(dat$estimate)
  }


  # remove facet column if not necessary
  if (!show.zeroinf && tibble::has_name(dat, "wrap.facet"))
    dat <- dplyr::select(dat, -.data$wrap.facet)

  dat
}


#' @importFrom broom tidy
#' @importFrom tibble has_name
#' @importFrom sjstats p_value
#' @importFrom nlme intervals
tidy_lme_model <- function(model, ci.lvl) {
  # get tidy summary. for lme, this excludes CI,
  # so we compute them separately

  dat <- broom::tidy(model, conf.int = TRUE, conf.level = ci.lvl, effects = "fixed")
  ci <- as.data.frame(nlme::intervals(model, level = ci.lvl, which = "fixed")$fixed)

  dat$conf.low <- ci$lower
  dat$conf.high <- ci$upper

  # see if we have p-values. if not, add them
  if (!tibble::has_name(dat, "p.value"))
    dat$p.value <- sjstats::p_value(model)[["p.value"]]

  dat
}


#' @importFrom sjmisc var_rename
#' @importFrom nlme intervals
#' @importFrom dplyr select
#' @importFrom tibble rownames_to_column
tidy_gls_model <- function(model, ci.lvl) {
  # get tidy summary. for lme, this excludes CI,
  # so we compute them separately

  dat <- as.data.frame(summary(model)$tTable)
  ci <- as.data.frame(nlme::intervals(model, level = ci.lvl, which = "coef")$coef)

  dat$conf.low <- ci$lower
  dat$conf.high <- ci$upper


  sjmisc::var_rename(
    dat,
    Value = "estimate",
    Std.Error = "std.error",
    `t-value` = "statistic",
    `p-value` = "p.value"
  ) %>%
    tibble::rownames_to_column("term")
}


#' @importFrom glmmTMB fixef
#' @importFrom stats vcov qnorm pnorm
#' @importFrom tibble tibble
#' @importFrom sjmisc is_empty
#' @importFrom dplyr bind_rows
#' @importFrom rlang .data
tidy_glmmTMB_model <- function(model, ci.lvl, show.zeroinf) {

  # compute ci, two-ways

  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- 1 - ((1 - ci.lvl) / 2)
  else
    ci <- .975


  # get fixed effects

  est <- glmmTMB::fixef(model)
  vcovs <- stats::vcov(model)


  # save conditional model

  cond <- tibble::tibble(
    term = names(est[[1]]),
    estimate = est[[1]],
    std.error = sqrt(diag(vcovs[[1]])),
    statistic = .data$estimate / .data$std.error,
    conf.low = .data$estimate - stats::qnorm(ci) * .data$std.error,
    conf.high = .data$estimate + stats::qnorm(ci) * .data$std.error,
    p.value = 2 * stats::pnorm(abs(.data$estimate / .data$std.error), lower.tail = FALSE),
    wrap.facet = "Conditional Model"
  )

  # save zi model

  if (!sjmisc::is_empty(est[[2]]) && show.zeroinf) {
    zi <- tibble::tibble(
      term = names(est[[2]]),
      estimate = est[[2]],
      std.error = sqrt(diag(vcovs[[2]])),
      statistic = .data$estimate / .data$std.error,
      conf.low = .data$estimate - stats::qnorm(ci) * .data$std.error,
      conf.high = .data$estimate + stats::qnorm(ci) * .data$std.error,
      p.value = 2 * stats::pnorm(abs(.data$estimate / .data$std.error), lower.tail = FALSE),
      wrap.facet = "Zero-Inflated Model"
    )

    cond <- dplyr::bind_rows(cond, zi)
  }


  # remove facet column if not necessary
  if (!show.zeroinf) cond <- dplyr::select(cond, -.data$wrap.facet)

  cond
}


#' @importFrom stats qnorm
#' @importFrom tibble rownames_to_column
#' @importFrom sjmisc var_rename
#' @importFrom dplyr mutate
#' @importFrom purrr map2_df
#' @importFrom rlang .data
tidy_hurdle_model <- function(model, ci.lvl) {

  # compute ci, two-ways

  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- 1 - ((1 - ci.lvl) / 2)
  else
    ci <- .975


  # get estimates

  est <- summary(model)$coefficients
  mn <- c("Count Model", ifelse(inherits(model, "hurdle"), "Zero Hurdle Model", "Zero Inflation Model"))

  purrr::map2_df(est, mn, function(x, y) {
    x %>%
      as.data.frame() %>%
      tibble::rownames_to_column(var = "term") %>%
      sjmisc::var_rename(
        Estimate = "estimate",
        `Std. Error` = "std.error",
        `z value` = "statistic",
        `Pr(>|z|)` = "p.value"
      ) %>%
      dplyr::mutate(
        conf.low = .data$estimate - stats::qnorm(ci) * .data$std.error,
        conf.high = .data$estimate + stats::qnorm(ci) * .data$std.error,
        wrap.facet = y
      )
  })
}


#' @importFrom stats qnorm
#' @importFrom tibble tibble
#' @importFrom rlang .data
tidy_logistf_model <- function(model, ci.lvl) {

  # compute ci, two-ways

  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- 1 - ((1 - ci.lvl) / 2)
  else
    ci <- .975


  # get estimates

  est <- model$coefficients
  se <- sqrt(diag(model$var))


  tibble::tibble(
    term = model$terms,
    estimate = est,
    std.error = se,
    statistic = .data$estimate / .data$std.error,
    conf.low = .data$estimate - stats::qnorm(ci) * .data$std.error,
    conf.high = .data$estimate + stats::qnorm(ci) * .data$std.error,
    p.value = model$prob
  )
}


#' @importFrom stats qnorm
#' @importFrom tibble rownames_to_column
#' @importFrom rlang .data
#' @importFrom dplyr mutate
tidy_clm_model <- function(model, ci.lvl) {

  # compute ci, two-ways

  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- 1 - ((1 - ci.lvl) / 2)
  else
    ci <- .975


  # get estimates, as data frame

  smry <- summary(model)
  est <- smry$coefficients %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "term")

  # proper column names
  colnames(est) <- c("term", "estimate", "std.error", "statistic", "p.value")

  # mark intercepts
  intercepts <- stats::na.omit(match(dimnames(model$Theta)[[2]], est$term))
  est$term[intercepts] <- sprintf("(Intercept: %s)", dimnames(model$Theta)[[2]])

  # add conf. int.

  est <- est %>%
    dplyr::mutate(
      conf.low = .data$estimate - stats::qnorm(ci) * .data$std.error,
      conf.high = .data$estimate + stats::qnorm(ci) * .data$std.error
    )

  # re-arrange columns
  est[, c(1:4, 6:7, 5)]
}


#' @importFrom stats qnorm pnorm
#' @importFrom tibble rownames_to_column
#' @importFrom rlang .data
#' @importFrom dplyr mutate
tidy_polr_model <- function(model, ci.lvl) {

  # compute ci, two-ways

  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- 1 - ((1 - ci.lvl) / 2)
  else
    ci <- .975


  # get estimates, as data frame

  smry <- summary(model)
  est <- smry$coefficients %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "term")

  # proper column names
  colnames(est) <- c("term", "estimate", "std.error", "statistic")

  # mark intercepts
  intercepts <- stats::na.omit(match(names(model$zeta), est$term))
  est$term[intercepts] <- sprintf("(Intercept: %s)", names(model$zeta))

  # add conf. int. and p.value

  est %>%
    dplyr::mutate(
      conf.low = .data$estimate - stats::qnorm(ci) * .data$std.error,
      conf.high = .data$estimate + stats::qnorm(ci) * .data$std.error,
      p.value = 2 * stats::pnorm(abs(.data$estimate / .data$std.error), lower.tail = FALSE)
    )
}


#' @importFrom stats qnorm pnorm
#' @importFrom rlang .data
#' @importFrom dplyr mutate
#' @importFrom broom tidy
#' @importFrom sjmisc var_rename
tidy_multinom_model <- function(model, ci.lvl, facets) {

  # compute ci, two-ways

  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- 1 - ((1 - ci.lvl) / 2)
  else
    ci <- .975


  # get estimates, as data frame
  dat <- broom::tidy(model, conf.int = FALSE, exponentiate = FALSE)


  # add conf. int.

  dat <- dat %>%
    dplyr::mutate(
      conf.low = .data$estimate - stats::qnorm(ci) * .data$std.error,
      conf.high = .data$estimate + stats::qnorm(ci) * .data$std.error
    )


  # check whether each category should be printed in facets, or
  # in a single graph (with dodged geoms)

  if (isTRUE(facets))
    colnames(dat)[1] <- "facet"
  else
    colnames(dat)[1] <- "response.level"

  dat
}


#' @importFrom stats coef qnorm pnorm
#' @importFrom tibble tibble
#' @importFrom rlang .data
tidy_gam_model <- function(model, ci.lvl) {

  # compute ci, two-ways

  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- 1 - ((1 - ci.lvl) / 2)
  else
    ci <- .975


  # get estimates

  est <- stats::coef(model)
  se <- sqrt(diag(model$Ve))
  sm <- summary(model)

  tibble::tibble(
    term = names(est),
    estimate = est,
    std.error = se,
    statistic = sm$p.t,
    conf.low = .data$estimate - stats::qnorm(ci) * .data$std.error,
    conf.high = .data$estimate + stats::qnorm(ci) * .data$std.error,
    p.value = sm$p.pv
  )
}


#' @importFrom rlang .data
#' @importFrom stats coef qnorm
tidy_zelig_model <- function(model, ci.lvl) {

  # compute ci, two-ways

  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    ci <- 1 - ((1 - ci.lvl) / 2)
  else
    ci <- .975


  if (!requireNamespace("Zelig"))
    stop("Package `Zelig` required. Please install", call. = F)

  # get estimates

  est <- Zelig::coef(model)
  se <- unlist(Zelig::get_se(model))

  tibble::tibble(
    term = names(est),
    estimate = est,
    std.error = se,
    statistic = .data$estimate / .data$std.error,
    conf.low = .data$estimate - stats::qnorm(ci) * .data$std.error,
    conf.high = .data$estimate + stats::qnorm(ci) * .data$std.error,
    p.value = unname(unlist(Zelig::get_pvalue(model)))
  )
}
