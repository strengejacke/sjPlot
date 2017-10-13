## TODO provide own tidier for not-supported models

tidy_model <- function(model, ci.lvl, tf, type, bpe, ...) {
  if (is.stan(model))
    tidy_stan_model(model, ci.lvl, tf, type, bpe, ...)
  else if (inherits(model, "lme"))
    tidy_lme_model(model, ci.lvl)
  else if (inherits(model, "gls"))
    tidy_gls_model(model, ci.lvl)
  else if (inherits(model, "coxph"))
    tidy_cox_model(model, ci.lvl)
  else if (inherits(model, "svyglm.nb"))
    tidy_svynb_model(model, ci.lvl)
  else if (inherits(model, "glmmTMB"))
    tidy_glmmTMB_model(model, ci.lvl)
  else if (inherits(model, c("hurdle", "zeroinfl")))
    tidy_hurdle_model(model, ci.lvl)
  else if (inherits(model, "logistf"))
    tidy_logistf_model(model, ci.lvl)
  else if (inherits(model, "gam"))
    tidy_gam_model(model, ci.lvl)
  else
    tidy_generic(model, ci.lvl)
}



#' @importFrom broom tidy
#' @importFrom tibble has_name
#' @importFrom sjstats p_value
tidy_generic <- function(model, ci.lvl) {
  # tidy the model
  dat <- broom::tidy(model, conf.int = TRUE, conf.level = ci.lvl, effects = "fixed")

  # see if we have p-values. if not, add them
  if (!tibble::has_name(dat, "p.value"))
    dat$p.value <- sjstats::p_value(model)[["p.value"]]

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


#' @importFrom sjstats hdi typical_value
#' @importFrom sjmisc var_rename add_columns is_empty
#' @importFrom dplyr select filter slice
#' @importFrom tibble add_column
#' @importFrom purrr map_dbl
#' @importFrom rlang .data
#' @importFrom tidyselect starts_with ends_with
tidy_stan_model <- function(model, ci.lvl, tf, type, bpe, ...) {

  # set defaults

  p.inner <- .5
  p.outer <- ci.lvl


  # additional arguments for 'effects()'-function?
  add.args <- lapply(match.call(expand.dots = F)$`...`, function(x) x)

  # check whether we have "prob.inner" and "prob.outer" argument
  # and if so, use these for HDI and Bayesian point estimate

  if ("prob.inner" %in% names(add.args)) p.inner <- add.args[["prob.inner"]]
  if ("prob.outer" %in% names(add.args)) p.outer <- add.args[["prob.outer"]]


  # get two HDI-intervals

  d1 <- sjstats::hdi(model, prob = p.outer, trans = tf, type = "all")
  d2 <- sjstats::hdi(model, prob = p.inner, trans = tf, type = "all")


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

    brmsfit.removers <- unique(c(re.sd, re.cor))

    if (!sjmisc::is_empty(brmsfit.removers))
      mod.dat <- dplyr::select(mod.dat, !! -brmsfit.removers)
  }


  # add bayesian point estimate

  dat <- dat %>%
    tibble::add_column(
      estimate = purrr::map_dbl(mod.dat, sjstats::typical_value, bpe),
      .after = 1
    ) %>%
    tibble::add_column(p.value = 0)


  # remove sigma and lp__ row

  if ("sigma" %in% dat$term) dat <- dplyr::filter(dat, .data$term != "sigma")
  if ("lp__" %in% dat$term) dat <- dplyr::filter(dat, .data$term != "lp__")

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


  # need to transform point estimate as well
  if (!is.null(tf)) {
    funtrans <- match.fun(tf)
    dat$estimate <- funtrans(dat$estimate)
  }

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
tidy_glmmTMB_model <- function(model, ci.lvl) {

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

  if (!sjmisc::is_empty(est[[2]])) {
    zi <- tibble::tibble(
      term = names(est[[1]]),
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


#' @importFrom stats qnorm pnorm
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
