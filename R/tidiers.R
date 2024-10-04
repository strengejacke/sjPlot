tidy_model <- function(
  model, ci.lvl, tf, type, bpe, robust, facets, show.zeroinf, p.val = NULL,
  standardize = FALSE, bootstrap = FALSE, iterations = 1000, seed = NULL,
  p_adjust = NULL, keep = NULL, drop = NULL, std.response = TRUE, ...) {

  if (!is.logical(standardize) && standardize == "") standardize <- NULL
  if (is.logical(standardize) && standardize == FALSE) standardize <- NULL

  if (is.stan(model)) {
    out <- tidy_stan_model(model, ci.lvl, tf, type, bpe, show.zeroinf, facets, ...)
  } else {
    if (!is.null(standardize)) {
      if (isTRUE(standardize)) standardize <- "std"
      model <- datawizard::standardize(
        model,
        two_sd = isTRUE(standardize == "std2"),
        include_response = isTRUE(std.response)
      )
    }
    if (!is.null(seed)) {
      set.seed(seed)
    }

    minfo <- insight::model_info(model)
    if (!is.null(minfo) && ((show.zeroinf && minfo$is_zero_inflated) || minfo$is_dispersion)) {
      component <- "all"
    } else {
      component <- "conditional"
    }

    if (is.null(p.val)) {
      if (inherits(model, c("glm", "polr")) && !inherits(model, "svyglm")) {
        p.val <- "profile"
      } else {
        p.val <- "wald"
      }
    }

    ci_method <- switch(
      p.val,
      "r" = ,
      "residual" = "residual",
      "wald" = "wald",
      "kr" = ,
      "kenward" = "kenward",
      "s" = ,
      "satterthwaite" = "satterthwaite",
      "n" = ,
      "normal" = "normal",
      "profile" = "profile",
      p.val
    )

    if (isTRUE(bootstrap)) {
      ci_method <- "eti"
    }

    if (!insight::is_empty_object(insight::compact_list(robust))) {
      if (!is.null(robust$vcov.type)) {
        robust$vcov.args[["type"]] <- robust$vcov.type
      }
      model_params <- parameters::model_parameters(model, ci = ci.lvl, component = component, bootstrap = bootstrap, iterations = iterations, vcov = robust$vcov.fun, vcov_args = robust$vcov.args, ci_method = ci_method, p_adjust = p_adjust, effects = "fixed", keep = keep, drop = drop, verbose = FALSE)
    } else {
      model_params <- parameters::model_parameters(model, ci = ci.lvl, component = component, bootstrap = bootstrap, iterations = iterations, ci_method = ci_method, p_adjust = p_adjust, effects = "fixed", keep = keep, drop = drop, verbose = FALSE)
    }
    out <- insight::standardize_names(model_params, style = "broom")

    # warning for p-values?
    tryCatch({
      if (insight::model_info(model)$is_mixed && ci_method == "kenward" && insight::find_algorithm(model)$algorithm != "REML") {
        warning("Model was not fitted by REML. Re-fitting model using REML, but p-values, df, etc. still might be unreliable.", call. = FALSE)
      }
    },
    error = function(e) { NULL }
    )

    column <- which(colnames(out) == "response")
    if (length(column)) colnames(out)[column] <- ifelse(isTRUE(facets), "facet", "response.level")

    column <- which(colnames(out) == "component")
    if (length(column)) colnames(out)[column] <- "wrap.facet"

    if (!is.null(out$effect) && "random" %in% out$effect) {
      out$group <- NULL
    }

    column <- which(colnames(out) == "group")
    if (length(column)) colnames(out)[column] <- "wrap.facet"

    # remove duplicated column names
    dupl_cols <- duplicated(colnames(out))
    if (any(dupl_cols)) {
      out <- out[!dupl_cols]
    }

    if ("component" %in% colnames(out)) {
      out$component[out$component == "zero_inflated"] <- "Zero-Inflated Model"
      out$component[out$component == "zi"] <- "Zero-Inflated Model"
      out$component[out$component == "conditional"] <- "Conditional Model"
      out$component[out$component == "count"] <- "Conditional Model"
    }

    attr(out, "pretty_names") <- attributes(model_params)$pretty_names
  }

  out
}




#' @importFrom rlang .data
tidy_stan_model <- function(model, ci.lvl, tf, type, bpe, show.zeroinf, facets, ...) {

  # set defaults

  p.inner <- .5
  p.outer <- ci.lvl

  # get model information
  modfam <- insight::model_info(model)

  if (insight::is_multivariate(model))
    modfam <- modfam[[1]]

  # additional arguments for 'effects()'-function?
  add.args <- lapply(match.call(expand.dots = F)$`...`, function(x) x)

  # check whether we have "prob.inner" and "prob.outer" argument
  # and if so, use these for CI and Bayesian point estimate

  if ("prob.inner" %in% names(add.args)) p.inner <- eval(add.args[["prob.inner"]])
  if ("prob.outer" %in% names(add.args)) p.outer <- eval(add.args[["prob.outer"]])


  # get two CI-intervals

  if (type == "re")
    ty <- "random"
  else
    ty <- "fixed"

  d1 <- bayestestR::ci(model, ci = p.outer, effects = ty, component = "all")
  d2 <- bayestestR::ci(model, ci = p.inner, effects = ty, component = "all")

  if (!is.null(tf)) {
    funtrans <- match.fun(tf)

    d1$CI_low <- funtrans(d1$CI_low)
    d1$CI_high <- funtrans(d1$CI_high)
    d2$CI_low <- funtrans(d2$CI_low)
    d2$CI_high <- funtrans(d2$CI_high)
  }

  # bind columns, so we have inner and outer hdi interval

  dat <- d2 %>%
    dplyr::select(.data$CI_low, .data$CI_high) %>%
    sjmisc::var_rename(CI_low = "conf.low50", CI_high = "conf.high50") %>%
    sjmisc::add_columns(d1) %>%
    sjmisc::var_rename(CI_low = "conf.low", CI_high = "conf.high", Parameter = "term") %>%
    dplyr::select(-.data$CI, -.data$Effects, -.data$Component)

  # for brmsfit models, we need to remove some columns here to
  # match data rows later

  mod.dat <- as.data.frame(model, optional = FALSE)

  if (inherits(model, "brmsfit")) {
    re.sd <- string_starts_with("sd_", x = colnames(mod.dat))
    re.cor <- string_starts_with("cor_", x = colnames(mod.dat))
    lp <- string_starts_with("lp__", x = colnames(mod.dat))
    resp.cor <- string_starts_with("rescor__", x = colnames(mod.dat))
    priors <- string_starts_with("prior_", x = colnames(mod.dat))
    xme <- string_starts_with(pattern = "Xme_me", x = colnames(mod.dat))
    xme.sd <- string_starts_with(pattern = "sdme_me", x = colnames(mod.dat))

    brmsfit.removers <- unique(c(re.sd, re.cor, lp, resp.cor, priors, xme, xme.sd))

    if (!sjmisc::is_empty(brmsfit.removers))
      mod.dat <- dplyr::select(mod.dat, !! -brmsfit.removers)

    # also clean prepared data frame
    resp.cor <- string_starts_with("rescor__", x = dat$term)

    if (!sjmisc::is_empty(resp.cor))
      dat <- dplyr::slice(dat, !! -resp.cor)
  }

  # do transformation on posterior samples first,
  # then summarize (see https://discourse.mc-stan.org/t/monotonic-effects-in-non-gaussian-models/6353/5)

  # need to transform point estimate as well
  if (!is.null(tf)) {
    funtrans <- match.fun(tf)
    all.cols <- sjmisc::seq_col(mod.dat)
    simp.pars <- string_starts_with("simo_mo", colnames(mod.dat))
    if (!sjmisc::is_empty(simp.pars)) all.cols <- all.cols[-simp.pars]
    for (i in all.cols) mod.dat[[i]] <- funtrans(mod.dat[[i]])
  }


  # add bayesian point estimate

  est <- purrr::map_dbl(mod.dat, ~ sjmisc::typical_value(.x, fun = bpe))

  dat <- data_frame(
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
  sorted_columns <- intersect(
    c("term", "estimate", "std.error", "conf.low", "conf.high", "conf.low50", "conf.high50", "p.value"),
    colnames(dat)
  )
  dat <- dat[, sorted_columns]

  # remove some of the information not needed for plotting

  if ("sigma" %in% dat$term) dat <- dplyr::filter(dat, .data$term != "sigma")
  if ("lp__" %in% dat$term) dat <- dplyr::filter(dat, .data$term != "lp__")
  if ("shape" %in% dat$term) dat <- dplyr::filter(dat, .data$term != "shape")


  # remove sd_c and cor_ row

  re <- string_starts_with("sd_", x = dat$term)
  if (!sjmisc::is_empty(re)) dat <- dplyr::slice(dat, !! -re)

  re <- string_starts_with("cor_", x = dat$term)
  if (!sjmisc::is_empty(re)) dat <- dplyr::slice(dat, !! -re)


  # check if we need to keep or remove random effects

  re <- string_starts_with("b[", x = dat$term)
  re.s <- string_starts_with("Sigma[", x = dat$term)
  re.i <- intersect(
    string_starts_with("r_", x = dat$term),
    string_ends_with(".", x = dat$term)
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

    if (length(string_starts_with("b_mu", x = dat$term)) == nrow(dat)) {
      dat$term <- substr(dat$term, 5, max(nchar(dat$term)))
      # create "response-level" variable
      dat <- sjmisc::add_variables(dat, response.level = "", .before = 1)
      dat$response.level <- gsub("(.*)\\_(.*)", "\\1", dat$term)
      dat$term <- gsub("(.*)\\_(.*)", "\\2", dat$term)
    }
  }


  # multivariate-response model?

  if (inherits(model, "brmsfit") && insight::is_multivariate(model)) {

    # get response variables

    responses <- stats::formula(model)$responses

    # also clean prepared data frame
    resp.sigma1 <- string_starts_with("sigma_", x = dat$term)
    resp.sigma2 <- string_starts_with("b_sigma_", x = dat$term)

    resp.sigma <- c(resp.sigma1, resp.sigma2)

    if (!sjmisc::is_empty(resp.sigma))
      dat <- dplyr::slice(dat, !! -resp.sigma)


    # create "response-level" variable

    dat <- sjmisc::add_variables(dat, response.level = "", .before = 1)

    # copy name of response into new character variable
    # and remove response name from term name

    for (i in responses) {
      m <- grep(pattern = sprintf("_%s_", i), x = dat$term)
      dat$response.level[intersect(which(dat$response.level == ""), m)] <- i
      dat$term <- gsub(sprintf("b_%s_", i), "", dat$term, fixed = TRUE)
      dat$term <- gsub(sprintf("s_%s_", i), "", dat$term, fixed = TRUE)
    }


    # check whether each category should be printed in facets, or
    # in a single graph (with dodged geoms)

    if (!missing(facets) && isTRUE(facets))
      colnames(dat)[1] <- "facet"
    else
      colnames(dat)[1] <- "response.level"
  }


  # do we have a zero-inflation model?

  if (modfam$is_zero_inflated || sjmisc::str_contains(dat$term, "b_zi_", ignore.case = T)) {
    dat$wrap.facet <- "Conditional Model"

    # zero-inflated part
    zi <- string_starts_with("b_zi_", x = dat$term)

    # check if zero-inflated part should be shown or removed
    if (show.zeroinf) {
      dat$wrap.facet[zi] <- "Zero-Inflated Model"
      dat$term[zi] <- sub(pattern = "b_zi_", replacement = "b_", x = dat$term[zi], fixed = T)
    } else {
      if (!sjmisc::is_empty(zi)) dat <- dplyr::slice(dat, !! -zi)
    }
  }


  # check model for monotonic effects

  simplex.terms <- string_starts_with(pattern = "simo_mo", x = dat$term)
  if (!sjmisc::is_empty(simplex.terms)) {
    if (!obj_has_name(dat, "wrap.facet")) {
      dat$wrap.facet <- ""
      dat$wrap.facet[simplex.terms] <- "Simplex Parameters"
    } else {
      dat$wrap.facet[simplex.terms] <- sprintf(
        "%s (Simplex Parameters)",
        dat$wrap.facet[simplex.terms]
      )
    }
  }


  # remove facet column if not necessary
  if (!show.zeroinf && obj_has_name(dat, "wrap.facet"))
    dat <- dplyr::select(dat, -.data$wrap.facet)

  dat
}


.get_confint <- function(ci.lvl = .95) {
  if (!is.null(ci.lvl) && !is.na(ci.lvl))
    (1 + ci.lvl) / 2
  else
    .975
}
