plot_type_slope <- function(model,
                            terms,
                            rm.terms,
                            ci.lvl,
                            colors,
                            show.data,
                            jitter,
                            facets,
                            axis.title,
                            case,
                            useResiduals,
                            ...) {

  alpha <- .2
  show.loess <- TRUE

  if (missing(facets)) facets <- TRUE

  # additional arguments?

  add.args <- lapply(match.call(expand.dots = F)$`...`, function(x) x)
  if ("alpha" %in% names(add.args)) alpha <- eval(add.args[["alpha"]])
  if ("show.loess" %in% names(add.args)) show.loess <- eval(add.args[["show.loess"]])


  # set color defaults

  colors <- col_check2(colors, if (isTRUE(show.loess)) 3 else 2)

  if (isTRUE(show.loess)) {
    lineColor <- colors[1]
    loessLineColor <- colors[2]
    pointColor <- colors[3]
  } else {
    lineColor <- colors[1]
    pointColor <- colors[2]
  }



  # retrieve amount of predictor variables and
  # retrieve column names of dataset so we can identify in which
  # column the data for each predictor is.

  model_data <- insight::get_data(model, verbose = FALSE)
  depvar.label <- sjlabelled::get_label(model_data[[1]], def.value = insight::find_response(model), case = case)
  predvars <- insight::find_predictors(model, component = "conditional", flatten = TRUE)


  # tell user that interaction terms are not supported by this method

  if (sjmisc::str_contains(deparse(stats::formula(model)), c(":", "*"), logic = "or")) {
    warning("Interaction terms are not supported by this plot type. Output for interaction terms may be inappropriate.", call. = F)
  }


  # remove estimates?

  if (!is.null(rm.terms)) {
    remcols <- match(rm.terms, predvars)

    if (!sjmisc::is_empty(remcols))
      predvars <- predvars[-remcols]
  }


  # select specific setimates?

  if (!is.null(terms)) {
    remcols <- match(terms, predvars)

    if (!sjmisc::is_empty(remcols))
      predvars <- predvars[remcols]
  }


  # retrieve name of dependent variable

  response <- ifelse(isTRUE(useResiduals), "residuals", depvar.label)

  # iterate all predictors

  mydat <- suppressWarnings(purrr::map_df(predvars, function(p_v) {

    # make sure we have the variable in our data. for mixed
    # models, we might have some random effects which were not
    # in the model frame

    if (obj_has_name(model_data, p_v)) {

      if (useResiduals) {
        data_frame(
          x = sjlabelled::as_numeric(model_data[[p_v]]),
          y = stats::residuals(model),
          group = sjlabelled::get_label(model_data[[p_v]], def.value = p_v, case = case)
        )
      } else {
        data_frame(
          x = sjlabelled::as_numeric(model_data[[p_v]]),
          y = insight::get_response(model),
          group = sjlabelled::get_label(model_data[[p_v]], def.value = p_v, case = case)
        )
      }

    }
  }))


  # facets, all in one plot

  if (facets) {

    p <- ggplot(mydat, aes(x = .data$x, y = .data$y)) +
      stat_smooth(
        method = "lm", se = !is.na(ci.lvl), colour = lineColor,
        fill = lineColor, alpha = alpha, level = ci.lvl
      )

    if (isTRUE(show.loess))
      p <- p + stat_smooth(method = "loess", colour = loessLineColor, se = FALSE)


    # plot raw data if requested

    if (show.data) {
      if (!is.null(jitter))
        p <- p + geom_jitter(alpha = .2, colour = pointColor, shape = 16, width = jitter)
      else
        p <- p + geom_point(alpha = .2, colour = pointColor, shape = 16)
    }



    p <- p + facet_wrap(~group, scales = "free")


    # set plot labs
    p <- p + labs(x = NULL, y = response)

  } else {

    p <- list()

    for (p_v in unique(mydat$group)) {

      dat <- dplyr::filter(mydat, .data$group == !! p_v)

      pl <- ggplot(dat, aes(x = .data$x, y = .data$y)) +
        stat_smooth(
          method = "lm", se = !is.na(ci.lvl), colour = lineColor,
          fill = lineColor, alpha = alpha, level = ci.lvl
        )

      if (isTRUE(show.loess))
        pl <- pl + stat_smooth(method = "loess", colour = loessLineColor, se = FALSE)


      # plot raw data if requested

      if (show.data)
        pl <- pl + geom_point(alpha = .2, colour = pointColor, shape = 16)


      # set plot labs. check if we have custom axis titles

      if (!is.null(axis.title)) {

        if (is.list(axis.title)) {
          xt <- axis.title[[length(p) + 1]][1]
          yt <- axis.title[[length(p) + 1]][2]
        } else {
          xt <- axis.title[1]
          yt <- axis.title[2]
        }

      } else {
        xt <- p_v
        yt <- response
      }

      pl <- pl +
        labs(x = xt, y = yt)


      # add plot object to list
      p[[length(p) + 1]] <- pl
    }
  }

  p
}
