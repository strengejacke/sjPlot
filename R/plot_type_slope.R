#' @importFrom sjlabelled get_label
#' @importFrom sjstats resp_val pred_vars resp_var
#' @importFrom sjmisc str_contains is_empty
#' @importFrom stats formula residuals
#' @importFrom dplyr filter
plot_type_slope <- function(model,
                            terms,
                            rm.terms,
                            ci.lvl,
                            colors,
                            show.data,
                            facets,
                            case,
                            useResiduals,
                            ...) {

  alpha <- .2
  show.loess <- TRUE

  if (missing(facets)) facets <- TRUE

  # additional arguments?

  add.args <- lapply(match.call(expand.dots = F)$`...`, function(x) x)
  if ("alpha" %in% names(add.args)) alpha <- add.args[["alpha"]]
  if ("show.loess" %in% names(add.args)) show.loess <- add.args[["show.loess"]]


  # set color defaults

  colors <- col_check2(colors, ifelse(isTRUE(show.loess), 3, 2))

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

  model_data <- get_model_frame(model)
  depvar.label <- sjlabelled::get_label(model_data[[1]], def.value = sjstats::resp_var(model), case = case)
  predvars <- sjstats::pred_vars(model)


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

  mydat <- purrr::map_df(predvars, function(p_v) {
    if (useResiduals) {
      tibble::tibble(
        x = sjlabelled::as_numeric(model_data[[p_v]]),
        y = stats::residuals(model),
        group = sjlabelled::get_label(model_data[[p_v]], def.value = p_v, case = case)
      )
    } else {
      tibble::tibble(
        x = sjlabelled::as_numeric(model_data[[p_v]]),
        y = sjstats::resp_val(model),
        group = sjlabelled::get_label(model_data[[p_v]], def.value = p_v, case = case)
      )
    }
  })


  # facets, all in one plot

  if (facets) {

    p <- ggplot(mydat, aes_string(x = "x", y = "y")) +
      stat_smooth(
        method = "lm", se = !is.na(ci.lvl), colour = lineColor,
        fill = lineColor, alpha = alpha, level = ci.lvl
      )

    if (isTRUE(show.loess))
      p <- p + stat_smooth(method = "loess", colour = loessLineColor, se = FALSE)


    # plot raw data if requested

    if (show.data)
      p <- p + geom_point(alpha = .2, colour = pointColor, shape = 16)


    p <- p + facet_wrap(~group, scales = "free")


    # set plot labs
    p <- p + labs(y = response)

  } else {

    p <- list()

    for (p_v in unique(mydat$group)) {

      dat <- dplyr::filter(mydat, .data$group == !! p_v)

      pl <- ggplot(dat, aes_string(x = "x", y = "y")) +
        stat_smooth(
          method = "lm", se = !is.na(ci.lvl), colour = lineColor,
          fill = lineColor, alpha = alpha, level = ci.lvl
        )

      if (isTRUE(show.loess))
        pl <- pl + stat_smooth(method = "loess", colour = loessLineColor, se = FALSE)


      # plot raw data if requested

      if (show.data)
        pl <- pl + geom_point(alpha = .2, colour = pointColor, shape = 16)


      # set plot labs

      pl <- pl +
        labs(
          x = sjlabelled::get_label(model_data[[p_v]], def.value = p_v, case = case),
          y = response
        )


      # add plot object to list
      p[[length(p) + 1]] <- pl
    }
  }

  p
}
