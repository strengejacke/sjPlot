#' @title Forest plot of multiple regression models
#' @name plot_models
#'
#' @description Plot and compare regression coefficients with confidence
#'                intervals of multiple regression models in one plot.
#'
#' @param ... One or more regression models, including glm's or mixed models.
#'        May also be a \code{list} with fitted models. See 'Examples'.
#' @param std.est For linear models, choose whether standardized coefficients should
#'        be used for plotting. Default is no standardization.
#'        \describe{
#'          \item{\code{NULL}}{(default) no standardization, returns original estimates.}
#'          \item{\code{"std"}}{standardized beta values.}
#'          \item{\code{"std2"}}{standardized beta values, however, standardization is done by rescaling estimates by dividing them by two sd (see \code{\link[sjstats]{std_beta}}).}
#'        }
#' @param m.labels Character vector, used to indicate the different models
#'          in the plot's legend. If not specified, the labels of the dependent
#'          variables for each model are used.
#' @param legend.pval.title Character vector, used as title of the plot legend that
#'        indicates the p-values. Default is \code{"p-level"}. Only applies if
#'        \code{p.shape = TRUE}.
#' @param spacing Numeric, spacing between the dots and error bars of the
#'        plotted fitted models. Default is 0.3.
#' @param p.shape Logical, if \code{TRUE}, significant levels are distinguished by
#'        different point shapes and a related legend is plotted. Default
#'        is \code{FALSE}.
#'
#' @inheritParams plot_model
#' @inheritParams sjp.lm
#' @inheritParams sjp.lmer
#' @inheritParams sjt.lm
#' @inheritParams sjp.grpfrq
#'
#' @return A ggplot-object.
#'
#' @examples
#' data(efc)
#'
#' # fit three models
#' fit1 <- lm(barthtot ~ c160age + c12hour + c161sex + c172code, data = efc)
#' fit2 <- lm(neg_c_7 ~ c160age + c12hour + c161sex + c172code, data = efc)
#' fit3 <- lm(tot_sc_e ~ c160age + c12hour + c161sex + c172code, data = efc)
#'
#' # plot multiple models
#' plot_models(fit1, fit2, fit3, grid = TRUE)
#'
#' # plot multiple models with legend labels and
#' # point shapes instead of value labels
#' plot_models(
#'   fit1, fit2, fit3,
#'   axis.labels = c(
#'     "Carer's Age", "Hours of Care", "Carer's Sex", "Educational Status"
#'   ),
#'   m.labels = c("Barthel Index", "Negative Impact", "Services used"),
#'   show.values = FALSE, show.p = FALSE, p.shape = TRUE
#' )
#'
#' # plot multiple models from nested lists argument
#' all.models <- list()
#' all.models[[1]] <- fit1
#' all.models[[2]] <- fit2
#' all.models[[3]] <- fit3
#'
#' plot_models(all.models)
#'
#' # plot multiple models with different predictors (stepwise inclusion),
#' # standardized estimates
#' fit1 <- lm(mpg ~ wt + cyl + disp + gear, data = mtcars)
#' fit2 <- update(fit1, . ~ . + hp)
#' fit3 <- update(fit2, . ~ . + am)
#'
#' plot_models(fit1, fit2, fit3, std.est = "std2")
#'
#' @import ggplot2
#' @importFrom purrr map map_df map2
#' @importFrom dplyr slice bind_rows filter
#' @importFrom broom tidy
#' @importFrom forcats fct_rev
#' @importFrom sjstats std_beta p_value
#' @importFrom sjlabelled get_dv_labels get_term_labels
#' @importFrom rlang .data
#' @importFrom sjmisc word_wrap var_rename
#' @importFrom tibble tidy_names add_column
#' @export
plot_models <- function(...,
                        transform,
                        std.est = NULL,
                        rm.terms = NULL,
                        title = NULL,
                        m.labels = NULL,
                        legend.title = "Dependent Variables",
                        legend.pval.title = "p-level",
                        axis.labels = NULL,
                        axis.title = NULL,
                        axis.lim = NULL,
                        wrap.title = 50,
                        wrap.labels = 25,
                        wrap.legend.title = 20,
                        grid.breaks = NULL,
                        dot.size = 3,
                        spacing = 0.4,
                        colors = "Set1",
                        show.values = FALSE,
                        show.legend = TRUE,
                        show.intercept = FALSE,
                        show.p = TRUE,
                        p.shape = FALSE,
                        ci.lvl = .95,
                        vline.color = NULL,
                        digits = 2,
                        grid = FALSE) {
  # retrieve list of fitted models
  input_list <- tibble::lst(...)

  # check length. if we have a list of fitted model, we need to "unlist" them
  if (length(input_list) == 1 && class(input_list[[1]]) == "list")
    input_list <- purrr::map(input_list[[1]], ~ .x)

  # check whether estimates should be exponentiated or not
  if (missing(transform)) {
    if (inherits(input_list[[1]], c("glm", "glmerMod", "glmmTMB")))
      tf <- "exp"
    else
      tf <- NULL
  } else
    tf <- transform

  # check for standardization, only applies to linear models
  if (!any(inherits(input_list[[1]], c("lm", "lmerMod", "lme"), which = TRUE) == 1))
    std.est <- NULL


  # tidy output
  if (!is.null(std.est)) {

    # for standardized estimates, we need to rename a column,
    # and manually add p-values to the output. intercept is already
    # removed from output

    fl <- input_list %>%
      purrr::map(~ sjstats::std_beta(.x, type = std.est)) %>%
      purrr::map(~ sjmisc::var_rename(.x, std.estimate = "estimate")) %>%
      purrr::map2(input_list, ~ tibble::add_column(
        .x, p.value = sjstats::p_value(.y)[["p.value"]][-1])
      )

  } else {

    # if not standardized, we can get simple tidy output and
    # need to check whether intercept should be removed or not

    fl <- purrr::map(
      input_list, ~ tidy_model(.x, ci.lvl, tf = transform, type = "est", bpe = "line", ...)
    )

    # remove intercept from output
    if (!show.intercept) fl <- purrr::map(fl, ~ dplyr::slice(.x, -1))

  }


  # exponentiation from broom::tidy does not work with merMod-objecs,
  # so we do it manually for all model classes

  if (!is.null(tf)) {
    funtrans <- match.fun(tf)
    fl <- purrr::map(fl, function(x) {
      x[["estimate"]] <- funtrans(x[["estimate"]])
      x[["conf.low"]] <- funtrans(x[["conf.low"]])
      x[["conf.high"]] <- funtrans(x[["conf.high"]])

      x
    })
  }


  # add grouping index
  for (i in 1:length(fl)) fl[[i]] <- tibble::add_column(fl[[i]], group = as.character(i))

  # merge models to one data frame
  ff <- dplyr::bind_rows(fl)


  # rename terms, if we did std2-type of standardization. pkg "arm" adds
  # a "z." suffix to each term name

  if (!is.null(std.est) && std.est == "std2")
    ff$term <- substring(ff$term, first = 3)


  # remove further estimates

  rems <- !(ff$term %in% rm.terms)
  if (!is.null(rm.terms)) ff <- dplyr::filter(ff, !! rems)


  # get labels of dependent variables, and wrap them if too long

  if (is.null(m.labels)) m.labels <- sjlabelled::get_dv_labels(input_list)
  m.labels <- sjmisc::word_wrap(m.labels, wrap = wrap.labels)


  # make sure we have distinct labels, because we use them as
  # factor levels. else, duplicated factor levels will be dropped,
  # leading to missing groups in plot output

  if (anyDuplicated(m.labels) > 0)
    m.labels <- suppressMessages(tibble::tidy_names(m.labels))

  ff$group <- as.factor(ff$group)
  levels(ff$group) <- m.labels


  # reverse group, to plot correct order from top to bottom
  ff$group <- forcats::fct_rev(ff$group)


  # add p-asterisks to data

  ff$p.stars <- get_p_stars(ff$p.value)
  ff$p.label <- sprintf("%.*f", digits, ff$estimate)
  if (show.p) ff$p.label <- sprintf("%s %s", ff$p.label, ff$p.stars)


  # axis limits and tick breaks for y-axis

  axis.scaling <- get_axis_limits_and_ticks(
    axis.lim = axis.lim,
    min.val = min(ff$conf.low),
    max.val = max(ff$conf.high),
    grid.breaks = grid.breaks,
    exponentiate = isTRUE(tf == "exp"),
    min.est = min(ff$estimate),
    max.est = max(ff$estimate)
  )


  # based on current ggplot theme, highlights vertical default line

  yintercept = ifelse(isTRUE(tf == "exp"), 1, 0)
  layer_vertical_line <- geom_intercep_line(yintercept, axis.scaling, vline.color)


  # set up base plot

  if (p.shape)
    p <- ggplot(ff, aes_string(x = "term", y = "estimate", colour = "group", shape = "p.stars"))
  else
    p <- ggplot(ff, aes_string(x = "term", y = "estimate", colour = "group"))


  p <- p +
    layer_vertical_line +
    geom_point(position = position_dodge(spacing), size = dot.size) +
    geom_errorbar(
      aes_string(ymin = "conf.low", ymax = "conf.high"),
      position = position_dodge(spacing),
      width = 0
    ) +
    coord_flip() +
    guides(colour = guide_legend(reverse = TRUE))


  # show different shapes depending on p-value

  if (p.shape) p <- p +
    scale_shape_manual(
      values = c(1, 16, 17, 15),
      labels = c("n.s.", "*", "**", "***")
    )


  # add value labels

  if (show.values) p <- p +
    geom_text(
      aes_string(label = "p.label"),
      position = position_dodge(spacing),
      vjust = spacing * -1.5,
      hjust = -.1,
      show.legend = FALSE
    )


  # check axis labels
  if (is.null(axis.labels)) axis.labels <- sjlabelled::get_term_labels(input_list)

  # set axis labels
  p <- p + scale_x_discrete(labels = sjmisc::word_wrap(axis.labels, wrap = wrap.labels))


  # hide legend?
  if (!show.legend) p <- p + guides(colour = "none", shape = "none")

  # facets
  if (grid) p <- p + facet_grid(~group)


  # we need transformed scale for exponentiated estimates

  if (isTRUE(tf == "exp")) {
    p <- p + scale_y_continuous(
      trans = "log10",
      limits = axis.scaling$axis.lim,
      breaks = axis.scaling$ticks,
      labels = prettyNum
    )
  } else {
    p <- p + scale_y_continuous(
      limits = axis.scaling$axis.lim,
      breaks = axis.scaling$ticks,
      labels = axis.scaling$ticks
    )
  }


  # set colors
  p <- p + scale_colour_manual(values = col_check2(colors, length(m.labels)))


  # set axis and plot titles

  p <-
    p + labs(
      x = NULL,
      y = sjmisc::word_wrap(get_estimate_axis_title(input_list[[1]], axis.title, type = "est"), wrap = wrap.title),
      title = sjmisc::word_wrap(title, wrap = wrap.title),
      colour = sjmisc::word_wrap(legend.title, wrap = wrap.legend.title),
      shape = sjmisc::word_wrap(legend.pval.title, wrap = wrap.legend.title)
    )

  p
}


#' @rdname plot_models
#' @export
sjp.lmm <- function(...) {
  .Deprecated(new = "plot_models")
}

#' @rdname plot_models
#' @export
sjp.glmm <- function(...) {
  .Deprecated(new = "plot_models")
}
