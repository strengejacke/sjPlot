#' @title Forest plot of multiple regression models
#' @name plot_models
#'
#' @description Plot and compare regression coefficients with confidence
#'   intervals of multiple regression models in one plot.
#'
#' @param ... One or more regression models, including glm's or mixed models.
#'   May also be a \code{list} with fitted models. See 'Examples'.
#' @param std.est Choose whether standardized coefficients should be used
#'   for plotting. Default is no standardization (\code{std.est = NULL}).
#'   May be \code{"std"} for standardized beta values or \code{"std2"}, where
#'   standardization is done by rescaling estimates by dividing them by two sd.
#' @param m.labels Character vector, used to indicate the different models
#'   in the plot's legend. If not specified, the labels of the dependent
#'   variables for each model are used.
#' @param legend.pval.title Character vector, used as title of the plot legend that
#'   indicates the p-values. Default is \code{"p-level"}. Only applies if
#'   \code{p.shape = TRUE}.
#' @param spacing Numeric, spacing between the dots and error bars of the
#'   plotted fitted models. Default is 0.3.
#' @param p.shape Logical, if \code{TRUE}, significant levels are distinguished by
#'   different point shapes and a related legend is plotted. Default
#'   is \code{FALSE}.
#'
#' @inheritParams plot_model
#' @inheritParams plot_grpfrq
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
#' \dontrun{
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
#' }
#' @import ggplot2
#' @importFrom rlang .data
#' @export
plot_models <- function(...,
                        transform = NULL,
                        std.est = NULL,
                        std.response = TRUE,
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
                        line.size = NULL,
                        value.size = NULL,
                        spacing = 0.4,
                        colors = "Set1",
                        show.values = FALSE,
                        show.legend = TRUE,
                        show.intercept = FALSE,
                        show.p = TRUE,
                        p.shape = FALSE,
                        p.threshold = c(0.05, 0.01, 0.001),
                        p.adjust = NULL,
                        ci.lvl = .95,
                        robust = FALSE,
                        vcov.fun = NULL,
                        vcov.type = c("HC3", "const", "HC", "HC0", "HC1", "HC2", "HC4", "HC4m", "HC5"),
                        vcov.args = NULL,
                        vline.color = NULL,
                        digits = 2,
                        grid = FALSE,
                        auto.label = TRUE,
                        prefix.labels = c("none", "varname", "label")) {
  # retrieve list of fitted models
  input_list <- list(...)
  names(input_list) <- unlist(lapply(match.call(expand.dots = FALSE)$`...`, deparse))

  vcov.type <- match.arg(vcov.type)

  if (isTRUE(robust)) {
    vcov.type <- "HC3"
    vcov.fun <- "vcovHC"
  }

  # check se-argument
  vcov.fun <- check_se_argument(se = vcov.fun, type = "est")

  if (missing(line.size) || is.null(line.size)) line.size <- .7
  if (missing(value.size) || is.null(value.size)) value.size <- 4

  # check length. if we have a list of fitted model, we need to "unlist" them
  if (length(input_list) == 1 && inherits(input_list[[1]], "list"))
    input_list <- purrr::map(input_list[[1]], ~ .x)

  # check input if really models
  is_model <- vapply(input_list, insight::is_model, logical(1))
  if (!all(is_model)) {
    insight::format_error(
      "Some of the provided objects were not recognized as regression models.",
      "Maybe you are using invalid function arguments? Please check the documentation (`?plot_models`) and your code."
    )
  }

  # get info on model family
  fam.info <- insight::model_info(input_list[[1]])

  if (insight::is_multivariate(input_list[[1]]))
    fam.info <- fam.info[[1]]

  # check whether estimates should be transformed or not

  if (missing(transform)) {
    if (fam.info$is_linear) {
      tf <- NULL
    } else {
      tf <- "exp"
    }
  } else {
    tf <- transform
  }


  # check for standardization, only applies to linear models
  # if (!any(inherits(input_list[[1]], c("lm", "lmerMod", "lme"), which = TRUE) == 1))
  #   std.est <- NULL


  if (!is.null(std.est)) {
    std_method <- switch(std.est, "std" = "refit", "std2" = "2sd", "refit")
  } else {
    std_method <- FALSE
  }

  # if not standardized, we can get simple tidy output and
  # need to check whether intercept should be removed or not

  fl <- purrr::map(
    input_list,
    ~ tidy_model(
      model = .x,
      ci.lvl = ci.lvl,
      tf = transform,
      type = "est",
      bpe = "median",
      robust = list(vcov.fun = vcov.fun, vcov.type = vcov.type, vcov.args = vcov.args),
      facets = TRUE,
      show.zeroinf = FALSE,
      p.val = "wald",
      standardize = std_method,
      std.response = std.response,
      bootstrap = FALSE,
      iterations = 1000,
      seed = NULL,
      p_adjust = p.adjust
    )
  )


  # remove intercept from output
  if (!show.intercept) {
    fl <- purrr::map(fl, function(x) {
      rm.i <- string_ends_with("(Intercept)", x = x$term)
      if (length(rm.i)) {
        dplyr::slice(x, !! -rm.i)
      } else {
        x
      }
    })
  }


  # exponentiation

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
  for (i in seq_along(fl)) {
    fl[[i]] <- sjmisc::add_variables(fl[[i]], group = as.character(i), .after = Inf)
  }

  # merge models to one data frame
  ff <- dplyr::bind_rows(fl)


  # remove further estimates

  rm.terms <- parse_terms(rm.terms)
  rems <- !(ff$term %in% rm.terms)
  if (!is.null(rm.terms)) ff <- dplyr::filter(ff, !! rems)


  # get labels of dependent variables, and wrap them if too long

  if (is.null(m.labels)) m.labels <- sjlabelled::response_labels(input_list)
  m.labels <- sjmisc::word_wrap(m.labels, wrap = wrap.labels)


  # make sure we have distinct labels, because we use them as
  # factor levels. else, duplicated factor levels will be dropped,
  # leading to missing groups in plot output

  if (anyDuplicated(m.labels) > 0)
    m.labels <- suppressMessages(tidy_label(m.labels))

  ff$group <- as.factor(ff$group)
  levels(ff$group) <- m.labels


  # reverse group, to plot correct order from top to bottom
  ff$group <- factor(ff$group, levels = rev(unique(ff$group)))


  # add p-asterisks to data

  ff$p.stars <- get_p_stars(ff$p.value, p.threshold)
  ff$p.label <- sprintf("%.*f", digits, ff$estimate)
  if (show.p) ff$p.label <- sprintf("%s %s", ff$p.label, ff$p.stars)


  # axis limits and tick breaks for y-axis

  axis.scaling <- axis_limits_and_ticks(
    axis.lim = axis.lim,
    min.val = min(ff$conf.low),
    max.val = max(ff$conf.high),
    grid.breaks = grid.breaks,
    exponentiate = isTRUE(tf == "exp"),
    min.est = min(ff$estimate),
    max.est = max(ff$estimate)
  )


  # based on current ggplot theme, highlights vertical default line

  yintercept <- if (isTRUE(tf == "exp")) 1 else 0
  layer_vertical_line <- geom_intercept_line(yintercept, axis.scaling, vline.color)

  # reorder terms
  ff$term <- factor(ff$term, levels = rev(unique(ff$term)))

  # ensure correct legend labels
  ff$p.stars[ff$p.stars == ""] <- "n.s."
  ff$p.stars <- factor(ff$p.stars, levels = c("n.s.", "*", "**", "***"))

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
      width = 0,
      size = line.size
    ) +
    coord_flip() +
    guides(colour = guide_legend(reverse = TRUE))


  # show different shapes depending on p-value

  if (p.shape) p <- p + scale_shape_manual(values = c(1, 16, 17, 15))


  # add value labels

  if (show.values) p <- p +
    geom_text(
      aes_string(label = "p.label"),
      position = position_dodge(spacing),
      vjust = spacing * -1.5,
      hjust = -.1,
      show.legend = FALSE,
      size = value.size
    )


  # check axis labels
  if (is.null(axis.labels) && isTRUE(auto.label))
    axis.labels <- sjlabelled::term_labels(input_list, prefix = prefix.labels)

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
      y = sjmisc::word_wrap(estimate_axis_title(input_list[[1]], axis.title, type = "est", transform = !is.null(tf)), wrap = wrap.title),
      title = sjmisc::word_wrap(title, wrap = wrap.title),
      colour = sjmisc::word_wrap(legend.title, wrap = wrap.legend.title),
      shape = sjmisc::word_wrap(legend.pval.title, wrap = wrap.legend.title)
    )

  p
}
