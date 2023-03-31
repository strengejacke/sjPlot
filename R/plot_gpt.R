utils::globalVariables("n")

#' @title Plot grouped proportional tables
#' @name plot_gpt
#'
#' @description Plot grouped proportional crosstables, where the proportion of
#'                each level of \code{x} for the highest category in \code{y}
#'                is plotted, for each subgroup of \code{grp}.
#'
#' @param x Categorical variable, where the proportion of each category in
#'            \code{x} for the highest category of \code{y} will be printed
#'            along the x-axis.
#' @param y Categorical or numeric variable. If not a binary variable, \code{y}
#'            will be recoded into a binary variable, dichtomized at the highest
#'            category and all remaining categories.
#' @param grp Grouping variable, which will define the y-axis
#' @param shape.fill.color Optional color vector, fill-color for non-filled shapes
#' @param shapes Numeric vector with shape styles, used to map the different
#'          categories of \code{x}.
#' @param show.total Logical, if \code{TRUE}, a total summary line for all aggregated
#'          \code{grp} is added.
#' @param annotate.total Logical, if \code{TRUE} and \code{show.total = TRUE},
#'          the total-row in the figure will be highlighted with a slightly
#'          shaded background.
#' @param axis.lim Numeric vector of length 2, defining the range of the plot axis.
#'          Depending on plot type, may effect either x- or y-axis, or both.
#'          For multiple plot outputs (e.g., from \code{type = "eff"} or
#'          \code{type = "slope"} in \code{\link{plot_model}}), \code{axis.lim} may
#'          also be a list of vectors of length 2, defining axis limits for each
#'          plot (only if non-faceted).
#' @param show.p Logical, adds significance levels to values, or value and
#'          variable labels.
#'
#' @return A ggplot-object.
#'
#' @inheritParams plot_scatter
#' @inheritParams plot_grpfrq
#' @inheritParams plot_xtab
#'
#' @details The p-values are based on \code{\link[stats]{chisq.test}} of \code{x}
#'            and \code{y} for each \code{grp}.
#'
#' @examples
#' if (requireNamespace("haven")) {
#'   data(efc)
#'
#'   # the proportion of dependency levels in female
#'   # elderly, for each family carer's relationship
#'   # to elderly
#'   plot_gpt(efc, e42dep, e16sex, e15relat)
#'
#'   # proportion of educational levels in highest
#'   # dependency category of elderly, for different
#'   # care levels
#'   plot_gpt(efc, c172code, e42dep, n4pstu)
#' }
#' @import ggplot2
#' @export
plot_gpt <- function(
  data,
  x,
  y,
  grp,
  colors = "metro",
  geom.size = 2.5,
  shape.fill.color = "#f0f0f0",
  shapes = c(15, 16, 17, 18, 21, 22, 23, 24, 25, 7, 8, 9, 10, 12),
  title = NULL,
  axis.labels = NULL,
  axis.titles = NULL,
  legend.title = NULL,
  legend.labels = NULL,
  wrap.title = 50,
  wrap.labels = 15,
  wrap.legend.title = 20,
  wrap.legend.labels = 20,
  axis.lim = NULL,
  grid.breaks = NULL,
  show.total = TRUE,
  annotate.total = TRUE,
  show.p = TRUE,
  show.n = TRUE)
{

  # get data

  name.x <- deparse(substitute(x))
  name.y <- deparse(substitute(y))
  name.grp <- deparse(substitute(grp))

  pl <- NULL

  if (inherits(data, "grouped_df")) {
    # get grouped data
    grps <- get_grouped_data(data)

    # now plot everything
    for (i in seq_len(nrow(grps))) {
      # copy back labels to grouped data frame
      tmp <- sjlabelled::copy_labels(grps$data[[i]], data)

      # prepare argument list, including title
      tmp.title <- get_grouped_plottitle(data, grps, i, sep = "\n")

      # copy data

      x <- tmp[[name.x]]
      y <- tmp[[name.y]]
      grp <- tmp[[name.grp]]

      # plot

      plots <- gpt_helper(
        x, y, grp, colors, geom.size, shape.fill.color, shapes, title = tmp.title,
        axis.labels, axis.titles, legend.title, legend.labels, wrap.title,
        wrap.labels, wrap.legend.title, wrap.legend.labels, axis.lim,
        grid.breaks, show.total, annotate.total, show.p, show.n, name.x, name.y, name.grp
      )

      # add plots, check for NULL results
      pl <- c(pl, list(plots))
    }
  } else {
    # copy data
    x <- data[[name.x]]
    y <- data[[name.y]]
    grp <- data[[name.grp]]

    # plot

    pl <- gpt_helper(
      x, y, grp, colors, geom.size, shape.fill.color, shapes, title,
      axis.labels, axis.titles, legend.title, legend.labels, wrap.title,
      wrap.labels, wrap.legend.title, wrap.legend.labels, axis.lim,
      grid.breaks, show.total, annotate.total, show.p, show.n, name.x, name.y, name.grp
    )
  }

  pl
}


gpt_helper <- function(
  x, y, grp, colors, geom.size, shape.fill.color, shapes, title,
  axis.labels, axis.titles, legend.title, legend.labels, wrap.title,
  wrap.labels, wrap.legend.title, wrap.legend.labels, axis.lim,
  grid.breaks, show.total, annotate.total, show.p, show.n, name.x, name.y, name.grp
) {
  # any missing names?

  if (is.null(name.x) || name.x == "NULL") name.x <- ""
  if (is.null(name.y) || name.y == "NULL") name.y <- ""
  if (is.null(name.grp) || name.grp == "NULL") name.grp <- ""

  # copy titles
  if (is.null(axis.titles)) {
    axisTitle.x <- NULL
    axisTitle.y <- NULL
  } else {
    axisTitle.x <- axis.titles[1]
    if (length(axis.titles) > 1)
      axisTitle.y <- axis.titles[2]
    else
      axisTitle.y <- NULL
  }

  # try to automatically set labels if not passed as argument
  x <- suppressMessages(sjmisc::to_factor(x))
  ylabels <-
    sjlabelled::get_labels(
      y,
      attr.only = F,
      values = NULL,
      non.labelled = T
    )

  # get only value label for hightest category
  ylabels <- ylabels[length(ylabels)]
  if (is.null(axis.labels)) {
    axis.labels <-
      sjlabelled::get_labels(
        grp,
        attr.only = F,
        values = NULL,
        non.labelled = T
      )
  }

  if (is.null(axisTitle.y)) {
    axisTitle.y <-
      paste0(
        "Proportion of ",
        sjlabelled::get_label(x, def.value = name.x),
        " in ",
        sjlabelled::get_label(y, def.value = name.y),
        " (",
        ylabels,
        ")"
      )
  }

  if (is.null(legend.title)) {
    legend.title <- sjlabelled::get_label(x, def.value = name.x)
  }

  if (is.null(legend.labels)) {
    legend.labels <-
      sjlabelled::get_labels(
        x,
        attr.only = F,
        values = NULL,
        non.labelled = T
      )
  }

  # set labels that are still missing, but which need values
  if (is.null(axis.labels)) axis.labels <- as.character(seq_len(length(grp)))

  # wrap titles and labels
  if (!is.null(legend.labels)) legend.labels <- sjmisc::word_wrap(legend.labels, wrap.legend.labels)
  if (!is.null(legend.title)) legend.title <- sjmisc::word_wrap(legend.title, wrap.legend.title)
  if (!is.null(title)) title <- sjmisc::word_wrap(title, wrap.title)
  if (!is.null(axisTitle.x)) axisTitle.x <- sjmisc::word_wrap(axisTitle.x, wrap.title)
  if (!is.null(axisTitle.y)) axisTitle.y <- sjmisc::word_wrap(axisTitle.y, wrap.title)
  if (!is.null(axis.labels)) axis.labels <- sjmisc::word_wrap(axis.labels, wrap.labels)

  # final data frame for plot
  newdf <- data.frame()
  group.p <- character()
  group.n <- character()

  # create data frame, for dplyr-chain
  mydf <-
    stats::na.omit(data.frame(
      grp = sjlabelled::as_numeric(grp, keep.labels = F),
      xpos = x,
      dep = sjlabelled::as_numeric(y, keep.labels = F)
    ))

  # recode dependent variable's categorues
  # max and all others, so we have proportion
  # between maximux value and rest
  mydf$dep <- sjmisc::rec(mydf$dep, rec = "max=1;else=0", append = FALSE)

  # group data by grouping variable, and inside
  # groups, group the x-variable

  newdf <- mydf %>%
    dplyr::group_by(.data$grp, .data$xpos) %>%
    dplyr::summarise(ypos = mean(.data$dep))

  # group data by grouping variable,
  # and summarize N per group and chisq.test
  # of grp and x within each group

  pvals <- mydf %>%
    dplyr::group_by(.data$grp) %>%
    dplyr::summarise(N = dplyr::n(), p = suppressWarnings(stats::chisq.test(table(.data$xpos, .data$dep))$p.value))

  # copy p values
  for (i in seq_len(length(pvals$grp))) group.p[i] <- get_p_stars(pvals$p[i])

  # copy N
  for (i in seq_len(length(pvals$grp)))
    group.n[i] <- prettyNum(pvals$N[i], big.mark = ",", scientific = F)

  # if we want total line, repeat all for
  # complete data frame
  if (show.total) {
    tmp <- mydf %>%
      dplyr::group_by(.data$xpos) %>%
      dplyr::summarise(ypos = mean(.data$dep))

    # pvalues and N
    pvals <- mydf %>%
      dplyr::summarise(N = dplyr::n(), p = suppressWarnings(stats::chisq.test(table(.data$xpos, .data$dep))$p.value))

    # bind total row to final df
    newdf <- dplyr::bind_rows(newdf, tmp)

    # copy p values
    group.p <- c(group.p, get_p_stars(pvals$p))
    # copy N
    group.n <- c(group.n, prettyNum(pvals$N, big.mark = ",", scientific = F))
    # add "total" to axis labels
    axis.labels <- c(axis.labels, "Total")
  }

  # make group variables categorical
  newdf$grp <- suppressMessages(sjmisc::to_factor(newdf$grp))
  newdf$xpos <- suppressMessages(sjmisc::to_factor(newdf$xpos))

  # proportion needs to be numeric
  newdf$ypos <- sjlabelled::as_numeric(newdf$ypos, keep.labels = F)

  # add N and p-values to axis labels?
  if (show.n) axis.labels <- paste0(axis.labels, " (n=", group.n, ")")
  if (show.p) axis.labels <- paste0(axis.labels, " ", group.p)

  # Set up axis limits
  if (is.null(axis.lim)) axis.lim <- c(0, max(pretty(max(newdf$ypos, na.rm = TRUE), 10)))

  # Set up grid breaks
  if (is.null(grid.breaks))
    gridbreaks <- waiver()
  else
    gridbreaks <- seq(axis.lim[1], axis.lim[2], by = grid.breaks)

  # Set up geom colors
  pal.len <- length(legend.labels)
  geom.colors <- col_check2(colors, pal.len)

  # Set up plot
  p <- ggplot(newdf, aes(x = rev(.data$grp), y = .data$ypos, colour = .data$xpos, shape = .data$xpos)) +
    geom_point(size = geom.size, fill = shape.fill.color) +
    scale_y_continuous(labels = scales::percent, breaks = gridbreaks, limits = axis.lim) +
    scale_x_discrete(labels = rev(axis.labels)) +
    scale_shape_manual(name = legend.title, labels = legend.labels, values = shapes[1:pal.len]) +
    scale_colour_manual(name = legend.title, labels = legend.labels, values = geom.colors) +
    labs(x = axisTitle.x, y = axisTitle.y, title = title) +
    coord_flip()

  # Annotate total line?
  if (show.total && annotate.total)
    p <- p + annotate("rect", xmin = 0.5,  xmax = 1.5, ymin = -Inf, ymax = Inf, alpha = 0.15)

  p
}
