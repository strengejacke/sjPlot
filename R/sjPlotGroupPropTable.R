utils::globalVariables(c("dep", "n"))

#' @title Plot grouped proportional tables
#' @name sjp.gpt
#'
#' @description Plot grouped proportional crosstables, where the proportion of
#'                each level of \code{x} for the highest category in \code{y}
#'                is plotted, for each subgroup of \code{groups}.
#'
#' @param x categorical variable, where the proportion of each category in 
#'            \code{x} for the highest category of \code{y} will be printed 
#'            along the x-axis.
#' @param y categorical or numeric variable. If not a binary variable, \code{y}
#'            will be recoded into a binary variable, dichtomized at the highest
#'            category and all remaining categories.
#' @param groups grouping variable, which will define the y-axis
#' @param shape.fill.color optional color vector, fill-color for non-filled shapes
#' @param shapes numeric vector with shape styles, used to map the different
#'          categories of \code{x}.
#' @param show.total logical, if \code{TRUE}, a total summary line for all aggregated
#'          \code{groups} is added.
#' @param annotate.total logical, if \code{TRUE} and \code{show.total = TRUE},
#'          the total-row in the figure will be highlighted with a slightly
#'          shaded background.
#' @param axis.lim numeric vector of length 2, defining the range of the plot axis.
#'          Depending on plot type, may effect either x- or y-axis, or both.
#'
#' @return (Insisibily) returns the ggplot-object with the complete plot
#'           (\code{plot}) as well as the data frame that
#'           was used for setting up the ggplot-object (\code{df}).
#'
#' @inheritParams sjp.grpfrq
#' @inheritParams sjp.lm
#' @inheritParams sjp.xtab
#'
#' @details The p-values are based on \code{\link[stats]{chisq.test}} of \code{x} 
#'            and \code{y} for each \code{groups}.
#'
#' @examples
#' library(sjmisc)
#' data(efc)
#'
#' # the proportion of dependency levels in female
#' # elderly, for each family carer's relationship
#' # to elderly
#' sjp.gpt(efc$e42dep, efc$e16sex, efc$e15relat)
#'
#' # proportion of educational levels in highest
#' # dependency category of elderly, for different
#' # care levels
#' sjp.gpt(efc$c172code, efc$e42dep, efc$n4pstu)
#'
#' @import ggplot2
#' @import sjmisc
#' @importFrom magrittr "%>%"
#' @importFrom dplyr group_by summarise bind_rows
#' @importFrom scales percent
#' @importFrom stats na.omit chisq.test
#' @export
sjp.gpt <- function(x,
                    y,
                    groups,
                    geom.colors = "Set1",
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
                    show.n = TRUE,
                    prnt.plot = TRUE) {
  # --------------------------------------------------------
  # get variable name
  # --------------------------------------------------------
  var.name.x <- get_var_name(deparse(substitute(x)))
  var.name.y <- get_var_name(deparse(substitute(y)))
  # --------------------------------------------------------
  # copy titles
  # --------------------------------------------------------
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
  # --------------------------------------------------------
  # try to automatically set labels if not passed as argument
  # --------------------------------------------------------
  x <- suppressMessages(sjmisc::to_factor(x))
  ylabels <- sjmisc::get_labels(y, attr.only = F, include.values = NULL,
                                include.non.labelled = T)
  # get only value label for hightest category
  ylabels <- ylabels[length(ylabels)]
  if (is.null(axis.labels)) {
    axis.labels <- sjmisc::get_labels(groups, attr.only = F, include.values = NULL, 
                                      include.non.labelled = T)
  }
  if (is.null(axisTitle.y)) {
    axisTitle.y <- paste0("Proportion of ", sjmisc::get_label(x, def.value = var.name.x),
                          " in ", sjmisc::get_label(y, def.value = var.name.y), 
                          " (", ylabels, ")")
  }
  if (is.null(legend.title)) {
    legend.title <- sjmisc::get_label(x, def.value = var.name.x)
  }
  if (is.null(legend.labels)) {
    legend.labels <- sjmisc::get_labels(x, attr.only = F, include.values = NULL,
                                        include.non.labelled = T)
  }
  # ---------------------------------------------
  # set labels that are still missing, but which need values
  # ---------------------------------------------
  if (is.null(axis.labels)) axis.labels <- as.character(c(1:length(groups)))
  # ---------------------------------------------
  # wrap titles and labels
  # ---------------------------------------------
  if (!is.null(legend.labels)) legend.labels <- sjmisc::word_wrap(legend.labels, wrap.legend.labels)
  if (!is.null(legend.title)) legend.title <- sjmisc::word_wrap(legend.title, wrap.legend.title)
  if (!is.null(title)) title <- sjmisc::word_wrap(title, wrap.title)
  if (!is.null(axisTitle.x)) axisTitle.x <- sjmisc::word_wrap(axisTitle.x, wrap.title)
  if (!is.null(axisTitle.y)) axisTitle.y <- sjmisc::word_wrap(axisTitle.y, wrap.title)
  if (!is.null(axis.labels)) axis.labels <- sjmisc::word_wrap(axis.labels, wrap.labels)
  # ------------------------------------
  # final data frae for plot
  # ------------------------------------
  newdf <- data.frame()
  group.p <- c()
  group.n <- c()
    # ------------------------------------
  # create data frame, for dplyr-chain
  # ------------------------------------
  mydf <- stats::na.omit(data.frame(grp = sjmisc::to_value(groups, keep.labels = F),
                                    xpos = x,
                                    dep = sjmisc::to_value(y, keep.labels = F)))
  # ------------------------------------
  # recode dependent variable's categorues
  # max and all others, so we have proportion
  # between maximux value and rest
  # ------------------------------------
  mydf$dep <- sjmisc::rec(mydf$dep, "max=1;else=0")
  # ------------------------------------
  # group data by grouping variable, and inside
  # groups, group the x-variable
  # ------------------------------------
  newdf <- mydf %>%
    dplyr::group_by(grp, xpos) %>%
    dplyr::summarise(ypos = mean(dep))
  # ------------------------------------
  # group data by grouping variable,
  # and summarize N per group and chisq.test
  # of grp and x within each group
  # ------------------------------------
  pvals <- mydf %>%
    dplyr::group_by(grp) %>%
    dplyr::summarise(N = n(), p = suppressWarnings(stats::chisq.test(table(xpos, dep))$p.value))
  # ------------------------------------
  # copy p values
  # ------------------------------------
  for (i in 1:length(pvals$grp)) group.p <- c(group.p, get_p_stars(pvals$p[i]))
  # ------------------------------------
  # copy N
  # ------------------------------------
  for (i in 1:length(pvals$grp)) group.n <- c(group.n, prettyNum(pvals$N[i],
                                                                 big.mark = ",",
                                                                 scientific = F))
  # --------------------------------
  # if we want total line, repeat all for
  # complete data frame
  # --------------------------------
  if (show.total) {
    tmp <- mydf %>%
      dplyr::group_by(xpos) %>%
      dplyr::summarise(ypos = mean(dep))
    # pvalues and N
    pvals <- mydf %>%
      dplyr::summarise(N = n(), p = suppressWarnings(stats::chisq.test(table(xpos, dep))$p.value))
    # bind total row to final df
    newdf <- dplyr::bind_rows(newdf, tmp)
    # copy p values
    group.p <- c(group.p, get_p_stars(pvals$p))
    # copy N
    group.n <- c(group.n, prettyNum(pvals$N, big.mark = ",", scientific = F))
    # add "total" to axis labels
    axis.labels <- c(axis.labels, "Total")
  }
  # ------------------------------------
  # make group variables categorical
  # ------------------------------------
  newdf$grp <- suppressMessages(sjmisc::to_factor(newdf$grp))
  newdf$xpos <- suppressMessages(sjmisc::to_factor(newdf$xpos))
  # ------------------------------------
  # proportion needs to be numeric
  # ------------------------------------
  newdf$ypos <- sjmisc::to_value(newdf$ypos, keep.labels = F)
  # ------------------------------------
  # add N and p-values to axis labels?
  # ------------------------------------
  if (show.n) axis.labels <- paste0(axis.labels, " (n=", group.n, ")")
  if (show.p) axis.labels <- paste0(axis.labels, " ", group.p)
  # --------------------------------------------------------
  # Set up axis limits
  # --------------------------------------------------------
  if (is.null(axis.lim)) axis.lim <- c(0, max(pretty(max(newdf$ypos, na.rm = TRUE), 10)))
  # --------------------------------------------------------
  # Set up grid breaks
  # --------------------------------------------------------
  if (is.null(grid.breaks))
    gridbreaks <- ggplot2::waiver()
  else
    gridbreaks <- c(seq(axis.lim[1], axis.lim[2], by = grid.breaks))
  # --------------------------------------------------------
  # Set up geom colors
  # --------------------------------------------------------
  pal.len <- length(legend.labels)
  if (is.brewer.pal(geom.colors[1])) {
    geom.colors <- scales::brewer_pal(palette = geom.colors[1])(pal.len)
  } else if (geom.colors[1] == "gs") {
    geom.colors <- scales::grey_pal()(pal.len)
  } else if (length(geom.colors) > pal.len) {
    warning("More colors provided than needed. Shortening color palette.")
    geom.colors <- geom.colors[1:pal.len]
  }
  # --------------------------------------------------------
  # Set up plot
  # --------------------------------------------------------
  p <- ggplot(newdf, aes(x = rev(grp), y = ypos, colour = xpos, shape = xpos)) +
    geom_point(size = geom.size, fill = shape.fill.color) +
    scale_y_continuous(labels = scales::percent, breaks = gridbreaks, limits = axis.lim) +
    scale_x_discrete(labels = rev(axis.labels)) +
    scale_shape_manual(name = legend.title, labels = legend.labels, values = shapes[1:pal.len]) +
    scale_colour_manual(name = legend.title, labels = legend.labels, values = geom.colors) +
    labs(x = axisTitle.x, y = axisTitle.y, title = title) +
    coord_flip()
  # --------------------------------------------------------
  # Annotate total line?
  # --------------------------------------------------------
  if (show.total && annotate.total)
    p <- p + annotate("rect", xmin = 0.5,  xmax = 1.5, ymin = -Inf, ymax = Inf, alpha = 0.15)
  # --------------------------------------------------------
  # print plot
  # --------------------------------------------------------
  if (prnt.plot) graphics::plot(p)
  invisible(structure(list(plot = p, df = newdf)))
}
