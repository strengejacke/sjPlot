#' @title Plot (grouped) scatter plots
#' @name plot_scatter
#'
#' @description Display scatter plot of two variables. Adding a grouping variable to
#'   the scatter plot is possible. Furthermore, fitted lines can be added
#'   for each group as well as for the overall plot.
#'
#' @param data A data frame, or a grouped data frame.
#' @param x Name of the variable for the x-axis.
#' @param y Name of the variable for the y-axis.
#' @param grp Optional, name of the grouping-variable. If not missing, the
#'   scatter plot will be grouped. See 'Examples'.
#' @param dot.labels Character vector with names for each coordinate pair given
#'   by \code{x} and \code{y}, so text labels are added to the plot.
#'   Must be of same length as \code{x} and \code{y}.
#'   If \code{dot.labels} has a different length, data points will be trimmed
#'   to match \code{dot.labels}. If \code{dot.labels = NULL} (default),
#'   no labels are printed.
#' @param label.size Size of text labels if argument \code{dot.labels} is used.
#' @param fit.line,fit.grps Specifies the method to add a fitted line accross
#'   the data points. Possible values are for instance \code{"lm"}, \code{"glm"},
#'   \code{"loess"} or \code{"auto"}. If \code{NULL}, no line is plotted.
#'   \code{fit.line} adds a fitted line for the complete data, while \code{fit.grps}
#'   adds a fitted line for each subgroup of \code{grp}.
#' @param emph.dots Logical, if \code{TRUE}, overlapping points at same coordinates
#'          will be becomme larger, so point size indicates amount of overlapping.
#' @param show.rug Logical, if \code{TRUE}, a marginal rug plot is displayed
#'          in the graph.
#'
#' @return A ggplot-object. For grouped data frames, a list of ggplot-objects for
#'   each group in the data.
#'
#' @inheritParams plot_model
#' @inheritParams plot_grpfrq
#'
#' @examples
#' # load sample date
#' library(sjmisc)
#' library(sjlabelled)
#' data(efc)
#'
#' # simple scatter plot
#' plot_scatter(efc, e16sex, neg_c_7)
#'
#' # simple scatter plot, increased jittering
#' plot_scatter(efc, e16sex, neg_c_7, jitter = .4)
#'
#' # grouped scatter plot
#' plot_scatter(efc, c160age, e17age, e42dep)
#'
#' # grouped scatter plot with marginal rug plot
#' # and add fitted line for complete data
#' plot_scatter(
#'   efc, c12hour, c160age, c172code,
#'   show.rug = TRUE, fit.line = "lm"
#' )
#'
#' # grouped scatter plot with marginal rug plot
#' # and add fitted line for each group
#' plot_scatter(
#'   efc, c12hour, c160age, c172code,
#'   show.rug = TRUE, fit.grps = "loess",
#'   grid = TRUE
#' )
#'
#' @import ggplot2
#' @export
plot_scatter <- function(
  data,
  x,
  y,
  grp,
  title = "",
  legend.title = NULL,
  legend.labels = NULL,
  dot.labels = NULL,
  axis.titles = NULL,
  dot.size = 1.5,
  label.size = 3,
  colors = "metro",
  fit.line = NULL,
  fit.grps = NULL,
  show.rug = FALSE,
  show.legend = TRUE,
  show.ci = FALSE,
  wrap.title = 50,
  wrap.legend.title = 20,
  wrap.legend.labels = 20,
  jitter = .05,
  emph.dots = FALSE,
  grid = FALSE
) {

  # check available packages

  if (!is.null(dot.labels) && !requireNamespace("ggrepel", quietly = TRUE)) {
    stop("Package `ggrepel` needed to plot labels. Please install it.", call. = FALSE)
  }


  # get data

  name.x <- deparse(substitute(x))
  name.y <- deparse(substitute(y))

  if (!missing(grp))
    name.grp <- deparse(substitute(grp))
  else
    name.grp <- NULL


  # optionally hide legend if not needed

  if (!is.null(name.grp) && grid && missing(show.legend)) show.legend <- FALSE

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
      if (!is.null(name.grp))
        grp <- tmp[[name.grp]]
      else
        grp <- NULL

      # prepare color palette

      if (!is.null(grp))
        collen <- dplyr::n_distinct(grp, na.rm = TRUE)
      else
        collen <- 1

      colors <- col_check2(colors, collen)

      # plot

      plots <- scatter_helper(
        x, y, grp, title = tmp.title, legend.title, legend.labels, dot.labels, axis.titles,
        dot.size, label.size, colors, fit.line, fit.grps, show.rug,
        show.legend, show.ci, wrap.title, wrap.legend.title, wrap.legend.labels,
        jitter, emph.dots, grid, name.x, name.y, name.grp
      )

      # add plots, check for NULL results
      pl <- c(pl, list(plots))
    }
  } else {
    # copy data
    x <- data[[name.x]]
    y <- data[[name.y]]
    if (!is.null(name.grp))
      grp <- data[[name.grp]]
    else
      grp <- NULL

    # prepare color palette

    if (!is.null(grp))
      collen <- dplyr::n_distinct(grp, na.rm = TRUE)
    else
      collen <- 1

    colors <- col_check2(colors, collen)

    # plot

    pl <- scatter_helper(
      x, y, grp, title, legend.title, legend.labels, dot.labels, axis.titles,
      dot.size, label.size, colors, fit.line, fit.grps, show.rug,
      show.legend, show.ci, wrap.title, wrap.legend.title, wrap.legend.labels,
      jitter, emph.dots, grid, name.x, name.y, name.grp
    )
  }

  pl
}


#' @importFrom stats na.omit
#' @importFrom sjlabelled get_labels get_label
#' @importFrom sjmisc word_wrap
scatter_helper <- function(
  x, y, grp, title, legend.title, legend.labels, dot.labels, axis.titles,
  dot.size, label.size, colors, fit.line, fit.grps, show.rug,
  show.legend, show.ci, wrap.title, wrap.legend.title, wrap.legend.labels,
  jitter, emph.dots, grid, name.x, name.y, name.grp

) {
  # any missing names?

  if (is.null(name.x) || name.x == "NULL") name.x <- ""
  if (is.null(name.y) || name.y == "NULL") name.y <- ""

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


  # try to automatically set labels is not passed as parameter

  if (is.null(legend.labels) && !is.null(grp)) {
    legend.labels <- sjlabelled::get_labels(
      grp,
      attr.only = F,
      values = NULL,
      non.labelled = T
    )
  }

  if (is.null(legend.title) && !is.null(grp)) legend.title <- sjlabelled::get_label(grp, def.value = name.grp)
  if (is.null(axisTitle.x)) axisTitle.x <- sjlabelled::get_label(x, def.value = name.x)
  if (is.null(axisTitle.y)) axisTitle.y <- sjlabelled::get_label(y, def.value = name.y)

  if (is.null(title)) {
    t1 <- sjlabelled::get_label(x, def.value = name.x)
    t2 <- sjlabelled::get_label(y, def.value = name.y)
    if (!is.null(t1) && !is.null(t2)) {
      title <- paste0(t1, " by ", t2)
      if (!is.null(grp)) {
        t3 <- sjlabelled::get_label(grp, def.value = name.grp)
        if (!is.null(t3)) title <- paste0(title, " (grouped by ", t3, ")")
      }
    }
  }

  # remove titles if empty

  if (!is.null(legend.title) && legend.title == "") legend.title <- NULL
  if (!is.null(axisTitle.x) && axisTitle.x == "") axisTitle.x <- NULL
  if (!is.null(axisTitle.y) && axisTitle.y == "") axisTitle.y <- NULL
  if (!is.null(title) && title == "") title <- NULL


  # create data frame

  # check whether we have grouping variable
  if (is.null(grp)) {
    # if not, add a dummy grouping variable
    grp <- rep(1, length(x))
    # we don't need legend here
    show.legend <- FALSE
  }

  # get value labels from attribute
  grl <- sjlabelled::get_labels(grp, attr.only = T)

  # simple data frame
  dat <- stats::na.omit(data.frame(x = x, y = y, grp = grp))

  # group as factor
  dat$grp <- as.factor(dat$grp)

  # set labelled levels, for facets
  if (grid && !is.null(grl)) levels(dat$grp) <- grl

  # do we have point labels?
  if (!is.null(dot.labels)) {
    # check length
    if (length(dot.labels) > nrow(dat)) {
      # Tell user that we have too many point labels
      warning("More point labels than data points. Omitting remaining point labels", call. = F)
      # shorten vector
      dot.labels <- dot.labels[seq_len(nrow(dat))]
    } else if (length(dot.labels) < nrow(dat)) {
      # Tell user that we have too less point labels
      warning("Less point labels than data points. Omitting remaining data point", call. = F)
      # shorten data frame
      dat <- dat[seq_len(length(dot.labels)), ]
    }
    # append labels
    dat$dot.lab <- as.character(dot.labels)
  }

  # fix and wrap labels and titles

  if (is.null(legend.labels)) legend.labels <- as.character(sort(unique(dat$grp)))
  legend.labels <- sjmisc::word_wrap(legend.labels, wrap.legend.labels)

  if (!is.null(legend.title)) legend.title <- sjmisc::word_wrap(legend.title, wrap.legend.title)
  if (!is.null(title)) title <- sjmisc::word_wrap(title, wrap.title)
  if (!is.null(axisTitle.x)) axisTitle.x <- sjmisc::word_wrap(axisTitle.x, wrap.title)
  if (!is.null(axisTitle.y)) axisTitle.y <- sjmisc::word_wrap(axisTitle.y, wrap.title)

  # Plot scatter plot

  scp <- ggplot(dat, aes_string(x = "x", y = "y", colour = "grp"))


  # add marginal rug

  if (show.rug) {
    scp <- scp + geom_rug(position = position_jitter(width = jitter))
  }

  # add data points

  if (emph.dots) {
    # indicate overlapping dots by point size
    scp <- scp + geom_count(show.legend = F, position = position_jitter(width = jitter))
  } else {
    # else plot dots
    scp <- scp + geom_jitter(size = dot.size, position = position_jitter(width = jitter))
  }


  # add labels

  if (!is.null(dot.labels)) {
    scp <- scp +
      ggrepel::geom_text_repel(aes_string(label = "dot.lab"), size = label.size)

  }


  # Show fitted lines

  if (!is.null(fit.grps)) {
    scp <- scp +
      stat_smooth(data = dat, aes_string(colour = "grp"), method = fit.grps, se = show.ci)
  }

  if (!is.null(fit.line)) {
    scp <- scp +
      stat_smooth(method = fit.line, se = show.ci, colour = "black")
  }


  # set font size for axes.

  scp <- scp +
    labs(title = title, x = axisTitle.x, y = axisTitle.y, colour = legend.title)


  # facet plot

  if (grid) scp <- scp + facet_wrap(~grp)

  sj.setGeomColors(
    scp,
    colors,
    length(legend.labels),
    show.legend,
    legend.labels
  )
}
