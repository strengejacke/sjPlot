#' @title Plot grouped or stacked frequencies
#' @name plot_grpfrq
#'
#' @description Plot grouped or stacked frequencies of variables as bar/dot,
#'                box or violin plots, or line plot.
#'
#' @param var.cnt Vector of counts, for which frequencies or means will be plotted or printed.
#' @param var.grp Factor with the cross-classifying variable, where \code{var.cnt}
#'          is grouped into the categories represented by \code{var.grp}.
#' @param weight.by Vector of weights that will be applied to weight all cases.
#'          Must be a vector of same length as the input vector. Default is
#'          \code{NULL}, so no weights are used.
#' @param title.wtd.suffix Suffix (as string) for the title, if \code{weight.by} is specified,
#'          e.g. \code{title.wtd.suffix=" (weighted)"}. Default is \code{NULL}, so
#'          title will not have a suffix when cases are weighted.
#' @param intr.var An interaction variable which can be used for box plots. Divides each category indicated
#'          by \code{var.grp} into the factors of \code{intr.var}, so that each category of \code{var.grp}
#'          is subgrouped into \code{intr.var}'s categories. Only applies when
#'          \code{type = "boxplot"} or \code{type = "violin"}.
#' @param bar.pos Indicates whether bars should be positioned side-by-side (default),
#'          or stacked (\code{bar.pos = "stack"}). May be abbreviated.
#' @param type Specifies the plot type. May be abbreviated.
#'          \describe{
#'            \item{\code{"bar"}}{for simple bars (default)}
#'            \item{\code{"dot"}}{for a dot plot}
#'            \item{\code{"histogram"}}{for a histogram (does not apply to grouped frequencies)}
#'            \item{\code{"line"}}{for a line-styled histogram with filled area}
#'            \item{\code{"density"}}{for a density plot (does not apply to grouped frequencies)}
#'            \item{\code{"boxplot"}}{for box plot}
#'            \item{\code{"violin"}}{for violin plots}
#'            }
#' @param show.legend logical, if \code{TRUE}, and depending on plot type and
#'          function, a legend is added to the plot.
#' @param ylim numeric vector of length two, defining lower and upper axis limits
#'          of the y scale. By default, this argument is set to \code{NULL}, i.e. the
#'          y-axis fits to the required range of the data.
#' @param facet.grid \code{TRUE} to arrange the lay out of of multiple plots
#'          in a grid of an integrated single plot. This argument calls
#'          \code{\link[ggplot2]{facet_wrap}} or \code{\link[ggplot2]{facet_grid}}
#'          to arrange plots. Use \code{\link{plot_grid}} to plot multiple plot-objects
#'          as an arranged grid with \code{\link[gridExtra]{grid.arrange}}.
#' @param title character vector, used as plot title. Depending on plot type and function,
#'          will be set automatically. If \code{title = ""}, no title is printed.
#'          For effect-plots, may also be a character vector of length > 1,
#'          to define titles for each sub-plot or facet.
#' @param legend.title character vector, used as title for the plot legend.
#' @param axis.labels character vector with labels used as axis labels. Optional
#'          argument, since in most cases, axis labels are set automatically.
#' @param intr.var.labels a character vector with labels for the x-axis breaks
#'          when having interaction variables included.
#'          These labels replace the \code{axis.labels}. Only applies, when using box or violin plots
#'          (i.e. \code{type = "boxplot"} or \code{"violin"}) and \code{intr.var} is not \code{NULL}.
#' @param legend.labels character vector with labels for the guide/legend.
#' @param wrap.title numeric, determines how many chars of the plot title are displayed in
#'          one line and when a line break is inserted.
#' @param wrap.labels numeric, determines how many chars of the value, variable or axis
#'          labels are displayed in one line and when a line break is inserted.
#' @param wrap.legend.title numeric, determines how many chars of the legend's title
#'          are displayed in one line and when a line break is inserted.
#' @param wrap.legend.labels numeric, determines how many chars of the legend labels are
#'          displayed in one line and when a line break is inserted.
#' @param grid.breaks numeric; sets the distance between breaks for the axis,
#'          i.e. at every \code{grid.breaks}'th position a major grid is being printed.
#' @param inner.box.width width of the inner box plot that is plotted inside of violin plots. Only applies
#'          if \code{type = "violin"}. Default value is 0.15
#' @param inner.box.dotsize size of mean dot insie a violin or box plot. Applies only
#'          when \code{type = "violin"} or \code{"boxplot"}.
#' @param geom.colors user defined color for geoms. See 'Details' in \code{\link{plot_grpfrq}}.
#' @param geom.size size resp. width of the geoms (bar width, line thickness or point size,
#'          depending on plot type and function). Note that bar and bin widths mostly
#'          need smaller values than dot sizes.
#' @param geom.spacing the spacing between geoms (i.e. bar spacing)
#' @param smooth.lines prints a smooth line curve. Only applies, when argument \code{type = "line"}.
#' @param expand.grid logical, if \code{TRUE}, the plot grid is expanded, i.e. there is a small margin between
#'          axes and plotting region. Default is \code{FALSE}.
#' @param show.values Logical, whether values should be plotted or not.
#' @param show.n logical, if \code{TRUE}, adds total number of cases for each
#'          group or category to the labels.
#' @param show.axis.values logical, whether category, count or percentage values for the axis
#'          should be printed or not.
#' @param show.prc logical, if \code{TRUE} (default), percentage values are plotted to each bar
#'          If \code{FALSE}, percentage values are removed.
#' @param show.ci Logical, if \code{TRUE)}, adds notches to the box plot, which are
#'          used to compare groups; if the notches of two boxes do not overlap,
#'          medians are considered to be significantly different.
#' @param emph.dots logical, if \code{TRUE}, the groups of dots in a dot-plot are highlighted
#'          with a shaded rectangle.
#' @param show.summary logical, if \code{TRUE} (default), a summary with chi-squared
#'          statistics (see \code{\link{chisq.test}}), Cramer's V or Phi-value etc.
#'          is shown. If a cell contains expected values lower than five (or lower than 10
#'          if df is 1), the Fisher's exact test (see \code{\link{fisher.test}}) is
#'          computed instead of chi-squared test. If the table's matrix is larger
#'          than 2x2, Fisher's exact test with Monte Carlo simulation is computed.
#' @param show.grpcnt logical, if \code{TRUE}, the count within each group is added
#'          to the category labels (e.g. \code{"Cat 1 (n=87)"}). Default value is \code{FALSE}.
#' @param summary.pos position of the model summary which is printed when \code{show.summary}
#'          is \code{TRUE}. Default is \code{"r"}, i.e. it's printed to the upper right corner.
#'          Use \code{"l"} for upper left corner.
#' @param axis.titles character vector of length one or two, defining the title(s)
#'          for the x-axis and y-axis.
#' @param drop.empty Logical, if \code{TRUE} and the variable's values are labeled, values / factor
#'          levels with no occurrence in the data are omitted from the output. If \code{FALSE},
#'          labeled values that have no observations are still printed in the table (with frequency \code{0}).
#' @param auto.group numeric value, indicating the minimum amount of unique values
#'          in the count variable, at which automatic grouping into smaller units
#'          is done (see \code{\link[sjmisc]{group_var}}). Default value for
#'          \code{auto.group} is \code{NULL}, i.e. auto-grouping is off.
#'          See \code{\link[sjmisc]{group_var}} for examples on grouping.
#' @param coord.flip logical, if \code{TRUE}, the x and y axis are swapped.
#' @param vjust character vector, indicating the vertical position of value
#'          labels. Allowed are same values as for \code{vjust} aesthetics from
#'          \code{ggplot2}: "left", "center", "right", "bottom", "middle", "top" and
#'          new options like "inward" and "outward", which align text towards and
#'          away from the center of the plot respectively.
#' @param hjust character vector, indicating the horizontal position of value
#'          labels. Allowed are same values as for \code{vjust} aesthetics from
#'          \code{ggplot2}: "left", "center", "right", "bottom", "middle", "top" and
#'          new options like "inward" and "outward", which align text towards and
#'          away from the center of the plot respectively.
#' @param y.offset numeric, offset for text labels when their alignment is adjusted
#'          to the top/bottom of the geom (see \code{hjust} and \code{vjust}).
#' @param show.na logical, if \code{TRUE}, \code{\link{NA}}'s (missing values)
#'          are added to the output.
#'
#' @return A ggplot-object.
#'
#' @details \code{geom.colors} may be a character vector of color values
#'          in hex-format, valid color value names (see \code{demo("colors")} or
#'          a name of a \href{ https://colorbrewer2.org/}{color brewer} palette.
#'          Following options are valid for the \code{geom.colors} argument:
#'          \itemize{
#'            \item If not specified, a default color brewer palette will be used, which is suitable for the plot style (i.e. diverging for likert scales, qualitative for grouped bars etc.).
#'            \item If \code{"gs"}, a greyscale will be used.
#'            \item If \code{"bw"}, and plot-type is a line-plot, the plot is black/white and uses different line types to distinguish groups (see \href{https://strengejacke.github.io/sjPlot/articles/blackwhitefigures.html}{this package-vignette}).
#'            \item If \code{geom.colors} is any valid color brewer palette name, the related palette will be used. Use \code{RColorBrewer::display.brewer.all()} to view all available palette names.
#'            \item Else specify own color values or names as vector (e.g. \code{geom.colors = c("#f00000", "#00ff00")}).
#'          }
#'
#' @examples
#' data(efc)
#' plot_grpfrq(efc$e17age, efc$e16sex, show.values = FALSE)
#'
#' # boxplot
#' plot_grpfrq(efc$e17age, efc$e42dep, type = "box")
#'
#' # grouped bars
#' plot_grpfrq(efc$e42dep, efc$e16sex, title = NULL)
#'
#' # box plots with interaction variable
#' plot_grpfrq(efc$e17age, efc$e42dep, intr.var = efc$e16sex, type = "box")
#'
#' # Grouped bar plot
#' plot_grpfrq(efc$neg_c_7, efc$e42dep, show.values = FALSE)
#'
#' # same data as line plot
#' plot_grpfrq(efc$neg_c_7, efc$e42dep, type = "line")
#'
#' # show ony categories where we have data (i.e. drop zero-counts)
#' library(dplyr)
#' efc <- dplyr::filter(efc, e42dep %in% c(3,4))
#' plot_grpfrq(efc$c161sex, efc$e42dep, drop.empty = TRUE)
#'
#' # show all categories, even if not in data
#' plot_grpfrq(efc$c161sex, efc$e42dep, drop.empty = FALSE)
#'
#' @import ggplot2
#' @importFrom rlang .data
#' @export
plot_grpfrq <- function(var.cnt,
                       var.grp,
                       type = c("bar", "dot", "line", "boxplot", "violin"),
                       bar.pos = c("dodge", "stack"),
                       weight.by = NULL,
                       intr.var = NULL,
                       title = "",
                       title.wtd.suffix = NULL,
                       legend.title = NULL,
                       axis.titles = NULL,
                       axis.labels = NULL,
                       legend.labels = NULL,
                       intr.var.labels = NULL,
                       wrap.title = 50,
                       wrap.labels = 15,
                       wrap.legend.title = 20,
                       wrap.legend.labels = 20,
                       geom.size = NULL,
                       geom.spacing = 0.15,
                       geom.colors = "Paired",
                       show.values = TRUE,
                       show.n = TRUE,
                       show.prc = TRUE,
                       show.axis.values = TRUE,
                       show.ci = FALSE,
                       show.grpcnt = FALSE,
                       show.legend = TRUE,
                       show.na = FALSE,
                       show.summary = FALSE,
                       drop.empty = TRUE,
                       auto.group = NULL,
                       ylim = NULL,
                       grid.breaks = NULL,
                       expand.grid = FALSE,
                       inner.box.width = 0.15,
                       inner.box.dotsize = 3,
                       smooth.lines = FALSE,
                       emph.dots = TRUE,
                       summary.pos = "r",
                       facet.grid = FALSE,
                       coord.flip = FALSE,
                       y.offset = NULL,
                       vjust = "bottom",
                       hjust = "center") {

  # get variable names
  var.name.cnt <- get_var_name(deparse(substitute(var.cnt)))
  var.name.grp <- get_var_name(deparse(substitute(var.grp)))

  # remove empty value-labels
  if (drop.empty) {
    var.cnt <- sjlabelled::drop_labels(var.cnt)
    var.grp <- sjlabelled::drop_labels(var.grp)
  }

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

  # match arguments
  type <- match.arg(type)
  bar.pos <- match.arg(bar.pos)

  # turn off legend by default for facet grids
  if (facet.grid && missing(show.legend)) show.legend <- FALSE

  # Plot margins
  if (expand.grid)
    expand.grid <- waiver()
  else
    expand.grid <- c(0, 0)

  # check default geom.size
  if (is.null(geom.size)) {
    geom.size <- dplyr::case_when(
      type == "bar" ~ .7,
      type == "dot" ~ 3,
      type == "line" ~ .8,
      type == "boxplot" ~ .5,
      type == "violin" ~ .6,
      TRUE ~ .7
    )
  }

  # set text label offset
  if (is.null(y.offset)) {
    # get maximum y-pos
    y.offset <- ceiling(max(table(var.cnt, var.grp)) / 100)

    if (coord.flip) {
      if (missing(vjust)) vjust <- "center"
      if (missing(hjust)) hjust <- "bottom"

      # for flipped coordinates, we need to adjust
      # y-offset according to horizontal adjustemnt of labels
      if (hjust == "bottom")
        y_offset <- y.offset
      else if (hjust == "top")
        y_offset <- -y.offset
      else
        y_offset <- 0
    } else {
      # for non-flipped coordinates, we need to adjust
      # y-offset according to vertical adjustemnt of labels
      if (vjust == "bottom")
        y_offset <- y.offset
      else if (vjust == "top")
        y_offset <- -y.offset
      else
        y_offset <- 0
    }
  } else {
    y_offset <- y.offset
  }

  # Interaction variable defined for invalid plot type?
  if (!is.null(intr.var) && type != "boxplot" && type != "violin") {
    message("`intr.var` only applies to boxplots and violinplots (see `type`) and will be ignored.")
  }

  if (show.grpcnt && type %in% c("boxplot", "violin")) {
    message("`show.grpcnt` does not apply to boxplots and violinplots and will be ignored.")
    show.grpcnt <- FALSE
  }

  # auto-set plot title for box plots?
  if (missing(title) && (type == "boxplot" || type == "violin")) title <- NULL

  # check whether variable should be auto-grouped
  if (!is.null(auto.group) && length(unique(var.cnt)) >= auto.group) {
    message(sprintf(
      "%s has %i unique values and was grouped...",
      var.name.cnt,
      length(unique(var.cnt))
    ))

    # check for default auto-group-size or user-defined groups
    agcnt <- ifelse(auto.group < 30, auto.group, 30)

    # group axis labels
    axis.labels <-
      sjmisc::group_labels(
        sjmisc::to_value(var.cnt, keep.labels = F),
        size = "auto",
        n = agcnt
      )

    # group variable
    grp.var.cnt <-
      sjmisc::group_var(
        sjmisc::to_value(var.cnt, keep.labels = F),
        size = "auto",
        as.num = TRUE,
        n = agcnt,
        append = FALSE
      )

    # set value labels
    grp.var.cnt <- sjlabelled::set_labels(grp.var.cnt, labels = axis.labels)
  } else {
    grp.var.cnt <- var.cnt
  }

  # create cross table of frequencies and percentages
  mydat <-
    create.xtab.df(
      grp.var.cnt,
      var.grp,
      round.prz = 2,
      na.rm = !show.na,
      weight.by = weight.by
    )

  # x-position as numeric factor, added later after
  # tidying
  bars.xpos <- seq_len(nrow(mydat$mydat))

  # try to automatically set labels if not passed as argument
  if (missing(axis.labels) && (type == "boxplot" || type == "violin")) {
    axis.labels <- mydat$labels.grp
    # if we have interaction variable, legend should be shown by default,
    # unless explicitely set to FALSE
    if (missing(show.legend)) show.legend <- !is.null(intr.var)
  }

  if (is.null(axis.labels)) axis.labels <- mydat$labels.cnt

  # we need to know later whether user has supplied legend labels or not
  we_have_legend_labels <- FALSE

  # check for auto-getting labels, ot if user passed legend labels as argument
  if (is.null(legend.labels))
    legend.labels <- mydat$labels.grp
  else
    we_have_legend_labels <- TRUE

  # go to interaction terms. in this case, due to interaction, the axis
  # labels become legend labels, but only if user has not specified
  # legend labels yet. In the latter case, leave legend labels unchanged.
  if (is.null(intr.var.labels) && !is.null(intr.var)) {
    intr.var.labels <- sjlabelled::get_labels(
      intr.var,
      attr.only = F,
      values = F,
      non.labelled = T
    )

    # create repeating label for x-axis
    intr.var.labels <- rep(intr.var.labels, length.out = length(axis.labels) * length(intr.var.labels))

    # we need a legend, cause x axis is labelled with interaction var value
    show.legend <- TRUE

    # has user specified legend labels before?
    if (!we_have_legend_labels) legend.labels <- axis.labels
  }

  if (is.null(axisTitle.x)) axisTitle.x <- sjlabelled::get_label(var.cnt, def.value = var.name.cnt)
  if (is.null(legend.title)) legend.title <- sjlabelled::get_label(var.grp, def.value = var.name.grp)

  if (is.null(title)) {
    t1 <- sjlabelled::get_label(var.cnt, def.value = var.name.cnt)
    t2 <- sjlabelled::get_label(var.grp, def.value = var.name.grp)
    if (!is.null(t1) && !is.null(t2)) title <- paste0(t1, " by ", t2)
  }

  # remove titles if empty
  if (!is.null(legend.title) && legend.title == "") legend.title <- NULL
  if (!is.null(axisTitle.x) && axisTitle.x == "") axisTitle.x <- NULL
  if (!is.null(axisTitle.y) && axisTitle.y == "") axisTitle.y <- NULL
  if (!is.null(title) && title == "") title <- NULL

  # variables may not be factors
  if (anyNA(as.numeric(stats::na.omit(var.cnt))))
    var.cnt <- sjmisc::to_value(var.cnt, keep.labels = F)
  else
    var.cnt <- as.numeric(var.cnt)

  if (anyNA(as.numeric(stats::na.omit(var.grp))))
    var.grp <- sjmisc::to_value(var.grp, keep.labels = F)
  else
    var.grp <- as.numeric(var.grp)

  # Define amount of categories
  grpcount <- length(legend.labels)

  # create cross table for stats, summary etc.
  # and weight variable
  colrange <- 2:(grpcount + 1)
  mydf <-
    tidyr::gather(mydat$mydat, key = "group", value = "frq", !! colrange, factor_key = TRUE)

  # add xpos now
  mydf$xpos <- as.factor(as.numeric(bars.xpos))

  # add half of Percentage values as new y-position for stacked bars
  # mydat <- ddply(mydat, "count", transform, ypos = cumsum(frq) - 0.5*frq)
  mydf <- mydf %>%
    dplyr::group_by(.data$label) %>%
    dplyr::mutate(ypos = cumsum(.data$frq) - 0.5 * .data$frq) %>%
    dplyr::arrange(.data$label)

  # add percentages
  mydf$prz <- round(100 * mydf$frq / sum(mydf$frq), 2)

  # If we have boxplots, use different data frame structure
  if (type == "boxplot" || type == "violin") {
    # weight variable
    w <- ifelse(is.null(weight.by), 1, weight.by)

    # interaction variable
    if (is.null(intr.var))
      iav <- 1
    else
      iav <- intr.var

    # new data frame for box plots
    mydf <-
      stats::na.omit(data_frame(cbind(
        group = var.grp,
        frq = var.cnt,
        ia = iav,
        wb = w
      )))

    if (!is.null(axis.labels) &&
        length(axis.labels) > dplyr::n_distinct(mydf$group, na.rm = TRUE)) {
      axis.labels <- axis.labels[na.omit(unique(mydf$group))]
    }

    mydf$ia <- as.factor(mydf$ia)
    mydf$group <- as.factor(mydf$group)
  }

  # create expression with model summarys. used
  # for plotting in the diagram later
  mannwhitneyu <- function(count, grp) {
    if (min(grp, na.rm = TRUE) == 0) grp <- grp + 1
    completeString <- ""
    cnt <- length(unique(stats::na.omit(grp)))
    for (i in 1:cnt) {
      for (j in i:cnt) {
        if (i != j) {
          xsub <- count[which(grp == i | grp == j)]
          ysub <- grp[which(grp == i | grp == j)]
          ysub <- ysub[which(!is.na(xsub))]
          xsub <- as.numeric(stats::na.omit(xsub))
          ysub <- as.numeric(stats::na.omit(ysub))
          wt <- stats::wilcox.test(xsub ~ ysub)

          if (wt$p.value < 0.001) {
            modsum <- as.character(as.expression(substitute(
              p[pgrp] < pval, list(pgrp = sprintf("(%i|%i)", i, j), pval = 0.001)
            )))
          } else {
            modsum <- as.character(as.expression(substitute(
              p[pgrp] == pval,
              list(pgrp = sprintf("(%i|%i)", i, j),
                   pval = sprintf("%.3f", wt$p.value)))))
          }
          completeString <- sprintf("%s * \",\" ~ ~ %s",
                                    completeString,
                                    modsum)
        }
      }
    }
    return(paste("\"Mann-Whitney-U:\" ~ ~ ",
                 substring(completeString, 12),
                 sep = ""))
  }

  # Check whether table summary should be printed
  modsum <- NULL
  if (show.summary) {
    if (type == "boxplot" || type == "violin")
      modsum <- mannwhitneyu(var.cnt, var.grp)
    else
      modsum <- crosstabsum(var.cnt, var.grp, weight.by)
  }

  # Prepare and trim legend labels to appropriate size
  if (!is.null(legend.labels))
    legend.labels <- sjmisc::word_wrap(legend.labels, wrap.legend.labels)

  if (!is.null(legend.title))
    legend.title <- sjmisc::word_wrap(legend.title, wrap.legend.title)

  if (!is.null(title)) {
    # if we have weighted values, say that in diagram's title
    if (!is.null(title.wtd.suffix))
      title <- paste(title, title.wtd.suffix, sep = "")
    title <- sjmisc::word_wrap(title, wrap.title)
  }

  if (!is.null(axisTitle.x))
    axisTitle.x <- sjmisc::word_wrap(axisTitle.x, wrap.title)

  if (!is.null(axisTitle.y))
    axisTitle.y <- sjmisc::word_wrap(axisTitle.y, wrap.title)

  if (!is.null(axis.labels))
    axis.labels <- sjmisc::word_wrap(axis.labels, wrap.labels)

  if (!is.null(intr.var)) {
    if (!is.null(intr.var.labels)) {
      intr.var.labels <- sjmisc::word_wrap(intr.var.labels, wrap.labels)
    }
    # If interaction-variable-labels were not defined, simply set numbers from 1 to
    # amount of categories instead
    else {
      iavarLabLength <- length(unique(stats::na.omit(intr.var)))
      intr.var.labels <- 1:iavarLabLength
    }
  }

  # add group counts to category labels
  if (show.grpcnt) {
    nas <- ifelse(isTRUE(show.na), "ifany", "no")
    # check whether we have interaction variables or not
    if (!is.null(intr.var.labels)) {
      # retrieve group counts by converting data column
      # into table
      if (is.null(weight.by)) {
        gc <- table(var.grp, intr.var, useNA = nas)
      } else {
        gc <- table(sjstats::weight2(var.grp, weight.by), intr.var, useNA = nas)
      }
      # determinte loop-steps
      lst <- length(intr.var.labels)
      # iterate category labels
      for (i in seq_len(lst)) {
        # remember original label
        ial <- intr.var.labels[i]
        # add group count to each cat. label
        intr.var.labels[i] <- paste(ial, " (n=", gc[1, i], ")", sep = "")
        intr.var.labels[i + lst] <- paste(ial, " (n=", gc[2, i], ")", sep = "")
      }
    } else {
      sums <- unname(rowSums(mydat$mydat[, -1]))
      # add group count to each cat. label
      axis.labels <- paste(axis.labels, " (n=", sums, ")", sep = "")
      sums <- unname(colSums(mydat$mydat[, -1]))
      # add group count to each cat. label
      legend.labels <- paste(legend.labels, " (n=", sums, ")", sep = "")
    }
  }

  # Prepare bar charts
  trimViolin <- FALSE
  lower_lim <- 0

  # calculate upper y-axis-range
  # if we have a fixed value, use this one here
  if (!is.null(ylim) && length(ylim) == 2) {
    lower_lim <- ylim[1]
    upper_lim <- ylim[2]
  } else {
    # if we have boxplots, we have different ranges, so we can adjust
    # the y axis
    if (type == "boxplot" || type == "violin") {
      # use an extra standard-deviation as limits for the y-axis when we have boxplots
      lower_lim <- min(var.cnt, na.rm = TRUE) - floor(stats::sd(var.cnt, na.rm = TRUE))
      upper_lim <- max(var.cnt, na.rm = TRUE) + ceiling(stats::sd(var.cnt, na.rm = TRUE))
      # make sure that the y-axis is not below zero
      if (lower_lim < 0) {
        lower_lim <- 0
        trimViolin <- TRUE
      }
      # else calculate upper y-axis-range depending
      # on the amount of cases...
    } else if (bar.pos == "stack") {
      upper_lim <- max(pretty(table(grp.var.cnt) * 1.05))
    } else {
      # ... or the amount of max. answers per category
      upper_lim <- max(pretty(table(grp.var.cnt, var.grp) * 1.05))
    }
  }

  # align dodged position of labels to bar positions
  if (type == "line")
    posdodge <- 0
  else if (type == "dot")
    posdodge <- geom.spacing
  else
    posdodge <- geom.size + geom.spacing

  # init shaded rectangles for plot
  ganno <- NULL

  # check whether we have dots or bars
  if (type == "dot") {
    # position_dodge displays dots in a dodged position so we avoid overlay here. This may lead
    # to a more difficult distinction of group belongings, since the dots are "horizontally spread"
    # over the digram. For a better overview, we can add a "PlotAnnotation" (see "emph.dots) here.
    geob <- geom_point(position = position_dodge(posdodge),size = geom.size, shape = 16)

    # create shaded rectangle, so we know which dots belong to the same category
    if (emph.dots) {
      ganno <- annotate(
        "rect",
        xmin = as.numeric(mydf$xpos) - 0.4,
        xmax = as.numeric(mydf$xpos) + 0.4,
        ymin = lower_lim,
        ymax = upper_lim,
        fill = "grey80",
        alpha = 0.1
      )
    }
  } else if (type == "bar") {
    if (bar.pos == "dodge")
      geob <- geom_bar(stat = "identity", width = geom.size, position = position_dodge(posdodge))
    else
      geob <- geom_bar(stat = "identity", width = geom.size, position = position_stack(reverse = TRUE))
  } else if (type == "line") {
    if (smooth.lines)
      geob <- geom_line(linewidth = geom.size, stat = "smooth", method = "loess")
    else
      geob <- geom_line(linewidth = geom.size)
  } else if (type == "boxplot") {
      geob <- geom_boxplot(width = geom.size, notch = show.ci)
  } else if (type == "violin") {
    geob <- geom_violin(trim = trimViolin, width = geom.size)
  } else {
    geob <- geom_bar(stat = "identity", position = bar.pos, width = geom.size)
  }

  # don't display value labels when we have boxplots or violin plots
  if (type == "boxplot" || type == "violin") show.values <- FALSE

  if (show.values) {
    # set text positioning
    if (facet.grid)
      text.pos <- "identity"
    else
      text.pos <- position_dodge(posdodge)

    # if we have stacked bars, we need to apply
    # this stacked y-position to the labels as well
    if (bar.pos == "stack") {
      if (show.prc && show.n) {
        ggvaluelabels <-
          geom_text(aes(y = .data$ypos, label = sprintf("%i\n(%.01f%%)", .data$frq, .data$prz)), show.legend = FALSE)
      } else if (show.n) {
        ggvaluelabels <-
          geom_text(aes(y = .data$ypos, label = sprintf("%i", .data$frq)), show.legend = FALSE)
      } else if (show.prc) {
        ggvaluelabels <-
          geom_text(aes(y = .data$ypos, label = sprintf("%.01f%%", .data$prz)), show.legend = FALSE)
      } else {
        ggvaluelabels <- geom_text(aes(y = .data$frq), label = "", show.legend = FALSE)
      }
    } else {
      # if we have dodged bars or dots, we have to use a slightly
      # dodged position for labels
      # as well, sofor better reading
      if (show.prc && show.n) {
        if (coord.flip) {
          ggvaluelabels <-
            geom_text(
              aes(y = .data$frq + y_offset, label = sprintf("%i (%.01f%%)", .data$frq, .data$prz)),
              position = text.pos,
              vjust = vjust,
              hjust = hjust,
              show.legend = FALSE
            )
        } else {
          ggvaluelabels <-
            geom_text(
              aes(y = .data$frq + y_offset, label = sprintf("%i\n(%.01f%%)", .data$frq, .data$prz)),
              position = text.pos,
              vjust = vjust,
              hjust = hjust,
              show.legend = FALSE
            )
        }
      } else if (show.n) {
        ggvaluelabels <-
          geom_text(
            aes(y = .data$frq + y_offset, label = sprintf("%i", .data$frq)),
            position = text.pos,
            hjust = hjust,
            vjust = vjust,
            show.legend = FALSE
          )
      } else if (show.prc) {
        ggvaluelabels <-
          geom_text(
            aes(y = .data$frq + y_offset, label = sprintf("%.01f%%", .data$prz)),
            position = text.pos,
            hjust = hjust,
            vjust = vjust,
            show.legend = FALSE
          )
      } else {
        ggvaluelabels <- geom_text(aes(y = .data$frq), label = "", show.legend = FALSE)
      }
    }
  } else {
    ggvaluelabels <- geom_text(aes(y = .data$frq), label = "", show.legend = FALSE)
  }

  # Set up grid breaks
  if (is.null(grid.breaks))
    gridbreaks <- waiver()
  else
    gridbreaks <- seq(lower_lim, upper_lim, by = grid.breaks)

  # Print plot
  if (type == "line") {
    # line plot need numeric x-scale
    mydf$xpos <- sjmisc::to_value(mydf$xpos, keep.labels = FALSE)

    # lines need colour aes
    baseplot <-
      ggplot(mydf,
             aes_string(
               x = "xpos",
               y = "frq",
               colour = "group",
               linetype = "group"
             )) + geob

    # continuous scale for lines needed
    scalex <- scale_x_continuous()
  } else if (type == "boxplot" || type == "violin") {
    if (is.null(intr.var)) {
      baseplot <-
        ggplot(mydf,
               aes_string(
                 x = "group",
                 y = "frq",
                 fill = "group",
                 weight = "wb"
               )) + geob
      scalex <- scale_x_discrete(labels = axis.labels)
    } else {
      baseplot <-
        ggplot(mydf, aes(
          x = interaction(.data$ia, .data$group),
          y = .data$frq,
          fill = .data$group,
          weight = .data$wb
        )) + geob
      scalex <- scale_x_discrete(labels = intr.var.labels)
    }

    # if we have a violin plot, add an additional boxplot inside to show
    # more information
    if (type == "violin") {
      if (show.ci) {
        baseplot <- baseplot +
          geom_boxplot(width = inner.box.width, fill = "white", outlier.colour = NA, notch = TRUE)
      } else {
        baseplot <- baseplot +
          geom_boxplot(width = inner.box.width, fill = "white", outlier.colour = NA)
      }
    }

    # if we have boxplots or violon plots, also add a point that indicates
    # the mean value
    # different fill colours, because violin boxplots have white background
    fcsp <- ifelse(type == "boxplot", "white", "black")
    baseplot <- baseplot +
      stat_summary(fun.y = "mean", geom = "point", shape = 21,
                   size = inner.box.dotsize, fill = fcsp)
  } else {
    if (type == "dot") {
      baseplot <- ggplot(mydf, aes_string(x = "xpos", y = "frq", colour = "group"))

      # check whether we have dots plotted, and if so, use annotation
      # We have to use annotation first, because the diagram's layers are plotted
      # in the order as they're passed to the ggplot-command. Since we don't want the
      # shaded rectangles to overlay the dots, we add them first
      if (!is.null(ganno) && !facet.grid) baseplot <- baseplot + ganno
    } else {
      baseplot <- ggplot(mydf, aes_string(x = "xpos", y = "frq", fill = "group"))
    }

    # add geom
    baseplot <- baseplot + geob

    # define x axis
    scalex <- scale_x_discrete(labels = axis.labels)
  }

  # If we have bars or dot plots, we show
  # Pearson's chi-square test results
  baseplot <- print.table.summary(baseplot, modsum, summary.pos)

  # prepare y-axis and
  # show or hide y-axis-labels
  if (show.axis.values) {
    y_scale <- scale_y_continuous(
      breaks = gridbreaks,
      limits = c(lower_lim, upper_lim),
      expand = expand.grid
    )
  } else {
    y_scale <- scale_y_continuous(
      breaks = gridbreaks,
      limits = c(lower_lim, upper_lim),
      expand = expand.grid,
      labels = NULL
    )
  }

  # continue with plot objects...
  baseplot <- baseplot +
    # show absolute and percentage values for each bar
    ggvaluelabels +
    # add labels to x- and y-axis, and diagram title
    labs(
      title = title,
      x = axisTitle.x,
      y = axisTitle.y,
      fill = legend.title,
      colour = legend.title
    ) +
    # print value labels to the x-axis.
    # If argument "axis.labels" is NULL, the category numbers (1 to ...)
    # appear on the x-axis
    scalex +
    # set Y-axis, depending on the calculated upper y-range.
    # It either corresponds to the maximum amount of cases in the data set
    # (length of var) or to the highest count of var's categories.
    y_scale

  # check whether coordinates should be flipped
  if (coord.flip) baseplot <- baseplot + coord_flip()

  # Here we start when we have a faces grid instead of
  # a grouped bar plot.
  if (facet.grid) {
    baseplot <- baseplot +
      # set font size for axes.
      # theme(strip.text = element_text(face = "bold", size = rel(1.1))) +
      facet_wrap(~group, scales = "free")
  }

  # set geom colors
  baseplot <-
    sj.setGeomColors(baseplot,
                     geom.colors,
                     length(legend.labels),
                     show.legend,
                     legend.labels)

  # Plot integrated bar chart here
  baseplot
}
