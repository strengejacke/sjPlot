utils::globalVariables("density")

#' @title Plot frequencies of variables
#' @name plot_frq
#'
#' @description Plot frequencies of a variable as bar graph, histogram, box plot etc.
#'
#' @note This function only works with variables with integer values (or numeric
#'         factor levels), i.e. scales / centered variables
#'         with fractional part may result in unexpected behaviour.
#'
#' @param ... Optional, unquoted names of variables that should be selected for
#'   further processing. Required, if \code{data} is a data frame (and no
#'   vector) and only selected variables from \code{data} should be processed.
#'   You may also use functions like \code{:} or tidyselect's
#'   select_helpers.
#' @param sort.frq Determines whether categories should be sorted
#'          according to their frequencies or not. Default is \code{"none"}, so
#'          categories are not sorted by frequency. Use \code{"asc"} or
#'          \code{"desc"} for sorting categories ascending or descending order.
#' @param geom.colors User defined color for geoms, e.g. \code{geom.colors = "#0080ff"}.
#' @param errorbar.color Color of confidence interval bars (error bars).
#'          Only applies to \code{type = "bar"}. In case of dot plots, error bars
#'          will have same colors as dots (see \code{geom.colors}).
#' @param show.mean Logical, if \code{TRUE}, a vertical line in histograms
#'          is drawn to indicate the mean value of the variables. Only
#'          applies to histogram-charts.
#' @param show.mean.val Logical, if \code{TRUE} (default), the mean value
#'          is printed to the vertical line that indicates the variable's
#'          mean. Only applies to histogram-charts.
#' @param show.sd Logical, if \code{TRUE}, the standard deviation
#'          is annotated as shaded rectangle around the mean intercept
#'          line. Only applies to histogram-charts.
#' @param mean.line.type Numeric value, indicating the linetype of the mean
#'          intercept line. Only applies to histogram-charts and
#'          when \code{show.mean = TRUE}.
#' @param mean.line.size Numeric, size of the mean intercept line. Only
#'          applies to histogram-charts and when \code{show.mean = TRUE}.
#' @param normal.curve Logical, if \code{TRUE}, a normal curve, which is adjusted to the data,
#'          is plotted over the histogram or density plot. Default is
#'          \code{FALSE}. Only applies when histograms or density plots are plotted (see \code{type}).
#' @param normal.curve.color Color of the normal curve line. Only
#'          applies if \code{normal.curve = TRUE}.
#' @param normal.curve.size Numeric, size of the normal curve line. Only
#'          applies if \code{normal.curve = TRUE}.
#' @param normal.curve.alpha Transparancy level (alpha value) of the normal curve. Only
#'          applies if \code{normal.curve = TRUE}.
#' @param xlim Numeric vector of length two, defining lower and upper axis limits
#'          of the x scale. By default, this argument is set to \code{NULL}, i.e. the
#'          x-axis fits to the required range of the data.
#' @param axis.title Character vector of length one or two (depending on
#'          the plot function and type), used as title(s) for the x and y axis.
#'          If not specified, a default labelling  is chosen.
#'          \strong{Note:} Some plot types do not support this argument. In such
#'          cases, use the return value and add axis titles manually with
#'          \code{\link[ggplot2]{labs}}, e.g.: \code{$plot.list[[1]] + labs(x = ...)}
#'
#' @inheritParams plot_scatter
#' @inheritParams plot_grpfrq
#' @inheritParams tab_xtab
#'
#' @return A ggplot-object.
#'
#' @examples
#' library(sjlabelled)
#' data(efc)
#' data(iris)
#'
#' # simple plots, two different notations
#' plot_frq(iris, Species)
#' plot_frq(efc$tot_sc_e)
#'
#' # boxplot
#' plot_frq(efc$e17age, type = "box")
#'
#' if (require("dplyr")) {
#'   # histogram, pipe-workflow
#'   efc %>%
#'     dplyr::select(e17age, c160age) %>%
#'     plot_frq(type = "hist", show.mean = TRUE)
#'
#'   # bar plot(s)
#'   plot_frq(efc, e42dep, c172code)
#' }
#'
#' if (require("dplyr") && require("gridExtra")) {
#'   # grouped data frame, all panels in one plot
#'   efc %>%
#'     group_by(e42dep) %>%
#'     plot_frq(c161sex) %>%
#'     plot_grid()
#' }
#'
#' \donttest{
#' library(sjmisc)
#' # grouped variable
#' ageGrp <- group_var(efc$e17age)
#' ageGrpLab <- group_labels(efc$e17age)
#' plot_frq(ageGrp, title = get_label(efc$e17age), axis.labels = ageGrpLab)
#'
#' # plotting confidence intervals. expand grid and v/hjust for text labels
#' plot_frq(
#'   efc$e15relat, type = "dot", show.ci = TRUE, sort.frq = "desc",
#'   coord.flip = TRUE, expand.grid = TRUE, vjust = "bottom", hjust = "left"
#' )
#'
#' # histogram with overlayed normal curve
#' plot_frq(efc$c160age, type = "h", show.mean = TRUE, show.mean.val = TRUE,
#'         normal.curve = TRUE, show.sd = TRUE, normal.curve.color = "blue",
#'         normal.curve.size = 3, ylim = c(0,50))
#' }
#' @import ggplot2
#' @importFrom sjstats weighted_sd
#' @importFrom sjmisc group_labels group_var to_value frq
#' @importFrom sjlabelled set_labels drop_labels
#' @importFrom stats na.omit sd weighted.mean dnorm
#' @importFrom rlang .data
#' @export
plot_frq <- function(data,
                     ...,
                    title = "",
                    weight.by = NULL,
                    title.wtd.suffix = NULL,
                    sort.frq = c("none", "asc", "desc"),
                    type = c("bar", "dot", "histogram", "line", "density", "boxplot", "violin"),
                    geom.size = NULL,
                    geom.colors = "#336699",
                    errorbar.color = "darkred",
                    axis.title = NULL,
                    axis.labels = NULL,
                    xlim = NULL,
                    ylim = NULL,
                    wrap.title = 50,
                    wrap.labels = 20,
                    grid.breaks = NULL,
                    expand.grid = FALSE,
                    show.values = TRUE,
                    show.n = TRUE,
                    show.prc = TRUE,
                    show.axis.values = TRUE,
                    show.ci = FALSE,
                    show.na = FALSE,
                    show.mean = FALSE,
                    show.mean.val = TRUE,
                    show.sd = TRUE,
                    drop.empty = TRUE,
                    mean.line.type = 2,
                    mean.line.size = 0.5,
                    inner.box.width = 0.15,
                    inner.box.dotsize = 3,
                    normal.curve = FALSE,
                    normal.curve.color = "red",
                    normal.curve.size = 0.8,
                    normal.curve.alpha = 0.4,
                    auto.group = NULL,
                    coord.flip = FALSE,
                    vjust = "bottom",
                    hjust = "center",
                    y.offset = NULL) {

  # Match arguments -----
  type <- match.arg(type)
  sort.frq <- match.arg(sort.frq)

  plot_data  <- get_dplyr_dot_data(data, dplyr::quos(...))

  if (!is.data.frame(plot_data)) {
    plot_data <- data.frame(plot_data, stringsAsFactors = FALSE)
    colnames(plot_data) <- deparse(substitute(data))
  }

  pl <- NULL

  if (inherits(plot_data, "grouped_df")) {
    # get grouped data
    grps <- get_grouped_data(plot_data)

    # now plot everything
    for (i in seq_len(nrow(grps))) {
      # copy back labels to grouped data frame
      tmp <- sjlabelled::copy_labels(grps$data[[i]], data)

      # prepare argument list, including title
      tmp.title <- get_grouped_plottitle(plot_data, grps, i, sep = "\n")

      # plot

      plots <- lapply(colnames(tmp), function(.d) {
        plot_frq_helper(
          var.cnt = tmp[[.d]], title = tmp.title, weight.by = weight.by, title.wtd.suffix, sort.frq, type, geom.size, geom.colors,
          errorbar.color, axis.title, axis.labels, xlim, ylim, wrap.title, wrap.labels, grid.breaks,
          expand.grid, show.values, show.n, show.prc, show.axis.values, show.ci, show.na,
          show.mean, show.mean.val, show.sd, drop.empty, mean.line.type, mean.line.size,
          inner.box.width, inner.box.dotsize, normal.curve, normal.curve.color,
          normal.curve.size, normal.curve.alpha, auto.group, coord.flip, vjust,
          hjust, y.offset, var.name = .d
        )
      })

      # add plots, check for NULL results
      pl <- c(pl, plots)
    }
  } else {
    pl <- lapply(colnames(plot_data), function(.d) {
      plot_frq_helper(
        var.cnt = plot_data[[.d]], title, weight.by = weight.by, title.wtd.suffix, sort.frq, type, geom.size, geom.colors,
        errorbar.color, axis.title, axis.labels, xlim, ylim, wrap.title, wrap.labels, grid.breaks,
        expand.grid, show.values, show.n, show.prc, show.axis.values, show.ci, show.na,
        show.mean, show.mean.val, show.sd, drop.empty, mean.line.type, mean.line.size,
        inner.box.width, inner.box.dotsize, normal.curve, normal.curve.color,
        normal.curve.size, normal.curve.alpha, auto.group, coord.flip, vjust,
        hjust, y.offset, var.name = .d
      )
    })

    if (length(pl) == 1) pl <- pl[[1]]
  }

  pl
}


plot_frq_helper <- function(
  var.cnt, title, weight.by, title.wtd.suffix, sort.frq, type, geom.size, geom.colors,
  errorbar.color, axis.title, axis.labels, xlim, ylim, wrap.title, wrap.labels, grid.breaks,
  expand.grid, show.values, show.n, show.prc, show.axis.values, show.ci, show.na,
  show.mean, show.mean.val, show.sd, drop.empty, mean.line.type, mean.line.size,
  inner.box.width, inner.box.dotsize, normal.curve, normal.curve.color,
  normal.curve.size, normal.curve.alpha, auto.group, coord.flip, vjust,
  hjust, y.offset, var.name = NULL) {

  # remove empty value-labels
  if (drop.empty) {
    var.cnt <- sjlabelled::drop_labels(var.cnt)
  }


  # try to find some useful default offsets for textlabels,
  # depending on plot range and flipped coordinates
  if (is.null(y.offset)) {
    # get maximum y-pos
    y.offset <- ceiling(max(table(var.cnt)) / 100)
    if (coord.flip) {
      if (missing(vjust)) vjust <- "center"
      if (missing(hjust)) hjust <- "bottom"
      if (hjust == "bottom")
        y_offset <- y.offset
      else if (hjust == "top")
        y_offset <- -y.offset
      else
        y_offset <- 0
    } else {
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

  if (is.null(axis.title)) axis.title <- sjlabelled::get_label(var.cnt, def.value = var.name)
  if (is.null(title)) title <- sjlabelled::get_label(var.cnt, def.value = var.name)

  # remove titles if empty
  if (!is.null(axis.title) && axis.title == "") axis.title <- NULL
  if (!is.null(title) && title == "") title <- NULL

  # check color argument
  if (length(geom.colors) > 1) geom.colors <- geom.colors[1]

  # default grid-expansion
  if (isTRUE(expand.grid) || (missing(expand.grid) && type == "histogram")) {
    expand.grid <- waiver()
  } else {
    expand.grid <- c(0, 0)
  }

  # for histograms or density plots...
  xv <- sjmisc::to_value(stats::na.omit(var.cnt))

  # check for nice bin-width defaults
  if (type %in% c("histogram", "density") &&
      !is.null(geom.size) &&
      geom.size < round(diff(range(xv)) / 40))
    message("Using very small binwidth. Consider adjusting `geom.size` argument.")

  # create second data frame
  hist.dat <- data.frame(xv)

  # check default geom.size -----
  if (is.null(geom.size)) {
    geom.size <- dplyr::case_when(
      type == "bar" ~ .7,
      type == "dot" ~ 2.5,
      type == "density" ~ ceiling(diff(range(xv)) / 40),
      type == "histogram" ~ ceiling(diff(range(xv)) / 40),
      type == "line" ~ .8,
      type == "boxplot" ~ .3,
      type == "violin" ~ .3,
      TRUE ~ .7
    )
  }

  # check whether variable should be auto-grouped -----
  if (!is.null(auto.group) && length(unique(var.cnt)) >= auto.group) {
    message(sprintf(
      "`%s` has %i unique values and was grouped...",
      var.name,
      length(unique(var.cnt))
    ))
  }

  if (!is.null(weight.by)) {
    dat <- data.frame(
      var.cnt = var.cnt,
      weight.by = weight.by,
      stringsAsFactors = FALSE
    )
  } else {
    dat <- data.frame(
      var.cnt = var.cnt,
      stringsAsFactors = FALSE
    )
  }

  # create frequency data frame -----
  df.frq <- suppressMessages(sjmisc::frq(
    x = dat,
    "var.cnt",
    sort.frq = sort.frq,
    weights = "weight.by",
    auto.grp = auto.group,
    show.na = show.na
  ))

  mydat <- df.frq[[1]]
  # remove empty
  if (drop.empty) mydat <- mydat[mydat$frq > 0, ]

  # add confindence intervals for frequencies
  total_n = sum(mydat$frq)
  rel_frq <- as.numeric(mydat$frq / total_n)
  ci <- 1.96 * suppressWarnings(sqrt(rel_frq * (1 - rel_frq) / total_n))
  mydat$upper.ci <- total_n * (rel_frq + ci)
  mydat$lower.ci <- total_n * (rel_frq - ci)
  mydat$rel.upper.ci <- rel_frq + ci
  mydat$rel.lower.ci <- rel_frq - ci

  # any labels detected?
  if (!is.null(mydat$label) && is.null(axis.labels) && !all(stats::na.omit(mydat$label) == "<none>"))
    axis.labels <- mydat$label
  else if (is.null(axis.labels))
    axis.labels <- mydat$val

  # wrap labels
  axis.labels <- sjmisc::word_wrap(axis.labels, wrap.labels)

  # define text label position
  if (show.ci)
    mydat$label.pos <- mydat$upper.ci
  else
    mydat$label.pos <- mydat$frq

  # Trim labels and title to appropriate size -----
  # check length of diagram title and split longer string into new lines
  # every 50 chars
  if (!is.null(title)) {
    # if we have weighted values, say that in diagram's title
    if (!is.null(title.wtd.suffix)) title <- paste(title, title.wtd.suffix, sep = "")
    title <- sjmisc::word_wrap(title, wrap.title)
  }
  # check length of x-axis title and split longer string into new lines
  # every 50 chars
  if (!is.null(axis.title)) axis.title <- sjmisc::word_wrap(axis.title, wrap.title)

  # count variable may not be a factor!
  if (is.factor(var.cnt) || is.character(var.cnt)) {
    var.cnt <- sjmisc::to_value(var.cnt, keep.labels = F)
  }

  # If we have a histogram, caluclate means of groups
  if (is.null(weight.by)) {
    mittelwert <- mean(var.cnt, na.rm = TRUE)
    stddev <- stats::sd(var.cnt, na.rm = TRUE)
  } else {
    mittelwert <- stats::weighted.mean(var.cnt, weight.by, na.rm = TRUE)
    stddev <- sjstats::weighted_sd(var.cnt, weights = weight.by)
  }

  # If we have boxplots, use different data frame structure
  if (type == "boxplot" || type == "violin") {
    mydat <- stats::na.omit(data.frame(cbind(
      grp = 1,
      frq = var.cnt,
      val = var.cnt
    )))
    mydat$grp <- as.factor(mydat$grp)
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
    } else if (type == "histogram") {
      # what is the maximum values after binning for histograms?
      hist.grp.cnt <- ceiling(diff(range(var.cnt, na.rm = T)) / geom.size)
      # ... or the amount of max. answers per category
      # add 10% margin to upper limit
      upper_lim <- max(pretty(table(
        sjmisc::group_var(
          var.cnt,
          size = "auto",
          n = hist.grp.cnt,
          append = FALSE
        )
      ) * 1.1))
    } else {
      if (show.ci)
        upper_lim <- max(pretty(mydat$upper.ci * 1.1))
      else
        upper_lim <- max(pretty(mydat$frq * 1.1))
    }
  }

  # If we want to include NA, use raw percentages as valid percentages
  if (show.na) mydat$valid.prc <- mydat$raw.prc

  # don't display value labels when we have boxplots or violin plots
  if (type == "boxplot" || type == "violin") show.values <- FALSE
  if (show.values) {
    # here we have counts and percentages
    if (show.prc && show.n) {
      if (coord.flip) {
        ggvaluelabels <-
          geom_text(
            label = sprintf("%i (%.01f%%)", mydat$frq, mydat$valid.prc),
            hjust = hjust,
            vjust = vjust,
            aes(y = .data$label.pos + y_offset)
          )
      } else {
        ggvaluelabels <-
          geom_text(
            label = sprintf("%i\n(%.01f%%)", mydat$frq, mydat$valid.prc),
            hjust = hjust,
            vjust = vjust,
            aes(y = .data$label.pos + y_offset)
          )
      }
    } else if (show.n) {
      # here we have counts, without percentages
      ggvaluelabels <-  geom_text(
        label = sprintf("%i", mydat$frq),
        hjust = hjust,
        vjust = vjust,
        aes(y = .data$label.pos + y_offset)
      )
    } else if (show.prc) {
      # here we have counts, without percentages
      ggvaluelabels <-
        geom_text(
          label = sprintf("%.01f%%", mydat$valid.prc),
          hjust = hjust,
          vjust = vjust,
          aes(y = .data$label.pos + y_offset)
        )
    } else {
      # no labels
      ggvaluelabels <-  geom_text(aes(y = .data$frq), label = "")
    }
  } else {
    # no labels
    ggvaluelabels <-  geom_text(aes(y = .data$frq), label = "")
  }

  # Set up grid breaks
  maxx <- if (is.numeric(mydat$val))
    max(mydat$val) + 1
  else
    nrow(mydat)

  if (is.null(grid.breaks)) {
    gridbreaks <- waiver()
    histgridbreaks <- waiver()
  } else {
    gridbreaks <- c(seq(lower_lim, upper_lim, by = grid.breaks))
    histgridbreaks <- c(seq(lower_lim, maxx, by = grid.breaks))
  }

  # set Y-axis, depending on the calculated upper y-range.
  # It either corresponds to the maximum amount of cases in the data set
  # (length of var) or to the highest count of var's categories.
  if (show.axis.values) {
    yscale <- scale_y_continuous(
      limits = c(lower_lim, upper_lim),
      expand = expand.grid,
      breaks = gridbreaks
    )
  } else {
    yscale <- scale_y_continuous(
      limits = c(lower_lim, upper_lim),
      expand = expand.grid,
      breaks = gridbreaks,
      labels = NULL
    )
  }

  # bar and dot plot start here! -----
  if (type == "bar" || type == "dot") {
    # define geom
    if (type == "bar") {
      geob <- geom_bar(stat = "identity", width = geom.size, fill = geom.colors)
    } else if (type == "dot") {
      geob <- geom_point(size = geom.size, colour = geom.colors)
    }

    # as factor, but preserve order
    mydat$val <- factor(mydat$val, levels = unique(mydat$val))

    # mydat is a data frame that only contains one variable (var).
    # Must be declared as factor, so the bars are central aligned to
    # each x-axis-break.
    baseplot <- ggplot(mydat, aes(x = .data$val, y = .data$frq)) +
      geob +
      yscale +
      # remove guide / legend
      guides(fill = "none") +
      # show absolute and percentage value of each bar.
      ggvaluelabels +
      # print value labels to the x-axis.
      # If argument "axis.labels" is NULL, the category numbers (1 to ...)
      # appear on the x-axis
      scale_x_discrete(labels = axis.labels)

    # add error bars
    if (show.ci) {
      ebcol <- ifelse(type == "dot", geom.colors, errorbar.color)
      # print confidence intervalls (error bars)
      baseplot <- baseplot +
        geom_errorbar(aes_string(ymin = "lower.ci", ymax = "upper.ci"), colour = ebcol, width = 0)
    }

    # check whether coordinates should be flipped, i.e.
    # swap x and y axis
    if (coord.flip) baseplot <- baseplot + coord_flip()

  # Start box plot here -----
  } else if (type == "boxplot" || type == "violin") {
    # setup base plot
    baseplot <- ggplot(mydat, aes_string(x = "grp", y = "frq"))
    # and x-axis
    scalex <- scale_x_discrete(labels = "")
    if (type == "boxplot") {
      baseplot <- baseplot +
        geom_boxplot(width = geom.size, fill = geom.colors, notch = show.ci)
    } else {
      baseplot <- baseplot +
        geom_violin(trim = trimViolin, width = geom.size, fill = geom.colors)
      # if we have a violin plot, add an additional boxplot inside to show
      # more information
      if (show.ci) {
        baseplot <- baseplot +
          geom_boxplot(width = inner.box.width, fill = "white", notch = TRUE)
      } else {
        baseplot <- baseplot +
          geom_boxplot(width = inner.box.width, fill = "white")
      }
    }

    # if we have boxplots or violon plots, also add a point that indicates
    # the mean value
    # different fill colours, because violin boxplots have white background
    fcsp <- ifelse(type == "boxplot", "white", "black")
    baseplot <- baseplot +
      stat_summary(fun.y = "mean", geom = "point", shape = 21,
                   size = inner.box.dotsize, fill = fcsp)
    # no additional labels for the x- and y-axis, only diagram title
    baseplot <- baseplot + yscale + scalex

  # Start density plot here -----
  } else if (type == "density") {
    # First, plot histogram with density curve
    baseplot <- ggplot(hist.dat, aes(x = .data$xv)) +
      geom_histogram(aes(y = stat(density)), binwidth = geom.size, fill = geom.colors) +
      # transparent density curve above bars
      geom_density(aes(y = stat(density)), fill = "cornsilk", alpha = 0.3) +
      # remove margins from left and right diagram side
      scale_x_continuous(expand = expand.grid, breaks = histgridbreaks, limits = xlim)

    # check whether user wants to overlay the histogram
    # with a normal curve
    if (normal.curve) {
      baseplot <- baseplot +
        stat_function(
          fun = dnorm,
          args = list(
            mean = mean(hist.dat$xv),
            sd = stats::sd(hist.dat$xv)
          ),
          colour = normal.curve.color,
          size = normal.curve.size,
          alpha = normal.curve.alpha
        )
    }
  } else {
    # Since the density curve shows no absolute numbers (counts) on the
    # y-axis, have also the opportunity to plot "real" histrograms with
    # counts on the y-axis
    if (type == "histogram") {
      # original data needed for normal curve
      baseplot <- ggplot(mydat) +
        # second data frame mapped to the histogram geom
        geom_histogram(data = hist.dat, aes(x = .data$xv), binwidth = geom.size, fill = geom.colors)
    } else {
      baseplot <- ggplot(mydat, aes(x = .data$val, y = .data$frq)) +
        geom_area(alpha = 0.3) +
        geom_line(linewidth = geom.size, colour = geom.colors) +
        ggvaluelabels
    }
    # check whether user wants to overlay the histogram
    # with a normal curve
    if (normal.curve) {
      baseplot <- baseplot +
        stat_function(
          fun = function(xx, mean, sd, n) {
            n * stats::dnorm(x = xx, mean = mean, sd = sd)
          },
          args = with(mydat, c(
            mean = mittelwert,
            sd = stddev,
            n = length(var.cnt)
          )),
          colour = normal.curve.color,
          size = normal.curve.size,
          alpha = normal.curve.alpha
        )
    }
    # if we have a histogram, add mean-lines
    if (show.mean) {
      baseplot <- baseplot +
        # vertical lines indicating the mean
        geom_vline(xintercept = mittelwert, linetype = mean.line.type, size = mean.line.size)
      # check whether meanvalue should be shown.
      if (show.mean.val) {
        baseplot <- baseplot +
          # use annotation instead of geomtext, because we need mean value only printed once
          annotate(
            "text",
            x = mittelwert,
            y = upper_lim,
            parse = TRUE,
            label = paste(
              "italic(bar(x)) == ",
              round(mittelwert, 1),
              "~~italic(s) == ",
              round(stddev, 1)
            ),
            vjust = "top",
            hjust = "top"
          )
      }
      # check whether the user wants to plot standard deviation area
      if (show.sd) {
        baseplot <- baseplot +
          # first draw shaded rectangle. these are by default in grey colour with very high transparancy
          annotate("rect",
                   xmin = mittelwert - stddev,
                   xmax = mittelwert + stddev,
                   ymin = 0,
                   ymax = c(upper_lim),
                   fill = "grey70",
                   alpha = 0.2) +
          # draw border-lines for shaded rectangle
          geom_vline(xintercept = mittelwert - stddev,
                     linetype = 3,
                     size = mean.line.size,
                     alpha = 0.7) +
          geom_vline(xintercept = mittelwert + stddev,
                     linetype = 3,
                     size = mean.line.size,
                     alpha = 0.7)
      }
    }

    # show absolute and percentage value of each bar.
    baseplot <- baseplot + yscale +
      # continuous x-scale for histograms
      scale_x_continuous(limits = xlim, expand = expand.grid, breaks = histgridbreaks)
  }

  # set axes text and
  baseplot <- baseplot + labs(title = title, x = axis.title, y = NULL)

  # Check whether ggplot object should be returned or plotted
  baseplot
}
