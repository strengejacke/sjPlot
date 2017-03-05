#' @title Plot (grouped) scatter plots
#' @name sjp.scatter
#'
#' @description Display scatter plot of two variables. Adding a grouping variable to
#'                the scatter plot is possible. Furthermore, fitted lines can be added
#'                for each group as well as for the overall plot.
#'
#' @seealso \href{http://www.strengejacke.de/sjPlot/sjp.scatter}{sjPlot manual: sjp.scatter}
#'
#' @param x Vector indicating the x positions. If not specified (i.e. if
#'          \code{NULL}), a range from 1 to length of \code{y} is used to spread the
#'          dots along the x axis.
#' @param y Vector indicating the y positions. If not specified (i.e. if
#'          \code{NULL}), a range from 1 to length of \code{x} is used to spread the
#'          dots along the y axis.
#' @param grp Grouping variable. If not \code{NULL}, the scatter plot will be grouped. See
#'          'Examples'. Default is \code{NULL}, i.e. not grouping is done.
#' @param dot.labels Character vector with names for each coordinate pair given
#'          by \code{x} and \code{y}, so text labels are added to the plot.
#'          Must be of same length as \code{x} and \code{y}.
#'          If \code{dot.labels} has a different length, data points will be trimmed
#'          to match \code{dot.labels}. If \code{dot.labels = NULL} (default),
#'          no labels are printed.
#' @param label.size Size of text labels if argument \code{dot.labels} is used.
#' @param fit.line.grps Logical, if \code{TRUE}, a fitted line for each group
#'          is drawn. See \code{fitmethod} to change the fit method of the fitted lines.
#' @param fit.line Logical, if \code{TRUE}, a fitted line for the overall
#'          scatterplot is drawn. See \code{fitmethod} to change the fit method
#'          of the fitted line.
#' @param fitmethod By default, a linear method (\code{"lm"}) is used for fitting
#'          the fit lines. Possible values are for instance \code{"lm"}, \code{"glm"},
#'          \code{"loess"} or \code{"auto"}.
#' @param jitter.dots Logical, if \code{TRUE}, points will be jittered (to avoid overplotting).
#' @param emph.dots Logical, if \code{TRUE}, overlapping points at same coordinates
#'          will be becomme larger, so point size indicates amount of overlapping.
#' @param auto.jitter Logical, if \code{TRUE}, points will be jittered according
#'          to an overlap-estimation. A matrix of \code{x} and \code{y} values
#'          is created and the amount of cells (indicating a unique point position)
#'          is calculated. If more than 15\% (see \code{jitter.ratio}) of the
#'          approximated amount of unique point coordinates seem to
#'          overlap, they are automatically jittered.
#' @param jitter.ratio Ratio of tolerated overlapping (see \code{auto.jitter}).
#'          If approximated amount of overlapping  points exceed this ratio,
#'          they are automatically jittered. Default is 0.15. Valid values range
#'          between 0 and 1.
#' @param show.rug Logical, if \code{TRUE}, a marginal rug plot is displayed
#'          in the graph.
#'
#' @return (Insisibily) returns the ggplot-object with the complete plot (\code{plot}) as well as the data frame that
#'           was used for setting up the ggplot-object (\code{data}).
#'
#' @inheritParams sjp.grpfrq
#' @inheritParams sjp.lm
#'
#' @examples
#' # load sample date
#' library(sjmisc)
#' data(efc)
#'
#' # simple scatter plot, auto-jittering
#' sjp.scatter(efc$e16sex, efc$neg_c_7)
#'
#' # simple scatter plot, no jittering needed
#' sjp.scatter(efc$c160age, efc$e17age)
#'
#' # grouped scatter plot
#' sjp.scatter(efc$c160age, efc$e17age, efc$e42dep)
#'
#' # grouped and jittered scatter plot with marginal rug plot
#' sjp.scatter(efc$e16sex,efc$neg_c_7, efc$c172code, show.rug = TRUE)
#'
#' # grouped and labelled scatter plot, not using the auto-detection
#' # of labels, but instead pass labels as arguments
#' sjp.scatter(efc$c160age, efc$e17age, efc$e42dep,
#'             title = "Scatter Plot", legend.title = get_label(efc)['e42dep'],
#'             legend.labels = get_labels(efc)[['e42dep']],
#'             axis.titles = c(get_label(efc)['c160age'], get_label(efc)['e17age']),
#'             fit.line.grps = TRUE)
#'
#' # grouped and labelled scatter plot as facets
#' sjp.scatter(efc$c160age,efc$e17age, efc$e42dep, fit.line.grps = TRUE,
#'             facet.grid = TRUE, show.ci = TRUE)
#'
#' # plot residuals of fitted models
#' fit <- lm(neg_c_7 ~ quol_5, data = efc)
#' sjp.scatter(y = fit$residuals, fit.line = TRUE)
#'
#' # "hide" axis titles
#' sjp.scatter(efc$c160age, efc$e17age, efc$e42dep, title = "",
#'             axis.titles = c("", ""))
#'
#' # plot text labels
#' pl <- c(1:10)
#' for (i in 1:10)
#'   pl[i] <- paste(sample(c(0:9, letters, LETTERS), 8, replace = TRUE), collapse = "")
#' sjp.scatter(runif(10), runif(10), dot.labels = pl)
#'
#' @importFrom scales brewer_pal
#' @importFrom stats na.omit
#' @import ggplot2
#' @export
sjp.scatter <- function(x = NULL,
                        y = NULL,
                        grp = NULL,
                        title = "",
                        legend.title = NULL,
                        legend.labels = NULL,
                        dot.labels = NULL,
                        axis.titles = NULL,
                        wrap.title = 50,
                        wrap.legend.title = 20,
                        wrap.legend.labels = 20,
                        geom.size = 2,
                        label.size = 3,
                        geom.colors = NULL,
                        show.axis.values = TRUE,
                        fit.line.grps = FALSE,
                        fit.line = FALSE,
                        show.ci = FALSE,
                        fitmethod = "lm",
                        jitter.dots = FALSE,
                        emph.dots = FALSE,
                        auto.jitter = TRUE,
                        jitter.ratio = 0.15,
                        show.rug = FALSE,
                        show.legend = TRUE,
                        facet.grid = FALSE,
                        prnt.plot = TRUE) {
  # ------------------------
  # check if suggested packages are available
  # ------------------------
  if (!is.null(dot.labels) && !requireNamespace("ggrepel", quietly = TRUE)) {
    stop("Package `ggrepel` needed to plot labels. Please install it.", call. = FALSE)
  }
  # --------------------------------------------------------
  # get variable name
  # --------------------------------------------------------
  name.x <- get_var_name(deparse(substitute(x)))
  name.y <- get_var_name(deparse(substitute(y)))
  name.grp <- get_var_name(deparse(substitute(grp)))
  # --------------------------------------------------------
  # any missing names?
  # --------------------------------------------------------
  if (is.null(name.x) || name.x == "NULL") name.x <- ""
  if (is.null(name.y) || name.y == "NULL") name.y <- ""
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
  # check parameters
  # --------------------------------------------------------
  if (is.null(x) && is.null(y)) {
    stop("At least either 'x' or 'y' must be specified.", call. = FALSE)
  }
  if (jitter.dots && emph.dots) {
    warning("Only one of `jitter.dots` and `emph.dots` may be `TRUE`. Defaulting `jitter.dots` to `FALSE`.")
    jitter.dots <- FALSE
  }
  if (is.null(x)) x <- seq_len(length(y))
  if (is.null(y)) y <- seq_len(length(x))
  # disable auto-jitter?
  if (emph.dots) auto.jitter <- FALSE
  # --------------------------------------------------------
  # try to automatically set labels is not passed as parameter
  # --------------------------------------------------------
  if (is.null(legend.labels) && !is.null(grp)) {
    legend.labels <- sjmisc::get_labels(grp, attr.only = F, include.values = NULL,
                                        include.non.labelled = T)
  }
  if (is.null(legend.title) && !is.null(grp)) legend.title <- sjmisc::get_label(grp, def.value = name.grp)
  if (is.null(axisTitle.x)) axisTitle.x <- sjmisc::get_label(x, def.value = name.x)
  if (is.null(axisTitle.y)) axisTitle.y <- sjmisc::get_label(y, def.value = name.y)
  if (is.null(title)) {
    t1 <- sjmisc::get_label(x, def.value = name.x)
    t2 <- sjmisc::get_label(y, def.value = name.y)
    if (!is.null(t1) && !is.null(t2)) {
      title <- paste0(t1, " by ", t2)
      if (!is.null(grp)) {
        t3 <- sjmisc::get_label(grp, def.value = name.grp)
        if (!is.null(t3)) title <- paste0(title, " (grouped by ", t3, ")")
      }
    }
  }
  # --------------------------------------------------------
  # remove titles if empty
  # --------------------------------------------------------
  if (!is.null(legend.title) && legend.title == "") legend.title <- NULL
  if (!is.null(axisTitle.x) && axisTitle.x == "") axisTitle.x <- NULL
  if (!is.null(axisTitle.y) && axisTitle.y == "") axisTitle.y <- NULL
  if (!is.null(title) && title == "") title <- NULL
  # ------------------------------------------
  # check for auto-jittering
  # ------------------------------------------
  if (auto.jitter && !jitter.dots) {
    # check for valid range of jitter ratio
    if (jitter.ratio <= 0 || jitter.ratio >= 1) {
      # inform user
      warning("`jitter.ratio` out of valid bounds. Using 0.15 for `jitter.ratio`...")
      jitter.ratio <- 0.15
    }
    # retrieve the highest amount of points lying
    # on the same coordinate
    overlap <- nrow(table(x, y)) * ncol(table(x, y))
    # check ratio of overlapping points according to total points
    if (overlap < (length(x) * jitter.ratio)) {
      # use jittering now
      jitter.dots <- TRUE
      message("auto-jittering values...")
    }
  }
  # ------------------------------------------
  # create data frame
  # ------------------------------------------
  # check whether we have grouping variable
  if (is.null(grp)) {
    # if not, add a dummy grouping variable
    grp <- rep(1, length(x))
    # we don't need legend here
    show.legend <- FALSE
  }
  # get value labels from attribute
  grl <- sjmisc::get_labels(grp, attr.only = T)
  # simple data frame
  df <- stats::na.omit(data.frame(cbind(x = x, y = y, grp = grp)))
  # group as factor
  df$grp <- as.factor(df$grp)
  # set labelled levels, for facets
  if (facet.grid && !is.null(grl)) levels(df$grp) <- grl
  # do we have point labels?
  if (!is.null(dot.labels)) {
    # check length
    if (length(dot.labels) > nrow(df)) {
      # Tell user that we have too many point labels
      warning("More point labels than data points. Omitting remaining point labels", call. = F)
      # shorten vector
      dot.labels <- dot.labels[seq_len(nrow(df))]
    } else if (length(dot.labels) < nrow(df)) {
      # Tell user that we have too less point labels
      warning("Less point labels than data points. Omitting remaining data point", call. = F)
      # shorten data frame
      df <- df[seq_len(length(dot.labels)), ]
    }
    # append labels
    df$dot.lab <- as.character(dot.labels)
  }
  # --------------------------------------------------------
  # Prepare and trim legend labels to appropriate size
  # --------------------------------------------------------
  # Check whether we have any labels passed as parameter
  # if not, use category text of group variable as legend text
  if (is.null(legend.labels)) legend.labels <- as.character(sort(unique(df$grp)))
  # wrap legend text lines
  legend.labels <- sjmisc::word_wrap(legend.labels, wrap.legend.labels)
  # check whether we have a title for the legend
  # if yes, wrap legend title line
  if (!is.null(legend.title)) legend.title <- sjmisc::word_wrap(legend.title, wrap.legend.title)
  # check length of diagram title and split longer string at into new lines
  # every 50 chars
  if (!is.null(title)) title <- sjmisc::word_wrap(title, wrap.title)
  # check length of x-axis title and split longer string at into new lines
  # every 50 chars
  if (!is.null(axisTitle.x)) axisTitle.x <- sjmisc::word_wrap(axisTitle.x, wrap.title)
  # check length of x-axis title and split longer string at into new lines
  # every 50 chars
  if (!is.null(axisTitle.y)) axisTitle.y <- sjmisc::word_wrap(axisTitle.y, wrap.title)
  # --------------------------------------------------------
  # Plot scatter plot
  # --------------------------------------------------------
  scatter <- ggplot(df, aes_string(x = "x", y = "y", colour = "grp"))
  # --------------------------------------------------------
  # Add marginal rug
  # --------------------------------------------------------
  if (show.rug) {
    if (jitter.dots) {
      scatter <- scatter + geom_rug(position = "jitter")
    } else {
      scatter <- scatter + geom_rug()
    }
  }
  # --------------------------------------------------------
  # Use Jitter/Points
  # --------------------------------------------------------
  if (jitter.dots) {
    # else plot dots
    scatter <- scatter + geom_jitter(size = geom.size)
    # do we have text?
    if (!is.null(dot.labels))
      scatter <- scatter +
        ggrepel::geom_text_repel(aes_string(label = "dot.lab"), size = label.size, position = "jitter")
  } else {
    if (emph.dots) {
      # indicate overlapping dots by point size
      scatter <- scatter + geom_count(show.legend = F)
    } else {
      # else plot dots
      scatter <- scatter + geom_point(size = geom.size)
    }
    # do we have text?
    if (!is.null(dot.labels)) {
      scatter <- scatter +
        ggrepel::geom_text_repel(aes_string(label = "dot.lab"), size = label.size)

    }
  }
  # --------------------------------------------------------
  # Show fitted lines
  # --------------------------------------------------------
  if (fit.line.grps) scatter <- scatter +
    stat_smooth(data = df, aes_string(colour = "grp"), method = fitmethod, se = show.ci)
  if (fit.line) scatter <- scatter +
    stat_smooth(method = fitmethod, se = show.ci, colour = "black")
  # --------------------------------------------------------
  # set font size for axes.
  # --------------------------------------------------------
  scatter <- scatter +
    labs(title = title, x = axisTitle.x, y = axisTitle.y, colour = legend.title)
  # --------------------------------------------------------
  # Hide or show tick marks
  # --------------------------------------------------------
  if (!show.axis.values)
    scatter <- scatter + scale_y_continuous(labels = NULL) +
                         scale_x_continuous(labels = NULL)
  # --------------------------------------
  # facet plot
  # --------------------------------------
  if (facet.grid) scatter <- scatter + facet_wrap(~grp)
  # --------------------------------------------------------
  # Prepare fill colors
  # --------------------------------------------------------
  if (is.null(geom.colors)) {
    colen <- length(unique(na.omit(grp)))
    if (colen == 1) {
      geom.colors <- "#003399"
    } else {
      geom.colors <- "Dark2"
    }
  }
  # ---------------------------------------------------------
  # set geom colors
  # ---------------------------------------------------------
  scatter <- sj.setGeomColors(scatter,
                              geom.colors,
                              length(legend.labels),
                              show.legend,
                              legend.labels)
  # ---------------------------------------------------------
  # Check whether ggplot object should be returned or plotted
  # ---------------------------------------------------------
  if (prnt.plot) graphics::plot(scatter)
  # -------------------------------------
  # return results
  # -------------------------------------
  invisible(structure(class = "sjpscatter",
                      list(plot = scatter,
                           data = df)))
}
