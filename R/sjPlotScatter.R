#' @title Plot (grouped) scatter plots
#' @name sjp.scatter
#'
#' @description Display scatter plot of two variables. Adding a grouping variable to
#'                the scatter plot is possible. Furthermore, fitted lines can be added
#'                for each group as well as for the overall plot.
#'
#' @seealso \href{http://www.strengejacke.de/sjPlot/sjp.scatter}{sjPlot manual: sjp.scatter}
#'
#' @param x vector indicating the x positions. If not specified (i.e. if
#'          \code{NULL}), a range from 1 to length of \code{y} is used to spread the
#'          dots along the x axis.
#' @param y vector indicating the y positions. If not specified (i.e. if
#'          \code{NULL}), a range from 1 to length of \code{x} is used to spread the
#'          dots along the y axis.
#' @param grp grouping variable. If not \code{NULL}, the scatter plot will be grouped. See
#'          'Examples'. Default is \code{NULL}, i.e. not grouping is done.
#' @param pointLabels character vector with names for each coordinate pair given
#'          by \code{x} and \code{y}, so text labels are added to the plot. 
#'          Must be of same length as \code{x} and \code{y}.
#'          If \code{pointLabels} has a different length, data points will be trimmed
#'          to match \code{pointLabels}. If \code{pointLabels = NULL} (default),
#'          no labels are printed.
#' @param label.size Size of text labels if argument \code{pointLabels} is used.
#' @param axisTitle.x title for the x axis. Use \code{NULL} to automatically
#'          detect variable names that will be used as title
#'          (see \code{\link[sjmisc]{set_label}}) for details).
#' @param axisTitle.y title for the y axis.
#'          Use \code{NULL} to automatically detect variable names that will be used as title
#'          (see \code{\link[sjmisc]{set_label}}) for details).
#' @param geom.size size of point geoms.
#' @param geom.colors color(s) of point geoms. If \code{grp} is not \code{NULL},
#'          groups are indicated by different colors, thus a vector with multiple
#'          color values has to be supplied.
#' @param showTickMarkLabels.x logica, whether x axis tick mark labels should be shown or not.
#' @param showTickMarkLabels.y logical, hether y axis tick mark labels  should be shown or not.
#' @param showGroupFitLine logical, if \code{TRUE}, a fitted line for each group
#'          is drawn. See \code{fitmethod} to change the fit method of the fitted lines.
#' @param showTotalFitLine logical, if \code{TRUE}, a fitted line for the overall
#'          scatterplot is drawn. See \code{fitmethod} to change the fit method
#'          of the fitted line.
#' @param show.ci logical, if \code{TRUE}, a shaded region indicating the
#'          confidence interval of the fitted lines will be added.
#' @param fitmethod By default, a linear method (\code{"lm"}) is used for fitting
#'          the fit lines. Possible values are for instance \code{"lm"}, \code{"glm"},
#'          \code{"loess"} or \code{"auto"}.
#' @param useJitter logical, if \code{TRUE}, points will be jittered (to avoid overplotting).
#' @param useCount logical, if \code{TRUE}, overlapping points at same coordinates
#'          will be becomme larger, so point size indicates amount of overlapping.
#' @param autojitter logical, if \code{TRUE}, points will be jittered according
#'          to an overlap-estimation. A matrix of \code{x} and \code{y} values
#'          is created and the amount of cells (indicating a unique point position)
#'          is calculated. If more than 15\% (see \code{jitterRatio}) of the
#'          approximated amount of unique point coordinates seem to
#'          overlap, they are automatically jittered.
#' @param jitterRatio ratio of tolerated overlapping (see \code{autojitter}).
#'          If approximated amount of overlapping  points exceed this ratio,
#'          they are automatically jittered. Default is 0.15. Valid values range
#'          between 0 and 1.
#' @param showRug logical, if \code{TRUE}, a marginal rug plot is displayed
#'          in the graph.
#' @param facet.grid \code{TRUE} when each scatter plot group should be plotted as single facet instead of
#'          an integrated single graph. Only applies if \code{grp} is not \code{NULL}. Each category of
#'          \code{grp} will be plotted in an own facet.
#'
#' @return (Insisibily) returns the ggplot-object with the complete plot (\code{plot}) as well as the data frame that
#'           was used for setting up the ggplot-object (\code{df}).
#'
#' @inheritParams sjp.grpfrq
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
#' sjp.scatter(efc$e16sex,efc$neg_c_7, efc$c172code, showRug = TRUE)
#'
#' # grouped and labelled scatter plot, not using the auto-detection
#' # of labels, but instead pass labels as arguments
#' sjp.scatter(efc$c160age, efc$e17age, efc$e42dep,
#'             title = "Scatter Plot",
#'             legendTitle = get_label(efc)['e42dep'],
#'             legendLabels = get_labels(efc)[['e42dep']],
#'             axisTitle.x = get_label(efc)['c160age'],
#'             axisTitle.y = get_label(efc)['e17age'],
#'             showGroupFitLine = TRUE)
#'
#' # grouped and labelled scatter plot as facets
#' sjp.scatter(efc$c160age,efc$e17age, efc$e42dep,
#'             showGroupFitLine = TRUE,
#'             facet.grid = TRUE,
#'             show.ci = TRUE)
#'
#' # plot residuals of fitted models
#' fit <- lm(neg_c_7 ~ quol_5, data = efc)
#' sjp.scatter(y = fit$residuals, showTotalFitLine = TRUE)
#'
#' # "hide" axis titles
#' sjp.scatter(efc$c160age, efc$e17age, efc$e42dep,
#'             title = "", axisTitle.x = "", axisTitle.y = "")
#'
#' # plot text labels
#' pl <- c(1:10)
#' for (i in 1:10) pl[i] <- paste(sample(c(0:9, letters, LETTERS),
#'                                       8, replace = TRUE),
#'                                collapse = "")
#' sjp.scatter(runif(10), runif(10), pointLabels = pl)
#'
#' @importFrom scales brewer_pal
#' @importFrom stats na.omit
#' @import ggplot2
#' @export
sjp.scatter <- function(x = NULL,
                        y = NULL,
                        grp = NULL,
                        title = "",
                        legendTitle = NULL,
                        legendLabels = NULL,
                        pointLabels = NULL,
                        axisTitle.x = NULL,
                        axisTitle.y = NULL,
                        breakTitleAt = 50,
                        breakLegendTitleAt = 20,
                        breakLegendLabelsAt = 20,
                        geom.size = 2,
                        label.size = 3,
                        geom.colors = NULL,
                        showTickMarkLabels.x = TRUE,
                        showTickMarkLabels.y = TRUE,
                        showGroupFitLine = FALSE,
                        showTotalFitLine = FALSE,
                        show.ci = FALSE,
                        fitmethod = "lm",
                        useJitter = FALSE,
                        useCount = FALSE,
                        autojitter = TRUE,
                        jitterRatio = 0.15,
                        showRug = FALSE,
                        hideLegend = FALSE,
                        facet.grid = FALSE,
                        printPlot = TRUE) {
  # ------------------------
  # check if suggested packages are available
  # ------------------------
  if (!is.null(pointLabels) && !requireNamespace("ggrepel", quietly = TRUE)) {
    stop("Package `ggrepel` needed to plot labels. Please install it.", call. = FALSE)
  }
  
  # --------------------------------------------------------
  # get variable name
  # --------------------------------------------------------
  name.x <- get_var_name(deparse(substitute(x)))
  name.y <- get_var_name(deparse(substitute(y)))
  name.grp <- get_var_name(deparse(substitute(grp)))
  # --------------------------------------------------------
  # check parameters
  # --------------------------------------------------------
  if (is.null(x) && is.null(y)) {
    stop("At least either 'x' or 'y' must be specified.", call. = FALSE)
  }
  if (useJitter && useCount) {
    warning("Only one of `useJitter` and `useCount` may be `TRUE`. Defaulting `useJitter` to `FALSE`.")
    useJitter <- FALSE
  }
  if (is.null(x)) x <- c(1:length(y))
  if (is.null(y)) y <- c(1:length(x))
  # disable auto-jitter?
  if (useCount) autojitter <- FALSE
  # --------------------------------------------------------
  # try to automatically set labels is not passed as parameter
  # --------------------------------------------------------
  if (is.null(legendLabels) && !is.null(grp)) legendLabels <- sjmisc::get_labels(grp,
                                                                                 attr.only = F,
                                                                                 include.values = NULL,
                                                                                 include.non.labelled = T)
  if (is.null(legendTitle) && !is.null(grp)) legendTitle <- sjmisc::get_label(grp, def.value = name.grp)
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
  if (!is.null(legendTitle) && legendTitle == "") legendTitle <- NULL
  if (!is.null(axisTitle.x) && axisTitle.x == "") axisTitle.x <- NULL
  if (!is.null(axisTitle.y) && axisTitle.y == "") axisTitle.y <- NULL
  if (!is.null(title) && title == "") title <- NULL
  # ------------------------------------------
  # check for auto-jittering
  # ------------------------------------------
  if (autojitter && !useJitter) {
    # check for valid range of jitter ratio
    if (jitterRatio <= 0 || jitterRatio >= 1) {
      # inform user
      warning("`jitterRatio` out of valid bounds. Using 0.15 for `jitterRatio`...")
      jitterRatio <- 0.15
    }
    # retrieve the highest amount of points lying
    # on the same coordinate
    overlap <- nrow(table(x, y)) * ncol(table(x, y))
    # check ratio of overlapping points according to total points
    if (overlap < (length(x) * jitterRatio)) {
      # use jittering now
      useJitter <- TRUE
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
    hideLegend <- TRUE
  }
  # simple data frame
  df <- stats::na.omit(data.frame(cbind(x = x, y = y, grp = grp)))
  # group as factor
  df$grp <- as.factor(df$grp)
  # do we have point labels?
  if (!is.null(pointLabels)) {
    # check length
    if (length(pointLabels) > nrow(df)) {
      # Tell user that we have too many point labels
      warning("More point labels than data points. Omitting remaining point labels", call. = F)
      # shorten vector
      pointLabels <- pointLabels[1:nrow(df)]
    } else if (length(pointLabels) < nrow(df)) {
      # Tell user that we have too less point labels
      warning("Less point labels than data points. Omitting remaining data point", call. = F)
      # shorten data frame
      df <- df[1:length(pointLabels), ]
    }
    # append labels
    df$pointLabels <- as.character(pointLabels)
  }
  # --------------------------------------------------------
  # Prepare and trim legend labels to appropriate size
  # --------------------------------------------------------
  # Check whether we have any labels passed as parameter
  # if not, use category text of group variable as legend text
  if (is.null(legendLabels)) legendLabels <- c(sort(unique(df$grp)))
  # wrap legend text lines
  legendLabels <- sjmisc::word_wrap(legendLabels, breakLegendLabelsAt)
  # check whether we have a title for the legend
  # if yes, wrap legend title line
  if (!is.null(legendTitle)) legendTitle <- sjmisc::word_wrap(legendTitle, breakLegendTitleAt)
  # check length of diagram title and split longer string at into new lines
  # every 50 chars
  if (!is.null(title)) title <- sjmisc::word_wrap(title, breakTitleAt)
  # check length of x-axis title and split longer string at into new lines
  # every 50 chars
  if (!is.null(axisTitle.x)) axisTitle.x <- sjmisc::word_wrap(axisTitle.x, breakTitleAt)
  # check length of x-axis title and split longer string at into new lines
  # every 50 chars
  if (!is.null(axisTitle.y)) axisTitle.y <- sjmisc::word_wrap(axisTitle.y, breakTitleAt)
  # --------------------------------------------------------
  # Plot scatter plot
  # --------------------------------------------------------
  scatter <- ggplot(df,aes(x, y, colour = grp))
  # --------------------------------------------------------
  # Add marginal rug
  # --------------------------------------------------------
  if (showRug) {
    if (useJitter) {
      scatter <- scatter + geom_rug(position = "jitter")
    } else {
      scatter <- scatter + geom_rug()
    }
  }
  # --------------------------------------------------------
  # Use Jitter/Points
  # --------------------------------------------------------
  if (useJitter) {
    # else plot dots
    scatter <- scatter + geom_jitter(size = geom.size)
    # do we have text?
    if (!is.null(pointLabels))
      scatter <- scatter + ggrepel::geom_text_repel(aes(label = pointLabels),
                                           size = label.size,
                                           position = "jitter")
  } else {
    if (useCount) {
      # indicate overlapping dots by point size
      scatter <- scatter + geom_count(show.legend = F)
    } else {
      # else plot dots
      scatter <- scatter + geom_point(size = geom.size)
    }
    # do we have text?
    if (!is.null(pointLabels)) {
      scatter <- scatter + 
        ggrepel::geom_text_repel(aes(label = pointLabels),
                                 size = label.size)
      
    }
  }
  # --------------------------------------------------------
  # Show fitted lines
  # --------------------------------------------------------
  if (showGroupFitLine) scatter <- scatter + stat_smooth(data = df,
                                                         aes(colour = grp),
                                                         method = fitmethod,
                                                         se = show.ci)
  if (showTotalFitLine) scatter <- scatter + stat_smooth(method = fitmethod,
                                                         se = show.ci,
                                                         colour = "black")
  # --------------------------------------------------------
  # set font size for axes.
  # --------------------------------------------------------
  scatter <- scatter +
    labs(title = title,
         x = axisTitle.x,
         y = axisTitle.y,
         colour = legendTitle)
  # --------------------------------------------------------
  # Hide or show tick marks
  # --------------------------------------------------------
  if (!showTickMarkLabels.x) scatter <- scatter + scale_x_continuous(labels = NULL)
  if (!showTickMarkLabels.y) scatter <- scatter + scale_y_continuous(labels = NULL)
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
                              length(legendLabels),
                              ifelse(isTRUE(hideLegend), FALSE, TRUE),
                              legendLabels)
  # ---------------------------------------------------------
  # Check whether ggplot object should be returned or plotted
  # ---------------------------------------------------------
  if (printPlot) graphics::plot(scatter)
  # -------------------------------------
  # return results
  # -------------------------------------
  invisible(structure(class = "sjpscatter",
                      list(plot = scatter,
                           df = df)))
}
