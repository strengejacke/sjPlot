#' @title Plot (grouped) scatter plots
#' @name sjp.scatter
#'             
#' @description Display scatter plot of two variables. Adding a grouping variable to
#'                the scatter plot is possible. Furthermore, fitted lines can be added
#'                for each group as well as for the overall plot.
#'
#' @seealso \href{http://www.strengejacke.de/sjPlot/sjp.scatter}{sjPlot manual: sjp.scatter}
#'              
#' @param x A vector (variable) indicating the x positions. If not specified (i.e. if
#'          \code{NULL}), a range from 1 to length of \code{y} is used to spread the
#'          dots along the x axis.
#' @param y A vector (variable) indicating the y positions. If not specified (i.e. if
#'          \code{NULL}), a range from 1 to length of \code{x} is used to spread the
#'          dots along the y axis.
#' @param grp A grouping variable. If not \code{NULL}, the scatter plot will be grouped. See
#'          examples below. Default is \code{NULL}, i.e. not grouping is done.
#' @param title Title of the diagram, plotted above the whole diagram panel.
#'          Use \code{NULL} to automatically detect variable names that will be used as title
#'          (see \code{\link[sjmisc]{set_var_labels}}) for details).
#' @param legendTitle Title of the diagram's legend.
#' @param legendLabels Labels for the guide/legend.
#' @param axisTitle.x A label (title) for the x axis.
#'          Use \code{NULL} to automatically detect variable names that will be used as title
#'          (see \code{\link[sjmisc]{set_var_labels}}) for details).
#' @param axisTitle.y A label (title) for the y axis.
#'          Use \code{NULL} to automatically detect variable names that will be used as title
#'          (see \code{\link[sjmisc]{set_var_labels}}) for details).
#' @param breakTitleAt Wordwrap for diagram title. Determines how many chars of the title are displayed in
#'          one line and when a line break is inserted into the title.
#' @param breakLegendTitleAt Wordwrap for diagram legend title. Determines how many chars of the legend's title 
#'          are displayed in one line and when a line break is inserted.
#' @param breakLegendLabelsAt Wordwrap for diagram legend labels. Determines how many chars of the legend labels are 
#'          displayed in one line and when a line break is inserted.
#' @param geom.size The size of scattered points.
#' @param geom.colors The color(s) of scattered points. If \code{grp} is not \code{NULL}, groups are indicated
#'          by different colors, thus a vector with multiple color values has to be supplied. By default,
#'          the \code{Set1} palette of diverging palette type is chosen (see \url{http://colorbrewer2.org}).
#' @param showTickMarkLabels.x Whether x axis tick mark labels should be shown or not.
#' @param showTickMarkLabels.y Whether y axis tick mark labels  should be shown or not.
#' @param showGroupFitLine If \code{TRUE}, a fitted line for each group is drawn. See \code{fitmethod} to change the
#'          fit method of the fitted lines.
#' @param showTotalFitLine If \code{TRUE}, a fitted line for the overall scatterplot is drawn. See \code{fitmethod} to change the
#'          fit method of the fitted line.
#' @param showSE If \code{TRUE}, a shaded region indicating the standard error of the fitted lines will be added.
#' @param fitmethod By default, a linear method (\code{"lm"}) is used for fitting the fit lines. Possible values are
#'          for instance:
#'          \itemize{
#'            \item \code{"lm"}
#'            \item \code{"glm"}
#'            \item \code{"loess"}
#'            \item \code{"auto"}
#'          }
#'          (see \href{http://docs.ggplot2.org/current/stat_smooth.html}{ggplot-docs} for more details).
#' @param useJitter If \code{TRUE}, points will be jittered (to avoid overplotting).
#' @param autojitter If \code{TRUE}, points will be jittered according to an overlap-estimation. A matrix of \code{x}
#'          and \code{y} values is created and the amount of cells (indicating a unique point position) is calculated.
#'          If more than 15\% (see \code{jitterRatio}) of the approximated amount of unique point coordinates seem to
#'          overlap, they are automatically jittered.
#' @param jitterRatio The ratio of tolerated overlapping (see \code{autojitter}). If approximated amount of overlapping 
#'          points exceed this ration, they are automatically jittered. Default is 0.15. Valid values range between 0 and 1.
#' @param showRug If \code{TRUE}, a marginal rug plot is displayed in the graph (see \href{http://docs.ggplot2.org/current/geom_rug.html}{ggplot-docs}
#'          for more details).)
#' @param hideLegend Indicates whether legend (guide) should be shown or not.
#' @param facet.grid \code{TRUE} when each scatter plot group should be plotted as single facet instead of 
#'          an integrated single graph. Only applies if \code{grp} is not \code{NULL}. Each category of
#'          \code{grp} will be plotted in an own facet.
#' @param printPlot If \code{TRUE} (default), plots the results as graph. Use \code{FALSE} if you don't
#'          want to plot any graphs. In either case, the ggplot-object will be returned as value.
#' @return (Insisibily) returns the ggplot-object with the complete plot (\code{plot}) as well as the data frame that
#'           was used for setting up the ggplot-object (\code{df}).
#'
#' @examples
#' # load sample date
#' library(sjmisc)
#' data(efc)
#' 
#' # simple scatter plot, auto-jittering
#' sjp.scatter(efc$e16sex,efc$neg_c_7)
#'
#' # simple scatter plot, no jittering needed
#' sjp.scatter(efc$c160age,efc$e17age)
#'
#' # grouped scatter plot
#' sjp.scatter(efc$c160age,efc$e17age, efc$e42dep)
#'
#' # grouped and jittered scatter plot with marginal rug plot
#' sjp.scatter(efc$e16sex,efc$neg_c_7, efc$c172code, showRug=TRUE)
#' 
#' # grouped and labelled scatter plot
#' sjp.scatter(efc$c160age,efc$e17age, efc$e42dep, title="Scatter Plot",
#'             legendTitle = get_var_labels(efc)['e42dep'],
#'             legendLabels = get_val_labels(efc)[['e42dep']],
#'             axisTitle.x = get_var_labels(efc)['c160age'],
#'             axisTitle.y = get_var_labels(efc)['e17age'],
#'             showGroupFitLine = TRUE)
#' 
#' # grouped and labelled scatter plot as facets
#' sjp.scatter(efc$c160age,efc$e17age, efc$e42dep, title="Scatter Plot",
#'             legendTitle = get_var_labels(efc)['e42dep'],
#'             legendLabels = get_val_labels(efc)[['e42dep']],
#'             axisTitle.x = get_var_labels(efc)['c160age'],
#'             axisTitle.y = get_var_labels(efc)['e17age'],
#'             showGroupFitLine = TRUE, facet.grid = TRUE, showSE = TRUE)
#' 
#' # plot residuals of fitted models
#' fit <- lm(neg_c_7 ~ quol_5, data = efc)
#' sjp.scatter(y = fit$residuals, showTotalFitLine = TRUE)
#' 
#' # -------------------------------
#' # auto-detection of labels
#' # -------------------------------
#' efc <- set_var_labels(efc, get_var_labels(efc))
#' 
#' # show axis titles
#' sjp.scatter(efc$c160age,efc$e17age, efc$e42dep)
#'             
#' # hide axis titles
#' sjp.scatter(efc$c160age,efc$e17age, efc$e42dep,
#'             title="", axisTitle.x="", axisTitle.y="")
#' 
#'   
#' @importFrom scales brewer_pal
#' @import ggplot2
#' @export
sjp.scatter <- function(x=NULL,
                        y=NULL,
                        grp=NULL,
                        title="", 
                        legendTitle=NULL,
                        legendLabels=NULL,
                        axisTitle.x=NULL,
                        axisTitle.y=NULL,
                        breakTitleAt=50, 
                        breakLegendTitleAt=20, 
                        breakLegendLabelsAt=20,
                        geom.size=3,
                        geom.colors=NULL,
                        showTickMarkLabels.x=TRUE,
                        showTickMarkLabels.y=TRUE,
                        showGroupFitLine=FALSE,
                        showTotalFitLine=FALSE,
                        showSE=FALSE,
                        fitmethod="lm",
                        useJitter=FALSE,
                        autojitter=TRUE,
                        jitterRatio=0.15,
                        showRug=FALSE,
                        hideLegend=FALSE,
                        facet.grid=FALSE,
                        printPlot=TRUE) {
  # --------------------------------------------------------
  # check parameters
  # --------------------------------------------------------
  if (is.null(x) && is.null(y)) {
    stop("At least either 'x' or 'y' must be specified.", call. = FALSE)
  }
  if (is.null(x)) x <- c(1:length(y))
  if (is.null(y)) y <- c(1:length(x))
  # --------------------------------------------------------
  # try to automatically set labels is not passed as parameter
  # --------------------------------------------------------
  if (is.null(legendLabels) && !is.null(grp)) legendLabels <- sjmisc:::autoSetValueLabels(grp)
  if (is.null(legendTitle) && !is.null(grp)) legendTitle <- sjmisc:::autoSetVariableLabels(grp)
  if (is.null(axisTitle.x)) axisTitle.x <- sjmisc:::autoSetVariableLabels(x)
  if (is.null(axisTitle.y)) axisTitle.y <- sjmisc:::autoSetVariableLabels(y)
  if (is.null(title)) {
    t1 <- sjmisc:::autoSetVariableLabels(x)
    t2 <- sjmisc:::autoSetVariableLabels(y)
    if (!is.null(t1) && !is.null(t2)) {
      title <- paste0(t1, " by ", t2)
      if (!is.null(grp)) {
        t3 <- sjmisc:::autoSetVariableLabels(grp)
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
      warning("jitterRatio out of valid bounds. Using 0.15 for jitterRatio...")
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
  # --------------------------------------------------------
  # unlist labels
  # --------------------------------------------------------
  if (!is.null(legendLabels) && is.list(legendLabels)) legendLabels <- unlistlabels(legendLabels)
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
  df <- na.omit(data.frame(cbind(x = x, y = y, grp = grp)))
  # group as factor
  df$grp <- as.factor(df$grp)
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
    scatter <- scatter + geom_jitter(size = geom.size)
  } else {
    scatter <- scatter + geom_point(size = geom.size)
  }
  # --------------------------------------------------------
  # Show fitted lines
  # --------------------------------------------------------
  if (showGroupFitLine) scatter <- scatter + stat_smooth(data = df, 
                                                         aes(colour = grp), 
                                                         method = fitmethod, 
                                                         se = showSE)
  if (showTotalFitLine) scatter <- scatter + stat_smooth(method = fitmethod, 
                                                         se = showSE, 
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
  if (facet.grid) scatter <- scatter + facet_wrap(~ grp)
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
                              ifelse(hideLegend == TRUE, FALSE, TRUE), 
                              legendLabels)
  # ---------------------------------------------------------
  # Check whether ggplot object should be returned or plotted
  # ---------------------------------------------------------
  if (printPlot) plot(scatter)
  # -------------------------------------
  # return results
  # -------------------------------------
  invisible (structure(class = "sjpscatter",
                       list(plot = scatter,
                            df = df)))
}
                       