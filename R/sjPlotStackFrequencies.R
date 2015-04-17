#' @title Plot stacked proportional bars
#' @name sjp.stackfrq
#' 
#' @seealso \itemize{
#'              \item \href{http://www.strengejacke.de/sjPlot/sjp.stackfrq/}{sjPlot manual: sjp.stackfrq}
#'              \item \code{\link{sjt.stackfrq}}
#'              }
#' 
#' @description Plot items (variables) of a scale as stacked proportional bars. This
#'                function is useful when several items with identical scale/categoroies
#'                should be plotted to compare the distribution of answers.
#' 
#' @note Thanks to \href{http://www.clas.ufl.edu/users/forrest/}{Forrest Stevens} for bug fixes.
#' 
#' @param items A data frame with each column representing one likert-item.
#' @param legendLabels A list or vector of strings that indicate the likert-scale-categories and which
#'          appear as legend text.
#' @param sort.frq Indicates whether the \code{items} should be ordered by
#'          by highest count of first or last category of \code{items}.
#'          \itemize{
#'            \item Use \code{"first.asc"} to order ascending by lowest count of first category,
#'            \item \code{"first.desc"} to order descending by lowest count of first category,
#'            \item \code{"last.asc"} to order ascending by lowest count of last category,
#'            \item \code{"last.desc"} to order descending by lowest count of last category,
#'            \item or \code{NULL} (default) for no sorting.
#'          }
#' @param weightBy A weight factor that will be applied to weight all cases from \code{items}.
#'          Must be a vector of same length as \code{nrow(items)}. Default is \code{NULL}, so no weights are used.
#' @param weightByTitleString If a weight factor is supplied via the parameter \code{weightBy}, the diagram's title
#'          may indicate this with a remark. Default is \code{NULL}, so the diagram's title will not be modified when
#'          cases are weighted. Use a string as parameter, e.g.: \code{weightByTitleString=" (weighted)"}.
#' @param hideLegend Indicates whether legend (guide) should be shown or not.
#' @param title Title of the diagram, plotted above the whole diagram panel.
#' @param legendTitle Title of the diagram's legend.
#' @param includeN If \code{TRUE} (default), the N of each item is included into axis labels.
#' @param geom.colors User defined color palette for geoms. If specified, must either be vector with color values 
#'          of same length as groups defined in \code{legendLabels}, or a specific color palette code (see below).
#'          \itemize{
#'            \item If not specified, the sequential \code{"Blues"} color brewer palette will be used.
#'            \item If \code{"gs"}, a greyscale will be used.
#'            \item If \code{geom.colors} is any valid color brewer palette name, the related \href{http://colorbrewer2.org}{color brewer} palette will be used. Use \code{display.brewer.all()} from the \code{RColorBrewer} package to view all available palette names.
#'          }
#'          Else specify your own color values as vector (e.g. \code{geom.colors=c("#f00000", "#00ff00", "#0080ff")}).
#' @param geom.size size resp. width of the geoms (bar width)
#' @param axisLabels.y Labels for the y-axis (the labels of the \code{items}). These parameters must
#'          be passed as list! Example: \code{axisLabels.y=list(c("Q1", "Q2", "Q3"))}
#'          Axis labels will automatically be detected, when they have
#'          a variable label attribute (see \code{\link[sjmisc]{set_var_labels}}) for details).
#' @param breakTitleAt Wordwrap for diagram title. Determines how many chars of the title are displayed in
#'          one line and when a line break is inserted into the title.
#' @param breakLabelsAt Wordwrap for diagram labels. Determines how many chars of the category labels are displayed in 
#'          one line and when a line break is inserted.
#' @param breakLegendTitleAt Wordwrap for diagram legend title. Determines how many chars of the legend's title 
#'          are displayed in one line and when a line break is inserted.
#' @param breakLegendLabelsAt Wordwrap for diagram legend labels. Determines how many chars of the legend labels are 
#'          displayed in one line and when a line break is inserted.
#' @param gridBreaksAt Sets the breaks on the y axis, i.e. at every n'th position a major
#'          grid is being printed. Valid values range from 0 to 1.
#' @param expand.grid If \code{TRUE} (default), the diagram has margins, i.e. the y-axis is not exceeded
#'          to the diagram's boundaries.
#' @param axisTitle.x A label for the x axis. Useful when plotting histograms with metric scales where no category labels
#'          are assigned to the x axis.
#' @param axisTitle.y A label for the y axis. Useful when plotting histograms with metric scales where no category labels
#'          are assigned to the y axis.
#' @param showValueLabels Whether counts and percentage values should be plotted to each bar.
#' @param labelDigits The amount of digits for rounding \code{value.labels}. Default is 1, 
#'          i.e. value labels have 1 digit after decimal point.
#' @param showPercentageAxis If \code{TRUE} (default), the percentage values at the x-axis are shown.
#' @param jitterValueLabels If \code{TRUE}, the value labels on the bars will be "jittered", i.e. they have
#'          alternating vertical positions to avoid overlapping of labels in case bars are
#'          very short. Default is \code{FALSE}.
#' @param showItemLabels Whether x axis text (category names) should be shown or not.
#' @param showSeparatorLine If \code{TRUE}, a line is drawn to visually "separate" each bar in the diagram.
#' @param separatorLineColor The color of the separator line. only applies, if \code{showSeparatorLine} is \code{TRUE}.
#' @param separatorLineSize The size of the separator line. only applies, if \code{showSeparatorLine} is \code{TRUE}.
#' @param coord.flip If \code{TRUE}, the x and y axis are swapped.
#' @param printPlot If \code{TRUE} (default), plots the results as graph. Use \code{FALSE} if you don't
#'          want to plot any graphs. In either case, the ggplot-object will be returned as value.
#' @return (Insisibily) returns the ggplot-object with the complete plot (\code{plot}) as well as the data frame that
#'           was used for setting up the ggplot-object (\code{df}).
#' 
#' @examples
#' # -------------------------------
#' # random sample
#' # -------------------------------
#' # prepare data for 4-category likert scale, 5 items
#' Q1 <- as.factor(sample(1:4, 500, replace = TRUE, prob = c(0.2, 0.3, 0.1, 0.4)))
#' Q2 <- as.factor(sample(1:4, 500, replace = TRUE, prob = c(0.5, 0.25, 0.15, 0.1)))
#' Q3 <- as.factor(sample(1:4, 500, replace = TRUE, prob = c(0.25, 0.1, 0.4, 0.25)))
#' Q4 <- as.factor(sample(1:4, 500, replace = TRUE, prob = c(0.1, 0.4, 0.4, 0.1)))
#' Q5 <- as.factor(sample(1:4, 500, replace = TRUE, prob = c(0.35, 0.25, 0.15, 0.25)))
#' 
#' likert_4 <- data.frame(Q1, Q2, Q3, Q4, Q5)
#' 
#' # create labels
#' levels_4 <- list(c("Independent", 
#'                    "Slightly dependent", 
#'                    "Dependent", 
#'                    "Severely dependent"))
#' 
#' # plot stacked frequencies of 5 (ordered) item-scales
#' sjp.stackfrq(likert_4, legendLabels = levels_4)
#' 
#' 
#' # -------------------------------
#' # Data from the EUROFAMCARE sample dataset
#' # -------------------------------
#' library(sjmisc)
#' data(efc)
#' 
#' # recveive first item of COPE-index scale
#' start <- which(colnames(efc) == "c82cop1")
#' 
#' # recveive first item of COPE-index scale
#' end <- which(colnames(efc) == "c90cop9")
#' 
#' # retrieve variable and value labels
#' varlabs <- get_var_labels(efc)
#' vallabs <- get_val_labels(efc)
#' 
#' # create value labels. We need just one variable of
#' # the COPE-index scale because they have all the same
#' # level / categorie / value labels
#' levels <- vallabs['c82cop1']
#' 
#' # create item labels
#' items <- list(varlabs[c(start:end)])
#' 
#' sjp.stackfrq(efc[, c(start:end)], 
#'              legendLabels = levels,
#'              axisLabels.y = items, 
#'              jitterValueLabels = TRUE)
#' 
#' # -------------------------------
#' # auto-detection of labels
#' # -------------------------------
#' efc <- set_var_labels(efc, varlabs)
#' sjp.stackfrq(efc[, c(start:end)])
#' 
#' 
#' @import ggplot2
#' @import dplyr
#' @import sjmisc
#' @importFrom scales percent
#' @export
sjp.stackfrq <- function(items,
                        legendLabels=NULL,
                        sort.frq=NULL,
                        weightBy=NULL,
                        weightByTitleString=NULL,
                        hideLegend=FALSE,
                        title=NULL,
                        legendTitle=NULL,
                        includeN=TRUE,
                        axisLabels.y=NULL,
                        breakTitleAt=50, 
                        breakLabelsAt=30, 
                        breakLegendTitleAt=30, 
                        breakLegendLabelsAt=28,
                        gridBreaksAt=0.2,
                        expand.grid=FALSE,
                        geom.size=0.5, 
                        geom.colors="Blues",
                        axisTitle.x=NULL,
                        axisTitle.y=NULL,
                        showValueLabels=TRUE,
                        labelDigits = 1,
                        showPercentageAxis=TRUE,
                        jitterValueLabels=FALSE,
                        showItemLabels=TRUE,
                        showSeparatorLine=FALSE,
                        separatorLineColor="grey80",
                        separatorLineSize=0.3,
                        coord.flip=TRUE,
                        printPlot=TRUE) {
  # --------------------------------------------------------
  # check sorting
  # --------------------------------------------------------
  if (!is.null(sort.frq)) {
    if (sort.frq == "first.asc") {
      sort.frq  <- "first"
      reverseOrder <- FALSE
    } else if (sort.frq == "first.desc") {
      sort.frq  <- "first"
      reverseOrder <- TRUE
    } else if (sort.frq == "last.asc") {
      sort.frq  <- "last"
      reverseOrder <- TRUE
    } else if (sort.frq == "last.desc") {
      sort.frq  <- "last"
      reverseOrder <- FALSE
    } else {
      sort.frq  <- NULL
      reverseOrder <- FALSE
    }
    
  } else {
    reverseOrder <- FALSE
  }
  # --------------------------------------------------------
  # try to automatically set labels is not passed as parameter
  # --------------------------------------------------------
  if (is.null(legendLabels)) legendLabels <- sjmisc:::autoSetValueLabels(items[, 1])
  if (is.null(axisLabels.y)) {
    axisLabels.y <- c()
    # if yes, iterate each variable
    for (i in 1:ncol(items)) {
      # retrieve variable name attribute
      vn <- sjmisc:::autoSetVariableLabels(items[, i])
      # if variable has attribute, add to variableLabel list
      if (!is.null(vn)) {
        axisLabels.y <- c(axisLabels.y, vn)
      } else {
        # else break out of loop
        axisLabels.y <- NULL
        break
      }
    }
  }
  # --------------------------------------------------------
  # If axisLabels.y were not defined, simply use column names
  # --------------------------------------------------------
  if (is.null(axisLabels.y)) axisLabels.y <- colnames(items)
  # --------------------------------------------------------
  # unlist labels
  # --------------------------------------------------------
  if (!is.null(axisLabels.y) && is.list(axisLabels.y)) axisLabels.y <- unlistlabels(axisLabels.y)
  if (!is.null(legendLabels) && is.list(legendLabels)) legendLabels <- unlistlabels(legendLabels)
  if (is.null(legendLabels)) legendLabels <- c(as.character(sort(unique(items[, 1]))))
  # --------------------------------------------------------
  # Check whether N of each item should be included into
  # axis labels
  # --------------------------------------------------------
  if (includeN && !is.null(axisLabels.y)) {
    for (i in 1:length(axisLabels.y)) {
      axisLabels.y[i] <- paste(axisLabels.y[i], 
                               sprintf(" (n=%i)", length(na.omit(items[, i]))), 
                               sep = "")
    }
  }
  # -----------------------------------------------
  # if we have legend labels, we know the exact
  # amount of groups
  # -----------------------------------------------
  countlen <- length(legendLabels)
  # -----------------------------------------------
  # create cross table for stats, summary etc.
  # and weight variable. do this for each item that was
  # passed as parameter
  #---------------------------------------------------
  mydat <- c()
  # ----------------------------
  # determine minimum value. if 0, add one, because
  # vector indexing starts with 1
  # ----------------------------
  if (any(apply(items, c(1, 2), is.factor)) || any(apply(items, c(1, 2), is.character))) {
    diff <- ifelse(min(apply(items, c(1, 2), as.numeric), na.rm = TRUE) == 0, 1, 0)
  } else {
    diff <- ifelse(min(items, na.rm = TRUE) == 0, 1, 0)
  }
  # iterate item-list
  for (i in 1:ncol(items)) {
    # get each single items
    variable <- items[ ,i]
    # -----------------------------------------------
    # create proportional table so we have the percentage
    # values that should be used as y-value for the bar charts
    # We now have a data frame with categories, group-association
    # and percentage values (i.e. each cell as separate row in the
    # data frame)
    # -----------------------------------------------
    # check whether counts should be weighted or not
    if (is.null(weightBy)) {
      df <- as.data.frame(prop.table(table(variable)))
    } else {
      df <- as.data.frame(prop.table(round(xtabs(weightBy ~ variable), 0)))
    }
    # give columns names
    names(df) <- c("var", "prc")
    # need to be numeric, so percentage values (see below) are
    # correctly assigned, i.e. missing categories are considered
    df$var <- sjmisc::to_value(df$var, keep.labels = F) + diff # if categories start with zero, fix this here
    # Create a vector of zeros 
    prc <- rep(0, countlen)
    # Replace the values in prc for those indices which equal df$var
    prc[df$var] <- df$prc
    # create new data frame. We now have a data frame with all
    # variable categories abd their related percentages, including
    # zero counts, but no(!) missings!
    mydf <- data.frame(grp = i, 
                       cat = 1:countlen, 
                       prc)
    # now, append data frames
    mydat <- data.frame(rbind(mydat, mydf))
  }
  # ----------------------------
  # make sure group and count variable 
  # are factor values
  # ----------------------------
  mydat$grp <- as.factor(mydat$grp)
  mydat$cat <- as.factor(mydat$cat)
  # add half of Percentage values as new y-position for stacked bars
  mydat <- mydat %>% 
    dplyr::group_by(grp) %>% 
    dplyr::mutate(ypos = cumsum(prc) - 0.5 * prc) %>% 
    dplyr::arrange(grp)
  # --------------------------------------------------------
  # Caculate vertical adjustment to avoid overlapping labels
  # --------------------------------------------------------
  jvert <- rep(c(1.1, -0.1), length.out = length(unique(mydat$cat)))
  jvert <- rep(jvert, length.out = nrow(mydat))
  # --------------------------------------------------------
  # Prepare and trim legend labels to appropriate size
  # --------------------------------------------------------
  # wrap legend text lines
  legendLabels <- sjmisc::word_wrap(legendLabels, breakLegendLabelsAt)    
  # check whether we have a title for the legend
  # if yes, wrap legend title line
  if (!is.null(legendTitle)) legendTitle <- sjmisc::word_wrap(legendTitle, breakLegendTitleAt)
  # check length of diagram title and split longer string at into new lines
  # every 50 chars
  if (!is.null(title)) {
    # if we have weighted values, say that in diagram's title
    if (!is.null(weightByTitleString)) title <- paste0(title, weightByTitleString)
    title <- sjmisc::word_wrap(title, breakTitleAt)    
  }
  # check length of x-axis-labels and split longer strings at into new lines
  # every 10 chars, so labels don't overlap
  if (!is.null(axisLabels.y)) axisLabels.y <- sjmisc::word_wrap(axisLabels.y, breakLabelsAt)    
  # ----------------------------
  # Check if ordering was requested
  # ----------------------------
  if (!is.null(sort.frq)) {
    # order by first cat
    if (sort.frq == "first") {
      facord <- order(mydat$prc[which(mydat$cat == 1)])
    } else {
      # order by last cat
      facord <- order(mydat$prc[which(mydat$cat == countlen)])
    }
    # create dummy vectors from 1 to itemlength
    dummy1 <- dummy2 <- c(1:length(facord))
    # facords holds the ordered item indices! we now need to
    # change the original item-index with its ordered position index.
    # example:
    # we have 4 items, and they may be ordered like this:
    # 1 3 4 2
    # so the first item is the one with the lowest count , item 3 is on second postion, 
    # item 4 is on third position and item 2 is the last item (with highest count)
    # we now need their order as subsequent vector: 1 4 2 3
    # (i.e. item 1 is on first pos, item 2 is on fourth pos, item 3 is on
    # second pos and item 4 is on third pos in order)
    if (reverseOrder) {
      dummy2[rev(facord)] <- dummy1
    } else {
      dummy2[facord] <- dummy1
    }
    # now we have the order of either lowest to highest counts of first
    # or last category of "items". We now need to repeat these values as 
    # often as we have answer categories
    orderedrow <- unlist(tapply(dummy2, 1:length(dummy2), function (x) rep(x, countlen)))
    # replace old grp-order by new order
    mydat$grp <- as.factor(orderedrow)
    # reorder axis labels as well
    axisLabels.y <- axisLabels.y[order(dummy2)]
  }
  # --------------------------------------------------------
  # check if category-oder on x-axis should be reversed
  # change category label order then
  # --------------------------------------------------------
  if (reverseOrder && is.null(sort.frq)) axisLabels.y <- rev(axisLabels.y)
  # --------------------------------------------------------
  # define vertical position for labels
  # --------------------------------------------------------
  if (coord.flip) {
    # if we flip coordinates, we have to use other parameters
    # than for the default layout
    vert <- 0.35
  } else {
    vert <- waiver()
  }
  # --------------------------------------------------------
  # set diagram margins
  # --------------------------------------------------------
  if (expand.grid) {
    expgrid <- waiver()
  } else {
    expgrid <- c(0, 0)
  }
  # --------------------------------------------------------
  # Set value labels and label digits
  # --------------------------------------------------------
  mydat$labelDigits <- labelDigits
  if (showValueLabels) {
    if (jitterValueLabels) {
      ggvaluelabels <-  geom_text(aes(y = ypos, label = sprintf("%.*f%%", labelDigits, 100 * prc)),
                                  vjust = jvert)
    } else {
      ggvaluelabels <-  geom_text(aes(y = ypos, label = sprintf("%.*f%%", labelDigits, 100 * prc)),
                                  vjust = vert)
    }
  } else {
    ggvaluelabels <-  geom_text(label = "")
  }
  # --------------------------------------------------------
  # Set up grid breaks
  # --------------------------------------------------------
  if (is.null(gridBreaksAt)) {
    gridbreaks <- waiver()
  } else {
    gridbreaks <- c(seq(0, 1, by = gridBreaksAt))
  }
  # --------------------------------------------------------
  # check if category-oder on x-axis should be reversed
  # change x axis order then
  # --------------------------------------------------------
  if (reverseOrder && is.null(sort.frq)) {
    baseplot <- ggplot(mydat, aes(x = rev(grp), y = prc, fill = cat))
  } else {
    baseplot <- ggplot(mydat, aes(x = grp, y = prc, fill = cat))
  }  
  baseplot <- baseplot +
    # plot bar chart
    geom_bar(stat = "identity", position = "stack", width = geom.size)
  # --------------------------------------------------------
  # check whether bars should be visually separated by an 
  # additional separator line
  # --------------------------------------------------------
  if (showSeparatorLine) {
    baseplot <- baseplot +
      geom_vline(x = c(seq(1.5, length(items), by = 1)), 
                 size = separatorLineSize, 
                 colour = separatorLineColor)
  }
  # -----------------
  # show/hide percentage values on x axis
  # ----------------------------
  if (!showPercentageAxis) percent <- NULL
  baseplot <- baseplot +
    # show absolute and percentage value of each bar.
    ggvaluelabels +
    # no additional labels for the x- and y-axis, only diagram title
    labs(title = title, x = axisTitle.x, y = axisTitle.y, fill = legendTitle) +
    # print value labels to the x-axis.
    # If parameter "axisLabels.y" is NULL, the category numbers (1 to ...) 
    # appear on the x-axis
    scale_x_discrete(labels = axisLabels.y) +
    # set Y-axis, depending on the calculated upper y-range.
    # It either corresponds to the maximum amount of cases in the data set
    # (length of var) or to the highest count of var's categories.
    scale_y_continuous(breaks = gridbreaks, 
                       limits = c(0, 1), 
                       expand = expgrid, 
                       labels = percent)
  # check whether coordinates should be flipped, i.e.
  # swap x and y axis
  if (coord.flip) baseplot <- baseplot + coord_flip()
  # ---------------------------------------------------------
  # set geom colors
  # ---------------------------------------------------------
  baseplot <- sj.setGeomColors(baseplot, 
                               geom.colors, 
                               length(legendLabels), 
                               ifelse(hideLegend == TRUE, FALSE, TRUE), 
                               legendLabels)
  # ---------------------------------------------------------
  # Check whether ggplot object should be returned or plotted
  # ---------------------------------------------------------
  if (printPlot) plot(baseplot)
  # -------------------------------------
  # return results
  # -------------------------------------
  invisible (structure(class = "sjpstackfrq",
                       list(plot = baseplot,
                            df = mydat)))
}
