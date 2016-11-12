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
#' @param items \code{data.frame} with each column representing one item.
#' @param sort.frq indicates whether the \code{items} should be ordered by
#'          by highest count of first or last category of \code{items}.
#'          \describe{
#'            \item{\code{"first.asc"}}{to order ascending by lowest count of first category,}
#'            \item{\code{"first.desc"}}{to order descending by lowest count of first category,}
#'            \item{\code{"last.asc"}}{to order ascending by lowest count of last category,}
#'            \item{\code{"last.desc"}}{to order descending by lowest count of last category,}
#'            \item{\code{NULL}}{(default) for no sorting.}
#'          }
#' @param show.prc If \code{TRUE} (default), the percentage values at the x-axis are shown.
#' @return (Insisibily) returns the ggplot-object with the complete plot (\code{plot}) as well as the data frame that
#'           was used for setting up the ggplot-object (\code{df}).
#' 
#' @inheritParams sjp.grpfrq
#' @inheritParams sjp.frq
#' @inheritParams sjp.glmer
#' 
#' @examples
#' # random data for 4-category likert scale, 5 items
#' Q1 <- as.factor(sample(1:4, 500, replace = TRUE, prob = c(0.2, 0.3, 0.1, 0.4)))
#' Q2 <- as.factor(sample(1:4, 500, replace = TRUE, prob = c(0.5, 0.25, 0.15, 0.1)))
#' Q3 <- as.factor(sample(1:4, 500, replace = TRUE, prob = c(0.25, 0.1, 0.4, 0.25)))
#' Q4 <- as.factor(sample(1:4, 500, replace = TRUE, prob = c(0.1, 0.4, 0.4, 0.1)))
#' Q5 <- as.factor(sample(1:4, 500, replace = TRUE, prob = c(0.35, 0.25, 0.15, 0.25)))
#' 
#' likert_4 <- data.frame(Q1, Q2, Q3, Q4, Q5)
#' 
#' # create labels
#' levels_4 <- c("Independent", "Slightly dependent", 
#'               "Dependent", "Severely dependent")
#' 
#' # plot stacked frequencies of 5 (ordered) item-scales
#' sjp.stackfrq(likert_4, legend.labels = levels_4)
#' 
#' # -------------------------------
#' # Data from the EUROFAMCARE sample dataset
#' # -------------------------------
#' library(sjmisc)
#' data(efc)
#' # recveive first item of COPE-index scale
#' start <- which(colnames(efc) == "c82cop1")
#' # recveive first item of COPE-index scale
#' end <- which(colnames(efc) == "c90cop9")
#' # auto-detection of labels
#' sjp.stackfrq(efc[, c(start:end)])
#' 
#' 
#' @import ggplot2
#' @importFrom dplyr group_by mutate arrange
#' @importFrom scales percent
#' @importFrom stats na.omit xtabs
#' @export
sjp.stackfrq <- function(items,
                         title = NULL,
                         legend.title = NULL,
                         legend.labels = NULL,
                         axis.titles = NULL,
                         axis.labels = NULL,
                         weight.by = NULL,
                         sort.frq = NULL,
                         wrap.title = 50,
                         wrap.labels = 30,
                         wrap.legend.title = 30,
                         wrap.legend.labels = 28,
                         geom.size = 0.5,
                         geom.colors = "Blues",
                         show.values = TRUE,
                         show.n = TRUE,
                         show.prc = TRUE,
                         show.legend = TRUE,
                         grid.breaks = 0.2,
                         expand.grid = FALSE,
                         digits = 1,
                         vjust = "center",
                         coord.flip = TRUE,
                         prnt.plot = TRUE) {
  # --------------------------------------------------------
  # check param. if we have a single vector instead of
  # a data frame with several items, convert vector to data frame
  # --------------------------------------------------------
  if (!is.data.frame(items) && !is.matrix(items)) items <- as.data.frame(items)
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
  # try to automatically set labels if not passed as parameter
  # --------------------------------------------------------
  if (is.null(legend.labels)) 
    legend.labels <- sjmisc::get_labels(items[[1]], attr.only = F,
                                        include.values = NULL, include.non.labelled = T)
  if (is.null(axis.labels)) {
    axis.labels <- c()
    # if yes, iterate each variable
    for (i in seq_len(ncol(items))) {
      # retrieve variable name attribute
      axis.labels <- c(axis.labels, sjmisc::get_label(items[[i]], def.value = colnames(items)[i]))
    }
  }
  # --------------------------------------------------------
  # unname labels, if necessary, so we have a simple
  # character vector
  # --------------------------------------------------------
  if (!is.null(names(axis.labels))) axis.labels <- as.vector(axis.labels)
  # --------------------------------------------------------
  # unname labels, if necessary, so we have a simple
  # character vector
  # --------------------------------------------------------
  if (!is.null(legend.labels) && !is.null(names(legend.labels))) legend.labels <- as.vector(legend.labels)
  # --------------------------------------------------------
  # if we have no legend labels, we iterate all data frame's
  # columns to find all unique items of the data frame.
  # In case one item has missing categories, this may be
  # "compensated" by looking at all items, so we have the
  # actual values of all items.
  # --------------------------------------------------------
  if (is.null(legend.labels)) {
    legend.labels <- as.character(sort(unique(unlist(
      apply(items, 2, function(x) unique(stats::na.omit(x)))))))
  }
  # --------------------------------------------------------
  # Check whether N of each item should be included into
  # axis labels
  # --------------------------------------------------------
  if (show.n) {
    for (i in seq_len(length(axis.labels))) {
      axis.labels[i] <- paste(axis.labels[i], 
                              sprintf(" (n=%i)", length(stats::na.omit(items[[i]]))), 
                              sep = "")
    }
  }
  # -----------------------------------------------
  # if we have legend labels, we know the exact
  # amount of groups
  # -----------------------------------------------
  countlen <- length(legend.labels)
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
  for (i in seq_len(ncol(items))) {
    # get each single items
    variable <- items[[i]]
    # -----------------------------------------------
    # create proportional table so we have the percentage
    # values that should be used as y-value for the bar charts
    # We now have a data frame with categories, group-association
    # and percentage values (i.e. each cell as separate row in the
    # data frame)
    # -----------------------------------------------
    # check whether counts should be weighted or not
    if (is.null(weight.by)) {
      df <- as.data.frame(prop.table(table(variable)))
    } else {
      df <- as.data.frame(prop.table(round(stats::xtabs(weight.by ~ variable), 0)))
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
    mydf <- data.frame(grp = i, cat = seq_len(countlen), prc)
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
  # Prepare and trim legend labels to appropriate size
  # --------------------------------------------------------
  # wrap legend text lines
  legend.labels <- sjmisc::word_wrap(legend.labels, wrap.legend.labels)    
  # check whether we have a title for the legend
  # if yes, wrap legend title line
  if (!is.null(legend.title)) legend.title <- sjmisc::word_wrap(legend.title, wrap.legend.title)
  # check length of diagram title and split longer string at into new lines
  # every 50 chars
  if (!is.null(title)) title <- sjmisc::word_wrap(title, wrap.title)    
  # check length of x-axis-labels and split longer strings at into new lines
  # every 10 chars, so labels don't overlap
  if (!is.null(axis.labels)) axis.labels <- sjmisc::word_wrap(axis.labels, wrap.labels)    
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
    dummy1 <- dummy2 <- seq_len(length(facord))
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
    orderedrow <- unlist(tapply(dummy2, seq_len(length(dummy2)), function(x) rep(x, countlen)))
    # replace old grp-order by new order
    mydat$grp <- as.factor(orderedrow)
    # reorder axis labels as well
    axis.labels <- axis.labels[order(dummy2)]
  }
  # --------------------------------------------------------
  # check if category-oder on x-axis should be reversed
  # change category label order then
  # --------------------------------------------------------
  if (reverseOrder && is.null(sort.frq)) axis.labels <- rev(axis.labels)
  # --------------------------------------------------------
  # set diagram margins
  # --------------------------------------------------------
  if (expand.grid) {
    expgrid <- ggplot2::waiver()
  } else {
    expgrid <- c(0, 0)
  }
  # --------------------------------------------------------
  # Set value labels and label digits
  # --------------------------------------------------------
  mydat$digits <- digits
  if (show.values) {
    ggvaluelabels <-  geom_text(aes(y = ypos, label = sprintf("%.*f%%", digits, 100 * prc)),
                                vjust = vjust)
  } else {
    ggvaluelabels <-  geom_text(aes(y = ypos), label = "")
  }
  # --------------------------------------------------------
  # Set up grid breaks
  # --------------------------------------------------------
  if (is.null(grid.breaks)) {
    gridbreaks <- ggplot2::waiver()
  } else {
    gridbreaks <- c(seq(0, 1, by = grid.breaks))
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
    geom_bar(stat = "identity", position = position_stack(reverse = TRUE), width = geom.size)
  # -----------------
  # show/hide percentage values on x axis
  # ----------------------------
  if (show.prc)
    perc.val <- scales::percent
  else
    perc.val <- NULL
  # -----------------
  # start plot here
  # ----------------------------
  baseplot <- baseplot +
    # show absolute and percentage value of each bar.
    ggvaluelabels +
    # no additional labels for the x- and y-axis, only diagram title
    labs(title = title, x = axisTitle.x, y = axisTitle.y, fill = legend.title) +
    # print value labels to the x-axis.
    # If parameter "axis.labels" is NULL, the category numbers (1 to ...) 
    # appear on the x-axis
    scale_x_discrete(labels = axis.labels) +
    # set Y-axis, depending on the calculated upper y-range.
    # It either corresponds to the maximum amount of cases in the data set
    # (length of var) or to the highest count of var's categories.
    scale_y_continuous(breaks = gridbreaks, 
                       limits = c(0, 1), 
                       expand = expgrid, 
                       labels = perc.val)
  # check whether coordinates should be flipped, i.e.
  # swap x and y axis
  if (coord.flip) baseplot <- baseplot + coord_flip()
  # ---------------------------------------------------------
  # set geom colors
  # ---------------------------------------------------------
  baseplot <- sj.setGeomColors(baseplot, 
                               geom.colors, 
                               length(legend.labels), 
                               show.legend, 
                               legend.labels)
  # ---------------------------------------------------------
  # Check whether ggplot object should be returned or plotted
  # ---------------------------------------------------------
  if (prnt.plot) graphics::plot(baseplot)
  # -------------------------------------
  # return results
  # -------------------------------------
  invisible(structure(class = "sjpstackfrq",
                      list(plot = baseplot,
                           df = mydat)))
}
