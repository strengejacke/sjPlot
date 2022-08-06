#' @title Plot stacked proportional bars
#' @name plot_stackfrq
#'
#' @description Plot items (variables) of a scale as stacked proportional bars. This
#'                function is useful when several items with identical scale/categoroies
#'                should be plotted to compare the distribution of answers.
#'
#' @param items Data frame, or a grouped data frame, with each column representing one item.
#' @param sort.frq Indicates whether the \code{items} should be ordered by
#'   by highest count of first or last category of \code{items}.
#'   \describe{
#'     \item{\code{"first.asc"}}{to order ascending by lowest count of first category,}
#'     \item{\code{"first.desc"}}{to order descending by lowest count of first category,}
#'     \item{\code{"last.asc"}}{to order ascending by lowest count of last category,}
#'     \item{\code{"last.desc"}}{to order descending by lowest count of last category,}
#'     \item{\code{NULL}}{(default) for no sorting.}
#'   }
#' @param show.axis.prc Logical, if \code{TRUE} (default), the percentage values at the x-axis are shown.
#' @param show.total logical, if \code{TRUE}, adds total number of cases for each
#'   group or category to the labels.
#' @param show.prc Logical, whether percentage values should be plotted or not.
#' @param show.n Logical, whether count values hould be plotted or not.
#'
#' @return A ggplot-object.
#'
#' @inheritParams plot_grpfrq
#' @inheritParams plot_frq
#' @inheritParams plot_model
#'
#' @examples
#' # Data from the EUROFAMCARE sample dataset
#' library(sjmisc)
#' data(efc)
#' # recveive first item of COPE-index scale
#' start <- which(colnames(efc) == "c82cop1")
#' # recveive first item of COPE-index scale
#' end <- which(colnames(efc) == "c90cop9")
#' # auto-detection of labels
#' plot_stackfrq(efc[, start:end])
#'
#' # works on grouped data frames as well
#' library(dplyr)
#' efc %>%
#'   group_by(c161sex) %>%
#'   select(start:end) %>%
#'   plot_stackfrq()
#'
#' @import ggplot2
#' @export
plot_stackfrq <- function(items,
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
                         show.prc = TRUE,
                         show.n = FALSE,
                         show.total = TRUE,
                         show.axis.prc = TRUE,
                         show.legend = TRUE,
                         grid.breaks = 0.2,
                         expand.grid = FALSE,
                         digits = 1,
                         vjust = "center",
                         coord.flip = TRUE) {

  # check param. if we have a single vector instead of
  # a data frame with several items, convert vector to data frame

  if (!is.data.frame(items) && !is.matrix(items)) items <- as.data.frame(items)

  pl <- NULL

  if (inherits(items, "grouped_df")) {
    # get grouped data
    grps <- get_grouped_data(items)

    # now plot everything
    for (i in seq_len(nrow(grps))) {
      # copy back labels to grouped data frame
      tmp <- sjlabelled::copy_labels(grps$data[[i]], items)

      # prepare argument list, including title
      tmp.title <- get_grouped_plottitle(items, grps, i, sep = "\n")

      # plot
      plots <- .plot_stackfrq_helper(
        items = tmp, title = tmp.title, legend.title = legend.title, legend.labels = legend.labels,
        axis.titles = axis.titles, axis.labels = axis.labels, weight.by = weight.by,
        sort.frq = sort.frq, wrap.title = wrap.title, wrap.labels = wrap.labels,
        wrap.legend.title = wrap.legend.title, wrap.legend.labels = wrap.legend.labels,
        geom.size = geom.size, geom.colors = geom.colors, show.prc = show.prc,
        show.n = show.n, show.total = show.total, show.axis.prc = show.axis.prc,
        show.legend = show.legend, grid.breaks = grid.breaks, expand.grid = expand.grid,
        digits = digits, vjust = vjust, coord.flip = coord.flip
      )

      # add plots, check for NULL results
      pl <- list(pl, plots)
    }
  } else {
    pl <- .plot_stackfrq_helper(
      items = items, title = title, legend.title = legend.title, legend.labels = legend.labels,
      axis.titles = axis.titles, axis.labels = axis.labels, weight.by = weight.by,
      sort.frq = sort.frq, wrap.title = wrap.title, wrap.labels = wrap.labels,
      wrap.legend.title = wrap.legend.title, wrap.legend.labels = wrap.legend.labels,
      geom.size = geom.size, geom.colors = geom.colors, show.prc = show.prc,
      show.n = show.n, show.total = show.total, show.axis.prc = show.axis.prc,
      show.legend = show.legend, grid.breaks = grid.breaks, expand.grid = expand.grid,
      digits = digits, vjust = vjust, coord.flip = coord.flip
    )
  }

  pl
}

.plot_stackfrq_helper <- function(
  items, title, legend.title, legend.labels, axis.titles, axis.labels,
  weight.by, sort.frq, wrap.title, wrap.labels, wrap.legend.title,
  wrap.legend.labels, geom.size, geom.colors, show.prc, show.n,
  show.total, show.axis.prc, show.legend, grid.breaks, expand.grid, digits,
  vjust, coord.flip) {

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

  # check sorting

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

  # try to automatically set labels if not passed as parameter

  if (is.null(legend.labels))
    legend.labels <- sjlabelled::get_labels(
      items[[1]],
      attr.only = F,
      values = NULL,
      non.labelled = T
    )

  if (is.null(axis.labels)) {
    axis.labels <- sjlabelled::get_label(items, def.value = colnames(items))
  }

  # unname labels, if necessary, so we have a simple
  # character vector

  if (!is.null(names(axis.labels))) axis.labels <- as.vector(axis.labels)

  # unname labels, if necessary, so we have a simple
  # character vector

  if (!is.null(legend.labels) && !is.null(names(legend.labels))) legend.labels <- as.vector(legend.labels)

  # if we have no legend labels, we iterate all data frame's
  # columns to find all unique items of the data frame.
  # In case one item has missing categories, this may be
  # "compensated" by looking at all items, so we have the
  # actual values of all items.

  if (is.null(legend.labels)) {
    legend.labels <- as.character(sort(unique(unlist(
      apply(items, 2, function(x) unique(stats::na.omit(x)), simplify = FALSE)))))
  }

  # if we have legend labels, we know the exact
  # amount of groups

  countlen <- length(legend.labels)

  # create cross table for stats, summary etc.
  # and weight variable. do this for each item that was
  # passed as parameter

  if (is.null(weight.by)) {
    dummy <- sjmisc::frq(items, show.na = TRUE)
    dummy <- lapply(dummy, function(.i) .i[-nrow(.i), ])
  } else {
    items$weights <- weight.by
    dummy <- sjmisc::frq(items, weights = items$weights, show.na = TRUE)
    dummy <- lapply(dummy, function(.i) .i[-nrow(.i), ])
  }

  dummy <- lapply(1:length(dummy), function(.i) {
    dummy[[.i]]$grp <- .i
    dummy[[.i]]$ypos <- (cumsum(dummy[[.i]]$valid.prc) - 0.5 * dummy[[.i]]$valid.prc) / 100
    dummy[[.i]]
  })

  mydat <- do.call(rbind, dummy)
  # remove NA row
  mydat <- mydat[!is.na(mydat$ypos), ]

  mydat$grp <- as.factor(mydat$grp)
  mydat$cat <- as.factor(mydat$val)
  mydat$prc <- mydat$valid.prc / 100

  # Check whether N of each item should be included into
  # axis labels

  if (show.total) {
    for (i in seq_len(length(axis.labels))) {
      axis.labels[i] <- paste(axis.labels[i],
                              sprintf(" (n=%i)", sum(dummy[[i]]$frq, na.rm = TRUE)),
                              sep = "")
    }
  }

  # Prepare and trim legend labels to appropriate size

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

  # Check if ordering was requested

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


  # check if category-oder on x-axis should be reversed
  # change category label order then

  if (reverseOrder && is.null(sort.frq)) axis.labels <- rev(axis.labels)

  # set diagram margins

  if (expand.grid) {
    expgrid <- waiver()
  } else {
    expgrid <- c(0, 0)
  }

  # Set value labels and label digits

  mydat$digits <- digits
  if (show.prc && !show.n) {
    ggvaluelabels <- geom_text(
      aes(y = .data$ypos, label = sprintf("%.*f%%", .data$digits, 100 * .data$prc)),
      vjust = vjust
    )
  } else if (show.n && !show.prc) {
    ggvaluelabels <- geom_text(
      aes(y = .data$ypos, label = sprintf("%i", as.integer(.data$frq))),
      vjust = vjust
    )
  } else if (show.n && show.prc) {
    ggvaluelabels <- geom_text(
      aes(y = .data$ypos, label = sprintf("%.*f%% (n=%i)", .data$digits, 100 * .data$prc, as.integer(.data$frq))),
      vjust = vjust
    )
  } else {
    ggvaluelabels <-  geom_text(aes(y = .data$ypos), label = "")
  }

  # Set up grid breaks

  if (is.null(grid.breaks)) {
    gridbreaks <- waiver()
  } else {
    gridbreaks <- c(seq(0, 1, by = grid.breaks))
  }

  # check if category-oder on x-axis should be reversed
  # change x axis order then

  if (reverseOrder && is.null(sort.frq)) {
    baseplot <- ggplot(mydat, aes(x = rev(.data$grp), y = .data$prc, fill = .data$cat))
  } else {
    baseplot <- ggplot(mydat, aes(x = .data$grp, y = .data$prc, fill = .data$cat))
  }

  baseplot <- baseplot +
    # plot bar chart
    geom_bar(stat = "identity", position = position_stack(reverse = TRUE), width = geom.size)

  # show/hide percentage values on x axis

  if (show.axis.prc)
    perc.val <- scales::percent
  else
    perc.val <- NULL

  # start plot here

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
                       limits = c(-0.02, 1.02),
                       expand = expgrid,
                       labels = perc.val)
  # check whether coordinates should be flipped, i.e.
  # swap x and y axis
  if (coord.flip) baseplot <- baseplot + coord_flip()

  # set geom colors

  sj.setGeomColors(
    baseplot,
    geom.colors,
    length(legend.labels),
    show.legend,
    legend.labels
  )
}
