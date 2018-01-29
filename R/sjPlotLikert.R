# bind global variables
utils::globalVariables(c("offset"))

#' @title Plot likert scales as centered stacked bars
#' @name sjp.likert
#'
#' @seealso \href{http://www.strengejacke.de/sjPlot/sjp.likert/}{sjPlot manual: sjp.likert}
#'
#' @description Plot likert scales as centered stacked bars.
#'
#' @note Note that only even numbers of categories are possible to plot, so the "positive"
#'        and "negative" values can be splitted into two halfs. A neutral category (like "don't know")
#'        can be used, but must be indicated by \code{cat.neutral}. \cr \cr
#'        The \code{catcount}-argument indicates how many item categories are in the
#'        Likert scale. Normally, this argument can be ignored because the amount of
#'        valid categories is retrieved automatically. However, sometimes (for instance,
#'        if a certain category is missing in all items), auto-detection of the amount
#'        of categories fails. In such cases, specify the amount of categories
#'        with the \code{catcount}-argument.
#'
#' @param catcount Optional, amount of categories of \code{items} (e.g. \emph{"strongly disagree",
#'          "disagree", "agree"} and \emph{"strongly agree"} would be \code{catcount = 4}).
#'          Note that this argument only applies to "valid" answers, i.e. if you
#'          have an additional neutral category (see \code{cat.neutral}) like \emph{"don't know"},
#'          this won't count for \code{catcount} (e.g. "strongly disagree",
#'          "disagree", "agree", "strongly agree" and neutral category "don't know"
#'          would still mean that \code{catcount = 4}). See 'Note'.
#' @param cat.neutral If there's a neutral category (like "don't know" etc.), specify
#'          the index number (value) for this category. Else, set \code{cat.neutral = NULL} (default).
#'          The proportions of neutral category answers are plotted as grey bars on the left side of
#'          the figure.
#' @param sort.frq Indicates whether the items of \code{items} should be ordered by
#'          total sum of positive or negative answers.
#'          \describe{
#'            \item{\code{"pos.asc"}}{to order ascending by sum of positive answers}
#'            \item{\code{"pos.desc"}}{to order descending by sum of positive answers}
#'            \item{\code{"neg.asc"}}{for sorting ascending negative answers}
#'            \item{\code{"neg.desc"}}{for sorting descending negative answers}
#'            \item{\code{NULL}}{(default) for no sorting}
#'          }
#' @param reverse.colors Logical, if \code{TRUE}, the color scale from \code{geom.colors} will be reversed,
#'          so positive and negative values switch colors.
#' @param cat.neutral.color Color of the neutral category, if plotted (see \code{cat.neutral}).
#' @param intercept.line.color Color of the vertical intercept line that divides positive and negative values.
#' @param values Determines style and position of percentage value labels on the bars:
#'          \describe{
#'            \item{\code{"show"}}{(default) shows percentage value labels in the middle of each category bar}
#'            \item{\code{"hide"}}{hides the value labels, so no percentage values on the bars are printed}
#'            \item{\code{"sum.inside"}}{shows the sums of percentage values for both negative and positive values and prints them inside the end of each bar}
#'            \item{\code{"sum.outide"}}{shows the sums of percentage values for both negative and positive values and prints them outside the end of each bar}
#'          }
#' @param show.prc.sign Logical, if \code{TRUE}, \%-signs for value labels are shown.
#' @param grid.range Numeric, limits of the x-axis-range, as proportion of 100.
#'          Default is 1, so the x-scale ranges from zero to 100\% on
#'          both sides from the center. You can use values beyond 1
#'          (100\%) in case bar labels are not printed because they exceed the axis range.
#'          E.g. \code{grid.range = 1.4} will set the axis from -140 to +140\%, however, only
#'          (valid) axis labels from -100 to +100\% are printed. Neutral categories are
#'          adjusted to the most left limit.
#'
#' @inheritParams sjp.grpfrq
#' @inheritParams sjp.stackfrq
#' @inheritParams sjp.glmer
#'
#' @return (Insisibily) returns the ggplot-object with the complete plot (\code{plot}) as well as the data frame that
#'           was used for setting up the ggplot-object (\code{df.neg} for the negative values,
#'           \code{df.pos} for the positive values and \code{df.neutral} for the neutral category values).
#'
#' @examples
#' library(sjmisc)
#' data(efc)
#' # find all variables from COPE-Index, which all have a "cop" in their
#' # variable name, and then plot that subset as likert-plot
#' find_var(efc, pattern = "cop", out = "df") %>% sjp.likert()
#'
#' sjp.likert(
#'   find_var(efc, pattern = "cop", out = "df"),
#'   grid.range = 1.2,
#'   expand.grid = FALSE,
#'   values = "sum.outside",
#'   show.prc.sign = TRUE
#' )
#'
#' @import ggplot2
#' @importFrom stats na.omit xtabs
#' @importFrom sjmisc is_odd set_na
#' @importFrom sjlabelled as_numeric
#' @export
sjp.likert <- function(items,
                       title = NULL,
                       legend.title = NULL,
                       legend.labels = NULL,
                       axis.titles = NULL,
                       axis.labels = NULL,
                       catcount = NULL,
                       cat.neutral = NULL,
                       sort.frq = NULL,
                       weight.by = NULL,
                       title.wtd.suffix = NULL,
                       wrap.title = 50,
                       wrap.labels = 30,
                       wrap.legend.title = 30,
                       wrap.legend.labels = 28,
                       geom.size = .6,
                       geom.colors = "BrBG",
                       cat.neutral.color = "grey70",
                       intercept.line.color = "grey50",
                       reverse.colors = FALSE,
                       values = "show",
                       show.n = TRUE,
                       show.legend = TRUE,
                       show.prc.sign = FALSE,
                       grid.range = 1,
                       grid.breaks = 0.2,
                       expand.grid = TRUE,
                       digits = 1,
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
    if (sort.frq == "pos.asc") {
      sort.frq  <- "pos"
      reverseOrder <- FALSE
    }
    if (sort.frq == "pos.desc") {
      sort.frq  <- "pos"
      reverseOrder <- TRUE
    }
    if (sort.frq == "neg.asc") {
      sort.frq  <- "neg"
      reverseOrder <- FALSE
    }
    if (sort.frq == "neg.desc") {
      sort.frq  <- "neg"
      reverseOrder <- TRUE
    }
  } else {
    reverseOrder <- FALSE
  }
  # --------------------------------------------------------
  # try to automatically set labels is not passed as argument
  # --------------------------------------------------------
  if (is.null(legend.labels)) {
    legend.labels <- sjlabelled::get_labels(items[[1]], attr.only = F,
                                        include.values = NULL, include.non.labelled = T)
  }
  if (is.null(axis.labels)) {
    # retrieve variable name attribute
    axis.labels <- unname(sjlabelled::get_label(items, def.value = colnames(items)))
  }
  # --------------------------------------------------------
  # unname labels, if necessary, so we have a simple character vector
  if (!is.null(names(axis.labels))) axis.labels <- as.vector(axis.labels)
  # --------------------------------------------------------
  # unlist/ unname axis labels
  # --------------------------------------------------------
  if (!is.null(legend.labels)) {
    # unname labels, if necessary, so we have a simple character vector
    if (!is.null(names(legend.labels))) legend.labels <- as.vector(legend.labels)
  }
  # --------------------------------------------------------
  # determine catcount
  # --------------------------------------------------------
  adding <- ifelse(is.null(cat.neutral), 0, 1)

  if (is.null(catcount)) {
    catcount <- c()

    # iterate all items
    for (i in seq_len(ncol(items))) {
      # add new unique item values to catcount, so catcount
      # finally contains all unique values of items
      catcount <- unique(c(catcount, unique(stats::na.omit(items[[i]]))))
    }

    # remove neutral category
    if (!is.null(cat.neutral)) {
      # find neutral cat value in catcount
      ncv_pos <- which(catcount == cat.neutral)
      # if not empty, remove
      if (!sjmisc::is_empty(ncv_pos)) catcount <- catcount[-ncv_pos]
    }

    # detect range of valid categories, which
    # then equals catcount
    catcount <- max(catcount) - min(catcount) + 1

    # check if category count matches category label count
    if (!is.null(legend.labels)) {
      # how many labels do we have?
      # substract 1, if we have neutral category
      lll <- length(legend.labels) - adding
      # catcount and legend label count equal?
      if (catcount < lll) {
        # warn user that detected amount of categories and supplied legend labels
        # are different.
        warning("Length of labels for item categories `legend.labels` differs from detected amount of categories. Use `catcount` argument to define amount of item categories, if plotting does not work.", call. = F)
        # adjust catcount to length of legend labels, because
        # we assume that labels represent the valid range of
        # item categories
        catcount <- lll
      }
    }

    # is catcount odd or even? make catcount even
    if (sjmisc::is_odd(catcount)) {
      # warn user about uneven category count
      warning("Detected uneven category count in items. Dropping last category.", call. = F)
      catcount <- catcount - 1
    }
  }

  # --------------------------------------------------------
  # set legend labels, if we have none yet
  # --------------------------------------------------------
  if (is.null(legend.labels)) legend.labels <- seq_len(catcount + adding)
  # --------------------------------------------------------
  # prepare data frames
  # --------------------------------------------------------
  mydat.pos <- data.frame()
  mydat.neg <- data.frame()
  mydat.dk <- data.frame()
  freq.df <- data.frame()

  # If we have neutral category in between and not as last
  # category, recode neutral category to last category
  if (!is.null(cat.neutral) && cat.neutral <= catcount) {
    # first, each other category has to be moved down one position
    # therefore, we create a pattern with values from neutral
    # category to category count
    downvote <- seq(cat.neutral, catcount + 1, by = 1)

    # now we "shift" this value pattern and make a
    # string out of it
    recode.pattern <- paste0(
      paste0(sprintf("%i=%i", c(downvote[-1], downvote[1]), downvote),
             collapse = ";"), ";else=copy"
      )

    # all factors with char labels need to be numeric,
    # else, recode won't work

    items <- purrr::modify_if(
      items,
      is_labelled_factor,
      sjlabelled::as_numeric,
      keep.labels = FALSE
    )

    # finally, recode data
    items <- sjmisc::rec(items, rec = recode.pattern, append = FALSE)

    # re-order legend labels as well
    ll.order <- c(seq_len(catcount + adding)[-cat.neutral], cat.neutral)
    legend.labels <- legend.labels[ll.order]
  }

  # loop through all likert-items
  for (i in seq_len(ncol(items))) {
    # --------------------------------------------------------
    # convert to numeric values
    # --------------------------------------------------------
    if (!is.numeric(items[[i]])) {
      # --------------------------------------------------------
      # convert non-numeric factors to numeric values
      # --------------------------------------------------------
      items[[i]] <- sjlabelled::as_numeric(items[[i]], keep.labels = F)
    }
    # --------------------------------------------------------
    # If we don't plot neutral category, but item still contains
    # that category, replace it with NA
    # --------------------------------------------------------
    if (is.null(cat.neutral) && max(items[[i]], na.rm = T) > catcount)
      items[[i]] <- sjmisc::set_na(items[[i]], na = catcount + 1, as.tag = F)
    # --------------------------------------------------------
    # create proportional frequency table
    # --------------------------------------------------------
    if (is.null(weight.by)) {
      tab <- round(prop.table(table(items[[i]])), 3)
    } else {
      tab <- round(prop.table(stats::xtabs(weight.by ~ items[[i]])), 3)
    }
    # --------------------------------------------------------
    # retrieve category number and related frequencies
    # --------------------------------------------------------
    counts <- as.numeric(tab)
    valid <- as.numeric(names(tab))
    # --------------------------------------------------------
    # create frequency vector, so zero-categories are cared for
    # --------------------------------------------------------
    freq <- rep(0, catcount + adding)
    freq[valid] <- counts
    # --------------------------------------------------------
    # append to data frame
    # --------------------------------------------------------
    if (ncol(freq.df) == 0)
      freq.df <- as.data.frame(freq)
    else {
      # check for valid rows. if we hav missing categories
      # in all items, argument "catcount" must be set, because
      # automatic detection of amount of categories does not
      # work then.
      if (length(freq) != nrow(freq.df))
        stop("Could not determine amount of item categories. Please use argument `catcount`.", call. = F)
      else
        freq.df <- as.data.frame(cbind(freq.df, freq))
    }

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
  # --------------------------------------------------------
  # determine split between pos and neg values
  # --------------------------------------------------------
  lower.half <- rev(seq(catcount / 2))
  upper.half <- 1 + catcount - lower.half
  # --------------------------------------------------------
  # sum up values to total, so we can sort items
  # --------------------------------------------------------
  sums.lower <- unname(apply(freq.df[lower.half, , drop = FALSE], 2, sum))
  sums.upper <- unname(apply(freq.df[upper.half, , drop = FALSE], 2, sum))
  # --------------------------------------------------------
  # sort items
  # --------------------------------------------------------
  if (is.null(sort.frq))
    sort.freq <- seq_len(ncol(freq.df))
  else if (sort.frq == "pos")
    sort.freq <- order(sums.lower)
  else if (sort.frq == "neg")
    sort.freq <- order(sums.upper)
  else
    sort.freq <- seq_len(ncol(freq.df))
  # --------------------------------------------------------
  # reverse item order?
  # --------------------------------------------------------
  if (!reverseOrder) sort.freq <- rev(sort.freq)
  # --------------------------------------------------------
  # save summed up y-values, for label positioning and annotation
  # --------------------------------------------------------
  ypos.sum.pos <- c()
  ypos.sum.neg <- c()
  ypos.sum.dk <- c()
  # --------------------------------------------------------
  # iterate all frequencies of the items. we have the simple
  # data rows in this data frame and now need to "split"
  # positive and negative values
  # --------------------------------------------------------
  for (i in seq_len(ncol(freq.df))) {
    # sort
    fr <- freq.df[, sort.freq[i]]
    # --------------------------------------------------------
    # positive values. we need an x-position for each item,
    # a group indicator, the frequencies (as percent value),
    # and the y position for labels.
    # --------------------------------------------------------
    mydat.pos <- as.data.frame(rbind(mydat.pos,
                                     cbind(x = i,
                                           grp = lower.half,
                                           frq = fr[lower.half],
                                           ypos = cumsum(fr[lower.half]) - 0.5 * (fr[lower.half]),
                                           ypos2 = sum(fr[lower.half]))))
    # --------------------------------------------------------
    # summed y-position for plotting the summed up frequency
    # labels
    # --------------------------------------------------------
    ypos.sum.pos <- c(ypos.sum.pos, sum(fr[lower.half]))
    # --------------------------------------------------------
    # same as above for negative values
    # --------------------------------------------------------
    mydat.neg <- as.data.frame(rbind(mydat.neg,
                                     cbind(x = i,
                                           grp = upper.half,
                                           frq = -fr[upper.half],
                                           ypos = -1 * (cumsum(fr[upper.half]) - 0.5 * (fr[upper.half])),
                                           ypos2 = -1 * sum(fr[upper.half]))))
    # summed up (cumulative) percs
    ypos.sum.neg <- c(ypos.sum.neg, -1 * sum(fr[upper.half]))
    # --------------------------------------------------------
    # same as above for neutral category, if we have any
    # --------------------------------------------------------
    if (!is.null(cat.neutral)) {
      mydat.dk <- as.data.frame(rbind(mydat.dk,
                                      cbind(x = i,
                                            grp = catcount + adding,
                                            frq = -1 + fr[catcount + adding],
                                            ypos = -1 + (fr[catcount + adding] / 2),
                                            ypos2 = -1 + fr[catcount + adding],
                                            offset = -1 * grid.range)))
      # cumulative neutral cat
      ypos.sum.dk <- c(ypos.sum.dk, -1 + fr[catcount + adding])
    }
  }
  # --------------------------------------------------------
  # x-positions for cumulative percentages
  # --------------------------------------------------------
  xpos.sum.dk <- xpos.sum.neg <- xpos.sum.pos <- seq_len(length(ypos.sum.pos))
  # --------------------------------------------------------
  # grp as factor
  # --------------------------------------------------------
  mydat.pos$grp <- as.factor(mydat.pos$grp)
  mydat.neg$grp <- as.factor(mydat.neg$grp)
  # same for neutral
  if (!is.null(cat.neutral)) {
    mydat.dk$grp <- as.factor("neutral")
    mydat.dk$geom.size <- geom.size
    mydat.dk$digits <- digits
  }
  # --------------------------------------------------------
  # label digits needed
  # --------------------------------------------------------
  mydat.neg$digits <- digits
  mydat.pos$digits <- digits
  # --------------------------------------------------------
  # Prepare and trim legend labels to appropriate size
  # --------------------------------------------------------
  # wrap legend text lines
  legend.labels <- sjmisc::word_wrap(legend.labels, wrap.legend.labels)
  # check whether we have a title for the legend
  if (!is.null(legend.title)) {
    # if yes, wrap legend title line
    legend.title <- sjmisc::word_wrap(legend.title, wrap.legend.title)
  }
  # check length of diagram title and split longer string at into new lines
  # every 50 chars
  if (!is.null(title)) {
    # if we have weighted values, say that in diagram's title
    if (!is.null(title.wtd.suffix)) {
      title <- paste(title, title.wtd.suffix, sep = "")
    }
    title <- sjmisc::word_wrap(title, wrap.title)
  }
  # --------------------------------------------------------
  # check length of x-axis-labels and split longer strings at into new lines
  # every 10 chars, so labels don't overlap
  # --------------------------------------------------------
  axis.labels <- sjmisc::word_wrap(axis.labels, wrap.labels)
  # sort labels
  axis.labels <- axis.labels[sort.freq]
  # --------------------------------------------------------
  # set diagram margins
  # --------------------------------------------------------
  if (expand.grid) {
    expgrid <- ggplot2::waiver()
  } else {
    expgrid <- c(0, 0)
  }
  # --------------------------------------------------------
  # Set up grid breaks
  # --------------------------------------------------------
  gridbreaks <- round(seq(-grid.range, grid.range, by = grid.breaks), 2)
  gridlabs <- ifelse(abs(gridbreaks) > 1, "", paste0(abs(round(100 * gridbreaks)), "%"))
  # --------------------------------------------------------
  # start plot here
  # --------------------------------------------------------
  gp <- ggplot() +
    # positive value bars
    geom_col(data = mydat.pos,
             aes_string(x = "x", y = "frq", fill = "grp"),
             width = geom.size) +
    # negative value bars
    geom_col(data = mydat.neg,
             aes_string(x = "x", y = "frq", fill = "grp"),
             width = geom.size,
             position = position_stack(reverse = T))
  # --------------------------------------------------------
  # print bar for neutral category. this is a "fake" bar created
  # with geom_rect. to make this work, we need to set the x-axis
  # to a continuous scale...
  # --------------------------------------------------------
  if (!is.null(cat.neutral)) {
    gp <- gp +
      geom_rect(data = mydat.dk, aes(xmin = x - (geom.size / 2),
                                     xmax = x + (geom.size / 2),
                                     ymin = offset,
                                     ymax = frq + (offset + 1),
                                     fill = "neutral"))
  }
  # --------------------------------------------------------
  # if we have neutral colors, we need to add the geom-color
  # to the color values.
  # --------------------------------------------------------
  if (!is.null(cat.neutral)) geom.colors <- c(geom.colors, cat.neutral.color)
  # --------------------------------------------------------
  # should percentage value labels be printed?
  # --------------------------------------------------------
  percsign <- mydat.pos$percsign <- mydat.neg$percsign <- ifelse(isTRUE(show.prc.sign), "%", "")
  if (nrow(mydat.dk) > 0) mydat.dk$percsign <- percsign
  # --------------------------------------------------------
  # creating value labels for cumulative percentages, so
  # zero-percentages are not printed
  # --------------------------------------------------------
  ypos.sum.pos.lab  <- ifelse(ypos.sum.pos > 0, sprintf("%.*f%s", digits, 100 * ypos.sum.pos, percsign), "")
  ypos.sum.neg.lab  <- ifelse(ypos.sum.neg < 0, sprintf("%.*f%s", digits, 100 * abs(ypos.sum.neg), percsign), "")
  ypos.sum.dk.lab  <- ifelse(ypos.sum.dk > -1, sprintf("%.*f%s", digits, 100 * (1 + ypos.sum.dk), percsign), "")

  if (values == "show") {
    # show them in middle of bar
    gp <- gp +
      geom_text(data = subset(mydat.pos, frq > 0),
                aes(x = x, y = ypos, label = sprintf("%.*f%s", digits, 100 * frq, percsign))) +
      geom_text(data = subset(mydat.neg, frq < 0),
                aes(x = x, y = ypos, label = sprintf("%.*f%s", digits, 100 * abs(frq), percsign)))
    if (!is.null(cat.neutral)) {
      gp <- gp +
        geom_text(data = subset(mydat.dk, frq > -1), aes(x = x, y = ypos + offset + 1, label = sprintf("%.*f%s", digits, 100 * (1 + frq), percsign)))
    }
  } else if (values == "sum.inside" || values == "sum.outside") {
    # show cumulative outside bar
    if (values == "sum.outside") {
      hort.pos <- -0.15
      hort.neg <- 1.15
      hort.dk <- -0.15
    # show cumulative inside bar
    } else {
      hort.pos <- 1.15
      hort.neg <- -0.15
      hort.dk <- 1.15
    }
    gp <- gp +
      annotate("text", x = xpos.sum.pos, y = ypos.sum.pos, hjust = hort.pos, label = ypos.sum.pos.lab) +
      annotate("text", x = xpos.sum.neg, y = ypos.sum.neg, hjust = hort.neg, label = ypos.sum.neg.lab)
    if (!is.null(cat.neutral)) {
      gp <- gp +
        annotate("text", x = xpos.sum.dk, y = ypos.sum.dk + 1 - grid.range, hjust = hort.dk, label = ypos.sum.dk.lab)
    }
  }
  # ---------------------------------------------------------
  # continues with plot
  # ---------------------------------------------------------
  gp <- gp +
    labs(title = title, x = axisTitle.x, y = axisTitle.y, fill = legend.title) +
    # ---------------------------------------------------------
    # scale x is continuous to make plotting the bar annotation
    # for neutral category work...
    # ---------------------------------------------------------
    scale_x_continuous(breaks = seq_len(ncol(freq.df)), labels = axis.labels) +
    scale_y_continuous(breaks = gridbreaks, limits = c(-grid.range, grid.range), expand = expgrid, labels = gridlabs) +
    geom_hline(yintercept = 0, color = intercept.line.color)
  # ---------------------------------------------------------
  # check whether coordinates should be flipped, i.e.
  # swap x and y axis
  # ---------------------------------------------------------
  if (coord.flip) gp <- gp + coord_flip()
  # ---------------------------------------------------------
  # set geom colors
  # ---------------------------------------------------------
  gp <- sj.setGeomColors(gp,
                         geom.colors,
                         (catcount + adding),
                         show.legend,
                         legend.labels,
                         reverse.colors)
  # ---------------------------------------------------------
  # Check whether ggplot object should be returned or plotted
  # ---------------------------------------------------------
  if (prnt.plot) suppressWarnings(graphics::plot(gp))
  # -------------------------------------
  # return results
  # -------------------------------------
  invisible(structure(class = "sjplikert",
                      list(plot = gp,
                           df.neg = mydat.neg,
                           df.pos = mydat.pos,
                           df.neutral = mydat.dk)))
}
