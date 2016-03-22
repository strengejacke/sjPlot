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
#' @param items \code{\link{data.frame}} with each column representing one likert-item.
#' @param catcount optional, amount of categories of \code{items} (e.g. \emph{"strongly disagree", 
#'          "disagree", "agree"} and \emph{"strongly agree"} would be \code{catcount = 4}).
#'          Note that this argument only applies to "valid" answers, i.e. if you
#'          have an additional neutral category (see \code{cat.neutral}) like \emph{"don't know"},
#'          this won't count for \code{catcount} (e.g. "strongly disagree", 
#'          "disagree", "agree", "strongly agree" and neutral category "don't know"
#'          would still mean that \code{catcount = 4}). See 'Note'.
#' @param cat.neutral if there's a neutral category (like "don't know" etc.), specify
#'          the index number (value) for this category. Else, set \code{cat.neutral = NULL} (default).
#'          The proportions of neutral category answers are plotted as grey bars on the left side of
#'          the figure.
#' @param weightBy weight factor that will be applied to weight all cases of \code{items}.
#'          Must be a vector of same length as \code{nrow(items)}. Default is \code{NULL}, 
#'          so no weights are used.
#' @param sort.frq indicates whether the items of \code{items} should be ordered by 
#'          total sum of positive or negative answers.
#'          \describe{
#'            \item{\code{"pos.asc"}}{to order ascending by sum of positive answers}
#'            \item{\code{"pos.desc"}}{to order descending by sum of positive answers}
#'            \item{\code{"neg.asc"}}{for sorting ascending negative answers}
#'            \item{\code{"neg.desc"}}{for sorting descending negative answers}
#'            \item{\code{NULL}}{(default) for no sorting}
#'          }
#' @param geom.colors user defined color palette for geoms. If specified, must either be vector with color values 
#'          of same length as groups defined in \code{legendLabels}, or a specific color palette code.
#'          See 'Note' in \code{\link{sjp.grpfrq}}.
#' @param reverse.colors logical, if \code{TRUE}, the color scale from \code{geom.colors} will be reversed,
#'          so positive and negative values switch colors.
#' @param geom.size width of bars. Recommended values for this argument are from 0.4 to 1.5
#' @param cat.neutral.color color of the neutral category, if plotted (see \code{cat.neutral}).
#' @param intercept.line.color color of the vertical intercept line that divides positive and negative values.
#' @param legendLabels character vector with names of the 
#'          likert-scale-categories that appear as legend text.
#' @param includeN logical, if \code{TRUE} (default), the N of each item will be included in axis labels.
#' @param value.labels determines style and position of percentage value labels on the bars:
#'          \describe{
#'            \item{\code{"show"}}{(default) shows percentage value labels in the middle of each category bar}
#'            \item{\code{"hide"}}{hides the value labels, so no percentage values on the bars are printed}
#'            \item{\code{"sum.inside"}}{shows the sums of percentage values for both negative and positive values and prints them inside the end of each bar}
#'            \item{\code{"sum.outide"}}{shows the sums of percentage values for both negative and positive values and prints them outside the end of each bar}
#'          }
#' @param showPercentageSign logical, if \code{TRUE}, \%-signs for value labels are shown.
#' @param labelDigits amount of digits for rounding \code{value.labels}. Default is 1, 
#'          i.e. value labels have 1 digit after decimal point.
#' @param showItemLabels logical, whether x-axis text (category names) should be shown or not
#' @param axisLabels.y character vector with labels for the y-axis (names of the 
#'          \code{items}). Example: \code{axisLabels.y = c("Q1", "Q2", "Q3")}.
#'          Axis labels will automatically be detected, when they have
#'          label attributes (see \code{\link[sjmisc]{set_label}} for details).
#' @param gridRange numeric, limits of the x-axis-range, as proportion of 100. 
#'          Default is 1, so the x-scale ranges from zero to 100\% on 
#'          both sides from the center. You can use values beyond 1
#'          (100\%) in case bar labels are not printed because they exceed the axis range.
#'          E.g. \code{gridRange = 1.4} will set the axis from -140 to +140\%, however, only
#'          (valid) axis labels from -100 to +100\% are printed. Neutral categories are
#'          adjusted to the most left limit.
#' @param axisTitle.x title for the x-axis. Default is \code{NULL} (no title).
#' @param axisTitle.y title for the y-axis. Default is \code{NULL} (no title).
#' 
#' @inheritParams sjp.grpfrq
#' 
#' @return (Insisibily) returns the ggplot-object with the complete plot (\code{plot}) as well as the data frame that
#'           was used for setting up the ggplot-object (\code{df.neg} for the negative values,
#'           \code{df.pos} for the positive values and \code{df.neutral} for the neutral category values).
#' 
#' @examples
#' # prepare data for dichotomous likert scale, 5 items
#' likert_2 <- data.frame(
#'   as.factor(sample(1:2, 500, replace = TRUE, prob = c(0.3,0.7))),
#'   as.factor(sample(1:2, 500, replace = TRUE, prob = c(0.6,0.4))),
#'   as.factor(sample(1:2, 500, replace = TRUE, prob = c(0.25,0.75))),
#'   as.factor(sample(1:2, 500, replace = TRUE, prob = c(0.9,0.1))),
#'   as.factor(sample(1:2, 500, replace = TRUE, prob = c(0.35,0.65))))
#'   
#' # create labels
#' levels_2 <- c("Agree", "Disagree")
#'                        
#' # prepare data for 4-category likert scale, with neutral category 5 items
#' Q1 <- as.factor(sample(1:4, 500, replace = TRUE, prob = c(0.2, 0.3, 0.1, 0.4)))
#' Q2 <- as.factor(sample(1:4, 500, replace = TRUE, prob = c(0.5, 0.25, 0.15, 0.1)))
#' Q3 <- as.factor(sample(1:4, 500, replace = TRUE, prob = c(0.25, 0.1, 0.4, 0.25)))
#' Q4 <- as.factor(sample(1:4, 500, replace = TRUE, prob = c(0.1, 0.4, 0.4, 0.1)))
#' Q5 <- as.factor(sample(1:4, 500, replace = TRUE, prob = c(0.35, 0.25, 0.15, 0.25)))
#' 
#' likert_4 <- data.frame(Q1, Q2, Q3, Q4, Q5)
#' 
#' # create labels
#' levels_4 <- c("Strongly agree", 
#'               "Agree", 
#'               "Disagree", 
#'               "Strongly Disagree", 
#'               "Don't know")
#' 
#' # prepare data for 6-category likert scale, 5 items
#' likert_6 <- data.frame()
#' 
#' Q1 <- as.factor(sample(1:6, 500, replace = TRUE, prob = c(0.2,0.1,0.1,0.3,0.2,0.1)))
#' Q2 <- as.factor(sample(1:6, 500, replace = TRUE, prob = c(0.15,0.15,0.3,0.1,0.1,0.2)))
#' Q3 <- as.factor(sample(1:6, 500, replace = TRUE, prob = c(0.2,0.25,0.05,0.2,0.2,0.2)))
#' Q4 <- as.factor(sample(1:6, 500, replace = TRUE, prob = c(0.2,0.1,0.1,0.4,0.1,0.1)))
#' Q5 <- as.factor(sample(1:6, 500, replace = TRUE, prob = c(0.1,0.4,0.1,0.3,0.05,0.15)))
#' 
#' likert_6 <- data.frame(Q1, Q2, Q3, Q4, Q5)
#'
#' # create labels
#' levels_6 <- c("Very strongly agree", "Strongly agree", "Agree",
#'               "Disagree", "Strongly disagree", "Very strongly disagree")
#' 
#' # create item labels
#' items <- c("Q1", "Q2", "Q3", "Q4", "Q5")
#' 
#' # plot dichotomous likert scale, ordered by "negative" values
#' sjp.likert(likert_2,
#'            geom.colors = c("green", "red"),
#'            legendLabels = levels_2, 
#'            axisLabels.y = items, 
#'            sort.frq = "neg.desc")
#' 
#' # plot 4-category-likert-scale, no order
#' sjp.likert(likert_4, 
#'            cat.neutral = 5,
#'            legendLabels = levels_4, 
#'            axisLabels.y = items,
#'            gridRange = 1.2,
#'            expand.grid = FALSE,
#'            value.labels = "sum.outside",
#'            showPercentageSign = TRUE)
#' 
#' # plot 6-category-likert-scale, ordered by positive values,
#' # in brown color scale
#' sjp.likert(likert_6, 
#'            legendLabels = levels_6, 
#'            axisLabels.y = items, 
#'            sort.frq = "pos.asc", 
#'            labelDigits = 0,
#'            showPercentageSign = TRUE,
#'            value.labels = "sum.inside")
#' 
#' @import ggplot2
#' @import sjmisc
#' @importFrom stats na.omit xtabs
#' @export
sjp.likert <- function(items,
                       catcount = NULL, 
                       cat.neutral = NULL,
                       weightBy = NULL,
                       weightByTitleString = NULL,
                       sort.frq = NULL,
                       geom.size = .6,
                       geom.colors = "BrBG",
                       reverse.colors = FALSE,
                       cat.neutral.color = "grey70",
                       intercept.line.color = "grey50",
                       value.labels = "show",
                       showPercentageSign = FALSE,
                       labelDigits = 1,
                       legendLabels = NULL,
                       hideLegend = FALSE,
                       title = NULL, 
                       legendTitle = NULL,
                       includeN = TRUE,
                       showItemLabels = TRUE,
                       axisLabels.y = NULL,
                       breakTitleAt = 50, 
                       breakLabelsAt = 30, 
                       breakLegendTitleAt = 30, 
                       breakLegendLabelsAt = 28,
                       gridRange = 1,
                       gridBreaksAt = 0.2,
                       expand.grid = TRUE,
                       axisTitle.x = NULL,
                       axisTitle.y = NULL,
                       coord.flip = TRUE,
                       printPlot = TRUE) {
  # --------------------------------------------------------
  # check param. if we have a single vector instead of
  # a data frame with several items, convert vector to data frame
  # --------------------------------------------------------
  if (!is.data.frame(items) && !is.matrix(items)) items <- as.data.frame(items)
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
  if (is.null(legendLabels)) legendLabels <- sjmisc::get_labels(items[[1]],
                                                                attr.only = F,
                                                                include.values = NULL,
                                                                include.non.labelled = T)
  if (is.null(axisLabels.y)) {
    axisLabels.y <- c()
    # if yes, iterate each variable
    for (i in 1:ncol(items)) {
      # retrieve variable name attribute
      axisLabels.y <- c(axisLabels.y, sjmisc::get_label(items[[i]], def.value = colnames(items)[i]))
    }
  }  
  # --------------------------------------------------------
  # If axisLabels.y were not defined, simply use column names
  # --------------------------------------------------------
  if (is.null(axisLabels.y)) axisLabels.y <- colnames(items)
  # --------------------------------------------------------
  # unlist/ unname axis labels
  # --------------------------------------------------------
  if (!is.null(axisLabels.y)) {
    # unname labels, if necessary, so we have a simple character vector
    if (!is.null(names(axisLabels.y))) axisLabels.y <- as.vector(axisLabels.y)
  } 
  # --------------------------------------------------------
  # unlist/ unname axis labels
  # --------------------------------------------------------
  if (!is.null(legendLabels)) {
    # unname labels, if necessary, so we have a simple character vector
    if (!is.null(names(legendLabels))) legendLabels <- as.vector(legendLabels)
  } 
  # --------------------------------------------------------
  # determine catcount
  # --------------------------------------------------------
  adding <- ifelse(is.null(cat.neutral), 0, 1)
  if (is.null(catcount)) {
    catcount <- c()
    # iterate all items
    for (i in 1:ncol(items)) {
      # add new unique item values to catcount, so catcount
      # finally contains all unique values of items
      catcount <- unique(c(catcount, unique(stats::na.omit(items[[i]]))))
    }
    # remove neutral category
    if (!is.null(cat.neutral)) catcount <- catcount[-which(catcount == cat.neutral)]
    # detect range of valid categories, which
    # then equals catcount
    catcount <- max(catcount) - min(catcount) + 1
    # check if category count matches category label count
    if (!is.null(legendLabels)) {
      # how many labels do we have?
      # substract 1, if we have neutral category
      lll <- length(legendLabels) - adding
      # catcount and legend label count equal?
      if (catcount < lll) {
        # warn user that detected amount of categories and supplied legend labels
        # are different.
        warning("Length of labels for item categories 'legendLabels' differs from detected amount of categories. Use 'catcount' argument to define amount of item categories, if plotting does not work.", call. = F)
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
  if (is.null(legendLabels)) {
    legendLabels <- c(1:(catcount + adding))
  }
  # --------------------------------------------------------
  # prepare data frames
  # --------------------------------------------------------
  mydat.pos <- data.frame()
  mydat.neg <- data.frame()
  mydat.dk <- data.frame()
  freq.df <- data.frame()
  # --------------------------------------------------------
  # loop through all likert-items
  # --------------------------------------------------------
  for (i in 1:ncol(items)) {
    # --------------------------------------------------------
    # convert to numeric values
    # --------------------------------------------------------
    if (!is.numeric(items[[i]])) {
      # --------------------------------------------------------
      # convert non-numeric factors to numeric values
      # --------------------------------------------------------
      items[[i]] <- sjmisc::to_value(items[[i]], keep.labels = F)
    }
    # --------------------------------------------------------
    # If we have neutral category in between and not as last
    # category, recode neutral category to last category
    # --------------------------------------------------------
    if (!is.null(cat.neutral) && cat.neutral <= catcount) {
      # first, each other category has to be moved down one position
      # therefore, we create a pattern with values from neutral
      # category to category count
      downvote <- seq(cat.neutral, catcount + 1, by = 1)
      # now we "shift" this value pattern and make a
      # string out of it
      recode.pattern <- paste0(paste0(sprintf("%i=%i",
                                              c(downvote[-1], downvote[1]),
                                              downvote),
                                      collapse = ";"), ";else=copy")
      # finally, recode data
      items[[i]] <- sjmisc::rec(items[[i]], recodes = recode.pattern)
    }
    # --------------------------------------------------------
    # If we don't plot neutral category, but item still contains
    # that category, replace it with NA
    # --------------------------------------------------------
    if (is.null(cat.neutral) && max(items[[i]], na.rm = T) > catcount)
      items[[i]] <- sjmisc::set_na(items[[i]], catcount + 1)
    # --------------------------------------------------------
    # create proportional frequency table
    # --------------------------------------------------------
    if (is.null(weightBy)) {
      tab <- round(prop.table(table(items[[i]])), 3)
    } else {
      tab <- round(prop.table(stats::xtabs(weightBy ~ items[[i]])), 3)
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
        stop("Could not determine amount of item categories. Please use argument 'catcount'.", call. = F)
      else
        freq.df <- as.data.frame(cbind(freq.df, freq))
    }
      
  }
  # --------------------------------------------------------
  # Check whether N of each item should be included into
  # axis labels
  # --------------------------------------------------------
  if (includeN && !is.null(axisLabels.y)) {
    for (i in 1:length(axisLabels.y)) {
      axisLabels.y[i] <- paste(axisLabels.y[i], 
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
    sort.freq <- c(1:ncol(freq.df))
  else if (sort.frq == "pos")
    sort.freq <- order(sums.lower)
  else if (sort.frq == "neg")
    sort.freq <- order(sums.upper)
  else
    sort.freq <- c(1:ncol(freq.df))
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
  for (i in 1:ncol(freq.df)) {
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
                                           frq = c(fr[lower.half]),
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
                                           frq = c(-fr[upper.half]),
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
                                            offset = -1 * gridRange)))
      # cumulative neutral cat
      ypos.sum.dk <- c(ypos.sum.dk, -1 + fr[catcount + adding])
    }
  }
  # --------------------------------------------------------
  # x-positions for cumulative percentages
  # --------------------------------------------------------
  xpos.sum.dk <- xpos.sum.neg <- xpos.sum.pos <- c(1:length(ypos.sum.pos))
  # --------------------------------------------------------
  # grp as factor
  # --------------------------------------------------------
  mydat.pos$grp <- as.factor(mydat.pos$grp)
  mydat.neg$grp <- as.factor(mydat.neg$grp)
  # same for neutral
  if (!is.null(cat.neutral)) {
    mydat.dk$grp <- as.factor("neutral")
    mydat.dk$geom.size <- geom.size
    mydat.dk$labelDigits <- labelDigits
  }
  # --------------------------------------------------------
  # label digits needed
  # --------------------------------------------------------
  mydat.neg$labelDigits <- labelDigits
  mydat.pos$labelDigits <- labelDigits
  # --------------------------------------------------------
  # Prepare and trim legend labels to appropriate size
  # --------------------------------------------------------
  # wrap legend text lines
  legendLabels <- sjmisc::word_wrap(legendLabels, breakLegendLabelsAt)
  # check whether we have a title for the legend
  if (!is.null(legendTitle)) {
    # if yes, wrap legend title line
    legendTitle <- sjmisc::word_wrap(legendTitle, breakLegendTitleAt)
  }
  # check length of diagram title and split longer string at into new lines
  # every 50 chars
  if (!is.null(title)) {
    # if we have weighted values, say that in diagram's title
    if (!is.null(weightByTitleString)) {
      title <- paste(title, weightByTitleString, sep = "")
    }
    title <- sjmisc::word_wrap(title, breakTitleAt)
  }
  # check length of x-axis-labels and split longer strings at into new lines
  # every 10 chars, so labels don't overlap
  if (!is.null(axisLabels.y)) {
    axisLabels.y <- sjmisc::word_wrap(axisLabels.y, breakLabelsAt)
  }
  # --------------------------------------------------------
  # set diagram margins
  # --------------------------------------------------------
  if (expand.grid) {
    expgrid <- ggplot2::waiver()
  } else {
    expgrid <- c(0, 0)
  }
  # --------------------------------------------------------
  # Hide or show Category Labels (x axis text) 
  # --------------------------------------------------------
  if (!showItemLabels) {
    axisLabels.y <- c("")
  } else {
    axisLabels.y <- axisLabels.y[sort.freq]
  }
  # --------------------------------------------------------
  # Set up grid breaks
  # --------------------------------------------------------
  gridbreaks <- round(c(seq(-gridRange, gridRange, by = gridBreaksAt)), 2)
  gridlabs <- ifelse(abs(gridbreaks) > 1, "", paste0(c(abs(round(100 * gridbreaks))), "%"))
  # --------------------------------------------------------
  # start plot here
  # --------------------------------------------------------
  gp <- ggplot() +
    # positive value bars
    geom_bar(data = mydat.pos, 
             aes(x = x, y = frq, fill = grp), 
             width = geom.size, 
             stat = "identity") +
    # negative value bars
    geom_bar(data = mydat.neg, 
             aes(x = x, y = frq, fill = grp), 
             width = geom.size, 
             stat = "identity")
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
  percsign <- mydat.pos$percsign <- mydat.neg$percsign <- ifelse(isTRUE(showPercentageSign), "%", "")
  if (nrow(mydat.dk) > 0) mydat.dk$percsign <- percsign
  # --------------------------------------------------------
  # creating value labels for cumulative percentages, so
  # zero-percentages are not printed
  # --------------------------------------------------------
  ypos.sum.pos.lab  <- ifelse(ypos.sum.pos > 0, sprintf("%.*f%s", labelDigits, 100 * ypos.sum.pos, percsign), "")
  ypos.sum.neg.lab  <- ifelse(ypos.sum.neg < 0, sprintf("%.*f%s", labelDigits, 100 * abs(ypos.sum.neg), percsign), "")
  ypos.sum.dk.lab  <- ifelse(ypos.sum.dk > -1, sprintf("%.*f%s", labelDigits, 100 * (1 + ypos.sum.dk), percsign), "")
  
  if (value.labels == "show") {
    # show them in middle of bar
    gp <- gp +
      geom_text(data = subset(mydat.pos, frq > 0), 
                aes(x = x, y = ypos, label = sprintf("%.*f%s", labelDigits, 100 * frq, percsign))) +
      geom_text(data = subset(mydat.neg, frq < 0), 
                aes(x = x, y = ypos, label = sprintf("%.*f%s", labelDigits, 100 * abs(frq), percsign)))
    if (!is.null(cat.neutral)) {
      gp <- gp +
        geom_text(data = subset(mydat.dk, frq > -1), aes(x = x, y = ypos + offset + 1, label = sprintf("%.*f%s", labelDigits, 100 * (1 + frq), percsign)))
    }
  } else if (value.labels == "sum.inside" || value.labels == "sum.outside") {
    # show cumulative outside bar
    if (value.labels == "sum.outside") {
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
        annotate("text", x = xpos.sum.dk, y = ypos.sum.dk + 1 - gridRange, hjust = hort.dk, label = ypos.sum.dk.lab)
    }
  }
  # ---------------------------------------------------------
  # continues with plot
  # ---------------------------------------------------------
  gp <- gp +
    labs(title = title, x = axisTitle.x, y = axisTitle.y, fill = legendTitle) +
    # ---------------------------------------------------------
    # scale x is continuous to make plotting the bar annotation
    # for neutral category work...
    # ---------------------------------------------------------
    scale_x_continuous(breaks = c(1:ncol(freq.df)), labels = axisLabels.y) +
    scale_y_continuous(breaks = gridbreaks, limits = c(-gridRange, gridRange), expand = expgrid, labels = gridlabs) +
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
                         ifelse(isTRUE(hideLegend), FALSE, TRUE), 
                         legendLabels,
                         reverse.colors)
  # ---------------------------------------------------------
  # Check whether ggplot object should be returned or plotted
  # ---------------------------------------------------------
  if (printPlot) graphics::plot(gp)
  # -------------------------------------
  # return results
  # -------------------------------------
  invisible(structure(class = "sjplikert",
                      list(plot = gp,
                           df.neg = mydat.neg,
                           df.pos = mydat.pos,
                           df.neutral = mydat.dk)))
}
