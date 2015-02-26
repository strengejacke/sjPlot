# bind global variables
# if(getRversion() >= "2.15.1") utils::globalVariables(c("Freq", "ypos", "Question", "Response"))

#' @title Plot likert scales as centered stacked bars
#' @name sjp.likert
#'             
#' @seealso \itemize{
#'              \item \code{\link{sjp.stackfrq}}
#'              \item \code{\link{sjt.stackfrq}}
#'              \item \href{http://www.strengejacke.de/sjPlot/sjp.likert/}{sjPlot manual: sjp.likert}
#'              }
#' 
#' @description Plot likert scales as centered stacked bars.
#' 
#' @note Note that only even numbers of categories are possible to plot, so the "positive" 
#'        and "negative" values can be splitted into two halfs. A neutral category (like "don't know")
#'        can be used, but must be indicated by \code{cat.neutral}.
#' 
#' @param items A data frame with each column representing one likert-item.
#' @param catcount the amount of categories of the items (e.g. "strongly disagree", 
#'          "disagree", "agree" and "strongly agree" would be \code{catcount=4}).
#'          Note that this parameter only applies to "valid" answers, i.e. if you
#'          have an additional neutral category (see \code{cat.neutral}) like "don't know",
#'          this won't count for \code{catcount} (e.g. "strongly disagree", 
#'          "disagree", "agree", "strongly agree" and neutral category "son't know"
#'          would still mean that \code{catcount=4}). \cr
#'          Normally, this parameter can be ignored because the amount of valid categories
#'          is retrieved by the function itself.
#' @param cat.neutral If there's a neutral category (like "don't know" etc.), specify
#'          the index number for this category. Else, set \code{cat.neutral=NULL} (default).
#'          The frequencies of neutral categories are plotted as grey bars on the left side of
#'          the figure.
#' @param weightBy A weight factor that will be applied to weight all cases from \code{items}.
#'          Must be a vector of same length as \code{nrow(items)}. Default is \code{NULL}, so no weights are used.
#' @param weightByTitleString If a weight factor is supplied via the parameter \code{weightBy}, the diagram's title
#'          may indicate this with a remark. Default is \code{NULL}, so the diagram's title will not be modified when
#'          cases are weighted. Use a string as parameter, e.g.: \code{weightByTitleString=" (weighted)"}
#' @param sort.frq Indicates whether the items of \code{items} should be ordered by total sum of positive or negative answers.
#'          \itemize{
#'            \item Use \code{"pos.asc"} to order ascending by sum of positive answers,
#'            \item \code{"pos.desc"} to order descending by sum of positive answers,
#'            \item \code{"neg.asc"} for sorting ascending negative answers,
#'            \item \code{"neg.desc"} for sorting descending negative answers
#'            \item or \code{NULL} (default) for no sorting.
#'          }
#' @param geom.colors User defined color palette for geoms. If specified, must either be vector with color values 
#'          of same length as groups defined in \code{legendLabels}, or a specific color palette code (see below).
#'          \itemize{
#'            \item If not specified, the diverging \code{"GnBu"} color brewer palette will be used.
#'            \item If \code{"gs"}, a greyscale will be used.
#'            \item If \code{geom.colors} is any valid color brewer palette name, the related \href{http://colorbrewer2.org}{color brewer} palette will be used. Use \code{display.brewer.all()} from the \code{RColorBrewer} package to view all available palette names.
#'          }
#'          Else specify your own color values as vector (e.g. \code{geom.colors=c("#f00000", "#00ff00", "#0080ff")}).
#' @param reverse.colors If \code{TRUE}, the color scale from \code{geom.colors} will be reversed,
#'          so positive and negative value switch colors.
#' @param geom.size Width of bars. Recommended values for this parameter are from 0.4 to 1.5
#' @param cat.neutral.color Color of the neutral category, if plotted (see \code{cat.neutral}).
#' @param intercept.line.color Color of the vertical intercept line that divides positive and negative values.
#' @param legendLabels A list or vector of strings that indicate the likert-scale-categories and which
#'          appear as legend text.
#' @param hideLegend Indicates whether legend (guide) should be shown or not.
#' @param title Title of the diagram, plotted above the whole diagram panel.
#' @param legendTitle Title of the diagram's legend.
#' @param includeN If \code{TRUE} (default), the N of each item is included into axis labels.
#' @param value.labels determines style and position of percentage value labels on the bars:
#'          \itemize{
#'            \item \code{"hide"} hides the value labels, so no percentage values on the bars are printed.
#'            \item \code{"show"} (default) shows percentage value labels in the middle of each category bar.
#'            \item \code{"sum.inside"} shows the sums of percentage values for both negative and positive values and prints them inside the end of each bar.
#'            \item \code{"sum.outide"} shows the sums of percentage values for both negative and positive values and prints them outside the end of each bar.
#'          }
#' @param showPercentageSign If \code{TRUE}, percentage signs on value labels are shown.
#' @param showItemLabels Whether x axis text (category names) should be shown or not
#' @param axisLabels.y a character vector with labels for the y-axis (the labels of the 
#'          \code{items}). Example: \code{axisLabels.y=c("Q1", "Q2", "Q3")}
#'          Axis labels will automatically be detected, when they have
#'          a \code{"variable.lable"} attribute (see \code{\link{set_var_labels}}) for details).
#' @param breakTitleAt Wordwrap for diagram title. Determines how many chars of the title are displayed in
#'          one line and when a line break is inserted into the title.
#' @param breakLabelsAt Wordwrap for diagram labels. Determines how many chars of the category labels are displayed in 
#'          one line and when a line break is inserted.
#' @param breakLegendTitleAt Wordwrap for diagram legend title. Determines how many chars of the legend's title 
#'          are displayed in one line and when a line break is inserted.
#' @param breakLegendLabelsAt Wordwrap for diagram legend labels. Determines how many chars of the legend labels are 
#'          displayed in one line and when a line break is inserted.
#' @param gridRange Sets the limit of the x-axis-range. Default is 1, so the x-scale ranges
#'          from zero to 100 percent on both sides from the center. You can use values beyond 1
#'          (100 percent) in case bar labels are not printed because they exceed the axis range.
#'          E.g. \code{gridRange=1.4} will set the axis from -140 to +140 percent, however, only
#'          (valid) axis labels from -100 to +100 percent are printed. Neutral categories are
#'          adjusted to the most left limit.
#' @param gridBreaksAt Sets the breaks on the y axis, i.e. at every n'th position a major
#'          grid is being printed. Valid values range from 0 to 1.
#' @param expand.grid If \code{TRUE} (default), the diagram has margins, i.e. the y-axis is not exceeded
#'          to the diagram's boundaries.
#' @param axisTitle.x A label for the x axis. Useful when plotting histograms with metric scales where no category labels
#'          are assigned to the x axis.
#' @param axisTitle.y A label for the y axis. Useful when plotting histograms with metric scales where no category labels
#'          are assigned to the y axis.
#' @param coord.flip If \code{TRUE}, the x and y axis are swapped.
#' @param printPlot If \code{TRUE} (default), plots the results as graph. Use \code{FALSE} if you don't
#'          want to plot any graphs. In either case, the ggplot-object will be returned as value.
#' @return (Insisibily) returns the ggplot-object with the complete plot (\code{plot}) as well as the data frame that
#'           was used for setting up the ggplot-object (\code{df.neg} for the negative values,
#'           \code{df.pos} for the positive values and \code{df.neutral} for the neutral category values).
#' 
#' @examples
#' # prepare data for dichotomous likert scale, 5 items
#' likert_2 <- data.frame(as.factor(sample(1:2, 500, replace=TRUE, prob=c(0.3,0.7))),
#'                        as.factor(sample(1:2, 500, replace=TRUE, prob=c(0.6,0.4))),
#'                        as.factor(sample(1:2, 500, replace=TRUE, prob=c(0.25,0.75))),
#'                        as.factor(sample(1:2, 500, replace=TRUE, prob=c(0.9,0.1))),
#'                        as.factor(sample(1:2, 500, replace=TRUE, prob=c(0.35,0.65))))
#' # create labels
#' levels_2 <- c("Agree", "Disagree")
#'                        
#' # prepare data for 4-category likert scale, with neutral category 5 items
#' likert_4 <- data.frame(as.factor(sample(1:5, 500, replace=TRUE, 
#'                                         prob=c(0.2,0.3,0.1,0.35,0.05))),
#'                        as.factor(sample(1:5, 500, replace=TRUE, 
#'                                         prob=c(0.5,0.25,0.15,0.1,0.0))),
#'                        as.factor(sample(1:5, 500, replace=TRUE, 
#'                                         prob=c(0.25,0.1,0.38,0.24,0.03))),
#'                        as.factor(sample(1:5, 500, replace=TRUE, 
#'                                         prob=c(0.1,0.32,0.37,0.1,0.11))),
#'                        as.factor(sample(1:5, 500, replace=TRUE, 
#'                                         prob=c(0.35,0.22,0.15,0.25, 0.03))))
#' # create labels
#' levels_4 <- c("Strongly agree", 
#'               "Agree", 
#'               "Disagree", 
#'               "Strongly Disagree", 
#'               "Don't know")
#' 
#' # prepare data for 6-category likert scale, 5 items
#' likert_6 <- data.frame(
#'                as.factor(sample(1:6, 500, replace=TRUE, prob=c(0.2,0.1,0.1,0.3,0.2,0.1))),
#'                as.factor(sample(1:6, 500, replace=TRUE, prob=c(0.15,0.15,0.3,0.1,0.1,0.2))),
#'                as.factor(sample(1:6, 500, replace=TRUE, prob=c(0.2,0.25,0.05,0.2,0.2,0.2))),
#'                as.factor(sample(1:6, 500, replace=TRUE, prob=c(0.2,0.1,0.1,0.4,0.1,0.1))),
#'                as.factor(sample(1:6, 500, replace=TRUE, prob=c(0.1,0.4,0.1,0.3,0.05,0.15))))
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
#'            value.labels = "sum.inside")
#' 
#' @import ggplot2
#' @importFrom car recode
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
                       coord.flip=TRUE,
                       printPlot=TRUE) {
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
  }
  else {
    reverseOrder <- FALSE
  }
  # --------------------------------------------------------
  # try to automatically set labels is not passed as parameter
  # --------------------------------------------------------
  if (is.null(legendLabels)) legendLabels <- autoSetValueLabels(items[, 1])
  if (is.null(axisLabels.y)) {
    axisLabels.y <- c()
    # if yes, iterate each variable
    for (i in 1:ncol(items)) {
      # retrieve variable name attribute
      vn <- autoSetVariableLabels(items[, i])
      # if variable has attribute, add to variableLabel list
      if (!is.null(vn)) {
        axisLabels.y <- c(axisLabels.y, vn)
      }
      else {
        # else break out of loop
        axisLabels.y <- NULL
        break
      }
    }
  }  
  # --------------------------------------------------------
  # If axisLabels.y were not defined, simply use column names
  # --------------------------------------------------------
  if (is.null(axisLabels.y)) {
    axisLabels.y <- colnames(items)
  }
  # --------------------------------------------------------
  # unlist labels
  # --------------------------------------------------------
  if (!is.null(axisLabels.y) && is.list(axisLabels.y)) {
    axisLabels.y <- unlistlabels(axisLabels.y)
  }
  if (!is.null(legendLabels) && is.list(legendLabels)) {
    legendLabels <- unlistlabels(legendLabels)
  }
  # --------------------------------------------------------
  # determine catcount
  # --------------------------------------------------------
  adding <- ifelse (is.null(cat.neutral), 0, 1)
  if (is.null(catcount)) {
    catcount <- c()
    # iterate all items
    for (i in 1 : ncol(items)) {
      # add new unique item values to catcount, so catcount
      # finally contains all unique values of items
      catcount <- unique(c(catcount, unique(na.omit(items[, i]))))
    }
    # length of catcount
    catcount <- length(catcount)
    # if catcount odd or even?
    if ((catcount %% 2) == 1)
      # make catcount even
      catcount <- catcount - 1
  }
  # --------------------------------------------------------
  # set legend labels, if we have none yet
  # --------------------------------------------------------
  if (is.null(legendLabels)) {
    legendLabels <- c(1 : (catcount + adding))
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
  for (i in 1 : ncol(items)) {
    # --------------------------------------------------------
    # convert to numeric values
    # --------------------------------------------------------
    if (!is.numeric(items[ ,i])) {
      # --------------------------------------------------------
      # convert non-numeric factors to numeric values
      # --------------------------------------------------------
      isnum <- na.omit(as.numeric(levels(items[, i])))
      if (length(isnum) == 0) {
        items[ ,i] <- to_value(items[ ,i])
      }
      items[ ,i] <- as.numeric(items[ ,i])
    }
    # --------------------------------------------------------
    # If we have neutral category in between and not as last
    # category, recode neutral category to last category
    # --------------------------------------------------------
    if (!is.null(cat.neutral) && cat.neutral <= catcount) {
      items[, i] <- car::recode(items[, i], sprintf("%i=%i;%i=%i", 
                                                    cat.neutral, 
                                                    catcount + 1, 
                                                    catcount + 1,
                                                    cat.neutral))
    }
    # --------------------------------------------------------
    # If we don't plot neutral category, but item still contains
    # that category, replace it with NA
    # --------------------------------------------------------
    if (is.null(cat.neutral) && max(items[, i], na.rm = T) > catcount)
      items[, i] <- set_na(items[, i], catcount + 1)
    # --------------------------------------------------------
    # create proportional frequency table
    # --------------------------------------------------------
    if (is.null(weightBy)) {
      tab <- round(prop.table(table(items[, i])), 3)
    }
    else {
      tab <- round(prop.table(xtabs(weightBy ~ items[, i])), 3)
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
    else
      freq.df <- as.data.frame(cbind(freq.df, freq))
  }
  # --------------------------------------------------------
  # Check whether N of each item should be included into
  # axis labels
  # --------------------------------------------------------
  if (includeN && !is.null(axisLabels.y)) {
    for (i in 1 : length(axisLabels.y)) {
      axisLabels.y[i] <- paste(axisLabels.y[i], 
                               sprintf(" (n=%i)", length(na.omit(items[, i]))), 
                               sep="")
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
  sums.lower <- unname(apply(freq.df[lower.half, ], 2, sum))
  sums.upper <- unname(apply(freq.df[upper.half, ], 2, sum))
  # --------------------------------------------------------
  # sort items
  # --------------------------------------------------------
  if (is.null(sort.frq))
    sort.freq <- c(1 : ncol(freq.df))
  else if (sort.frq == "pos")
    sort.freq <- order(sums.lower)
  else if (sort.frq == "neg")
    sort.freq <- order(sums.upper)
  else
    sort.freq <- c(1 : ncol(freq.df))
  # --------------------------------------------------------
  # reverse item order?
  # --------------------------------------------------------
  if (!reverseOrder)
    sort.freq <- rev(sort.freq)
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
  for (i in 1 : ncol(freq.df)) {
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
  xpos.sum.dk <- xpos.sum.neg <- xpos.sum.pos <- c(1 : length(ypos.sum.pos))
  # --------------------------------------------------------
  # grp as factor
  # --------------------------------------------------------
  mydat.pos$grp <- as.factor(mydat.pos$grp)
  mydat.neg$grp <- as.factor(mydat.neg$grp)
  # same for neutral
  if (!is.null(cat.neutral)) {
    mydat.dk$grp <- as.factor("neutral")
    mydat.dk$geom.size <- geom.size
  }
  # --------------------------------------------------------
  # Prepare and trim legend labels to appropriate size
  # --------------------------------------------------------
  # wrap legend text lines
  legendLabels <- word_wrap(legendLabels, breakLegendLabelsAt)
  # check whether we have a title for the legend
  if (!is.null(legendTitle)) {
    # if yes, wrap legend title line
    legendTitle <- word_wrap(legendTitle, breakLegendTitleAt)
  }
  # check length of diagram title and split longer string at into new lines
  # every 50 chars
  if (!is.null(title)) {
    # if we have weighted values, say that in diagram's title
    if (!is.null(weightByTitleString)) {
      title <- paste(title, weightByTitleString, sep="")
    }
    title <- word_wrap(title, breakTitleAt)
  }
  # check length of x-axis-labels and split longer strings at into new lines
  # every 10 chars, so labels don't overlap
  if (!is.null(axisLabels.y)) {
    axisLabels.y <- word_wrap(axisLabels.y, breakLabelsAt)
  }
  # --------------------------------------------------------
  # set diagram margins
  # --------------------------------------------------------
  if (expand.grid) {
    expgrid <- waiver()
  }
  else {
    expgrid <- c(0,0)
  }
  # --------------------------------------------------------
  # Hide or show Category Labels (x axis text) 
  # --------------------------------------------------------
  if (!showItemLabels) {
    axisLabels.y <- c("")
  }
  else {
    axisLabels.y <- axisLabels.y[sort.freq]
  }
  # --------------------------------------------------------
  # Set up grid breaks
  # --------------------------------------------------------
  gridbreaks <- round(c(seq(-gridRange, gridRange, by = gridBreaksAt)), 2)
  gridlabs <- ifelse (abs(gridbreaks) > 1, "", paste0(c(abs(round(100 * gridbreaks))), "%"))
  # --------------------------------------------------------
  # start plot here
  # --------------------------------------------------------
  gp <- ggplot() +
    # positive value bars
    geom_bar(data = mydat.pos, aes(x=x, y=frq, fill=grp), width = geom.size, stat="identity") +
    # negative value bars
    geom_bar(data = mydat.neg, aes(x=x, y=frq, fill=grp), width = geom.size, stat="identity")
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
  if (!is.null(cat.neutral))
    geom.colors <- c(geom.colors, cat.neutral.color)
  # --------------------------------------------------------
  # should percentage value labels be printed?
  # --------------------------------------------------------
  percsign <- mydat.pos$percsign <- mydat.neg$percsign <- ifelse (showPercentageSign == TRUE, "%", "")
  if (nrow(mydat.dk) > 0) mydat.dk$percsign <- percsign
  # --------------------------------------------------------
  # creating value labels for cumulative percentages, so
  # zero-percentages are not printed
  # --------------------------------------------------------
  ypos.sum.pos.lab  <- ifelse (ypos.sum.pos > 0, sprintf("%.01f%s", 100 * ypos.sum.pos, percsign), "")
  ypos.sum.neg.lab  <- ifelse (ypos.sum.neg < 0, sprintf("%.01f%s", 100 * abs(ypos.sum.neg), percsign), "")
  ypos.sum.dk.lab  <- ifelse (ypos.sum.dk > -1, sprintf("%.01f%s", 100 * (1 + ypos.sum.dk), percsign), "")
  
  if (value.labels == "show") {
    # show them in middle of bar
    gp <- gp +
      geom_text(data = subset(mydat.pos, frq > 0), aes(x = x, y = ypos, label = sprintf("%.01f%s", 100 * frq, percsign))) +
      geom_text(data = subset(mydat.neg, frq < 0), aes(x = x, y = ypos, label = sprintf("%.01f%s", 100 * abs(frq), percsign)))
    if (!is.null(cat.neutral)) {
      gp <- gp +
        geom_text(data = subset(mydat.dk, frq > -1), aes(x = x, y = ypos + offset + 1, label = sprintf("%.01f%s", 100 * (1 + frq), percsign)))
    }
  }
  else if (value.labels == "sum.inside" || value.labels == "sum.outside") {
    # show cumulative outside bar
    if (value.labels == "sum.outside") {
      hort.pos <- -0.15
      hort.neg <- 1.15
      hort.dk <- -0.15
    }
    # show cumulative inside bar
    else {
      hort.pos <- 1.15
      hort.neg <- -0.15
      hort.dk <- 1.15
    }
    gp <- gp +
      annotate("text", x = xpos.sum.pos, y = ypos.sum.pos, hjust = hort.pos, label = ypos.sum.pos.lab) +
      annotate("text", x = xpos.sum.neg, y = ypos.sum.neg, hjust = hort.neg, label = ypos.sum.neg.lab)
    if (!is.null(cat.neutral)) {
      gp <- gp +
        annotate("text", x = xpos.sum.dk, y = ypos.sum.dk + 1 -gridRange, hjust = hort.dk, label = ypos.sum.dk.lab)
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
    scale_x_continuous(breaks = c(1 : ncol(freq.df)), labels = axisLabels.y) +
    scale_y_continuous(breaks = gridbreaks, limits = c(-gridRange, gridRange), expand = expgrid, labels = gridlabs) +
    geom_hline(yintercept = 0, color = intercept.line.color)
  # ---------------------------------------------------------
  # check whether coordinates should be flipped, i.e.
  # swap x and y axis
  # ---------------------------------------------------------
  if (coord.flip) {
    gp <- gp + coord_flip()
  }
  # ---------------------------------------------------------
  # set geom colors
  # ---------------------------------------------------------
  gp <- sj.setGeomColors(gp, 
                         geom.colors, 
                         (catcount + adding), 
                         ifelse(hideLegend==TRUE, FALSE, TRUE), 
                         legendLabels,
                         reverse.colors)
  # ---------------------------------------------------------
  # Check whether ggplot object should be returned or plotted
  # ---------------------------------------------------------
  if (printPlot) plot(gp)
  # -------------------------------------
  # return results
  # -------------------------------------
  invisible (structure(class = "sjplikert",
                       list(plot = gp,
                            df.neg = mydat.neg,
                            df.pos = mydat.pos,
                            df.neutral = mydat.dk)))
}
