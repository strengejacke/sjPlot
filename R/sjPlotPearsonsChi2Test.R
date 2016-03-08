# bind global variables
utils::globalVariables(c("Row", "Column", "p.value"))


#' @title Plot Pearson's Chi2-Test of multiple contingency tables
#' @name sjp.chi2
#' 
#' @seealso \href{http://talesofr.wordpress.com/2013/05/05/ridiculously-photogenic-factors-heatmap-with-p-values/}{Tales of R}.
#' 
#' @description Plot p-values of Pearson's Chi2-tests for multiple contingency tables as ellipses or tiles. 
#'                Requires a data frame with dichotomous (dummy) variables.
#'                Calculation of Chi2-matrix taken from 
#'                \href{https://talesofr.wordpress.com/2013/05/05/ridiculously-photogenic-factors-heatmap-with-p-values/}{Tales of R}.
#' 
#' @param df a data frame of (dichotomous) factor variables.
#' @param title Title of the diagram, plotted above the whole diagram panel
#' @param axisLabels Labels for the x- andy y-axis
#'          axisLabels are detected automatically if each variable has
#'          a label attribute (see \code{\link[sjmisc]{set_label}}) for details).
#' @param breakTitleAt Wordwrap for diagram title. Determines how many chars of the title are displayed in
#'          one line and when a line break is inserted into the title
#' @param breakLabelsAt Wordwrap for diagram labels. Determines how many chars of the category labels are displayed in 
#'          one line and when a line break is inserted
#' @param hideLegend show or hide the legend. The legend indicates the strength of correlations
#'          by gradient colour fill.
#' @param legendTitle the legend title, provided as string, e.g. \code{legendTitle=c("Strength of correlation")}.
#'          Default is \code{NULL}, hence no legend title is used.
#' @param printPlot If \code{TRUE} (default), plots the results as graph. Use \code{FALSE} if you don't
#'          want to plot any graphs. In either case, the ggplot-object will be returned as value.
#' @return (Insisibily) returns the ggplot-object with the complete plot (\code{plot}) as well as the data frame that
#'           was used for setting up the ggplot-object (\code{mydf}).
#' 
#' @examples
#' # create data frame with 5 dichotomous (dummy) variables
#' mydf <- data.frame(as.factor(sample(1:2, 100, replace=TRUE)),
#'                    as.factor(sample(1:2, 100, replace=TRUE)),
#'                    as.factor(sample(1:2, 100, replace=TRUE)),
#'                    as.factor(sample(1:2, 100, replace=TRUE)),
#'                    as.factor(sample(1:2, 100, replace=TRUE)))
#' # create variable labels
#' items <- list(c("Item 1", "Item 2", "Item 3", "Item 4", "Item 5"))
#' 
#' # plot Chi2-contingency-table
#' sjp.chi2(mydf, axisLabels = items)
#' 
#' @import ggplot2
#' @importFrom dplyr bind_rows
#' @import sjmisc
#' @export
sjp.chi2 <- function(df,
                     title = "Pearson's Chi2-Test of Independence",
                     axisLabels = NULL,
                     breakTitleAt = 50,
                     breakLabelsAt = 20,
                     hideLegend = TRUE,
                     legendTitle = NULL,
                     printPlot = TRUE) {
  # --------------------------------------------------------
  # try to automatically set labels is not passed as parameter
  # --------------------------------------------------------
  if (is.null(axisLabels)) {
    axisLabels <- c()
    # if yes, iterate each variable
    for (i in 1:ncol(df)) {
      # retrieve variable name attribute
      vn <- sjmisc::get_label(df[[i]], def.value = colnames(df)[i])
      # if variable has attribute, add to variableLabel list
      if (!is.null(vn)) {
        axisLabels <- c(axisLabels, vn)
      } else {
        # else break out of loop
        axisLabels <- NULL
        break
      }
    }
  }
  # ----------------------------------------------------------------
  # Calculation of Chi2-matrix taken from following blog-posting:
  # http://talesofr.wordpress.com/2013/05/05/ridiculously-photogenic-factors-heatmap-with-p-values/
  # ----------------------------------------------------------------
  combos <- expand.grid(rep(list(1:ncol(df)), 2)) # combinations with repetitions
  combos <- as.matrix(combos)
  combos <- t(combos) # transpose matrix
  # ----------------------------------------------------------------
  # when 2 variables are *not* significant, they are independent
  # ----------------------------------------------------------------
  m <- data.frame()
  for (i in 1:ncol(combos)) {
    test <- chisq.test(df[, combos[1, i]], df[, combos[2, i]])
    out <- data.frame(Row = colnames(df)[combos[1, i]], 
                      Column = colnames(df)[combos[2, i]],
                      Chi.Square = round(test$statistic, 4), 
                      df =  test$parameter, 
                      p.value = round(test$p.value, 4))
    m <- suppressWarnings(dplyr::bind_rows(m, out))
  }
  # ----------------------------
  # check if user defined labels have been supplied
  # if not, use variable names from data frame
  # ----------------------------
  if (is.null(axisLabels)) axisLabels <- row.names(m)
  # --------------------------------------------------------
  # unlist labels
  # --------------------------------------------------------
  if (!is.null(axisLabels) && is.list(axisLabels)) {
    axisLabels <- unlistlabels(axisLabels)
  }
  # ----------------------------
  # Prepare length of title and labels
  # ----------------------------
  # check length of diagram title and split longer string at into new lines
  if (!is.null(title)) title <- sjmisc::word_wrap(title, breakTitleAt)
  # check length of x-axis-labels and split longer strings at into new lines
  if (!is.null(axisLabels)) axisLabels <- sjmisc::word_wrap(axisLabels, breakLabelsAt)
  # --------------------------------------------------------
  # start with base plot object here
  # --------------------------------------------------------
  chiPlot <- ggplot(data = m, aes(x = Row, y = Column, fill = p.value, label = p.value)) +
    geom_tile() +
    scale_x_discrete(labels = axisLabels) +
    scale_y_discrete(labels = axisLabels) +
    scale_fill_gradient2(low = rgb(128, 205, 193, maxColorValue = 255), 
                         mid = "white", 
                         high = rgb(5, 113, 176, maxColorValue = 255), 
                         midpoint = 0.05) +
    geom_text(label = sprintf("%.3f", m$p.value)) +
    labs(title = title, 
         x = NULL, 
         y = NULL, 
         fill = legendTitle)
  # ---------------------------------------------------------
  # hide legend?
  # ---------------------------------------------------------
  if (hideLegend) chiPlot <- chiPlot + guides(fill = FALSE)
  # ---------------------------------------------------------
  # Check whether ggplot object should be returned or plotted
  # ---------------------------------------------------------
  if (printPlot) graphics::plot(chiPlot)
  # -------------------------------------
  # return results
  # -------------------------------------
  invisible(structure(class = "sjpchi2",
                      list(plot = chiPlot,
                           mydf = m)))
}
