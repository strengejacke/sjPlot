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
#' @param df data frame with (dichotomous) factor variables.
#' @return (Insisibily) returns the ggplot-object with the complete plot (\code{plot}) as well as the data frame that
#'           was used for setting up the ggplot-object (\code{mydf}).
#' 
#' @inheritParams sjp.grpfrq
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
#' sjp.chi2(mydf, axis.labels = items)
#' 
#' @import ggplot2
#' @importFrom dplyr bind_rows
#' @import sjmisc
#' @export
sjp.chi2 <- function(df,
                     title = "Pearson's Chi2-Test of Independence",
                     axis.labels = NULL,
                     wrap.title = 50,
                     wrap.labels = 20,
                     show.legend = FALSE,
                     legend.title = NULL,
                     prnt.plot = TRUE) {
  # --------------------------------------------------------
  # try to automatically set labels is not passed as parameter
  # --------------------------------------------------------
  if (is.null(axis.labels)) {
    axis.labels <- c()
    # if yes, iterate each variable
    for (i in 1:ncol(df)) {
      # retrieve variable name attribute
      vn <- sjmisc::get_label(df[[i]], def.value = colnames(df)[i])
      # if variable has attribute, add to variableLabel list
      if (!is.null(vn)) {
        axis.labels <- c(axis.labels, vn)
      } else {
        # else break out of loop
        axis.labels <- NULL
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
  if (is.null(axis.labels)) axis.labels <- row.names(m)
  # --------------------------------------------------------
  # unlist labels
  # --------------------------------------------------------
  if (!is.null(axis.labels) && is.list(axis.labels)) {
    axis.labels <- unlistlabels(axis.labels)
  }
  # ----------------------------
  # Prepare length of title and labels
  # ----------------------------
  # check length of diagram title and split longer string at into new lines
  if (!is.null(title)) title <- sjmisc::word_wrap(title, wrap.title)
  # check length of x-axis-labels and split longer strings at into new lines
  if (!is.null(axis.labels)) axis.labels <- sjmisc::word_wrap(axis.labels, wrap.labels)
  # --------------------------------------------------------
  # start with base plot object here
  # --------------------------------------------------------
  chiPlot <- ggplot(data = m, aes(x = Row, y = Column, fill = p.value, label = p.value)) +
    geom_tile() +
    scale_x_discrete(labels = axis.labels) +
    scale_y_discrete(labels = axis.labels) +
    scale_fill_gradient2(low = rgb(128, 205, 193, maxColorValue = 255), 
                         mid = "white", 
                         high = rgb(5, 113, 176, maxColorValue = 255), 
                         midpoint = 0.05) +
    geom_text(label = sprintf("%.3f", m$p.value)) +
    labs(title = title, 
         x = NULL, 
         y = NULL, 
         fill = legend.title)
  # ---------------------------------------------------------
  # hide legend?
  # ---------------------------------------------------------
  if (!show.legend) chiPlot <- chiPlot + guides(fill = FALSE)
  # ---------------------------------------------------------
  # Check whether ggplot object should be returned or plotted
  # ---------------------------------------------------------
  if (prnt.plot) graphics::plot(chiPlot)
  # -------------------------------------
  # return results
  # -------------------------------------
  invisible(structure(class = "sjpchi2",
                      list(plot = chiPlot,
                           mydf = m)))
}
