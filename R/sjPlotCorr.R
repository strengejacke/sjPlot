#' @title Plot correlation matrix
#' @name sjp.corr
#'
#' @description Plot correlation matrix as ellipses or tiles.
#'
#' @param data Matrix with correlation coefficients as returned by the
#'          \code{\link{cor}}-function, or a \code{data.frame} of variables where
#'          correlations between columns should be computed.
#' @param sort.corr Logical, if \code{TRUE} (default), the axis labels are sorted
#'          according to the correlation strength. If \code{FALSE}, axis labels
#'          appear in order of how variables were included in the cor-computation or
#'          data frame.
#' @param decimals Indicates how many decimal values after comma are printed when
#'          the values labels are shown. Default is 3. Only applies when
#'          \code{show.values = TRUE}.
#' @param na.deletion Indicates how missing values are treated. May be either
#'          \code{"listwise"} (default) or \code{"pairwise"}. May be
#'          abbreviated.
#' @param corr.method Indicates the correlation computation method. May be one of
#'          \code{"pearson"} (default), \code{"spearman"} or \code{"kendall"}.
#'          May be abbreviated.
#' @param p.numeric Logical, if \code{TRUE}, the p-values are printed
#'          as numbers. If \code{FALSE} (default), asterisks are used.
#'
#' @inheritParams plot_grpfrq
#' @inheritParams plot_gpt
#'
#' @return (Insisibily) returns the ggplot-object with the complete plot (\code{plot}) as well as the data frame that
#'           was used for setting up the ggplot-object (\code{df}) and the original correlation matrix
#'           (\code{corr.matrix}).
#'
#' @note If \code{data} is a matrix with correlation coefficients as returned by
#'       the \code{\link{cor}}-function, p-values can't be computed.
#'       Thus, \code{show.p} and \code{p.numeric}
#'       only have an effect if \code{data} is a \code{\link{data.frame}}.
#'
#' @details Required argument is either a \code{\link{data.frame}} or a matrix with correlation coefficients
#'            as returned by the \code{\link{cor}}-function. In case of ellipses, the
#'            ellipses size indicates the strength of the correlation. Furthermore,
#'            blue and red colors indicate positive or negative correlations, where
#'            stronger correlations are darker.
#'
#' @import ggplot2
#' @importFrom tidyr gather
#' @importFrom scales brewer_pal grey_pal
#' @importFrom stats cor cor.test na.omit
#' @export
sjp.corr <- function(data,
                     title = NULL,
                     axis.labels = NULL,
                     sort.corr = TRUE,
                     decimals = 3,
                     na.deletion = c("listwise", "pairwise"),
                     corr.method = c("pearson", "spearman", "kendall"),
                     geom.colors = "RdBu",
                     wrap.title = 50,
                     wrap.labels = 20,
                     show.legend = FALSE,
                     legend.title = NULL,
                     show.values = TRUE,
                     show.p = TRUE,
                     p.numeric = FALSE) {
  .Deprecated(msg = "'sjp.corr' is deprecated. Please use 'correlation::correlation()' and its related plot()-method.")

  # --------------------------------------------------------
  # check p-value-style option
  # --------------------------------------------------------
  opt <- getOption("p_zero")
  if (is.null(opt) || opt == FALSE) {
    p_zero <- ""
  } else {
    p_zero <- "0"
  }
  # --------------------------------------------------------
  # check args
  # --------------------------------------------------------
  na.deletion <- match.arg(na.deletion)
  corr.method <- match.arg(corr.method)
  # --------------------------------------------------------
  # try to automatically set labels is not passed as argument
  # --------------------------------------------------------
  if (is.null(axis.labels) && is.data.frame(data)) {
    axis.labels <- unname(sjlabelled::get_label(data, def.value = colnames(data)))
  }
  # ----------------------------
  # set color palette
  # ----------------------------
  if (is.brewer.pal(geom.colors[1])) {
    geom.colors <- scales::brewer_pal(palette = geom.colors[1])(5)
  } else if (geom.colors[1] == "gs") {
    geom.colors <- scales::grey_pal()(5)
  }
  # ----------------------------
  # check if user has passed a data frame
  # or a pca object
  # ----------------------------
  if (any(class(data) == "matrix")) {
    corr <- data
    cpvalues <- NULL
  } else {
    # missing deletion corresponds to
    # SPSS listwise
    if (na.deletion == "listwise") {
      data <- stats::na.omit(data)
      corr <- stats::cor(data, method = corr.method)
    }
    # missing deletion corresponds to
    # SPSS pairwise
    else {
      corr <- stats::cor(data, method = corr.method, use = "pairwise.complete.obs")
    }
    #---------------------------------------
    # if we have a data frame as argument,
    # compute p-values of significances
    #---------------------------------------
    computePValues <- function(df) {
      cp <- c()
      for (i in seq_len(ncol(df))) {
        pv <- c()
        for (j in seq_len(ncol(df))) {
          test <- suppressWarnings(stats::cor.test(df[[i]], df[[j]],
                                                   alternative = "two.sided",
                                                   method = corr.method))
          pv <- c(pv, round(test$p.value, 4))
        }
        cp <- rbind(cp, pv)
      }
      return(cp)
    }
    cpvalues <- computePValues(data)
  }
  # ----------------------------
  # check if user defined labels have been supplied
  # if not, use variable names from data frame
  # ----------------------------
  if (is.null(axis.labels)) axis.labels <- row.names(corr)
  # ----------------------------
  # Prepare length of title and labels
  # ----------------------------
  # check length of diagram title and split longer string at into new lines
  if (!is.null(title)) title <- sjmisc::word_wrap(title, wrap.title)
  # check length of x-axis-labels and split longer strings at into new lines
  if (!is.null(axis.labels)) axis.labels <- sjmisc::word_wrap(axis.labels, wrap.labels)
  # --------------------------------------------------------
  # order correlations from highest to lowest correlation coefficient
  # --------------------------------------------------------
  if (sort.corr) {
    neword <- order(corr[1, ])
    orderedCorr <- corr[neword, neword]
    # order variable labels as well
    axis.labels <- axis.labels[neword]
    if (!is.null(cpvalues)) cpvalues <- cpvalues[neword, neword]
  } else {
    cl <- ncol(corr)
    orderedCorr <- corr[cl:1, cl:1]
    axis.labels <- rev(axis.labels)
    if (!is.null(cpvalues)) cpvalues <- cpvalues[cl:1, cl:1]
  }
  # --------------------------------------------------------
  # prepare a ordering-index-column needed for the data frame
  # that is passed to the ggplot
  # --------------------------------------------------------
  yo <- c()
  for (i in seq_len(nrow(corr))) {
    yo <- c(yo, rep(i, nrow(corr)))
  }
  # --------------------------------------------------------
  # melt correlation matrix and create data frame
  # --------------------------------------------------------
  orderedCorr <- tidyr::gather(data.frame(orderedCorr), "var", "value",
                               !! seq_len(ncol(orderedCorr)), factor_key = TRUE)
  # orderedCorr <- melt(orderedCorr)
  if (!is.null(cpvalues))
    cpvalues <- tidyr::gather(data.frame(cpvalues), "var", "value",
                              !! seq_len(ncol(cpvalues)), factor_key = TRUE)
  # if (!is.null(cpvalues)) cpvalues <- melt(cpvalues)
  # bind additional information like order for x- and y-axis
  # as well as the size of plotted points
  orderedCorr <- cbind(orderedCorr, ordx = seq_len(nrow(corr)), ordy = yo)
  # --------------------------------------------------------
  # add column with significance value
  # --------------------------------------------------------
  if (!is.null(cpvalues)) {
    if (!p.numeric) {
      cpv <- sapply(cpvalues$value, get_p_stars)
    } else {
      cpv <- sapply(cpvalues$value, function(x) {
        if (x < 0.001)
          x <- sprintf("\n(< %s.001)", p_zero)
        else
          x <- sub("0", p_zero, sprintf("\n(%.*f)", decimals, x))
      })
    }
  } else {
    cpv <- ""
  }
  orderedCorr$ps <- cpv
  # --------------------------------------------------------
  # set visibility of labels
  # --------------------------------------------------------
  if (!show.values) {
    orderedCorr$val.labels <- ""
  } else {
    if (show.p) {
      orderedCorr$val.labels <- sprintf("%.*f%s", decimals, orderedCorr$value, orderedCorr$ps)
    } else {
      orderedCorr$val.labels <- sprintf("%.*f", decimals, orderedCorr$value)
    }
  }
  orderedCorr$val.labels[orderedCorr$ordx >= orderedCorr$ordy] <- NA

  orderedCorr$ordx <- as.factor(orderedCorr$ordx)
  orderedCorr$ordy <- as.factor(orderedCorr$ordy)
  message(sprintf("Computing correlation using %s-method with %s-deletion...", corr.method, na.deletion))
  # --------------------------------------------------------
  # start with base plot object here
  # --------------------------------------------------------
  corrPlot <- ggplot(orderedCorr, aes_string(x = "ordx", y = "ordy", fill = "value", label = "val.labels")) +
    geom_tile(size = 0, colour = "black") +
  # fill gradient colour from distinct color brewer palette. negative correlations are dark
  # red, positive corr. are dark blue, and they become lighter the closer they are to a
  # correlation coefficient of zero
    scale_x_discrete(labels = axis.labels, breaks = seq_len(length(axis.labels))) +
    scale_y_discrete(labels = axis.labels, breaks = seq_len(length(axis.labels))) +
    # set limits to (-1,1) to make sure the whole color palette is used. this
    # is the colour scale for geoms
    scale_fill_gradientn(colours = geom.colors, limits = c(-1,1)) +
    geom_text(size = 3.5, colour = "black") +
    labs(title = title, x = NULL, y = NULL)
  if (show.legend)
    corrPlot <- corrPlot + guides(fill = legend.title)
  else
    corrPlot <- corrPlot + guides(fill = "none")

  corrPlot
}
