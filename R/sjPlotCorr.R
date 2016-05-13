# bind global variables
utils::globalVariables(c("ordx", "ordy"))

#' @title Plot correlation matrix
#' @name sjp.corr
#'
#' @description Plot correlation matrix as ellipses or tiles.
#'
#' @seealso \code{\link{sjt.corr}}
#'
#' @param data matrix with correlation coefficients as returned by the 
#'          \code{\link{cor}}-function, or a \code{data.frame} of variables where
#'          correlations between columns should be computed.
#' @param type indicates whether the geoms of correlation values should be plotted
#'          as \code{"circle"} (default) or as \code{"tile"}.
#' @param sort.corr logical, if \code{TRUE} (default), the axis labels are sorted
#'          according to the correlation strength. If \code{FALSE}, axis labels
#'          appear in order of how variables were included in the cor-computation or
#'          data frame.
#' @param decimals indicates how many decimal values after comma are printed when
#'          the values labels are shown. Default is 3. Only applies when
#'          \code{show.values = TRUE}.
#' @param na.deletion indicates how missing values are treated. May be either
#'          \code{"listwise"} (default) or \code{"pairwise"}. May be
#'          abbreviated.
#' @param corr.method indicates the correlation computation method. May be one of
#'          \code{"spearman"} (default), \code{"pearson"} or \code{"kendall"}.
#'          May be abbreviated.
#' @param p.numeric logical, if \code{TRUE}, the p-values are printed 
#'          as numbers. If \code{FALSE} (default), asterisks are used.
#'          
#' @inheritParams sjp.grpfrq
#' @inheritParams sjp.lm
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
#' @examples
#' # create data frame with 5 random variables
#' mydf <- data.frame(cbind(runif(10), runif(10), runif(10), 
#'                          runif(10), runif(10)))
#'
#' # plot correlation matrix using circles
#' sjp.corr(mydf)
#'
#' # plot correlation matrix using square tiles without diagram background
#' sjp.corr(mydf, type = "tile")
#'
#' # -------------------------------
#' # Data from the EUROFAMCARE sample dataset
#' # -------------------------------
#' library(sjmisc)
#' data(efc)
#'
#' # retrieve variable and value labels
#' varlabs <- get_label(efc)
#'
#' # create data frame
#' vars.index <- c(1, 4, 15, 19, 20, 21, 22, 24, 25)
#' mydf <- data.frame(efc[, vars.index])
#' colnames(mydf) <- varlabs[vars.index]
#'
#' # show legend
#' sjp.corr(mydf, type = "tile", show.legend = TRUE)
#'
#' # -------------------------------
#' # auto-detection of labels
#' # -------------------------------
#' # blank theme
#' sjp.setTheme(theme = "blank", axis.angle.x = 90)
#' sjp.corr(efc[, vars.index])
#'
#'
#' @import ggplot2
#' @import sjmisc
#' @importFrom tidyr gather
#' @importFrom scales brewer_pal grey_pal
#' @importFrom stats cor cor.test na.omit
#' @export
sjp.corr <- function(data,
                     title = NULL,
                     axis.labels = NULL,
                     type = c("circle", "tile"),
                     sort.corr = TRUE,
                     decimals = 3,
                     na.deletion = c("listwise", "pairwise"),
                     corr.method = c("spearman", "pearson", "kendall"),
                     geom.colors = "RdBu",
                     geom.size = 15,
                     wrap.title = 50,
                     wrap.labels = 20,
                     show.legend = FALSE,
                     legend.title = NULL,
                     show.values = TRUE,
                     show.p = TRUE,
                     p.numeric = FALSE,
                     prnt.plot = TRUE) {
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
  type <- match.arg(type)
  # --------------------------------------------------------
  # try to automatically set labels is not passed as argument
  # --------------------------------------------------------
  if (is.null(axis.labels) && is.data.frame(data)) {
    axis.labels <- c()
    # if yes, iterate each variable
    for (i in 1:ncol(data)) {
      # retrieve variable name attribute
      vn <- sjmisc::get_label(data[[i]], def.value = colnames(data)[i])
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
      for (i in 1:ncol(df)) {
        pv <- c()
        for (j in 1:ncol(df)) {
          test <- stats::cor.test(df[[i]], df[[j]], alternative = "two.sided", 
                                  method = corr.method)
          pv <- cbind(pv, round(test$p.value, 4))
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
  # --------------------------------------------------------
  # unlist labels
  # --------------------------------------------------------
  if (!is.null(axis.labels) && is.list(axis.labels)) axis.labels <- unlistlabels(axis.labels)
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
    if (!is.null(cpvalues)) {
      cpvalues <- cpvalues[neword, neword]
    }
  } else {
    orderedCorr <- rev(corr)
    axis.labels <- rev(axis.labels)
    if (!is.null(cpvalues)) cpvalues <- rev(cpvalues)
  }
  # --------------------------------------------------------
  # prepare a ordering-index-column needed for the data frame
  # that is passed to the ggplot
  # --------------------------------------------------------
  yo <- c()
  for (i in 1:nrow(corr)) {
    yo <- c(yo, c(rep(i, nrow(corr))))
  }
  # --------------------------------------------------------
  # melt correlation matrix and create data frame
  # --------------------------------------------------------
  # first, save original matrix for return value
  oricor <- orderedCorr
  orderedCorr <- tidyr::gather(data.frame(orderedCorr), "var", "value", 
                               1:ncol(orderedCorr), factor_key = TRUE)
  # orderedCorr <- melt(orderedCorr)
  if (!is.null(cpvalues)) 
    cpvalues <- tidyr::gather(data.frame(cpvalues), "var", "value",
                              1:ncol(cpvalues), factor_key = TRUE)
  # if (!is.null(cpvalues)) cpvalues <- melt(cpvalues)
  # bind additional information like order for x- and y-axis
  # as well as the size of plotted points
  orderedCorr <- cbind(orderedCorr, ordx = c(1:nrow(corr)), ordy = yo, 
                       psize = c(exp(abs(orderedCorr$value)) * geom.size))
  # diagonal circles should be hidden, set their point size to 0
  orderedCorr$psize[which(orderedCorr$value >= 0.999)] <- 0
  # remove lower trianglwe of geoms
  orderedCorr$psize[which(orderedCorr$ordx > orderedCorr$ordy)]  <- NA

  orderedCorr$ordx <- as.factor(orderedCorr$ordx)
  orderedCorr$ordy <- as.factor(orderedCorr$ordy)
  # --------------------------------------------------------
  # add column with significance value
  # --------------------------------------------------------
  cpv <- c()
  if (!is.null(cpvalues)) {
    if (!p.numeric) {
      for (cpi in 1:nrow(cpvalues)) {
        cpv <- c(cpv, get_p_stars(cpvalues$value[cpi]))
      }
    } else {
      cpv <- cpvalues$value
      cpv <- sapply(cpv, function(x) if (x < 0.001) 
                                       x <- sprintf("\n(< %s.001)", p_zero) 
                                     else 
                                       x <- sub("0", p_zero, sprintf("\n(%.*f)", decimals, x)))
    }
  } else {
    cpv <- ""
  }
  orderedCorr$ps <- cpv
  # --------------------------------------------------------
  # set visibility of labels
  # --------------------------------------------------------
  if (!show.values) {
    correlationValueLabels <- ""
    correlationPValues <- ""
  } else {
    correlationValueLabels <- ifelse(is.na(orderedCorr$psize), sprintf("%.*f", decimals, orderedCorr$value), "")
    if (show.p) {
      correlationPValues <- ifelse(is.na(orderedCorr$psize), orderedCorr$ps, "")
    } else {
      correlationPValues <- ""
    }
  }
  message(sprintf("Computing correlation using %s-method with %s-deletion...", corr.method, na.deletion))
  # --------------------------------------------------------
  # start with base plot object here
  # --------------------------------------------------------
  corrPlot <- ggplot(data = orderedCorr, aes(x = ordx, y = ordy, fill = value, colour = value))
  # corrPlot <- ggplot(data=orderedCorr, aes(x=ordx, y=ordy, fill=value))
  # --------------------------------------------------------
  # determine the geom type, either points when "type" is "circles"
  # --------------------------------------------------------
  if (type == "circle") {
    corrPlot <- corrPlot +
      geom_point(shape = 21, size = orderedCorr$psize, colour = "black")
  }
  # --------------------------------------------------------
  # or boxes / tiles when "type" is "tile"
  # --------------------------------------------------------
  else {
    corrPlot <- corrPlot + geom_tile()
  }
  # fill gradient colour from distinct color brewer palette. negative correlations are dark
  # red, positive corr. are dark blue, and they become lighter the closer they are to a
  # correlation coefficient of zero
  corrPlot <- corrPlot +
    scale_x_discrete(labels = axis.labels) +
    scale_y_discrete(labels = axis.labels) +
    # set limits to (-1,1) to make sure the whole color palette is used. this
    # is the colour scale for geoms
    scale_fill_gradientn(colours = geom.colors, limits = c(-1,1)) +
    # and here we have the colour scale for the text labels
    scale_colour_gradient2(low = "#ca0020", mid = "grey40", high = "#0571b0", limits = c(-1,1)) +
    geom_text(label = sprintf("%s%s", correlationValueLabels, correlationPValues)) +
    labs(title = title, x = NULL, y = NULL, fill = legend.title)
  if (show.legend) {
    corrPlot <- corrPlot + guides(colour = FALSE)
  } else {
    corrPlot <- corrPlot + guides(fill = FALSE, colour = FALSE)
  }
  # ---------------------------------------------------------
  # Check whether ggplot object should be returned or plotted
  # ---------------------------------------------------------
  if (prnt.plot) graphics::plot(corrPlot)
  # -------------------------------------
  # return results
  # -------------------------------------
  invisible(structure(class = "sjpcorr",
                      list(plot = corrPlot,
                           df = orderedCorr,
                           corr.matrix = oricor)))
}
