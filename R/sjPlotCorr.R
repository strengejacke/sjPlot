# bind global variables
utils::globalVariables(c("ordx", "ordy"))

#' @title Plot correlation matrix
#' @name sjp.corr
#'
#' @description Plot correlation matrix as ellipses or tiles. Required argument is either
#'                a \code{\link{data.frame}} or a matrix with correlation coefficients 
#'                as returned by the \code{\link{cor}}-function. In case of ellipses, the
#'                ellipses size indicates the strength of the correlation. Furthermore,
#'                blue and red colors indicate positive or negative correlations, where
#'                stronger correlations are darker.
#'
#' @seealso \code{\link{sjt.corr}}
#'
#' @param data matrix with correlation coefficients as returned by the 
#'          \code{\link{cor}}-function, or a \code{\link{data.frame}} of variables that
#'          should be correlated.
#' @param axisLabels labels for the x- andy y-axis. AxisLabels are detected automatically
#'          if \code{data} is a \code{\link{data.frame}} where variables have 
#'          label attributes (see \code{\link[sjmisc]{set_label}}) 
#'          for details).
#' @param type indicates whether the geoms of correlation values should be plotted
#'          as \code{"circle"} (default) or as \code{"tile"}.
#' @param sortCorrelations logical, if \code{TRUE} (default), the axis labels are sorted
#'          according to the correlation strength. If \code{FALSE}, axis labels
#'          appear in order of how variables were included in the cor-computation or
#'          data frame.
#' @param decimals indicates how many decimal values after comma are printed when
#'          the values labels are shown. Default is 3. Only applies when
#'          \code{showValueLabels = TRUE}.
#' @param missingDeletion indicates how missing values are treated. May be either
#'          \code{"listwise"} (default) or \code{"pairwise"}.
#' @param corMethod indicates the correlation computation method. May be one of
#'          \code{"spearman"} (default), \code{"pearson"} or \code{"kendall"}.
#' @param geom.size specifies the circle size factor. The circle size depends on the correlation
#'          value multiplicated with this factor. Default is 15.
#' @param showValueLabels logical, whether correlation values should be plotted to each geom
#' @param showPValues logical, whether significance levels (p-values) of correlations should
#'          be plotted to each geom. See 'Note'.
#' @param pvaluesAsNumbers logical, if \code{TRUE}, the significance levels (p-values) are printed as numbers.
#'          if \code{FALSE} (default), asterisks are used. See 'Note'.
#' @param geom.colors color palette for fillng the geoms. If not specified, the diverging color palette
#'          from the color brewer palettes (RdBu) is used, resulting in red colors for negative and blue colors
#'          for positive correlations, that become lighter the weaker the correlations are. Use any
#'          color palette that is suitbale for the \code{scale_fill_gradientn} argument of ggplot2.
#'          
#' @inheritParams sjp.grpfrq
#' 
#' @return (Insisibily) returns the ggplot-object with the complete plot (\code{plot}) as well as the data frame that
#'           was used for setting up the ggplot-object (\code{df}) and the original correlation matrix
#'           (\code{corr.matrix}).
#'
#' @note If \code{data} is a matrix with correlation coefficients as returned by 
#'       the \code{\link{cor}}-function, p-values can't be computed.
#'       Thus, \code{showPValues} and \code{pvaluesAsNumbers}
#'       only have an effect if \code{data} is a \code{\link{data.frame}}.
#'
#' @examples
#' # create data frame with 5 random variables
#' mydf <- data.frame(cbind(runif(10), 
#'                          runif(10), 
#'                          runif(10), 
#'                          runif(10), 
#'                          runif(10)))
#'
#' # plot correlation matrix using circles
#' sjp.corr(mydf)
#'
#' # plot correlation matrix using square tiles without diagram background
#' sjp.corr(mydf, type = "tile")
#'
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
#' sjp.corr(mydf, type = "tile", hideLegend = FALSE)
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
                     axisLabels = NULL,
                     type = "circle",
                     sortCorrelations = TRUE,
                     decimals = 3,
                     missingDeletion = "listwise",
                     corMethod = "spearman",
                     geom.colors = "RdBu",
                     geom.size = 15,
                     breakTitleAt = 50,
                     breakLabelsAt = 20,
                     hideLegend = TRUE,
                     legendTitle = NULL,
                     showValueLabels = TRUE,
                     showPValues = TRUE,
                     pvaluesAsNumbers = FALSE,
                     printPlot = TRUE) {
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
  # try to automatically set labels is not passed as argument
  # --------------------------------------------------------
  if (is.null(axisLabels) && is.data.frame(data)) {
    axisLabels <- c()
    # if yes, iterate each variable
    for (i in 1:ncol(data)) {
      # retrieve variable name attribute
      vn <- sjmisc::get_label(data[[i]], def.value = colnames(data)[i])
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
  # ----------------------------
  # set color palette
  # ----------------------------
  if (is.brewer.pal(geom.colors[1])) {
    geom.colors <- scales::brewer_pal(palette = geom.colors[1])(5)
  } else if (geom.colors[1] == "gs") {
    geom.colors <- scales::grey_pal()(5)
  }
  # ----------------------------
  # check for valid argument
  # ----------------------------
  if (corMethod != "pearson" && corMethod != "spearman" && corMethod != "kendall") {
    warning("argument 'corMethod' must be one of: pearson, spearman or kendall.", call. = F)
    return(invisible(NULL))
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
    if (missingDeletion == "listwise") {
      data <- stats::na.omit(data)
      corr <- stats::cor(data, method = corMethod)
    }
    # missing deletion corresponds to
    # SPSS pairwise
    else {
      corr <- stats::cor(data, method = corMethod, use = "pairwise.complete.obs")
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
          test <- stats::cor.test(df[[i]], 
                                  df[[j]], 
                                  alternative = "two.sided", 
                                  method = corMethod)
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
  if (is.null(axisLabels)) axisLabels <- row.names(corr)
  # --------------------------------------------------------
  # unlist labels
  # --------------------------------------------------------
  if (!is.null(axisLabels) && is.list(axisLabels)) axisLabels <- unlistlabels(axisLabels)
  # ----------------------------
  # Prepare length of title and labels
  # ----------------------------
  # check length of diagram title and split longer string at into new lines
  if (!is.null(title)) title <- sjmisc::word_wrap(title, breakTitleAt)
  # check length of x-axis-labels and split longer strings at into new lines
  if (!is.null(axisLabels)) axisLabels <- sjmisc::word_wrap(axisLabels, breakLabelsAt)
  # --------------------------------------------------------
  # order correlations from highest to lowest correlation coefficient
  # --------------------------------------------------------
  if (sortCorrelations) {
    neword <- order(corr[1, ])
    orderedCorr <- corr[neword, neword]
    # order variable labels as well
    axisLabels <- axisLabels[neword]
    if (!is.null(cpvalues)) {
      cpvalues <- cpvalues[neword, neword]
    }
  } else {
    orderedCorr <- rev(corr)
    axisLabels <- rev(axisLabels)
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
  orderedCorr <- tidyr::gather(data.frame(orderedCorr), 
                               "var", 
                               "value", 
                               1:ncol(orderedCorr),
                               factor_key = TRUE)
  # orderedCorr <- melt(orderedCorr)
  if (!is.null(cpvalues)) cpvalues <- tidyr::gather(data.frame(cpvalues), 
                                                    "var", 
                                                    "value", 
                                                    1:ncol(cpvalues),
                                                    factor_key = TRUE)
  # if (!is.null(cpvalues)) cpvalues <- melt(cpvalues)
  # bind additional information like order for x- and y-axis
  # as well as the size of plotted points
  orderedCorr <- cbind(orderedCorr, 
                       ordx = c(1:nrow(corr)), 
                       ordy = yo, 
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
    if (!pvaluesAsNumbers) {
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
    cpv <- c("")
  }
  orderedCorr$ps <- cpv
  # --------------------------------------------------------
  # set visibility of labels
  # --------------------------------------------------------
  if (!showValueLabels) {
    correlationValueLabels <- c("")
    correlationPValues <- c("")
  } else {
    correlationValueLabels <- ifelse(is.na(orderedCorr$psize), sprintf("%.*f", decimals, orderedCorr$value), "")
    if (showPValues) {
      correlationPValues <- ifelse(is.na(orderedCorr$psize), orderedCorr$ps, "")
    } else {
      correlationPValues <- c("")
    }
  }
  message(sprintf("Computing correlation using %s-method with %s-deletion...", corMethod, missingDeletion))
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
    scale_x_discrete(labels = axisLabels) +
    scale_y_discrete(labels = axisLabels) +
    # set limits to (-1,1) to make sure the whole color palette is used
    scale_fill_gradientn(colours = geom.colors, limits = c(-1,1)) +
    scale_colour_gradient2(low = "#ca0020", mid = "grey40", high = "#0571b0", limits = c(-1,1)) +
    geom_text(label = sprintf("%s%s", correlationValueLabels, correlationPValues)) +
    labs(title = title, x = NULL, y = NULL, fill = legendTitle)
  if (hideLegend) {
    corrPlot <- corrPlot + guides(fill = FALSE, colour = FALSE)
  } else {
    corrPlot <- corrPlot + guides(colour = FALSE)
  }
  # ---------------------------------------------------------
  # Check whether ggplot object should be returned or plotted
  # ---------------------------------------------------------
  if (printPlot) graphics::plot(corrPlot)
  # -------------------------------------
  # return results
  # -------------------------------------
  invisible(structure(class = "sjpcorr",
                      list(plot = corrPlot,
                           df = orderedCorr,
                           corr.matrix = oricor)))
}
