#' @title Plot polynomials for linear regression
#' @name sjp.poly
#'
#' @description This function plots a scatter plot of a term \code{poly.term}
#'                against a response variable \code{x} and adds - depending on
#'                the amount of numeric values in \code{poly.degree} - multiple
#'                polynomial curves. A loess-smoothed line can be added to see
#'                which of the polynomial curves fits best to the data.
#'
#' @param x a vector, representing the response variable of a linear (mixed) model; or
#'          a linear (mixed) model as returned by \code{\link{lm}} or \code{\link[lme4]{lmer}}.
#' @param poly.term if \code{x} is a vector, \code{poly.term} should also be a vector, representing
#'          the polynomial term (independent variabl) in the model; if \code{x} is a
#'          fitted model, \code{poly.term} should be the polynomial term's name as character string.
#'          See 'Examples'.
#' @param poly.degree numeric, or numeric vector, indicating the degree of the polynomial.
#'          If \code{poly.degree} is a numeric vector, multiple polynomial curves for
#'          each degree are plotted. See 'Examples'.
#' @param axisTitle.x A label for the x axis. Use \code{NULL} to automatically detect 
#'          variable names that will be used as title (see \code{\link[sjmisc]{set_var_labels}}) for details).
#' @param axisTitle.y A label for the y axis. Use \code{NULL} to automatically detect 
#'          variable names that will be used as title (see \code{\link[sjmisc]{set_var_labels}}) for details).
#' @param showScatterPlot If \code{TRUE} (default), a scatter plot of response and predictor values
#'          for each predictor of the fitted model \code{fit} is plotted.
#'          Only applies if \code{type = "lm"} and fitted model has only one predictor,
#'          or if \code{type = "pred"} or \code{type = "resid"}.
#' @param showLoess If \code{TRUE}, an additional loess-smoothed line is plotted.
#' @param showLoessCI If \code{TRUE}, a confidence region for the loess-smoothed line
#'          will be plotted.
#' @param geom.colors User defined color palette for geoms. Must either be vector with two color values
#'          or a specific color palette code (see below).
#'          \itemize{
#'            \item If not specified, the \code{"Set1"} color brewer palette will be used.
#'            \item If \code{"gs"}, a greyscale will be used.
#'            \item If \code{geom.colors} is any valid color brewer palette name, the related \href{http://colorbrewer2.org}{color brewer} palette will be used. Use \code{display.brewer.all()} from the \code{RColorBrewer} package to view all available palette names.
#'            \item Else specify your own color values as vector, e.g. \code{geom.colors = c("#f00000", "#00ff00")}.
#'          }
#' @param geom.size size resp. width of plotted lines.
#' @param loessLineColor color of the loess-smoothed line. Only applies, if \code{showLoess = TRUE}.
#' @param pointColor color of the scatter plot's point. Only applies, if \code{showScatterPlot = TRUE}.
#' @param pointAlpha The alpha values of the scatter plot's point-geoms.
#'          Default is 0.2.
#'          Only applies if \code{type = "lm"} and fitted model has only one predictor,
#'          or if \code{type = "pred"} or \code{type = "resid"}.
#' @param printPlot If \code{TRUE} (default), plots the results as graph. Use \code{FALSE} if you don't
#'          want to plot any graphs. In either case, the ggplot-object will be returned as value.
#' @return (insisibily) returns the ggplot-object with the complete plot (\code{plot})
#'           as well as the data frame that was used for setting up the
#'           ggplot-object (\code{df}).
#' 
#' @details This function evaluates raw polynomials, \emph{not orthogonal} polynomials.
#'            Polynomials are computed using the \code{\link{poly}} function,
#'            with parameter \code{raw = TRUE}. \cr \cr
#'            To find out which polynomial degree fits best to the data, a loess-smoothed
#'            line can be added (with \code{showLoess = TRUE}). The polynomial curves
#'            that comes closest to the loess-smoothed line should be the best
#'            fit to the data.
#'   
#' @examples 
#' library(sjmisc)
#' data(efc)
#' # linear fit. loess-smoothed line indicates a more
#' # or less cubic curve
#' sjp.poly(efc$c160age, efc$quol_5, 1)
#' 
#' # quadratic fit
#' sjp.poly(efc$c160age, efc$quol_5, 2)
#' 
#' # cubic fit
#' sjp.poly(efc$c160age, efc$quol_5, 3)
#' 
#' # linear to cubic fit
#' sjp.poly(efc$c160age, efc$quol_5, 
#'          1:4, showScatterPlot = FALSE)
#' 
#' 
#' # fit sample model
#' fit <- lm(tot_sc_e ~ c12hour + e17age + e42dep, data = efc)
#' # inspect relationship between predictors and response
#' sjp.lm(fit, type = "pred", 
#'        showLoess = TRUE, showScatterPlot = FALSE)
#' # "e17age" not seems to be linear correlated to response
#' # try to find appropiate polynomial. Grey line (loess smoothed)
#' # indicates best fit. Looks like x^4 has the best fit.
#' sjp.poly(fit, "e17age", 2:4, showScatterPlot = FALSE)
#' 
#' @import ggplot2
#' @importFrom scales grey_pal brewer_pal
#' @export
sjp.poly <- function(x, 
                     poly.term, 
                     poly.degree,
                     axisTitle.x = NULL,
                     axisTitle.y = NULL,
                     showScatterPlot = TRUE,
                     showLoess = TRUE,
                     showLoessCI = TRUE,
                     geom.colors = NULL,
                     geom.size = .8,
                     loessLineColor = "#808080",
                     pointColor = "#404040",
                     pointAlpha = .2,
                     printPlot = TRUE) {
  # --------------------------------------------
  # check color parameter
  # --------------------------------------------
  # define required length of color palette
  collen <- length(poly.degree)
  # check for corrct color parameter
  if (!is.null(geom.colors)) {
    # do we have correct amount of colours?
    if (length(geom.colors) != collen) {
      # warn user abount wrong color palette
      warning(sprintf("Insufficient length of color palette provided. %i color values needed.", collen), call. = F)
      # set default palette
      geom.colors <- "Set1"
    }
  } else {
    geom.colors <- "Set1"
  }
  # check for color brewer palette
  if (is.brewer.pal(geom.colors[1])) {
    geom.colors <- scales::brewer_pal(palette = geom.colors[1])(collen)
  } else if (geom.colors[1] == "gs") {
    geom.colors <- scales::grey_pal()(collen)
  }
  # --------------------------------------------
  # parameter check: fitted model or variables?
  # --------------------------------------------
  if ((any(class(x) == "lmerMod" || any(class(x) == "merModLmerTest"))) && !requireNamespace("lme4", quietly = TRUE)) {
    stop("Package 'lme4' needed for this function to work. Please install it.", call. = FALSE)
  }
  if (any(class(x) == "lmerMod") || any(class(x) == "merModLmerTest")) {
    # retrieve response vector
    resp <- lme4::getME(x, "y")
    # retrieve polynomial term
    poly.term <- x@frame[[which(colnames(x@frame) == poly.term)]]
  } else if (any(class(x) == "lm")) {
    # retrieve response vector
    resp <- x$model[[1]]
    # retrieve polynomial term
    poly.term <- x$model[[which(colnames(x$model) == poly.term)]]
  } else {
    resp <- x
  }
  # --------------------------------------------
  # retrieve labels
  # --------------------------------------------
  if (is.null(axisTitle.x)) axisTitle.x <- sjmisc:::autoSetVariableLabels(poly.term)
  if (is.null(axisTitle.y)) axisTitle.y <- sjmisc:::autoSetVariableLabels(resp)
  # no labels found? set default then
  if (is.null(axisTitle.x)) axisTitle.x <- "Polynomial term"
  if (is.null(axisTitle.y)) axisTitle.y <- "Response"
  # --------------------------------------------
  # init data frame
  # --------------------------------------------
  plot.df <- data.frame()
  # --------------------------------------------
  # if user wants to plot multiple curves for
  # polynomials, create data frame for each curve here
  # --------------------------------------------
  for (i in poly.degree) {
    # poly-function can't cope with missings, so remove them here
    mydat <- na.omit(data.frame(x = poly.term, y = resp))
    # fit model with polynomials
    fit <- lm(mydat$y ~ poly(mydat$x, i, raw = TRUE))
    # create data frame with raw data and the fitted poly-curve
    plot.df <- rbind(plot.df, cbind(mydat, predict(fit), sprintf("x^%i", i)))
  }
  # name df
  colnames(plot.df) <- c("x","y", "pred", "grp")
  # create plot
  polyplot <- ggplot(plot.df, aes(x, y, colour = grp))
  # show scatter plot as well?
  if (showScatterPlot) polyplot <- polyplot + geom_jitter(colour = pointColor, alpha = pointAlpha)
  # show loess curve? this curve indicates the "perfect" curve through
  # the data
  if (showLoess) polyplot <- polyplot + stat_smooth(method = "loess", 
                                                    color = loessLineColor,
                                                    se = showLoessCI,
                                                    size = geom.size)
  # add curves for polynomials
  polyplot <- polyplot + geom_line(aes(y = pred), size = geom.size) + 
    scale_color_manual(values = geom.colors,
                       labels = lapply(poly.degree, function(j) bquote(x^.(j)))) +
    labs(x = axisTitle.x, y = axisTitle.y, colour = "Polynomial\ndegrees")
  # ---------------------------------------------------------
  # Check whether ggplot object should be returned or plotted
  # ---------------------------------------------------------
  if (printPlot) print(polyplot)
  # -------------------------------------
  # return results
  # -------------------------------------
  invisible(structure(class = "sjppoly",
                      list(plot = polyplot,
                           df = plot.df)))
}
