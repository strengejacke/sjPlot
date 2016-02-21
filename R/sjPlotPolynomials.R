#' @title Plot polynomials for (generalized) linear regression
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
#' @param poly.scale logical, if \code{TRUE}, \code{poly.term} will be scaled before
#'          linear regression is computed. Default is \code{FALSE}. Scaling the polynomial
#'          term may have an impact on the resulting p-values.
#' @param fun linear function when modelling polynomial terms. Use \code{fun = "lm"}
#'          for linear models, or \code{fun = "glm"} for generalized linear models.
#'          When \code{x} is not a vector, but a fitted model object, the function
#'          is detected automatically. If \code{x} is a vector, \code{fun} defaults
#'          to \code{"lm"}.
#' @param axisTitle.x A label for the x axis. Use \code{NULL} to automatically detect 
#'          variable names that will be used as title (see \code{\link[sjmisc]{set_label}}) for details).
#' @param axisTitle.y A label for the y axis. Use \code{NULL} to automatically detect 
#'          variable names that will be used as title (see \code{\link[sjmisc]{set_label}}) for details).
#' @param showScatterPlot If \code{TRUE} (default), a scatter plot of response and predictor values
#'          for each predictor of the fitted model \code{fit} is plotted.
#'          Only applies if \code{type = "lm"} and fitted model has only one predictor,
#'          or if \code{type = "pred"} or \code{type = "resid"}.
#' @param showLoess If \code{TRUE}, an additional loess-smoothed line is plotted.
#' @param showLoessCI If \code{TRUE}, a confidence region for the loess-smoothed line
#'          will be plotted.
#' @param showPValues logical, if \code{TRUE} (default), p-values for polynomial terms are
#'          printed to the console.
#' @param geom.colors User defined color palette for geoms. Must either be vector with two color values
#'          or a specific color palette code. See 'Note' in \code{\link{sjp.grpfrq}}.
#' @param geom.size size resp. width of plotted lines.
#' @param loessLineColor color of the loess-smoothed line. Only applies, if \code{showLoess = TRUE}.
#' @param pointColor color of the scatter plot's point. Only applies, if \code{showScatterPlot = TRUE}.
#' @param pointAlpha The alpha values of the scatter plot's point-geoms. Default is 0.2.
#' @param printPlot If \code{TRUE} (default), plots the results as graph. Use \code{FALSE} if you don't
#'          want to plot any graphs. In either case, the ggplot-object will be returned as value.
#' @return (insisibily) returns 
#'           \describe{
#'            \item{\code{plot}}{the ggplot-object with the complete plot}
#'            \item{\code{df}}{the data frame that was used for setting up the ggplot-object}
#'            \item{\code{cutpoints}}{a data frame that indicates x-values and predicted y-values of each direction change in the loess curvature}
#'           }
#' 
#' @details For each polynomial degree, a simple linear regression on \code{x} (resp.
#'            the extracted response, if \code{x} is a fitted model) is performed,
#'            where only the polynomial term \code{poly.term} is included as independent variable.
#'            Thus, \code{lm(y ~ x + I(x^2) + ... + I(x^i))} is repeatedly computed 
#'            for all values in \code{poly.degree}, and the predicted values of
#'            the reponse are plotted against the raw values of \code{poly.term}.
#'            If \code{x} is a fitted model, other covariates are ignored when 
#'            finding the best fitting polynomial. \cr \cr
#'            This function evaluates raw polynomials, \emph{not orthogonal} polynomials.
#'            Polynomials are computed using the \code{\link{poly}} function,
#'            with argument \code{raw = TRUE}. \cr \cr
#'            To find out which polynomial degree fits best to the data, a loess-smoothed
#'            line (in dark grey) can be added (with \code{showLoess = TRUE}). The polynomial curves
#'            that comes closest to the loess-smoothed line should be the best
#'            fit to the data.
#'   
#' @seealso To plot marginal effects of polynomial terms, call \code{\link{sjp.lm}} with \code{type = "poly"},
#'            or \code{\link{sjp.lmer}} respectively for linear mixed models.
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
#' # linear to cubic fit
#' sjp.poly(efc$c160age, efc$quol_5, 
#'          1:4, showScatterPlot = FALSE)
#' 
#' 
#' library(sjmisc)
#' data(efc)
#' # fit sample model
#' fit <- lm(tot_sc_e ~ c12hour + e17age + e42dep, data = efc)
#' # inspect relationship between predictors and response
#' sjp.lm(fit, type = "pred", 
#'        showLoess = TRUE, showScatterPlot = FALSE)
#' # "e17age" does not seem to be linear correlated to response
#' # try to find appropiate polynomial. Grey line (loess smoothed)
#' # indicates best fit. Looks like x^4 has the best fit,
#' # however, only x^3 has significant p-values.
#' sjp.poly(fit, "e17age", 2:4, showScatterPlot = FALSE)
#' 
#' \dontrun{
#' # fit new model
#' fit <- lm(tot_sc_e ~ c12hour + e42dep +
#'           e17age + I(e17age^2) + I(e17age^3),
#'           data = efc)
#' # plot marginal effects of polynomial term
#' sjp.lm(fit, type = "poly", poly.term = "e17age")}
#' 
#' @import ggplot2
#' @importFrom scales grey_pal brewer_pal
#' @importFrom stats lm glm binomial predict
#' @export
sjp.poly <- function(x, 
                     poly.term, 
                     poly.degree,
                     poly.scale = FALSE,
                     fun = NULL,
                     axisTitle.x = NULL,
                     axisTitle.y = NULL,
                     showScatterPlot = TRUE,
                     showLoess = TRUE,
                     showLoessCI = TRUE,
                     showPValues = TRUE,
                     geom.colors = NULL,
                     geom.size = .8,
                     loessLineColor = "#808080",
                     pointColor = "#404040",
                     pointAlpha = .2,
                     printPlot = TRUE) {
  # --------------------------------------------
  # check color parameter
  # --------------------------------------------
  geom.colors <- col_check2(geom.colors, length(poly.degree))
  # --------------------------------------------
  # check poly.term parameter
  # --------------------------------------------
  if (is.character(poly.term))
    defv <- poly.term
  else
    defv <- get_var_name(deparse(substitute(poly.term)))
  # --------------------------------------------
  # parameter check: fitted model or variables?
  # --------------------------------------------
  if ((any(class(x) == "glmerMod") || any(class(x) == "lmerMod" || any(class(x) == "merModLmerTest"))) && !requireNamespace("lme4", quietly = TRUE)) {
    stop("Package 'lme4' needed for this function to work. Please install it.", call. = FALSE)
  }
  if (any(class(x) == "glmerMod") || any(class(x) == "lmerMod") || any(class(x) == "merModLmerTest")) {
    # retrieve response vector
    resp <- lme4::getME(x, "y")
    # retrieve polynomial term
    poly.term <- x@frame[[which(colnames(x@frame) == poly.term)]]
  } else if (any(class(x) == "lm") || any(class(x) == "glm")) {
    # retrieve response vector
    resp <- x$model[[1]]
    # retrieve polynomial term
    poly.term <- x$model[[which(colnames(x$model) == poly.term)]]
  } else {
    resp <- x
  }
  # --------------------------------------------
  # check for glm or lm
  # --------------------------------------------
  if (is.null(fun)) {
    if (any(class(x) == "glmerMod") || any(class(x) == "glm")) {
      fun <- "glm"
    } else {
      fun <- "lm"
    }
  }
  # --------------------------------------------
  # retrieve labels
  # --------------------------------------------
  if (is.null(axisTitle.x))
    axisTitle.x <- sjmisc::get_label(poly.term, def.value = defv)
  if (is.null(axisTitle.y)) 
    axisTitle.y <- sjmisc::get_label(resp, def.value = "Response")
  # no labels found? set default then
  if (is.null(axisTitle.x)) axisTitle.x <- "Polynomial term"
  if (is.null(axisTitle.y)) axisTitle.y <- "Response"
  # --------------------------------------------
  # init data frame
  # --------------------------------------------
  plot.df <- data.frame()
  # scale polynomial term?
  if (poly.scale) poly.term <- scale(poly.term)
  # --------------------------------------------
  # get cutpoints for loess curve
  # --------------------------------------------
  cutpoints <- get_loess_cutpoints(na.omit(data.frame(x = poly.term, y = resp)))
  # --------------------------------------------
  # if user wants to plot multiple curves for
  # polynomials, create data frame for each curve here
  # --------------------------------------------
  for (i in poly.degree) {
    # poly-function can't cope with missings, so remove them here
    mydat <- na.omit(data.frame(x = poly.term, y = resp))
    # fit model with polynomials
    if (fun == "lm")
      fit <- stats::lm(mydat$y ~ poly(mydat$x, i, raw = TRUE))
    else
      fit <- stats::glm(mydat$y ~ poly(mydat$x, i, raw = TRUE), 
                        family = stats::binomial(link = "logit"))
    # check whether we have an integer poly.degree
    # or a float value
    poly.digit <- ifelse(i %% 1 == 0, 0, 1)
    # create data frame with raw data and the fitted poly-curve
    plot.df <- rbind(plot.df, cbind(mydat, 
                                    stats::predict(fit), 
                                    sprintf("x^%.*f", poly.digit, i)))
    # print p-values?
    if (showPValues) {
      # get p-values
      pvals <- summary(fit)$coefficients[-1, 4]
      # prepare output string
      p.out <- sprintf("Polynomial degrees: %.*f\n---------------------\n", poly.digit, i)
      # iterate polynomial terms and print p-value for each polynom
      for (j in 1:i) p.out <- paste0(p.out, sprintf("p(x^%i): %.3f\n", j, unname(pvals[j])))
      # add separator line after each model
      p.out <- paste0(p.out, "\n")
      # print p-values for fitted model
      cat(p.out)
    }
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
  polyplot <- polyplot + 
    geom_line(aes(y = pred), size = geom.size) + 
    scale_color_manual(values = geom.colors, labels = lapply(poly.degree, function(j) bquote(x^.(j)))) +
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
                           df = plot.df,
                           cutpoints = cutpoints)))
}


#' @importFrom stats loess predict
get_loess_cutpoints <- function(mydat) {
  # sort data frame by x-values
  mydat <- mydat[order(mydat$x), ]
  # fit loess
  fit <- stats::loess(y ~ x, mydat)
  # get predicted values
  preds <- unique(stats::predict(fit))
  xuni <- unique(mydat$x)
  # define counter
  cnt <- 1
  cutpoints <- c()
  xvals <- c()
  # initial direction for finding first cutpoint?
  direction <- ifelse(preds[cnt + 1] > preds[cnt], "up", "down")
  # "follow" path of loess line until cutpoint
  # then save value and change direction
  while (cnt < length(preds)) {
    if (direction == "up") {
      if (preds[cnt + 1] < preds[cnt]) {
        direction <- "down"
        cutpoints <- c(cutpoints, preds[cnt])
        xvals <- c(xvals, xuni[cnt])
      }
    } else {
      if (preds[cnt + 1] > preds[cnt]) {
        direction <- "up"
        cutpoints <- c(cutpoints, preds[cnt])
        xvals <- c(xvals, xuni[cnt])
      }
    }
    cnt <- cnt + 1
  }
  
  return(data.frame(cutpoint.x = xvals, cutpoint.y = cutpoints))
}