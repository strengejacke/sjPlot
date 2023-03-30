#' @title Plot polynomials for (generalized) linear regression
#' @name sjp.poly
#'
#' @description This function plots a scatter plot of a term \code{poly.term}
#'                against a response variable \code{x} and adds - depending on
#'                the amount of numeric values in \code{poly.degree} - multiple
#'                polynomial curves. A loess-smoothed line can be added to see
#'                which of the polynomial curves fits best to the data.
#'
#' @param x A vector, representing the response variable of a linear (mixed) model; or
#'          a linear (mixed) model as returned by \code{\link{lm}} or \code{\link[lme4]{lmer}}.
#' @param poly.term If \code{x} is a vector, \code{poly.term} should also be a vector, representing
#'          the polynomial term (independent variabl) in the model; if \code{x} is a
#'          fitted model, \code{poly.term} should be the polynomial term's name as character string.
#'          See 'Examples'.
#' @param poly.degree Numeric, or numeric vector, indicating the degree of the polynomial.
#'          If \code{poly.degree} is a numeric vector, multiple polynomial curves for
#'          each degree are plotted. See 'Examples'.
#' @param poly.scale Logical, if \code{TRUE}, \code{poly.term} will be scaled before
#'          linear regression is computed. Default is \code{FALSE}. Scaling the polynomial
#'          term may have an impact on the resulting p-values.
#' @param fun Linear function when modelling polynomial terms. Use \code{fun = "lm"}
#'          for linear models, or \code{fun = "glm"} for generalized linear models.
#'          When \code{x} is not a vector, but a fitted model object, the function
#'          is detected automatically. If \code{x} is a vector, \code{fun} defaults
#'          to \code{"lm"}.
#' @param show.loess Logical, if \code{TRUE}, an additional loess-smoothed line is plotted.
#' @param show.loess.ci Logical, if \code{TRUE}, a confidence region for the loess-smoothed line
#'          will be plotted.
#' @param show.p Logical, if \code{TRUE} (default), p-values for polynomial terms are
#'          printed to the console.
#' @param loess.color Color of the loess-smoothed line. Only applies, if \code{show.loess = TRUE}.
#' @param show.scatter Logical, if TRUE (default), adds a scatter plot of data
#'    points to the plot.
#' @param point.alpha Alpha value of point-geoms in the scatter plots. Only
#'    applies, if \code{show.scatter = TRUE}.
#' @param point.color Color of of point-geoms in the scatter plots. Only applies,
#'    if \code{show.scatter = TRUE.}
#'
#' @return A ggplot-object.
#'
#'
#' @inheritParams plot_model
#' @inheritParams plot_scatter
#' @inheritParams plot_grpfrq
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
#'            line (in dark grey) can be added (with \code{show.loess = TRUE}). The polynomial curves
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
#' # linear to cubic fit
#' sjp.poly(efc$c160age, efc$quol_5, 1:4, show.scatter = FALSE)
#'
#'
#' # fit sample model
#' fit <- lm(tot_sc_e ~ c12hour + e17age + e42dep, data = efc)
#' # inspect relationship between predictors and response
#' plot_model(fit, type = "slope")
#' # "e17age" does not seem to be linear correlated to response
#' # try to find appropiate polynomial. Grey line (loess smoothed)
#' # indicates best fit. Looks like x^4 has the best fit,
#' # however, only x^3 has significant p-values.
#' sjp.poly(fit, "e17age", 2:4, show.scatter = FALSE)
#'
#' \dontrun{
#' # fit new model
#' fit <- lm(tot_sc_e ~ c12hour + e42dep + e17age + I(e17age^2) + I(e17age^3),
#'           data = efc)
#' # plot marginal effects of polynomial term
#' plot_model(fit, type = "pred", terms = "e17age")}
#'
#' @import ggplot2
#' @importFrom scales grey_pal brewer_pal
#' @importFrom stats lm glm binomial predict poly
#' @importFrom graphics plot
#' @export
sjp.poly <- function(x,
                     poly.term,
                     poly.degree,
                     poly.scale = FALSE,
                     fun = NULL,
                     axis.title = NULL,
                     geom.colors = NULL,
                     geom.size = .8,
                     show.loess = TRUE,
                     show.loess.ci = TRUE,
                     show.p = TRUE,
                     show.scatter = TRUE,
                     point.alpha = .2,
                     point.color = "#404040",
                     loess.color = "#808080") {
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
  if (!is.vector(x) && !is.numeric(x) && !is.factor(x)) {
    mf <- insight::get_data(x, verbose = FALSE)
    # retrieve response vector
    resp <- insight::get_response(x)
    # retrieve polynomial term
    poly.term <- mf[[poly.term]]
  } else {
    resp <- x
  }

  # --------------------------------------------
  # check for glm or lm
  # --------------------------------------------
  if (is.null(fun)) {
    if (inherits(x, c("glmerMod", "glm"))) {
      fun <- "glm"
    } else {
      fun <- "lm"
    }
  }
  # --------------------------------------------
  # retrieve labels
  # --------------------------------------------
  if (is.null(axis.title)) axis.title <- sjlabelled::get_label(poly.term, def.value = defv)
  axisTitle.y <- sjlabelled::get_label(resp, def.value = "Response")
  # --------------------------------------------
  # init data frame
  # --------------------------------------------
  plot.df <- data.frame()
  # scale polynomial term?
  if (poly.scale) poly.term <- scale(poly.term)
  # --------------------------------------------
  # get cutpoints for loess curve
  # --------------------------------------------
  # cutpoints <- get_loess_cutpoints(stats::na.omit(data.frame(x = poly.term, y = resp)))
  # --------------------------------------------
  # if user wants to plot multiple curves for
  # polynomials, create data frame for each curve here
  # --------------------------------------------
  for (i in poly.degree) {
    # poly-function can't cope with missings, so remove them here
    mydat <- stats::na.omit(data.frame(x = poly.term, y = resp))
    # fit model with polynomials
    if (fun == "lm")
      fit <- stats::lm(mydat$y ~ stats::poly(mydat$x, i, raw = TRUE))
    else
      fit <- stats::glm(mydat$y ~ stats::poly(mydat$x, i, raw = TRUE), family = stats::family(x))
    # check whether we have an integer poly.degree
    # or a float value
    poly.digit <- ifelse(i %% 1 == 0, 0, 1)
    # create data frame with raw data and the fitted poly-curve
    plot.df <- rbind(plot.df, cbind(mydat,
                                    stats::predict(fit),
                                    sprintf("x^%.*f", poly.digit, i)))
    # print p-values?
    if (show.p) {
      # get p-values
      pvals <- summary(fit)$coefficients[-1, 4]
      # prepare output string
      p.out <- sprintf("Polynomial degrees: %.*f\n---------------------\n", poly.digit, i)
      # iterate polynomial terms and print p-value for each polynom
      for (j in seq_len(i)) p.out <- paste0(p.out, sprintf("p(x^%i): %.3f\n", j, unname(pvals[j])))
      # add separator line after each model
      p.out <- paste0(p.out, "\n")
      # print p-values for fitted model
      cat(p.out)
    }
  }
  # name df
  colnames(plot.df) <- c("x","y", "pred", "grp")
  # create plot
  polyplot <- ggplot(plot.df, aes_string(x = "x", y = "y", colour = "grp"))
  # show scatter plot as well?
  if (show.scatter) polyplot <- polyplot +
    geom_jitter(colour = point.color, alpha = point.alpha, shape = 16)
  # show loess curve? this curve indicates the "perfect" curve through
  # the data
  if (show.loess) polyplot <- polyplot + stat_smooth(method = "loess",
                                                    color = loess.color,
                                                    se = show.loess.ci,
                                                    size = geom.size)
  # add curves for polynomials
  polyplot <- polyplot +
    geom_line(aes_string(y = "pred"), linewidth = geom.size) +
    scale_color_manual(values = geom.colors, labels = lapply(poly.degree, function(j) bquote(x^.(j)))) +
    labs(x = axis.title, y = axisTitle.y, colour = "Polynomial\ndegrees")

  polyplot
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

  data.frame(cutpoint.x = xvals, cutpoint.y = cutpoints)
}
