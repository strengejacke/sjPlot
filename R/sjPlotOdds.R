# bind global variables
utils::globalVariables(c("OR", "lower", "upper", "p", "grp.est"))


#' @title Plot estimates, predictions or effects of generalized linear models
#' @name sjp.glm
#'
#' @seealso \href{http://www.strengejacke.de/sjPlot/sjp.glm/}{sjPlot manual: sjp.glm}
#'
#' @description Plot odds or incident rate ratios with confidence intervalls as dot plot.
#'                Depending on the \code{type} argument, this function may also plot model
#'                assumptions for generalized linear models, or marginal effects
#'                (predicted probabilities or events).
#'
#' @param fit fitted generalized linear model (\code{\link{glm}}- or \code{logistf}-object).
#' @param type type of plot. Use one of following:
#'          \describe{
#'            \item{\code{"dots"}}{(or \code{"glm"} or \code{"or"} (default)) for odds or incident rate ratios (forest plot). Note that this type plots the exponentiated estimates, thus being appropriate only for specific link-functions.}
#'            \item{\code{"slope"}}{to plot probability or incidents slopes (predicted probabilities or incidents) for each model term, where all remaining co-variates are set to zero (i.e. ignored). Use \code{facet.grid} to decide whether to plot each coefficient as separate plot or as integrated faceted plot.}
#'            \item{\code{"eff"}}{to plot marginal effects of predicted probabilities or incidents for each model term, where all remaining co-variates are set to the mean (see 'Details'). Use \code{facet.grid} to decide whether to plot each coefficient as separate plot or as integrated faceted plot.}
#'            \item{\code{"pred"}}{to plot predicted values for the response, related to specific model predictors. See 'Details'.}
#'            \item{\code{"ma"}}{to check model assumptions. Note that the only relevant argument for this option is \code{fit}. All other arguments are ignored.}
#'            \item{\code{"vif"}}{to plot Variance Inflation Factors.}
#'          }
#' @param trns.ticks logical, if \code{TRUE}, the grid lines have exponential 
#'          distances (equidistant), i.e. they visually have the same distance from 
#'          one panel grid to the next. If \code{FALSE}, grids are 
#'          plotted on every \code{grid.breaks}'s position, thus the grid lines become narrower with 
#'          higher odds ratio values.
#' @param show.intercept logical, if \code{TRUE}, the intercept of the fitted model is also plotted.
#'          Default is \code{FALSE}. For \code{glm}'s, please note that due to exponential 
#'          transformation of estimates, the intercept in some cases can not be calculated, thus the
#'          function call is interrupted and no plot printed.
#'          
#' @inheritParams sjp.lm
#' @inheritParams sjp.grpfrq
#' @inheritParams sjp.aov1
#' @inheritParams sjp.glmer
#' 
#' @return (Insisibily) returns, depending on the plot type
#'          \itemize{
#'            \item The ggplot-object (\code{plot}). For multiple plots and if \code{facet.grid = FALSE}) a \code{plot.list} is returned.
#'            \item A data frame \code{data} with the data used to build the ggplot-object(s), or a list of data frames (\code{data.list}).
#'            }
#'
#' @details \describe{
#'            \item{\code{type = "slope"}}{the predicted values
#'            are based on the intercept's estimate and each specific term's estimate.
#'            All other co-variates are set to zero (i.e. ignored), which corresponds
#'            to \code{family(fit)$linkinv(eta = b0 + bi * xi)} (where \code{xi} is the estimate).
#'            This plot type can be seen as equivalent to \code{type = "slope"} for \code{\link{sjp.lm}},
#'            just for glm objects. This plot type may give similar results as 
#'            \code{type = "pred"}, however, \code{type = "slope"} does not adjust
#'            for other predictors.}
#'            \item{\code{type = "eff"}}{computes marginal effects of all higher order
#'            terms in the model. The predicted values computed by \code{type = "eff"}
#'            are adjusted for all other co-variates, by setting them to the mean
#'            (as returned by the \code{\link[effects]{allEffects}} function).
#'            You can pass further arguments down to \code{allEffects} for flexible
#'            function call via the \code{...}-argument.}
#'            \item{\code{type = "pred"}}{the predicted values
#'            of the response are computed, based on the \code{\link{predict.glm}}
#'            method. Corresponds to \code{\link{predict}(fit, type = "response")}.
#'            This plot type requires the \code{vars} argument to select specific terms
#'            that should be used for the x-axis and - optional - as grouping factor. 
#'            Hence, \code{vars} must be a character vector with the names of
#'            one or two model predictors. See 'Examples'.}
#'          }
#'
#' @examples
#' # prepare dichotomous dependent variable
#' swiss$y <- ifelse(swiss$Fertility < median(swiss$Fertility), 0, 1)
#'
#' # fit model
#' fitOR <- glm(y ~ Education + Examination + Infant.Mortality + Catholic,
#'              family = binomial(link = "logit"), data = swiss)
#'
#' # print Odds Ratios as dots
#' sjp.glm(fitOR)
#'
#' # -------------------------------
#' # Predictors for negative impact of care. Data from 
#' # the EUROFAMCARE sample dataset
#' # -------------------------------
#' library(sjmisc)
#' data(efc)
#' # create binary response
#' y <- ifelse(efc$neg_c_7 < median(na.omit(efc$neg_c_7)), 0, 1)
#' # create data frame for fitted model
#' mydf <- data.frame(y = as.factor(y),
#'                    sex = efc$c161sex,
#'                    dep = to_factor(efc$e42dep),
#'                    barthel = efc$barthtot,
#'                    education = to_factor(efc$c172code))
#' # fit model
#' fit <- glm(y ~., data = mydf, family = binomial(link = "logit"))
#' 
#' # plot odds ratios
#' sjp.glm(fit, title = get_label(efc$neg_c_7))
#'
#' # plot probability curves (relationship between predictors and response)
#' sjp.glm(fit, title = get_label(efc$neg_c_7), type = "slope")
#'
#' # --------------------------
#' # grouping estimates
#' # --------------------------
#' sjp.glm(fit,  group.estimates = c(1, 2, 2, 2, 3, 4, 4))
#'
#' # --------------------------
#' # model predictions, with selected model terms.
#' # 'vars' needs to be a character vector of length 1 or 2
#' # with names of model terms for x-axis and grouping factor.
#' # --------------------------
#' sjp.glm(fit, type = "pred", vars = "barthel")
#' # faceted, with ci
#' sjp.glm(fit, type = "pred", vars = c("barthel", "dep"), show.ci = TRUE)
#' # w/o facets
#' sjp.glm(fit, type = "pred", vars = c("barthel", "dep"), facet.grid = FALSE)
#' 
#' @import ggplot2
#' @importFrom stats na.omit coef confint logLik
#' @export
sjp.glm <- function(fit,
                    type = "dots",
                    vars = NULL,
                    group.estimates = NULL,
                    remove.estimates = NULL,
                    sort.est = TRUE,
                    title = NULL,
                    legend.title = NULL,
                    axis.labels = NULL,
                    axis.title = "Odds Ratios",
                    geom.size = NULL,
                    geom.colors = "Set1",
                    wrap.title = 50,
                    wrap.labels = 25,
                    axis.lim = NULL,
                    grid.breaks = 0.5,
                    trns.ticks = TRUE,
                    show.intercept = FALSE,
                    show.values = TRUE,
                    show.p = TRUE,
                    show.ci = FALSE,
                    show.legend = FALSE,
                    show.summary = FALSE,
                    digits = 2,
                    vline.type = 2,
                    vline.color = "grey70",
                    coord.flip = TRUE,
                    y.offset = .15,
                    facet.grid = TRUE,
                    prnt.plot = TRUE,
                    ...) {
  # check args -----
  if (type == "pc" || type == "prob") type <- "slope"

  if (any(class(fit) == "logistf")) {
    # no model summary currently supported for logistf class
    show.summary <- FALSE
    # create "dummy" variable, to avoid errors
    fit$model <- fit$data
    # no probability curves currently supported
    if (type == "slope") {
      warning("Predicted probability plots currently not supported for `logistf` objects.", call. = F)
      type <- "dots"
    }
  }
  
  # set default title
  if (is.null(title) && (type != "eff" && type != "slope" && type != "pred")) title <- get_model_response_label(fit)
  
  # check plot-type -----
  if (type == "slope") {
    return(invisible(sjp.glm.slope(fit, title, geom.size, geom.colors, remove.estimates, vars,
                                   ylim = axis.lim, show.ci, facet.grid, prnt.plot)))
  }
  if (type == "eff") {
    return(invisible(sjp.glm.eff(fit, title, geom.size, remove.estimates, vars,
                                 show.ci, ylim = axis.lim, facet.grid, fun = "glm", 
                                 prnt.plot, ...)))
  }
  if (type == "pred") {
    return(invisible(sjp.glm.predy(fit, vars, t.title = title, l.title = legend.title,
                                   a.title = axis.title,
                                   geom.colors, show.ci, geom.size, ylim = axis.lim,
                                   facet.grid, type = "fe", show.loess = F, prnt.plot)))
  }
  if (type == "ma") {
    return(invisible(sjp.glm.ma(fit)))
  }
  if (type == "vif") {
    return(invisible(sjp.vif(fit)))
  }
  
  # check type param -----
  if (type == "or" || type == "glm") type <- "dots"
  if (type != "dots") {
    warning("Invalid `type` argument. Defaulting to `dots`.", call. = F)
    type <- "dots"
  }
  
  # check size param
  if (is.null(geom.size)) geom.size <- 3
  
  # auto-retrieve value labels
  if (is.null(axis.labels)) {
    axis.labels <- suppressWarnings(retrieveModelLabels(list(fit), group.pred = FALSE))
  }
  
  # check model family, do we have count model?
  fitfam <- get_glm_family(fit)
  
  # create logical for family
  poisson_fam <- fitfam$is_pois
  binom_fam <- fitfam$is_bin
  logit_link <- fitfam$is_logit
  
  # Prepare length of title and labels -----
  
  # check default label and fit family
  if (!is.null(axis.title) && axis.title == "Odds Ratios") {
    if (poisson_fam)
      axis.title <- "Incident Rate Ratios"
    else if (binom_fam && !logit_link)
      axis.title <- "Risk Ratios"
  }
  # check length of diagram title and split longer string at into new lines
  if (!is.null(title)) title <- sjmisc::word_wrap(title, wrap.title)
  # check length of x-axis title and split longer string at into new lines
  # every 50 chars
  if (!is.null(axis.title)) axis.title <- sjmisc::word_wrap(axis.title, wrap.title)
  # check length of x-axis-labels and split longer strings at into new lines
  if (!is.null(axis.labels)) axis.labels <- sjmisc::word_wrap(axis.labels, wrap.labels)
  
  # get model coefficients -----
  model_coef <- exp(stats::coef(fit))
  # create data frame for ggplot
  tmp <- data.frame(cbind(model_coef, exp(stats::confint(fit))))
  
  # print p-values in bar charts
  
  # retrieve sigificance level of independent variables (p-values)
  if (any(class(fit) == "logistf")) {
    pv <- fit$prob
  } else {
    pv <- stats::coef(summary(fit))[,4]
  }
  # retrieve odds ratios
  ov <- model_coef
  
  # copy OR-values into data column
  ps <- rep("", length(ov))
  if (show.values) ps <- sprintf("%.*f", digits, ov)
  
  # copy p-values into data column
  if (show.p) {
    for (i in seq_len(length(pv))) {
      # for better readability, convert p-values to asterisks
      # with:
      # p < 0.001 = ***
      # p < 0.01 = **
      # p < 0.05 = *
      ps[i] <- sjmisc::trim(paste(ps[i], get_p_stars(pv[i])))
    }
  }
  
  # remove intercept
  odds <- tmp[-1, ]
  
  # retrieve odds ratios, without intercept. now we can order
  # the predictors according to their OR value, while the intercept
  # is always shown on top
  ov <- model_coef[-1]
  
  # check if user defined labels have been supplied
  # if not, use variable names from data frame
  
  # auto-retrieving variable labels does not work when we
  # have factors with different levels, which appear as
  # "multiple predictors", but are only one variable
  if (is.null(axis.labels) || length(axis.labels) < length(row.names(odds))) {
    axis.labels <- row.names(odds)
  }
  
  # bind p-values to data frame
  odds <- cbind(odds, ps[-1], pv[-1])
  # we repeat the whole procedure for our
  # tmp-data frame as well, since this data frame
  # contains the intercepts. We than later just copy the
  # intercept row to our odds-data frame, if needed. The intercept
  # is not included from the beginning, because when sorting the OR values,
  # the intercept should not be sorted, but alway placed on top
  tmp <- cbind(tmp, ps, pv)
  # set column names
  names(odds) <- c("OR", "lower", "upper", "p", "pvalue")
  names(tmp) <- c("OR", "lower", "upper", "p", "pvalue")
  # init grouping variable
  odds$grp.est <- NA
  tmp$grp.est <- NA
  
  # group estimates? -----
  if (!is.null(group.estimates)) {
    # check for correct length
    if (length(group.estimates) != nrow(odds)) {
      warning("Length of `group.estimates` does not equal number of model coefficients. Ignoring this argument.", call. = F)
      group.estimates = NULL
      show.legend <- FALSE
      legend.title <- NULL
    } else {
      odds$grp.est <- as.character(group.estimates)
    }
  } else {
    show.legend <- FALSE
    legend.title <- NULL
  }
  
  # remove any estimates from the output? -----
  if (!is.null(remove.estimates)) {
    # get row indices of rows that should be removed
    remrows <- match(remove.estimates, row.names(odds))
    # remember old rownames
    keepnames <- row.names(odds)[-remrows]
    # remove rows
    odds <- dplyr::slice(odds, c(1:nrow(odds))[-remrows])
    # set back rownames
    row.names(odds) <- keepnames
    # remove labels?
    if (!is.null(axis.labels) && length(axis.labels) > nrow(odds))
      axis.labels <- axis.labels[-remrows]
    # remove p-values
    ov <- ov[-remrows]
  }
  
  # Calculate axis limits. The range is from lowest lower-CI
  # to highest upper-CI, or a user defined range
  if (is.null(axis.lim)) {
    # if intercept is shown, we have to adjuste the axis limits to max/min
    # values of odds ratios AND intercept
    if (show.intercept) {
      rdf <- tmp
    } else {
      # else, we have to adjuste the axis limits to max/min
      # values just of odds ratios
      rdf <- odds
    }
    # the range corresponds to the boundaries given by
    # the CI's
    maxval <- max(rdf$upper)
    minval <- min(rdf$lower)
    upper_lim <- ceiling(10 * maxval) / 10
    lower_lim <- floor(10 * minval) / 10
    # avoid zero or NA axis limit!
    if (is.na(upper_lim)) upper_lim <- ceiling(10 * max(stats::na.omit(maxval))) / 10
    if (lower_lim == 0 || is.na(lower_lim)) lower_lim <- 0.01
    # give warnings when auto-limits are very low/high
    if ((minval < 0.1) || (maxval > 100)) {
      warning("Exp. coefficients and/or exp. confidence intervals may be out of printable bounds. Consider using `axis.lim` argument!")
    }
  } else {
    # Here we have user defind axis range
    lower_lim <- axis.lim[1]
    upper_lim <- axis.lim[2]
  }
  
  # Define axis ticks
  ticks <- seq(lower_lim, upper_lim, by = grid.breaks)
  
  # create expression with model summarys -----
  # used for plotting in the diagram later
  if (show.summary) {
    psr <- sjstats::r2(fit)
    modsum <- as.character(as.expression(
      substitute("(Intercept)" == ic * "," ~~ italic(R)[CS]^2 == r2cs * "," ~~ italic(R)[N]^2 == r2n * "," ~~ -2 * lambda == la * "," ~~ chi^2 == c2 * "," ~~ "AIC" == aic,
                 list(ic = sprintf("%.2f", exp(stats::coef(fit)[1])),
                      r2cs = sprintf("%.3f", psr$CoxSnell),
                      r2n = sprintf("%.3f", psr$Nagelkerke),
                      la = sprintf("%.2f", -2 * stats::logLik(fit)),
                      c2 = sprintf("%.2f", Chisquare.glm(fit)),
                      aic = sprintf("%.2f", fit$aic)))))
    cat(sprintf("Intercept = %.2f\nR2[cs] = %.3f\nR2[n] = %.3f\nLambda = %.2f\nChi2 = %.2f\nAIC = %.2f\n",
                exp(stats::coef(fit)[1]),
                psr$CoxSnell,
                psr$Nagelkerke,
                -2 * stats::logLik(fit),
                Chisquare.glm(fit),
                fit$aic))
  } else {
    modsum <- NULL
  }
  
  # Sort odds and labels according to b-coefficients
  if (sort.est) {
    if (!is.null(group.estimates)) {
      axis.labels <- rev(axis.labels[order(odds$grp.est, odds$OR)])
      odds <- odds[rev(order(odds$grp.est, odds$OR)), ]
    } else {
      axis.labels <- axis.labels[order(odds$OR)]
      odds <- odds[order(odds$OR), ]
    }
  }

  # check whether intercept should be shown
  if (show.intercept) {
    odds <- data.frame(rbind(tmp[1, ], odds))
    axis.labels <- c("Intercept", axis.labels)
  }
  odds$vars <- as.factor(seq_len(nrow(odds)))
  
  # Start plot here! First check how to colour geoms
  # (whether grouped or not)
  if (!is.null(group.estimates)) {
    plotHeader <- ggplot(odds, aes_string(y = "OR", x = "vars", colour = "grp.est"))
    pal.len <- length(unique(group.estimates))
    legend.labels <- unique(odds$grp.est)
  } else {
    plotHeader <- ggplot(odds, aes(y = OR, x = vars, colour = (OR > 1)))
    pal.len <- 2
    legend.labels <- NULL
  }
  
  # start with dot-plotting here -----
  plotHeader <- plotHeader +
    # Order odds according to beta-coefficients, colour points and lines according to
    # OR-value greater / lower than 1
    geom_point(size = geom.size) +
    # print confidence intervalls (error bars)
    geom_errorbar(aes_string(ymin = "lower", ymax = "upper"), width = 0) +
    # print value labels and p-values
    geom_text(aes_string(label = "p", y = "OR"), nudge_x = y.offset)
  
  # add annotations with model summary
  # here we print out the log-lik-ratio "lambda" and the chi-square significance of the model
  # compared to the null-model
  plotHeader <- print.table.summary(plotHeader, modsum)
  plotHeader <- plotHeader +
    # Intercept-line
    geom_hline(yintercept = 1, linetype = vline.type, color = vline.color) +
    labs(title = title, x = NULL, y = axis.title, colour = legend.title) +
    scale_x_discrete(labels = axis.labels)
  
  # create pretty breaks for log-scale -----
  if (trns.ticks) {
    # since the odds are plotted on a log-scale, the grid bars'
    # distance shrinks with higher odds values. to provide a visual
    # proportional distance of the grid bars, we can apply the
    # exponential-function on the tick marks
    plotHeader <- plotHeader +
      scale_y_continuous(trans = "log10",
                         limits = c(lower_lim, upper_lim),
                         breaks = base_breaks(upper_lim),
                         labels = prettyNum)
  } else {
    plotHeader <- plotHeader +
      # logarithmic scale for odds
      scale_y_log10(limits = c(lower_lim, upper_lim),
                    breaks = ticks,
                    labels = ticks)
  }
  
  # flip coordinates?
  if (coord.flip) plotHeader <- plotHeader + coord_flip()
  
  # set geom colors -----
  plotHeader <- sj.setGeomColors(plotHeader, geom.colors, pal.len, show.legend, legend.labels)
  
  # Check whether ggplot object should be returned or plotted
  if (prnt.plot) graphics::plot(plotHeader)
  
  # set proper column names
  odds <- tibble::rownames_to_column(odds)
  colnames(odds) <- c("term", "estimate", "conf.low", "conf.high", 
                      "p.string", "p.value", "xpos")
  
  # return results -----
  invisible(structure(class = c("sjPlot", "sjpglm"),
                      list(plot = plotHeader,
                           data = odds)))
}


#' @importFrom stats predict coef formula model.frame
sjp.glm.slope <- function(fit, title, geom.size, geom.colors, remove.estimates, vars,
                          ylim, show.ci, facet.grid, prnt.plot) {
  # check size argument
  if (is.null(geom.size)) geom.size <- .7
  # check geom-color argument
  geom.colors <- col_check2(geom.colors, 1)
  # ----------------------------
  # do we have mermod object?
  # ----------------------------
  isMerMod <- any(class(fit) == "glmerMod")
  # ----------------------------
  # prepare additional plots, when metric
  # predictors should also be plotted
  # ----------------------------
  # init lists with all additional data frames and plots
  mydf.metricpred <- list()
  plot.metricpred <- list()
  axisLabels.mp <- c()
  plot.facet <- NULL
  mydf.facet <- NULL
  # ----------------------------
  # retrieve data frame of model to check whether
  # we have any numeric terms in fitted model
  # and get model family, for link-inverse function
  # ----------------------------
  fit.df <- stats::model.frame(fit)
  fitfam <- stats::family(fit)
  faminfo <- get_glm_family(fit)
  # --------------------------------------------------------
  # create logical for family
  # --------------------------------------------------------
  poisson_fam <- faminfo$is_pois
  binom_fam <- faminfo$is_bin
  # ----------------------------
  # retrieve term names, so we find the estimates in the
  # coefficients list
  # ----------------------------
  if (isMerMod) {
    fit.term.length <- length(names(lme4::fixef(fit))[-1])
    b0 <- unname(lme4::fixef(fit))[1]
    estimates <- lme4::fixef(fit)
    coef.names <- names(lme4::fixef(fit))
  } else {
    fit.term.length <- length(coef(fit)) - 1
    b0 <- unname(stats::coef(fit)[1])
    estimates <- stats::coef(fit)
    coef.names <- names(stats::coef(fit))
  }
  # retrieve term names, so we find the estimates in the
  # coefficients list
  fit.term.names <- all.vars(stats::formula(fit))[-1][seq_len(fit.term.length)]
  # ------------------------
  # remove estimates?
  # ------------------------
  if (!is.null(remove.estimates)) {
    remcols <- match(remove.estimates, fit.term.names)
    # remember old rownames
    if (!sjmisc::is_empty(remcols))
      fit.term.names <- fit.term.names[-remcols]
  }
  # ------------------------
  # select specific setimates?
  # ------------------------
  if (!is.null(vars)) {
    remcols <- match(vars, fit.term.names)
    # remember old rownames
    if (!sjmisc::is_empty(remcols))
      fit.term.names <- fit.term.names[remcols]
  }
  # ------------------------
  # Retrieve response for automatic title
  # ------------------------
  resp <- fit.df[[1]]
  resp.col <- sjstats::resp_var(fit)
  # ----------------------------
  # loop through all coefficients
  # ----------------------------
  for (i in seq_len(length(fit.term.names))) {
    # get values from coefficient
    coef.column <- which(colnames(fit.df) == fit.term.names[i])
    # check if we have found the coefficient
    if (length(coef.column) > 0) {
      # get values from numeric term
      values <- fit.df[, coef.column]
      # find unique values, for x axis
      vals.unique <- sort(values)
      # melt variable
      mydf.vals <- data.frame(values = vals.unique)
      # convert factor to numeric
      if (is.factor(mydf.vals$values)) mydf.vals$values <- sjmisc::to_value(mydf.vals$values, start.at = 0, keep.labels = F)
      # check if we have a factor, then we may have reference levels
      if (is.factor(values)) {
        # add reference level to coefficient name
        ll <- levels(values)
        fit.fac.name <- paste0(fit.term.names[i], ll[length(ll)])
      } else {
        fit.fac.name <- fit.term.names[i]
      }
      # find coef-position
      coef.pos <- which(coef.names == fit.fac.name)
      # ---------------------------------------------
      # Here we go with predicted values
      # for each single term, w/o including remaining
      # co-variates to predict the values. This can be
      # used to investigate a term "isolated"
      # ---------------------------------------------
      if (length(coef.pos) > 0) {
        # calculate x-beta by multiplying original values with estimate of that term
        mydf.vals$xbeta <- mydf.vals$value * estimates[coef.pos]
        # calculate probability (y) via cdf-function
        mydf.vals$y <- fitfam$linkinv(eta = b0 + mydf.vals$xbeta)
        # save predictor name
        pred.name <- fit.term.names[i]
        axisLabels.mp <- c(axisLabels.mp, pred.name)
        # assign group
        mydf.vals$grp <- pred.name
        # add mydf to list
        mydf.metricpred[[length(mydf.metricpred) + 1]] <- mydf.vals
      }
    }
  }
  # ---------------------------------------------------------
  # axis title, depending on model family
  # ---------------------------------------------------------
  if (binom_fam) {
    y.title <- "Predicted probabilities"
    title.fix <- "probabilities"
  } else if (poisson_fam) {
    y.title <- "Predicted incidents"
    title.fix <- "incidents"
  } else {
    y.title <- sjmisc::get_label(resp, def.value = resp.col)
    title.fix <- "effects"
  }
  # ---------------------------------------------------------
  # default plot title
  # ---------------------------------------------------------
  if (is.null(title)) title <- sprintf("Predicted %s for %s", 
                                       title.fix,
                                       get_model_response_label(fit))
  # ---------------------------------------------------------
  # Prepare metric plots
  # ---------------------------------------------------------
  if (length(mydf.metricpred) > 0) {
    # create mydf for integrated plot
    mydf.ges <- data.frame()
    for (i in seq_len(length(mydf.metricpred))) {
      # "melt" all single mydf's to one
      mydf.ges <- rbind(mydf.ges, mydf.metricpred[[i]])
      # ------------------------------
      # check axis limits
      # ------------------------------
      if (is.null(ylim)) {
        y.limits <- c(as.integer(floor(10 * min(mydf.metricpred[[i]]$y, na.rm = T) * .9)) / 10,
                      as.integer(ceiling(10 * max(mydf.metricpred[[i]]$y, na.rm = T) * 1.1)) / 10)
      } else {
        y.limits <- ylim
      }
      # create single plots for each numeric predictor
      mp <- ggplot(mydf.metricpred[[i]], aes_string(x = "values", y = "y")) +
        labs(x = axisLabels.mp[i], y = y.title, title = title)
      # special handling for negativ binomial
      if (sjmisc::str_contains(fitfam$family, "negative binomial", ignore.case = T)) {
        mp <- mp +
          stat_smooth(method = "glm", 
                      method.args = list(family = "poisson"), 
                      se = show.ci,
                      size = geom.size,
                      colour = geom.colors)
      } else {
        mp <- mp +
          stat_smooth(method = "glm", 
                      method.args = list(family = fitfam$family), 
                      se = show.ci,
                      size = geom.size,
                      colour = geom.colors)
      }
      # y-limits
      mp <- mp + coord_cartesian(ylim = y.limits)
      # add plot to list
      plot.metricpred[[length(plot.metricpred) + 1]] <- mp
    }
    # if we have more than one numeric var, also create integrated plot
    if (length(mydf.metricpred) > 1) {
      # ------------------------------
      # check axis limits
      # ------------------------------
      if (is.null(ylim)) {
        y.limits <- c(as.integer(floor(10 * min(mydf.ges$y, na.rm = T) * .9)) / 10,
                      as.integer(ceiling(10 * max(mydf.ges$y, na.rm = T) * 1.1)) / 10)
      } else {
        y.limits <- ylim
      }
      mp <- ggplot(mydf.ges, aes_string(x = "values", y = "y")) +
        labs(x = NULL, y = y.title, title = title)
      # special handling for negativ binomial
      if (sjmisc::str_contains(fitfam$family, "negative binomial", ignore.case = T)) {
        mp <- mp +
          stat_smooth(method = "glm", 
                      method.args = list(family = "poisson"), 
                      se = show.ci,
                      size = geom.size,
                      colour = geom.colors)
      } else {
        mp <- mp +
          stat_smooth(method = "glm", 
                      method.args = list(family = fitfam$family), 
                      se = show.ci,
                      size = geom.size,
                      colour = geom.colors)
      }
      mp <- mp +
        facet_wrap(~grp,
                   ncol = round(sqrt(length(mydf.metricpred))),
                   scales = "free_x") +
        guides(colour = FALSE)
      # y-limits
      mp <- mp + coord_cartesian(ylim = y.limits)
      # add integrated plot to plot list
      plot.facet <- mp
      # add integrated data frame to plot list
      mydf.facet <- mydf.ges
    }
  }
  # --------------------------
  # plot plots
  # --------------------------
  if (prnt.plot) {
    if (facet.grid && !is.null(plot.facet)) {
      suppressWarnings(graphics::plot(plot.facet))
    } else {
      for (i in seq_len(length(plot.metricpred))) {
        suppressWarnings(graphics::plot(plot.metricpred[[i]]))
      }
    }
  }

  invisible(structure(class = c("sjPlot", "sjpglm.pc"),
                      list(data.list = mydf.metricpred,
                           plot.list = plot.metricpred,
                           data = mydf.facet,
                           plot = plot.facet)))
}


#' @importFrom stats model.frame predict predict.glm family
#' @importFrom dplyr select
sjp.glm.predy <- function(fit,
                          vars,
                          t.title,
                          l.title,
                          a.title,
                          geom.colors,
                          show.ci,
                          geom.size,
                          ylim,
                          facet.grid,
                          type = "fe",
                          show.loess = FALSE,
                          prnt.plot) {
  # -----------------------------------------------------------
  # check class of fitted model
  # -----------------------------------------------------------
  c.f <- class(fit)
  fun <- "glm"
  fit.m <- "glm"
  if (any(c.f == "glm")) {
    fun <- "glm"
    fit.m <- "glm"
  } else if (any(c.f == "lm")) {
    fun <- "lm"
    fit.m <- "lm"
  } else if (any(c.f == "glmerMod")) {
    fun <- "glmer"
    fit.m <- "glm"
  } else if (any(c.f == "nlmerMod")) {
    fun <- "nlmer"
    fit.m <- "nlm"
  } else if (any(c.f == "lmerMod") || any(c.f == "merModLmerTest")) {
    fun <- "lmer"
    fit.m <- "lm"
  }
  # ----------------------------
  # check size argument
  # ----------------------------
  if (is.null(geom.size)) geom.size <- .7
  # ----------------------------
  # check vars argument
  # ----------------------------
  if (is.null(vars)) {
    warning("`vars` needs to be a character vector with one or two predictor names: one term used for the x-axis, another optional term as grouping factor.", call. = F)
    return(NULL)
  }
  # ----------------------------
  # get predicted values for response
  # ----------------------------
  fitfram <- stats::model.frame(fit)
  if (fun == "glm") {
    fitfram$predicted.values <- stats::predict.glm(fit, newdata = fitfram, type = "response")
  } else if (fun %in% c("glmer", "lmer", "nlmer")) {
    if (type == "fe")
      fitfram$predicted.values <- stats::predict(fit, newdata = fitfram, type = "response", re.form = NA)
    else
      fitfram$predicted.values <- stats::predict(fit, newdata = fitfram, type = "response", re.form = NULL)
  } else {
    fitfram$predicted.values <- stats::predict(fit, newdata = fitfram, type = "response")
  }
  # ----------------------------
  # check model family, do we have count model?
  # ----------------------------
  faminfo <- get_glm_family(fit)
  # only for glm
  if (fit.m == "lm") {
    fitfam <- ""
  } else {
    fitfam <- stats::family(fit)
  }
  # --------------------------------------------------------
  # create logical for family
  # --------------------------------------------------------
  binom_fam <- faminfo$is_bin
  poisson_fam <- faminfo$is_pois
  # ----------------------------
  # check default titles
  # ----------------------------
  if (is.null(t.title)) {
    if (poisson_fam)
      t.title <- "Predicted incident rates"
    else if (binom_fam)
      t.title <- "Predicted probabilties"
    else
      t.title <- "Predicted values"
  }
  # axis titles
  if (is.null(a.title) || length(a.title) != 2) {
    x.title <- sjmisc::get_label(fitfram[[vars[1]]], def.value = vars[1])
    y.title <- sjmisc::get_label(fitfram[[1]], def.value = colnames(fitfram)[1])
  } else {
    x.title <- a.title[1]
    y.title <- a.title[2]
  }
  # legend title
  if (is.null(l.title))
    l.title <- sjmisc::get_label(fitfram[[vars[2]]], def.value = vars[2])
  # ----------------------------
  # check for correct length of vector
  # ----------------------------
  if (length(vars) > 2) {
    message("`vars` must have not more than two values. Using first two values now.")
    vars <- vars[1:2]
  } 
  # ----------------------------
  # check for correct vars specification
  # ----------------------------
  if (!all(vars %in% colnames(fitfram))) {
    stop("At least one term specified in `vars` is no valid model term.", call. = F)
  }
  mydf <- dplyr::select(fitfram, match(c(vars, "predicted.values"), colnames(fitfram)))
  # init legend labels
  legend.labels <- NULL
  # check if we have a categorical variable with value
  # labels at the x-axis.
  axis_labels <- sjmisc::get_labels(mydf[[1]])
  # ----------------------------
  # with or w/o grouping factor?
  # ----------------------------
  if (length(vars) == 1) {
    colnames(mydf) <- c("x", "y")
    # x needs to be numeric
    mydf$x <- sjmisc::to_value(mydf$x)
    # convert to factor for proper legend
    mydf$grp <- sjmisc::to_factor(1)
    # set colors
    geom.colors <- col_check2(geom.colors, 1)
    # init plot
    mp <- ggplot(mydf, aes_string(x = "x", y = "y", colour = "grp")) +
      labs(x = x.title, y = y.title, title = t.title, colour = NULL)
  } else {
    colnames(mydf) <- c("x", "grp", "y")
    # x needs to be numeric
    mydf$x <- sjmisc::to_value(mydf$x)
    # convert to factor for proper legend
    mydf$grp <- sjmisc::to_factor(mydf$grp)
    # check if we have legend labels
    legend.labels <- sjmisc::get_labels(mydf$grp)
    # set colors
    geom.colors <- col_check2(geom.colors, length(legend.labels))
    # init plot
    mp <- ggplot(mydf, aes_string(x = "x", y = "y", colour = "grp", group = "grp")) +
      labs(x = x.title, y = y.title, title = t.title, colour = l.title)
  }
  # ------------------------------
  # check axis limits
  # ------------------------------
  if (is.null(ylim)) {
    ylim <- c(as.integer(floor(10 * min(mydf$y, na.rm = T) * .9)) / 10,
              as.integer(ceiling(10 * max(mydf$y, na.rm = T) * 1.1)) / 10)
  }
  # ---------------------------------------------------------
  # Prepare plot
  # ---------------------------------------------------------
  if (!is.null(axis_labels)) {
    # if we have value labels, use these as axis labels
    mp <- mp + scale_x_continuous(breaks = sort(unique(mydf$x)), labels = axis_labels)
  }
  if (fit.m == "lm") {
    mp <- mp +
      stat_smooth(method = fit.m, 
                  se = show.ci,
                  size = geom.size)
  } else {
    # special handling for negativ binomial
    if (sjmisc::str_contains(fitfam$family, "negative binomial", ignore.case = T)) {
      mp <- mp +
        stat_smooth(method = "glm.nb",
                    se = show.ci,
                    size = geom.size)
    } else {
      mp <- mp +
        stat_smooth(method = fit.m, 
                    method.args = list(family = fitfam$family), 
                    se = show.ci,
                    size = geom.size)
    }
  }
  # ---------------------------------------------------------
  # Add Loess-Line
  # ---------------------------------------------------------
  if (show.loess) mp <- mp + stat_smooth(method = "loess",
                                         se = F,
                                         size = geom.size,
                                         colour = "darkred")
  # ---------------------------------------------------------
  # coord-system for y-axis limits
  # cartesian coord still plots range of se, even
  # when se exceeds plot range.
  # ---------------------------------------------------------
  # add percentage labels for binomial family
  if (binom_fam) {
    mp <- mp + 
      scale_y_continuous(labels = scales::percent) +
      coord_cartesian(ylim = ylim)
  } else {
    mp <- mp + ylim(ylim)    
  }
  # ---------------------------------------------------------
  # facet grid, if we have grouping variable
  # ---------------------------------------------------------
  if (facet.grid && length(vars) == 2) {
    mp <- mp + 
      facet_wrap(~grp, ncol = round(sqrt(length(unique(mydf$grp)))), 
                 scales = "free_x") +
      scale_colour_manual(values = geom.colors) +
      guides(colour = FALSE)
  } else if (!is.null(legend.labels)) {
    if (length(legend.labels) == 1) {
      mp <- mp +
        scale_colour_manual(values = geom.colors) +
        guides(colour = FALSE)
    } else {
      mp <- mp +
        scale_colour_manual(values = geom.colors, labels = legend.labels)
    }
  } else {
    mp <- mp +
      scale_colour_manual(values = geom.colors) +
      guides(colour = FALSE)
  }
  
  # --------------------------
  # plot plots
  # --------------------------
  if (prnt.plot) suppressWarnings(graphics::plot(mp))
  return(structure(class = c("sjPlot", "sjpglm.ppresp"),
                   list(data = mydf, plot = mp)))
}


#' @importFrom stats update qqnorm qqline residuals anova
#' @importFrom graphics points text abline plot
sjp.glm.ma <- function(logreg) {
  # -----------------------------------
  # check package availability
  # -----------------------------------
  if (!requireNamespace("car", quietly = TRUE)) {
    stop("Package `car` needed for this function to work. Please install it.", call. = F)
  }
  # ---------------------------------
  # remove outliers
  # ---------------------------------
  # copy current model
  model <- logreg
  # get AIC-Value
  aic <- logreg$aic
  # maximum loops
  maxloops <- 5
  maxcnt <- maxloops
  # remember how many cases have been removed
  removedcases <- 0
  outlier <- c()
  loop <- TRUE
  # start loop
  while (isTRUE(loop)) {
    # get outliers of model
    # ol <- car::outlierTest(model)
    # retrieve variable numbers of outliers
    # vars <- as.numeric(attr(ol$p, "names"))
    vars <- as.numeric(names(which(car::outlierTest(model, cutoff = Inf, n.max = Inf)$bonf.p < 1)))    # update model by removing outliers
    if (sjmisc::is_empty(vars)) {
      loop <- FALSE
    } else {
      dummymodel <- stats::update(model, subset = -c(vars))
      # retrieve new AIC-value
      dummyaic <- dummymodel$aic
      # decrease maximum loops
      maxcnt <- maxcnt - 1
      # check whether AIC-value of updated model is larger
      # than previous AIC-value or if we have already all loop-steps done,
      # stop loop
      if (dummyaic >= aic || maxcnt < 1) {
        loop <- FALSE
      } else {
        # else copy new model, which is the better one (according to AIC-value)
        model <- dummymodel
        # and get new AIC-value
        aic <- dummyaic
        # count removed cases
        removedcases <- removedcases + length(vars)
        # add outliers to final return value
        outlier <- c(outlier, vars)
      }
    }
  }
  # ---------------------------------
  # print steps from original to updated model
  # ---------------------------------
  message(sprintf(("Removed %i cases during %i step(s).\nAIC-value of original model: %.2f\nAIC-value of updated model:  %.2f\n"),
                  removedcases,
                  maxloops - (maxcnt + 1),
                  logreg$aic,
                  model$aic))
  # ------------------------------------------------------
  # Overdispersion
  # Sometimes we can get a deviance that is much larger than expected
  # if the model was correct. It can be due to the presence of outliers,
  # sparse data or clustering of data. A half-normal plot of the residuals
  # can help checking for outliers:
  # ------------------------------------------------------
  halfnorm <- function(x, nlab = 2, labs = as.character(1:length(x)), ylab = "Sorted Data", ...) {
    x <- abs(x)
    labord <- order(x)
    x <- sort(x)
    i <- order(x)
    n <- length(x)
    ui <- qnorm((n + 1:n) / (2 * n + 1))
    graphics::plot(ui, 
         x[i], 
         xlab = "Half-normal quantiles", 
         ylab = ylab, 
         ylim = c(0, max(x)), 
         type = "n",
         ...)
    if (nlab < n) graphics::points(ui[1:(n - nlab)], x[i][1:(n - nlab)])
    graphics::text(ui[(n - nlab + 1):n], 
                   x[i][(n - nlab + 1):n], 
                   labs[labord][(n - nlab + 1):n])
  }
  # show half-normal quantiles for original model
  halfnorm(stats::residuals(logreg), main = "Over-/underdispersion")
  # ------------------------------------------------------
  # Influential and leverage points
  # ------------------------------------------------------
  car::influencePlot(logreg)
  # ------------------------------------------------------
  # Residual plot
  # ------------------------------------------------------
  res <- stats::residuals(logreg, type = "deviance")
  graphics::plot(log(abs(stats::predict(logreg))), 
       res, main = "Residual plot", 
       xlab = "Log-predicted values", 
       ylab = "Deviance residuals")
  graphics::abline(h = 0, lty = 2)
  stats::qqnorm(res)
  stats::qqline(res)
  # ------------------------------------------------------
  # Residual plot two
  # ------------------------------------------------------
  set_theme("scatterw")
  gp <- ggplot(data.frame(x = stats::predict(logreg), 
                          y = stats::residuals(logreg),
                          grp = stats::model.frame(logreg)[[1]]),
               aes(x, y)) + 
    geom_point(aes(colour = grp), show.legend = F) + 
    geom_hline(yintercept = 0) +
    stat_smooth(method = "loess", se = T) +
    labs(title = "Residual plot",
         x = "Log-predicted values",
         y = "Deviance residuals")
  graphics::plot(gp)
  # ------------------------------------------------------
  # Check "linearity"
  # ------------------------------------------------------
  preds <- colnames(logreg$model)[-1]
  for (pr in preds) {
    if (length(unique(logreg$model[[pr]])) > 4) {
      mydat <- data.frame(x = logreg$model[[pr]], 
                          y = stats::residuals(logreg),
                          grp = as.factor(stats::model.frame(logreg)[[1]]))
      gp <- ggplot(mydat, aes(x, y)) + 
        geom_point(aes(colour = grp), show.legend = F) + 
        geom_hline(yintercept = 0) +
        stat_smooth(method = "loess", se = T) +
        labs(x = pr, y = "Residuals",
             title = "Linear relationship between predictor and residuals")
      graphics::plot(gp)
    }
  }
  # -------------------------------------
  # Anova-Test
  # We can see that all terms were highly significant when they were
  # introduced into the model.
  # -------------------------------------
  message("--------------------\nCheck significance of terms when they entered the model...")
  message("Anova:\n")
  print(stats::anova(logreg, test = "Chisq"))
  # -------------------------------------
  set_theme("forest")
  sjp.glm(logreg)
  # return updated model
  invisible(structure(list(class = "sjp.glm.ma",
                           model = model,
                           outlier = outlier)))
}
