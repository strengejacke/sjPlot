# bind global variables

#' @title Plot estimates of multiple fitted lm(er)'s
#' @name sjp.lmm
#' 
#' @description Plot and compare coefficients (estimates) with confidence 
#'                intervals of  multiple fitted linear (mixed effects) models 
#'                in one plot. 
#'                Fitted models may have differing predictors, but only
#'                in a "stepwise" sense.
#'                
#' @param ... one or more fitted \code{lm} or \code{lmerMod}-objects. May also 
#'          be a \code{\link{list}}-object with fitted models, instead of separating 
#'          each model with comma. See 'Examples'.
#' @param type type of plot. Use one of following:
#'          \describe{
#'            \item{\code{"lm"}}{(default) for forest-plot like plot of estimates.}
#'            \item{\code{"std"}}{for forest-plot like plot of standardized beta values.}
#'            \item{\code{"std2"}}{for forest-plot like plot of standardized beta values, however, standardization is done by rescaling estimates by dividing them by two sd (see 'Details' in \code{\link{sjp.lm}}).}
#'          }
#' @param legend.pval.title character vector, used as title of the plot legend that 
#'          indicates the p-values. Default is \code{"p-level"}. Only applies if \code{p.shape = TRUE}.
#' @param geom.spacing spacing between the dots and error bars of the plotted fitted models. Default
#'          is 0.3.
#' @param fade.ns if \code{TRUE}, non significant estimates will be printed in slightly faded colors.
#' @param p.shape If \code{TRUE}, significant levels are distinguished by different point shapes and a related
#'          legend is plotted. Default is \code{FALSE}.
#'          
#' @inheritParams sjp.lm
#' @inheritParams sjp.lmer
#' @inheritParams sjt.lm
#' @inheritParams sjp.grpfrq
#'           
#' @note The fitted models may have differing predictors, but only in a 
#'         "stepwise" sense; i.e., models should share a common set of predictors,
#'         while some models may have additional predictors (e.g. added via
#'         the \code{\link[stats]{update}} function). See 'Examples'.
#'             
#' @return (Insisibily) returns the ggplot-object with the complete plot (\code{plot}) as well as the data frame that
#'           was used for setting up the ggplot-object (\code{data}).
#'          
#' @examples
#' # prepare dummy variables for binary logistic regression
#' # Now fit the models. Note that all models share the same predictors
#' # and only differ in their dependent variable
#' library(sjmisc)
#' data(efc)
#' 
#' # fit three models
#' fit1 <- lm(barthtot ~ c160age + c12hour + c161sex + c172code, data = efc)
#' fit2 <- lm(neg_c_7 ~ c160age + c12hour + c161sex + c172code, data = efc)
#' fit3 <- lm(tot_sc_e ~ c160age + c12hour + c161sex + c172code, data = efc)
#' 
#' # plot multiple models
#' sjp.lmm(fit1, fit2, fit3, facet.grid = TRUE)
#' 
#' # plot multiple models with legend labels and 
#' # point shapes instead of value labels
#' sjp.lmm(fit1, fit2, fit3,
#'         axis.labels = c("Carer's Age", "Hours of Care", "Carer's Sex",
#'                         "Educational Status"),
#'         depvar.labels = c("Barthel Index", "Negative Impact", "Services used"),
#'         show.values = FALSE, show.p = FALSE, fade.ns = TRUE, p.shape = TRUE)
#' 
#' # ------------------------------
#' # plot multiple models from nested lists argument
#' # ------------------------------
#' all.models <- list()
#' all.models[[1]] <- fit1
#' all.models[[2]] <- fit2
#' all.models[[3]] <- fit3
#' 
#' sjp.lmm(all.models)
#' 
#' # ------------------------------
#' # plot multiple models with different
#' # predictors (stepwise inclusion),
#' # standardized estimates
#' # ------------------------------
#' fit1 <- lm(mpg ~ wt + cyl + disp + gear, data = mtcars)
#' fit2 <- update(fit1, . ~ . + hp)
#' fit3 <- update(fit2, . ~ . + am)
#' 
#' sjp.lmm(fit1, fit2, fit3, type = "std2")
#' 
#' @import ggplot2
#' @importFrom stats coef confint
#' @importFrom dplyr slice
#' @importFrom sjstats merMod_p
#' @importFrom tibble lst
#' @export
sjp.lmm <- function(...,
                    type = "lm",
                    remove.estimates = NULL,
                    title = NULL,
                    depvar.labels = NULL,
                    legend.title = "Dependent Variables",
                    legend.pval.title = "p-level",
                    axis.labels = NULL,
                    axis.title = "Estimates",
                    axis.lim = NULL,
                    wrap.title = 50,
                    wrap.labels = 25,
                    wrap.legend.title = 20,
                    grid.breaks = NULL,
                    geom.size = 3,
                    geom.spacing = 0.4,
                    geom.colors = "Set1",
                    show.values = TRUE,
                    show.legend = TRUE,
                    show.intercept = FALSE,
                    show.p = TRUE,
                    fade.ns = FALSE,
                    p.shape = FALSE,
                    p.kr = TRUE,
                    vline.type = 2,
                    vline.color = "grey70",
                    digits = 2,
                    facet.grid = FALSE,
                    coord.flip = TRUE,
                    prnt.plot = TRUE) {
  # --------------------------------------------------------
  # retrieve list of fitted models
  # --------------------------------------------------------
  input_list <- tibble::lst(...)
  # --------------------------------------------------------
  # check length. if we have a list of fitted model, 
  # we need to "unlist" them
  # --------------------------------------------------------
  if (length(input_list) == 1 && class(input_list[[1]]) == "list")
    input_list <- lapply(input_list[[1]], function(x) x)
  # ----------------------------
  # init final data frame
  # ----------------------------
  finalbetas <- c()
  fitlength <- length(input_list)
  # ----------------------------
  # Prepare length of title and labels
  # ----------------------------
  # if we have no labels of dependent variables supplied, use a 
  # default string (Model) for legend
  if (is.null(depvar.labels)) {
    depvar.labels <- c()
    for (i in seq_len(fitlength)) {
      depvar.labels <- c(depvar.labels, 
                         get_model_response_label(input_list[[i]]))
    }
  }
  # check length of diagram title and split longer string at into new lines
  if (!is.null(title)) title <- sjmisc::word_wrap(title, wrap.title)
  # check length of x-axis title and split longer string at into new lines
  # every 50 chars
  if (!is.null(axis.title)) axis.title <- sjmisc::word_wrap(axis.title, wrap.title)
  # check length of dependent variables
  if (!is.null(depvar.labels)) depvar.labels <- sjmisc::word_wrap(depvar.labels, wrap.legend.title)
  # check length of x-axis-labels and split longer strings at into new lines
  if (!is.null(axis.labels)) axis.labels <- sjmisc::word_wrap(axis.labels, wrap.labels)
  # ----------------------------
  # iterate all fitted models
  # ----------------------------
  for (fitcnt in seq_len(fitlength)) {
    # retrieve fitted model
    fit <- input_list[[fitcnt]]
    # ----------------------------
    # retrieve beta's (lm)
    # ----------------------------
    if (type %in% c("std", "std2")) {
      # retrieve standardized betas
      betas <- suppressWarnings(sjstats::std_beta(fit, type = type)) %>% 
        dplyr::select_("-std.error")
      # no intercept for std
      show.intercept <- FALSE
      # remove intercept for merMod
      if (is_merMod(fit)) betas <- betas[-1, ]
      # add "std." to title?
      if (axis.title == "Estimates") axis.title <- "Std. Estimates"
    } else {
      # do we have mermod object?
      if (sjmisc::str_contains(class(fit), "merMod", ignore.case = T))
        betas <- get_cleaned_ciMerMod(fit, "lm")
      else {
        # copy estimates to data frame
        betas <- data.frame(stats::coef(fit), stats::confint(fit))
        betas <- tibble::rownames_to_column(betas, var = "term")
      }
      # show intercept?
      if (!show.intercept) betas <- betas[-1, ]
    }
    # ----------------------------
    # give proper column names
    # ----------------------------
    colnames(betas) <- c("term", "beta", "ci.low", "ci.hi")
    # ----------------------------
    # print p-values in bar charts
    # ----------------------------
    # retrieve sigificance level of independent variables (p-values)
    pv <- sjstats::get_model_pval(fit, p.kr = p.kr)$p.value
    # remove intercept from df, if necessary
    if (!show.intercept) pv <- pv[-1]
    # for better readability, convert p-values to asterisks
    # with:
    # p < 0.001 = ***
    # p < 0.01 = **
    # p < 0.05 = *
    ov <- betas$beta
    # "ps" holds the p-value of the coefficients, including asterisks, as
    # string vector
    ps <- NULL
    # point shapes indicate different shapes for geom_point, according to
    # the p-level
    pointshapes <- NULL
    # palpha indicates whether a coefficient is significant or not.
    # non-significant values can be drawn with a lesser alpha-level
    # (i.e. are more transparent)
    palpha <- NULL
    for (i in seq_len(length(pv))) {
      ps[i] <- ""
      pointshapes[i] <- 1
      palpha[i] <- "s"
    }
    # print values to p-string
    if (show.values) ps <- sprintf("%.*f", digits, ov)
    # ----------------------------
    # copy p-values into data column
    # ----------------------------
    for (i in seq_len(length(pv))) {
      if (pv[i] >= 0.05) {
        pointshapes[i] <- 1
        palpha[i] <- "ns"
      } else if (pv[i] >= 0.01 && pv[i] < 0.05) {
        if (show.p) ps[i] <- paste(ps[i], "*")
        pointshapes[i] <- 2
      } else if (pv[i] >= 0.001 && pv[i] < 0.01) {
        if (show.p) ps[i] <- paste(ps[i], "**")
        pointshapes[i] <- 3
      } else {
        if (show.p) ps[i] <- paste(ps[i], "***")
        pointshapes[i] <- 4
      }
    }  
    # ----------------------------
    # bind p-values to data frame
    # ----------------------------
    betas <- data.frame(betas, p = ps, pa = palpha, shape = pointshapes, grp = fitcnt, p.value = pv)
    # add data frame to final data frame
    finalbetas <- rbind(finalbetas, betas)
  }
  # ----------------------------
  # check if user defined labels have been supplied
  # if not, use variable names from data frame
  # ----------------------------
  # reverse x-pos, convert to factor
  finalbetas$xpos <- sjmisc::to_value(as.factor(finalbetas$term), keep.labels = F)
  finalbetas$xpos <- as.factor(finalbetas$xpos)
  finalbetas$grp <- as.factor(finalbetas$grp)
  # convert to character
  finalbetas$shape <- as.character(finalbetas$shape)
  # -------------------------------------------------
  # remove any estimates from the output?
  # -------------------------------------------------
  if (!is.null(remove.estimates)) {
    # get row indices of rows that should be removed
    remrows <- c()
    for (re in seq_len(length(remove.estimates))) {
      remrows <- c(remrows, which(substr(finalbetas$term, 
                                         start = 1, 
                                         stop = nchar(remove.estimates[re])) == remove.estimates[re]))
    }
    # remove rows
    finalbetas <- dplyr::slice(finalbetas, seq_len(nrow(finalbetas))[-remrows])
  }
  # set axis labels
  if (is.null(axis.labels)) {
    axis.labels <- unique(finalbetas$term)
    axis.labels <- axis.labels[order(unique(finalbetas$xpos))]
  }
  # --------------------------------------------------------
  # Calculate axis limits. The range is from lowest lower-CI
  # to highest upper-CI, or a user defined range
  # --------------------------------------------------------
  if (is.null(axis.lim)) {
    # we have confindence intervals displayed, so
    # the range corresponds to the boundaries given by
    # the CI's
    upper_lim <- ceiling(10 * max(finalbetas$ci.hi)) / 10
    lower_lim <- floor(10 * min(finalbetas$ci.low)) / 10
    # if we show p value labels, increase upper
    # limit of x axis, so labels are plotted inside
    # diagram range
    if (show.values || show.p) upper_lim <- upper_lim + 0.1
  } else {
    # Here we have user defind axis range
    lower_lim <- axis.lim[1]
    upper_lim <- axis.lim[2]
  }
  # --------------------------------------------------------
  # Define axis ticks, i.e. at which position we have grid
  # bars.
  # --------------------------------------------------------
  # determine gridbreaks
  if (is.null(grid.breaks)) {
    ticks <- pretty(c(lower_lim, upper_lim))
  } else {
    ticks <- c(seq(lower_lim, upper_lim, by = grid.breaks))
  }
  # --------------------------------------------------------
  # prepare star and shape values. we just copy those values
  # that are actually needed, so legend shapes are always 
  # identical, independent whether model have only two 
  # different p-levels or four.
  # --------------------------------------------------------
  shape.values <- c(1, 16, 17, 15)
  star.values <- c("n.s.", "*", "**", "***")
  shape.values <- shape.values[sort(as.numeric(unique(finalbetas$shape)))]
  star.values <- star.values[sort(as.numeric(unique(finalbetas$shape)))]
  # --------------------------------------------------------
  # body of plot
  # --------------------------------------------------------
  # The order of aesthetics matters in terms of ordering the error bars!
  # Using alpha-aes before colour would order error-bars according to
  # alpha-level instead of colour-aes.
  plotHeader <- ggplot(finalbetas, aes_string(y = "beta", x = "xpos", group = "grp", 
                                              colour = "grp", alpha = "pa"))
  # --------------------------------------------------------
  # start with dot-plotting here
  # first check, whether user wants different shapes for
  # different p-levels
  # --------------------------------------------------------
  if (p.shape) {
    plotHeader <- plotHeader +
      # set shape aesthetic. we have to repeat the other aesthestics as well,
      # because otherwise the order of point shapes differes from the order
      # of error bars.
      # The order of aesthetics matters in terms of ordering the error bars!
      # Using shape before colour would order points according to shapes instead
      # of colour-aes.
      geom_point(aes_string(shape = "shape"), 
                 size = geom.size, 
                 position = position_dodge(-geom.spacing)) +
      # and use a shape scale, in order to have a legend
      scale_shape_manual(values = shape.values, 
                         labels = star.values)
  } else {
    plotHeader <- plotHeader +
      geom_point(size = geom.size, position = position_dodge(-geom.spacing))
  }
  # --------------------------------------------------------
  # fade non-significant estimates?
  # --------------------------------------------------------
  nsAlpha <- ifelse(isTRUE(fade.ns), 0.3, 1.0)
  # --------------------------------------------------------
  # continue with errorbars, p-value-label and intercept line
  # --------------------------------------------------------
  plotHeader <- plotHeader +
    # --------------------------------------------------------
    # print confidence intervalls (error bars)
    # --------------------------------------------------------
    geom_errorbar(aes_string(ymin = "ci.low", ymax = "ci.hi"), 
                  position = position_dodge(-geom.spacing), 
                  width = 0) +
    # --------------------------------------------------------
    # print value labels and p-values
    # --------------------------------------------------------
    geom_text(aes_string(label = "p", y = "ci.hi"), 
              position = position_dodge(width = -geom.spacing), 
              hjust = -0.1,
              show.legend = FALSE) +
    # --------------------------------------------------------
    # Intercept-line
    # --------------------------------------------------------
    geom_hline(yintercept = 0, 
               linetype = vline.type, 
               color = vline.color) +
    labs(title = title, 
         x = NULL, 
         y = axis.title, 
         shape = legend.pval.title, 
         colour = legend.title) +
    scale_x_discrete(labels = axis.labels) +
    scale_y_continuous(limits = c(lower_lim, upper_lim), 
                       breaks = ticks, 
                       labels = ticks) +
    # --------------------------------------------------------
    # use transparancy if requested, but hide legend
    # --------------------------------------------------------
    scale_alpha_manual(values = c(nsAlpha, 1.0), guide = "none")
  # --------------------------------------------------------
  # flip coordinates?
  # --------------------------------------------------------
  if (coord.flip)  plotHeader <- plotHeader + coord_flip()
  if (facet.grid) plotHeader <- plotHeader + facet_grid(~grp)
  # ---------------------------------------------------------
  # set geom colors
  # ---------------------------------------------------------
  plotHeader <- sj.setGeomColors(plotHeader, 
                                 geom.colors, 
                                 length(depvar.labels), 
                                 show.legend, 
                                 depvar.labels)  
  # ---------------------------------------------------------
  # Check whether ggplot object should be returned or plotted
  # ---------------------------------------------------------
  if (prnt.plot) graphics::plot(plotHeader)
  # -------------------------------------
  # set proper column names
  # -------------------------------------
  colnames(finalbetas) <- c("estimate", "conf.low", "conf.high", "p.string", 
                            "p.alpha", "shape", "grp", "p.value", "term", "xpos")
  # -------------------------------------
  # return results
  # -------------------------------------
  invisible(structure(class = c("sjPlot", "sjplmm"),
                      list(plot = plotHeader,
                           data = finalbetas)))
}
