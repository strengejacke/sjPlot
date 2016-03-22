# bind global variables
utils::globalVariables("pv")


#' @title Plot One-Way-Anova tables
#' @name sjp.aov1
#' 
#' @description Plot One-Way-Anova table sum of squares (SS) of each factor level (group) 
#'                against the dependent variable. The SS of the factor variable against the 
#'                dependent variable (variance within and between groups) is printed to
#'                the model summary.
#'                
#' @seealso \code{\link{sjt.grpmean}}
#'                
#' @param depVar dependent variable. Will be used with following formula:
#'          \code{aov(depVar ~ grpVar)}
#' @param grpVar grouping variable, as unordered factor. Will be used with following formula:
#'          \code{aov(depVar ~ grpVar)}
#' @param meansums logical, if \code{TRUE}, the values reported are the true group mean values (see also \code{\link{sjt.grpmean}}).
#'          If \code{FALSE} (default), the values are reported in the standard way, i.e. the values indicate the difference of
#'          the group mean in relation to the intercept (reference group).
#' @param axisLabels.y character vector, indicating the value labels of \code{grpVar} that 
#'          are used for labelling the axis. See 'Examples'.
#' @param reverseOrder logical, if \code{TRUE}, the order of categories (groups) is reversed.
#'          Default is \code{FALSE}.
#' @param stringIntercept string that indicates the reference group (intercept), that is appended to
#'          the value label of the grouping variable. Default is \code{"(Intercept)"}.
#' @param axisLimits numeric vector of length 2, defining the range of the plot axis.
#'          By default, the limits range from the lowest confidence interval to the 
#'          highest, so plot has maximum zoom.
#' @param geom.colors vector of length two, indicating the colors of the points; 
#'          first value is for groups with positive means and the second for 
#'          negative means.
#' @param geom.size size of the points. Default is 3.
#' @param showValueLabels logical, whether the value labels (mean differences) should be plotted 
#'          to each dot or not.
#' @param showModelSummary logical, if \code{TRUE}, a summary of the anova model with 
#'          Sum of Squares between groups (ssb), Sum of Squares within groups (ssw), multiple and adjusted 
#'          R-square and F-Test is printed to the lower right corner
#'          of the diagram. Default is \code{TRUE}.
#'          
#' @inheritParams sjp.grpfrq
#' @inheritParams sjp.lm
#'          
#' @return (Insisibily) returns the ggplot-object with the complete plot (\code{plot}) as well as the data frame that
#'           was used for setting up the ggplot-object (\code{df}).
#' 
#' @examples
#' library(sjmisc)
#' data(efc)
#' # note: "grpVar" does not need to be a factor.
#' # coercion to factor is done by the function
#' sjp.aov1(efc$c12hour, efc$e42dep)
#' 
#' 
#' @import ggplot2
#' @import sjmisc
#' @importFrom stats confint aov summary.lm
#' @importFrom dplyr add_rownames
#' @export
sjp.aov1 <- function(depVar,
                     grpVar,
                     meansums = FALSE,
                     title = NULL,
                     axisLabels.y = NULL,
                     reverseOrder = FALSE,
                     stringIntercept = "(Intercept)",
                     showAxisLabels.y = TRUE,
                     axisTitle.x = "",
                     axisLimits = NULL,
                     geom.colors = c("#3366a0", "#aa3333"),
                     geom.size = 3,
                     breakTitleAt = 50,
                     breakLabelsAt = 25,
                     gridBreaksAt = NULL,
                     expand.grid = FALSE,
                     showValueLabels = TRUE,
                     labelDigits = 2,
                     y.offset = .1,
                     showPValueLabels = TRUE,
                     showModelSummary = FALSE,
                     printPlot = TRUE) {
  # --------------------------------------------------------
  # get variable name
  # --------------------------------------------------------
  grpVar.name <- get_var_name(deparse(substitute(grpVar)))
  depVar.name <- get_var_name(deparse(substitute(depVar)))
  # --------------------------------------------------------
  # try to automatically set labels is not passed as parameter
  # --------------------------------------------------------
  if (is.null(axisLabels.y)) axisLabels.y <- sjmisc::get_labels(grpVar, 
                                                                attr.only = F,
                                                                include.values = NULL,
                                                                include.non.labelled = T)
  if (is.null(axisTitle.x)) axisTitle.x <- sjmisc::get_label(depVar, def.value = depVar.name)
  if (is.null(title)) {
    t1 <- sjmisc::get_label(grpVar, def.value = grpVar.name)
    t2 <- sjmisc::get_label(depVar, def.value = depVar.name)
    if (!is.null(t1) && !is.null(t2)) title <- paste0(t1, " by ", t2)
  }
  # --------------------------------------------------------
  # remove titles if empty
  # --------------------------------------------------------
  if (!is.null(axisLabels.y) && axisLabels.y == "") axisLabels.y <- NULL
  if (!is.null(axisTitle.x) && axisTitle.x == "") axisTitle.x <- NULL  
  if (!is.null(title) && title == "") title <- NULL    
  # --------------------------------------------------------
  # unlist labels
  # --------------------------------------------------------
  if (!is.null(axisLabels.y)) {
    # append "intercept" string, to mark the reference category
    axisLabels.y[1] <- paste(axisLabels.y[1], stringIntercept)
  }
  # --------------------------------------------------------
  # Check if grpVar is factor. If not, convert to factor
  # --------------------------------------------------------
  if (!is.factor(grpVar)) grpVar <- as.factor(grpVar)
  # --------------------------------------------------------
  # Check spelling of type-param
  # --------------------------------------------------------
  if (isTRUE(expand.grid)) 
    expand.grid <- ggplot2::waiver()
  else
    expand.grid <- c(0, 0)
  # --------------------------------------------------------
  # check whether we have x-axis title. if not, use standard
  # value
  # --------------------------------------------------------
  if (is.null(axisTitle.x)) axisTitle.x <- c("")
  # check length of diagram title and split longer string at into new lines
  # every 50 chars
  if (!is.null(title)) title <- sjmisc::word_wrap(title, breakTitleAt)
  # check length of x-axis title and split longer string at into new lines
  # every 50 chars
  if (!is.null(axisTitle.x)) axisTitle.x <- sjmisc::word_wrap(axisTitle.x, breakTitleAt)
  # check length of x-axis-labels and split longer strings at into new lines
  # every 10 chars, so labels don't overlap
  if (!is.null(axisLabels.y)) axisLabels.y <- sjmisc::word_wrap(axisLabels.y, breakLabelsAt)
  # ----------------------------
  # Calculate one-way-anova. Since we have
  # only one group variable, Type of SS does
  # not matter.
  # ----------------------------
  fit <- stats::aov(depVar ~ grpVar)
  # coefficients (group mean)
  means <- stats::summary.lm(fit)$coefficients[, 1]
  # p-values of means
  means.p <- stats::summary.lm(fit)$coefficients[, 4]
  # lower confidence intervals of coefficients (group mean)
  means.lci <- stats::confint(fit)[, 1]
  # upper confidence intervals of coefficients (group mean)
  means.uci <- stats::confint(fit)[, 2]
  # ----------------------------
  # Check whether true group means should be reported
  # or the differences of group means in relation to the
  # intercept (reference group). The latter is the default.
  # ----------------------------
  if (meansums) {
    for (i in 2:length(means)) {
      means[i] <- means[i] + means[1]
      means.lci[i] <- means.lci[i] + means[1]
      means.uci[i] <- means.uci[i] + means[1]
    }
  }
  # ----------------------------
  # create expression with model summarys. used
  # for plotting in the diagram later
  # ----------------------------
  if (showModelSummary) {
    # sum of squares
    ss <- summary(fit)[[1]]['Sum Sq']
    # multiple r2
    r2 <- stats::summary.lm(fit)$r.squared
    # adj. r2
    r2.adj <- stats::summary.lm(fit)$adj.r.squared
    # get F-statistics
    fstat <- stats::summary.lm(fit)$fstatistic[1]
    # p-value for F-test
    pval <- summary(fit)[[1]]['Pr(>F)'][1, 1]
    # indicate significance level by stars
    pan <- get_p_stars(pval)
    # create mathematical term
    modsum <- as.character(as.expression(
      substitute(italic(SS[B]) == ssb * "," ~~ italic(SS[W]) == ssw * "," ~~ R^2 == mr2 * "," ~~ "adj." * R^2 == ar2 * "," ~~ "F" == f * panval,
                 list(ssb = sprintf("%.2f", ss[1, ]),
                      ssw = sprintf("%.2f", ss[2, ]),
                      mr2 = sprintf("%.3f", r2),
                      ar2 = sprintf("%.3f", r2.adj),
                      f = sprintf("%.2f", fstat),
                      panval = pan))))
  }
  # ----------------------------
  # print coefficients and p-values in plot
  # ----------------------------
  # init data column for p-values
  ps <- c(round(means, labelDigits))
  # if no values should be shown, clear
  # vector now
  if (!showValueLabels) ps <- rep(c(""), length(ps))
  # --------------------------------------------------------
  # copy p-values into data column
  # --------------------------------------------------------
  if (showPValueLabels) {
    for (i in 1:length(means.p)) {
      ps[i] <- sjmisc::trim(paste(ps[i], get_p_stars(means.p[i])))
    }  
  }
  # --------------------------------------------------------
  # check whether order of category items should be reversed
  # or not
  # --------------------------------------------------------
  if (reverseOrder)
    catorder <- c(length(means):1)
  else
    catorder <- c(1:length(means))
  # --------------------------------------------------------
  # create new data.frame, since ggplot requires data.frame as parameter
  # The data frame contains means, CI and p-values
  # --------------------------------------------------------
  df <- data.frame(means,     # Append coefficients
                   means.lci, # append CI
                   means.uci,
                   means.p,   # append p-value
                   ps,
                   catorder)
  # --------------------------------------------------------
  # check if user defined labels have been supplied
  # if not, use variable names from data frame
  # --------------------------------------------------------
  if (is.null(axisLabels.y)) axisLabels.y <- row.names(df)
  # order labels
  axisLabels.y <- axisLabels.y[catorder]
  # give columns names
  names(df) <- c("means", "lower", "upper", "p", "pv", "xv")
  df$means <- sjmisc::to_value(df$means, keep.labels = F)
  df$lower <- sjmisc::to_value(df$lower, keep.labels = F)
  df$upper <- sjmisc::to_value(df$upper, keep.labels = F)
  df$p <- sjmisc::to_value(df$p, keep.labels = F)
  df$pv <- as.character(df$pv)
  # bind color values to data frame, because we cannot use several
  # different color aesthetics in ggplot
  df <- cbind(df, geocol = ifelse(df$means >= 0, geom.colors[1], geom.colors[2]))
  # --------------------------------------------------------
  # Calculate axis limits. The range is from lowest lower-CI
  # to highest upper-CI, or a user-defined range (if "axisLimits"
  # is not NULL)
  # --------------------------------------------------------
  if (is.null(axisLimits)) {
    # we have confindence intervals displayed, so
    # the range corresponds to the boundaries given by
    # the CI's
    maxval <- max(df$upper)
    minval <- min(df$lower)
    if (maxval > 0)
      limfac <- ifelse(abs(maxval) < 5, 5, 10)
    else 
      limfac <- ifelse(abs(minval) < 5, 5, 10)
    upper_lim <- ifelse(maxval == 0, 0, limfac * ceiling((maxval + 1) / limfac))
    lower_lim <- ifelse(minval == 0, 0, limfac * floor(minval / limfac))
  } else {
    lower_lim <- axisLimits[1]
    upper_lim <- axisLimits[2]
  }
  # determine gridbreaks
  if (is.null(gridBreaksAt))
    ticks <- pretty(c(lower_lim, upper_lim))
  else
    ticks <- c(seq(lower_lim, upper_lim, by = gridBreaksAt))
  if (!showAxisLabels.y) axisLabels.y <- c("")
  # --------------------------------------------------------
  # Set up plot padding (margins inside diagram)
  # --------------------------------------------------------
  scaley <- scale_y_continuous(limits = c(lower_lim, upper_lim), 
                               breaks = ticks, 
                               labels = ticks)    
  # --------------------------------------------------------
  # Start plot here!
  # --------------------------------------------------------
  anovaplot <- ggplot(df, aes(y = means, x = xv)) +
    # print point
    geom_point(size = geom.size, colour = df$geocol) +
    # and error bar
    geom_errorbar(aes(ymin = lower, ymax = upper), 
                  colour = df$geocol, 
                  width = 0) +
    # Print p-values. With vertical adjustment, so 
    # they don't overlap with the errorbars
    geom_text(aes(label = pv, y = means), 
              nudge_x = y.offset, 
              show.legend = FALSE) +
    # set y-scale-limits, breaks and tick labels
    scaley +
    # set value labels to x-axis
    scale_x_discrete(labels = axisLabels.y, limits = c(1:nrow(df))) +
    # flip coordinates
    labs(title = title, x = NULL, y = axisTitle.x) +
    coord_flip()
  # check whether modelsummary should be printed
  if (showModelSummary) {
    # add annotations with model summary
    # annotations include intercept-value and model's r-square
    anovaplot <- anovaplot + annotate("text", 
                                      label = modsum, 
                                      parse = TRUE, 
                                      x = -Inf, 
                                      y = Inf)
  }
  # ---------------------------------------------------------
  # Check whether ggplot object should be returned or plotted
  # ---------------------------------------------------------
  if (printPlot) print(anovaplot)
  # -------------------------------------
  # set proper column names
  # -------------------------------------
  df <- dplyr::add_rownames(df)
  colnames(df) <- c("term", "estimate", "conf.low", "conf.high", 
                    "p.value", "p.string", "xpos", "geom.color")
  # -------------------------------------
  # return results
  # -------------------------------------
  invisible(structure(class = "sjpaov1",
                      list(plot = anovaplot,
                           data = df)))
}
