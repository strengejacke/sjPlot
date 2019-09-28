#' @title Plot One-Way-Anova tables
#' @name sjp.aov1
#'
#' @description Plot One-Way-Anova table sum of squares (SS) of each factor level (group)
#'                against the dependent variable. The SS of the factor variable against the
#'                dependent variable (variance within and between groups) is printed to
#'                the model summary.
#'
#' @param var.dep Dependent variable. Will be used with following formula:
#'          \code{aov(var.dep ~ var.grp)}
#' @param var.grp Factor with the cross-classifying variable, where \code{var.dep}
#'          is grouped into the categories represented by \code{var.grp}.
#' @param meansums Logical, if \code{TRUE}, the values reported are the true group mean values.
#'          If \code{FALSE} (default), the values are reported in the standard way, i.e. the values indicate the difference of
#'          the group mean in relation to the intercept (reference group).
#' @param string.interc Character vector that indicates the reference group (intercept), that is appended to
#'          the value label of the grouping variable. Default is \code{"(Intercept)"}.
#'
#' @inheritParams plot_grpfrq
#' @inheritParams plot_xtab
#' @inheritParams plot_gpt
#' @inheritParams plot_model
#'
#' @return A ggplot-object.
#'
#' @examples
#' data(efc)
#' # note: "var.grp" does not need to be a factor.
#' # coercion to factor is done by the function
#' sjp.aov1(efc$c12hour, efc$e42dep)
#'
#'
#' @import ggplot2
#' @importFrom sjmisc trim word_wrap to_value
#' @importFrom stats confint aov summary.lm
#' @importFrom rlang .data
#' @importFrom sjlabelled get_label get_labels
#' @export
sjp.aov1 <- function(var.dep,
                     var.grp,
                     meansums = FALSE,
                     title = NULL,
                     axis.labels = NULL,
                     rev.order = FALSE,
                     string.interc = "(Intercept)",
                     axis.title = "",
                     axis.lim = NULL,
                     geom.colors = c("#3366a0", "#aa3333"),
                     geom.size = 3,
                     wrap.title = 50,
                     wrap.labels = 25,
                     grid.breaks = NULL,
                     show.values = TRUE,
                     digits = 2,
                     y.offset = .15,
                     show.p = TRUE,
                     show.summary = FALSE) {
  # --------------------------------------------------------
  # get variable name
  # --------------------------------------------------------
  var.grp.name <- get_var_name(deparse(substitute(var.grp)))
  var.dep.name <- get_var_name(deparse(substitute(var.dep)))
  # --------------------------------------------------------
  # try to automatically set labels is not passed as parameter
  # --------------------------------------------------------
  if (is.null(axis.labels)) axis.labels <- sjlabelled::get_labels(var.grp,
                                                              attr.only = F,
                                                              values = NULL,
                                                              non.labelled = T)
  if (is.null(axis.title)) axis.title <- sjlabelled::get_label(var.dep, def.value = var.dep.name)
  if (is.null(title)) {
    t1 <- sjlabelled::get_label(var.grp, def.value = var.grp.name)
    t2 <- sjlabelled::get_label(var.dep, def.value = var.dep.name)
    if (!is.null(t1) && !is.null(t2)) title <- paste0(t1, " by ", t2)
  }
  # --------------------------------------------------------
  # remove titles if empty
  # --------------------------------------------------------
  if (!is.null(axis.labels) && length(axis.labels) == 1 && axis.labels == "") axis.labels <- NULL
  if (!is.null(axis.title) && length(axis.title) == 1 && axis.title == "") axis.title <- NULL
  if (!is.null(title) && length(title) == 1 && title == "") title <- NULL
  # --------------------------------------------------------
  # unlist labels
  # --------------------------------------------------------
  if (!is.null(axis.labels)) {
    # append "intercept" string, to mark the reference category
    axis.labels[1] <- paste(axis.labels[1], string.interc)
  }
  # --------------------------------------------------------
  # Check if var.grp is factor. If not, convert to factor
  # --------------------------------------------------------
  if (!is.factor(var.grp)) var.grp <- as.factor(var.grp)
  # --------------------------------------------------------
  # check whether we have x-axis title. if not, use standard
  # value
  # --------------------------------------------------------
  # check length of diagram title and split longer string at into new lines
  # every 50 chars
  if (!is.null(title)) title <- sjmisc::word_wrap(title, wrap.title)
  # check length of x-axis title and split longer string at into new lines
  # every 50 chars
  if (!is.null(axis.title)) axis.title <- sjmisc::word_wrap(axis.title, wrap.title)
  # check length of x-axis-labels and split longer strings at into new lines
  # every 10 chars, so labels don't overlap
  if (!is.null(axis.labels)) axis.labels <- sjmisc::word_wrap(axis.labels, wrap.labels)
  # ----------------------------
  # Calculate one-way-anova. Since we have
  # only one group variable, Type of SS does
  # not matter.
  # ----------------------------
  fit <- stats::aov(var.dep ~ var.grp)
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
  if (show.summary) {
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
  ps <- round(means, digits)
  # if no values should be shown, clear
  # vector now
  if (!show.values) ps <- rep("", length(ps))
  # --------------------------------------------------------
  # copy p-values into data column
  # --------------------------------------------------------
  if (show.p) {
    for (i in seq_len(length(means.p))) {
      ps[i] <- sjmisc::trim(paste(ps[i], get_p_stars(means.p[i])))
    }
  }
  # --------------------------------------------------------
  # check whether order of category items should be reversed
  # or not
  # --------------------------------------------------------
  if (rev.order)
    catorder <- length(means):1
  else
    catorder <- seq_len(length(means))
  # --------------------------------------------------------
  # create new data.frame, since ggplot requires data.frame as parameter
  # The data frame contains means, CI and p-values
  # --------------------------------------------------------
  df <- data_frame(
    means = means,     # Append coefficients
    lower = means.lci, # append CI
    upper = means.uci,
    p = means.p,   # append p-value
    pv = ps,
    xv = catorder
  )
  # --------------------------------------------------------
  # check if user defined labels have been supplied
  # if not, use variable names from data frame
  # --------------------------------------------------------
  if (is.null(axis.labels)) axis.labels <- row.names(df)
  # order labels
  axis.labels <- axis.labels[catorder]
  df$means <- sjmisc::to_value(df$means, keep.labels = F)
  df$lower <- sjmisc::to_value(df$lower, keep.labels = F)
  df$upper <- sjmisc::to_value(df$upper, keep.labels = F)
  df$p <- sjmisc::to_value(df$p, keep.labels = F)
  df$pv <- as.character(df$pv)
  df$xv <- as.factor(df$xv)
  # bind color values to data frame, because we cannot use several
  # different color aesthetics in ggplot
  df <- cbind(df, geocol = ifelse(df$means >= 0, geom.colors[1], geom.colors[2]))
  # --------------------------------------------------------
  # Calculate axis limits. The range is from lowest lower-CI
  # to highest upper-CI, or a user-defined range (if "axis.lim"
  # is not NULL)
  # --------------------------------------------------------
  if (is.null(axis.lim)) {
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
    lower_lim <- axis.lim[1]
    upper_lim <- axis.lim[2]
  }
  # determine gridbreaks
  if (is.null(grid.breaks))
    ticks <- pretty(c(lower_lim, upper_lim))
  else
    ticks <- seq(lower_lim, upper_lim, by = grid.breaks)
  # --------------------------------------------------------
  # Set up plot padding (margins inside diagram)
  # --------------------------------------------------------
  scaley <- scale_y_continuous(
    limits = c(lower_lim, upper_lim),
    breaks = ticks,
    labels = ticks
  )
  # --------------------------------------------------------
  # Start plot here!
  # --------------------------------------------------------
  anovaplot <- ggplot(df, aes(y = .data$means, x = .data$xv)) +
    # print point
    geom_point(size = geom.size, colour = df$geocol) +
    # and error bar
    geom_errorbar(aes(ymin = .data$lower, ymax = .data$upper), colour = df$geocol, width = 0) +
    # Print p-values. With vertical adjustment, so
    # they don't overlap with the errorbars
    geom_text(aes(label = .data$pv, y = .data$means), nudge_x = y.offset, show.legend = FALSE) +
    # set y-scale-limits, breaks and tick labels
    scaley +
    # set value labels to x-axis
    scale_x_discrete(labels = axis.labels, limits = 1:length(axis.labels)) +
    # flip coordinates
    labs(title = title, x = NULL, y = axis.title) +
    coord_flip()

  # check whether modelsummary should be printed
  if (show.summary) {
    # add annotations with model summary
    # annotations include intercept-value and model's r-square
    anovaplot <- anovaplot +
      annotate("text", label = modsum, parse = TRUE, x = -Inf, y = Inf,
               hjust = "right", vjust = "bottom")
  }

  anovaplot
}
