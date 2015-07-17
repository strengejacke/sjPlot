#' @title Plot grouped proportional tables
#' @name sjp.gpt
#' 
#' @description Plot grouped proportional crosstables, where the proportion of
#'                each level of \code{x} for the highest category in \code{y}
#'                is plotted, for each subgroup pf \code{groups}.
#' 
#' @param x
#' @param y
#' @param groups
#' @param shape.fill.color
#' @param shapes
#' @param axisLabels
#' @param showTotal
#' @param showP
#' @param showN
#' 
#' @return (Insisibily) returns the ggplot-object with the complete plot (\code{plot}) as well as the data frame that
#'           was used for setting up the ggplot-object (\code{mydf}).
#'
#' @inheritParams sjp.grpfrq
#' @inheritParams sjp.xtab
#'
#' @examples 
#' library(sjmisc)
#' data(efc)
#' 
#' sjp.gpt(efc$e42dep, efc$e16sex, efc$15relat)
#' 
#' sjp.gpt(efc$c172code, efc$e42dep, efc$n4pstu)
#'
#' @import ggplot2
#' @import dplyr
#' @import sjmisc
#' @importFrom scales percent
#' @importFrom stats na.omit chisq.test fisher.test
#' @export
sjp.gpt <- function(x, 
                    y, 
                    groups, 
                    geom.colors = "Set1",
                    geom.size = 4,
                    shape.fill.color = "#f0f0f0",
                    shapes = c(15, 16, 17, 18, 21, 22, 23, 24, 25, 7, 8, 9, 10, 12),
                    axisLabels = NULL, 
                    axisTitle.x = NULL,
                    axisTitle.y = NULL,
                    legendTitle = NULL, 
                    legendLabels = NULL, 
                    title = NULL, 
                    showTotal = TRUE,
                    annotateTotal = TRUE,
                    showP = TRUE,
                    showN = TRUE,
                    hideLegend = FALSE,
                    printPlot = TRUE) {
  # --------------------------------------------------------
  # try to automatically set labels is not passed as argument
  # --------------------------------------------------------
  ylabels <- sjmisc::get_labels(y)
  if (is.null(ylabels)) {
    ylabels <- sjmisc:::autoSetVariableLabels(y)
  } else {
    ylabels <- ylabels[length(ylabels)]
  }
  if (is.null(axisLabels)) axisLabels <- sjmisc:::autoSetValueLabels(groups)
  if (is.null(axisTitle.y)) axisTitle.y <- paste0("Proportion of ", 
                                                  sjmisc:::autoSetVariableLabels(x), 
                                                  " in ",
                                                  ylabels)
  if (is.null(legendTitle)) legendTitle <- sjmisc:::autoSetVariableLabels(x)
  if (is.null(legendLabels)) legendLabels <- sjmisc:::autoSetValueLabels(x)
  # ------------------------------------
  # final data frae for plot
  # ------------------------------------
  newdf <- data.frame()
  group.p <- c()
  group.n <- c()
  # ------------------------------------
  # create data frame
  # ------------------------------------
  mydf <- data.frame(grp = sjmisc::to_value(groups, keep.labels = F),
                     x = sjmisc::to_factor(x),
                     dep = sjmisc::to_value(y, keep.labels = F))

  len.x <- length(unique(stats::na.omit(x)))
  len.y <- length(unique(stats::na.omit(y)))
  # ------------------------------------
  # create grouping variable
  # ------------------------------------
  groups <- sort(unique(stats::na.omit(sjmisc::to_value(groups))))
  # ------------------------------------
  # iterate all groups
  # ------------------------------------
  for (cnt in groups) {
    # ------------------------------------
    # select cases from groups (subset)
    # ------------------------------------
    dummy <- dplyr::filter(mydf, grp == cnt)
    # ------------------------------------
    # create proportional table of x and y
    # for this subgroup, retrieve proportion
    # of upper category
    # ------------------------------------
    ptab <- table(dummy$x, dummy$dep)
    y <- prop.table(ptab, margin = 1)
    if (len.y > ncol(y))
      y <- rep(0, nrow(y))
    else
      y <- y[, len.y]
    # -----------------
    # p-values
    #---------------------
#     if (any(apply(ptab, c(1,2), function(x) x < 5)))
#       pval <- suppressWarnings(stats::fisher.test(ptab)$p.value)
#     else
#       pval <- suppressWarnings(stats::chisq.test(ptab)$p.value)
    pval <- suppressWarnings(stats::chisq.test(ptab)$p.value)
    stern <- c("")
    if (!is.na(pval)) {
      if (pval < 0.001) {
        stern <- c("***")
      } else if (pval < 0.01) {
        stern <- c("**")
      } else if (pval < 0.05) {
        stern <- c("*")
      }
    }
    # add p-values for eacg group
    group.p <- c(group.p, stern)
    # create n for each group
    group.n <- c(group.n, prettyNum(sum(table(dummy$x, dummy$dep)),
                                    big.mark = ",",
                                    scientific = F))
    # add data for this subgroup to final data frame
    newdf <- data.frame(rbind(newdf,
                              cbind(grp = cnt,
                                    x = 1:len.x,
                                    y = y)))
  }
  # --------------------------------
  # if we want total line, repeat all for 
  # complete data frame
  # --------------------------------
  if (showTotal) {
    y <- prop.table(table(mydf$x, mydf$dep), margin = 1)
    if (len.y > ncol(y))
      y <- rep(0, nrow(y))
    else
      y <- y[, len.y]

    newdf <- data.frame(rbind(newdf,
                              cbind(grp = "Total",
                                    x = 1:len.x,
                                    y = y)))
    
    pval <- suppressWarnings(stats::chisq.test(table(mydf$x, mydf$dep))$p.value)
    stern <- c("")
    if (!is.na(pval)) {
      if (pval < 0.001) {
        stern <- c("***")
      } else if (pval < 0.01) {
        stern <- c("**")
      } else if (pval < 0.05) {
        stern <- c("*")
      }
    }
    group.p <- c(group.p, stern)
    group.n <- c(group.n, prettyNum(sum(table(mydf$x, mydf$dep)),
                                    big.mark = ",",
                                    scientific = F))
    
    axisLabels <- c(axisLabels, "Total")
  }
  # ------------------------------------
  # make group variables categorical
  # ------------------------------------
  newdf$grp <- sjmisc::to_factor(newdf$grp)
  newdf$x <- sjmisc::to_factor(newdf$x)
  # ------------------------------------
  # proportion needs to be numeric
  # ------------------------------------
  newdf$y <- sjmisc::to_value(newdf$y, keep.labels = F)
  # ------------------------------------
  # ------------------------------------
  if (showN)
    axisLabels <- paste0(axisLabels, " (n=", group.n, ")")
  if (showP)
    axisLabels <- paste0(axisLabels, " ", group.p)
  
  pal.len <- length(legendLabels)
  
  if (is.brewer.pal(geom.colors[1])) {
    geom.colors <- scales::brewer_pal(palette = geom.colors[1])(pal.len)
  } else if (geom.colors[1] == "gs") {
    geom.colors <- scales::grey_pal()(pal.len)
  } else if (length(geom.colors) > pal.len) {
    warning("More colors provided than needed. Shortening color palette.")
    geom.colors <- geom.colors[1:pal.len]
  }
  
  p <- ggplot(newdf, aes(x = rev(grp),
                         y = y,
                         colour = x,
                         shape = x)) +
    geom_point(size = geom.size, fill = shape.fill.color) +
    scale_y_continuous(labels = scales::percent) +
    scale_x_discrete(labels = rev(axisLabels)) +
    labs(x = axisTitle.x,
         y = axisTitle.y,
         title = title) +
    coord_flip() +
    scale_shape_manual(name = legendTitle, labels = legendLabels, values = shapes[1:pal.len]) +
    scale_colour_manual(name = legendTitle, labels = legendLabels, values = geom.colors)

  if (showTotal && annotateTotal)
    p <- p + annotate("rect", xmin = 0.5,  xmax = 1.5, ymin = -Inf, ymax = Inf, alpha = 0.15)
  
  if (printPlot) print(p)
  invisible(structure(list(plot = p,
                           df = mydf)))
}
