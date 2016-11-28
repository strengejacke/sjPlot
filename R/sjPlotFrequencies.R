# bind global variables
utils::globalVariables(c("val", "frq", "grp", "label.pos", "upper.ci", "lower.ci", "..density.."))


#' @title Plot frequencies of variables
#' @name sjp.frq
#' 
#' @seealso \itemize{
#'            \item \href{http://www.strengejacke.de/sjPlot/sjp.frq/}{sjPlot manual: sjp.frq}
#'            \item \code{\link{sjt.frq}}
#'          }
#' 
#' @description Plot frequencies of a variable as bar graph, histogram,
#'                box plot etc.
#' 
#' @note This function only works with variables with integer values (or numeric
#'         factor levels), i.e. scales / centred variables
#'         with decimals may result in unexpected behaviour.
#' 
#' @param sort.frq Determines whether categories should be sorted 
#'          according to their frequencies or not. Default is \code{"none"}, so 
#'          categories are not sorted by frequency. Use \code{"asc"} or
#'          \code{"desc"} for sorting categories ascending or descending order.
#' @param geom.colors user defined color for geoms, e.g. \code{geom.colors = "#0080ff"}.
#' @param errorbar.color color of confidence interval bars (error bars). 
#'          Only applies to \code{type = "bar"}. In case of dot plots, error bars 
#'          will have same colors as dots (see \code{geom.colors}).
#' @param show.mean logical, if \code{TRUE}, a vertical line in histograms 
#'          is drawn to indicate the mean value of the variables. Only 
#'          applies to histogram-charts.
#' @param show.mean.val logical, if \code{TRUE} (default), the mean value 
#'          is printed to the vertical line that indicates the variable's
#'          mean. Only applies to histogram-charts.
#' @param show.sd logical, if \code{TRUE}, the standard deviation 
#'          is annotated as shaded rectangle around the mean intercept
#'          line. Only applies to histogram-charts.
#' @param mean.line.type numeric value, indicating the linetype of the mean 
#'          intercept line. Only applies to histogram-charts and 
#'          when \code{show.mean = TRUE}.
#' @param mean.line.size numeric, size of the mean intercept line. Only 
#'          applies to histogram-charts and when \code{show.mean = TRUE}.
#' @param normal.curve logical, if \code{TRUE}, a normal curve, which is adjusted to the data,
#'          is plotted over the histogram or density plot. Default is
#'          \code{FALSE}. Only applies when histograms or density plots are plotted (see \code{type}).
#' @param normal.curve.color color of the normal curve line. Only
#'          applies if \code{normal.curve = TRUE}.
#' @param normal.curve.size numeric, size of the normal curve line. Only
#'          applies if \code{normal.curve = TRUE}.
#' @param normal.curve.alpha transparancy level (alpha value) of the normal curve. Only
#'          applies if \code{normal.curve = TRUE}.
#' @param xlim numeric vector of length two, defining lower and upper axis limits
#'          of the x scale. By default, this argument is set to \code{NULL}, i.e. the 
#'          x-axis fits to the required range of the data.
#'          
#' @inheritParams sjp.grpfrq
#' @inheritParams sjp.lm
#' @inheritParams sjp.glmer
#' 
#' @return (Insisibily) returns the ggplot-object with the complete plot (\code{plot}) as well as the data frame that
#'           was used for setting up the ggplot-object (\code{data}).
#' 
#' @examples
#' # boxplot
#' sjp.frq(ChickWeight$weight, type = "box")
#' 
#' # histogram
#' sjp.frq(discoveries, type = "hist", show.mean = TRUE)
#'         
#' # violin plot
#' sjp.frq(ChickWeight$weight, type = "v")
#' 
#' # bar plot
#' sjp.frq(ChickWeight$Diet)
#' 
#' 
#' # bar plot with EUROFAMCARE sample dataset
#' # dataset was importet from an SPSS-file, using:
#' # efc <- sjmisc::read_spss("efc.sav", enc = "UTF-8")
#' library(sjmisc)
#' data(efc)
#' # you may use sjp.setTheme here to change axis textangle
#' sjp.frq(efc$e15relat)
#' 
#' # bar plot with EUROFAMCARE sample dataset
#' # grouped variable
#' ageGrp <- group_var(efc$e17age)
#' ageGrpLab <- group_labels(efc$e17age)
#' sjp.frq(ageGrp, title = get_label(efc$e17age), axis.labels = ageGrpLab)
#' 
#' # negative impact scale, ranging from 7-28
#' sjp.frq(efc$neg_c_7)
#' 
#' # plotting confidence intervals. expand grid and v/hjust for text labels
#' sjp.frq(efc$e15relat, type = "dot", show.ci = TRUE, sort.frq = "desc",
#'         coord.flip = TRUE, expand.grid = TRUE, vjust = "bottom", 
#'         hjust = "left")
#' 
#' # Simulate ggplot-default histogram
#' sjp.frq(efc$c160age, type = "h", geom.size = 3)
#' 
#' # histogram with overlayed normal curve
#' sjp.frq(efc$c160age, type = "h", show.mean = TRUE, show.mean.val = TRUE,
#'         normal.curve = TRUE, show.sd = TRUE, normal.curve.color = "blue",
#'         normal.curve.size = 3, ylim = c(0,50))
#' 
#' @import ggplot2
#' @importFrom sjstats wtd_sd
#' @importFrom sjmisc set_labels group_labels group_var to_value
#' @importFrom stats na.omit sd weighted.mean
#' @export
sjp.frq <- function(var.cnt,
                    title = "",
                    weight.by = NULL,
                    title.wtd.suffix = NULL,
                    sort.frq = c("none", "asc", "desc"),
                    type = c("bar", "dot", "histogram", "line", "density", "boxplot", "violin"),
                    geom.size = NULL,
                    geom.colors = "#336699",
                    errorbar.color = "darkred",
                    axis.title = NULL,
                    axis.labels = NULL,
                    xlim = NULL,
                    ylim = NULL,
                    wrap.title = 50,
                    wrap.labels = 20,
                    grid.breaks = NULL,
                    expand.grid = FALSE,
                    show.values = TRUE,
                    show.n = TRUE,
                    show.prc = TRUE,
                    show.axis.values = TRUE,
                    show.ci = FALSE,
                    show.na = FALSE,
                    show.mean = FALSE,
                    show.mean.val = TRUE,
                    show.sd = TRUE,
                    mean.line.type = 2,
                    mean.line.size = 0.5,
                    inner.box.width = 0.15,
                    inner.box.dotsize = 3,
                    normal.curve = FALSE,
                    normal.curve.color = "red",
                    normal.curve.size = 0.8,
                    normal.curve.alpha = 0.4,
                    auto.group = NULL,
                    coord.flip = FALSE,
                    vjust = "bottom",
                    hjust = "center",
                    y.offset = NULL,
                    prnt.plot = TRUE) {
  
  # get variable name, used as default label if variable
  # has no label attributes
  var.name <- get_var_name(deparse(substitute(var.cnt)))
  
  # try to find some useful default offsets for textlabels,
  # depending on plot range and flipped coordinates
  if (is.null(y.offset)) {
    # get maximum y-pos
    y.offset <- ceiling(max(table(var.cnt)) / 100)
    if (coord.flip) {
      if (missing(vjust)) vjust <- "center"
      if (missing(hjust)) hjust <- "bottom"
      if (hjust == "bottom")
        y_offset <- y.offset
      else if (hjust == "top")
        y_offset <- -y.offset
      else
        y_offset <- 0
    } else {
      if (vjust == "bottom")
        y_offset <- y.offset
      else if (vjust == "top")
        y_offset <- -y.offset
      else
        y_offset <- 0
    }
  } else {
    y_offset <- y.offset
  }
  
  # try to automatically set labels, if not passed as argument -----
  # to make plot annotations more beautiful, supporting labelled data
  if (is.null(axis.labels)) {
    axis.labels <- sjmisc::get_labels(var.cnt, attr.only = F, include.values = NULL, 
                                      include.non.labelled = T)
  }
  if (is.null(axis.title)) axis.title <- sjmisc::get_label(var.cnt, def.value = var.name)
  if (is.null(title)) title <- sjmisc::get_label(var.cnt, def.value = var.name)
  
  # remove titles if empty
  if (!is.null(axis.title) && axis.title == "") axis.title <- NULL
  if (!is.null(title) && title == "") title <- NULL    
  
  # check color argument
  if (length(geom.colors) > 1) geom.colors <- geom.colors[1]

  # Match arguments -----
  type <- match.arg(type)
  sort.frq <- match.arg(sort.frq)
  
  # default grid-expansion
  if (isTRUE(expand.grid) || (missing(expand.grid) && type == "histogram")) {
    expand.grid <- ggplot2::waiver()
  } else {
    expand.grid <- c(0, 0)
  }

  # for histograms or density plots...
  xv <- sjmisc::to_value(stats::na.omit(var.cnt))
  # check for nice bin-width defaults
  if (type %in% c("histogram", "density") && 
      !is.null(geom.size) && 
      geom.size < round(diff(range(xv)) / 40))
    message("Using very small binwidth. Consider adjusting `geom.size` argument.")
  # create second data frame
  hist.dat <- data.frame(xv)
  
  # check default geom.size -----
  if (is.null(geom.size)) {
    geom.size <- dplyr::case_when(
      type == "bar" ~ .7,
      type == "dot" ~ 2.5,
      type == "density" ~ ceiling(diff(range(xv)) / 40),
      type == "histogram" ~ ceiling(diff(range(xv)) / 40),
      type == "line" ~ .8,
      type == "boxplot" ~ .3,
      type == "violin" ~ .3,
      TRUE ~ .7
    )
  }
  
  # check whether variable should be auto-grouped -----
  if (!is.null(auto.group) && length(unique(var.cnt)) >= auto.group) {
    message(sprintf("`%s` has %i unique values and was grouped...", 
                    var.name, 
                    length(unique(var.cnt))))
    # group axis labels
    axis.labels <- sjmisc::group_labels(sjmisc::to_value(var.cnt, keep.labels = F),
                                        groupsize = "auto", 
                                        groupcount = auto.group)
    # group variable
    var.cnt <- sjmisc::group_var(sjmisc::to_value(var.cnt, keep.labels = F), 
                                 groupsize = "auto", 
                                 as.num = TRUE, 
                                 groupcount = auto.group)
    # set label attributes
    sjmisc::set_labels(var.cnt) <- axis.labels
  }
  
  # create frequency data frame -----
  df.frq <- create.frq.df(var.cnt, 
                          wrap.labels = wrap.labels, 
                          order.frq = sort.frq, 
                          round.prz = 2,
                          na.rm = !show.na, 
                          weight.by = weight.by)
  mydat <- df.frq$mydat
  # any labels detected?
  if (!is.null(df.frq$labels) && is.null(axis.labels)) 
    axis.labels <- df.frq$labels
  else if (!is.null(axis.labels) && sort.frq != "none")
    # sort labels in required order
    axis.labels <- axis.labels[mydat$order]
  
  # define text label position
  if (show.ci)
    mydat$label.pos <- mydat$upper.ci
  else
    mydat$label.pos <- mydat$frq
  
  # Trim labels and title to appropriate size -----
  # check length of diagram title and split longer string into new lines
  # every 50 chars
  if (!is.null(title)) {
    # if we have weighted values, say that in diagram's title
    if (!is.null(title.wtd.suffix)) title <- paste(title, title.wtd.suffix, sep = "")
    title <- sjmisc::word_wrap(title, wrap.title)    
  }
  # check length of x-axis title and split longer string into new lines
  # every 50 chars
  if (!is.null(axis.title)) axis.title <- sjmisc::word_wrap(axis.title, wrap.title)    
  
  # count variable may not be a factor!
  if (is.factor(var.cnt) || is.character(var.cnt)) {
    var.cnt <- sjmisc::to_value(var.cnt, keep.labels = F)
  }
  
  # If we have a histogram, caluclate means of groups
  if (is.null(weight.by)) {
    mittelwert <- mean(var.cnt, na.rm = TRUE)
    stddev <- stats::sd(var.cnt, na.rm = TRUE)
  } else {
    mittelwert <- stats::weighted.mean(var.cnt, weight.by, na.rm = TRUE)
    stddev <- sjstats::wtd_sd(var.cnt, weights = weight.by)
  }
  
  # If we have boxplots, use different data frame structure
  if (type == "boxplot" || type == "violin") {
    mydat <- stats::na.omit(data.frame(cbind(grp = 1, 
                                             frq = var.cnt, 
                                             val = var.cnt)))
    mydat$grp <- as.factor(mydat$grp)
  }  
  
  # Prepare bar charts
  trimViolin <- FALSE
  lower_lim <- 0
  # calculate upper y-axis-range
  # if we have a fixed value, use this one here
  if (!is.null(ylim) && length(ylim) == 2) {
    lower_lim <- ylim[1]
    upper_lim <- ylim[2]
  } else {
    # if we have boxplots, we have different ranges, so we can adjust
    # the y axis
    if (type == "boxplot" || type == "violin") {
      # use an extra standard-deviation as limits for the y-axis when we have boxplots
      lower_lim <- min(var.cnt, na.rm = TRUE) - floor(stats::sd(var.cnt, na.rm = TRUE))
      upper_lim <- max(var.cnt, na.rm = TRUE) + ceiling(stats::sd(var.cnt, na.rm = TRUE))
      # make sure that the y-axis is not below zero
      if (lower_lim < 0) {
        lower_lim <- 0
        trimViolin <- TRUE
      }
    } else if (type == "histogram") {
      # what is the maximum values after binning for histograms?
      hist.grp.cnt <- ceiling(diff(range(var.cnt, na.rm = T)) / geom.size)
      # ... or the amount of max. answers per category
      # add 10% margin to upper limit
      upper_lim <- max(pretty(table(sjmisc::group_var(var.cnt, 
                                                      groupsize = "auto", 
                                                      groupcount = hist.grp.cnt)) * 1.1))
    } else {
      if (show.ci)
        upper_lim <- max(pretty(mydat$upper.ci * 1.1))
      else
        upper_lim <- max(pretty(table(var.cnt) * 1.1))
    }
  }
  
  # If we want to include NA, use raw percentages as valid percentages
  if (show.na) mydat$valid.prc <- mydat$raw.prc
  
  # don't display value labels when we have boxplots or violin plots
  if (type == "boxplot" || type == "violin") show.values <- FALSE
  if (show.values) {
    # here we have counts and percentages
    if (show.prc && show.n) {
      if (coord.flip) {
        ggvaluelabels <-  geom_text(label = sprintf("%i (%.01f%%)", mydat$frq, mydat$valid.prc),
                                    hjust = hjust,
                                    vjust = vjust,
                                    aes(y = label.pos + y_offset))
      } else {
        ggvaluelabels <-  geom_text(label = sprintf("%i\n(%.01f%%)", mydat$frq, mydat$valid.prc),
                                    hjust = hjust,
                                    vjust = vjust,
                                    aes(y = label.pos + y_offset))
      }
    } else if (show.n) {
      # here we have counts, without percentages
      ggvaluelabels <-  geom_text(label = sprintf("%i", mydat$frq),
                                  hjust = hjust,
                                  vjust = vjust,
                                  aes(y = label.pos + y_offset))
    } else if (show.prc) {
      # here we have counts, without percentages
      ggvaluelabels <-  geom_text(label = sprintf("%.01f%%", mydat$valid.prc),
                                  hjust = hjust,
                                  vjust = vjust,
                                  aes(y = label.pos + y_offset))
    } else {
      # no labels
      ggvaluelabels <-  geom_text(aes(y = frq), label = "")
    }
  } else {
    # no labels
    ggvaluelabels <-  geom_text(aes(y = frq), label = "")
  }
  
  # Set up grid breaks
  maxx <- max(mydat$val) + 1
  if (is.null(grid.breaks)) {
    gridbreaks <- ggplot2::waiver()
    histgridbreaks <- ggplot2::waiver()
  } else {
    gridbreaks <- c(seq(lower_lim, upper_lim, by = grid.breaks))
    histgridbreaks <- c(seq(lower_lim, maxx, by = grid.breaks))
  }
  
  # set Y-axis, depending on the calculated upper y-range.
  # It either corresponds to the maximum amount of cases in the data set
  # (length of var) or to the highest count of var's categories.
  if (show.axis.values) {
    yscale <- scale_y_continuous(limits = c(lower_lim, upper_lim), 
                                 expand = expand.grid, 
                                 breaks = gridbreaks)
  } else {
    yscale <- scale_y_continuous(limits = c(lower_lim, upper_lim), 
                                 expand = expand.grid, 
                                 breaks = gridbreaks, 
                                 labels = NULL)
  }
  
  # bar and dot plot start here! -----
  if (type == "bar" || type == "dot") {
    # define geom
    if (type == "bar") {
      geob <- geom_bar(stat = "identity", width = geom.size, fill = geom.colors)
    } else if (type == "dot") {
      geob <- geom_point(size = geom.size, colour = geom.colors)
    }
    # mydat is a data frame that only contains one variable (var).
    # Must be declared as factor, so the bars are central aligned to
    # each x-axis-break. 
    baseplot <- ggplot(mydat, aes(x = factor(val), y = frq)) + 
      geob +
      yscale + 
      # remove guide / legend
      guides(fill = FALSE) +
      # show absolute and percentage value of each bar.
      ggvaluelabels +
      # print value labels to the x-axis.
      # If argument "axis.labels" is NULL, the category numbers (1 to ...) 
      # appear on the x-axis
      scale_x_discrete(labels = axis.labels)
    if (show.ci) {
      ebcol <- ifelse(type == "dot", geom.colors, errorbar.color)
      # print confidence intervalls (error bars)
      baseplot <- baseplot + 
        geom_errorbar(aes(ymin = lower.ci, ymax = upper.ci), colour = ebcol, width = 0)
    }
    # check whether coordinates should be flipped, i.e.
    # swap x and y axis
    if (coord.flip) baseplot <- baseplot + coord_flip()
  
  # Start box plot here -----
  } else if (type == "boxplot" || type == "violin") {
    # setup base plot
    baseplot <- ggplot(mydat, aes(x = grp, y = frq))
    # and x-axis
    scalex <- scale_x_discrete(labels = "")
    if (type == "boxplot") {
      baseplot <- baseplot + 
        geom_boxplot(width = geom.size, fill = geom.colors)
    } else {
      baseplot <- baseplot + 
        geom_violin(trim = trimViolin, width = geom.size, fill = geom.colors) +
        # if we have a violin plot, add an additional boxplot inside to show
        # more information
        geom_boxplot(width = inner.box.width, fill = "white")
    }
    # if we have boxplots or violon plots, also add a point that indicates
    # the mean value
    # different fill colours, because violin boxplots have white background
    fcsp <- ifelse(type == "boxplot", "white", "black")
    baseplot <- baseplot +
      stat_summary(fun.y = "mean", geom = "point", shape = 21, 
                   size = inner.box.dotsize, fill = fcsp)
    # no additional labels for the x- and y-axis, only diagram title
    baseplot <- baseplot + yscale + scalex
  # --------------------------------------------------
  # Start density plot here
  # --------------------------------------------------
  } else if (type == "density") {
    # First, plot histogram with density curve
    baseplot <- ggplot(hist.dat, aes(x = xv)) +
      geom_histogram(aes(y = ..density..), binwidth = geom.size, fill = geom.colors) +
      # transparent density curve above bars
      geom_density(aes(y = ..density..), fill = "cornsilk", alpha = 0.3) +
      # remove margins from left and right diagram side
      scale_x_continuous(expand = expand.grid, breaks = histgridbreaks, limits = xlim)
    # check whether user wants to overlay the histogram
    # with a normal curve
    if (normal.curve) {
      baseplot <- baseplot +
        stat_function(fun = dnorm,
                      args = list(mean = mean(hist.dat$xv),
                                  sd = stats::sd(hist.dat$xv)),
                      colour = normal.curve.color,
                      size = normal.curve.size,
                      alpha = normal.curve.alpha)
    }
  } else {
    # -----------------------------------------------------------------
    # Since the density curve shows no absolute numbers (counts) on the
    # y-axis, have also the opportunity to plot "real" histrograms with 
    # counts on the y-axis
    # -----------------------------------------------------------------
    if (type == "histogram") {
      # original data needed for normal curve
      baseplot <- ggplot(mydat) +
        # second data frame mapped to the histogram geom
        geom_histogram(data = hist.dat, aes(x = xv), binwidth = geom.size, fill = geom.colors)
    } else {
      baseplot <- ggplot(mydat, aes(x = val, y = frq)) +
        geom_area(alpha = 0.3) +
        geom_line(size = geom.size, colour = geom.colors)
        ggvaluelabels
    }
    # check whether user wants to overlay the histogram
    # with a normal curve
    if (normal.curve) {
      baseplot <- baseplot +
        stat_function(fun = function(xx, mean, sd, n) { n * dnorm(x = xx, mean = mean, sd = sd) },
                      args = with(mydat, c(mean = mittelwert, sd = stddev, n = length(var.cnt))),
                      colour = normal.curve.color,
                      size = normal.curve.size,
                      alpha = normal.curve.alpha)
    }
    # if we have a histogram, add mean-lines
    if (show.mean) {
      baseplot <- baseplot + 
        # vertical lines indicating the mean
        geom_vline(xintercept = mittelwert, linetype = mean.line.type, size = mean.line.size)
      # check whether meanvalue should be shown.
      if (show.mean.val) {
        baseplot <- baseplot + 
          # use annotation instead of geomtext, because we need mean value only printed once
          annotate("text", 
                   x = mittelwert, 
                   y = upper_lim, 
                   parse = TRUE, 
                   label = paste("italic(bar(x)) == ", round(mittelwert, 1),
                                 "~~italic(s) == ", round(stddev, 1)),
                   vjust = "top",
                   hjust = "top")
      }
      # check whether the user wants to plot standard deviation area
      if (show.sd) {
        baseplot <- baseplot +
          # first draw shaded rectangle. these are by default in grey colour with very high transparancy
          annotate("rect", 
                   xmin = mittelwert - stddev, 
                   xmax = mittelwert + stddev, 
                   ymin = 0, 
                   ymax = c(upper_lim), 
                   fill = "grey70", 
                   alpha = 0.2) +
          # draw border-lines for shaded rectangle
          geom_vline(xintercept = mittelwert - stddev, 
                     linetype = 3, 
                     size = mean.line.size, 
                     alpha = 0.7) +
          geom_vline(xintercept = mittelwert + stddev, 
                     linetype = 3, 
                     size = mean.line.size, 
                     alpha = 0.7)
      }
    }
    # show absolute and percentage value of each bar.
    baseplot <- baseplot + yscale +
      # continuous x-scale for histograms
      scale_x_continuous(limits = xlim, expand = expand.grid, breaks = histgridbreaks)
  }
  # set axes text and 
  baseplot <- baseplot + labs(title = title, x = axis.title, y = NULL)
  # ---------------------------------------------------------
  # Check whether ggplot object should be returned or plotted
  # ---------------------------------------------------------
  if (prnt.plot) graphics::plot(baseplot)
  # -------------------------------------
  # return results
  # -------------------------------------
  invisible(structure(class = "sjpfrq",
                      list(plot = baseplot,
                           data = mydat)))
}
