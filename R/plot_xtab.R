#' @title Plot contingency tables
#' @name plot_xtab
#'
#' @description Plot proportional crosstables (contingency tables) of two variables as ggplot diagram.
#'
#' @param x A vector of values (variable) describing the bars which make up the plot.
#' @param grp Grouping variable of same length as \code{x}, where \code{x}
#'          is grouped into the categories represented by \code{grp}.
#' @param type Plot type. may be either \code{"bar"} (default) for bar charts,
#'          or \code{"line"} for line diagram.
#' @param margin Indicates which data of the proportional table should be plotted. Use \code{"row"} for
#'          calculating row percentages, \code{"col"} for column percentages and \code{"cell"} for cell percentages.
#'          If \code{margin = "col"}, an additional bar with the total sum of each column
#'          can be added to the plot (see \code{show.total}).
#' @param rev.order Logical, if \code{TRUE}, order of categories (groups) is reversed.
#' @param dot.size Dot size, only applies, when argument \code{type = "line"}.
#' @param string.total String for the legend label when a total-column is added. Only applies
#'          if \code{show.total = TRUE}. Default is \code{"Total"}.
#' @param show.total When \code{margin = "col"}, an additional bar
#'          with the sum within each category and it's percentages will be added
#'          to each category.
#'
#' @inheritParams plot_grpfrq
#'
#' @return A ggplot-object.
#'
#' @examples
#' # create 4-category-items
#' grp <- sample(1:4, 100, replace = TRUE)
#' # create 3-category-items
#' x <- sample(1:3, 100, replace = TRUE)
#'
#' # plot "cross tablulation" of x and grp
#' plot_xtab(x, grp)
#'
#' # plot "cross tablulation" of x and y, including labels
#' plot_xtab(x, grp, axis.labels = c("low", "mid", "high"),
#'          legend.labels = c("Grp 1", "Grp 2", "Grp 3", "Grp 4"))
#'
#' # plot "cross tablulation" of x and grp
#' # as stacked proportional bars
#' plot_xtab(x, grp, margin = "row", bar.pos = "stack",
#'          show.summary = TRUE, coord.flip = TRUE)
#'
#' # example with vertical labels
#' library(sjmisc)
#' library(sjlabelled)
#' data(efc)
#' set_theme(geom.label.angle = 90)
#' plot_xtab(efc$e42dep, efc$e16sex, vjust = "center", hjust = "bottom")
#'
#' # grouped bars with EUROFAMCARE sample dataset
#' # dataset was importet from an SPSS-file,
#' # see ?sjmisc::read_spss
#' data(efc)
#' efc.val <- get_labels(efc)
#' efc.var <- get_label(efc)
#'
#' plot_xtab(efc$e42dep, efc$e16sex, title = efc.var['e42dep'],
#'          axis.labels = efc.val[['e42dep']], legend.title = efc.var['e16sex'],
#'          legend.labels = efc.val[['e16sex']])
#'
#' plot_xtab(efc$e16sex, efc$e42dep, title = efc.var['e16sex'],
#'          axis.labels = efc.val[['e16sex']], legend.title = efc.var['e42dep'],
#'          legend.labels = efc.val[['e42dep']])
#'
#' # -------------------------------
#' # auto-detection of labels works here
#' # so no need to specify labels. For
#' # title-auto-detection, use NULL
#' # -------------------------------
#' plot_xtab(efc$e16sex, efc$e42dep, title = NULL)
#'
#' plot_xtab(efc$e16sex, efc$e42dep, margin = "row",
#'          bar.pos = "stack", coord.flip = TRUE)
#'
#' @import ggplot2
#' @importFrom dplyr group_by mutate arrange filter select summarize
#' @importFrom scales percent
#' @importFrom stats na.omit
#' @export
plot_xtab <- function(x,
                     grp,
                     type = c("bar", "line"),
                     margin = c("col", "cell", "row"),
                     bar.pos = c("dodge", "stack"),
                     title = "",
                     title.wtd.suffix = NULL,
                     axis.titles = NULL,
                     axis.labels = NULL,
                     legend.title = NULL,
                     legend.labels = NULL,
                     weight.by = NULL,
                     rev.order = FALSE,
                     show.values = TRUE,
                     show.n = TRUE,
                     show.prc = TRUE,
                     show.total = TRUE,
                     show.legend = TRUE,
                     show.summary = FALSE,
                     summary.pos = "r",
                     drop.empty = TRUE,
                     string.total = "Total",
                     wrap.title = 50,
                     wrap.labels = 15,
                     wrap.legend.title = 20,
                     wrap.legend.labels = 20,
                     geom.size = 0.7,
                     geom.spacing = 0.1,
                     geom.colors = "Paired",
                     dot.size = 3,
                     smooth.lines = FALSE,
                     grid.breaks = 0.2,
                     expand.grid = FALSE,
                     ylim = NULL,
                     vjust = "bottom",
                     hjust = "center",
                     y.offset = NULL,
                     coord.flip = FALSE) {
  # --------------------------------------------------------
  # get variable name
  # --------------------------------------------------------
  var.name.cnt <- get_var_name(deparse(substitute(x)))
  var.name.grp <- get_var_name(deparse(substitute(grp)))
  # --------------------------------------------------------
  # match arguments
  # --------------------------------------------------------
  bar.pos <- match.arg(bar.pos)
  type <- match.arg(type)
  margin <- match.arg(margin)

  # remove empty value-labels
  if (drop.empty) {
    x <- sjlabelled::drop_labels(x)
    grp <- sjlabelled::drop_labels(grp)
  }

  # --------------------------------------------------------
  # copy titles
  # --------------------------------------------------------
  if (is.null(axis.titles)) {
    axisTitle.x <- NULL
    axisTitle.y <- NULL
  } else {
    axisTitle.x <- axis.titles[1]
    if (length(axis.titles) > 1)
      axisTitle.y <- axis.titles[2]
    else
      axisTitle.y <- NULL
  }
  # --------------------------------------------------------
  # grid-expansion
  # --------------------------------------------------------
  if (expand.grid) {
    expand.grid <- waiver()
  } else {
    expand.grid <- c(0, 0)
  }
  # --------------------------------------------------------
  # set text label offset
  # --------------------------------------------------------
  if (is.null(y.offset)) {
    # stacked bars?
    if (bar.pos == "stack") {
      y_offset <- 0
    } else {
      y.offset <- .005
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
    }
  } else {
    y_offset <- y.offset
  }
  # --------------------------------------------------------
  # total column only applies to column percentages
  # --------------------------------------------------------
  if (margin != "col") show.total <- FALSE

  # need to set this to FALSE
  if (!show.n && !show.prc) show.values <- F
  # --------------------------------------------------------
  # create cross table of frequencies and percentages
  # --------------------------------------------------------
  mydat <- create.xtab.df(x, grp, round.prz = 2, na.rm = T, weight.by = weight.by)
  # --------------------------------------------------------
  # x-position as numeric factor, added later after
  # tidying
  # --------------------------------------------------------
  bars.xpos <- seq_len(nrow(mydat$mydat))
  # --------------------------------------------------------
  # try to automatically set labels is not passed as argument
  # --------------------------------------------------------
  if (is.null(axis.labels)) axis.labels <- mydat$labels.cnt
  if (is.null(legend.labels)) legend.labels <- mydat$labels.grp
  if (is.null(axisTitle.x)) axisTitle.x <- sjlabelled::get_label(x, def.value = var.name.cnt)
  if (is.null(legend.title)) legend.title <- sjlabelled::get_label(grp, def.value = var.name.grp)
  if (is.null(title)) {
    t1 <- sjlabelled::get_label(x, def.value = var.name.cnt)
    t2 <- sjlabelled::get_label(grp, def.value = var.name.grp)
    if (!is.null(t1) && !is.null(t2)) title <- paste0(t1, " by ", t2)
  }
  # --------------------------------------------------------
  # remove titles if empty
  # --------------------------------------------------------
  if (!is.null(legend.title) && legend.title == "") legend.title <- NULL
  if (!is.null(axisTitle.x) && axisTitle.x == "") axisTitle.x <- NULL
  if (!is.null(axisTitle.y) && axisTitle.y == "") axisTitle.y <- NULL
  if (!is.null(title) && title == "") title <- NULL
  # --------------------------------------------------------
  # Check if user wants to add total column, and if so,
  # define amount of categories
  # --------------------------------------------------------
  if (show.total) legend.labels <- c(legend.labels, string.total)
  grpcount <- length(legend.labels)
  # -----------------------------------------------
  # check whether row, column or cell percentages are requested
  #---------------------------------------------------
  if (margin == "cell")
    myptab <- mydat$proptab.cell
  else if (margin == "col")
    myptab <- mydat$proptab.col
  else if (margin == "row")
    myptab <- mydat$proptab.row
  myptab <- rownames_as_column(data.frame(myptab))
  # -----------------------------------------------
  # tidy data
  #---------------------------------------------------
  mydf <- .gather(myptab, names_to = "group", values_to = "prc", columns = 2:(grpcount + 1))
  mydf$group <- factor(mydf$group, levels = unique(mydf$group))
  # -----------------------------------------------
  # add total column and row to n-values
  #---------------------------------------------------
  if (margin != "row")
    mydat$mydat$total <- unname(rowSums(mydat$mydat[, -1]))
  if (margin != "col")
    mydat$mydat <- rbind(mydat$mydat, c("total", unname(colSums(mydat$mydat[, -1]))))
  # -----------------------------------------------
  # add n-values to tidy data frame
  #---------------------------------------------------
  dummydf <- .gather(mydat$mydat, names_to = "group", values_to = "n", columns = 2:(grpcount + 1))
  mydf$n <- as.numeric(dummydf$n)
  # -----------------------------------------------
  # remove total for row and column index
  #---------------------------------------------------
  if (margin != "col") mydf <- dplyr::filter(mydf, .data$rowname != "total")
  if (margin == "cell") mydf <- dplyr::select(mydf, -.data$total)
  # --------------------------------------------------------
  # add xpos now
  # --------------------------------------------------------
  mydf$xpos <- as.factor(as.numeric(bars.xpos))
  # --------------------------------------------------------
  # add half of Percentage values as new y-position for stacked bars
  # --------------------------------------------------------
  mydf <- mydf %>%
    dplyr::group_by(.data$xpos) %>%
    dplyr::mutate(ypos = cumsum(.data$prc) - 0.5 * .data$prc) %>%
    dplyr::arrange(.data$group)
  # --------------------------------------------------------
  # add line-break char
  # --------------------------------------------------------
  if (show.prc && show.n) {
    mydf$line.break <- ifelse(isTRUE(coord.flip), ' ', '\n')
  } else {
    mydf$line.break <- ""
  }
  # --------------------------------------------------------
  # define label position for dodged bars
  # --------------------------------------------------------
  if (bar.pos == "dodge") mydf$ypos <- mydf$prc
  # --------------------------------------------------------
  # finally, percentage values need to be between 0 and 1
  # --------------------------------------------------------
  mydf$prc <- mydf$prc / 100
  mydf$ypos <- mydf$ypos / 100
  # --------------------------------------------------------
  # Prepare and trim legend labels to appropriate size
  # --------------------------------------------------------
  if (!is.null(legend.labels)) legend.labels <- sjmisc::word_wrap(legend.labels, wrap.legend.labels)
  if (!is.null(legend.title)) legend.title <- sjmisc::word_wrap(legend.title, wrap.legend.title)
  if (!is.null(title)) {
    # if we have weighted values, say that in diagram's title
    if (!is.null(title.wtd.suffix)) title <- paste(title, title.wtd.suffix, sep = "")
    title <- sjmisc::word_wrap(title, wrap.title)
  }
  if (!is.null(axisTitle.x)) axisTitle.x <- sjmisc::word_wrap(axisTitle.x, wrap.title)
  if (!is.null(axisTitle.y)) axisTitle.y <- sjmisc::word_wrap(axisTitle.y, wrap.title)
  if (!is.null(axis.labels)) axis.labels <- sjmisc::word_wrap(axis.labels, wrap.labels)
  # ----------------------------
  # create expression with model summarys. used
  # for plotting in the diagram later
  # ----------------------------
  if (show.summary) {
    modsum <- crosstabsum(x, grp, weight.by)
  } else {
    modsum <- NULL
  }
  # --------------------------------------------------------
  # Prepare bar charts
  # --------------------------------------------------------
  # calculate upper y-axis-range
  # if we have a fixed value, use this one here
  lower_lim <- 0
  # calculate upper y-axis-range
  # if we have a fixed value, use this one here
  if (!is.null(ylim) && length(ylim) == 2) {
    lower_lim <- ylim[1]
    upper_lim <- ylim[2]
  } else if (bar.pos == "stack") {
    # check upper limits. we may have rounding errors, so values
    # sum up to more than 100%
    ul <- max(mydf %>%
                dplyr::group_by(.data$rowname) %>%
                dplyr::summarize(ges = sum(.data$prc)) %>%
                dplyr::select(.data$ges), na.rm = T)
    if (ul > 1L)
      upper_lim <- ul
    else
      upper_lim <- 1
  } else {
    # factor depends on labels
    if (show.values)
      mlp <- 1.2
    else
      mlp <- 1.1
    # else calculate upper y-axis-range depending
    # on the amount of max. answers per category
    upper_lim <- max(mydf$prc) * mlp
  }
  # --------------------------------------------------------
  # check if category-oder on x-axis should be reversed
  # change category label order then
  # --------------------------------------------------------
  if (rev.order) {
    axis.labels <- rev(axis.labels)
    mydf$xpos <- rev(mydf$xpos)
  }
  # --------------------------------------------------------
  # align dodged position of labels to bar positions
  # --------------------------------------------------------
  posdodge <- ifelse(type == "line", 0, geom.size + geom.spacing)
  # --------------------------------------------------------
  # Set value labels
  # --------------------------------------------------------
  if (show.values) {
    # if we have dodged bars or dots, we have to use a slightly dodged position for labels
    # as well, sofor better reading
    if (bar.pos == "dodge") {
      if (show.prc && show.n) {
        ggvaluelabels <- geom_text(aes(y = .data$ypos + y_offset, label = sprintf("%.01f%%%s(n=%i)", 100 * .data$prc, .data$line.break, .data$n)),
                                   position = position_dodge(posdodge),
                                   vjust = vjust, hjust = hjust)
      } else if (show.prc) {
        ggvaluelabels <- geom_text(aes(y = .data$ypos + y_offset, label = sprintf("%.01f%%", 100 * .data$prc)),
                                   position = position_dodge(posdodge),
                                   vjust = vjust, hjust = hjust)
      } else if (show.n) {
        ggvaluelabels <- geom_text(aes(y = .data$ypos + y_offset, label = sprintf("n=%i", .data$n)),
                                   position = position_dodge(posdodge),
                                   vjust = vjust, hjust = hjust)
      }
    } else {
      if (show.prc && show.n) {
        ggvaluelabels <- geom_text(aes(y = .data$ypos, label = sprintf("%.01f%%%s(n=%i)", 100 * .data$prc, .data$line.break, .data$n)),
                                   vjust = vjust, hjust = hjust)
      } else if (show.prc) {
        ggvaluelabels <- geom_text(aes(y = .data$ypos, label = sprintf("%.01f%%", 100 * .data$prc)),
                                   vjust = vjust, hjust = hjust)
      } else if (show.n) {
        ggvaluelabels <- geom_text(aes(y = .data$ypos, label = sprintf("n=%i", .data$n)),
                                   vjust = vjust, hjust = hjust)
      }
    }
  } else {
    ggvaluelabels <- geom_text(aes_string(y = "ypos"), label = "")
  }
  # --------------------------------------------------------
  # Set up grid breaks
  # --------------------------------------------------------
  if (is.null(grid.breaks)) {
    gridbreaks <- waiver()
  } else {
    gridbreaks <- seq(lower_lim, upper_lim, by = grid.breaks)
  }
  # ----------------------------------
  # construct final plot, base constructor
  # first, set x scale
  # ----------------------------------
  if (type == "line")
    scalex <- scale_x_continuous(labels = axis.labels)
  else
    scalex <- scale_x_discrete(labels = axis.labels)
  # ----------------------------------
  # check whether bars or lines should be printed
  # ----------------------------------
  if (type == "bar") {
    if (bar.pos == "dodge") {
      geob <- geom_bar(stat = "identity",
                       position = position_dodge(posdodge),
                       width = geom.size)
    } else {
      geob <- geom_bar(stat = "identity",
                       position = position_stack(reverse = TRUE),
                       width = geom.size)
    }
  # check if we have lines
  } else if (type == "line") {
    # for lines, numeric scale
    mydf$xpos <- sjlabelled::as_numeric(mydf$xpos, keep.labels = F)
    line.stat <- ifelse(isTRUE(smooth.lines), "smooth", "identity")
    geob <- geom_line(aes_string(colour = "group"), linewidth = geom.size, stat = line.stat)
  }
  # --------------------------------------------------------
  # start plot here
  # --------------------------------------------------------
  baseplot <- ggplot(mydf, aes_string(x = "xpos", y = "prc", fill = "group")) + geob
  # if we have line diagram, print lines here
  if (type == "line") {
    baseplot <- baseplot +
      geom_point(size = dot.size, shape = 21, show.legend = FALSE)
  }
  # ------------------------------------------
  # check whether table summary should be printed
  # ------------------------------------------
  baseplot <- print.table.summary(baseplot, modsum, summary.pos)
  baseplot <- baseplot +
    # show absolute and percentage value of each bar.
    ggvaluelabels +
    # no additional labels for the x- and y-axis, only diagram title
    labs(title = title, x = axisTitle.x, y = axisTitle.y, fill = legend.title) +
    # print value labels to the x-axis.
    # If argument "axis.labels" is NULL, the category numbers (1 to ...)
    # appear on the x-axis
    scalex +
    # set Y-axis, depending on the calculated upper y-range.
    # It either corresponds to the maximum amount of cases in the data set
    # (length of var) or to the highest count of var's categories.
    scale_y_continuous(breaks = gridbreaks,
                       limits = c(lower_lim, upper_lim),
                       expand = expand.grid,
                       labels = scales::percent)
  # check whether coordinates should be flipped, i.e.
  # swap x and y axis
  if (coord.flip) baseplot <- baseplot + coord_flip()
  # ---------------------------------------------------------
  # set geom colors
  # ---------------------------------------------------------
  sj.setGeomColors(
    baseplot,
    geom.colors,
    length(legend.labels),
    show.legend,
    legend.labels
  )
}
