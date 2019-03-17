#' Plot likert scales as centered stacked bars in groups
#' 
#' @name plot_likert_grp
#' 
#' @description Plot likert scales as centered stacked bars in groups
#'
#' @param factor.groups Must be a vector of same length as \code{ncol(df)}, 
#'                      where each item in this vector represents the group number 
#'                      of the related columns of \code{df}. See 'Examples'.
#' @param factor.groups.titles Titles for each factor group that will be used as table caption for each
#'          component-table. Must be a character vector of same length as \code{length(unique(factor.groups))}.
#'          Default is \code{"auto"}, which means that each table has a standard caption \emph{Component x}.
#'          Use \code{NULL} to use names as supplied to \code{factor.groups} and use \code{FALSE} to suppress table captions.
#' @param sort.groups If groups should be sorted according to the values supplied to \code{factor.groups}. Defaults to \code{TRUE}.
#' @param legend.pos Defines the legend position. Possible values are \code{c("bottom", "top", "all", "none")}. 
#'                   \code{"all"} will print legends as defined with \code{\link{set_theme}}.
#' @param rel_heights This option can be used to adjust the height of the subplots. The bars in subplots can have different heights due to a differing number of items 
#'                    or due to legend placement. This can be adjusted here. 
#'                    Takes a vector of numbers, one for each plot. Values are evaluated relative to each other.
#' @param label_x,hjust,align,label_size,label_fontfamily,label_fontface,label_colour,label_y,vjust Label options. See \code{\link[cowplot]{plot_grid}}.
#' @param ... Extra arguments to be passed to \code{\link{plot_likert}}.
#'
#' @inheritParams sjp.grpfrq
#' @inheritParams sjp.stackfrq
#' 
#' @examples
#' library(sjmisc)
#' data(efc)
#' # find all variables from COPE-Index, which all have a "cop" in their
#' # variable name, and then plot that subset as likert-plot
#' mydf <- find_var(efc, pattern = "cop", out = "df")
#' 
#' plot_likert_grp(mydf, c(2,1,1,1,1,2,2,2,1))
#' 
#' factor.groups <- sjt.pca(mydf)$factor.index
#' plot_likert_grp(mydf, c(2,1,1,1,1,2,2,2,1), factor.groups = factor.groups)
#' 
#' plot_likert_grp(mydf, 
#'                 c(rep("B",4),rep("A",5)), 
#'                 sort.groups = F, 
#'                 grid.range = c(0.9,1.1), 
#'                 geom.colors = "RdBu", 
#'                 rel_heights = c(6,8), 
#'                 wrap.labels = 40, 
#'                 reverse.scale=T)
#' 
#' @export
plot_likert_grp <- function(items,
                            factor.groups,
                            factor.groups.titles = "auto",
                            sort.groups = TRUE,
                            legend.pos = "bottom",
                            rel_heights = 1,
                            title = NULL,
                            label_x = 0.01, # Fix for label position depending on label length bug in cowplot
                            hjust = 0,
                            align = "v",
                            label_size = 14, # Cowplot label Options
                            label_fontfamily = NULL,
                            label_fontface = "bold",
                            label_colour = NULL,
                            label_y = 1,
                            vjust = 1.5,
                            ...) {
  
  if (!requireNamespace("cowplot", quietly = T)) 
    stop("plot_likert_grp: Please install the package \"cowplot\"", call. = F)

  if (ncol(items) != length(factor.groups))
    stop("plot_likert_grp: Length of groups has to equal the number of items: ncol(items) != length(groups)", call. = F)

  # retrieve unique factor / group index values
  findex <- unique(factor.groups)

  if (sort.groups) findex <- sort(findex)

  # Add empty title to plots, to create space for the group.labels
  if (is.null(title)) title <- rep("", length(findex))

  .plot_list <- list()

  # iterate all sub-plots (groups)
  for (i in seq_along(findex)) {
    index <- which(factor.groups == findex[i])

    .pl <- plot_likert(items[, index], title = title[i], ...)

    # Only the first or the last plot will have a legend, Maybe add option both
    if (legend.pos == "top" & i == 1)
      .pl <- .pl + theme(legend.position = "top")
    else if (legend.pos == "bottom" & i == length(findex))
      .pl <- .pl + theme(legend.position = "bottom")
    else if (legend.pos != "all") 
      .pl <- .pl + theme(legend.position = "none")

    .plot_list[i] <-  list(.pl)
  }

  # Options to turn off or overwrite cowplot group.labels
  if (isFALSE(factor.groups.titles))
    factor.groups.titles <- rep("", length(findex))
  else if (!is.null(factor.groups.titles) && (factor.groups.titles[1] == "auto" || length(factor.groups.titles) != length(findex)) && (is.numeric(factor.groups))) {
    factor.groups.titles <- sprintf("Component %i", seq_along(findex)) # For sjt.itemanalysis compatibility
  } else if (length(factor.groups.titles) != length(findex))
    factor.groups.titles <- findex

  return(cowplot::plot_grid(plotlist = .plot_list,
                            labels = factor.groups.titles,
                            rel_heights = rel_heights,
                            ncol = 1,
                            align = align,
                            label_x = label_x, # Cowplot label options can't be passed on using ...
                            label_size = label_size,
                            label_fontfamily = label_fontfamily,
                            label_fontface = label_fontface,
                            label_colour = label_colour,
                            label_y = label_y,
                            hjust = hjust,
                            vjust = vjust)
  )
}
