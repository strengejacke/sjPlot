#' @title Modify plot appearance
#' @name sjPlot-themes
#'
#' @description Set default plot themes, use pre-defined color scales or modify
#'   plot or table appearance.
#'
#' @param base_size Base font size.
#' @param base_family Base font family.
#' @param title Font size for plot titles.
#' @param axis_title.x Font size for x-axis titles.
#' @param axis_title.y Font size for y-axis titles.
#' @param labels.x Font size for x-axis labels.
#' @param labels.y Font size for y-axis labels.
#' @param angle.x Angle for x-axis labels.
#' @param angle.y Angle for y-axis labels.
#' @param offset.x Offset for x-axis titles.
#' @param offset.y Offset for y-axis titles.
#' @param pos Position of the legend, if a legend is drawn.
#'   \describe{
#'     \item{\emph{Legend outside plot}}{
#'       Use \code{"bottom"}, \code{"top"}, \code{"left"} or \code{"right"}
#'       to position the legend above, below, on the left or right side
#'       of the diagram.
#'     }
#'     \item{\emph{Legend inside plot}}{
#'       If \code{inside = TRUE}, legend can be placed inside
#'       plot. Use \code{"top left"}, \code{"top right"}, \code{"bottom left"}
#'       and \code{"bottom right"} to position legend in any of these corners,
#'       or a two-element numeric vector with values from 0-1. See also
#'       \code{inside}.
#'     }
#'   }
#' @param justify Justification of legend, relative to its position (\code{"center"} or
#'   two-element numeric vector with values from 0-1.
#' @param inside Logical, use \code{TRUE} to put legend inside the plotting area.
#'   See also \code{pos}.
#' @param base.theme Optional ggplot-theme-object, which is needed in case multiple
#'   functions should be combined, e.g. \code{theme_sjplot() + label_angle()}.
#'   In such cases, use \code{label_angle(base.theme = theme_sjplot())}.
#' @param palette Character name of color palette.
#' @param discrete Logical, if \code{TRUE}, a discrete colour palette is returned.
#'   Else, a gradient palette is returned, where colours of the requested palette
#'   are interpolated using \code{\link[grDevices]{colorRampPalette}}.
#' @param reverse Logical, if \code{TRUE}, order of returned colours is reversed.
#' @param n Numeric, number of colors to be returned. By default, the complete
#'   colour palette is returned.
#' @param css.theme Name of the CSS pre-set theme-style. Can be used for table-functions.
#' @param ... Further arguments passed down to ggplot's \code{scale()}-functions.
#'
#' @details
#'   When using the \code{colors} argument in function calls (e.g.
#'   \code{plot_model()}) or when calling one of the predefined scale-functions
#'   (e.g. \code{scale_color_sjplot()}), there are pre-defined colour palettes
#'   in this package. Use \code{show_sjplot_pals()} to show all available
#'   colour palettes.
#'
#'
#' @examples
#' # prepare data
#' if (requireNamespace("haven")) {
#' library(sjmisc)
#' data(efc)
#' efc <- to_factor(efc, c161sex, e42dep, c172code)
#' m <- lm(neg_c_7 ~ pos_v_4 + c12hour + e42dep + c172code, data = efc)
#'
#' # create plot-object
#' p <- plot_model(m)
#'
#' # change theme
#' p + theme_sjplot()
#'
#' # change font-size
#' p + font_size(axis_title.x = 30)
#'
#' # apply color theme
#' p + scale_color_sjplot()
#'
#' # show all available colour palettes
#' show_sjplot_pals()
#'
#' # get colour values from specific palette
#' sjplot_pal(pal = "breakfast club")
#' }
#'
#' @import ggplot2
#' @rdname sjPlot-themes
#' @export
theme_sjplot <- function(base_size = 12, base_family = "") {
  (theme_minimal(base_size = base_size, base_family = base_family) +
     theme(
       axis.line.x      = element_line(colour = "grey80"),
       axis.line.y      = element_line(colour = "grey80"),
       axis.text        = element_text(colour = "grey50"),
       axis.title       = element_text(colour = "grey30"),
       strip.background = element_rect(colour = "grey70", fill = "grey90"),
       strip.text       = element_text(colour = "grey30"),
       legend.title     = element_text(colour = "grey30"),
       legend.text      = element_text(colour = "grey30")
     ))
}


#' @rdname sjPlot-themes
#' @export
theme_sjplot2 <- function(base_size = 12, base_family = "") {
  (theme_minimal(base_size = base_size, base_family = base_family) +
     theme(
       axis.line.x      = element_line(colour = "grey50"),
       axis.line.y      = element_line(colour = "grey50"),
       axis.text        = element_text(colour = "grey10"),
       axis.title       = element_text(colour = "black"),
       strip.background = element_rect(colour = "grey50", fill = "grey70"),
       strip.text       = element_text(colour = "grey20"),
       legend.title     = element_text(colour = "grey10"),
       legend.text      = element_text(colour = "grey20")
     ))
}


#' @rdname sjPlot-themes
#' @export
theme_blank <- function(base_size = 12, base_family = "") {
  (theme_minimal(base_size = base_size, base_family = base_family) +
     theme(
       axis.line.x      = element_line(colour = "white"),
       axis.line.y      = element_line(colour = "white"),
       axis.text        = element_text(colour = "grey50"),
       axis.title       = element_text(colour = "grey30"),
       panel.grid.minor = element_line(colour = "white", linetype = 1),
       panel.grid.major = element_line(colour = "white", linetype = 1)
     ))
}


#' @rdname sjPlot-themes
#' @export
theme_538 <- function(base_size = 12, base_family = "") {
  (theme_minimal(base_size = base_size, base_family = base_family) +
     theme(
       axis.line.x        = element_line(colour = "#F0F0F0"),
       axis.line.y        = element_line(colour = "#F0F0F0"),
       axis.text          = element_text(colour = "#737373"),
       axis.title         = element_text(colour = "#525252"),
       plot.background    = element_rect(colour = "#F0F0F0", fill = "#F0F0F0"),
       panel.grid.minor.x = element_line(colour = "#F0F0F0", linetype = 1),
       panel.grid.major   = element_line(colour = "#BDBDBD", linetype = 1),
       panel.grid.major.y = element_line(colour = "#F0F0F0", linetype = 1),
       panel.grid.minor.y = element_line(colour = "#F0F0F0", linetype = 1)
     ))
}


#' @rdname sjPlot-themes
#' @export
font_size <- function(title, axis_title.x, axis_title.y, labels.x, labels.y, offset.x, offset.y, base.theme) {
  # get current theme
  if (!missing(base.theme))
    cur.theme <- base.theme
  else
    cur.theme <- theme_get()

  if (!missing(title)) {
    cur.theme <- cur.theme +
      theme(title = element_text(size = title))
  }

  if (!missing(axis_title.x)) {
    cur.theme <- cur.theme +
      theme(axis.title.x = element_text(size = axis_title.x))
  }

  if (!missing(axis_title.y)) {
    cur.theme <- cur.theme +
      theme(axis.title.y = element_text(size = axis_title.y))
  }

  if (!missing(labels.x)) {
    cur.theme <- cur.theme +
      theme(axis.text.x =  element_text(size = labels.x))
  }

  if (!missing(labels.y)) {
    cur.theme <- cur.theme +
      theme(axis.text.y =  element_text(size = labels.y))
  }

  if (!missing(offset.x)) {
    cur.theme <- cur.theme +
      theme(axis.title.x = element_text(vjust = offset.x))
  }

  if (!missing(offset.y)) {
    cur.theme <- cur.theme +
      theme(axis.title.y = element_text(vjust = offset.y))
  }

  cur.theme
}


#' @rdname sjPlot-themes
#' @export
label_angle <- function(angle.x, angle.y, base.theme) {
  # get current theme
  if (!missing(base.theme))
    cur.theme <- base.theme
  else
    cur.theme <- theme_get()

  if (!missing(angle.x)) {
    cur.theme <- cur.theme +
      theme(axis.text.x = element_text(angle = angle.x))
  }

  if (!missing(angle.y)) {
    cur.theme <- cur.theme +
      theme(axis.text.y = element_text(angle = angle.y))
  }

  cur.theme
}


#' @rdname sjPlot-themes
#' @importFrom dplyr case_when
#' @export
legend_style <- function(inside, pos, justify, base.theme) {
  # get current theme
  if (!missing(base.theme))
    cur.theme <- base.theme
  else
    cur.theme <- theme_get()

  # convert legend position from character to numeric index
  if (!missing(inside) && inside) {
    if (!missing(pos) && is.character(pos)) {
      pos <- dplyr::case_when(
        pos == "top right" ~ c(1, 1),
        pos == "bottom right" ~ c(1, 0),
        pos == "bottom left" ~ c(0, 0),
        pos == "top left" ~ c(0, 1),
        TRUE ~ c(1, 1)
      )

      if (missing(justify)) justify <- pos
    }
  }

  # set default justification
  if (missing(justify)) justify <- "center"

  if (!missing(pos)) {
    cur.theme <- cur.theme +
      theme(
        legend.position = pos,
        legend.justification = justify
      )
  }

  cur.theme
}


sjplot_colors <- list(
  `aqua` = c("#BAF5F3", "#46A9BE", "#8B7B88", "#BD7688", "#F2C29E"),
  `warm` = c("#072835", "#664458", "#C45B46", "#F1B749", "#F8EB85"),
  `dust` = c("#232126", "#7B5756", "#F7B98B", "#F8F7CF", "#AAAE9D"),
  `blambus` = c("#E02E1F", "#5D8191", "#BD772D", "#494949", "#F2DD26"),
  `simply` = c("#CD423F", "#0171D3", "#018F77", "#FCDA3B", "#F5C6AC"),
  `us` = c("#004D80", "#376C8E", "#37848E", "#9BC2B6", "#B5D2C0"),
  `reefs` = c("#43a9b6", "#218282", "#dbdcd1", "#44515c", "#517784"),
  `breakfast club` = c("#b6411a", "#4182dd", "#2d6328", "#eec3d8", "#ecf0c8"),
  `metro` = c("#d11141", "#00aedb", "#00b159", "#f37735", "#8c8c8c", "#ffc425", "#cccccc"),
  `viridis` = c("#440154", "#46337E", "#365C8D", "#277F8E", "#1FA187", "#4AC16D", "#9FDA3A", "#FDE725"),
  `ipsum` = c("#3f2d54", "#75b8d1", "#2d543d", "#d18975", "#8fd175", "#d175b8", "#758bd1", "#d1ab75", "#c9d175"),
  `quadro` = c("#ff0000", "#1f3c88", "#23a393", "#f79f24", "#625757"),
  `eight` = c("#003f5c", "#2f4b7c", "#665191", "#a05195", "#d45087", "#f95d6a", "#ff7c43", "#ffa600"),
  `circus` = c("#C1241E", "#0664C9", "#EBD90A", "#6F130D", "#111A79"),
  `system` = c("#0F2838", "#F96207", "#0DB0F3", "#04EC04", "#FCC44C"),
  `hero` = c("#D2292B", "#165E88", "#E0BD1C", "#D57028", "#A5CB39", "#8D8F70"),
  `flat` = c("#c0392b", "#2980b9", "#16a085", "#f39c12", "#8e44ad", "#7f8c8d", "#d35400"),
  `social` = c("#b92b27", "#0077B5", "#00b489", "#f57d00", "#410093", "#21759b", "#ff3300")
)


#' @rdname sjPlot-themes
#' @export
scale_color_sjplot <- function(palette = "metro", discrete = TRUE, reverse = FALSE, ...) {
  pal <- get_sjplot_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("colour", paste0("sjplot_pal_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}


#' @rdname sjPlot-themes
#' @export
scale_fill_sjplot <- function(palette = "metro", discrete = TRUE, reverse = FALSE, ...) {
  pal <- get_sjplot_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("fill", paste0("sjplot_pal_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}


#' @importFrom stats quantile
#' @rdname sjPlot-themes
#' @export
sjplot_pal <- function(palette = "metro", n = NULL) {
  pl <- sjplot_colors[[palette]]

  if (!is.null(n) && n <= length(pl)) {
    if (.is_cont_scale(palette)) {
      pl <- pl[stats::quantile(1:length(pl), probs = seq(0, 1, length.out = n))]
    } else {
      pl <- pl[1:n]
    }
  }

  pl
}

# palettes with a continuous color scale
.is_cont_scale <- function(p) {
  p %in% c("aqua", "dust", "eight", "greyscale", "us", "viridis", "warm")
}


#' @rdname sjPlot-themes
#' @importFrom purrr map_df
#' @importFrom tidyr gather
#' @importFrom dplyr arrange mutate
#' @importFrom rlang .data
#' @export
show_sjplot_pals <- function() {
  longest.pal <- max(purrr::map_dbl(sjplot_colors, ~ length(.x)))

  sjpc <- lapply(sjplot_colors, function(.x) {
    if (length(.x) == longest.pal)
      .x
    else
      c(.x, rep("#ffffff", times = longest.pal - length(.x)))
  })

  x <- suppressWarnings(
    sjpc %>%
      as.data.frame() %>%
      purrr::map_df(~ .x[length(.x):1]) %>%
      tidyr::gather() %>%
      dplyr::arrange(.data$key)
  )

  x$y <- rep_len(1:longest.pal, nrow(x))
  x$cols = as.factor(1:nrow(x))

  x$key <- factor(x$key, levels = rev(unique(x$key)))

  x$group <- "Other Palettes"
  x$group[.is_cont_scale(x$key)] <- "Continuous Palettes"
  x$group[x$key %in% c("breakfast.club", "flat", "metro", "quadro", "set1", "simply", "social")] <- "Red-Blue-Green Palettes"

  ggplot(x, aes_string(x = "key", fill = "cols")) +
    geom_bar(width = .7) +
    scale_fill_manual(values = x$value) +
    scale_y_continuous(breaks = NULL, labels = NULL) +
    guides(fill = "none") +
    coord_flip() +
    theme_minimal() +
    labs(x = NULL, y = NULL) +
    facet_wrap(~group, ncol = 1, scales = "free")
}


#' @importFrom grDevices colorRampPalette
get_sjplot_pal <- function(palette = "metro", reverse = FALSE, ...) {
  pal <- sjplot_colors[[palette]]
  if (reverse) pal <- rev(pal)
  grDevices::colorRampPalette(pal, ...)
}


#' @rdname sjPlot-themes
#' @export
css_theme <- function(css.theme = "regression") {

  if (!(css.theme %in% names(css.themes))) {
    warning(sprintf("No valid CSS-theme name. Current available themes are: %s", paste(names(css.themes), collapse = ", ")), call. = FALSE)
    return(NULL)
  }

  css.themes[[css.theme]]
}


css.themes <- list(
  `regression` = list(
    css.thead = "border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm;",
    css.firsttablerow = "",
    css.summarydata = "text-align:left;"
  ),
  `cells` = list(
    css.td = "border:1px solid black;",
    css.thead = "border:1px solid black;"
  ),
  `right_aligned` = list(
    css.tdata = "padding:0.2cm; text-align:right; vertical-align:middle;"
  )
)
