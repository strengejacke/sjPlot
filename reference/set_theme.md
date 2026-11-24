# Set global theme options for sjp-functions

Set global theme options for sjp-functions.

## Usage

``` r
set_theme(
  base = ggplot2::theme_grey(),
  theme.font = NULL,
  title.color = "black",
  title.size = 1.2,
  title.align = "left",
  title.vjust = NULL,
  geom.outline.color = NULL,
  geom.outline.size = 0,
  geom.boxoutline.size = 0.5,
  geom.boxoutline.color = "black",
  geom.alpha = 1,
  geom.linetype = 1,
  geom.errorbar.size = 0.7,
  geom.errorbar.linetype = 1,
  geom.label.color = NULL,
  geom.label.size = 4,
  geom.label.alpha = 1,
  geom.label.angle = 0,
  axis.title.color = "grey30",
  axis.title.size = 1.1,
  axis.title.x.vjust = NULL,
  axis.title.y.vjust = NULL,
  axis.angle.x = 0,
  axis.angle.y = 0,
  axis.angle = NULL,
  axis.textcolor.x = "grey30",
  axis.textcolor.y = "grey30",
  axis.textcolor = NULL,
  axis.linecolor.x = NULL,
  axis.linecolor.y = NULL,
  axis.linecolor = NULL,
  axis.line.size = 0.5,
  axis.textsize.x = 1,
  axis.textsize.y = 1,
  axis.textsize = NULL,
  axis.tickslen = NULL,
  axis.tickscol = NULL,
  axis.ticksmar = NULL,
  axis.ticksize.x = NULL,
  axis.ticksize.y = NULL,
  panel.backcol = NULL,
  panel.bordercol = NULL,
  panel.col = NULL,
  panel.major.gridcol = NULL,
  panel.minor.gridcol = NULL,
  panel.gridcol = NULL,
  panel.gridcol.x = NULL,
  panel.gridcol.y = NULL,
  panel.major.linetype = 1,
  panel.minor.linetype = 1,
  plot.backcol = NULL,
  plot.bordercol = NULL,
  plot.col = NULL,
  plot.margins = NULL,
  legend.pos = "right",
  legend.just = NULL,
  legend.inside = FALSE,
  legend.size = 1,
  legend.color = "black",
  legend.title.size = 1,
  legend.title.color = "black",
  legend.title.face = "bold",
  legend.backgroundcol = "white",
  legend.bordercol = "white",
  legend.item.size = NULL,
  legend.item.backcol = "grey90",
  legend.item.bordercol = "white"
)
```

## Arguments

- base:

  base theme where theme is built on. By default, all metrics from
  [`theme_gray()`](https://ggplot2.tidyverse.org/reference/ggtheme.html)
  are used. See 'Details'.

- theme.font:

  base font family for the plot.

- title.color:

  Color of plot title. Default is `"black"`.

- title.size:

  size of plot title. Default is 1.3.

- title.align:

  alignment of plot title. Must be one of `"left"` (default), `"center"`
  or `"right"`. You may use initial letter only.

- title.vjust:

  numeric, vertical adjustment for plot title.

- geom.outline.color:

  Color of geom outline. Only applies, if `geom.outline.size` is larger
  than 0.

- geom.outline.size:

  size of bar outlines. Default is 0.1. Use size of `0` to remove geom
  outline.

- geom.boxoutline.size:

  size of outlines and median bar especially for boxplots. Default is
  0.5. Use size of `0` to remove boxplot outline.

- geom.boxoutline.color:

  Color of outlines and median bar especially for boxplots. Only
  applies, if `geom.boxoutline.size` is larger than 0.

- geom.alpha:

  specifies the transparancy (alpha value) of geoms

- geom.linetype:

  linetype of line geoms. Default is `1` (solid line).

- geom.errorbar.size:

  size (thickness) of error bars. Default is `0.8`

- geom.errorbar.linetype:

  linetype of error bars. Default is `1` (solid line).

- geom.label.color:

  Color of geom's value and annotation labels

- geom.label.size:

  size of geom's value and annotation labels

- geom.label.alpha:

  alpha level of geom's value and annotation labels

- geom.label.angle:

  angle of geom's value and annotation labels

- axis.title.color:

  Color of x- and y-axis title labels

- axis.title.size:

  size of x- and y-axis title labels

- axis.title.x.vjust:

  numeric, vertical adjustment of x-axis-title.

- axis.title.y.vjust:

  numeric, vertical adjustment of y-axis-title.

- axis.angle.x:

  angle for x-axis labels

- axis.angle.y:

  angle for y-axis labels

- axis.angle:

  angle for x- and y-axis labels. If set, overrides both `axis.angle.x`
  and `axis.angle.y`

- axis.textcolor.x:

  Color for x-axis labels. If not specified, a default dark gray color
  palette will be used for the labels.

- axis.textcolor.y:

  Color for y-axis labels. If not specified, a default dark gray color
  palette will be used for the labels.

- axis.textcolor:

  Color for both x- and y-axis labels. If set, overrides both
  `axis.textcolor.x` and `axis.textcolor.y`

- axis.linecolor.x:

  Color of x-axis border

- axis.linecolor.y:

  Color of y-axis border

- axis.linecolor:

  Color for both x- and y-axis borders. If set, overrides both
  `axis.linecolor.x` and `axis.linecolor.y`.

- axis.line.size:

  size (thickness) of axis lines. Only affected, if `axis.linecolor` is
  set.

- axis.textsize.x:

  size of x-axis labels

- axis.textsize.y:

  size of y-axis labels

- axis.textsize:

  size for both x- and y-axis labels. If set, overrides both
  `axis.textsize.x` and `axis.textsize.y`.

- axis.tickslen:

  length of axis tick marks

- axis.tickscol:

  Color of axis tick marks

- axis.ticksmar:

  margin between axis labels and tick marks

- axis.ticksize.x:

  size of tick marks at x-axis.

- axis.ticksize.y:

  size of tick marks at y-axis.

- panel.backcol:

  Color of the diagram's background

- panel.bordercol:

  Color of whole diagram border (panel border)

- panel.col:

  Color of both diagram's border and background. If set, overrides both
  `panel.bordercol` and `panel.backcol`.

- panel.major.gridcol:

  Color of the major grid lines of the diagram background

- panel.minor.gridcol:

  Color of the minor grid lines of the diagram background

- panel.gridcol:

  Color for both minor and major grid lines of the diagram background.
  If set, overrides both `panel.major.gridcol` and
  `panel.minor.gridcol`.

- panel.gridcol.x:

  See `panel.gridcol`.

- panel.gridcol.y:

  See `panel.gridcol`.

- panel.major.linetype:

  line type for major grid lines

- panel.minor.linetype:

  line type for minor grid lines

- plot.backcol:

  Color of the plot's background

- plot.bordercol:

  Color of whole plot's border (panel border)

- plot.col:

  Color of both plot's region border and background. If set, overrides
  both `plot.backcol` and `plot.bordercol`.

- plot.margins:

  numeric vector of length 4, indicating the top, right, bottom and left
  margin of the plot region.

- legend.pos:

  position of the legend, if a legend is drawn.

  *legend outside plot*

  :   Use `"bottom"`, `"top"`, `"left"` or `"right"` to position the
      legend above, below, on the left or right side of the diagram.
      Right positioning is default.

  *legend inside plot*

  :   If `legend.inside = TRUE`, legend can be placed inside plot. Use
      `"top left"`, `"top right"`, `"bottom left"` and `"bottom right"`
      to position legend in any of these corners, or a two-element
      numeric vector with values from 0-1. See also `legend.inside`.

- legend.just:

  justification of legend, relative to its position (`"center"` or
  two-element numeric vector with values from 0-1. By default (outside
  legend), justification is centered. If legend is inside and
  justification not specified, legend justification is set according to
  legend position.

- legend.inside:

  logical, use `TRUE` to put legend inside the plotting area. See
  `legend.pos`.

- legend.size:

  text size of the legend. Default is 1. Relative size, so recommended
  values are from 0.3 to 2.5

- legend.color:

  Color of the legend labels

- legend.title.size:

  text size of the legend title

- legend.title.color:

  Color of the legend title

- legend.title.face:

  font face of the legend title. By default, `"bold"` face is used.

- legend.backgroundcol:

  fill color of the legend's background. Default is `"white"`, so no
  visible background is drawn.

- legend.bordercol:

  Color of the legend's border. Default is `"white"`, so no visible
  border is drawn.

- legend.item.size:

  size of legend's item (legend key), in centimetres.

- legend.item.backcol:

  fill color of the legend's item-background. Default is `"grey90"`.

- legend.item.bordercol:

  Color of the legend's item-border. Default is `"white"`.

## Value

The customized theme object, or `NULL`, if a ggplot-theme was used.

## See also

[`sjPlot-themes`](https://strengejacke.github.io/sjPlot/reference/sjPlot-themes.md)

## Examples

``` r
if (FALSE) { # \dontrun{
library(sjmisc)
data(efc)
# set sjPlot-defaults, a slightly modification
# of the ggplot base theme
set_theme()

# legends of all plots inside
set_theme(legend.pos = "top left", legend.inside = TRUE)
plot_xtab(efc$e42dep, efc$e16sex)

# Use classic-theme. you may need to
# load the ggplot2-library.
library(ggplot2)
set_theme(base = theme_classic())
plot_frq(efc$e42dep)

# adjust value labels
set_theme(
  geom.label.size = 3.5,
  geom.label.color = "#3366cc",
  geom.label.angle = 90
)

# hjust-aes needs adjustment for this
update_geom_defaults('text', list(hjust = -0.1))
plot_xtab(efc$e42dep, efc$e16sex, vjust = "center", hjust = "center")

# Create own theme based on classic-theme
set_theme(
  base = theme_classic(), axis.linecolor = "grey50",
  axis.textcolor = "#6699cc"
)
plot_frq(efc$e42dep)} # }
```
