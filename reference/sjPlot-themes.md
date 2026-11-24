# Modify plot appearance

Set default plot themes, use pre-defined color scales or modify plot or
table appearance.

## Usage

``` r
theme_sjplot(base_size = 12, base_family = "")

theme_sjplot2(base_size = 12, base_family = "")

theme_blank(base_size = 12, base_family = "")

theme_538(base_size = 12, base_family = "")

font_size(
  title,
  axis_title.x,
  axis_title.y,
  labels.x,
  labels.y,
  offset.x,
  offset.y,
  base.theme
)

label_angle(angle.x, angle.y, base.theme)

legend_style(inside, pos, justify, base.theme)

scale_color_sjplot(palette = "metro", discrete = TRUE, reverse = FALSE, ...)

scale_fill_sjplot(palette = "metro", discrete = TRUE, reverse = FALSE, ...)

sjplot_pal(palette = "metro", n = NULL)

show_sjplot_pals()

css_theme(css.theme = "regression")
```

## Arguments

- base_size:

  Base font size.

- base_family:

  Base font family.

- title:

  Font size for plot titles.

- axis_title.x:

  Font size for x-axis titles.

- axis_title.y:

  Font size for y-axis titles.

- labels.x:

  Font size for x-axis labels.

- labels.y:

  Font size for y-axis labels.

- offset.x:

  Offset for x-axis titles.

- offset.y:

  Offset for y-axis titles.

- base.theme:

  Optional ggplot-theme-object, which is needed in case multiple
  functions should be combined, e.g. `theme_sjplot() + label_angle()`.
  In such cases, use `label_angle(base.theme = theme_sjplot())`.

- angle.x:

  Angle for x-axis labels.

- angle.y:

  Angle for y-axis labels.

- inside:

  Logical, use `TRUE` to put legend inside the plotting area. See also
  `pos`.

- pos:

  Position of the legend, if a legend is drawn.

  *Legend outside plot*

  :   Use `"bottom"`, `"top"`, `"left"` or `"right"` to position the
      legend above, below, on the left or right side of the diagram.

  *Legend inside plot*

  :   If `inside = TRUE`, legend can be placed inside plot. Use
      `"top left"`, `"top right"`, `"bottom left"` and `"bottom right"`
      to position legend in any of these corners, or a two-element
      numeric vector with values from 0-1. See also `inside`.

- justify:

  Justification of legend, relative to its position (`"center"` or
  two-element numeric vector with values from 0-1.

- palette:

  Character name of color palette.

- discrete:

  Logical, if `TRUE`, a discrete colour palette is returned. Else, a
  gradient palette is returned, where colours of the requested palette
  are interpolated using
  [`colorRampPalette`](https://rdrr.io/r/grDevices/colorRamp.html).

- reverse:

  Logical, if `TRUE`, order of returned colours is reversed.

- ...:

  Further arguments passed down to ggplot's
  [`scale()`](https://rdrr.io/r/base/scale.html)-functions.

- n:

  Numeric, number of colors to be returned. By default, the complete
  colour palette is returned.

- css.theme:

  Name of the CSS pre-set theme-style. Can be used for table-functions.

## Details

When using the `colors` argument in function calls (e.g.
[`plot_model()`](https://strengejacke.github.io/sjPlot/reference/plot_model.md))
or when calling one of the predefined scale-functions (e.g.
`scale_color_sjplot()`), there are pre-defined colour palettes in this
package. Use `show_sjplot_pals()` to show all available colour palettes.

## Examples

``` r
# prepare data
if (requireNamespace("haven")) {
library(sjmisc)
data(efc)
efc <- to_factor(efc, c161sex, e42dep, c172code)
m <- lm(neg_c_7 ~ pos_v_4 + c12hour + e42dep + c172code, data = efc)

# create plot-object
p <- plot_model(m)

# change theme
p + theme_sjplot()

# change font-size
p + font_size(axis_title.x = 30)

# apply color theme
p + scale_color_sjplot()

# show all available colour palettes
show_sjplot_pals()

# get colour values from specific palette
sjplot_pal(pal = "breakfast club")
}
#> Scale for colour is already present.
#> Adding another scale for colour, which will replace the existing scale.
#> [1] "#b6411a" "#4182dd" "#2d6328" "#eec3d8" "#ecf0c8"
```
