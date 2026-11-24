# Save ggplot-figure for print publication

Convenient function to save the last ggplot-figure in high quality for
publication.

## Usage

``` r
save_plot(
  filename,
  fig = ggplot2::last_plot(),
  width = 12,
  height = 9,
  dpi = 300,
  theme = ggplot2::theme_get(),
  label.color = "black",
  label.size = 2.4,
  axis.textsize = 0.8,
  axis.titlesize = 0.75,
  legend.textsize = 0.6,
  legend.titlesize = 0.65,
  legend.itemsize = 0.5
)
```

## Arguments

- filename:

  Name of the output file; filename must end with one of the following
  accepted file types: ".png", ".jpg", ".svg" or ".tif".

- fig:

  The plot that should be saved. By default, the last plot is saved.

- width:

  Width of the figure, in centimetres.

- height:

  Height of the figure, in centimetres.

- dpi:

  Resolution in dpi (dots per inch). Ignored for vector formats, such as
  ".svg".

- theme:

  The default theme to use when saving the plot.

- label.color:

  Color value for labels (axis, plot, etc.).

- label.size:

  Fontsize of value labels inside plot area.

- axis.textsize:

  Fontsize of axis labels.

- axis.titlesize:

  Fontsize of axis titles.

- legend.textsize:

  Fontsize of legend labels.

- legend.titlesize:

  Fontsize of legend title.

- legend.itemsize:

  Size of legend's item (legend key), in centimetres.

## Note

This is a convenient function with some default settings that should
come close to most of the needs for fontsize and scaling in figures when
saving them for printing or publishing. It uses cairographics
anti-aliasing (see [`png`](https://rdrr.io/r/grDevices/png.html)).  
  
For adjusting plot appearance, see also
[`sjPlot-themes`](https://strengejacke.github.io/sjPlot/reference/sjPlot-themes.md).
