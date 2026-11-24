# Wrapper to create plots and tables within a pipe-workflow

This function has a pipe-friendly argument-structure, with the first
argument always being the data, followed by variables that should be
plotted or printed as table. The function then transforms the input and
calls the requested sjp.- resp. sjt.-function to create a plot or
table.  
  
Both `sjplot()` and `sjtab()` support grouped data frames.

## Usage

``` r
sjplot(data, ..., fun = c("grpfrq", "xtab", "aov1", "likert"))

sjtab(data, ..., fun = c("xtab", "stackfrq"))
```

## Arguments

- data:

  A data frame. May also be a grouped data frame (see 'Note' and
  'Examples').

- ...:

  Names of variables that should be plotted, and also further arguments
  passed down to the sjPlot-functions. See 'Examples'.

- fun:

  Plotting function. Refers to the function name of sjPlot-functions.
  See 'Details' and 'Examples'.

## Value

See related sjp. and sjt.-functions.

## Details

Following `fun`-values are currently supported:

- `"grpfrq"`:

  calls
  [`plot_grpfrq`](https://strengejacke.github.io/sjPlot/reference/plot_grpfrq.md).
  The first two variables in `data` are used (and required) to create
  the plot.

- `"likert"`:

  calls
  [`plot_likert`](https://strengejacke.github.io/sjPlot/reference/plot_likert.md).
  `data` must be a data frame with items to plot.

- `"stackfrq"`:

  calls
  [`tab_stackfrq`](https://strengejacke.github.io/sjPlot/reference/tab_stackfrq.md).
  `data` must be a data frame with items to create the table.

- `"xtab"`:

  calls
  [`plot_xtab`](https://strengejacke.github.io/sjPlot/reference/plot_xtab.md)
  or
  [`tab_xtab`](https://strengejacke.github.io/sjPlot/reference/tab_xtab.md).
  The first two variables in `data` are used (and required) to create
  the plot or table.

## Note

The `...`-argument is used, first, to specify the variables from `data`
that should be plotted, and, second, to name further arguments that are
used in the subsequent plotting functions. Refer to the online-help of
supported plotting-functions to see valid arguments.  
  
`data` may also be a grouped data frame (see
[`group_by`](https://dplyr.tidyverse.org/reference/group_by.html)) with
up to two grouping variables. Plots are created for each subgroup then.

## Examples

``` r
library(dplyr)
data(efc)

# Grouped frequencies
efc |> sjplot(e42dep, c172code, fun = "grpfrq")
#> Ignoring unknown labels:
#> • colour : "carer's level of education"


# Grouped frequencies, as box plots
efc |> sjplot(e17age, c172code, fun = "grpfrq",
               type = "box", geom.colors = "Set1")
#> Ignoring unknown labels:
#> • colour : "carer's level of education"


if (FALSE) { # \dontrun{
# table output of grouped data frame
efc |>
  group_by(e16sex, c172code) |>
  select(e42dep, n4pstu, e16sex, c172code) |>
  sjtab(fun = "xtab", use.viewer = FALSE) # open all tables in browser
} # }
```
