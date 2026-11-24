# View structure of labelled data frames

Save (or show) content of an imported SPSS, SAS or Stata data file, or
any similar labelled `data.frame`, as HTML table. This quick overview
shows variable ID number, name, label, type and associated value labels.
The result can be considered as "codeplan" of the data frame.

## Usage

``` r
view_df(
  x,
  weight.by = NULL,
  alternate.rows = TRUE,
  show.id = TRUE,
  show.type = FALSE,
  show.values = TRUE,
  show.string.values = FALSE,
  show.labels = TRUE,
  show.frq = FALSE,
  show.prc = FALSE,
  show.wtd.frq = FALSE,
  show.wtd.prc = FALSE,
  show.na = FALSE,
  max.len = 15,
  sort.by.name = FALSE,
  wrap.labels = 50,
  verbose = FALSE,
  CSS = NULL,
  encoding = NULL,
  file = NULL,
  use.viewer = TRUE,
  remove.spaces = TRUE
)
```

## Arguments

- x:

  A (labelled) data frame, imported by
  [`read_spss`](https://strengejacke.github.io/sjlabelled/reference/read_spss.html),
  [`read_sas`](https://strengejacke.github.io/sjlabelled/reference/read_spss.html)
  or
  [`read_stata`](https://strengejacke.github.io/sjlabelled/reference/read_spss.html)
  function, or any similar labelled data frame (see
  [`set_label`](https://strengejacke.github.io/sjlabelled/reference/set_label.html)
  and
  [`set_labels`](https://strengejacke.github.io/sjlabelled/reference/set_labels.html)).

- weight.by:

  Name of variable in `x` that indicated the vector of weights that will
  be applied to weight all observations. Default is `NULL`, so no
  weights are used.

- alternate.rows:

  Logical, if `TRUE`, rows are printed in alternatig colors (white and
  light grey by default).

- show.id:

  Logical, if `TRUE` (default), the variable ID is shown in the first
  column.

- show.type:

  Logical, if `TRUE`, adds information about the variable type to the
  variable column.

- show.values:

  Logical, if `TRUE` (default), the variable values are shown as
  additional column.

- show.string.values:

  Logical, if `TRUE`, elements of character vectors are also shown. By
  default, these are omitted due to possibly overlengthy tables.

- show.labels:

  Logical, if `TRUE` (default), the value labels are shown as additional
  column.

- show.frq:

  Logical, if `TRUE`, an additional column with frequencies for each
  variable is shown.

- show.prc:

  Logical, if `TRUE`, an additional column with percentage of
  frequencies for each variable is shown.

- show.wtd.frq:

  Logical, if `TRUE`, an additional column with weighted frequencies for
  each variable is shown. Weights strem from `weight.by`.

- show.wtd.prc:

  Logical, if `TRUE`, an additional column with weighted percentage of
  frequencies for each variable is shown. Weights strem from
  `weight.by`.

- show.na:

  logical, if `TRUE`, [`NA`](https://rdrr.io/r/base/NA.html)'s (missing
  values) are added to the output.

- max.len:

  Numeric, indicates how many values and value labels per variable are
  shown. Useful for variables with many different values, where the
  output can be truncated.

- sort.by.name:

  Logical, if `TRUE`, rows are sorted according to the variable names.
  By default, rows (variables) are ordered according to their order in
  the data frame.

- wrap.labels:

  Numeric, determines how many chars of the value, variable or axis
  labels are displayed in one line and when a line break is inserted.

- verbose:

  Logical, if `TRUE`, a progress bar is displayed while creating the
  output.

- CSS:

  A [`list`](https://rdrr.io/r/base/list.html) with user-defined
  style-sheet-definitions, according to the [official CSS
  syntax](https://www.w3.org/Style/CSS/). See 'Details' or [this
  package-vignette](https://strengejacke.github.io/sjPlot/articles/table_css.html).

- encoding:

  Character vector, indicating the charset encoding used for variable
  and value labels. Default is `"UTF-8"`. For Windows Systems,
  `encoding = "Windows-1252"` might be necessary for proper display of
  special characters.

- file:

  Destination file, if the output should be saved as file. If `NULL`
  (default), the output will be saved as temporary file and opened
  either in the IDE's viewer pane or the default web browser.

- use.viewer:

  Logical, if `TRUE`, the HTML table is shown in the IDE's viewer pane.
  If `FALSE` or no viewer available, the HTML table is opened in a web
  browser.

- remove.spaces:

  Logical, if `TRUE`, leading spaces are removed from all lines in the
  final string that contains the html-data. Use this, if you want to
  remove parantheses for html-tags. The html-source may look less
  pretty, but it may help when exporting html-tables to office tools.

## Value

Invisibly returns

- the web page style sheet (`page.style`),

- the web page content (`page.content`),

- the complete html-output (`page.complete`) and

- the html-table with inline-css for use with knitr (`knitr`)

for further use.

## Examples

``` r
if (FALSE) { # \dontrun{
# init dataset
data(efc)

# view variables
view_df(efc)

# view variables w/o values and value labels
view_df(efc, show.values = FALSE, show.labels = FALSE)

# view variables including variable typed, orderd by name
view_df(efc, sort.by.name = TRUE, show.type = TRUE)

# User defined style sheet
view_df(efc,
        CSS = list(css.table = "border: 2px solid;",
                   css.tdata = "border: 1px solid;",
                   css.arc = "color:blue;"))} # }
```
