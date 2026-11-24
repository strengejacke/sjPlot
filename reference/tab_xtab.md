# Summary of contingency tables as HTML table

Shows contingency tables as HTML file in browser or viewer pane, or
saves them as file.

## Usage

``` r
tab_xtab(
  var.row,
  var.col,
  weight.by = NULL,
  title = NULL,
  var.labels = NULL,
  value.labels = NULL,
  wrap.labels = 20,
  show.obs = TRUE,
  show.cell.prc = FALSE,
  show.row.prc = FALSE,
  show.col.prc = FALSE,
  show.exp = FALSE,
  show.legend = FALSE,
  show.na = FALSE,
  show.summary = TRUE,
  drop.empty = TRUE,
  statistics = c("auto", "cramer", "phi", "spearman", "kendall", "pearson", "fisher"),
  string.total = "Total",
  digits = 1,
  tdcol.n = "black",
  tdcol.expected = "#339999",
  tdcol.cell = "#993333",
  tdcol.row = "#333399",
  tdcol.col = "#339933",
  emph.total = FALSE,
  emph.color = "#f8f8f8",
  prc.sign = "&nbsp;&#37;",
  hundret = "100.0",
  CSS = NULL,
  encoding = NULL,
  file = NULL,
  use.viewer = TRUE,
  remove.spaces = TRUE,
  ...
)

sjt.xtab(
  var.row,
  var.col,
  weight.by = NULL,
  title = NULL,
  var.labels = NULL,
  value.labels = NULL,
  wrap.labels = 20,
  show.obs = TRUE,
  show.cell.prc = FALSE,
  show.row.prc = FALSE,
  show.col.prc = FALSE,
  show.exp = FALSE,
  show.legend = FALSE,
  show.na = FALSE,
  show.summary = TRUE,
  drop.empty = TRUE,
  statistics = c("auto", "cramer", "phi", "spearman", "kendall", "pearson", "fisher"),
  string.total = "Total",
  digits = 1,
  tdcol.n = "black",
  tdcol.expected = "#339999",
  tdcol.cell = "#993333",
  tdcol.row = "#333399",
  tdcol.col = "#339933",
  emph.total = FALSE,
  emph.color = "#f8f8f8",
  prc.sign = "&nbsp;&#37;",
  hundret = "100.0",
  CSS = NULL,
  encoding = NULL,
  file = NULL,
  use.viewer = TRUE,
  remove.spaces = TRUE,
  ...
)
```

## Arguments

- var.row:

  Variable that should be displayed in the table rows.

- var.col:

  Cariable that should be displayed in the table columns.

- weight.by:

  Vector of weights that will be applied to weight all cases. Must be a
  vector of same length as the input vector. Default is `NULL`, so no
  weights are used.

- title:

  String, will be used as table caption.

- var.labels:

  Character vector with variable names, which will be used to label
  variables in the output.

- value.labels:

  Character vector (or `list` of character vectors) with value labels of
  the supplied variables, which will be used to label variable values in
  the output.

- wrap.labels:

  Numeric, determines how many chars of the value, variable or axis
  labels are displayed in one line and when a line break is inserted.

- show.obs:

  Logical, if `TRUE`, observed values are shown

- show.cell.prc:

  Logical, if `TRUE`, cell percentage values are shown

- show.row.prc:

  Logical, if `TRUE`, row percentage values are shown

- show.col.prc:

  Logical, if `TRUE`, column percentage values are shown

- show.exp:

  Logical, if `TRUE`, expected values are also shown

- show.legend:

  logical, if `TRUE`, and depending on plot type and function, a legend
  is added to the plot.

- show.na:

  logical, if `TRUE`, [`NA`](https://rdrr.io/r/base/NA.html)'s (missing
  values) are added to the output.

- show.summary:

  Logical, if `TRUE`, a summary row with chi-squared statistics, degrees
  of freedom and Cramer's V or Phi coefficient and p-value for the
  chi-squared statistics.

- drop.empty:

  Logical, if `TRUE` and the variable's values are labeled, values /
  factor levels with no occurrence in the data are omitted from the
  output. If `FALSE`, labeled values that have no observations are still
  printed in the table (with frequency `0`).

- statistics:

  Name of measure of association that should be computed. May be one of
  `"auto"`, `"cramer"`, `"phi"`, `"spearman"`, `"kendall"`, `"pearson"`
  or `"fisher"`. See
  [`xtab_statistics`](https://strengejacke.github.io/sjstats/reference/crosstable_statistics.html).

- string.total:

  Character label for the total column / row header

- digits:

  Amount of decimals for estimates

- tdcol.n:

  Color for highlighting count (observed) values in table cells. Default
  is black.

- tdcol.expected:

  Color for highlighting expected values in table cells. Default is
  cyan.

- tdcol.cell:

  Color for highlighting cell percentage values in table cells. Default
  is red.

- tdcol.row:

  Color for highlighting row percentage values in table cells. Default
  is blue.

- tdcol.col:

  Color for highlighting column percentage values in table cells.
  Default is green.

- emph.total:

  Logical, if `TRUE`, the total column and row will be emphasized with a
  different background color. See `emph.color`.

- emph.color:

  Logical, if `emph.total = TRUE`, this color value will be used for
  painting the background of the total column and row. Default is a
  light grey.

- prc.sign:

  The percentage sign that is printed in the table cells, in
  HTML-format. Default is `"&nbsp;%"`, hence the percentage sign has a
  non-breaking-space after the percentage value.

- hundret:

  Default value that indicates the 100-percent column-sums (since
  rounding values may lead to non-exact results). Default is `"100.0"`.

- CSS:

  A [`list`](https://rdrr.io/r/base/list.html) with user-defined
  style-sheet-definitions, according to the [official CSS
  syntax](https://www.w3.org/Style/CSS/). See 'Details' or [this
  package-vignette](https://strengejacke.github.io/sjPlot/articles/table_css.html).

- encoding:

  String, indicating the charset encoding used for variable and value
  labels. Default is `NULL`, so encoding will be auto-detected depending
  on your platform (e.g., `"UTF-8"` for Unix and `"Windows-1252"` for
  Windows OS). Change encoding if specific chars are not properly
  displayed (e.g. German umlauts).

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

- ...:

  Other arguments, currently passed down to the test statistics
  functions [`chisq.test()`](https://rdrr.io/r/stats/chisq.test.html) or
  [`fisher.test()`](https://rdrr.io/r/stats/fisher.test.html).

## Value

Invisibly returns

- the web page style sheet (`page.style`),

- the web page content (`page.content`),

- the complete html-output (`page.complete`) and

- the html-table with inline-css for use with knitr (`knitr`)

for further use.

## Examples

``` r
# prepare sample data set
data(efc)

# print simple cross table with labels
if (FALSE) { # \dontrun{
if (interactive()) {
  tab_xtab(efc$e16sex, efc$e42dep)

  # print cross table with manually set
  # labels and expected values
  tab_xtab(
    efc$e16sex,
    efc$e42dep,
    var.labels = c("Elder's gender", "Elder's dependency"),
    show.exp = TRUE
  )

  # print minimal cross table with labels, total col/row highlighted
  tab_xtab(efc$e16sex, efc$e42dep, show.cell.prc = FALSE, emph.total = TRUE)

  # User defined style sheet
  tab_xtab(efc$e16sex, efc$e42dep,
           CSS = list(css.table = "border: 2px solid;",
                      css.tdata = "border: 1px solid;",
                      css.horline = "border-bottom: double blue;"))

  # ordinal data, use Kendall's tau
  tab_xtab(efc$e42dep, efc$quol_5, statistics = "kendall")

  # calculate Spearman's rho, with continuity correction
  tab_xtab(
    efc$e42dep,
    efc$quol_5,
    statistics = "spearman",
    exact = FALSE,
    continuity = TRUE
  )
}
} # }
```
