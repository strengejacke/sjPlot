# Summary of correlations as HTML table

Shows the results of a computed correlation as HTML table. Requires
either a [`data.frame`](https://rdrr.io/r/base/data.frame.html) or a
matrix with correlation coefficients as returned by the
[`cor`](https://rdrr.io/r/stats/cor.html)-function.

## Usage

``` r
tab_corr(
  data,
  na.deletion = c("listwise", "pairwise"),
  corr.method = c("pearson", "spearman", "kendall"),
  title = NULL,
  var.labels = NULL,
  wrap.labels = 40,
  show.p = TRUE,
  p.numeric = FALSE,
  fade.ns = TRUE,
  val.rm = NULL,
  digits = 3,
  triangle = "both",
  string.diag = NULL,
  CSS = NULL,
  encoding = NULL,
  file = NULL,
  use.viewer = TRUE,
  remove.spaces = TRUE
)
```

## Arguments

- data:

  Matrix with correlation coefficients as returned by the
  [`cor`](https://rdrr.io/r/stats/cor.html)-function, or a `data.frame`
  of variables where correlations between columns should be computed.

- na.deletion:

  Indicates how missing values are treated. May be either `"listwise"`
  (default) or `"pairwise"`. May be abbreviated.

- corr.method:

  Indicates the correlation computation method. May be one of
  `"pearson"` (default), `"spearman"` or `"kendall"`. May be
  abbreviated.

- title:

  String, will be used as table caption.

- var.labels:

  Character vector with variable names, which will be used to label
  variables in the output.

- wrap.labels:

  Numeric, determines how many chars of the value, variable or axis
  labels are displayed in one line and when a line break is inserted.

- show.p:

  Logical, if `TRUE`, p-values are also printed.

- p.numeric:

  Logical, if `TRUE`, the p-values are printed as numbers. If `FALSE`
  (default), asterisks are used.

- fade.ns:

  Logical, if `TRUE` (default), non-significant correlation-values
  appear faded (by using a lighter grey text color). See 'Note'.

- val.rm:

  Specify a number between 0 and 1 to suppress the output of correlation
  values that are smaller than `val.rm`. The absolute correlation values
  are used, so a correlation value of `-.5` would be greater than
  `val.rm = .4` and thus not be omitted. By default, this argument is
  `NULL`, hence all values are shown in the table. If a correlation
  value is below the specified value of `val.rm`, it is still printed to
  the HTML table, but made "invisible" with white foreground color. You
  can use the `CSS` argument (`"css.valueremove"`) to change color and
  appearance of those correlation value that are smaller than the limit
  specified by `val.rm`.

- digits:

  Amount of decimals for estimates

- triangle:

  Indicates whether only the upper right (use `"upper"`), lower left
  (use `"lower"`) or both (use `"both"`) triangles of the correlation
  table is filled with values. Default is `"both"`. You can specifiy the
  inital letter only.

- string.diag:

  A vector with string values of the same length as `ncol(data)` (number
  of correlated items) that can be used to display content in the
  diagonal cells where row and column item are identical (i.e. the
  "self-correlation"). By defauilt, this argument is `NULL` and the
  diagnal cells are empty.

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

## Note

If `data` is a matrix with correlation coefficients as returned by the
[`cor`](https://rdrr.io/r/stats/cor.html)-function, p-values can't be
computed. Thus, `show.p`, `p.numeric` and `fade.ns` only have an effect
if `data` is a [`data.frame`](https://rdrr.io/r/base/data.frame.html).

## Examples

``` r
if (FALSE) { # \dontrun{
if (interactive()) {
  # Data from the EUROFAMCARE sample dataset
  library(sjmisc)
  data(efc)

  # retrieve variable and value labels
  varlabs <- get_label(efc)

  # recveive first item of COPE-index scale
  start <- which(colnames(efc) == "c83cop2")
  # recveive last item of COPE-index scale
  end <- which(colnames(efc) == "c88cop7")

  # create data frame with COPE-index scale
  mydf <- data.frame(efc[, c(start:end)])
  colnames(mydf) <- varlabs[c(start:end)]

  # we have high correlations here, because all items
  # belong to one factor.
  tab_corr(mydf, p.numeric = TRUE)

  # auto-detection of labels, only lower triangle
  tab_corr(efc[, c(start:end)], triangle = "lower")

  # auto-detection of labels, only lower triangle, all correlation
  # values smaller than 0.3 are not shown in the table
  tab_corr(efc[, c(start:end)], triangle = "lower", val.rm = 0.3)

  # auto-detection of labels, only lower triangle, all correlation
  # values smaller than 0.3 are printed in blue
  tab_corr(efc[, c(start:end)], triangle = "lower",val.rm = 0.3,
           CSS = list(css.valueremove = 'color:blue;'))
}} # }
```
