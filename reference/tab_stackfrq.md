# Summary of stacked frequencies as HTML table

Shows the results of stacked frequencies (such as likert scales) as HTML
table. This function is useful when several items with identical
scale/categories should be printed as table to compare their
distributions (e.g. when plotting scales like SF, Barthel-Index,
Quality-of-Life-scales etc.).

## Usage

``` r
tab_stackfrq(
  items,
  weight.by = NULL,
  title = NULL,
  var.labels = NULL,
  value.labels = NULL,
  wrap.labels = 20,
  sort.frq = NULL,
  alternate.rows = FALSE,
  digits = 2,
  string.total = "N",
  string.na = "NA",
  show.n = FALSE,
  show.total = FALSE,
  show.na = FALSE,
  show.skew = FALSE,
  show.kurtosis = FALSE,
  digits.stats = 2,
  file = NULL,
  encoding = NULL,
  CSS = NULL,
  use.viewer = TRUE,
  remove.spaces = TRUE
)
```

## Arguments

- items:

  Data frame, or a grouped data frame, with each column representing one
  item.

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

- sort.frq:

  logical, indicates whether the `items` should be ordered by by highest
  count of first or last category of `items`.

  - Use `"first.asc"` to order ascending by lowest count of first
    category,

  - `"first.desc"` to order descending by lowest count of first
    category,

  - `"last.asc"` to order ascending by lowest count of last category,

  - `"last.desc"` to order descending by lowest count of last category,

  - or `NULL` (default) for no sorting.

- alternate.rows:

  Logical, if `TRUE`, rows are printed in alternatig colors (white and
  light grey by default).

- digits:

  Numeric, amount of digits after decimal point when rounding values.

- string.total:

  label for the total N column.

- string.na:

  label for the missing column/row.

- show.n:

  logical, if `TRUE`, adds total number of cases for each group or
  category to the labels.

- show.total:

  logical, if `TRUE`, an additional column with each item's total N is
  printed.

- show.na:

  logical, if `TRUE`, [`NA`](https://rdrr.io/r/base/NA.html)'s (missing
  values) are added to the output.

- show.skew:

  logical, if `TRUE`, an additional column with each item's skewness is
  printed. The skewness is retrieved from the
  [`describe`](https://rdrr.io/pkg/psych/man/describe.html)-function of
  the psych-package.

- show.kurtosis:

  Logical, if `TRUE`, the kurtosis for each item will also be shown (see
  [`kurtosi`](https://rdrr.io/pkg/psych/man/skew.html) and
  [`describe`](https://rdrr.io/pkg/psych/man/describe.html) in the
  `psych`-package for more details.

- digits.stats:

  amount of digits for rounding the skewness and kurtosis valuess.
  Default is 2, i.e. skewness and kurtosis values have 2 digits after
  decimal point.

- file:

  Destination file, if the output should be saved as file. If `NULL`
  (default), the output will be saved as temporary file and opened
  either in the IDE's viewer pane or the default web browser.

- encoding:

  Character vector, indicating the charset encoding used for variable
  and value labels. Default is `"UTF-8"`. For Windows Systems,
  `encoding = "Windows-1252"` might be necessary for proper display of
  special characters.

- CSS:

  A [`list`](https://rdrr.io/r/base/list.html) with user-defined
  style-sheet-definitions, according to the [official CSS
  syntax](https://www.w3.org/Style/CSS/). See 'Details' or [this
  package-vignette](https://strengejacke.github.io/sjPlot/articles/table_css.html).

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
# -------------------------------
# random sample
# -------------------------------
# prepare data for 4-category likert scale, 5 items
likert_4 <- data.frame(
  as.factor(sample(1:4, 500, replace = TRUE, prob = c(0.2, 0.3, 0.1, 0.4))),
  as.factor(sample(1:4, 500, replace = TRUE, prob = c(0.5, 0.25, 0.15, 0.1))),
  as.factor(sample(1:4, 500, replace = TRUE, prob = c(0.25, 0.1, 0.4, 0.25))),
  as.factor(sample(1:4, 500, replace = TRUE, prob = c(0.1, 0.4, 0.4, 0.1))),
  as.factor(sample(1:4, 500, replace = TRUE, prob = c(0.35, 0.25, 0.15, 0.25)))
)

# create labels
levels_4 <- c("Independent", "Slightly dependent",
              "Dependent", "Severely dependent")

# create item labels
items <- c("Q1", "Q2", "Q3", "Q4", "Q5")

# plot stacked frequencies of 5 (ordered) item-scales
if (FALSE) { # \dontrun{
if (interactive()) {
  tab_stackfrq(likert_4, value.labels = levels_4, var.labels = items)

  # -------------------------------
  # Data from the EUROFAMCARE sample dataset
  #  Auto-detection of labels
  # -------------------------------
  data(efc)
  # recveive first item of COPE-index scale
  start <- which(colnames(efc) == "c82cop1")
  # recveive first item of COPE-index scale
  end <- which(colnames(efc) == "c90cop9")

  tab_stackfrq(efc[, c(start:end)], alternate.rows = TRUE)

  tab_stackfrq(efc[, c(start:end)], alternate.rows = TRUE,
               show.n = TRUE, show.na = TRUE)

  # --------------------------------
  # User defined style sheet
  # --------------------------------
  tab_stackfrq(efc[, c(start:end)], alternate.rows = TRUE,
               show.total = TRUE, show.skew = TRUE, show.kurtosis = TRUE,
               CSS = list(css.ncol = "border-left:1px dotted black;",
                          css.summary = "font-style:italic;"))
}
} # }
```
