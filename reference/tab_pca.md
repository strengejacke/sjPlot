# Summary of principal component analysis as HTML table

Performes a principle component analysis on a data frame or matrix (with
varimax or oblimin rotation) and displays the factor solution as HTML
table, or saves them as file.  
  
In case a data frame is used as parameter, the Cronbach's Alpha value
for each factor scale will be calculated, i.e. all variables with the
highest loading for a factor are taken for the reliability test. The
result is an alpha value for each factor dimension.

## Usage

``` r
tab_pca(
  data,
  rotation = c("varimax", "quartimax", "promax", "oblimin", "simplimax", "cluster",
    "none"),
  nmbr.fctr = NULL,
  fctr.load.tlrn = 0.1,
  title = "Principal Component Analysis",
  var.labels = NULL,
  wrap.labels = 40,
  show.cronb = TRUE,
  show.msa = FALSE,
  show.var = FALSE,
  alternate.rows = FALSE,
  digits = 2,
  string.pov = "Proportion of Variance",
  string.cpov = "Cumulative Proportion",
  CSS = NULL,
  encoding = NULL,
  file = NULL,
  use.viewer = TRUE,
  remove.spaces = TRUE
)
```

## Arguments

- data:

  A data frame that should be used to compute a PCA, or a
  [`prcomp`](https://rdrr.io/r/stats/prcomp.html) object.

- rotation:

  Rotation of the factor loadings. May be one of
  `"varimax", "quartimax", "promax", "oblimin", "simplimax", "cluster"`
  or `"none"`.

- nmbr.fctr:

  Number of factors used for calculating the rotation. By default, this
  value is `NULL` and the amount of factors is calculated according to
  the Kaiser-criteria.

- fctr.load.tlrn:

  Specifies the minimum difference a variable needs to have between
  factor loadings (components) in order to indicate a clear loading on
  just one factor and not diffusing over all factors. For instance, a
  variable with 0.8, 0.82 and 0.84 factor loading on 3 possible factors
  can not be clearly assigned to just one factor and thus would be
  removed from the principal component analysis. By default, the minimum
  difference of loading values between the highest and 2nd highest
  factor should be 0.1

- title:

  String, will be used as table caption.

- var.labels:

  Character vector with variable names, which will be used to label
  variables in the output.

- wrap.labels:

  Numeric, determines how many chars of the value, variable or axis
  labels are displayed in one line and when a line break is inserted.

- show.cronb:

  Logical, if `TRUE` (default), the cronbach's alpha value for each
  factor scale will be calculated, i.e. all variables with the highest
  loading for a factor are taken for the reliability test. The result is
  an alpha value for each factor dimension. Only applies when `data` is
  a data frame.

- show.msa:

  Logical, if `TRUE`, shows an additional column with the measure of
  sampling adequacy according dor each component.

- show.var:

  Logical, if `TRUE`, the proportions of variances for each component as
  well as cumulative variance are shown in the table footer.

- alternate.rows:

  Logical, if `TRUE`, rows are printed in alternatig colors (white and
  light grey by default).

- digits:

  Amount of decimals for estimates

- string.pov:

  String for the table row that contains the proportions of variances.
  By default, *"Proportion of Variance"* will be used.

- string.cpov:

  String for the table row that contains the cumulative variances. By
  default, *"Cumulative Proportion"* will be used.

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

- the complete html-output (`page.complete`),

- the html-table with inline-css for use with knitr (`knitr`),

- the `factor.index`, i.e. the column index of each variable with the
  highest factor loading for each factor and

- the `removed.items`, i.e. which variables have been removed because
  they were outside of the `fctr.load.tlrn`'s range.

for further use.

## Examples

``` r
if (FALSE) { # \dontrun{
# Data from the EUROFAMCARE sample dataset
library(sjmisc)
data(efc)

# recveive first item of COPE-index scale
start <- which(colnames(efc) == "c82cop1")
# recveive last item of COPE-index scale
end <- which(colnames(efc) == "c90cop9")
# auto-detection of labels
if (interactive()) {
  tab_pca(efc[, start:end])
}} # }
```
