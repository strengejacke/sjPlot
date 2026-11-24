# Summary of item analysis of an item scale as HTML table

This function performs an item analysis with certain statistics that are
useful for scale or index development. The resulting tables are shown in
the viewer pane resp. webbrowser or can be saved as file. Following
statistics are computed for each item of a data frame:

- percentage of missing values

- mean value

- standard deviation

- skew

- item difficulty

- item discrimination

- Cronbach's Alpha if item was removed from scale

- mean (or average) inter-item-correlation

Optional, following statistics can be computed as well:

- kurstosis

- Shapiro-Wilk Normality Test

If `factor.groups` is not `NULL`, the data frame `df` will be splitted
into groups, assuming that `factor.groups` indicate those columns of the
data frame that belong to a certain factor (see return value of function
[`tab_pca`](https://strengejacke.github.io/sjPlot/reference/tab_pca.md)
as example for retrieving factor groups for a scale and see examples for
more details).

## Usage

``` r
tab_itemscale(
  df,
  factor.groups = NULL,
  factor.groups.titles = "auto",
  scale = FALSE,
  min.valid.rowmean = 2,
  alternate.rows = TRUE,
  sort.column = NULL,
  show.shapiro = FALSE,
  show.kurtosis = FALSE,
  show.corr.matrix = TRUE,
  CSS = NULL,
  encoding = NULL,
  file = NULL,
  use.viewer = TRUE,
  remove.spaces = TRUE
)

sjt.itemanalysis(
  df,
  factor.groups = NULL,
  factor.groups.titles = "auto",
  scale = FALSE,
  min.valid.rowmean = 2,
  alternate.rows = TRUE,
  sort.column = NULL,
  show.shapiro = FALSE,
  show.kurtosis = FALSE,
  show.corr.matrix = TRUE,
  CSS = NULL,
  encoding = NULL,
  file = NULL,
  use.viewer = TRUE,
  remove.spaces = TRUE
)
```

## Arguments

- df:

  A data frame with items.

- factor.groups:

  If not `NULL`, `df` will be splitted into sub-groups, where the item
  analysis is carried out for each of these groups. Must be a vector of
  same length as `ncol(df)`, where each item in this vector represents
  the group number of the related columns of `df`. If
  `factor.groups = "auto"`, a principal component analysis with Varimax
  rotation is performed, and the resulting groups for the components are
  used as group index. See 'Examples'.

- factor.groups.titles:

  Titles for each factor group that will be used as table caption for
  each component-table. Must be a character vector of same length as
  `length(unique(factor.groups))`. Default is `"auto"`, which means that
  each table has a standard caption *Component x*. Use `NULL` to
  suppress table captions.

- scale:

  Logical, if `TRUE`, the data frame's vectors will be scaled when
  calculating the Cronbach's Alpha value (see
  [`item_reliability`](https://easystats.github.io/performance/reference/item_reliability.html)).
  Recommended, when the variables have different measures / scales.

- min.valid.rowmean:

  Minimum amount of valid values to compute row means for index scores.
  Default is 2, i.e. the return values `index.scores` and
  `df.index.scores` are computed for those items that have at least
  `min.valid.rowmean` per case (observation, or technically, row). See
  `mean_n` for details.

- alternate.rows:

  Logical, if `TRUE`, rows are printed in alternatig colors (white and
  light grey by default).

- sort.column:

  Numeric vector, indicating the index of the column that should sorted.
  by default, the column is sorted in ascending order. Use negative
  index for descending order, for instance, `sort.column = -3` would
  sort the third column in descending order. Note that the first column
  with rownames is not counted.

- show.shapiro:

  Logical, if `TRUE`, a Shapiro-Wilk normality test is computed for each
  item. See [`shapiro.test`](https://rdrr.io/r/stats/shapiro.test.html)
  for details.

- show.kurtosis:

  Logical, if `TRUE`, the kurtosis for each item will also be shown (see
  [`kurtosi`](https://rdrr.io/pkg/psych/man/skew.html) and
  [`describe`](https://rdrr.io/pkg/psych/man/describe.html) in the
  `psych`-package for more details.

- show.corr.matrix:

  Logical, if `TRUE` (default), a correlation matrix of each component's
  index score is shown. Only applies if `factor.groups` is not `NULL`
  and `df` has more than one group. First, for each case (df's row), the
  sum of all variables (df's columns) is scaled (using the
  [`scale`](https://rdrr.io/r/base/scale.html)-function) and represents
  a "total score" for each component (a component is represented by each
  group of `factor.groups`). After that, each case (df's row) has a
  scales sum score for each component. Finally, a correlation of these
  "scale sum scores" is computed.

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

- `df.list`: List of data frames with the item analysis for each
  sub.group (or complete, if `factor.groups` was `NULL`)

- `index.scores`: A data frame with of standardized scale / index scores
  for each case (mean value of all scale items for each case) for each
  sub-group.

- `ideal.item.diff`: List of vectors that indicate the ideal item
  difficulty for each item in each sub-group. Item difficulty only
  differs when items have different levels.

- `cronbach.values`: List of Cronbach's Alpha values for the overall
  item scale for each sub-group.

- `knitr.list`: List of html-tables with inline-css for use with knitr
  for each table (sub-group)

- `knitr`: html-table of all complete output with inline-css for use
  with knitr

- `complete.page`: Complete html-output.

If `factor.groups = NULL`, each list contains only one elment, since
just one table is printed for the complete scale indicated by `df`. If
`factor.groups` is a vector of group-index-values, the lists contain
elements for each sub-group.

## Note

- The *Shapiro-Wilk Normality Test* (see column `W(p)`) tests if an item
  has a distribution that is significantly different from normal.

- *Item difficulty* should range between 0.2 and 0.8. Ideal value is
  `p+(1-p)/2` (which mostly is between 0.5 and 0.8).

- For *item discrimination*, acceptable values are 0.20 or higher; the
  closer to 1.00 the better. See
  [`item_reliability`](https://easystats.github.io/performance/reference/item_reliability.html)
  for more details.

- In case the total *Cronbach's Alpha* value is below the acceptable
  cut-off of 0.7 (mostly if an index has few items), the *mean
  inter-item-correlation* is an alternative measure to indicate
  acceptability. Satisfactory range lies between 0.2 and 0.4. See also
  [`item_intercor`](https://easystats.github.io/performance/reference/item_intercor.html).

## References

- Jorion N, Self B, James K, Schroeder L, DiBello L, Pellegrino J (2013)
  Classical Test Theory Analysis of the Dynamics Concept Inventory.
  ([web](https://www.academia.edu/4104752/Classical_Test_Theory_Analysis_of_the_Dynamics_Concept_Inventory))

- Briggs SR, Cheek JM (1986) The role of factor analysis in the
  development and evaluation of personality scales. Journal of
  Personality, 54(1), 106-148. doi: 10.1111/j.1467-6494.1986.tb00391.x

- McLean S et al. (2013) Stigmatizing attitudes and beliefs about
  bulimia nervosa: Gender, age, education and income variability in a
  community sample. International Journal of Eating Disorders. doi:
  10.1002/eat.22227

- Trochim WMK (2008) Types of Reliability.

## Examples

``` r
# Data from the EUROFAMCARE sample dataset
library(sjmisc)
library(sjlabelled)
data(efc)

# retrieve variable and value labels
varlabs <- get_label(efc)

# recveive first item of COPE-index scale
start <- which(colnames(efc) == "c82cop1")
# recveive last item of COPE-index scale
end <- which(colnames(efc) == "c90cop9")

# create data frame with COPE-index scale
mydf <- data.frame(efc[, start:end])
colnames(mydf) <- varlabs[start:end]

if (FALSE) { # \dontrun{
if (interactive()) {
  tab_itemscale(mydf)

  # auto-detection of labels
  tab_itemscale(efc[, start:end])

  # Compute PCA on Cope-Index, and perform a
  # item analysis for each extracted factor.
  indices <- tab_pca(mydf)$factor.index
  tab_itemscale(mydf, factor.groups = indices)

  # or, equivalent
  tab_itemscale(mydf, factor.groups = "auto")
}} # }
```
