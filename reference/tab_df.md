# Print data frames as HTML table.

These functions print data frames as HTML-table, showing the results in
RStudio's viewer pane or in a web browser.

## Usage

``` r
tab_df(
  x,
  title = NULL,
  footnote = NULL,
  col.header = NULL,
  show.type = FALSE,
  show.rownames = FALSE,
  show.footnote = FALSE,
  alternate.rows = FALSE,
  sort.column = NULL,
  digits = 2,
  encoding = "UTF-8",
  CSS = NULL,
  file = NULL,
  use.viewer = TRUE,
  ...
)

tab_dfs(
  x,
  titles = NULL,
  footnotes = NULL,
  col.header = NULL,
  show.type = FALSE,
  show.rownames = FALSE,
  show.footnote = FALSE,
  alternate.rows = FALSE,
  sort.column = NULL,
  digits = 2,
  encoding = "UTF-8",
  CSS = NULL,
  file = NULL,
  use.viewer = TRUE,
  rnames = NULL,
  ...
)
```

## Arguments

- x:

  For `tab_df()`, a data frame; and for `tab_dfs()`, a list of data
  frames.

- title, titles, footnote, footnotes:

  Character vector with table caption(s) resp. footnote(s). For
  `tab_df()`, must be a character of length 1; for `tab_dfs()`, a
  character vector of same length as `x` (i.e. one title or footnote per
  data frame).

- col.header:

  Character vector with elements used as column header for the table. If
  `NULL`, column names from `x` are used as column header.

- show.type:

  Logical, if `TRUE`, adds information about the variable type to the
  variable column.

- show.rownames:

  Logical, if `TRUE`, adds a column with the data frame's rowname to the
  table output.

- show.footnote:

  Logical, if `TRUE`,adds a summary footnote below the table. For
  `tab_df()`, specify the string in `footnote`, for `tab_dfs()` provide
  a character vector in `footnotes`.

- alternate.rows:

  Logical, if `TRUE`, rows are printed in alternatig colors (white and
  light grey by default).

- sort.column:

  Numeric vector, indicating the index of the column that should sorted.
  by default, the column is sorted in ascending order. Use negative
  index for descending order, for instance, `sort.column = -3` would
  sort the third column in descending order. Note that the first column
  with rownames is not counted.

- digits:

  Numeric, amount of digits after decimal point when rounding values.

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

- file:

  Destination file, if the output should be saved as file. If `NULL`
  (default), the output will be saved as temporary file and opened
  either in the IDE's viewer pane or the default web browser.

- use.viewer:

  Logical, if `TRUE`, the HTML table is shown in the IDE's viewer pane.
  If `FALSE` or no viewer available, the HTML table is opened in a web
  browser.

- ...:

  Currently not used.

- rnames:

  Character vector, can be used to set row names when
  `show.rownames=TRUE`.

## Value

A list with following items:

- the web page style sheet (`page.style`),

- the HTML content of the data frame (`page.content`),

- the complete HTML page, including header, style sheet and body
  (`page.complete`)

- the HTML table with inline-css for use with knitr (`knitr`)

- the file path, if the HTML page should be saved to disk (`file`)

## Details

**How do I use `CSS`-argument?**  
  
With the `CSS`-argument, the visual appearance of the tables can be
modified. To get an overview of all style-sheet-classnames that are used
in this function, see return value `page.style` for details. Arguments
for this list have following syntax:

1.  the class-name as argument name and

2.  each style-definition must end with a semicolon

You can add style information to the default styles by using a +
(plus-sign) as initial character for the argument attributes. Examples:

- `table = 'border:2px solid red;'` for a solid 2-pixel table border in
  red.

- `summary = 'font-weight:bold;'` for a bold fontweight in the summary
  row.

- `lasttablerow = 'border-bottom: 1px dotted blue;'` for a blue dotted
  border of the last table row.

- `colnames = '+color:green'` to add green color formatting to column
  names.

- `arc = 'color:blue;'` for a blue text color each 2nd row.

- `caption = '+color:red;'` to add red font-color to the default table
  caption style.

See further examples in [this
package-vignette](https://strengejacke.github.io/sjPlot/articles/table_css.html).

## Note

The HTML tables can either be saved as file and manually opened (use
argument `file`) or they can be saved as temporary files and will be
displayed in the RStudio Viewer pane (if working with RStudio) or opened
with the default web browser. Displaying resp. opening a temporary file
is the default behaviour.

## Examples

``` r
if (FALSE) { # \dontrun{
data(iris)
data(mtcars)
tab_df(iris[1:5, ])
tab_dfs(list(iris[1:5, ], mtcars[1:5, 1:5]))

# sort 2nd column ascending
tab_df(iris[1:5, ], sort.column = 2)

# sort 2nd column descending
tab_df(iris[1:5, ], sort.column = -2)} # }
```
