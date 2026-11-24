# Customizing HTML tables

All `tab_*`-functions create a HTML page with the table output. This
table, by default, is opened in the viewer pane of your IDE (in case
you’re using an IDE that also supports the viewer pane). If a viewer
pane is not available, the created HTML output is saved as temporary
file and opened in your default web browser. The temporary files are
deleted after your R session ends.

## Copying table output to office or word processors

### Export table as HTML file to open in word processors

You can save the HTML page as file for further usage by specifying the
`file`-argument The saved HTML file can be opened by word processors
like LibreOffice or Microsoft Office.

### Drag and drop from browser or RStudio viewer pane

You can directly drag and drop a table from the RStudio viewer pane or
browser into your word processor. Simply select the complete table with
your mouse and drag it into office.

## Customizing table output with the CSS parameter

The table output is in in HTML format. The table style (visual
appearance) is formatted using *Cascading Style Sheets* (CSS). If you
are a bit familiar with these topics, you can easily customize the
appearance of the table output.

Many table elements (header, row, column, cell, summary row, first row
or column…) have CSS-class attributes, which can be used to change the
table style. Since each `sjt.*` function as well as
[`tab_model()`](https://strengejacke.github.io/sjPlot/reference/tab_model.md)
has different table elements and thus different class attributes, you
first need to know which styles can be customized.

### Retrieving customizable styles

The table functions invisibly return several values. The return value
`page.style` contains the style information for the HTML table. You can
print this style sheet to console using the
[`cat()`](https://rdrr.io/r/base/cat.html)-function:

``` r

library(sjPlot)
data(efc)
m <- lm(barthtot ~ c160age + c12hour + c161sex + c172code, data = efc)
tab <- tab_model(m)
```

``` r

cat(tab$page.style)
#> <style>
#> html, body { background-color: white; }
#> table { border-collapse:collapse; border:none; }
#> caption { font-weight: bold; text-align:left; }
#> td {  }
#> .thead { border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; }
#> .tdata { padding:0.2cm; text-align:left; vertical-align:top; }
#> .arc { background-color:#f2f2f2; }
#> .summary { padding-top:0.1cm; padding-bottom:0.1cm; }
#> .summarydata { text-align:left; }
#> .fixedparts { font-weight:bold; text-align:left; }
#> .randomparts { font-weight:bold; text-align:left; padding-top:.8em; }
#> .zeroparts { font-weight:bold; text-align:left; padding-top:.8em; }
#> .simplexparts { font-weight:bold; text-align:left; padding-top:.8em; }
#> .lasttablerow { border-bottom: double; }
#> .firsttablerow {  }
#> .firstsumrow { border-top:1px solid; }
#> .labelcellborder { border-bottom:1px solid; }
#> .depvarhead { text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal; }
#> .depvarheadnodv { border-top: double; text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal; }
#> .leftalign { text-align:left; }
#> .centeralign { text-align:center; }
#> .firsttablecol { text-align:left; }
#> .footnote { font-style:italic; border-top:double black; text-align:right; }
#> .subtitle { font-weight: normal; }
#> .modelcolumn1 {  }
#> .modelcolumn2 {  }
#> .modelcolumn3 {  }
#> .modelcolumn4 {  }
#> .modelcolumn5 {  }
#> .modelcolumn6 {  }
#> .modelcolumn7 {  }
#> .col1 {  }
#> .col2 {  }
#> .col3 {  }
#> .col4 {  }
#> .col5 {  }
#> .col6 {  }
#> </style>
```

The HTML code is in the `page.content` return value. The following code
prints the HTML code of the table to the R console:

``` r

cat(tab$page.content)
#> <table>
#> <tr>
#> <th class="thead firsttablerow firsttablecol col1">&nbsp;</th>
#> <th colspan="3" class="thead firsttablerow">Total score BARTHEL INDEX</th>
#> </tr>
#> <tr>
#> <td class="depvarhead firsttablerow firsttablecol col1">Predictors</td>
#> <td class="depvarhead firsttablerow col2">Estimates</td>
#> <td class="depvarhead firsttablerow col3">CI</td>
#> <td class="depvarhead firsttablerow col4">p</td>
#> </tr>
#> <tr>
#> <td class="tdata firsttablecol col1">(Intercept)</td>
#> <td class="tdata centeralign modelcolumn1 col2">90.06</td>
#> <td class="tdata centeralign modelcolumn1 col3">77.95&nbsp;&ndash;&nbsp;102.18</td>
#> <td class="tdata centeralign modelcolumn1 col4"><strong>&lt;0.001</strong></td>
#> </tr>
#> <tr>
#> <td class="tdata firsttablecol col1">carer' age</td>
#> <td class="tdata centeralign modelcolumn1 col2">&#45;0.22</td>
#> <td class="tdata centeralign modelcolumn1 col3">&#45;0.36&nbsp;&ndash;&nbsp;-0.08</td>
#> <td class="tdata centeralign modelcolumn1 col4"><strong>0.002</strong></td>
#> </tr>
#> <tr>
#> <td class="tdata firsttablecol col1">average number of hours<br>of care per week</td>
#> <td class="tdata centeralign modelcolumn1 col2">&#45;0.28</td>
#> <td class="tdata centeralign modelcolumn1 col3">&#45;0.31&nbsp;&ndash;&nbsp;-0.24</td>
#> <td class="tdata centeralign modelcolumn1 col4"><strong>&lt;0.001</strong></td>
#> </tr>
#> <tr>
#> <td class="tdata firsttablecol col1">carer's gender</td>
#> <td class="tdata centeralign modelcolumn1 col2">&#45;0.26</td>
#> <td class="tdata centeralign modelcolumn1 col3">&#45;4.36&nbsp;&ndash;&nbsp;3.83</td>
#> <td class="tdata centeralign modelcolumn1 col4">0.900</td>
#> </tr>
#> <tr>
#> <td class="tdata firsttablecol col1">carer's level of<br>education</td>
#> <td class="tdata centeralign modelcolumn1 col2">&#45;0.76</td>
#> <td class="tdata centeralign modelcolumn1 col3">&#45;3.55&nbsp;&ndash;&nbsp;2.02</td>
#> <td class="tdata centeralign modelcolumn1 col4">0.592</td>
#> </tr>
#> <tr>
#> <td class="tdata leftalign summary firstsumrow">Observations</td>
#> <td class="tdata summary summarydata firstsumrow" colspan="3">821</td>
#> </tr>
#> <tr>
#> <td class="tdata leftalign summary">R<sup>2</sup> / R<sup>2</sup> adjusted</td>
#> <td class="tdata summary summarydata" colspan="3">0.270 / 0.266</td>
#> </tr>
#> 
#> </table>
```

Now you can see which table elements are associated with which CSS class
attributes.

## Customizing table output with the CSS parameter

You can customize the table output with the `CSS` parameter. This
parameter requires a list of attributes, which follow a certain pattern:

1.  each attributes needs a `css.` prefix
2.  followed by the class name (e.g. `caption`, `thead`, `centeralign`,
    etc.)
3.  equal-sign
4.  the CSS format (in (single) quotation marks)
5.  the CSS format must end with a colon (;)

Example:

``` r

tab_model(
  m,
  CSS = list(
    css.depvarhead = 'color: red;',
    css.centeralign = 'text-align: left;', 
    css.firsttablecol = 'font-weight: bold;', 
    css.summary = 'color: blue;'
  )
)
```

[TABLE]

In the above example, the header row lost the original style and just
became red. If you want to keep the original style and just add
additional style information, use the plus-sign (+) as initial character
for the parameter attributes. In the following example, the header row
keeps its original style and is additionally printed in red:

``` r

tab_model(m, CSS = list(css.depvarhead = '+color: red;'))
```

[TABLE]

## Pre-defined Table-Layouts

There are a few pre-defined CSS-themes, which can be accessed with the
[`css_theme()`](https://strengejacke.github.io/sjPlot/reference/sjPlot-themes.md)-function.
There are more pre-defined themes planned for the future.

``` r

tab_model(m, CSS = css_theme("cells"))
```

[TABLE]
