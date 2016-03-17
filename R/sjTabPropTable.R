#' @title Summary of contingency tables as HTML table
#' @name sjt.xtab
#' 
#' @description Shows contingency tables as HTML file in browser or viewer pane, or saves them as file.
#' 
#' @seealso \itemize{
#'            \item \href{http://www.strengejacke.de/sjPlot/sjt.xtab}{sjPlot manual: sjt.xtab}
#'            \item \code{\link{sjp.xtab}}
#'          }
#'              
#' @param var.row variable that should be displayed in the table rows.
#' @param var.col variable that should be displayed in the table columns.
#' @param weightBy weight factor that will be applied to weight all cases.
#'          Must be a vector of same length as \code{var.row}. Default is \code{NULL}, so no weights are used.
#' @param digits amount of digits used for the percentage values inside table cells.
#'          Default is 1.
#' @param variableLabels character vector of same length as supplied variables, with 
#'          the associated variable names. Following order is needed: name of \code{var.row},
#'          name of \code{var.col}, and - if \code{var.grp} is not \code{NULL} - name of \code{var.grp}.
#'          See 'Examples'.
#'          Variable labels are detected automatically, if \code{var.row} or \code{var.col}
#'          have label attributes (see \code{\link[sjmisc]{set_label}}) for details).
#' @param valueLabels \code{\link{list}} of two character vectors that indicate the value labels of the supplied
#'          variables. Following order is needed: value labels of \code{var.row}
#'          and value labels of \code{var.col}. See 'Examples'.
#' @param breakVariableLabelsAt determines how many chars of the variable labels are displayed in 
#'          one line and when a line break is inserted. Default is 40.
#' @param breakValueLabelsAt determines how many chars of the value labels are displayed in 
#'          one line and when a line break is inserted. Default is 20.
#' @param stringTotal label for the total column / row header
#' @param showCellPerc logical, if \code{TRUE}, cell percentage values are shown
#' @param showRowPerc logical, if \code{TRUE}, row percentage values are shown
#' @param showColPerc logical, if \code{TRUE}, column percentage values are shown
#' @param showObserved logical, if \code{TRUE}, observed values are shown
#' @param showExpected logical, if \code{TRUE}, expected values are also shown
#' @param showHorizontalLine logical, if \code{TRUE}, data rows are separated with a horizontal line
#' @param showSummary logical, if \code{TRUE} (default), a summary row with chi-square statistics (see \code{\link{chisq.test}}),
#'          Cramer's V or Phi-value etc. is shown. If a cell contains expected values lower than five (or lower than 10 
#'          if df is 1), the Fisher's excact test (see \code{\link{fisher.test}}) is computed instead of chi-square test. 
#'          If the table's matrix is larger than 2x2, Fisher's excact test with Monte Carlo simulation is computed.
#' @param showLegend logical, if \code{TRUE}, the color legend for coloring observed and expected
#'          values as well as cell, row and column percentages is shown. See \code{tdcol.n},
#'          \code{tdcol.expected}, \code{tdcol.cell}, \code{tdcol.row} and \code{tdcol.col}.
#' @param showNA logical, if \code{TRUE}, \code{\link{NA}}'s (missing values) are also printed in the table.
#' @param labelNA The label for the missing column/row.
#' @param tdcol.n Color for highlighting count (observed) values in table cells. Default is black.
#' @param tdcol.expected Color for highlighting expected values in table cells. Default is cyan.
#' @param tdcol.cell Color for highlighting cell percentage values in table cells. Default is red.
#' @param tdcol.row Color for highlighting row percentage values in table cells. Default is blue.
#' @param tdcol.col Color for highlighting column percentage values in table cells. Default is green.
#' @param highlightTotal logical, if \code{TRUE}, the total column and row will be highlighted with a
#'          different background color. See \code{highlightColor}.
#' @param highlightColor logical, if \code{highlightTotal = TRUE}, this color value will be used
#'          for painting the background of the total column and row. Default is a light grey.
#' @param percSign The percentage sign that is printed in the table cells, in HTML-format.
#'          Default is \code{"&nbsp;\%"}, hence the percentage sign has a non-breaking-space after
#'          the percentage value.
#' @param hundret Default value that indicates the 100-percent column-sums (since rounding values
#'          may lead to non-exact results). Default is \code{"100.0"}.
#'          
#' @inheritParams sjt.frq
#' @inheritParams sjt.df
#'          
#' @return Invisibly returns
#'          \itemize{
#'            \item the web page style sheet (\code{page.style}),
#'            \item the web page content (\code{page.content}),
#'            \item the complete html-output (\code{output.complete}) and
#'            \item the html-table with inline-css for use with knitr (\code{knitr})
#'            }
#'            for further use.
#'          
#' @note See 'Notes' in \code{\link{sjt.frq}}.
#'  
#' @details See 'Details' in \code{\link{sjt.frq}}.
#'         
#' @examples 
#' # prepare sample data set
#' library(sjmisc)
#' data(efc)
#' efc.labels <- get_labels(efc)
#' 
#' # print simple cross table with labels
#' \dontrun{
#' sjt.xtab(efc$e16sex, efc$e42dep)
#'          
#' # print cross table with manually set
#' # labels and expected values
#' sjt.xtab(efc$e16sex, 
#'          efc$e42dep, 
#'          variableLabels = c("Elder's gender", 
#'                             "Elder's dependency"),
#'          valueLabels = list(efc.labels[['e16sex']], 
#'                             efc.labels[['e42dep']]),
#'          showExpected = TRUE)
#' 
#' # print minimal cross table with labels, total col/row highlighted
#' sjt.xtab(efc$e16sex, efc$e42dep, 
#'          showHorizontalLine = FALSE,
#'          showCellPerc = FALSE,
#'          highlightTotal = TRUE)
#' 
#' # ---------------------------------------------------------------- 
#' # User defined style sheet
#' # ---------------------------------------------------------------- 
#' sjt.xtab(efc$e16sex, efc$e42dep, 
#'          CSS = list(css.table = "border: 2px solid;",
#'                     css.tdata = "border: 1px solid;",
#'                     css.horline = "border-bottom: double blue;"))}
#'
#' @import sjmisc
#' @importFrom stats xtabs ftable
#' @export
sjt.xtab <- function(var.row,
                     var.col,
                     weightBy = NULL,
                     digits = 1,
                     file = NULL,
                     variableLabels = NULL,
                     valueLabels = NULL,
                     title = NULL,
                     breakVariableLabelsAt = 40,
                     breakValueLabelsAt = 20,
                     stringTotal = "Total",
                     showObserved = TRUE,
                     showCellPerc = FALSE,
                     showRowPerc = FALSE,
                     showColPerc = FALSE,
                     showExpected = FALSE,
                     showHorizontalLine = FALSE,
                     showSummary = TRUE,
                     showLegend = FALSE,
                     showNA = FALSE,
                     labelNA = "NA",
                     tdcol.n = "black",
                     tdcol.expected = "#339999",
                     tdcol.cell = "#993333",
                     tdcol.row = "#333399",
                     tdcol.col = "#339933",
                     highlightTotal = FALSE,
                     highlightColor = "#f8f8f8",
                     percSign = "&nbsp;&#37;",
                     hundret = "100.0",
                     encoding = NULL,
                     CSS = NULL,
                     useViewer = TRUE,
                     no.output = FALSE,
                     remove.spaces = TRUE) {
  # --------------------------------------------------------
  # check p-value-style option
  # --------------------------------------------------------
  opt <- getOption("p_zero")
  if (is.null(opt) || opt == FALSE) {
    p_zero <- ""
  } else {
    p_zero <- "0"
  }
  # -------------------------------------
  # check encoding
  # -------------------------------------
  encoding <- get.encoding(encoding)
  # --------------------------------------------------------
  # get variable name
  # --------------------------------------------------------
  var.name.row <- get_var_name(deparse(substitute(var.row)))
  var.name.col <- get_var_name(deparse(substitute(var.col)))
  # --------------------------------------------------------
  # create cross table of frequencies and percentages
  # --------------------------------------------------------
  mydat <- create.xtab.df(var.row,
                          var.col,
                          round.prz = digits,
                          na.rm = !showNA,
                          weightBy = weightBy)
  # --------------------------------------------------------
  # try to automatically set labels is not passed as parameter
  # --------------------------------------------------------
  if (is.null(variableLabels)) {
    variableLabels <- c(sjmisc::get_label(var.row, def.value = var.name.row),
                        sjmisc::get_label(var.col, def.value = var.name.col))
  }
  # wrap long labels
  variableLabels <- sjmisc::word_wrap(variableLabels, breakVariableLabelsAt, "<br>")
  s.var.row <- variableLabels[1]
  s.var.col <- variableLabels[2]
  # -------------------------------------
  # init variable labels
  # -------------------------------------
  labels.var.row <- mydat$labels.cnt
  labels.var.col <- mydat$labels.grp
  # do we have labels?
  if (!is.null(valueLabels)) {
    # need to be a list
    if (!is.list(valueLabels)) {
      warning("`valueLables` needs to be a `list`-object.", call. = F)
    } else {
      labels.var.row <- valueLabels[[1]]
      labels.var.col <- valueLabels[[2]]
    }
    # correct length of labels?
    if (length(labels.var.row) != length(mydat$labels.cnt)) {
      warning("Length of `valueLabels` does not match length of category values of `var.row`.", call. = F)
      labels.var.row <- mydat$labels.cnt
    }
    # correct length of labels?
    if (length(labels.var.col) != length(mydat$labels.grp)) {
      warning("Length of `valueLabels` does not match length of category values of `var.grp`.", call. = F)
      labels.var.col <- mydat$labels.grp
    }
  }
  # wrap labels
  labels.var.row <- sjmisc::word_wrap(labels.var.row, breakValueLabelsAt, "<br>")
  labels.var.col <- sjmisc::word_wrap(labels.var.col, breakValueLabelsAt, "<br>")
  # add "total"
  labels.var.row <- c(labels.var.row, stringTotal)
  labels.var.col <- c(labels.var.col)
  # -------------------------------------
  # compute table counts and percentages
  # -------------------------------------
  tab <- mydat$mydat[, -1]
  tab$total <- unname(rowSums(tab))
  tab <- rbind(tab, unname(colSums(tab)))
  tab.cell <- mydat$proptab.cell
  tab.row <- mydat$proptab.row
  tab.row$total <- 100
  tab.col <- mydat$proptab.col
  tab.col <- rbind(tab.col, rep(100, times = ncol(tab.col)))
  tab.expected <- sjmisc::table_values(stats::ftable(as.matrix(tab)))$expected
  # -------------------------------------
  # determine total number of columns and rows
  # -------------------------------------
  totalncol <- ncol(tab)
  totalnrow <- nrow(tab)
  # -------------------------------------
  # table init
  # -------------------------------------
  # init web page header
  toWrite <- sprintf("<html>\n<head>\n<meta http-equiv=\"Content-type\" content=\"text/html;charset=%s\">\n", encoding)
  # -------------------------------------
  # init style sheet and tags used for css-definitions
  # we can use these variables for string-replacement
  # later for return value
  # -------------------------------------
  tag.table <- "table"
  tag.caption <- "caption"
  tag.thead <- "thead"
  tag.tdata <- "tdata"
  tag.firstcolborder <- "firstcolborder"
  tag.secondtablerow <- "secondtablerow"
  tag.leftalign <- "leftalign"
  tag.centeralign <- "centeralign"
  tag.lasttablerow <- "lasttablerow"
  tag.totcol <- "totcol"
  tag.tothi <- "tothi"
  tag.summary <- "summary"
  tag.horline <- "horline"
  tag.td_ex <- "td_ex"
  tag.td_cl <- "td_cl"
  tag.td_rw <- "td_rw"
  tag.td_c <- "td_c"
  tag.td_n <- "td_n"
  css.table <- "border-collapse:collapse; border:none;"
  css.caption <- "font-weight: bold; text-align:left;"
  css.thead <- "border-top:double; text-align:center; font-style:italic; font-weight:normal;"
  css.tdata <- "padding:0.2cm;"
  css.firstcolborder <- "border-bottom:1px solid;"
  css.secondtablerow <- "border-bottom:1px solid; text-align:center;"
  css.leftalign <- "text-align:left; vertical-align:middle;"
  css.centeralign <- "text-align:center;"
  css.lasttablerow <- ifelse(isTRUE(highlightTotal), sprintf(" border-bottom:double; background-color:%s;", highlightColor), " border-bottom:double;")
  css.totcol <- ifelse(isTRUE(highlightTotal), sprintf(" background-color:%s;", highlightColor), "")
  css.tothi <- "font-weight:bolder; font-style:italic;"
  css.summary <- "text-align:right; font-size:0.9em; font-style:italic;"
  css.horline <- ifelse(isTRUE(showHorizontalLine), "border-bottom:1px solid;", "")
  # ------------------------
  # check user defined style sheets
  # ------------------------
  if (!is.null(CSS)) {
    if (!is.null(CSS[['css.table']])) css.table <- ifelse(substring(CSS[['css.table']], 1, 1) == '+', paste0(css.table, substring(CSS[['css.table']], 2)), CSS[['css.table']])
    if (!is.null(CSS[['css.caption']])) css.caption <- ifelse(substring(CSS[['css.caption']], 1, 1) == '+', paste0(css.caption, substring(CSS[['css.caption']], 2)), CSS[['css.caption']])
    if (!is.null(CSS[['css.thead']])) css.thead <- ifelse(substring(CSS[['css.thead']], 1, 1) == '+', paste0(css.thead, substring(CSS[['css.thead']], 2)), CSS[['css.thead']])
    if (!is.null(CSS[['css.tdata']])) css.tdata <- ifelse(substring(CSS[['css.tdata']], 1, 1) == '+', paste0(css.tdata, substring(CSS[['css.tdata']], 2)), CSS[['css.tdata']])
    if (!is.null(CSS[['css.summary']])) css.summary <- ifelse(substring(CSS[['css.summary']], 1, 1) == '+', paste0(css.summary, substring(CSS[['css.summary']], 2)), CSS[['css.summary']])
    if (!is.null(CSS[['css.leftalign']])) css.leftalign <- ifelse(substring(CSS[['css.leftalign']], 1, 1) == '+', paste0(css.leftalign, substring(CSS[['css.leftalign']], 2)), CSS[['css.leftalign']])
    if (!is.null(CSS[['css.centeralign']])) css.centeralign <- ifelse(substring(CSS[['css.centeralign']], 1, 1) == '+', paste0(css.centeralign, substring(CSS[['css.centeralign']], 2)), CSS[['css.centeralign']])
    if (!is.null(CSS[['css.lasttablerow']])) css.lasttablerow <- ifelse(substring(CSS[['css.lasttablerow']], 1, 1) == '+', paste0(css.lasttablerow, substring(CSS[['css.lasttablerow']], 2)), CSS[['css.lasttablerow']])
    if (!is.null(CSS[['css.firstcolborder']])) css.firstcolborder <- ifelse(substring(CSS[['css.firstcolborder']], 1, 1) == '+', paste0(css.firstcolborder, substring(CSS[['css.firstcolborder']], 2)), CSS[['css.firstcolborder']])
    if (!is.null(CSS[['css.secondtablerow']])) css.secondtablerow <- ifelse(substring(CSS[['css.secondtablerow']], 1, 1) == '+', paste0(css.secondtablerow, substring(CSS[['css.secondtablerow']], 2)), CSS[['css.secondtablerow']])
    if (!is.null(CSS[['css.totcol']])) css.totcol <- ifelse(substring(CSS[['css.totcol']], 1, 1) == '+', paste0(css.totcol, substring(CSS[['css.totcol']], 2)), CSS[['css.totcol']])
    if (!is.null(CSS[['css.tothi']])) css.tothi <- ifelse(substring(CSS[['css.tothi']], 1, 1) == '+', paste0(css.tothi, substring(CSS[['css.tothi']], 2)), CSS[['css.tothi']])
    if (!is.null(CSS[['css.horline']])) css.horline <- ifelse(substring(CSS[['css.horline']], 1, 1) == '+', paste0(css.horline, substring(CSS[['css.horline']], 2)), CSS[['css.horline']])
  }
  # -------------------------------------
  # set style sheet
  # -------------------------------------
  page.style <- sprintf("<style>\n%s { %s }\n%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { color:%s }\n.%s { color:%s }\n.%s { color:%s }\n.%s { color:%s }\n.%s { color:%s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n</style>", 
                        tag.table, css.table, tag.caption, css.caption, tag.thead, css.thead, tag.tdata, css.tdata, tag.secondtablerow, css.secondtablerow, 
                        tag.leftalign, css.leftalign, tag.centeralign, css.centeralign, tag.lasttablerow, css.lasttablerow, 
                        tag.totcol, css.totcol, tag.tothi, css.tothi,
                        tag.td_n, tdcol.n, tag.td_c, tdcol.cell, tag.td_rw, tdcol.row,
                        tag.td_cl, tdcol.col, tag.td_ex, tdcol.expected, 
                        tag.summary, css.summary, tag.horline, css.horline,
                        tag.firstcolborder, css.firstcolborder)
  # start writing content
  toWrite <- paste(toWrite, page.style)
  toWrite <- paste(toWrite, "\n</head>\n<body>\n")
  # -------------------------------------
  # init first table row
  # -------------------------------------
  page.content <- "<table>\n"
  # -------------------------------------
  # table caption, variable label
  # -------------------------------------
  if (!is.null(title)) page.content <- paste0(page.content, sprintf("  <caption>%s</caption>\n", title))
  page.content <- paste(page.content, "  <tr>\n")
  # -------------------------------------
  # column with row-variable-name
  # -------------------------------------
  page.content <- paste(page.content, sprintf("    <th class=\"thead firstcolborder\" rowspan=\"2\">%s</th>\n", s.var.row))
  # -------------------------------------
  # column with column-variable-name
  # -------------------------------------
  page.content <- paste(page.content, sprintf("    <th class=\"thead\" colspan=\"%i\">%s</th>\n", length(labels.var.col), s.var.col))
  # -------------------------------------
  # total-column
  # -------------------------------------
  page.content <- paste(page.content, sprintf("    <th class=\"thead tothi firstcolborder totcol\" rowspan=\"2\">%s</th>\n", stringTotal))
  page.content <- paste(page.content, "  </tr>\n")
  # -------------------------------------
  # init second table row
  # -------------------------------------
  page.content <- paste(page.content, "\n  <tr>\n")
  # -------------------------------------
  # column variable labels
  # -------------------------------------
  for (i in seq_along(labels.var.col)) {
    page.content <- paste(page.content, sprintf("    <td class=\"secondtablerow tdata\">%s</td>\n", labels.var.col[i]))
  }
  page.content <- paste(page.content, "  </tr>\n")
  # -------------------------------------
  # table content
  # -------------------------------------
  rowlabelcnt <- 1:length(labels.var.row)
  # -------------------------------------
  # iterate all table data rows
  # -------------------------------------
  for (irow in 1:totalnrow) {
    # -------------------------------------
    # start new table row
    # -------------------------------------
    page.content <- paste(page.content, "\n  <tr>")
    # -------------------------------------
    # set row variable label
    # -------------------------------------
    if (irow == totalnrow) {
      css_last_row_th <- "lasttablerow tothi "
      css_last_row <- " lasttablerow"
    } else {
      css_last_row_th <- " "
      css_last_row <- ""
    }
    page.content <- paste(page.content, 
                          sprintf("\n    <td class=\"tdata %sleftalign\">%s</td>", 
                                  css_last_row_th,
                                  labels.var.row[rowlabelcnt[irow]]))
    # -------------------------------------
    # iterate all data columns
    # -------------------------------------
    for (icol in 1:totalncol) {
      cellstring <- ""
      # -------------------------------------
      # first table cell data contains observed values
      # -------------------------------------
      if (showObserved) cellstring <- sprintf("<span class=\"td_n\">%i</span>", tab[irow, icol])
      # -------------------------------------
      # if we have expected values, add them to table cell
      # -------------------------------------
      if (showExpected) {
        if (!sjmisc::is_empty(cellstring)) cellstring <- paste0(cellstring, "<br>")
        cellstring <- paste(cellstring, sprintf("<span class=\"td_ex\">%s</span>", tab.expected[irow, icol]), sep = "")
      }
      # -------------------------------------
      # if we have row-percentage, add percentage value to table cell
      # -------------------------------------
      if (showRowPerc) {
        if (!sjmisc::is_empty(cellstring)) cellstring <- paste0(cellstring, "<br>")
        cellstring <- paste(cellstring, sprintf("<span class=\"td_rw\">%s%s</span>", tab.row[irow, icol], percSign), sep = "")
      }
      # -------------------------------------
      # if we have col-percentage, add percentage value to table cell
      # -------------------------------------
      if (showColPerc) {
        if (!sjmisc::is_empty(cellstring)) cellstring <- paste0(cellstring, "<br>")
        cellstring <- paste(cellstring, sprintf("<span class=\"td_cl\">%s%s</span>", tab.col[irow, icol], percSign), sep = "")
      }
      # -------------------------------------
      # if we have cell-percentage, add percentage value to table cell
      # -------------------------------------
      if (showCellPerc) {
        if (!sjmisc::is_empty(cellstring)) cellstring <- paste0(cellstring, "<br>")
        cellstring <- paste(cellstring, sprintf("<span class=\"td_c\">%s%s</span>", tab.cell[irow, icol], percSign), sep = "")
      }
      # -------------------------------------
      # set column variable label
      # -------------------------------------
      css_tot_col <- ifelse(icol == totalncol, " totcol", "")
      # -------------------------------------
      # write table cell data
      # -------------------------------------
      page.content <- paste(page.content, sprintf("\n    <td class=\"tdata centeralign horline%s\">%s</td>", 
                                                  ifelse(css_last_row == "", css_tot_col, css_last_row),
                                                  cellstring), sep = "")
    }
    # close table row
    page.content <- paste(page.content, "\n  </tr>\n")
  }
  # -------------------------------------
  # table summary
  # -------------------------------------
  if (showSummary) {
    # re-compute simple table
    ftab <- stats::ftable(stats::xtabs(~var.row + var.col))
    # start new table row
    page.content <- paste(page.content, "\n  <tr>\n    ", sep = "")
    # calculate chi square value
    chsq <- chisq.test(ftab)
    fish <- NULL
    # check whether variables are dichotome or if they have more
    # than two categories. if they have more, use Cramer's V to calculate
    # the contingency coefficient
    if (nrow(ftab) > 2 || ncol(ftab) > 2) {
      kook <- sprintf("&Phi;<sub>c</sub>=%.3f", sjmisc::cramer(ftab))
      # if minimum expected values below 5, compute fisher's exact test
      if (min(tab.expected) < 5 || (min(tab.expected) < 10 && chsq$parameter == 1)) 
        fish <- fisher.test(ftab, simulate.p.value = TRUE)
    } else {
      kook <- sprintf("&Phi;=%.3f", sjmisc::phi(ftab))
      # if minimum expected values below 5 and df=1, compute fisher's exact test
      if (min(tab.expected) < 5 || (min(tab.expected) < 10 && chsq$parameter == 1)) 
        fish <- fisher.test(ftab)
    }
    # make phi-value apa style
    kook <- gsub("0.", paste0(p_zero, "."), kook, fixed = TRUE)
    # create summary row
    if (is.null(fish)) {
      pvalstring <- ifelse(chsq$p.value < 0.001, 
                           sprintf("p&lt;%s.001", p_zero), 
                           sub("0", p_zero, sprintf("p=%.3f", chsq$p.value)))
      page.content <- paste(page.content, 
                            sprintf("    <td class=\"summary tdata\" colspan=\"%i\">&Chi;<sup>2</sup>=%.3f &middot; df=%i &middot; %s &middot; %s</td>", 
                                    totalncol + 1, 
                                    chsq$statistic, 
                                    chsq$parameter, 
                                    kook, 
                                    pvalstring), 
                            sep = "")
    } else {
      pvalstring <- ifelse(fish$p.value < 0.001, 
                           sprintf("p&lt;%s.001", p_zero), 
                           sub("0", p_zero, sprintf("p=%.3f", fish$p.value)))
      page.content <- paste(page.content, 
                            sprintf("    <td class=\"summary tdata\" colspan=\"%i\">Fisher's %s &middot; df=%i &middot; %s</td>", 
                                    totalncol + 1, 
                                    pvalstring, 
                                    chsq$parameter, 
                                    kook), 
                            sep = "")
    }
    # close table row
    page.content <- paste(page.content, "\n  </tr>\n")
  }  
  # -------------------------------------
  # finish table
  # -------------------------------------
  page.content <- paste(page.content, "\n</table>")
  # -------------------------------------
  # print legend
  # -------------------------------------
  if (showLegend) {
    # add new paragraph
    page.content <- paste(page.content, "<p>\n  ")
    # -----------------
    # show observed?
    # -----------------
    if (showObserved) {
      page.content <- paste(page.content, "<span class=\"td_n\">observed values</span><br>\n")
    }
    # -----------------
    # show expected?
    # -----------------
    if (showExpected) {
      page.content <- paste(page.content, "<span class=\"td_ex\">expected values</span><br>\n")
    }
    # -----------------
    # show row percentage?
    # -----------------
    if (showRowPerc) {
      page.content <- paste(page.content, 
                            sprintf("<span class=\"td_rw\">&#37; within %s</span><br>\n", 
                                    gsub("<br>", " ", s.var.row, fixed = TRUE)))
    }
    # -----------------
    # show row percentage?
    # -----------------
    if (showColPerc) {
      page.content <- paste(page.content, 
                            sprintf("<span class=\"td_cl\">&#37; within %s</span><br>\n", 
                                    gsub("<br>", " ", s.var.col, fixed = TRUE)))
    }
    # -----------------
    # show row percentage?
    # -----------------
    if (showCellPerc) {
      page.content <- paste(page.content, "<span class=\"td_c\">&#37; of total</span>\n")
    }
    # close paragraph
    page.content <- paste(page.content, "</p>\n")
  }
  # -------------------------------------
  # add table to return value list, so user can access each
  # single frequency table
  # -------------------------------------
  toWrite <- paste(toWrite, page.content, "\n")
  # -------------------------------------
  # finish html page
  # -------------------------------------
  toWrite <- paste0(toWrite, "</body></html>")
  # -------------------------------------
  # replace class attributes with inline style,
  # useful for knitr
  # -------------------------------------
  # copy page content
  # -------------------------------------
  knitr <- page.content
  # -------------------------------------
  # set style attributes for main table tags
  # -------------------------------------
  knitr <- gsub("class=", "style=", knitr, fixed = TRUE, useBytes = TRUE)
  knitr <- gsub("<table", sprintf("<table style=\"%s\"", css.table), knitr, fixed = TRUE, useBytes = TRUE)
  knitr <- gsub("<caption", sprintf("<caption style=\"%s\"", css.caption), knitr, fixed = TRUE, useBytes = TRUE)
  # -------------------------------------
  # replace class-attributes with inline-style-definitions
  # -------------------------------------
  knitr <- gsub(tag.tdata, css.tdata, knitr, fixed = TRUE, useBytes = TRUE)
  knitr <- gsub(tag.thead, css.thead, knitr, fixed = TRUE, useBytes = TRUE)
  knitr <- gsub(tag.secondtablerow, css.secondtablerow, knitr, fixed = TRUE, useBytes = TRUE)
  knitr <- gsub(tag.firstcolborder, css.firstcolborder, knitr, fixed = TRUE, useBytes = TRUE)
  knitr <- gsub(tag.leftalign, css.leftalign, knitr, fixed = TRUE, useBytes = TRUE)  
  knitr <- gsub(tag.centeralign, css.centeralign, knitr, fixed = TRUE, useBytes = TRUE)  
  knitr <- gsub(tag.horline, css.horline, knitr, fixed = TRUE, useBytes = TRUE)  
  knitr <- gsub(tag.lasttablerow, css.lasttablerow, knitr, fixed = TRUE, useBytes = TRUE)  
  knitr <- gsub(tag.totcol, css.totcol, knitr, fixed = TRUE, useBytes = TRUE)  
  knitr <- gsub(tag.summary, css.summary, knitr, fixed = TRUE, useBytes = TRUE)  
  knitr <- gsub(tag.tothi, css.tothi, knitr, fixed = TRUE, useBytes = TRUE)  
  # -------------------------------------
  # replace color-attributes for legend
  # -------------------------------------
  knitr <- gsub(tag.td_ex, sprintf("color:%s;",tdcol.expected), knitr, fixed = TRUE, useBytes = TRUE)  
  knitr <- gsub(tag.td_cl, sprintf("color:%s;",tdcol.col), knitr, fixed = TRUE, useBytes = TRUE)  
  knitr <- gsub(tag.td_rw, sprintf("color:%s;",tdcol.row), knitr, fixed = TRUE, useBytes = TRUE)  
  knitr <- gsub(tag.td_c, sprintf("color:%s;",tdcol.cell), knitr, fixed = TRUE, useBytes = TRUE)  
  knitr <- gsub(tag.td_n, sprintf("color:%s;",tdcol.n), knitr, fixed = TRUE, useBytes = TRUE)  
  # -------------------------------------
  # remove spaces?
  # -------------------------------------
  if (remove.spaces) {
    knitr <- sju.rmspc(knitr)
    toWrite <- sju.rmspc(toWrite)
    page.content <- sju.rmspc(page.content)
  }
  # -------------------------------------
  # check if html-content should be printed
  # -------------------------------------
  out.html.table(no.output, file, knitr, toWrite, useViewer)   
  # -------------------------------------
  # return results
  # -------------------------------------
  invisible(structure(class = c("sjTable", "sjtxtab"),
                      list(page.style = page.style,
                           page.content = page.content,
                           output.complete = toWrite,
                           knitr = knitr)))
}
