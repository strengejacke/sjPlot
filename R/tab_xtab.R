#' @title Summary of contingency tables as HTML table
#' @name tab_xtab
#'
#' @description Shows contingency tables as HTML file in browser or viewer pane, or saves them as file.
#'
#' @param var.row Variable that should be displayed in the table rows.
#' @param var.col Cariable that should be displayed in the table columns.
#' @param var.labels Character vector with variable names, which will be used
#'          to label variables in the output.
#' @param string.total Character label for the total column / row header
#' @param show.cell.prc Logical, if \code{TRUE}, cell percentage values are shown
#' @param show.row.prc Logical, if \code{TRUE}, row percentage values are shown
#' @param show.col.prc Logical, if \code{TRUE}, column percentage values are shown
#' @param show.obs Logical, if \code{TRUE}, observed values are shown
#' @param show.exp Logical, if \code{TRUE}, expected values are also shown
#' @param show.summary Logical, if \code{TRUE}, a summary row with
#'          chi-squared statistics, degrees of freedom and Cramer's V or Phi
#'          coefficient and p-value for the chi-squared statistics.
#' @param tdcol.n Color for highlighting count (observed) values in table cells. Default is black.
#' @param tdcol.expected Color for highlighting expected values in table cells. Default is cyan.
#' @param tdcol.cell Color for highlighting cell percentage values in table cells. Default is red.
#' @param tdcol.row Color for highlighting row percentage values in table cells. Default is blue.
#' @param tdcol.col Color for highlighting column percentage values in table cells. Default is green.
#' @param emph.total Logical, if \code{TRUE}, the total column and row will be emphasized with a
#'          different background color. See \code{emph.color}.
#' @param emph.color Logical, if \code{emph.total = TRUE}, this color value will be used
#'          for painting the background of the total column and row. Default is a light grey.
#' @param prc.sign The percentage sign that is printed in the table cells, in HTML-format.
#'          Default is \code{"&nbsp;\%"}, hence the percentage sign has a non-breaking-space after
#'          the percentage value.
#' @param hundret Default value that indicates the 100-percent column-sums (since rounding values
#'          may lead to non-exact results). Default is \code{"100.0"}.
#' @param statistics Name of measure of association that should be computed. May
#'          be one of \code{"auto"}, \code{"cramer"}, \code{"phi"}, \code{"spearman"},
#'          \code{"kendall"}, \code{"pearson"} or \code{"fisher"}. See
#'          \code{\link[sjstats]{xtab_statistics}}.
#' @param ... Other arguments, currently passed down to the test statistics functions
#'        \code{chisq.test()} or \code{fisher.test()}.
#' @param encoding String, indicating the charset encoding used for variable and
#'          value labels. Default is \code{NULL}, so encoding will be auto-detected
#'          depending on your platform (e.g., \code{"UTF-8"} for Unix and \code{"Windows-1252"} for
#'          Windows OS). Change encoding if specific chars are not properly displayed (e.g. German umlauts).
#' @param remove.spaces Logical, if \code{TRUE}, leading spaces are removed from all lines in the final string
#'          that contains the html-data. Use this, if you want to remove parantheses for html-tags. The html-source
#'          may look less pretty, but it may help when exporting html-tables to office tools.
#' @param value.labels Character vector (or \code{list} of character vectors)
#'          with value labels of the supplied variables, which will be used
#'          to label variable values in the output.
#'
#' @inheritParams tab_model
#' @inheritParams plot_grpfrq
#'
#' @return Invisibly returns
#'          \itemize{
#'            \item the web page style sheet (\code{page.style}),
#'            \item the web page content (\code{page.content}),
#'            \item the complete html-output (\code{page.complete}) and
#'            \item the html-table with inline-css for use with knitr (\code{knitr})
#'            }
#'            for further use.
#'
#' @examples
#' # prepare sample data set
#' data(efc)
#'
#' # print simple cross table with labels
#' \dontrun{
#' if (interactive()) {
#'   tab_xtab(efc$e16sex, efc$e42dep)
#'
#'   # print cross table with manually set
#'   # labels and expected values
#'   tab_xtab(
#'     efc$e16sex,
#'     efc$e42dep,
#'     var.labels = c("Elder's gender", "Elder's dependency"),
#'     show.exp = TRUE
#'   )
#'
#'   # print minimal cross table with labels, total col/row highlighted
#'   tab_xtab(efc$e16sex, efc$e42dep, show.cell.prc = FALSE, emph.total = TRUE)
#'
#'   # User defined style sheet
#'   tab_xtab(efc$e16sex, efc$e42dep,
#'            CSS = list(css.table = "border: 2px solid;",
#'                       css.tdata = "border: 1px solid;",
#'                       css.horline = "border-bottom: double blue;"))
#'
#'   # ordinal data, use Kendall's tau
#'   tab_xtab(efc$e42dep, efc$quol_5, statistics = "kendall")
#'
#'   # calculate Spearman's rho, with continuity correction
#'   tab_xtab(
#'     efc$e42dep,
#'     efc$quol_5,
#'     statistics = "spearman",
#'     exact = FALSE,
#'     continuity = TRUE
#'   )
#' }
#' }
#' @importFrom stats ftable
#' @importFrom sjstats crosstable_statistics table_values
#' @export
tab_xtab <- function(var.row,
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
                     ...) {
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
  # match arguments
  statistics <- match.arg(statistics)
  # --------------------------------------------------------
  # get variable name
  # --------------------------------------------------------
  var.name.row <- get_var_name(deparse(substitute(var.row)))
  var.name.col <- get_var_name(deparse(substitute(var.col)))

  # remove empty value-labels
  if (drop.empty) {
    var.row <- sjlabelled::drop_labels(var.row)
    var.col <- sjlabelled::drop_labels(var.col)
  }

  # --------------------------------------------------------
  # create cross table of frequencies and percentages
  # --------------------------------------------------------
  mydat <- create.xtab.df(var.row,
                          var.col,
                          round.prz = digits,
                          na.rm = !show.na,
                          weight.by = weight.by)
  # --------------------------------------------------------
  # try to automatically set labels is not passed as parameter
  # --------------------------------------------------------
  if (is.null(var.labels)) {
    var.labels <- c(sjlabelled::get_label(var.row, def.value = var.name.row),
                    sjlabelled::get_label(var.col, def.value = var.name.col))
  }
  # wrap long labels
  var.labels <- sjmisc::word_wrap(var.labels, wrap.labels, "<br>")
  s.var.row <- var.labels[1]
  s.var.col <- var.labels[2]
  # --------------------------------------------------------
  # Do we have notes for vectors?
  # --------------------------------------------------------
  n.var.row <- comment(var.row)
  n.var.col <- comment(var.col)
  # -------------------------------------
  # init variable labels
  # -------------------------------------
  labels.var.row <- mydat$labels.cnt
  labels.var.col <- mydat$labels.grp
  # do we have labels?
  if (!is.null(value.labels)) {
    # need to be a list
    if (!is.list(value.labels)) {
      warning("`valueLables` needs to be a `list`-object.", call. = F)
    } else {
      labels.var.row <- value.labels[[1]]
      labels.var.col <- value.labels[[2]]
    }
    # correct length of labels?
    if (length(labels.var.row) != length(mydat$labels.cnt)) {
      warning("Length of `value.labels` does not match length of category values of `var.row`.", call. = F)
      labels.var.row <- mydat$labels.cnt
    }
    # correct length of labels?
    if (length(labels.var.col) != length(mydat$labels.grp)) {
      warning("Length of `value.labels` does not match length of category values of `var.grp`.", call. = F)
      labels.var.col <- mydat$labels.grp
    }
  }
  # wrap labels
  labels.var.row <- sjmisc::word_wrap(labels.var.row, wrap.labels, "<br>")
  labels.var.col <- sjmisc::word_wrap(labels.var.col, wrap.labels, "<br>")
  # add "total"
  labels.var.row <- c(labels.var.row, string.total)
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
  tab.expected <- sjstats::table_values(stats::ftable(as.matrix(tab)))$expected
  # -------------------------------------
  # determine total number of columns and rows
  # -------------------------------------
  totalncol <- ncol(tab)
  totalnrow <- nrow(tab)
  # -------------------------------------
  # table init
  # -------------------------------------
  # init web page header
  toWrite <- table.header <- sprintf("<html>\n<head>\n<meta http-equiv=\"Content-type\" content=\"text/html;charset=%s\">\n", encoding)
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
  css.lasttablerow <- ifelse(isTRUE(emph.total), sprintf(" border-bottom:double; background-color:%s;", emph.color), " border-bottom:double;")
  css.totcol <- ifelse(isTRUE(emph.total), sprintf(" background-color:%s;", emph.color), "")
  css.tothi <- "font-weight:bolder; font-style:italic;"
  css.summary <- "text-align:right; font-size:0.9em; font-style:italic;"
  css.horline <- ""
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
  page.style <- sprintf("<style>\nhtml, body { background-color: white; }\n%s { %s }\n%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { color:%s }\n.%s { color:%s }\n.%s { color:%s }\n.%s { color:%s }\n.%s { color:%s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n</style>",
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
  if (!is.null(n.var.row))
    th.title.tag <- sprintf(" title=\"%s\"", n.var.row)
  else
    th.title.tag <- ""
  page.content <- paste(page.content, sprintf("    <th class=\"thead firstcolborder\" rowspan=\"2\"%s>%s</th>\n", th.title.tag, s.var.row))
  # -------------------------------------
  # column with column-variable-name
  # -------------------------------------
  if (!is.null(n.var.col))
    th.title.tag <- sprintf(" title=\"%s\"", n.var.col)
  else
    th.title.tag <- ""
  page.content <- paste(page.content, sprintf("    <th class=\"thead\" colspan=\"%i\"%s>%s</th>\n", length(labels.var.col), th.title.tag, s.var.col))
  # -------------------------------------
  # total-column
  # -------------------------------------
  page.content <- paste(page.content, sprintf("    <th class=\"thead tothi firstcolborder totcol\" rowspan=\"2\">%s</th>\n", string.total))
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
  rowlabelcnt <- seq_len(length(labels.var.row))
  # -------------------------------------
  # iterate all table data rows
  # -------------------------------------
  for (irow in seq_len(totalnrow)) {
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
    for (icol in seq_len(totalncol)) {
      cellstring <- ""
      # -------------------------------------
      # first table cell data contains observed values
      # -------------------------------------
      if (show.obs) cellstring <- sprintf("<span class=\"td_n\">%i</span>", tab[irow, icol])
      # -------------------------------------
      # if we have expected values, add them to table cell
      # -------------------------------------
      if (show.exp) {
        if (!sjmisc::is_empty(cellstring)) cellstring <- paste0(cellstring, "<br>")
        cellstring <- paste(cellstring, sprintf("<span class=\"td_ex\">%s</span>", tab.expected[irow, icol]), sep = "")
      }
      # -------------------------------------
      # if we have row-percentage, add percentage value to table cell
      # -------------------------------------
      if (show.row.prc) {
        if (!sjmisc::is_empty(cellstring)) cellstring <- paste0(cellstring, "<br>")
        cellstring <- paste(cellstring, sprintf("<span class=\"td_rw\">%s%s</span>", tab.row[irow, icol], prc.sign), sep = "")
      }
      # -------------------------------------
      # if we have col-percentage, add percentage value to table cell
      # -------------------------------------
      if (show.col.prc) {
        if (!sjmisc::is_empty(cellstring)) cellstring <- paste0(cellstring, "<br>")
        cellstring <- paste(cellstring, sprintf("<span class=\"td_cl\">%s%s</span>", tab.col[irow, icol], prc.sign), sep = "")
      }
      # -------------------------------------
      # if we have cell-percentage, add percentage value to table cell
      # -------------------------------------
      if (show.cell.prc) {
        if (!sjmisc::is_empty(cellstring)) cellstring <- paste0(cellstring, "<br>")
        cellstring <- paste(cellstring, sprintf("<span class=\"td_c\">%s%s</span>", tab.cell[irow, icol], prc.sign), sep = "")
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
  if (show.summary) {

    xtsdf <- data.frame(var.row, var.col)

    if (!is.null(weight.by)) {
      xtsdf$weights <- weight.by
      xt_stat <- sjstats::crosstable_statistics(
        data = xtsdf,
        weights = "weights",
        statistics = statistics,
        ...
      )
    } else {
      xt_stat <- sjstats::crosstable_statistics(
        data = xtsdf,
        statistics = statistics,
        ...
      )
    }

    # fisher's exact test?
    if (xt_stat$fisher)
      pstring <- "Fisher's p"
    else
      pstring <- "p"

    page.content <- paste(
      page.content,
      sprintf(
        "    <td class=\"summary tdata\" colspan=\"%i\">%s=%.3f &middot; df=%i &middot; %s=%.3f &middot; %s=%.3f</td>",
        totalncol + 1,
        xt_stat$stat.html,
        xt_stat$statistic,
        xt_stat$df,
        xt_stat$method.html,
        xt_stat$estimate,
        pstring,
        xt_stat$p.value
      ),
      sep = ""
    )
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
  if (show.legend) {
    # add new paragraph
    page.content <- paste(page.content, "<p>\n  ")
    # -----------------
    # show observed?
    # -----------------
    if (show.obs) {
      page.content <- paste(page.content, "<span class=\"td_n\">observed values</span><br>\n")
    }
    # -----------------
    # show expected?
    # -----------------
    if (show.exp) {
      page.content <- paste(page.content, "<span class=\"td_ex\">expected values</span><br>\n")
    }
    # -----------------
    # show row percentage?
    # -----------------
    if (show.row.prc) {
      page.content <- paste(page.content,
                            sprintf("<span class=\"td_rw\">&#37; within %s</span><br>\n",
                                    gsub("<br>", " ", s.var.row, fixed = TRUE)))
    }
    # -----------------
    # show row percentage?
    # -----------------
    if (show.col.prc) {
      page.content <- paste(page.content,
                            sprintf("<span class=\"td_cl\">&#37; within %s</span><br>\n",
                                    gsub("<br>", " ", s.var.col, fixed = TRUE)))
    }
    # -----------------
    # show row percentage?
    # -----------------
    if (show.cell.prc) {
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
  # return results
  # -------------------------------------
  structure(class = c("sjTable", "sjtxtab"),
                      list(page.style = page.style,
                           page.content = page.content,
                           page.complete = toWrite,
                           knitr = knitr,
                           header = table.header,
                           file = file,
                           viewer = use.viewer))
}


#' @rdname tab_xtab
#' @export
sjt.xtab <- tab_xtab
