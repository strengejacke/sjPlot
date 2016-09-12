#' @title Flat (proportional) tables
#' @name sjt.ftab
#'
#' @description This function creates a labelled flat table or flat
#'              proportional (marginal) table.
#'
#' @param .data A data frame.
#' @param ... One or more variables of \code{.data} that should be printed as table.
#' @param digits Numeric; for proportional tables, \code{digits} indicates the
#'               number of decimal places.
#'
#' @inheritParams sjt.xtab
#' @inheritParams sjt.frq
#' 
#' @return An object of class \code{\link[stats]{ftable}}.
#'
#' @examples
#' data(efc)
#'
#' # flat table with counts
#' sjt.ftab(efc, e42dep, c172code, e16sex)
#'
#' # flat table with proportions
#' sjt.ftab(efc, e42dep, c172code, e16sex, margin = "row")
#'
#' @importFrom dplyr select_ arrange_ "%>%"
#' @importFrom sjmisc flat_table get_label
#' @importFrom tibble as_tibble
#' @importFrom tidyr spread_
#' @export
sjt.ftab <- function(.data, 
                     ..., 
                     show.obs = TRUE,
                     show.cell.prc = FALSE,
                     show.row.prc = FALSE,
                     show.col.prc = FALSE,
                     digits = 2,
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
                     no.output = FALSE,
                     remove.spaces = TRUE) {
  # check encoding
  encoding <- get.encoding(encoding)
  
  # get variable names from dots
  .dots <- match.call(expand.dots = FALSE)$`...`
  .vars <- unlist(lapply(.dots, as.character))
  
  # create flat table and convert to tibble
  tab.obs <- .data %>% 
    sjmisc::flat_table(..., margin = "counts", digits = digits) %>% 
    tibble::as_tibble() %>% 
    dplyr::arrange_(.vars[seq_len(length(.vars) - 1)]) %>%
    tidyr::spread_(key_col = .vars[length(.vars)], value_col = "Freq")

  # create flat table and convert to tibble
  tab.cell.prc <- .data %>% 
    sjmisc::flat_table(..., margin = "cell", digits = digits) %>% 
    tibble::as_tibble() %>% 
    dplyr::arrange_(.vars[seq_len(length(.vars) - 1)]) %>%
    tidyr::spread_(key_col = .vars[length(.vars)], value_col = "Freq")
  
  # create flat table and convert to tibble
  tab.col.prc <- .data %>% 
    sjmisc::flat_table(..., margin = "col", digits = digits) %>% 
    tibble::as_tibble() %>% 
    dplyr::arrange_(.vars[seq_len(length(.vars) - 1)]) %>%
    tidyr::spread_(key_col = .vars[length(.vars)], value_col = "Freq")
  
  # create flat table and convert to tibble
  tab.row.prc <- .data %>% 
    sjmisc::flat_table(..., margin = "row", digits = digits) %>% 
    tibble::as_tibble() %>% 
    dplyr::arrange_(.vars[seq_len(length(.vars) - 1)]) %>%
    tidyr::spread_(key_col = .vars[length(.vars)], value_col = "Freq")
  
  # get variable labels
  var.labels <- .data %>% 
    dplyr::select_(.dots = .vars) %>% 
    sjmisc::get_label()
  
  # we need to know how many columns with labels we have,
  # and how many columns with data, and their column indices
  label.col <- seq_len(length(var.labels) - 1)
  data.col <- seq_len(ncol(tab.obs))[-label.col]
  column.labels <- var.labels
  data.labels <- colnames(tab.obs)[data.col]

  # now get unique values for each "label" column, multiply with previous
  # column, to get the row spans for each label colummn for the HTML table
  rowspans <- c()
  for (i in rev(label.col)) rowspans <- c(rowspans, length(unique(tab.obs[[i]])))
  rowspans <- rev(cumprod(rowspans))[-1]

  # table init -----
  toWrite <- sprintf("<html>\n<head>\n<meta http-equiv=\"Content-type\" content=\"text/html;charset=%s\">\n", encoding)
  
  # init style sheet and tags used for css-definitions -----
  # we can use these variables for string-replacement
  # later for return value
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
  
  # check user defined style sheets -----
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
  
  # set style sheet -----
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
  
  # init first table row -----
  page.content <- "<table>\n  <tr>\n"

  # set variable labels as column header
  for (i in column.labels[label.col]) {
    page.content <- paste(page.content, sprintf("    <th class=\"thead\" rowspan=\"2\">%s</th>\n", i))
  }
  # set variable label of last column
  page.content <- paste(page.content, sprintf("    <th class=\"thead\" colspan=\"%i\">%s</th>\n", length(data.col), column.labels[length(column.labels)]))
  page.content <- paste0(page.content, "  </tr>\n  <tr>\n")
  
  # set variable labels as column header
  for (i in data.labels) page.content <- paste(page.content, sprintf("    <th>%s</th>\n", i))
  page.content <- paste0(page.content, "  </tr>\n  <tr>\n")
  
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
  out.html.table(no.output, file, knitr, toWrite, use.viewer)   
  # -------------------------------------
  # return results
  # -------------------------------------
  invisible(structure(class = c("sjTable", "sjtftab"),
                      list(page.style = page.style,
                           page.content = page.content,
                           output.complete = toWrite,
                           knitr = knitr)))
}