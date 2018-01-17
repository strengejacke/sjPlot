#' @importFrom sjmisc var_type is_even
tab_df <- function(x, encoding = "UTF-8", CSS = NULL, ...) {
  # init style sheet and tags used for css-definitions
  # we can use these variables for string-replacement
  # later for return value

  tag.table <- "table"
  tag.caption <- "caption"
  tag.thead <- "thead"
  tag.tdata <- "tdata"
  tag.arc <- "arc"
  tag.comment <- "comment"
  tag.firsttablerow <- "firsttablerow"
  tag.lasttablerow <- "lasttablerow"
  tag.firsttablecol <- "firsttablecol"
  tag.leftalign <- "leftalign"
  tag.centertalign <- "centertalign"
  css.table <- "border-collapse:collapse; border:none;"
  css.caption <- "font-weight: bold; text-align:left;"
  css.thead <- "border-top: double; text-align:center; font-style:italic; font-weight:normal; padding:0.2cm;"
  css.tdata <- "padding:0.2cm; text-align:left; vertical-align:top;"
  css.arc <- "background-color:#eaeaea;"
  css.lasttablerow <- "border-top:1px solid; border-bottom: double;"
  css.firsttablerow <- "border-bottom:1px solid black;"
  css.firsttablecol <- ""
  css.leftalign <- "text-align:left;"
  css.centeralign <- "text-align:center;"
  css.comment <- "font-style:italic; border-top:double black; text-align:right;"
  if (show.cmmn.row && repeat.header) css.comment <- "font-style:italic; text-align:right;"


  # check user defined style sheets

  if (!is.null(CSS)) {
    if (!is.null(CSS[['css.table']])) css.table <- ifelse(substring(CSS[['css.table']], 1, 1) == '+', paste0(css.table, substring(CSS[['css.table']], 2)), CSS[['css.table']])
    if (!is.null(CSS[['css.caption']])) css.caption <- ifelse(substring(CSS[['css.caption']], 1, 1) == '+', paste0(css.caption, substring(CSS[['css.caption']], 2)), CSS[['css.caption']])
    if (!is.null(CSS[['css.thead']])) css.thead <- ifelse(substring(CSS[['css.thead']], 1, 1) == '+', paste0(css.thead, substring(CSS[['css.thead']], 2)), CSS[['css.thead']])
    if (!is.null(CSS[['css.tdata']])) css.tdata <- ifelse(substring(CSS[['css.tdata']], 1, 1) == '+', paste0(css.tdata, substring(CSS[['css.tdata']], 2)), CSS[['css.tdata']])
    if (!is.null(CSS[['css.arc']])) css.arc <- ifelse(substring(CSS[['css.arc']], 1, 1) == '+', paste0(css.arc, substring(CSS[['css.arc']], 2)), CSS[['css.arc']])
    if (!is.null(CSS[['css.lasttablerow']])) css.lasttablerow <- ifelse(substring(CSS[['css.lasttablerow']], 1, 1) == '+', paste0(css.lasttablerow, substring(CSS[['css.lasttablerow']], 2)), CSS[['css.lasttablerow']])
    if (!is.null(CSS[['css.firsttablerow']])) css.firsttablerow <- ifelse(substring(CSS[['css.firsttablerow']], 1, 1) == '+', paste0(css.firsttablerow, substring(CSS[['css.firsttablerow']], 2)), CSS[['css.firsttablerow']])
    if (!is.null(CSS[['css.leftalign']])) css.leftalign <- ifelse(substring(CSS[['css.leftalign']], 1, 1) == '+', paste0(css.leftalign, substring(CSS[['css.leftalign']], 2)), CSS[['css.leftalign']])
    if (!is.null(CSS[['css.centeralign']])) css.centeralign <- ifelse(substring(CSS[['css.centeralign']], 1, 1) == '+', paste0(css.centeralign, substring(CSS[['css.centeralign']], 2)), CSS[['css.centeralign']])
    if (!is.null(CSS[['css.firsttablecol']])) css.firsttablecol <- ifelse(substring(CSS[['css.firsttablecol']], 1, 1) == '+', paste0(css.firsttablecol, substring(CSS[['css.firsttablecol']], 2)), CSS[['css.firsttablecol']])
    if (!is.null(CSS[['css.comment']])) css.comment <- ifelse(substring(CSS[['css.comment']], 1, 1) == '+', paste0(css.comment, substring(CSS[['css.comment']], 2)), CSS[['css.comment']])
  }


  # set style sheet

  page.style <- sprintf("<style>\nhtml, body { background-color: white; }\n%s { %s }\n%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n</style>",
                        tag.table, css.table, tag.caption, css.caption,
                        tag.thead, css.thead, tag.tdata, css.tdata, tag.arc, css.arc,
                        tag.lasttablerow, css.lasttablerow, tag.firsttablerow, css.firsttablerow,
                        tag.leftalign, css.leftalign, tag.centertalign, css.centeralign,
                        tag.firsttablecol, css.firsttablecol, tag.comment, css.comment)

  # first, save table header
  table.header <- sprintf("<html>\n<head>\n<meta http-equiv=\"Content-type\" content=\"text/html;charset=%s\">\n%s\n</head>\n<body>\n", encoding, page.style)

  # get row and column count of data frame
  rowcnt <- nrow(mydf)
  colcnt <- ncol(mydf)

  # start table tag
  page.content <- "<table>\n"

  # table caption, variable label
  if (!is.null(title)) page.content <- paste0(page.content, sprintf("  <caption>%s</caption>\n", title))

  # header row
  page.content <- paste0(page.content, "  <tr>\n")

  # first columns are rownames
  for (i in 1:colcnt) {
    # check variable type
    vartype <- sjmisc::var_type(mydf[[i]])
    # column names and variable as table headline
    page.content <- paste0(page.content, sprintf("    <th class=\"thead firsttablerow\">%s", cnames[i]))
    if (show.type) page.content <- paste0(page.content, sprintf("<br>(%s)", vartype))
    page.content <- paste0(page.content, "</th>\n")
  }
  page.content <- paste0(page.content, "  </tr>\n")

  # -------------------------------------
  # subsequent rows
  # -------------------------------------
  for (rcnt in 1:rowcnt) {
    # default row string
    arcstring <- ""
    # if we have alternating row colors, set css
    if (altr.row.col) arcstring <- ifelse(sjmisc::is_even(rcnt), " arc", "")
    page.content <- paste0(page.content, "  <tr>\n")
    # first table cell is rowname
    if (show.rownames) page.content <- paste0(page.content, sprintf("    <td class=\"tdata leftalign firsttablecol%s\">%s</td>\n", arcstring, rnames[rcnt]))
    # all columns of a row
    for (ccnt in 1:colcnt) {
      page.content <- paste0(page.content, sprintf("    <td class=\"tdata centertalign%s\">%s</td>\n", arcstring, mydf[rcnt, ccnt]))
    }
    # close row tag
    page.content <- paste0(page.content, "</tr>\n")
  }
  if (!hide.progress) close(pb)
  # -------------------------------------
  # repeat header row?
  # -------------------------------------
  if (repeat.header) {
    page.content <- paste0(page.content, "  <tr>\n")
    if (show.rownames) page.content <- paste0(page.content, sprintf("    <th class=\"thead lasttablerow firsttablecol\">%s</th>\n", string.var))
    for (i in 1:colcnt) {
      # check variable type
      vartype <- sjmisc::var_type(mydf[[i]])
      # column names and variable as table headline
      page.content <- paste0(page.content, sprintf("    <th class=\"thead lasttablerow\">%s", cnames[i]))
      if (show.type) page.content <- paste0(page.content, sprintf("<br>(%s)", vartype))
      page.content <- paste0(page.content, "</th>\n")
    }
    page.content <- paste0(page.content, "  </tr>\n")
  }
  # -------------------------------------
  # add optional "comment" row
  # -------------------------------------
  if (show.cmmn.row) {
    page.content <- paste0(page.content, "  <tr>\n")
    if (!show.rownames) colcnt <- colcnt - 1
    page.content <- paste0(page.content, sprintf("    <td colspan=\"%i\" class=\"comment\">%s</td>\n", colcnt + 1, string.cmmn))
    # close row tag
    page.content <- paste0(page.content, "</tr>\n")
  }
  # -------------------------------------
  # finish html page
  # -------------------------------------
  page.content <- paste0(page.content, "</table>\n")
  toWrite <- paste0(table.header, page.content, "\n</body></html>")
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
  knitr <- gsub(tag.arc, css.arc, knitr, fixed = TRUE, useBytes = TRUE)
  knitr <- gsub(tag.comment, css.comment, knitr, fixed = TRUE, useBytes = TRUE)
  knitr <- gsub(tag.lasttablerow, css.lasttablerow, knitr, fixed = TRUE, useBytes = TRUE)
  knitr <- gsub(tag.firsttablerow, css.firsttablerow, knitr, fixed = TRUE, useBytes = TRUE)
  knitr <- gsub(tag.firsttablecol, css.firsttablecol, knitr, fixed = TRUE, useBytes = TRUE)
  knitr <- gsub(tag.leftalign, css.leftalign, knitr, fixed = TRUE, useBytes = TRUE)
  knitr <- gsub(tag.centertalign, css.centeralign, knitr, fixed = TRUE, useBytes = TRUE)
  # -------------------------------------
  # remove spaces?
  # -------------------------------------
  knitr <- rmspc(knitr)
  toWrite <- rmspc(toWrite)
  page.content <- rmspc(page.content)
  # -------------------------------------
  # return results
  # -------------------------------------

  structure(
    class = c("sjTable"),
    list(
      page.style = page.style,
      page.content = page.content,
      output.complete = toWrite,
      header = table.header,
      knitr = knitr,
      file = file,
      show = !no.output,
      use.viewer = use.viewer
    )
  )
}


rmspc <- function(html.table) {
  cleaned <- gsub("      <", "<", html.table, fixed = TRUE, useBytes = TRUE)
  cleaned <- gsub("    <", "<", cleaned, fixed = TRUE, useBytes = TRUE)
  cleaned <- gsub("  <", "<", cleaned, fixed = TRUE, useBytes = TRUE)

  cleaned
}
