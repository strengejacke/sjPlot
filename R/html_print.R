#' @title Print data frames as HTML table.
#' @name tab_df
#'
#' @description These functions print data frames as HTML-table, showing
#'   the results in RStudio's viewer pane or in a web browser.
#'
#' @param x For \code{tab_df()}, a data frame; and for \code{tab_dfs()}, a
#'   list of data frames.
#' @param title,titles Character vector (of length 1) with table caption. Only
#'   applies to \code{tab_df()}. For \code{tab_dfs()}, add a character vector
#'   as \code{title}-attribute to each data frame, which will then be used as
#'   caption for each table.
#' @param footnote,footnotes Character vector (of length 1) with table footnote. Only
#'   applies to \code{tab_df()}. For \code{tab_dfs()}, add a character vector
#'   as \code{footnote}-attribute to each data frame, which will then be used
#'   as footnote under each table.
#' @param col.header Character vector with elements used as column header for
#'   the table. If \code{NULL}, column names from \code{x} are used as
#'   column header.
#' @param encoding Character vector, indicating the charset encoding used
#'   for variable and value labels. Default is \code{"UTF-8"}. For Windows
#'   Systems, \code{encoding = "Windows-1252"} might be necessary for proper
#'   display of special characters.
#' @param show.type
#' @param show.rownames
#' @param show.footnote
#' @param alternate.rows
#'
#' @inheritParams sjt.frq
#'
#' @return A list with following items:
#'   \itemize{
#'     \item the web page style sheet (\code{page.style}),
#'     \item the HTML content of the data frame (\code{page.content}),
#'     \item the complete HTML page, including header, style sheet and body (\code{page.complete})
#'     \item the HTML table with inline-css for use with knitr (\code{knitr})
#'     \item the file path, if the HTML page should be saved to disk (\code{file})
#'   }
#'
#' @importFrom sjmisc var_type is_even
#' @importFrom purrr flatten_chr map
#' @export
tab_df <- function(x,
                   title = NULL,
                   footnote = NULL,
                   col.header = NULL,
                   show.type = FALSE,
                   show.rownames = TRUE,
                   show.footnote = FALSE,
                   alternate.rows = FALSE,
                   encoding = "UTF-8",
                   CSS = NULL,
                   file = NULL,
                   use.viewer = TRUE,
                   ...) {

  # get style definition
  style <- tab_df_style(CSS = CSS, ...)


  # get HTML content
  page.content <- tab_df_content(
      mydf = x,
      title = title,
      footnote = footnote,
      col.header = col.header,
      show.type = show.type,
      show.rownames = show.rownames,
      show.footnote = show.footnote,
      altr.row.col = alternate.rows,
      ...
    )


  # create HTML page with header information
  page.complete <- tab_create_page(
    style = style,
    content = page.content,
    encoding = encoding
  )


  # replace CSS-style to inline, needed for knitr documents
  knitr <- tab_df_knitr(CSS = CSS, content = page.content, ...)

  # remove spaces
  knitr <- rmspc(knitr)
  page.content <- rmspc(page.content)


  structure(
    class = c("sjTable"),
    list(
      page.style = style,
      page.content = page.content,
      page.complete = page.complete,
      knitr = knitr,
      file = file,
      show = TRUE,
      viewer = use.viewer
    )
  )
}


#' @importFrom sjmisc var_type is_even
#' @importFrom purrr flatten_chr pmap
#' @rdname tab_df
#' @export
tab_dfs <- function(x,
                    titles = NULL,
                    footnotes = NULL,
                    col.header = NULL,
                    show.type = FALSE,
                    show.rownames = TRUE,
                    show.footnote = FALSE,
                    alternate.rows = FALSE,
                    encoding = "UTF-8",
                    CSS = NULL,
                    file = NULL,
                    use.viewer = TRUE,
                    ...) {

  # get style definition
  style <- tab_df_style(CSS = CSS, ...)

  # check arguments

  if (is.null(titles)) titles <- rep("", length(x))
  if (is.null(footnotes)) footnotes <- rep("", length(x))

  if (length(titles) != length(x))
    stop("Number of elements in `title` does not match number of data frames to print.", call. = F)

  if (length(footnotes) != length(x))
    stop("Number of elements in `footnote` does not match number of data frames to print.", call. = F)


  # get HTML content
  page.content <- paste(
      purrr::flatten_chr(purrr::pmap(list(x, titles, footnotes), function(dat, title, footnote) {
        tab_df_content(
          mydf = dat,
          title = title,
          footnote = footnote,
          col.header = col.header,
          show.type = show.type,
          show.rownames = show.rownames,
          show.footnote = show.footnote,
          altr.row.col = alternate.rows,
          ...
        )
      })),
      collapse = "<p>&nbsp;</p>"
    )


  # create HTML page with header information
  page.complete <- tab_create_page(
    style = style,
    content = page.content,
    encoding = encoding
  )


  # replace CSS-style to inline, needed for knitr documents
  knitr <- tab_df_knitr(CSS = CSS, content = page.content, ...)

  # remove spaces
  knitr <- rmspc(knitr)
  page.content <- rmspc(page.content)


  structure(
    class = c("sjTable"),
    list(
      page.style = style,
      page.content = page.content,
      page.complete = page.complete,
      knitr = knitr,
      file = file,
      show = TRUE,
      viewer = use.viewer
    )
  )
}


# This functions creates the body of the HTML page, i.e. it puts
# the content of a data frame into a HTML table that is returned.

#' @importFrom sjmisc is_empty
#' @importFrom tibble has_rownames has_name
tab_df_content <- function(mydf, title, footnote, col.header, show.type, show.rownames, show.footnote, altr.row.col, ...) {

  rowcnt <- nrow(mydf)
  colcnt <- ncol(mydf)

  cnames <- colnames(mydf)

  if (!sjmisc::is_empty(col.header) && length(col.header) == length(cnames))
    cnames <- col.header

  if (tibble::has_rownames(mydf))
    rnames <- rownames(mydf)
  else if (tibble::has_name(mydf, "rownames"))
    rnames <- mydf[["rownames"]]
  else
    rnames <- ""


  if (show.rownames && sjmisc::is_empty(rnames))
    show.rownames <- FALSE


  # start table tag
  page.content <- "<table>\n"

  # table caption, variable label
  if (!sjmisc::is_empty(title))
    page.content <- paste0(page.content, sprintf("  <caption>%s</caption>\n", title))


  # header row ----

  page.content <- paste0(page.content, "  <tr>\n")

  for (i in 1:colcnt) {

    # separate CSS for first column
    if (i == 1)
      ftc <- " firsttablecol"
    else
      ftc <- ""

    # column names and variable type as table headline
    vartype <- sjmisc::var_type(mydf[[i]])
    page.content <- paste0(
      page.content, sprintf("    <th class=\"thead firsttablerow%s\">%s", ftc, cnames[i])
    )

    if (show.type)
      page.content <- paste0(page.content, sprintf("<br>(%s)", vartype))

    page.content <- paste0(page.content, "</th>\n")
  }

  page.content <- paste0(page.content, "  </tr>\n")


  # subsequent rows ----

  for (rcnt in 1:rowcnt) {

    # if we have alternating row colors, set css

    arcstring <- ""

    if (altr.row.col)
      arcstring <- ifelse(sjmisc::is_even(rcnt), " arc", "")

    if (rcnt == rowcnt)
      ltr <- " lasttablerow"
    else
      ltr <- ""

    page.content <- paste0(page.content, "  <tr>\n")

    # first table cell is rowname
    if (show.rownames)
      page.content <- paste0(page.content, sprintf(
        "    <td class=\"tdata leftalign firsttablecol%s%s\">%s</td>\n",
        ltr,
        arcstring,
        rnames[rcnt])
      )

    # all columns of a row
    for (ccnt in 1:colcnt) {

      # separate CSS for first column
      if (ccnt == 1 && !show.rownames)
        ftc <- " firsttablecol"
      else
        ftc <- ""


      page.content <- paste0(page.content, sprintf(
          "    <td class=\"tdata%s centertalign%s%s col%i\">%s</td>\n",
          ftc,
          ltr,
          arcstring,
          ccnt,
          mydf[rcnt, ccnt])
        )
    }

    page.content <- paste0(page.content, "</tr>\n")
  }


  # add optional "footnote" row ----

  if (show.footnote) {
    page.content <- paste0(page.content, "  <tr>\n")
    if (!show.rownames) colcnt <- colcnt - 1
    page.content <- paste0(page.content, sprintf("    <td colspan=\"%i\" class=\"footnote\">%s</td>\n", colcnt + 1, footnote))
    page.content <- paste0(page.content, "</tr>\n")
  }


  # finish html page ----
  paste0(page.content, "</table>\n")
}


rmspc <- function(html.table) {
  cleaned <- gsub("      <", "<", html.table, fixed = TRUE, useBytes = TRUE)
  cleaned <- gsub("    <", "<", cleaned, fixed = TRUE, useBytes = TRUE)
  cleaned <- gsub("  <", "<", cleaned, fixed = TRUE, useBytes = TRUE)

  cleaned
}


# This function creates the CSS style sheet for HTML-output

tab_df_style <- function(CSS = NULL, ...) {
  tab_df_prepare_style(CSS = CSS, content = NULL, task = 1, ...)
}


# This function creates the CSS style sheet for HTML-output, but
# converts the style-definition into inline-CSS, which is required
# for knitr documents, i.e. when HTML tables should be included in
# knitr documents.

tab_df_knitr <- function(CSS = NULL, content = NULL, ...) {
  tab_df_prepare_style(CSS = CSS, content = content, task = 2, ...)
}


# This functions creates the complete HTML page, include head and meta
# section of the final HTML page. Required for display in the browser.

tab_create_page <- function(style, content, encoding = "UTF-8") {
  # first, save table header
  sprintf(
    "<html>\n<head>\n<meta http-equiv=\"Content-type\" content=\"text/html;charset=%s\">\n%s\n</head>\n<body>\n%s\n</body></html>",
    encoding,
    style,
    content
  )
}


# This function does the actual preparation and transformation of
# the HTML style sheets, used by \code{tab_df_style()} and
# \code{tab_df_knitr()}

tab_df_prepare_style <- function(CSS = NULL, content = NULL, task, ...) {

  # init style sheet and tags used for css-definitions
  # we can use these variables for string-replacement
  # later for return value

  tag.table <- "table"
  tag.caption <- "caption"
  tag.thead <- "thead"
  tag.tdata <- "tdata"
  tag.arc <- "arc"
  tag.footnote <- "footnote"
  tag.subtitle <- "subtitle"
  tag.firsttablerow <- "firsttablerow"
  tag.lasttablerow <- "lasttablerow"
  tag.firsttablecol <- "firsttablecol"
  tag.leftalign <- "leftalign"
  tag.centertalign <- "centertalign"
  css.table <- "border-collapse:collapse; border:none;"
  css.caption <- "font-weight: bold; text-align:left;"
  css.thead <- "border-top: double; text-align:center; font-style:italic; font-weight:normal; padding:0.2cm;"
  css.tdata <- "padding:0.2cm; text-align:left; vertical-align:top;"
  css.arc <- "background-color:#f2f2f2;"
  css.lasttablerow <- "border-top:1px solid; border-bottom: double;"
  css.firsttablerow <- "border-bottom:1px solid black;"
  css.firsttablecol <- ""
  css.leftalign <- "text-align:left;"
  css.centeralign <- "text-align:center;"
  css.footnote <- "font-style:italic; border-top:double black; text-align:right;"
  css.subtitle <- "font-weight: normal;"


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
    if (!is.null(CSS[['css.footnote']])) css.footnote <- ifelse(substring(CSS[['css.footnote']], 1, 1) == '+', paste0(css.footnote, substring(CSS[['css.footnote']], 2)), CSS[['css.footnote']])
    if (!is.null(CSS[['css.subtitle']])) css.subtitle <- ifelse(substring(CSS[['css.subtitle']], 1, 1) == '+', paste0(css.subtitle, substring(CSS[['css.subtitle']], 2)), CSS[['css.subtitle']])
  }


  # set style sheet

  if (task == 1) {
    content <- sprintf(
      "<style>\nhtml, body { background-color: white; }\n%s { %s }\n%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n</style>",
      tag.table,
      css.table,
      tag.caption,
      css.caption,
      tag.thead,
      css.thead,
      tag.tdata,
      css.tdata,
      tag.arc,
      css.arc,
      tag.lasttablerow,
      css.lasttablerow,
      tag.firsttablerow,
      css.firsttablerow,
      tag.leftalign,
      css.leftalign,
      tag.centertalign,
      css.centeralign,
      tag.firsttablecol,
      css.firsttablecol,
      tag.footnote,
      css.footnote,
      tag.subtitle,
      css.subtitle
    )

  } else if (task == 2) {
    # set style attributes for main table tags
    content <- gsub("class=", "style=", content, fixed = TRUE, useBytes = TRUE)
    content <- gsub("<table", sprintf("<table style=\"%s\"", css.table), content, fixed = TRUE, useBytes = TRUE)
    content <- gsub("<caption", sprintf("<caption style=\"%s\"", css.caption), content, fixed = TRUE, useBytes = TRUE)

    # replace class-attributes with inline-style-definitions
    content <- gsub(tag.tdata, css.tdata, content, fixed = TRUE, useBytes = TRUE)
    content <- gsub(tag.thead, css.thead, content, fixed = TRUE, useBytes = TRUE)
    content <- gsub(tag.arc, css.arc, content, fixed = TRUE, useBytes = TRUE)
    content <- gsub(tag.footnote, css.footnote, content, fixed = TRUE, useBytes = TRUE)
    content <- gsub(tag.subtitle, css.subtitle, content, fixed = TRUE, useBytes = TRUE)
    content <- gsub(tag.lasttablerow, css.lasttablerow, content, fixed = TRUE, useBytes = TRUE)
    content <- gsub(tag.firsttablerow, css.firsttablerow, content, fixed = TRUE, useBytes = TRUE)
    content <- gsub(tag.firsttablecol, css.firsttablecol, content, fixed = TRUE, useBytes = TRUE)
    content <- gsub(tag.leftalign, css.leftalign, content, fixed = TRUE, useBytes = TRUE)
    content <- gsub(tag.centertalign, css.centeralign, content, fixed = TRUE, useBytes = TRUE)
  }

  content
}
