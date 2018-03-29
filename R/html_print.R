#' @title Print data frames as HTML table.
#' @name tab_df
#'
#' @description These functions print data frames as HTML-table, showing
#'   the results in RStudio's viewer pane or in a web browser.
#'
#' @param x For \code{tab_df()}, a data frame; and for \code{tab_dfs()}, a
#'   list of data frames.
#' @param title,titles,footnote,footnotes Character vector with table
#'   caption(s) resp. footnote(s). For  \code{tab_df()}, must be a character
#'   of length 1; for \code{tab_dfs()}, a character vector of same length as
#'   \code{x} (i.e. one title or footnote per data frame).
#' @param col.header Character vector with elements used as column header for
#'   the table. If \code{NULL}, column names from \code{x} are used as
#'   column header.
#' @param encoding Character vector, indicating the charset encoding used
#'   for variable and value labels. Default is \code{"UTF-8"}. For Windows
#'   Systems, \code{encoding = "Windows-1252"} might be necessary for proper
#'   display of special characters.
#' @param show.type Logical, if \code{TRUE}, adds information about the
#'   variable type to the variable column.
#' @param show.rownames Logical, if \code{TRUE}, adds a column with the
#'   data frame's rowname to the table output.
#' @param show.footnote Logical, if \code{TRUE},adds a summary footnote below
#'   the table. For \code{tab_df()}, specify the string in \code{footnote},
#'   for \code{tab_dfs()} provide a character vector in \code{footnotes}.
#' @param sort.column Numeric vector, indicating the index of the column
#'   that should sorted. by default, the column is sorted in ascending order.
#'   Use negative index for descending order, for instance,
#'   \code{sort.column = -3} would sort the third column in descending order.
#'   Note that the first column with rownames is not counted.
#' @param alternate.rows Logical, if \code{TRUE}, rows are printed in
#'   alternatig colors (white and light grey by default).
#' @param ... Currently not used.
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
#' @note The HTML tables can either be saved as file and manually opened
#'   (use argument \code{file}) or they can be saved as temporary files and
#'   will be displayed in the RStudio Viewer pane (if working with RStudio)
#'   or opened with the default web browser. Displaying resp. opening a
#'   temporary file is the default behaviour.
#'
#' @details \strong{How do I use \code{CSS}-argument?}
#'   \cr \cr
#'   With the \code{CSS}-argument, the visual appearance of the tables
#'   can be modified. To get an overview of all style-sheet-classnames
#'   that are used in this function, see return value \code{page.style} for
#'   details. Arguments for this list have following syntax:
#'   \enumerate{
#'     \item the class-name as argument name and
#'     \item each style-definition must end with a semicolon
#'   }
#'   You can add style information to the default styles by using a +
#'   (plus-sign) as initial character for the argument attributes.
#'   Examples:
#'   \itemize{
#'     \item \code{table = 'border:2px solid red;'} for a solid 2-pixel table border in red.
#'     \item \code{summary = 'font-weight:bold;'} for a bold fontweight in the summary row.
#'     \item \code{lasttablerow = 'border-bottom: 1px dotted blue;'} for a blue dotted border of the last table row.
#'     \item \code{colnames = '+color:green'} to add green color formatting to column names.
#'     \item \code{arc = 'color:blue;'} for a blue text color each 2nd row.
#'     \item \code{caption = '+color:red;'} to add red font-color to the default table caption style.
#'   }
#'   See further examples in \href{../doc/sjtbasic.html}{this package-vignette}.
#'
#' @examples
#' \dontrun{
#' data(iris)
#' data(mtcars)
#' tab_df(iris[1:5, ])
#' tab_dfs(list(iris[1:5, ], mtcars[1:5, 1:5]))
#'
#' # sort 2nd column ascending
#' tab_df(iris[1:5, ], sort.column = 2)
#'
#' # sort 2nd column descending
#' tab_df(iris[1:5, ], sort.column = -2)}
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
                   sort.column = NULL,
                   encoding = "UTF-8",
                   CSS = NULL,
                   file = NULL,
                   use.viewer = TRUE,
                   ...) {

  # make sure list elements in CSS argument have proper name attribute
  CSS <- check_css_param(CSS)

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
      sort.column = sort.column,
      include.table.tag = TRUE,
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
                    sort.column = NULL,
                    encoding = "UTF-8",
                    CSS = NULL,
                    file = NULL,
                    use.viewer = TRUE,
                    ...) {

  # make sure list elements in CSS argument have proper name attribute
  CSS <- check_css_param(CSS)

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
          sort.column = sort.column,
          include.table.tag = TRUE,
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


# this function is used from tab_model()
tab_model_df <- function(x,
                         zeroinf.list,
                         title = NULL,
                         footnote = NULL,
                         col.header = NULL,
                         show.type = FALSE,
                         show.rownames = TRUE,
                         show.footnote = FALSE,
                         alternate.rows = FALSE,
                         sort.column = NULL,
                         encoding = "UTF-8",
                         CSS = NULL,
                         file = NULL,
                         use.viewer = TRUE,
                         ...) {
  # make sure list elements in CSS argument have proper name attribute
  CSS <- check_css_param(CSS)

  # get style definition
  style <- tab_df_style(CSS = CSS, ...)


  ## TODO surround output with table tag
  ## TODO add table caption afterwards

  # get HTML content
  page.content <- tab_df_content(
    mydf = x,
    title = NULL,
    footnote = footnote,
    col.header = col.header,
    show.type = show.type,
    show.rownames = show.rownames,
    show.footnote = show.footnote,
    altr.row.col = alternate.rows,
    sort.column = sort.column,
    include.table.tag = FALSE,
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
