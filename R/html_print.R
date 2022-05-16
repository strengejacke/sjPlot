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
#' @param digits Numeric, amount of digits after decimal point when rounding
#'   values.
#' @param rnames Character vector, can be used to set row names when \code{show.rownames=TRUE}.
#' @param ... Currently not used.
#'
#' @inheritParams tab_model
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
#'   See further examples in \href{https://strengejacke.github.io/sjPlot/articles/table_css.html}{this package-vignette}.
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
#' @export
tab_df <- function(x,
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
                   ...) {

  # make sure list elements in CSS argument have proper name attribute
  CSS <- check_css_param(CSS)

  # get style definition
  style <- tab_df_style(CSS = CSS, ...)

  rnames <- row.names(x)

  x <- as.data.frame(lapply(x, function(.i) {
    if (is.numeric(.i) && sjmisc::is_float(.i))
      sprintf("%.*f", digits, .i)
    else
      .i
  }))

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
      rnames = rnames,
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
        dat[] <- lapply(dat, function(.i) {
          if (is.numeric(.i) && sjmisc::is_float(.i))
            sprintf("%.*f", digits, .i)
          else
            .i
        })

        if (isTRUE(show.rownames)) {
          if (!is.null(rnames)) {
            tmp_rnames <- rnames
          } else {
            tmp_rnames <- row.names(dat)
          }
        }

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
          rnames = tmp_rnames,
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
      viewer = use.viewer
    )
  )
}

#' @importFrom dplyr "%>%"
# this function is used from tab_model()
tab_model_df <- function(x,
                         zeroinf,
                         is.zeroinf,
                         dv.labels,
                         rsq.list,
                         n_obs.list,
                         icc.list,
                         dev.list,
                         aic.list,
                         aicc.list,
                         variance.list,
                         ngrps.list,
                         loglik.list,
                         n.models,
                         title = NULL,
                         footnote = NULL,
                         col.header = NULL,
                         show.re.var = FALSE,
                         show.icc = FALSE,
                         digits.rsq = 3,
                         digits.re = 2,
                         encoding = "UTF-8",
                         CSS = NULL,
                         file = NULL,
                         use.viewer = TRUE,
                         ...) {

  # make sure list elements in CSS argument have proper name attribute
  CSS <- check_css_param(CSS)

  # get style definition
  style <- tab_df_style(CSS = CSS, ...)

  # check for monotonic effects
  sp <- string_starts_with("simo_mo", x$term)

  if (!sjmisc::is_empty(sp)) {
    x.sp <- dplyr::slice(x, !! sp)
    x <- dplyr::slice(x, -!! sp)
    x$term <- gsub(pattern = "^bsp_mo", replacement = "", x = x$term)
  } else
    x.sp <- NULL

  # get HTML content
  page.content <- tab_df_content(
    mydf = x,
    title = NULL,
    footnote = NULL,
    col.header = col.header,
    show.type = FALSE,
    show.rownames = FALSE,
    show.footnote = FALSE,
    show.header = TRUE,
    altr.row.col = FALSE,
    sort.column = NULL,
    include.table.tag = FALSE,
    no.last.table.row = TRUE,
    zeroinf = is.zeroinf,
    ...
  )

  # do we have labels for dependent variables / models?

  empty_dv <- is.null(dv.labels) | (length(dv.labels) == 1 && dv.labels == "")

  # replace CSS for first table row

  if (!empty_dv) {
    page.content <- gsub(
      pattern = "thead ",
      replacement = "depvarhead ",
      x = page.content,
      fixed = TRUE,
      useBytes = TRUE
    )
  } else {
    page.content <- gsub(
      pattern = "thead ",
      replacement = "depvarheadnodv ",
      x = page.content,
      fixed = TRUE,
      useBytes = TRUE
    )
  }

  # replace HTML-Tag for first table row

  page.content <- gsub(
    pattern = "<th ",
    replacement = "<td ",
    x = page.content,
    fixed = TRUE,
    useBytes = TRUE
  )

  page.content <- gsub(
    pattern = "</th",
    replacement = "</td",
    x = page.content,
    fixed = TRUE,
    useBytes = TRUE
  )


  # table column header, with label of dependent variables ----

  if (!empty_dv) {
    dv.content <- "  <tr>\n"
    dv.content <- paste0(dv.content, "    <th class=\"thead firsttablerow firsttablecol col1\">&nbsp;</th>\n")

    for (i in 1:length(dv.labels)) {
      colspan <- length(string_ends_with(sprintf("_%i", i), x = colnames(x)))
      dv.content <- paste0(
        dv.content,
        sprintf("    <th colspan=\"%i\" class=\"thead firsttablerow\">%s</th>\n", colspan, dv.labels[i])
      )
    }

    dv.content <- paste0(dv.content, "  </tr>\n")
    page.content <- paste0(dv.content, page.content)
  }


  # simplex parameters here ----

  if (!is.null(x.sp)) {

    x.sp$term <- gsub(
      pattern = "^simo_mo(.*)(\\.)(.*)(\\.)",
      replacement = "\\1 \\[\\3\\]",
      x = x.sp$term
    )

    sp.content <- tab_df_content(
      mydf = x.sp,
      title = NULL,
      footnote = NULL,
      col.header = NULL,
      show.type = FALSE,
      show.rownames = FALSE,
      show.footnote = FALSE,
      show.header = FALSE,
      altr.row.col = FALSE,
      sort.column = NULL,
      include.table.tag = FALSE,
      no.last.table.row = TRUE,
      ...
    )

    page.content <- paste0(page.content, "  <tr>\n")
    page.content <- paste0(page.content, sprintf("    <td colspan=\"%i\" class=\"simplexparts\">Monotonic Effects</td>\n", ncol(x)))
    page.content <- paste0(page.content, "  </tr>\n")

    page.content <- paste0(page.content, sp.content)
  }


  # zero inflation part here ----

  if (!is.null(zeroinf)) {

    rem <- 1:nrow(x)

    zero.part <- suppressMessages(
      x %>%
        dplyr::full_join(zeroinf) %>%
        dplyr::slice(!! -rem) %>%
        sjmisc::replace_na(value = "")
    )

    page.content <- paste0(page.content, "  <tr>\n")
    page.content <- paste0(page.content, sprintf("    <td colspan=\"%i\" class=\"zeroparts\">Zero-Inflated Model</td>\n", ncol(x)))
    page.content <- paste0(page.content, "  </tr>\n")

    zero.content <- tab_df_content(
      mydf = zero.part,
      title = NULL,
      footnote = NULL,
      col.header = NULL,
      show.type = FALSE,
      show.rownames = FALSE,
      show.footnote = FALSE,
      show.header = FALSE,
      altr.row.col = FALSE,
      sort.column = NULL,
      include.table.tag = FALSE,
      no.last.table.row = TRUE,
      zeroinf = FALSE,
      ...
    )

    page.content <- paste0(page.content, zero.content)
  }

  # prepare column span for summary information, including CSS

  summary.css <- "tdata summary summarydata"
  firstsumrow <- TRUE


  # add random effects ----

  if (!is_empty_list(variance.list) && show.re.var) {

    page.content <- paste0(page.content, "  <tr>\n")
    page.content <- paste0(page.content, sprintf("    <td colspan=\"%i\" class=\"randomparts\">Random Effects</td>\n", ncol(x)))
    page.content <- paste0(page.content, "  </tr>\n")


    ## random effects: within-group-variance: sigma ----

    s_css <- "tdata leftalign summary"
    page.content <- paste0(page.content, sprintf("\n  <tr>\n    <td class=\"%s\">&sigma;<sup>2</sup></td>\n", s_css))
    s_css <- summary.css

    for (i in 1:length(variance.list)) {

      if (length(variance.list) == 1)
        colspan <- ncol(x) - 1
      else
        colspan <- length(string_ends_with(sprintf("_%i", i), x = colnames(x)))

      if (is.null(variance.list[[i]])) {

        page.content <- paste0(
          page.content,
          sprintf("    <td class=\"%s\" colspan=\"%i\">&nbsp;</td>\n", s_css, as.integer(colspan))
        )

      } else {

        page.content <- paste0(
          page.content,
          sprintf(
            "    <td class=\"%s\" colspan=\"%i\">%.*f</td>\n",
            s_css,
            as.integer(colspan),
            digits.re,
            variance.list[[i]]$var.residual
          )
        )

      }
    }
    page.content <- paste0(page.content, "  </tr>\n")



    # random effects: Between-group-variance: tau.00 ----

    tau00 <- purrr::map(variance.list, ~ .x$var.intercept)
    tau00.len <- max(purrr::map_dbl(tau00, length))

    page.content <- paste0(
      page.content,
      create_random_effects(
        rv.len = tau00.len,
        rv = tau00,
        rv.string = "&tau;<sub>00</sub>",
        clean.rv = "tau.00",
        var.names = colnames(x),
        summary.css = summary.css,
        n.cols = ncol(x),
        digits.re = digits.re
    ))


    # random effects: random-slope-variance: tau11 ----

    has_rnd_slope <- purrr::map_lgl(variance.list, ~ !is.null(.x$var.slope))

    if (any(has_rnd_slope)) {

      tau11 <- purrr::map(variance.list, ~ .x$var.slope)
      tau11.len <- max(purrr::map_dbl(tau11, length))

      page.content <- paste0(
        page.content,
        create_random_effects(
          rv.len = tau11.len,
          rv = tau11,
          rv.string = "&tau;<sub>11</sub>",
          clean.rv = "tau.11",
          var.names = colnames(x),
          summary.css = summary.css,
          n.cols = ncol(x),
          digits.re = digits.re
      ))

      rho01 <- purrr::map(variance.list, ~ .x$cor.slope_intercept)
      rho01.len <- max(purrr::map_dbl(rho01, length))

      page.content <- paste0(
        page.content,
        create_random_effects(
          rv.len = rho01.len,
          rv = rho01,
          rv.string = "&rho;<sub>01</sub>",
          clean.rv = "rho.01",
          var.names = colnames(x),
          summary.css = summary.css,
          n.cols = ncol(x),
          digits.re = digits.re
      ))

    }

  }


  # add ICC ----

  if (!is_empty_list(icc.list) && show.icc) {

    page.content <- paste0(
      page.content,
      create_random_effects(
        rv.len = 1,
        rv = icc.list,
        rv.string = "ICC",
        clean.rv = "icc",
        var.names = colnames(x),
        summary.css = summary.css,
        n.cols = ncol(x),
        delim = ".adjusted",
        digits.re = digits.re
      ))

  }


  if (!is_empty_list(ngrps.list)) {

    ngrps.len <- max(purrr::map_dbl(ngrps.list, length))

    page.content <- paste0(
      page.content,
      create_random_effects(
        rv.len = ngrps.len,
        rv = ngrps.list,
        rv.string = "N",
        clean.rv = "",
        var.names = colnames(x),
        summary.css = summary.css,
        n.cols = ncol(x),
        delim = "ngrps.",
        as_int = TRUE
      ))

  }


  # add no of observations ----

  if (!is_empty_list(n_obs.list)) {

    # find first name occurence

    s_css <- "tdata leftalign summary"
    if (firstsumrow) s_css <- paste0(s_css, " firstsumrow")

    page.content <- paste0(page.content, "  <tr>\n")
    page.content <- paste0(page.content, sprintf("    <td class=\"%s\">Observations</td>\n", s_css))

    # print all r-squared to table

    s_css <- summary.css
    if (firstsumrow) s_css <- paste0(s_css, " firstsumrow")

    for (i in 1:length(n_obs.list)) {

      if (length(n_obs.list) == 1)
        colspan <- ncol(x) - 1
      else
        colspan <- length(string_ends_with(sprintf("_%i", i), x = colnames(x)))

      if (is.null(n_obs.list[[i]])) {

        page.content <- paste0(
          page.content,
          sprintf("    <td class=\"%s\" colspan=\"%i\">NA</td>\n", s_css, colspan)
        )

      } else {

        page.content <- paste0(
          page.content,
          sprintf(
            "    <td class=\"%s\" colspan=\"%i\">%i</td>\n",
            s_css,
            as.integer(colspan),
            as.integer(n_obs.list[[i]])
          )
        )

      }
    }

    firstsumrow <- FALSE
    page.content <- paste0(page.content, "  </tr>\n")
  }


  # add r-squared ----

  if (!is_empty_list(rsq.list)) {

    # find first name occurence

    for (i in 1:length(rsq.list)) {
      if (!is.null(rsq.list[[i]])) {
        rname <- names(rsq.list[[i]][1])
        if (length(rsq.list[[i]]) > 1)
          rname <- sprintf("%s / %s", rname, names(rsq.list[[i]][2]))
        break
      }
    }

    # superscript 2

    s_css <- "tdata leftalign summary"
    if (firstsumrow) s_css <- paste0(s_css, " firstsumrow")

    rname <- gsub("R2", "R<sup>2</sup>", rname, fixed = TRUE)
    rname <- gsub("R-squared", "R<sup>2</sup>", rname, fixed = TRUE)

    page.content <- paste0(page.content, "  <tr>\n")
    page.content <- paste0(page.content, sprintf("    <td class=\"%s\">%s</td>\n", s_css, rname))

    # print all r-squared to table

    s_css <- summary.css
    if (firstsumrow) s_css <- paste0(s_css, " firstsumrow")

    for (i in 1:length(rsq.list)) {

      if (length(rsq.list) == 1)
        colspan <- ncol(x) - 1
      else
        colspan <- length(string_ends_with(sprintf("_%i", i), x = colnames(x)))

      if (is.null(rsq.list[[i]]) || all(is.na(rsq.list[[i]])) || all(sjmisc::is_empty(rsq.list[[i]], first.only = FALSE))) {

        page.content <- paste0(
          page.content,
          sprintf("    <td class=\"%s\" colspan=\"%i\">NA</td>\n", s_css, as.integer(colspan))
        )

      } else if (length(rsq.list[[i]]) > 1) {

        page.content <- paste0(
          page.content,
          sprintf(
            "    <td class=\"%s\" colspan=\"%i\">%.*f / %.*f</td>\n",
            s_css,
            as.integer(colspan),
            digits.rsq,
            rsq.list[[i]][[1]],
            digits.rsq,
            rsq.list[[i]][[2]]
          )
        )

      } else {

        page.content <- paste0(
          page.content,
          sprintf(
            "    <td class=\"%s\" colspan=\"%i\">%.*f</td>\n",
            s_css,
            as.integer(colspan),
            digits.rsq,
            rsq.list[[i]][[1]]
          )
        )

      }
    }

    firstsumrow <- FALSE
    page.content <- paste0(page.content, "  </tr>\n")
  }


  # add deviance ----

  if (!is_empty_list(dev.list)) {
    page.content <- paste0(page.content, create_stats(
      data.list = dev.list,
      data.string = "Deviance",
      firstsumrow = firstsumrow,
      summary.css = summary.css,
      var.names = colnames(x),
      n.cols = ncol(x)
    ))
    firstsumrow <- FALSE
  }


  # add aic ----

  if (!is_empty_list(aic.list)) {
    page.content <- paste0(page.content, create_stats(
      data.list = aic.list,
      data.string = "AIC",
      firstsumrow = firstsumrow,
      summary.css = summary.css,
      var.names = colnames(x),
      n.cols = ncol(x)
    ))
    firstsumrow <- FALSE
  }


  # add aicc ----

  if (!is_empty_list(aicc.list)) {
    page.content <- paste0(page.content, create_stats(
      data.list = aicc.list,
      data.string = "AICc",
      firstsumrow = firstsumrow,
      summary.css = summary.css,
      var.names = colnames(x),
      n.cols = ncol(x)
    ))
    firstsumrow <- FALSE
  }


  # add logLik ----

  if (!is_empty_list(loglik.list)) {
    page.content <- paste0(page.content, create_stats(
      data.list = loglik.list,
      data.string = "log-Likelihood",
      firstsumrow = firstsumrow,
      summary.css = summary.css,
      var.names = colnames(x),
      n.cols = ncol(x)
    ))
    firstsumrow <- FALSE
  }


  ## TODO add bottom table border

  # add optional "footnote" row ----

  if (!is.null(footnote)) {
    page.content <- paste0(page.content, "  <tr>\n")
    page.content <- paste0(page.content, sprintf("    <td colspan=\"%i\" class=\"footnote\">%s</td>\n", ncol(x), footnote))
    page.content <- paste0(page.content, "</tr>\n")
  }


  # add table-caption ----

  if (!is.null(title))
    table.caption <- sprintf("<caption>%s</caption>\n", title)
  else
    table.caption <- ""


  # surround output with table-tag ----

  page.content <- paste0("<table>\n", table.caption, page.content, "\n</table>\n")

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
      viewer = use.viewer
    )
  )
}


create_random_effects <- function(rv.len, rv, rv.string, clean.rv, var.names, summary.css, n.cols, delim = "_", as_int = FALSE, digits.re = 2) {
  page.content <- ""
  pattern <- paste0("^", clean.rv, delim)

  for (i in 1:rv.len) {

    s_css <- "tdata leftalign summary"
    rvs <- rv.string
    rv.name <- gsub(pattern, "", names(rv[[1]][i]))

    if (length(rv) == 1 && !sjmisc::is_empty(rv.name))
      rvs <- sprintf("%s <sub>%s</sub>", rv.string, rv.name)
    else if (i > 1)
      rvs <- ""

    page.content <- paste0(
      page.content,
      sprintf("\n  <tr>\n    <td class=\"%s\">%s</td>\n", s_css, rvs)
    )
    s_css <- summary.css

    for (j in 1:length(rv)) {

      if (length(rv) == 1)
        colspan <- n.cols - 1
      else
        colspan <- length(string_ends_with(sprintf("_%i", j), x = var.names))

      if (is.null(rv[[j]]) || is.na(rv[[j]][i]) || sjmisc::is_empty(rv[[j]][i])) {

        page.content <- paste0(
          page.content,
          sprintf("    <td class=\"%s\" colspan=\"%i\">&nbsp;</td>\n", s_css, as.integer(colspan))
        )

      } else {

        rv.name <- gsub(pattern, "", names(rv[[j]][i]))

        if (length(rv) > 1 && !sjmisc::is_empty(rv.name))
          suffix <- sprintf(" <sub>%s</sub>", rv.name)
        else
          suffix <- ""

        if (as_int) {
          page.content <- paste0(
            page.content,
            sprintf(
              "    <td class=\"%s\" colspan=\"%i\">%i%s</td>\n",
              s_css,
              as.integer(colspan),
              as.integer(rv[[j]][i]),
              suffix
            )
          )
        } else {
          page.content <- paste0(
            page.content,
            sprintf(
              "    <td class=\"%s\" colspan=\"%i\">%.*f%s</td>\n",
              s_css,
              as.integer(colspan),
              digits.re,
              rv[[j]][i],
              suffix
            )
          )
        }

      }
    }
  }

  page.content
}


create_stats <- function(data.list, data.string, firstsumrow, summary.css, var.names, n.cols) {
  page.content <- ""

  s_css <- "tdata leftalign summary"
  if (firstsumrow) s_css <- paste0(s_css, " firstsumrow")

  page.content <- paste0(page.content, "  <tr>\n")
  page.content <- paste0(page.content, sprintf("    <td class=\"%s\">%s</td>\n", s_css, data.string))

  # print all r-squared to table

  s_css <- summary.css
  if (firstsumrow) s_css <- paste0(s_css, " firstsumrow")

  for (i in 1:length(data.list)) {

    if (length(data.list) == 1)
      colspan <- n.cols - 1
    else
      colspan <- length(string_ends_with(sprintf("_%i", i), x = var.names))

    if (is.null(data.list[[i]])) {

      page.content <- paste0(
        page.content,
        sprintf("    <td class=\"%s\" colspan=\"%i\">&nbsp;</td>\n", s_css, as.integer(colspan))
      )

    } else {

      page.content <- paste0(
        page.content,
        sprintf(
          "    <td class=\"%s\" colspan=\"%i\">%.3f</td>\n",
          s_css,
          as.integer(colspan),
          data.list[[i]]
        )
      )

    }
  }

  paste0(page.content, "  </tr>\n")
}
