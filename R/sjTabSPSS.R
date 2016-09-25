#' @title View structure of labelled data frames
#' @name view_df
#'
#' @description Save (or show) content of an imported SPSS, SAS or Stata data file,
#'                or any similar labelled \code{data.frame}, as HTML table.
#'                Similar to the SPSS variable view. This quick overview shows
#'                variable ID numner, name, label, type and associated
#'                value labels. The result can be considered as "codeplan" of
#'                the data frame.
#'
#' @seealso \itemize{
#'            \item \href{http://www.strengejacke.de/sjPlot/datainit/}{sjPlot manual: data initialization}
#'            \item \href{http://www.strengejacke.de/sjPlot/view_spss/}{sjPlot manual: inspecting (SPSS imported) data frames}
#'          }
#'
#' @param x \code{data.frame}, imported by \code{\link[sjmisc]{read_spss}},
#'          \code{\link[sjmisc]{read_sas}} or \code{\link[sjmisc]{read_stata}} function,
#'          or any similar labelled data frame (see \code{\link[sjmisc]{set_label}}
#'          and \code{\link[sjmisc]{set_labels}}).
#' @param show.id logical, if \code{TRUE} (default), the variable ID is shown in the first column.
#' @param show.values logical, if \code{TRUE} (default), the variable values are shown as additional column.
#' @param show.labels logical, if \code{TRUE} (default), the value labels are shown as additional column.
#' @param show.frq logical, if \code{TRUE}, an additional column with frequencies for each variable is shown.
#' @param show.prc logical, if \code{TRUE}, an additional column with percentage of frequencies for each variable is shown.
#' @param show.wtd.frq logical, if \code{TRUE}, an additional column with weighted
#'          frequencies for each variable is shown. Weights strem from \code{weight.by}.
#' @param show.wtd.prc logical, if \code{TRUE}, an additional column with weighted
#'          percentage of frequencies for each variable is shown.
#'          Weights strem from \code{weight.by}.
#' @param sort.by.name logical, if \code{TRUE}, rows are sorted according to the variable
#'          names. By default, rows (variables) are ordered according to their
#'          order in the data frame.
#'
#' @inheritParams sjt.frq
#' @inheritParams sjt.df
#' @inheritParams sjt.xtab
#' @inheritParams sjp.grpfrq
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
#' \dontrun{
#' # init dataset
#' library(sjmisc)
#' data(efc)
#'
#' # view variables
#' view_df(efc)
#'
#' # view variables w/o values and value labels
#' view_df(efc, show.values = FALSE, show.labels = FALSE)
#'
#' # view variables including variable typed, orderd by name
#' view_df(efc, sort.by.name = TRUE, show.type = TRUE)
#'
#' # ----------------------------------------------------------------
#' # User defined style sheet
#' # ----------------------------------------------------------------
#' view_df(efc,
#'         CSS = list(css.table = "border: 2px solid;",
#'                    css.tdata = "border: 1px solid;",
#'                    css.arc = "color:blue;"))}
#'
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @importFrom sjmisc is_even get_values
#' @export
view_df <- function(x,
                    weight.by = NULL,
                    altr.row.col = TRUE,
                    show.id = TRUE,
                    show.type = FALSE,
                    show.values = TRUE,
                    show.labels = TRUE,
                    show.frq = FALSE,
                    show.prc = FALSE,
                    show.wtd.frq = FALSE,
                    show.wtd.prc = FALSE,
                    show.na = FALSE,
                    sort.by.name = FALSE,
                    wrap.labels = 50,
                    hide.progress = FALSE,
                    CSS = NULL,
                    encoding = NULL,
                    file = NULL,
                    use.viewer = TRUE,
                    no.output = FALSE,
                    remove.spaces = TRUE) {
# -------------------------------------
  # check encoding
  # -------------------------------------
  encoding <- get.encoding(encoding, x)
  # -------------------------------------
  # make data frame of single variable, so we have
  # unique handling for the data
  # -------------------------------------
  if (!is.data.frame(x)) stop("Parameter needs to be a data frame!", call. = FALSE)
  # -------------------------------------
  # retrieve value and variable labels
  # -------------------------------------
  df.var <- sjmisc::get_label(x)
  df.val <- sjmisc::get_labels(x)
  # -------------------------------------
  # get row count and ID's
  # -------------------------------------
  colcnt <- ncol(x)
  id <- seq_len(colcnt)
  # -------------------------------------
  # Order data set if requested
  # -------------------------------------
  if (sort.by.name) id <- id[order(colnames(x))]
  # -------------------------------------
  # init style sheet and tags used for css-definitions
  # we can use these variables for string-replacement
  # later for return value
  # -------------------------------------
  tag.table <- "table"
  tag.thead <- "thead"
  tag.tdata <- "tdata"
  tag.arc <- "arc"
  tag.caption <- "caption"
  css.table <- "border-collapse:collapse; border:none;"
  css.thead <- "border-bottom:double; font-style:italic; font-weight:normal; padding:0.2cm; text-align:left; vertical-align:top;"
  css.tdata <- "padding:0.2cm; text-align:left; vertical-align:top;"
  css.arc <- "background-color:#eaeaea"
  css.caption <- "font-weight: bold; text-align:left;"
  # ------------------------
  # check user defined style sheets
  # ------------------------
  if (!is.null(CSS)) {
    if (!is.null(CSS[['css.table']])) css.table <- ifelse(substring(CSS[['css.table']], 1, 1) == '+', paste0(css.table, substring(CSS[['css.table']], 2)), CSS[['css.table']])
    if (!is.null(CSS[['css.thead']])) css.thead <- ifelse(substring(CSS[['css.thead']], 1, 1) == '+', paste0(css.thead, substring(CSS[['css.thead']], 2)), CSS[['css.thead']])
    if (!is.null(CSS[['css.tdata']])) css.tdata <- ifelse(substring(CSS[['css.tdata']], 1, 1) == '+', paste0(css.tdata, substring(CSS[['css.tdata']], 2)), CSS[['css.tdata']])
    if (!is.null(CSS[['css.arc']])) css.arc <- ifelse(substring(CSS[['css.arc']], 1, 1) == '+', paste0(css.arc, substring(CSS[['css.arc']], 2)), CSS[['css.arc']])
    if (!is.null(CSS[['css.caption']])) css.caption <- ifelse(substring(CSS[['css.caption']], 1, 1) == '+', paste0(css.caption, substring(CSS[['css.caption']], 2)), CSS[['css.caption']])
  }
  # -------------------------------------
  # set style sheet
  # -------------------------------------
  page.style <- sprintf("<style>\n%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n%s { %s }\n</style>",
                        tag.table, css.table, tag.thead, css.thead, tag.tdata, css.tdata,
                        tag.arc, css.arc, tag.caption, css.caption)
  # -------------------------------------
  # table init
  # -------------------------------------
  toWrite <- sprintf("<html>\n<head>\n<meta http-equiv=\"Content-type\" content=\"text/html;charset=%s\">\n%s\n</head>\n<body>\n", encoding, page.style)
  # -------------------------------------
  # table caption, data frame name
  # -------------------------------------
  page.content <- sprintf("<table>\n  <caption>Data frame: %s</caption>\n", deparse(substitute(x)))
  # -------------------------------------
  # header row
  # -------------------------------------
  page.content <- paste0(page.content, "  <tr>\n    ")
  if (show.id) page.content <- paste0(page.content, "<th class=\"thead\">ID</th>")
  page.content <- paste0(page.content, "<th class=\"thead\">Name</th>")
  if (show.type) page.content <- paste0(page.content, "<th class=\"thead\">Type</th>")
  page.content <- paste0(page.content, "<th class=\"thead\">Label</th>")
  if (show.na) page.content <- paste0(page.content, "<th class=\"thead\">missings</th>")
  if (show.values) page.content <- paste0(page.content, "<th class=\"thead\">Values</th>")
  if (show.labels) page.content <- paste0(page.content, "<th class=\"thead\">Value Labels</th>")
  if (show.frq) page.content <- paste0(page.content, "<th class=\"thead\">Freq.</th>")
  if (show.prc) page.content <- paste0(page.content, "<th class=\"thead\">%</th>")
  if (show.wtd.frq) page.content <- paste0(page.content, "<th class=\"thead\">weighted Freq.</th>")
  if (show.wtd.prc) page.content <- paste0(page.content, "<th class=\"thead\">weighted %</th>")
  page.content <- paste0(page.content, "\n  </tr>\n")
  # -------------------------------------
  # create progress bar
  # -------------------------------------
  if (!hide.progress) pb <- utils::txtProgressBar(min = 0, max = colcnt, style = 3)
  # -------------------------------------
  # subsequent rows
  # -------------------------------------
  for (ccnt in seq_len(colcnt)) {
    # get index number, depending on sorting
    index <- id[ccnt]
    # default row string
    arcstring <- ""
    # if we have alternating row colors, set css
    if (altr.row.col) arcstring <- ifelse(sjmisc::is_even(ccnt), " arc", "")
    page.content <- paste0(page.content, "  <tr>\n")
    # ID
    if (show.id) page.content <- paste0(page.content, sprintf("    <td class=\"tdata%s\">%i</td>\n", arcstring, index))
    # name, and note
    if (!is.null(sjmisc::get_note(x[[index]])))
      td.title.tag <- sprintf(" title=\"%s\"", sjmisc::get_note(x[[index]]))
    else
      td.title.tag <- ""
    page.content <- paste0(page.content, sprintf("    <td class=\"tdata%s\"%s>%s</td>\n", arcstring, td.title.tag, colnames(x)[index]))
    # type
    if (show.type) {
      vartype <- get.vartype(x[[index]])
      page.content <- paste0(page.content,
                             sprintf("    <td class=\"tdata%s\">%s</td>\n",
                                     arcstring,
                                     vartype))
    }
    # label
    if (index <= length(df.var)) {
      varlab <- df.var[index]
      if (!is.null(wrap.labels)) {
        # wrap long variable labels
        varlab <- sjmisc::word_wrap(varlab, wrap.labels, "<br>")
      }
    } else {
      varlab <- "<NA>"
    }
    page.content <- paste0(page.content, sprintf("    <td class=\"tdata%s\">%s</td>\n", arcstring, varlab))
    # ----------------------------
    # missings and missing percentage
    # ----------------------------
    if (show.na) {
      page.content <- paste0(page.content,
                             sprintf("    <td class=\"tdata%s\">%i (%.2f%%)</td>\n",
                                     arcstring,
                                     sum(is.na(x[[index]]), na.rm = T),
                                     100 * sum(is.na(x[[index]]), na.rm = T) / nrow(x)))
    }
    # ----------------------------
    # values
    # ----------------------------
    if (show.values) {
      valstring <- ""
      # do we have valid index?
      if (index <= ncol(x)) {
        # if yes, get variable values
        vals <- sjmisc::get_values(x[[index]])
        # check if we have any values...
        if (!is.null(vals)) {
          # if we have values, put all values into a string
          for (i in 1:length(vals)) {
            valstring <- paste0(valstring, vals[i])
            if (i < length(vals)) valstring <- paste0(valstring, "<br>")
          }
        }
      } else {
        valstring <- "<NA>"
      }
      page.content <- paste0(page.content, sprintf("    <td class=\"tdata%s\">%s</td>\n", arcstring, valstring))
    }
    # ----------------------------
    # value labels
    # ----------------------------
    if (show.labels) {
      valstring <- ""
      # do we have valid index?
      if (index <= length(df.val)) {
        # if yes, get value labels
        # the code here corresponds to the above code
        # for variable values
        vals <- df.val[[index]]
        # check if we have any values...
        if (!is.null(vals)) {
          # if yes, add all to a string
          for (i in 1:length(vals)) {
            valstring <- paste0(valstring, vals[i])
            if (i < length(vals)) valstring <- paste0(valstring, "<br>")
          }
        }
      } else {
        valstring <- "<NA>"
      }
      page.content <- paste0(page.content, sprintf("    <td class=\"tdata%s\">%s</td>\n", arcstring, valstring))
    }
    # ----------------------------
    # frequencies
    # ----------------------------
    if (show.frq) {
      page.content <- paste0(page.content,
                             sprintf("    <td class=\"tdata%s\">%s</td>\n",
                                     arcstring,
                                     frq.value(index, x, df.val)))
    }
    # ----------------------------
    # percentage of frequencies
    # ----------------------------
    if (show.prc) {
      page.content <- paste0(page.content,
                             sprintf("    <td class=\"tdata%s\">%s</td>\n",
                                     arcstring,
                                     prc.value(index, x, df.val)))
    }
    # ----------------------------
    # frequencies
    # ----------------------------
    if (show.wtd.frq && !is.null(weight.by)) {
      page.content <- paste0(page.content,
                             sprintf("    <td class=\"tdata%s\">%s</td>\n",
                                     arcstring,
                                     frq.value(index, x, df.val, weight.by)))
    }
    # ----------------------------
    # percentage of frequencies
    # ----------------------------
    if (show.prc && !is.null(weight.by)) {
      page.content <- paste0(page.content,
                             sprintf("    <td class=\"tdata%s\">%s</td>\n",
                                     arcstring,
                                     prc.value(index, x, df.val, weight.by)))
    }
    # update progress bar
    if (!hide.progress) utils::setTxtProgressBar(pb, ccnt)
    # close row tag
    page.content <- paste0(page.content, "  </tr>\n")
  }
  if (!hide.progress) close(pb)
  # -------------------------------------
  # finish html page
  # -------------------------------------
  page.content <- paste(page.content, "</table>", sep = "\n")
  toWrite <- paste0(toWrite, sprintf("%s\n</body></html>", page.content))
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
  knitr <- gsub(tag.arc, css.arc, knitr, fixed = TRUE, useBytes = TRUE)
  # -------------------------------------
  # remove spaces?
  # -------------------------------------
  if (remove.spaces) {
    knitr <- sju.rmspc(knitr)
    toWrite <- sju.rmspc(toWrite)
    page.content <- sju.rmspc(page.content)
  }
  # -------------------------------------
  # check if html-content should be outputted
  # -------------------------------------
  out.html.table(no.output, file, knitr, toWrite, use.viewer)
  # -------------------------------------
  # return results
  # -------------------------------------
  invisible(structure(class = c("sjTable", "view_df"),
                      list(page.style = page.style,
                           page.content = page.content,
                           output.complete = toWrite,
                           knitr = knitr)))
}


frq.value <- function(index, x, df.val, weights = NULL) {
  valstring <- ""
  # check if we have a valid index
  if (index <= ncol(x) && !is.null(df.val[[index]])) {
    # do we have weights?
    if (!is.null(weights))
      variab <- sjstats::weight(x[[index]], weights)
    else
      variab <- x[[index]]
    # create frequency table. same function as for
    # sjt.frq and sjp.frq
    ftab <- create.frq.df(variab, 20)$mydat$frq
    # remove last value, which is N for NA
    if (length(ftab) == 1 && is.na(ftab)) {
      valstring <- "<NA>"
    } else {
      for (i in 1:(length(ftab) - 1)) {
        valstring <- paste0(valstring, ftab[i])
        if (i < length(ftab)) valstring <- paste0(valstring, "<br>")
      }
    }
  } else {
    valstring <- ""
  }
  return(valstring)
}

prc.value <- function(index, x, df.val, weights = NULL) {
  valstring <- ""
  # check for valid indices
  if (index <= ncol(x) && !is.null(df.val[[index]])) {
    # do we have weights?
    if (!is.null(weights))
      variab <- sjstats::weight(x[[index]], weights)
    else
      variab <- x[[index]]
    # create frequency table, but only get valid percentages
    ftab <- create.frq.df(variab, 20)$mydat$valid.prc
    # remove last value, which is a NA dummy
    if (length(ftab) == 1 && is.na(ftab)) {
      valstring <- "<NA>"
    } else {
      for (i in 1:(length(ftab) - 1)) {
        valstring <- paste0(valstring, sprintf("%.2f", ftab[i]))
        if (i < length(ftab)) valstring <- paste0(valstring, "<br>")
      }
    }
  } else {
    valstring <- ""
  }
  return(valstring)
}