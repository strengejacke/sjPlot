#' @title View structure of labelled data frames
#' @name view_df
#'
#' @description Save (or show) content of an imported SPSS, SAS or Stata data file,
#'                or any similar labelled \code{data.frame}, as HTML table.
#'                This quick overview shows variable ID number, name, label,
#'                type and associated value labels. The result can be
#'                considered as "codeplan" of the data frame.
#'
#' @seealso \itemize{
#'            \item \href{http://www.strengejacke.de/sjPlot/datainit/}{sjPlot manual: data initialization}
#'            \item \href{http://www.strengejacke.de/sjPlot/view_spss/}{sjPlot manual: inspecting (SPSS imported) data frames}
#'          }
#'
#' @param x A (labelled) data frame, imported by \code{\link[sjlabelled]{read_spss}},
#'          \code{\link[sjlabelled]{read_sas}} or \code{\link[sjlabelled]{read_stata}} function,
#'          or any similar labelled data frame (see \code{\link[sjlabelled]{set_label}}
#'          and \code{\link[sjlabelled]{set_labels}}).
#' @param show.id Logical, if \code{TRUE} (default), the variable ID is shown in the first column.
#' @param show.values Logical, if \code{TRUE} (default), the variable values are shown as additional column.
#' @param show.string.values Logical, if \code{TRUE}, elements of character vectors
#'    are also shown. By default, these are omitted due to possibly overlengthy
#'    tables.
#' @param show.labels Logical, if \code{TRUE} (default), the value labels are shown as additional column.
#' @param show.frq Logical, if \code{TRUE}, an additional column with frequencies for each variable is shown.
#' @param show.prc Logical, if \code{TRUE}, an additional column with percentage of frequencies for each variable is shown.
#' @param show.wtd.frq Logical, if \code{TRUE}, an additional column with weighted
#'          frequencies for each variable is shown. Weights strem from \code{weight.by}.
#' @param show.wtd.prc Logical, if \code{TRUE}, an additional column with weighted
#'          percentage of frequencies for each variable is shown.
#'          Weights strem from \code{weight.by}.
#' @param sort.by.name Logical, if \code{TRUE}, rows are sorted according to the variable
#'          names. By default, rows (variables) are ordered according to their
#'          order in the data frame.
#' @param max.len Numeric, indicates how many values and value labels per variable
#'    are shown. Useful for variables with many different values, where the output
#'    can be truncated.
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
#' # User defined style sheet
#' view_df(efc,
#'         CSS = list(css.table = "border: 2px solid;",
#'                    css.tdata = "border: 1px solid;",
#'                    css.arc = "color:blue;"))}
#'
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @importFrom sjmisc is_even var_type
#' @importFrom sjlabelled get_values
#' @export
view_df <- function(x,
                    weight.by = NULL,
                    altr.row.col = TRUE,
                    show.id = TRUE,
                    show.type = FALSE,
                    show.values = TRUE,
                    show.string.values = FALSE,
                    show.labels = TRUE,
                    show.frq = FALSE,
                    show.prc = FALSE,
                    show.wtd.frq = FALSE,
                    show.wtd.prc = FALSE,
                    show.na = FALSE,
                    max.len = 15,
                    sort.by.name = FALSE,
                    wrap.labels = 50,
                    hide.progress = FALSE,
                    CSS = NULL,
                    encoding = NULL,
                    file = NULL,
                    use.viewer = TRUE,
                    no.output = FALSE,
                    remove.spaces = TRUE) {
  # check encoding
  encoding <- get.encoding(encoding, x)

  # make data frame of single variable, so we have
  # unique handling for the data
  if (!is.data.frame(x)) stop("Parameter needs to be a data frame!", call. = FALSE)

  # retrieve value and variable labels
  df.var <- sjlabelled::get_label(x)
  df.val <- sjlabelled::get_labels(x)

  # get row count and ID's
  colcnt <- ncol(x)
  id <- seq_len(colcnt)

  # Order data set if requested
  if (sort.by.name) id <- id[order(colnames(x))]

  # init style sheet and tags used for css-definitions
  # we can use these variables for string-replacement
  # later for return value
  tag.table <- "table"
  tag.thead <- "thead"
  tag.tdata <- "tdata"
  tag.arc <- "arc"
  tag.caption <- "caption"
  tag.omit <- "omit"
  css.table <- "border-collapse:collapse; border:none;"
  css.thead <- "border-bottom:double; font-style:italic; font-weight:normal; padding:0.2cm; text-align:left; vertical-align:top;"
  css.tdata <- "padding:0.2cm; text-align:left; vertical-align:top;"
  css.arc <- "background-color:#eeeeee"
  css.caption <- "font-weight: bold; text-align:left;"
  css.omit <- "color:#999999;"

  # check user defined style sheets
  if (!is.null(CSS)) {
    if (!is.null(CSS[['css.table']])) css.table <- ifelse(substring(CSS[['css.table']], 1, 1) == '+', paste0(css.table, substring(CSS[['css.table']], 2)), CSS[['css.table']])
    if (!is.null(CSS[['css.thead']])) css.thead <- ifelse(substring(CSS[['css.thead']], 1, 1) == '+', paste0(css.thead, substring(CSS[['css.thead']], 2)), CSS[['css.thead']])
    if (!is.null(CSS[['css.tdata']])) css.tdata <- ifelse(substring(CSS[['css.tdata']], 1, 1) == '+', paste0(css.tdata, substring(CSS[['css.tdata']], 2)), CSS[['css.tdata']])
    if (!is.null(CSS[['css.arc']])) css.arc <- ifelse(substring(CSS[['css.arc']], 1, 1) == '+', paste0(css.arc, substring(CSS[['css.arc']], 2)), CSS[['css.arc']])
    if (!is.null(CSS[['css.caption']])) css.caption <- ifelse(substring(CSS[['css.caption']], 1, 1) == '+', paste0(css.caption, substring(CSS[['css.caption']], 2)), CSS[['css.caption']])
    if (!is.null(CSS[['css.omit']])) css.omit <- ifelse(substring(CSS[['css.omit']], 1, 1) == '+', paste0(css.omit, substring(CSS[['css.omit']], 2)), CSS[['css.omit']])
  }

  # set style sheet
  page.style <- sprintf("<style>\nhtml, body { background-color: white; }\n%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n%s { %s }\n.%s { %s }\n</style>",
                        tag.table, css.table, tag.thead, css.thead, tag.tdata, css.tdata,
                        tag.arc, css.arc, tag.caption, css.caption, tag.omit, css.omit)

  # table init
  toWrite <- sprintf("<html>\n<head>\n<meta http-equiv=\"Content-type\" content=\"text/html;charset=%s\">\n%s\n</head>\n<body>\n", encoding, page.style)

  # table caption, data frame name
  page.content <- sprintf("<table>\n  <caption>Data frame: %s</caption>\n", deparse(substitute(x)))

  # header row
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

  # create progress bar
  if (!hide.progress) pb <- utils::txtProgressBar(min = 0, max = colcnt, style = 3)

  # subsequent rows
  for (ccnt in seq_len(colcnt)) {
    # get index number, depending on sorting
    index <- id[ccnt]
    # default row string
    arcstring <- ""

    # if we have alternating row colors, set css
    if (altr.row.col) arcstring <- ifelse(sjmisc::is_even(ccnt), " arc", "")
    page.content <- paste0(page.content, "  <tr>\n")

    # ID
    if (show.id)
      page.content <-
      paste0(page.content,
             sprintf("    <td class=\"tdata%s\">%i</td>\n", arcstring, index))

    # name, and note
    if (!is.list(x[[index]]) && !is.null(sjlabelled::get_note(x[[index]])))
      td.title.tag <- sprintf(" title=\"%s\"", sjlabelled::get_note(x[[index]]))
    else
      td.title.tag <- ""

    page.content <-
      paste0(
        page.content,
        sprintf(
          "    <td class=\"tdata%s\"%s>%s</td>\n",
          arcstring,
          td.title.tag,
          colnames(x)[index]
        )
      )

    # type
    if (show.type) {
      vartype <- sjmisc::var_type(x[[index]])
      page.content <-
        paste0(page.content,
               sprintf("    <td class=\"tdata%s\">%s</td>\n", arcstring, vartype))
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

    # missings and missing percentage
    if (show.na) {
      if (is.list(x[[index]])) {
        page.content <- paste0(
          page.content,
          sprintf("    <td class=\"tdata%s\"><span class=\"omit\">&lt;list&gt;</span></td>\n", arcstring)
        )
      } else {
        page.content <- paste0(
          page.content,
          sprintf(
            "    <td class=\"tdata%s\">%i (%.2f%%)</td>\n",
            arcstring,
            sum(is.na(x[[index]]), na.rm = T),
            100 * sum(is.na(x[[index]]), na.rm = T) / nrow(x)
          )
        )
      }
    }


    # if value labels are shown, and we have numeric, non-labelled vectors,
    # show range istead of value labels here

    if (is.numeric(x[[index]]) && !has_value_labels(x[[index]])) {
      if (show.values || show.labels) {
        valstring <- paste0(sprintf("%i", range(x[[index]], na.rm = T)), collapse = "-")

        if (show.values && show.labels) {
          colsp <- " colspan=\"2\""
          valstring <- paste0("<em>range: ", valstring, "</em>")
        } else {
          colsp <- ""
        }

        page.content <-
          paste0(page.content, sprintf("    <td class=\"tdata%s\"%s>%s</td>\n", arcstring, colsp, valstring))
      }
    } else {
      # values
      if (show.values) {
        valstring <- ""
        # do we have valid index?
        if (index <= ncol(x)) {
          if (is.list(x[[index]])) {
            valstring <- "<span class=\"omit\">&lt;list&gt;</span>"
          } else {
            # if yes, get variable values
            vals <- sjlabelled::get_values(x[[index]])
            # check if we have any values...
            if (!is.null(vals)) {
              # if we have values, put all values into a string
              loop <- na.omit(seq_len(length(vals))[1:max.len])
              for (i in loop) {
                valstring <- paste0(valstring, vals[i])
                if (i < length(vals)) valstring <- paste0(valstring, "<br>")
              }
              if (max.len < length(vals))
                valstring <- paste0(valstring, "<span class=\"omit\">&lt;...&gt;</span>")
            }
          }
        } else {
          valstring <- "<NA>"
        }
        page.content <-
          paste0(page.content,
                 sprintf("    <td class=\"tdata%s\">%s</td>\n", arcstring, valstring))
      }

      # value labels
      if (show.labels) {
        valstring <- ""
        # do we have valid index?
        if (index <= length(df.val)) {
          if (is.list(x[[index]])) {
            valstring <- "<span class=\"omit\">&lt;list&gt;</span>"
          } else {
            # if yes, get value labels
            # the code here corresponds to the above code
            # for variable values
            vals <- df.val[[index]]
            if (!is.null(vals)) vals <- na.omit(vals)

            # sort character vectors
            if (is.character(x[[index]]) && !is.null(vals) && !sjmisc::is_empty(vals)) {
              if (show.string.values)
                vals <- sort(vals)
              else
                vals <- "<span class=\"omit\" title =\"'show.string.values = TRUE' to show values.\">&lt;output omitted&gt;</span>"
            }

            # check if we have any values...
            if (!is.null(vals)) {
              # if yes, add all to a string
              loop <- na.omit(seq_len(length(vals))[1:max.len])
              for (i in loop) {
                valstring <- paste0(valstring, vals[i])
                if (i < length(vals)) valstring <- paste0(valstring, "<br>")
              }
              if (max.len < length(vals))
                valstring <- paste0(valstring, "<span class=\"omit\">&lt;... truncated&gt;</span>")
            }
          }
        } else {
          valstring <- "<NA>"
        }
        page.content <- paste0(page.content, sprintf("    <td class=\"tdata%s\">%s</td>\n", arcstring, valstring))
      }
    }

    # frequencies
    if (show.frq) {
      if (is.list(x[[index]]))
        valstring <- "<span class=\"omit\">&lt;list&gt;</span>"
      else
        valstring <- frq.value(index, x, df.val)

      page.content <-
        paste0(page.content,
               sprintf(
                 "    <td class=\"tdata%s\">%s</td>\n",
                 arcstring,
                 valstring
               ))
    }

    # percentage of frequencies
    if (show.prc) {
      if (is.list(x[[index]]))
        valstring <- "<span class=\"omit\">&lt;list&gt;</span>"
      else
        valstring <- prc.value(index, x, df.val)

      page.content <-
        paste0(page.content,
               sprintf(
                 "    <td class=\"tdata%s\">%s</td>\n",
                 arcstring,
                 valstring
               ))
    }

    # frequencies
    if (show.wtd.frq && !is.null(weight.by)) {
      if (is.list(x[[index]]))
        valstring <- "<span class=\"omit\">&lt;list&gt;</span>"
      else
        valstring <- frq.value(index, x, df.val, weight.by)

      page.content <-
        paste0(page.content,
               sprintf(
                 "    <td class=\"tdata%s\">%s</td>\n",
                 arcstring,
                 valstring
               ))
    }

    # percentage of frequencies
    if (show.prc && !is.null(weight.by)) {
      if (is.list(x[[index]]))
        valstring <- "<span class=\"omit\">&lt;list&gt;</span>"
      else
        valstring <- prc.value(index, x, df.val, weight.by)

      page.content <-
        paste0(page.content,
               sprintf(
                 "    <td class=\"tdata%s\">%s</td>\n",
                 arcstring,
                 valstring
               ))
    }

    # update progress bar
    if (!hide.progress) utils::setTxtProgressBar(pb, ccnt)
    # close row tag
    page.content <- paste0(page.content, "  </tr>\n")
  }
  if (!hide.progress) close(pb)

  # finish html page
  page.content <- paste(page.content, "</table>", sep = "\n")
  toWrite <- paste0(toWrite, sprintf("%s\n</body></html>", page.content))

  # replace class attributes with inline style,
  # useful for knitr
  knitr <- page.content

  # set style attributes for main table tags
  knitr <- gsub("class=", "style=", knitr, fixed = TRUE, useBytes = TRUE)
  knitr <- gsub("<table", sprintf("<table style=\"%s\"", css.table), knitr, fixed = TRUE, useBytes = TRUE)

  # replace class-attributes with inline-style-definitions
  knitr <- gsub(tag.tdata, css.tdata, knitr, fixed = TRUE, useBytes = TRUE)
  knitr <- gsub(tag.thead, css.thead, knitr, fixed = TRUE, useBytes = TRUE)
  knitr <- gsub(tag.arc, css.arc, knitr, fixed = TRUE, useBytes = TRUE)

  # remove spaces?
  if (remove.spaces) {
    knitr <- sju.rmspc(knitr)
    toWrite <- sju.rmspc(toWrite)
    page.content <- sju.rmspc(page.content)
  }

  # check if html-content should be outputted
  #out.html.table(no.output, file, knitr, toWrite, use.viewer)

  # return results
  structure(
    class = c("sjTable", "view_df"),
    list(
      page.style = page.style,
      page.content = page.content,
      output.complete = toWrite,
      header = NULL,
      knitr = knitr,
      file = file,
      show = !no.output,
      use.viewer = use.viewer
    )
  )
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
