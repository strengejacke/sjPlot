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
#' @param showID logical, if \code{TRUE} (default), the variable ID is shown in the first column.
#' @param showType logical, if \code{TRUE}, the variable type is shown in a separate column. Since
#'          SPSS variable types are mostly \code{\link{numeric}} after import, this column
#'          is hidden by default.
#' @param showValues logical, if \code{TRUE} (default), the variable values are shown as additional column.
#' @param showValueLabels logical, if \code{TRUE} (default), the value labels are shown as additional column.
#' @param showFreq logical, if \code{TRUE}, an additional column with frequencies for each variable is shown.
#' @param showPerc logical, if \code{TRUE}, an additional column with percentage of frequencies for each variable is shown.
#' @param showWtdFreq logical, if \code{TRUE}, an additional column with weighted
#'          frequencies for each variable is shown. Weights strem from \code{weightBy}.
#' @param showWtdPerc logical, if \code{TRUE}, an additional column with weighted
#'          percentage of frequencies for each variable is shown.
#'          Weights strem from \code{weightBy}.
#' @param sortByName logical, if \code{TRUE}, rows are sorted according to the variable
#'          names. By default, rows (variables) are ordered according to their
#'          order in the data frame.
#' @param breakVariableNamesAt Wordwrap for lomg variable names. Determines how many chars of
#'          a variable name are displayed in one line and when a line break is inserted.
#'          Default value is 50, use \code{NULL} to turn off word wrap.
#' @param hideProgressBar If \code{TRUE}, the progress bar that is displayed when creating the
#'          table is hidden. Default in \code{FALSE}, hence the bar is visible.
#'          
#' @inheritParams sjt.frq
#' @inheritParams sjt.df
#' @inheritParams sjt.xtab
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
#' view_df(efc, showValues = FALSE, showValueLabels = FALSE)
#' 
#' # view variables including variable typed, orderd by name
#' view_df(efc, sortByName = TRUE, showType = TRUE)
#' 
#' # ---------------------------------------------------------------- 
#' # User defined style sheet
#' # ---------------------------------------------------------------- 
#' view_df(efc,
#'         CSS = list(css.table = "border: 2px solid;",
#'                    css.tdata = "border: 1px solid;",
#'                    css.arc = "color:blue;"))}
#'
#' @import sjmisc 
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @export
view_df <- function(x,
                    weightBy = NULL,
                    file = NULL,
                    alternateRowColors = TRUE,
                    showID = TRUE,
                    showType = FALSE,
                    showValues = TRUE,
                    showValueLabels = TRUE,
                    showFreq = FALSE,
                    showPerc = FALSE,
                    showWtdFreq = FALSE,
                    showWtdPerc = FALSE,
                    showNA = FALSE,
                    sortByName = FALSE,
                    breakVariableNamesAt = 50,
                    encoding = NULL,
                    hideProgressBar = FALSE,
                    CSS = NULL,
                    useViewer = TRUE,
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
  id <- 1:colcnt
  # -------------------------------------
  # Order data set if requested
  # -------------------------------------
  if (sortByName) id <- id[order(colnames(x))]
  # -------------------------------------
  # init style sheet and tags used for css-definitions
  # we can use these variables for string-replacement
  # later for return value
  # -------------------------------------
  tag.table <- "table"
  tag.thead <- "thead"
  tag.tdata <- "tdata"
  tag.arc <- "arc"
  css.table <- "border-collapse:collapse; border:none;"
  css.thead <- "border-bottom:double; font-style:italic; font-weight:normal; padding:0.2cm; text-align:left; vertical-align:top;"
  css.tdata <- "padding:0.2cm; text-align:left; vertical-align:top;"
  css.arc <- "background-color:#eaeaea"
  # ------------------------
  # check user defined style sheets
  # ------------------------
  if (!is.null(CSS)) {
    if (!is.null(CSS[['css.table']])) css.table <- ifelse(substring(CSS[['css.table']], 1, 1) == '+', paste0(css.table, substring(CSS[['css.table']], 2)), CSS[['css.table']])
    if (!is.null(CSS[['css.thead']])) css.thead <- ifelse(substring(CSS[['css.thead']], 1, 1) == '+', paste0(css.thead, substring(CSS[['css.thead']], 2)), CSS[['css.thead']])
    if (!is.null(CSS[['css.tdata']])) css.tdata <- ifelse(substring(CSS[['css.tdata']], 1, 1) == '+', paste0(css.tdata, substring(CSS[['css.tdata']], 2)), CSS[['css.tdata']])
    if (!is.null(CSS[['css.arc']])) css.arc <- ifelse(substring(CSS[['css.arc']], 1, 1) == '+', paste0(css.arc, substring(CSS[['css.arc']], 2)), CSS[['css.arc']])
  }
  # -------------------------------------
  # set style sheet
  # -------------------------------------
  page.style <- sprintf("<style>\n%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n</style>",
                        tag.table, css.table, tag.thead, css.thead, tag.tdata, css.tdata, tag.arc, css.arc)
  # -------------------------------------
  # table init
  # -------------------------------------
  toWrite <- sprintf("<html>\n<head>\n<meta http-equiv=\"Content-type\" content=\"text/html;charset=%s\">\n%s\n</head>\n<body>\n", encoding, page.style)
  # -------------------------------------
  # start table tag
  # -------------------------------------
  page.content <- "<table>"
  # -------------------------------------
  # header row
  # -------------------------------------
  page.content <- paste0(page.content, "  <tr>\n    ")
  if (showID) page.content <- paste0(page.content, "<th class=\"thead\">ID</th>")
  page.content <- paste0(page.content, "<th class=\"thead\">Name</th>")
  if (showType) page.content <- paste0(page.content, "<th class=\"thead\">Type</th>")
  page.content <- paste0(page.content, "<th class=\"thead\">Label</th>")
  if (showNA) page.content <- paste0(page.content, "<th class=\"thead\">missings</th>")
  if (showValues) page.content <- paste0(page.content, "<th class=\"thead\">Values</th>")
  if (showValueLabels) page.content <- paste0(page.content, "<th class=\"thead\">Value Labels</th>")
  if (showFreq) page.content <- paste0(page.content, "<th class=\"thead\">Freq.</th>")
  if (showPerc) page.content <- paste0(page.content, "<th class=\"thead\">%</th>")
  if (showWtdFreq) page.content <- paste0(page.content, "<th class=\"thead\">weighted Freq.</th>")
  if (showWtdPerc) page.content <- paste0(page.content, "<th class=\"thead\">weighted %</th>")
  page.content <- paste0(page.content, "\n  </tr>\n")
  # -------------------------------------
  # create progress bar
  # -------------------------------------
  if (!hideProgressBar) pb <- utils::txtProgressBar(min = 0, 
                                                    max = colcnt, 
                                                    style = 3)
  # -------------------------------------
  # subsequent rows
  # -------------------------------------
  for (ccnt in 1:colcnt) {
    # get index number, depending on sorting
    index <- id[ccnt]
    # default row string
    arcstring <- ""
    # if we have alternating row colors, set css
    if (alternateRowColors) arcstring <- ifelse(sjmisc::is_even(ccnt), " arc", "")
    page.content <- paste0(page.content, "  <tr>\n")
    # ID
    if (showID) page.content <- paste0(page.content, sprintf("    <td class=\"tdata%s\">%i</td>\n", arcstring, index))
    # name
    page.content <- paste0(page.content, sprintf("    <td class=\"tdata%s\">%s</td>\n", arcstring, colnames(x)[index]))
    # type
    if (showType) {
      vartype <- get.vartype(x[[index]])
      page.content <- paste0(page.content, 
                             sprintf("    <td class=\"tdata%s\">%s</td>\n", 
                                     arcstring, 
                                     vartype))
    }
    # label
    if (index <= length(df.var)) {
      varlab <- df.var[index]
      if (!is.null(breakVariableNamesAt)) {
        # wrap long variable labels
        varlab <- sjmisc::word_wrap(varlab, breakVariableNamesAt, "<br>")
      }
    } else {
      varlab <- "<NA>"
    }
    page.content <- paste0(page.content, sprintf("    <td class=\"tdata%s\">%s</td>\n", arcstring, varlab))
    # ----------------------------
    # missings and missing percentage
    # ----------------------------
    if (showNA) {
      page.content <- paste0(page.content, 
                             sprintf("    <td class=\"tdata%s\">%i (%.2f%%)</td>\n", 
                                     arcstring, 
                                     sum(is.na(x[[index]]), na.rm = T),
                                     100 * sum(is.na(x[[index]]), na.rm = T) / nrow(x)))
    }
    # ----------------------------
    # values
    # ----------------------------
    if (showValues) {
      valstring <- c("")
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
    if (showValueLabels) {
      valstring <- c("")
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
    if (showFreq) {
      page.content <- paste0(page.content, 
                             sprintf("    <td class=\"tdata%s\">%s</td>\n", 
                                     arcstring, 
                                     frq.value(index, x, df.val)))
    }
    # ----------------------------
    # percentage of frequencies
    # ----------------------------
    if (showPerc) {
      page.content <- paste0(page.content, 
                             sprintf("    <td class=\"tdata%s\">%s</td>\n", 
                                     arcstring, 
                                     prc.value(index, x, df.val)))
    }
    # ----------------------------
    # frequencies
    # ----------------------------
    if (showWtdFreq && !is.null(weightBy)) {
      page.content <- paste0(page.content, 
                             sprintf("    <td class=\"tdata%s\">%s</td>\n", 
                                     arcstring, 
                                     frq.value(index, x, df.val, weightBy)))
    }
    # ----------------------------
    # percentage of frequencies
    # ----------------------------
    if (showPerc && !is.null(weightBy)) {
      page.content <- paste0(page.content, 
                             sprintf("    <td class=\"tdata%s\">%s</td>\n", 
                                     arcstring, 
                                     prc.value(index, x, df.val, weightBy)))
    }
    # update progress bar
    if (!hideProgressBar) utils::setTxtProgressBar(pb, ccnt)
    # close row tag
    page.content <- paste0(page.content, "  </tr>\n")
  }
  if (!hideProgressBar) close(pb)
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
  out.html.table(no.output, file, knitr, toWrite, useViewer)    
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
      variab <- sjmisc::weight(x[[index]], weights)
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
      variab <- sjmisc::weight(x[[index]], weights)
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