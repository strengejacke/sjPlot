#' @title View SPSS data set structure
#' @name sji.viewSPSS
#' 
#' @description Save (or show) content of an imported SPSS data file as HTML table.
#'                Similar to the SPSS variable view. This quick overview shows
#'                variable ID numner, name, label, type and associated
#'                value labels. The result can be considered as "codeplan" of
#'                the data frame.
#'
#' @seealso \itemize{
#'            \item \href{http://www.strengejacke.de/sjPlot/datainit/}{sjPlot manual: data initialization}
#'            \item \href{http://www.strengejacke.de/sjPlot/sji.viewSPSS/}{sjPlot manual: inspecting (SPSS imported) data frames}
#'            \item \code{\link{sji.SPSS}}
#'            \item \code{\link{sjt.df}}
#'          }
#' 
#' @param df An imported data frame, imported by \code{\link{sji.SPSS}} function.
#' @param file The destination file, which will be in html-format. If no filepath is specified,
#'          the file will be saved as temporary file and openend either in the IDE's viewer pane or
#'          in the default web browser.
#' @param alternateRowColors If \code{TRUE}, alternating rows are highlighted with a light gray
#'          background color.
#' @param showID If \code{TRUE} (default), the variable ID is shown in the first column.
#' @param showType If \code{TRUE}, the variable type is shown in a separate column. Since
#'          SPSS variable types are mostly \code{\link{numeric}} after import, this column
#'          is hidden by default.
#' @param showValues If \code{TRUE} (default), the variable values are shown as additional column.
#' @param showValueLabels If \code{TRUE} (default), the value labels are shown as additional column.
#' @param showFreq If \code{TRUE}, an additional column with frequencies for each variable is shown.
#' @param showPerc If \code{TRUE}, an additional column with percentage of frequencies for each variable is shown.
#' @param orderByName If \code{TRUE}, rows are ordered according to the variable
#'          names. By default, rows (variables) are ordered according to their
#'          order in the data frame.
#' @param breakVariableNamesAt Wordwrap for lomg variable names. Determines how many chars of
#'          a variable name are displayed in one line and when a line break is inserted.
#'          Default value is 50, use \code{NULL} to turn off word wrap.
#' @param hideProgressBar If \code{TRUE}, the progress bar that is displayed when creating the
#'          table is hidden. Default in \code{FALSE}, hence the bar is visible.
#' @param encoding The charset encoding used for variable and value labels. Default is \code{NULL}, so encoding
#'          will be auto-detected depending on your platform (\code{"UTF-8"} for Unix and \code{"Windows-1252"} for
#'          Windows OS). Change encoding if specific chars are not properly displayed (e.g.) German umlauts).
#' @param CSS A \code{\link{list}} with user-defined style-sheet-definitions, according to the 
#'          \href{http://www.w3.org/Style/CSS/}{official CSS syntax}. See return value \code{page.style} for details
#'          of all style-sheet-classnames that are used in this function. Parameters for this list need:
#'          \enumerate{
#'            \item the class-names with \code{"css."}-prefix as parameter name and
#'            \item each style-definition must end with a semicolon
#'          } 
#'          You can add style information to the default styles by using a + (plus-sign) as
#'          initial character for the parameter attributes. Examples:
#'          \itemize{
#'            \item \code{css.table='border:2px solid red;'} for a solid 2-pixel table border in red.
#'            \item \code{css.summary='font-weight:bold;'} for a bold fontweight in the summary row.
#'            \item \code{css.arc='color:blue;'} for a blue text color each 2nd row.
#'            \item \code{css.summary='+color:blue;'} to add blue font color style to the summary row.
#'          }
#'          See further examples below and \href{http://www.strengejacke.de/sjPlot/sjtbasics}{sjPlot manual: sjt-basics}.
#' @param useViewer If \code{TRUE}, the function tries to show the HTML table in the IDE's viewer pane. If
#'          \code{FALSE} or no viewer available, the HTML table is opened in a web browser.
#' @param no.output If \code{TRUE}, the html-output is neither opened in a browser nor shown in
#'          the viewer pane and not even saved to file. This option is useful when the html output
#'          should be used in \code{knitr} documents. The html output can be accessed via the return
#'          value.
#' @return Invisibly returns a \code{\link{structure}} with
#'          \itemize{
#'            \item the web page style sheet (\code{page.style}),
#'            \item the web page content (\code{page.content}),
#'            \item the complete html-output (\code{output.complete}) and
#'            \item the html-table with inline-css for use with knitr (\code{knitr})
#'            }
#'            for further use.
#'
#' @examples
#' \dontrun{
#' # init dataset
#' data(efc)
#' 
#' # view variables
#' sji.viewSPSS(efc)
#' 
#' # view variables w/o values and value labels
#' sji.viewSPSS(efc, showValues=FALSE, showValueLabels=FALSE)
#' 
#' # view variables including variable typed, orderd by name
#' sji.viewSPSS(efc, orderByName=TRUE, showType=TRUE)
#' 
#' # ---------------------------------------------------------------- 
#' # User defined style sheet
#' # ---------------------------------------------------------------- 
#' sji.viewSPSS(efc,
#'              CSS=list(css.table="border: 2px solid;",
#'                       css.tdata="border: 1px solid;",
#'                       css.arc="color:blue;"))}
#' 
#' @export
sji.viewSPSS <- function (df,
                          file=NULL,
                          alternateRowColors=TRUE,
                          showID=TRUE,
                          showType=FALSE,
                          showValues=TRUE,
                          showValueLabels=TRUE,
                          showFreq=FALSE,
                          showPerc=FALSE,
                          orderByName=FALSE,
                          breakVariableNamesAt=50,
                          encoding=NULL,
                          hideProgressBar=FALSE,
                          CSS=NULL,
                          useViewer=TRUE,
                          no.output=FALSE) {
  # -------------------------------------
  # check encoding
  # -------------------------------------
  encoding <- get.encoding(encoding)
  # -------------------------------------
  # make data frame of single variable, so we have
  # unique handling for the data
  # -------------------------------------
  if (!is.data.frame(df)) {
    stop("Parameter needs to be a data frame!", call.=FALSE)
  }
  # -------------------------------------
  # retrieve value and variable labels
  # -------------------------------------
  df.var <- sji.getVariableLabels(df)
  df.val <- sji.getValueLabels(df)
  # -------------------------------------
  # get row count and ID's
  # -------------------------------------
  rowcnt <- ncol(df)
  id <- 1:rowcnt
  # -------------------------------------
  # Order data set if requested
  # -------------------------------------
  if (orderByName) {
    # retrieve order
    id <- id[order(colnames(df))]
  }
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
    if (!is.null(CSS[['css.table']])) css.table <- ifelse(substring(CSS[['css.table']],1,1)=='+', paste0(css.table, substring(CSS[['css.table']],2)), CSS[['css.table']])
    if (!is.null(CSS[['css.thead']])) css.thead <- ifelse(substring(CSS[['css.thead']],1,1)=='+', paste0(css.thead, substring(CSS[['css.thead']],2)), CSS[['css.thead']])
    if (!is.null(CSS[['css.tdata']])) css.tdata <- ifelse(substring(CSS[['css.tdata']],1,1)=='+', paste0(css.tdata, substring(CSS[['css.tdata']],2)), CSS[['css.tdata']])
    if (!is.null(CSS[['css.arc']])) css.arc <- ifelse(substring(CSS[['css.arc']],1,1)=='+', paste0(css.arc, substring(CSS[['css.arc']],2)), CSS[['css.arc']])
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
  if (showValues) page.content <- paste0(page.content, "<th class=\"thead\">Values</th>")
  if (showValueLabels) page.content <- paste0(page.content, "<th class=\"thead\">Value Labels</th>")
  if (showFreq) page.content <- paste0(page.content, "<th class=\"thead\">Freq.</th>")
  if (showPerc) page.content <- paste0(page.content, "<th class=\"thead\">%</th>")
  page.content <- paste0(page.content, "\n  </tr>\n")
  # -------------------------------------
  # create progress bar
  # -------------------------------------
  if (!hideProgressBar) pb <- txtProgressBar(min=0, max=rowcnt, style=3)
  # -------------------------------------
  # subsequent rows
  # -------------------------------------
  for (rcnt in 1:rowcnt) {
    # get index number, depending on sorting
    index <- id[rcnt]
    # default row string
    arcstring <- ""
    # if we have alternating row colors, set css
    if (alternateRowColors) arcstring <- ifelse(rcnt %% 2 ==0, " arc", "")
    page.content <- paste0(page.content, "  <tr>\n")
    # ID
    if (showID) page.content <- paste0(page.content, sprintf("    <td class=\"tdata%s\">%i</td>\n", arcstring, index))
    # name
    page.content <- paste0(page.content, sprintf("    <td class=\"tdata%s\">%s</td>\n", arcstring, names(df.var[index])))
    # type
    if (showType) {
      vartype <- c("unknown type")
      if (is.character(df[,index])) vartype <- c("character")
      else if (is.factor(df[,index])) vartype <- c("factor")
      else if (is.numeric(df[,index])) vartype <- c("numeric")
      else if (is.atomic(df[,index])) vartype <- c("atomic")
      page.content <- paste0(page.content, sprintf("    <td class=\"tdata%s\">%s</td>\n", arcstring, vartype))
    }
    # label
    if (index<=length(df.var)) {
      varlab <- df.var[index]
      if (!is.null(breakVariableNamesAt)) {
        # wrap long variable labels
        varlab <- sju.wordwrap(varlab, breakVariableNamesAt, "<br>")
      }
    }
    else {
      varlab <- "<NA>"
    }
    page.content <- paste0(page.content, sprintf("    <td class=\"tdata%s\">%s</td>\n", arcstring, varlab))
    # values
    if (showValues) {
      if (index<=ncol(df)) {
        vals <- rev(unname(attr(df[,index], "value.labels")))
        valstring <- c("")
        for (i in 1:length(vals)) {
          valstring <- paste0(valstring, vals[i])
          if (i<length(vals)) valstring <- paste0(valstring, "<br>")
        }
      }
      else {
        valstring <- "<NA>"
      }
      page.content <- paste0(page.content, sprintf("    <td class=\"tdata%s\">%s</td>\n", arcstring, valstring))
    }
    # values
    if (showValueLabels) {
      if (index<=length(df.val)) {
        # value labels
        vals <- df.val[[index]]
        valstring <- c("")
        for (i in 1:length(vals)) {
          valstring <- paste0(valstring, vals[i])
          if (i<length(vals)) valstring <- paste0(valstring, "<br>")
        }
      }
      else {
        valstring <- "<NA>"
      }
      page.content <- paste0(page.content, sprintf("    <td class=\"tdata%s\">%s</td>\n", arcstring, valstring))
    }
    # frequencies
    if (showFreq) {
      if (index<=ncol(df) && !is.null(attr(df[,index], "value.labels"))) {
        ftab <- as.numeric(table(df[,index]))
        valstring <- c("")
        for (i in 1:length(ftab)) {
          valstring <- paste0(valstring, ftab[i])
          if (i<length(ftab)) valstring <- paste0(valstring, "<br>")
        }
      }
      else {
        valstring <- ""
      }
      page.content <- paste0(page.content, sprintf("    <td class=\"tdata%s\">%s</td>\n", arcstring, valstring))
    }
    # frequencies
    if (showPerc) {
      if (index<=ncol(df) && !is.null(attr(df[,index], "value.labels"))) {
        ftab <- 100*as.numeric(prop.table(table(df[,index])))
        valstring <- c("")
        for (i in 1:length(ftab)) {
          valstring <- paste0(valstring, sprintf("%.2f", ftab[i]))
          if (i<length(ftab)) valstring <- paste0(valstring, "<br>")
        }
      }
      else {
        valstring <- ""
      }
      page.content <- paste0(page.content, sprintf("    <td class=\"tdata%s\">%s</td>\n", arcstring, valstring))
    }
    # update progress bar
    if (!hideProgressBar) setTxtProgressBar(pb, rcnt)
    # close row tag
    page.content <- paste0(page.content, "  </tr>\n")
  }
  if (!hideProgressBar) close(pb)
  # -------------------------------------
  # finish html page
  # -------------------------------------
  page.content <- paste(page.content, "</table>", sep="\n")
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
  knitr <- gsub("class=", "style=", knitr)
  knitr <- gsub("<table", sprintf("<table style=\"%s\"", css.table), knitr)
  # -------------------------------------
  # replace class-attributes with inline-style-definitions
  # -------------------------------------
  knitr <- gsub(tag.tdata, css.tdata, knitr)
  knitr <- gsub(tag.thead, css.thead, knitr)
  knitr <- gsub(tag.arc, css.arc, knitr)
  # -------------------------------------
  # check if html-content should be outputted
  # -------------------------------------
  out.html.table(no.output, file, knitr, toWrite, useViewer)    
  # -------------------------------------
  # return results
  # -------------------------------------
  invisible (structure(class = "sjiviewspss",
                       list(page.style = page.style,
                            page.content = page.content,
                            output.complete = toWrite,
                            knitr = knitr)))
}
