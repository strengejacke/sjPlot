#' @title Summary of frequencies as HTML table
#' @name sjt.frq
#' 
#' @description Shows (multiple) frequency tables as HTML file, or saves them as file.
#' 
#' @seealso \itemize{
#'            \item \href{http://www.strengejacke.de/sjPlot/sjt.frq/}{sjPlot manual: sjt.frq}
#'            \item \code{\link{sjp.frq}}
#'          }
#' 
#' @param data variables which frequencies should be printed as table. Either use a single variable
#'          (vector) or a data frame where each column represents a variable (see 'Examples').
#' @param file destination file, if the output should be saved as file.
#'          If \code{NULL} (default), the output will be saved as temporary file and 
#'          openend either in the IDE's viewer pane or the default web browser.
#' @param title table caption, as character vector.
#' @param value.labels character vector (or \code{list} of character vectors)
#'          with value labels of the supplied variables, which will be used 
#'          to label variable values in the output.
#' @param altr.row.col logical, if \code{TRUE}, alternating rows are highlighted with a light gray
#'          background color.
#' @param string.val label for the very first table column containing the values (see
#'          \code{value.labels}).
#' @param string.cnt label for the first table data column containing the counts. Default is \code{"N"}.
#' @param string.prc label for the second table data column containing the raw percentages. Default is \code{"raw \%"}.
#' @param string.vprc String label for the third data table column containing the valid percentages, i.e. the
#'          count percentage value exluding possible missing values.
#' @param string.cprc String label for the last table data column containing the cumulative percentages.
#' @param string.na String label for the last table data row containing missing values.
#' @param emph.md If \code{TRUE}, the table row indicating the median value will
#'          be emphasized.
#' @param emph.quart If \code{TRUE}, the table row indicating the lower and upper quartiles will
#'          be emphasized.
#' @param skip.zero If \code{TRUE}, rows with only zero-values are not printed
#'          (e.g. if a variable has values or levels 1 to 8, and levels / values 
#'          4 to 6 have no counts, these values would not be printed in the table). 
#'          Use \code{FALSE} to print also zero-values, or use \code{"auto"} (default)
#'          to detect whether it makes sense or not to print zero-values (e.g., a variable
#'          "age" with values from 10 to 100, where at least 25 percent of all possible values have no
#'          counts, zero-values would be skipped automatically).
#' @param show.summary If \code{TRUE} (default), a summary row with total and valid N as well as mean and
#'          standard deviation is shown.
#' @param show.skew If \code{TRUE}, the variable's skewness is added to the summary.
#'          The skewness is retrieved from the \code{\link[psych]{describe}}-function 
#'          of the \pkg{psych}-package and indicated by a lower case Greek gamma.
#' @param show.kurtosis If \code{TRUE}, the variable's kurtosis is added to the summary.
#'          The kurtosis is retrieved from the \code{\link[psych]{describe}}-function 
#'          of the \pkg{psych}-package and indicated by a lower case Greek omega.
#' @param ignore.strings If \code{TRUE} (default), character vectors / string variables will be removed from
#'          \code{data} before frequency tables are computed.
#' @param auto.grp.strings if \code{TRUE} (default), string values in character 
#'          vectors (string variables) are automatically grouped based on their 
#'          similarity. The similarity is estimated with the \pkg{stringdist}-package.
#'          You can specify a distance-measure via \code{max.string.dist} argument. This argument only
#'          applies if \code{ignore.strings} is \code{FALSE}.
#' @param max.string.dist the allowed distance of string values in a character vector, which indicates
#'          when two string values are merged because they are considered as close enough.
#'          See \code{auto.grp.strings}.
#' @param encoding string, indicating the charset encoding used for variable and 
#'          value labels. Default is \code{NULL}, so encoding will be auto-detected 
#'          depending on your platform (e.g., \code{"UTF-8"} for Unix and \code{"Windows-1252"} for
#'          Windows OS). Change encoding if specific chars are not properly displayed (e.g. German umlauts).
#' @param CSS \code{\link{list}}-object with user-defined style-sheet-definitions, according to the 
#'          \href{http://www.w3.org/Style/CSS/}{official CSS syntax}. See 'Details'.
#' @param use.viewer If \code{TRUE}, the HTML table is shown in the IDE's viewer pane. If
#'          \code{FALSE} or no viewer available, the HTML table is opened in a web browser.
#' @param no.output logical, if \code{TRUE}, the html-output is neither opened in a browser nor shown in
#'          the viewer pane and not even saved to file. This option is useful when the html output
#'          should be used in \code{knitr} documents. The html output can be accessed via the return
#'          value.
#' @param remove.spaces logical, if \code{TRUE}, leading spaces are removed from all lines in the final string
#'          that contains the html-data. Use this, if you want to remove parantheses for html-tags. The html-source
#'          may look less pretty, but it may help when exporting html-tables to office tools.
#' @return Invisibly returns
#'          \itemize{
#'            \item the web page style sheet (\code{page.style}),
#'            \item each frequency table as web page content (\code{page.content.list}),
#'            \item the complete html-output (\code{output.complete}) and
#'            \item the html-table with inline-css for use with knitr (\code{knitr})
#'            }
#'            for further use.
#'          
#' @inheritParams sjp.glmer
#' @inheritParams sjp.grpfrq
#' @inheritParams sjp.frq
#' 
#' @note The HTML tables can either be saved as file and manually opened (specify argument \code{file}) or
#'         they can be saved as temporary files and will be displayed in the RStudio Viewer pane (if working with RStudio)
#'         or opened with the default web browser. Displaying resp. opening a temporary file is the
#'         default behaviour (i.e. \code{file = NULL}).
#' 
#' @details \bold{How does the \code{CSS}-argument work?}
#'            \cr \cr
#'            With the \code{CSS}-argument, the visual appearance of the tables
#'            can be modified. To get an overview of all style-sheet-classnames 
#'            that are used in this function, see return value \code{page.style} for details. 
#'            Arguments for this list have following syntax:
#'          \enumerate{
#'            \item the class-names with \code{"css."}-prefix as argument name and
#'            \item each style-definition must end with a semicolon
#'          } 
#'          You can add style information to the default styles by using a + (plus-sign) as
#'          initial character for the argument attributes. Examples:
#'          \itemize{
#'            \item \code{css.table = 'border:2px solid red;'} for a solid 2-pixel table border in red.
#'            \item \code{css.summary = 'font-weight:bold;'} for a bold fontweight in the summary row.
#'            \item \code{css.lasttablerow = 'border-bottom: 1px dotted blue;'} for a blue dotted border of the last table row.
#'            \item \code{css.colnames = '+color:green'} to add green color formatting to column names.
#'            \item \code{css.arc = 'color:blue;'} for a blue text color each 2nd row.
#'            \item \code{css.caption = '+color:red;'} to add red font-color to the default table caption style.
#'          }
#'          See further examples at \href{http://www.strengejacke.de/sjPlot/sjtbasics}{sjPlot manual: sjt-basics}.
#'          
#' @examples
#' \dontrun{
#' # load sample data
#' library(sjmisc)
#' data(efc)
#' 
#' # show frequencies of "e42dep" in RStudio Viewer Pane
#' # or default web browser
#' sjt.frq(efc$e42dep)
#' 
#' # plot and show frequency table of "e42dep" with labels
#' sjt.frq(efc$e42dep, title = "Dependency",
#'         value.labels = c("independent", "slightly dependent",
#'                          "moderately dependent", "severely dependent"))
#' 
#' # plot frequencies of e42dep, e16sex and c172code in one HTML file
#' # and show table in RStudio Viewer Pane or default web browser
#' # Note that value.labels of multiple variables have to be
#' # list-objects
#' sjt.frq(data.frame(efc$e42dep, efc$e16sex, efc$c172code),
#'         title = c("Dependency", "Gender", "Education"),
#'         value.labels = list(c("independent", "slightly dependent",
#'                               "moderately dependent", "severely dependent"),
#'                             c("male", "female"), c("low", "mid", "high")))
#' 
#' # auto-detection of labels
#' sjt.frq(data.frame(efc$e42dep, efc$e16sex, efc$c172code))
#' 
#' # plot larger scale including zero-counts
#' # indicating median and quartiles
#' sjt.frq(efc$neg_c_7, emph.md = TRUE, emph.quart = TRUE)
#' 
#' # sort frequencies
#' sjt.frq(efc$e42dep, sort.frq = "desc")
#' 
#' # User defined style sheet
#' sjt.frq(efc$e42dep,
#'         CSS = list(css.table = "border: 2px solid;",
#'                    css.tdata = "border: 1px solid;",
#'                    css.firsttablecol = "color:#003399; font-weight:bold;"))}
#' 
#' @importFrom psych describe
#' @importFrom stats na.omit weighted.mean
#' @importFrom sjmisc get_note group_str
#' @export
sjt.frq <- function(data,
                    weight.by = NULL,
                    title.wtd.suffix = " (weighted)",
                    title = NULL,
                    value.labels = NULL,
                    sort.frq = c("none", "asc", "desc"),
                    altr.row.col = FALSE,
                    string.val = "value",
                    string.cnt = "N",
                    string.prc = "raw %",
                    string.vprc = "valid %",
                    string.cprc = "cumulative %",
                    string.na = "missings",
                    emph.md = FALSE,
                    emph.quart = FALSE,
                    show.summary = TRUE,
                    show.skew = FALSE,
                    show.kurtosis = FALSE,
                    skip.zero = "auto",
                    ignore.strings = TRUE,
                    auto.group = NULL,
                    auto.grp.strings = TRUE,
                    max.string.dist = 3,
                    digits = 2,
                    CSS = NULL,
                    encoding = NULL,
                    file = NULL, 
                    use.viewer = TRUE,
                    no.output = FALSE,
                    remove.spaces = TRUE) {
  # -------------------------------------
  # check encoding
  # -------------------------------------
  encoding <- get.encoding(encoding, data)
  # save original value
  o.skip.zero <- skip.zero
  # -------------------------------------
  # match arguments
  # -------------------------------------
  sort.frq <- match.arg(sort.frq)
  # -------------------------------------
  # warning
  # -------------------------------------
  if (!is.null(sort.frq) && sort.frq != "none") message("Sorting may not work when data contains values with zero-counts.")
  # -------------------------------------
  # table init
  # -------------------------------------
  toWrite <- sprintf("<html>\n<head>\n<meta http-equiv=\"Content-type\" content=\"text/html;charset=%s\">\n", encoding)
  # -------------------------------------
  # init style sheet and tags used for css-definitions
  # we can use these variables for string-replacement
  # later for return value
  # -------------------------------------
  tag.table <- "table"
  tag.thead <- "thead"
  tag.tdata <- "tdata"
  tag.caption <- "caption"
  tag.summary <- "summary"
  tag.arc <- "arc"
  tag.qrow <- "qrow"
  tag.mdrow <- "mdrow"
  tag.abstand <- "abstand"
  tag.lasttablerow <- "lasttablerow"
  tag.firsttablerow <- "firsttablerow"
  tag.leftalign <- "leftalign"
  tag.centeralign <- "centeralign"
  tag.firsttablecol <- "firsttablecol"
  css.table <- "border-collapse:collapse; border:none;"
  css.thead <- "border-top:double; text-align:center; font-style:italic; font-weight:normal; padding-left:0.2cm; padding-right:0.2cm;"
  css.tdata <- "padding:0.2cm;"
  css.caption <- "font-weight: bold; text-align:left;"
  css.summary <- "text-align:right; font-style:italic; font-size:0.9em; padding-top:0.1cm; padding-bottom:0.1cm;"
  css.arc <- "background-color:#eaeaea;"
  css.qrow <- "border-bottom: 1px solid #cc3333;"
  css.mdrow <- "font-weight:bolder; font-style:italic; color:#993333;"
  css.abstand <- "margin-bottom: 2em;"
  css.lasttablerow <- "border-top:1px solid; border-bottom:double;"
  css.firsttablerow <- "border-bottom:1px solid;"
  css.leftalign <- "text-align:left;"
  css.centeralign <- "text-align:center;"
  css.firsttablecol <- ""
  # ------------------------
  # check user defined style sheets
  # ------------------------
  if (!is.null(CSS)) {
    if (!is.null(CSS[['css.table']])) css.table <- ifelse(substring(CSS[['css.table']], 1, 1) == '+', paste0(css.table, substring(CSS[['css.table']], 2)), CSS[['css.table']])
    if (!is.null(CSS[['css.thead']])) css.thead <- ifelse(substring(CSS[['css.thead']], 1, 1) == '+', paste0(css.thead, substring(CSS[['css.thead']], 2)), CSS[['css.thead']])
    if (!is.null(CSS[['css.tdata']])) css.tdata <- ifelse(substring(CSS[['css.tdata']], 1, 1) == '+', paste0(css.tdata, substring(CSS[['css.tdata']], 2)), CSS[['css.tdata']])
    if (!is.null(CSS[['css.caption']])) css.caption <- ifelse(substring(CSS[['css.caption']], 1, 1) == '+', paste0(css.caption, substring(CSS[['css.caption']], 2)), CSS[['css.caption']])
    if (!is.null(CSS[['css.summary']])) css.summary <- ifelse(substring(CSS[['css.summary']], 1, 1) == '+', paste0(css.summary, substring(CSS[['css.summary']], 2)), CSS[['css.summary']])
    if (!is.null(CSS[['css.arc']])) css.arc <- ifelse(substring(CSS[['css.arc']], 1, 1) == '+', paste0(css.arc, substring(CSS[['css.arc']], 2)), CSS[['css.arc']])
    if (!is.null(CSS[['css.qrow']])) css.qrow <- ifelse(substring(CSS[['css.qrow']], 1, 1) == '+', paste0(css.qrow, substring(CSS[['css.qrow']], 2)), CSS[['css.qrow']])
    if (!is.null(CSS[['css.mdrow']])) css.mdrow <- ifelse(substring(CSS[['css.mdrow']], 1, 1) == '+', paste0(css.mdrow, substring(CSS[['css.mdrow']], 2)), CSS[['css.mdrow']])
    if (!is.null(CSS[['css.abstand']])) css.abstand <- ifelse(substring(CSS[['css.abstand']], 1, 1) == '+', paste0(css.abstand, substring(CSS[['css.abstand']], 2)), CSS[['css.abstand']])
    if (!is.null(CSS[['css.lasttablerow']])) css.lasttablerow <- ifelse(substring(CSS[['css.lasttablerow']], 1, 1) == '+', paste0(css.lasttablerow, substring(CSS[['css.lasttablerow']], 2)), CSS[['css.lasttablerow']])
    if (!is.null(CSS[['css.firsttablerow']])) css.firsttablerow <- ifelse(substring(CSS[['css.firsttablerow']], 1, 1) == '+', paste0(css.firsttablerow, substring(CSS[['css.firsttablerow']], 2)), CSS[['css.firsttablerow']])
    if (!is.null(CSS[['css.leftalign']])) css.leftalign <- ifelse(substring(CSS[['css.leftalign']], 1, 1) == '+', paste0(css.leftalign, substring(CSS[['css.leftalign']], 2)), CSS[['css.leftalign']])
    if (!is.null(CSS[['css.centeralign']])) css.centeralign <- ifelse(substring(CSS[['css.centeralign']], 1, 1) == '+', paste0(css.centeralign, substring(CSS[['css.centeralign']], 2)), CSS[['css.centeralign']])
    if (!is.null(CSS[['css.firsttablecol']])) css.firsttablecol <- ifelse(substring(CSS[['css.firsttablecol']], 1, 1) == '+', paste0(css.firsttablecol, substring(CSS[['css.firsttablecol']], 2)), CSS[['css.firsttablecol']])
  }
  # -------------------------------------
  # set style sheet
  # -------------------------------------
  page.style <- sprintf("<style>\n%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n%s { %s }\n.%s { %s }\n</style>",
                        tag.table, css.table, tag.thead, css.thead, tag.tdata, css.tdata,
                        tag.summary, css.summary, tag.arc, css.arc, tag.qrow, css.qrow,
                        tag.mdrow, css.mdrow, tag.abstand, css.abstand, tag.lasttablerow, css.lasttablerow,
                        tag.firsttablerow, css.firsttablerow, tag.leftalign, css.leftalign,
                        tag.centeralign, css.centeralign, tag.caption, css.caption,
                        tag.firsttablecol, css.firsttablecol)
  # start writing content
  toWrite <- paste(toWrite, page.style)
  toWrite <- paste(toWrite, "\n</head>\n<body>\n")
  # -------------------------------------
  # check if string vectors should be removed
  # -------------------------------------
  if (ignore.strings) {
    # ---------------------------------------
    # check if we have data frame with several variables
    # ---------------------------------------
    if (is.data.frame(data)) {
      # remove string variables
      data <- data[, !sapply(data, is.character)]
    } else if (is.character(data)) {
      stop("argument `data` is a single string vector, where string vectors should be removed. No data to compute frequency table left. See argument `ignore.strings` for details.", call. = FALSE)
    }
  }
  # -------------------------------------
  # Remove variables that only have missings
  # -------------------------------------
  if (is.data.frame(data)) {
    # store column indices of variables that only have NA's
    NAcolumns <- c()
    # iterate all columns
    for (i in seq_len(ncol(data))) {
      # check type
      if (length(stats::na.omit(data[[i]])) == 0) NAcolumns <- c(NAcolumns, i)
    }
    # check if any NA-only variables found
    if (length(NAcolumns) > 0) {
      message(sprintf("%i variables have been removed from output, because they contained only NA's: %s", 
                      length(NAcolumns), paste(colnames(data)[NAcolumns], collapse = "; ")))
      data <- data[, -NAcolumns]
    }
  }
  # -------------------------------------
  # auto-retrieve variable labels
  # -------------------------------------
  if (is.null(title)) {
    # check if we have data frame with several variables
    if (is.data.frame(data)) {
      # retrieve variable name attribute
      title <- sjmisc::get_label(data, def.value = colnames(data))
    # we have a single variable only
    } else {
      # retrieve variable name attribute
      title <- sjmisc::get_label(data, def.value = deparse(substitute(data)))
    }
  }
  # -------------------------------------
  # auto-retrieve variable notes
  # -------------------------------------
  # init variable notes as list
  note.labels <- list()
  # check if we have data frame with several variables
  if (is.data.frame(data)) {
    # if yes, iterate each variable
    for (i in seq_len(ncol(data))) {
      # retrieve note attribute
      note.labels <- c(note.labels, sjmisc::get_note(data[[i]]))
    }
    # we have a single variable only
  } else {
    # retrieve note attribute
    note.labels <- c(note.labels, sjmisc::get_note(data))
  }
  # -------------------------------------
  # make data frame of single variable, so we have
  # unique handling for the data
  # -------------------------------------
  if (!is.data.frame(data)) {
    isString <- is.character(data)
    # check for auto-detection of labels, but only for non-character-vectors
    # characters will be handled later
    if (is.null(value.labels) && !isString)
      value.labels <-
        sjmisc::get_labels(data, attr.only = F, include.values = NULL, 
                           include.non.labelled = T)
    # copy variable to data frame for unuqie handling
    data <- as.data.frame(data)
    if (isString) {
      # reformat into string, if it was...
      data$data <- as.character(data$data)
    }
  }
  # -------------------------------------
  # determine number of variables
  # -------------------------------------
  nvar <- ncol(data)
  # -------------------------------------
  # auto-group string vectors
  # -------------------------------------
  if (auto.grp.strings) {
    # iterate data frame
    for (i in seq_len(nvar)) {
      # get variable
      sv <- data[[i]]
      # check if character
      if (is.character(sv)) {
        # group strings
        data[[i]] <- 
          sjmisc::group_str(strings = sv, maxdist = max.string.dist, remove.empty = F)
      }
    }
  }
  # -------------------------------------
  # transform variable and value labels 
  # to list object
  # -------------------------------------
  if (!is.null(value.labels) && !is.list(value.labels)) {
    # if we have value labels as vector, convert them to list
    value.labels <- list(value.labels)
  } else if (is.null(value.labels)) {
    # create list
    value.labels <- list()
    # iterate all variables
    for (i in seq_len(nvar)) {
      # retrieve variable
      dummy <- data[[i]]
      # usually, value labels are NULL if we have string vector. if so
      # set value labels according to values
      if (is.character(dummy)) {
        value.labels <- c(value.labels, list(names(table(dummy))))
      } else {
        # check for auto-detection of labels
        avl <-
          sjmisc::get_labels(dummy, attr.only = F, include.values = NULL, 
                             include.non.labelled = T)
        if (!is.null(avl)) {
          value.labels <- c(value.labels, list(avl))
        } else {
          value.labels <- c(value.labels, 
                           list(min(dummy, na.rm = TRUE):max(dummy, na.rm = TRUE)))
        }
      }
      # and add label range to value labels list
      if (is.null(value.labels))
        value.labels <-
          c(value.labels, list(min(dummy, na.rm = TRUE):max(dummy, na.rm = TRUE)))
    }
  }
  # -------------------------------------
  # header row of table
  # -------------------------------------
  page.content.list <- list()
  headerRow <- sprintf("   <tr>\n     <th class=\"thead firsttablerow firsttablecol\">%s</th>\n     <th class=\"thead firsttablerow\">%s</th>\n     <th class=\"thead firsttablerow\">%s</th>\n     <th class=\"thead firsttablerow\">%s</th>\n     <th class=\"thead firsttablerow\">%s</th>\n   </tr>\n\n", string.val, string.cnt, string.prc, string.vprc, string.cprc)
  # -------------------------------------
  # start iterating all variables
  # -------------------------------------
  for (cnt in seq_len(nvar)) {
    # -----------------------------------------------
    # check for length of unique values and skip if too long
    # -----------------------------------------------
    if (!is.null(auto.group) && !is.character(data[[cnt]]) && length(unique(data[[cnt]])) >= auto.group) {
      message(sprintf("Variable %s with %i unique values was grouped...",
                      colnames(data)[cnt], length(unique(data[[cnt]]))))
      # group labels
      value.labels[[cnt]] <- 
        sjmisc::group_labels(sjmisc::to_value(data[[cnt]], keep.labels = F),
                             groupsize = "auto", groupcount = auto.group)
      # group variable
      data[[cnt]] <-
        sjmisc::group_var(sjmisc::to_value(data[[cnt]], keep.labels = F),
                          groupsize = "auto", as.num = TRUE, groupcount = auto.group)
      # set labels
      data[[cnt]] <- sjmisc::set_labels(data[[cnt]], value.labels[[cnt]])
    }
    # -----------------------------------------------
    # prepare data: create frequencies and weight them,
    # if requested. put data into a data frame
    #---------------------------------------------------
    # check if we have a string-vector
    if (is.character(data[[cnt]])) {
      # convert string to numeric
      orivar <- varia <- as.numeric(as.factor(data[[cnt]]))
      # here we have numeric or factor variables
    } else {
      # convert to numeric
      orivar <- varia <- sjmisc::to_value(data[[cnt]], keep.labels = F)
    }
    # retrieve summary
    varsummary <- summary(varia)
    # retrieve median
    var.median <- stats::median(varia, na.rm = TRUE)
    # retrieve quartiles
    var.lowerq <- round(varsummary[[2]])
    var.upperq <- round(varsummary[[5]])
    #---------------------------------------------------
    # create frequency data frame
    #---------------------------------------------------
    df.frq <- create.frq.df(data[[cnt]],
                            wrap.labels = Inf,
                            order.frq = sort.frq,
                            round.prz = digits,
                            na.rm = F, 
                            weight.by = weight.by)
    df <- df.frq$mydat
    #---------------------------------------------------
    # auto-set skipping zero-rows?
    #---------------------------------------------------
    if (!is.logical(o.skip.zero)) {
      # retrieve range of values
      vonbis <- max(varia, na.rm = T) - min(varia, na.rm = T)
      # retrieve count of unique values
      anzval <- stats::na.omit(unique(varia))
      # check proportion of possible values and actual values
      # if we have more than 25% of zero-values, or if we have
      # in general a large variable range, skip zero-rows.
      skip.zero <- ((100 * anzval / vonbis) < 75) || (anzval > 20)
    } else {
      skip.zero <- o.skip.zero
    }
    # save labels
    vallab <- df.frq$labels
    # rename "NA" row
    rownames(df)[nrow(df)] <- "NA"
    # -------------------------------------
    # start table tag
    # -------------------------------------
    page.content <- "<table>\n"
    # -------------------------------------
    # retrieve variable label
    # -------------------------------------
    varlab <- title[cnt]
    # if we have weighted values, say that in diagram's title
    if (!is.null(weight.by)) {
      varlab <- paste(varlab, title.wtd.suffix, sep = "")
    }
    # -------------------------------------
    # table caption, variable label
    # -------------------------------------
    if (!is.null(note.labels) && !sjmisc::is_empty(note.labels))
      caption.title.tag <- sprintf(" title=\"%s\"", note.labels[[cnt]])
    else
      caption.title.tag <- ""
    page.content <- paste(page.content, sprintf("  <caption%s>%s</caption>\n", caption.title.tag, varlab))
    # -------------------------------------
    # header row with column labels
    # -------------------------------------
    page.content <- paste0(page.content, headerRow)
    # iterate all labels, each one in one row
    for (j in 1:(nrow(df) - 1)) {
      # retrieve data row
      datarow <- df[j, ]
      zerorow <- (datarow$frq == 0)
      # -------------------------------------
      # check if to skip zero rows
      # -------------------------------------
      if (skip.zero && zerorow) {
        # nothing here...
      } else {
        # -------------------------------------
        # access cell data via "celldata <- c(datarow[XY])
        # we have 4 data cells (freq, perc, valid and cumperc)
        # -------------------------------------
        # write table data row
        # -------------------------------------
        # init default values
        rowstring <- ""
        # init default value for alternating colors
        if (altr.row.col) rowstring <- ifelse(sjmisc::is_even(j), " arc", "")
        rowcss <- rowstring
        # check whether we have median row and whether it should be highlighted
        if (emph.md && ((j + df.frq$minval) == (var.median + 1))) {
          rowcss <- sprintf(" mdrow%s", rowstring)
        }
        # check whether we have lower quartile and whether it should be highlighted
        else if (emph.quart) {
          if (((j + df.frq$minval) == (var.lowerq + 1)) || ((j + df.frq$minval) == (var.upperq + 1))) {
            rowcss <- sprintf(" qrow%s", rowstring)
          }
        }
        # value label
        page.content <- paste(page.content, 
                              sprintf("  <tr>\n     <td class=\"tdata leftalign firsttablecol%s\">%s</td>\n", 
                                      rowcss, vallab[j]))
        # cell values, first value is integer
        page.content <- paste(page.content, 
                              sprintf("    <td class=\"tdata centeralign%s\">%i</td>\n", 
                                      rowcss, as.integer(datarow$frq)))
        for (i in 8:10) {
          # following values are float
          page.content <- paste(page.content, 
                                sprintf("    <td class=\"tdata centeralign%s\">%.*f</td>\n", 
                                        rowcss, digits, datarow[i]))
        }
        # close row-tag
        page.content <- paste(page.content, "  </tr>\n", "\n")
      }
    }
    # -------------------------------------
    # write last table data row with NA
    # -------------------------------------
    # retrieve data row
    datarow <- df[nrow(df), ]
    # -------------------------------------
    # write table data row
    # -------------------------------------
    # value label
    page.content <- paste(page.content, sprintf("  <tr>\n     <td class=\"tdata leftalign lasttablerow firsttablecol\">%s</td>\n", string.na))
    # cell values, first value is integer
    page.content <- paste(page.content, sprintf("    <td class=\"tdata centeralign lasttablerow\">%i</td>\n", as.integer(datarow$frq)))
    # 2nd value is float. we don't need 3rd and 4th value as they are always 0 and 100
    page.content <- paste(page.content, sprintf("    <td class=\"tdata centeralign lasttablerow\">%.*f</td>\n     <td class=\"tdata lasttablerow\"></td>\n     <td class=\"tdata lasttablerow\"></td>\n", digits, datarow$raw.prc))
    # -------------------------------------
    # add info for mean, standard deviation
    # -------------------------------------
    if (show.summary) {
      # sum of frequencies is total N. Use these numbers
      # instead of "length(varia)", because weighted data
      # has different N
      vartot <- sum(df$frq, na.rm = T)
      # last element in df$frq is amount of missings,
      # so substract from total to get valid N
      varvalid <- vartot - df$frq[nrow(df)]
      if (is.null(weight.by)) {
        mw <- mean(orivar, na.rm = TRUE)
        sum_var <- orivar
      } else {
        mw <- stats::weighted.mean(orivar, weight.by, na.rm = TRUE)
        sum_var <- sjstats::weight(orivar, weight.by)
      }
      descr <- ""
      if (show.skew || show.kurtosis) {
        pstat <- psych::describe(data.frame(sum_var))
        if (show.skew) descr <- sprintf(" &middot; &gamma;=%.*f", digits, pstat$skew)
        if (show.kurtosis) descr <- sprintf("%s &middot; &omega;=%.*f", descr, 
                                            digits, pstat$kurtosis)
      }
      page.content <- paste(page.content, 
                            sprintf("  </tr>\n  <tr>\n    <td class=\"tdata summary\" colspan=\"5\">total N=%i &middot; valid N=%i &middot; x&#772;=%.*f &middot; &sigma;=%.*f%s</td>\n", 
                                    vartot, varvalid, digits, mw, digits,
                                    stats::sd(sum_var, na.rm = TRUE), descr))
    }
    # -------------------------------------
    # finish table
    # -------------------------------------
    page.content <- paste(page.content, "  </tr>\n </table>")
    # -------------------------------------
    # add table to return value list, so user can access each
    # single frequency table
    # -------------------------------------
    page.content.list[[length(page.content.list) + 1]] <- page.content
    toWrite <- paste(toWrite, page.content, "\n")
    # -------------------------------------
    # add separator in case we have more than one table
    # -------------------------------------
    if (nvar > 1) toWrite = paste(toWrite, "\n<p class=\"abstand\">&nbsp;</p>\n", "\n")
  }
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
  if (nvar > 1) {
    knitr <- c()
    for (i in 1:length(page.content.list)) {
      knitr <- paste0(knitr, page.content.list[[i]], 
                      sprintf("\n<p style=\"%s\">&nbsp;</p>\n", css.abstand))
    }
  } else {
    knitr <- page.content
  }
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
  knitr <- gsub(tag.firsttablerow, css.firsttablerow, knitr, fixed = TRUE, useBytes = TRUE)
  knitr <- gsub(tag.firsttablecol, css.firsttablecol, knitr, fixed = TRUE, useBytes = TRUE)
  knitr <- gsub(tag.leftalign, css.leftalign, knitr, fixed = TRUE, useBytes = TRUE)  
  knitr <- gsub(tag.centeralign, css.centeralign, knitr, fixed = TRUE, useBytes = TRUE)  
  knitr <- gsub(tag.lasttablerow, css.lasttablerow, knitr, fixed = TRUE, useBytes = TRUE)  
  knitr <- gsub(tag.summary, css.summary, knitr, fixed = TRUE, useBytes = TRUE)  
  knitr <- gsub(tag.arc, css.arc, knitr, fixed = TRUE, useBytes = TRUE)  
  knitr <- gsub(tag.qrow, css.qrow, knitr, fixed = TRUE, useBytes = TRUE)  
  knitr <- gsub(tag.mdrow, css.mdrow, knitr, fixed = TRUE, useBytes = TRUE)  
  knitr <- gsub(tag.abstand, css.abstand, knitr, fixed = TRUE, useBytes = TRUE)  
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
  invisible(structure(class = c("sjTable", "sjtfrq"),
                      list(page.style = page.style,
                           page.content.list = page.content.list,
                           output.complete = toWrite,
                           knitr = knitr)))
}
