#' @title Summary of stacked frequencies as HTML table
#' @name sjt.stackfrq
#' 
#' @seealso \itemize{
#'              \item \href{http://www.strengejacke.de/sjPlot/sjt.stackfrq}{sjPlot manual: sjt-basics}
#'              \item \code{\link{sjp.stackfrq}}
#'              \item \code{\link{sjp.likert}}
#'              }
#' 
#' @description Shows the results of stacked frequencies (such as likert scales) as HTML table.
#'                This function is useful when several items with identical scale/categories
#'                should be printed as table to compare their distributions (e.g.
#'                when plotting scales like SF, Barthel-Index, Quality-of-Life-scales etc.).
#'                
#' @param items \code{\link{data.frame}} with each column representing one (likert- or scale-)item.
#' @param weightBy weight factor that will be applied to weight all cases from \code{items}.
#'          Must be a vector of same length as \code{nrow(items)}. Default is \code{NULL}, so no weights are used.
#' @param title table caption.
#' @param varlabels character vector with variable names. If not specified, row names of \code{items}
#'          will be used, resp. variable labels will automatically be detected, when they have
#'          a variable label attribute (see \code{\link[sjmisc]{set_label}}) for details).
#' @param breakLabelsAt determines how many chars of the variable labels are displayed in 
#'          one line and when a line break is inserted. Default is 40.
#' @param valuelabels character vector with category/value labels, which
#'          appear in the header row.
#' @param breakValueLabelsAt determines how many chars of the value labels are displayed in 
#'          one line and when a line break is inserted. Default is 20.
#' @param sort.frq logical, indicates whether the \code{items} should be ordered by
#'          by highest count of first or last category of \code{items}.
#'          \itemize{
#'            \item Use \code{"first.asc"} to order ascending by lowest count of first category,
#'            \item \code{"first.desc"} to order descending by lowest count of first category,
#'            \item \code{"last.asc"} to order ascending by lowest count of last category,
#'            \item \code{"last.desc"} to order descending by lowest count of last category,
#'            \item or \code{NULL} (default) for no sorting.
#'          }
#' @param digits amount of digits for rounding the percentage values.
#'          Default is 2, i.e. percentage values have 2 digits after decimal point.
#' @param showN logical, if \code{TRUE}, each item's category N is printed in the table cells.
#' @param showTotalN logical, if \code{TRUE}, an additional column with each item's total N is printed.
#' @param showNA logical, if \code{TRUE}, \code{\link{NA}}'s (missing values) are also printed in the table.
#' @param labelNA The label for the missing column/row.
#' @param showSkew logical, if \code{TRUE}, an additional column with each item's skewness is printed.
#'          The skewness is retrieved from the \code{\link[psych]{describe}}-function 
#'          of the \pkg{psych}-package.
#' @param digits.stats amount of digits for rounding the skewness and kurtosis valuess.
#'          Default is 2, i.e. skewness and kurtosis values have 2 digits after decimal point.
#' @param skewString string, which is used as header for the skew column (see \code{showSkew})).
#'          Default is \code{"Skew"}.
#' @param kurtosisString string, which is used as header for the kurtosis column (see \code{showKurtosis})).
#'          Default is \code{"Kurtosis"}.
#'          
#' @inheritParams sjt.frq
#' @inheritParams sjt.df
#' @inheritParams sjt.itemanalysis
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
#' # -------------------------------
#' # random sample
#' # -------------------------------
#' # prepare data for 4-category likert scale, 5 items
#' likert_4 <- data.frame(as.factor(sample(1:4, 
#'                                         500, 
#'                                         replace = TRUE, 
#'                                         prob = c(0.2, 0.3, 0.1, 0.4))),
#'                        as.factor(sample(1:4, 
#'                                         500, 
#'                                         replace = TRUE, 
#'                                         prob = c(0.5, 0.25, 0.15, 0.1))),
#'                        as.factor(sample(1:4, 
#'                                         500, 
#'                                         replace = TRUE, 
#'                                         prob = c(0.25, 0.1, 0.4, 0.25))),
#'                        as.factor(sample(1:4, 
#'                                         500, 
#'                                         replace = TRUE, 
#'                                         prob = c(0.1, 0.4, 0.4, 0.1))),
#'                        as.factor(sample(1:4, 
#'                                         500, 
#'                                         replace = TRUE, 
#'                                         prob = c(0.35, 0.25, 0.15, 0.25))))
#' # create labels
#' levels_4 <- c("Independent", 
#'               "Slightly dependent", 
#'               "Dependent", 
#'               "Severely dependent")
#' 
#' # create item labels
#' items <- c("Q1", "Q2", "Q3", "Q4", "Q5")
#' 
#' # plot stacked frequencies of 5 (ordered) item-scales
#' \dontrun{
#' sjt.stackfrq(likert_4, valuelabels = levels_4, varlabels = items)
#' 
#' 
#' # -------------------------------
#' # Data from the EUROFAMCARE sample dataset
#' # Auto-detection of labels
#' # -------------------------------
#' library(sjmisc)
#' data(efc)
#' # recveive first item of COPE-index scale
#' start <- which(colnames(efc) == "c82cop1")
#' # recveive first item of COPE-index scale
#' end <- which(colnames(efc) == "c90cop9")
#' 
#' sjt.stackfrq(efc[, c(start:end)],
#'              alternateRowColors = TRUE)
#' 
#' sjt.stackfrq(efc[, c(start:end)],
#'              alternateRowColors = TRUE,
#'              showN = TRUE,
#'              showNA = TRUE)
#'          
#'          
#' # -------------------------------- 
#' # User defined style sheet
#' # -------------------------------- 
#' sjt.stackfrq(efc[, c(start:end)],
#'              alternateRowColors = TRUE,
#'              showTotalN = TRUE,
#'              showSkew = TRUE,
#'              showKurtosis = TRUE,
#'              CSS = list(css.ncol = "border-left:1px dotted black;",
#'                         css.summary = "font-style:italic;"))}
#'              
#' @importFrom psych describe
#' @importFrom stats xtabs
#' @export
sjt.stackfrq <- function(items,
                         weightBy = NULL,
                         title = NULL,
                         varlabels = NULL,
                         breakLabelsAt = 40,
                         valuelabels = NULL,
                         breakValueLabelsAt = 20,
                         sort.frq = NULL,
                         alternateRowColors = FALSE,
                         digits = 2,
                         showN = FALSE,
                         showTotalN = FALSE,
                         showNA = FALSE,
                         labelNA = "NA",
                         showSkew = FALSE,
                         showKurtosis = FALSE,
                         digits.stats = 2,
                         skewString = "Skew",
                         kurtosisString = "Kurtosis",
                         file = NULL, 
                         encoding = NULL,
                         CSS = NULL,
                         useViewer = TRUE,
                         no.output = FALSE,
                         remove.spaces = TRUE) {
  # --------------------------------------------------------
  # check sorting
  # --------------------------------------------------------
  if (!is.null(sort.frq)) {
    if (sort.frq == "first.asc") {
      sort.frq  <- "first"
      reverseOrder <- FALSE
    } else if (sort.frq == "first.desc") {
      sort.frq  <- "first"
      reverseOrder <- TRUE
    } else if (sort.frq == "last.asc") {
      sort.frq  <- "last"
      reverseOrder <- FALSE
    } else if (sort.frq == "last.desc") {
      sort.frq  <- "last"
      reverseOrder <- TRUE
    } else {
      sort.frq  <- NULL
      reverseOrder <- FALSE
    }
  } else {
    reverseOrder <- FALSE
  }
  # --------------------------------------------------------
  # check encoding
  # --------------------------------------------------------
  encoding <- get.encoding(encoding, items)
  # --------------------------------------------------------
  # try to automatically set labels is not passed as parameter
  # --------------------------------------------------------
  if (is.null(valuelabels)) valuelabels <- sjmisc::get_labels(items[[1]],
                                                              attr.only = F,
                                                              include.values = NULL,
                                                              include.non.labelled = T)
  if (is.null(varlabels)) {
    varlabels <- c()
    # if yes, iterate each variable
    for (i in 1:ncol(items)) {
      # retrieve variable name attribute
      varlabels <- c(varlabels, sjmisc::get_label(items[[i]], def.value = colnames(items)[i]))
    }
  }
  # ----------------------------
  # retrieve min and max values
  # ----------------------------
  minval <- as.numeric(min(apply(items, 2, function(x) min(x, na.rm = TRUE))))
  maxval <- as.numeric(max(apply(items, 2, function(x) max(x, na.rm = TRUE))))
  # ----------------------------
  # if we have no value labels, set default labels and find amount
  # of unique categories
  # ----------------------------
  if (is.null(valuelabels)) valuelabels <- as.character(minval:maxval)
  # check whether missings should be shown
  if (showNA) valuelabels <- c(valuelabels, labelNA)
  # save amolunt of values
  catcount <- length(valuelabels)
  # check length of x-axis-labels and split longer strings at into new lines
  valuelabels <- sjmisc::word_wrap(valuelabels, breakValueLabelsAt, "<br>")
  # ----------------------------
  # if we have no variable labels, use row names
  # ----------------------------
  if (is.null(varlabels)) varlabels <- colnames(items)
  # check length of x-axis-labels and split longer strings at into new lines
  varlabels <- sjmisc::word_wrap(varlabels, breakLabelsAt, "<br>")
  # ----------------------------  
  # additional statistics required from psych-package?
  # ----------------------------
  if (showSkew || showKurtosis) pstat <- psych::describe(items)
  # ----------------------------
  # create data frame with each item in a row
  # therefore, iterate each item
  # ----------------------------
  # save counts for each items
  itemcount <- c()
  mat <- data.frame()
  mat.n <- data.frame()
  # ----------------------------
  # determine minimum value. if 0, add one, because
  # vector indexing starts with 1
  # ----------------------------
  if (any(apply(items, c(1, 2), is.factor)) || 
      any(apply(items, c(1, 2), is.character))) {
    diff <- ifelse(min(apply(items, c(1, 2), as.numeric), na.rm = TRUE) == 0, 1, 0)
  } else {
    diff <- ifelse(min(items, na.rm = TRUE) == 0, 1, 0)
  }
  # iterate item-list
  for (i in 1:ncol(items)) {
    # ----------------------------
    # if we don't have weights, create simple frequency table
    # of each item
    # ----------------------------
    if (showNA) {
      # ----------------------------
      # include missing
      # ----------------------------
      if (is.null(weightBy)) {
        dummy <- table(addNA(items[[i]]))
      } else {
        # else weight with xtabs
        dummy <- round(stats::xtabs(weightBy ~ addNA(items[[i]])), 0)
      }
    # ----------------------------
    # exclude missing
    # ----------------------------
    } else {
      if (is.null(weightBy)) {
        dummy <- table(items[[i]])
      } else {
        # else weight with xtabs
        dummy <- round(stats::xtabs(weightBy ~ items[[i]]), 0)
      }
    }
    # ----------------------------
    # save n
    # ----------------------------
    itemcount <- c(itemcount, sum(dummy))
    # ----------------------------
    # create frequency var, filled with zeros
    # need this to fill categories with zero counts
    # ----------------------------
    fr <- rep(0, catcount)
    # ----------------------------
    # if we have missings, manually change table names
    # ----------------------------
    if (showNA) {
      # retrieve amount of categories
      tl <- length(names(dummy))
      # retrieve maximum category value, omitting NA
      maxtl <- max(as.numeric(stats::na.omit(names(dummy))))
      # set NA table name to max-value+1, so we have continuous
      # vector-index (needed below)
      names(dummy)[tl] <- maxtl + 1
    }
    # ----------------------------
    # table name equals cateogory value,
    # table itself contains counts of each category
    # ----------------------------
    fr[as.numeric(names(dummy)) + diff] <- dummy
    # ----------------------------
    # add proportional percentages to data frame row
    # ----------------------------
    mat <- rbind(mat, round(prop.table(fr), 4))
    mat.n <- rbind(mat.n, fr)
  }
  # ----------------------------
  # Check if ordering was requested
  # ----------------------------
  # default order
  facord <- c(1:nrow(mat))
  if (!is.null(sort.frq)) {
    # ----------------------------
    # order by first cat
    # ----------------------------
    if (sort.frq == "first") {
      facord <- order(mat[, 1])
    # ----------------------------
    # order by last cat
    # ----------------------------
    } else {
      facord <- order(mat[, ncol(mat)])
    }
  }
  # ----------------------------
  # reverse order
  # ----------------------------
  if (reverseOrder) facord <- rev(facord)
  # -------------------------------------
  # init header
  # -------------------------------------
  toWrite <- sprintf("<html>\n<head>\n<meta http-equiv=\"Content-type\" content=\"text/html;charset=%s\">\n", encoding)
  # -------------------------------------
  # init style sheet and tags used for css-definitions
  # we can use these variables for string-replacement
  # later for return value
  # -------------------------------------
  tag.table <- "table"
  tag.caption <- "caption"
  tag.thead <- "thead"
  tag.tdata <- "tdata"
  tag.arc <- "arc"
  tag.centeralign <- "centeralign"
  tag.firsttablecol <- "firsttablecol"
  tag.ncol <- "ncol"
  tag.skewcol <- "skewcol"
  tag.kurtcol <- "kurtcol"
  tag.summary <- "summary"
  css.table <- "border-collapse:collapse; border:none; border-bottom:double black;"
  css.caption <- "font-weight: bold; text-align:left;"
  css.thead <- "border-top:double black; border-bottom:1px solid black; padding:0.2cm;"
  css.tdata <- "padding:0.2cm;"
  css.arc <- "background-color:#eaeaea;"
  css.centeralign <- "text-align:center;"
  css.firsttablecol <- "font-style:italic;"
  css.ncol <- ""
  css.summary <- ""
  css.skewcol <- ""
  css.kurtcol <- ""
  # ------------------------
  # check user defined style sheets
  # ------------------------
  if (!is.null(CSS)) {
    if (!is.null(CSS[['css.table']])) css.table <- ifelse(substring(CSS[['css.table']],1,1)=='+', paste0(css.table, substring(CSS[['css.table']],2)), CSS[['css.table']])
    if (!is.null(CSS[['css.thead']])) css.thead <- ifelse(substring(CSS[['css.thead']],1,1)=='+', paste0(css.thead, substring(CSS[['css.thead']],2)), CSS[['css.thead']])
    if (!is.null(CSS[['css.caption']])) css.caption <- ifelse(substring(CSS[['css.caption']],1,1)=='+', paste0(css.caption, substring(CSS[['css.caption']],2)), CSS[['css.caption']])
    if (!is.null(CSS[['css.summary']])) css.summary <- ifelse(substring(CSS[['css.summary']],1,1)=='+', paste0(css.summary, substring(CSS[['css.summary']],2)), CSS[['css.summary']])
    if (!is.null(CSS[['css.arc']])) css.arc <- ifelse(substring(CSS[['css.arc']],1,1)=='+', paste0(css.arc, substring(CSS[['css.arc']],2)), CSS[['css.arc']])
    if (!is.null(CSS[['css.tdata']])) css.tdata <- ifelse(substring(CSS[['css.tdata']],1,1)=='+', paste0(css.tdata, substring(CSS[['css.tdata']],2)), CSS[['css.tdata']])
    if (!is.null(CSS[['css.centeralign']])) css.centeralign <- ifelse(substring(CSS[['css.centeralign']],1,1)=='+', paste0(css.centeralign, substring(CSS[['css.centeralign']],2)), CSS[['css.centeralign']])
    if (!is.null(CSS[['css.firsttablecol']])) css.firsttablecol <- ifelse(substring(CSS[['css.firsttablecol']],1,1)=='+', paste0(css.firsttablecol, substring(CSS[['css.firsttablecol']],2)), CSS[['css.firsttablecol']])
    if (!is.null(CSS[['css.ncol']])) css.ncol <- ifelse(substring(CSS[['css.ncol']],1,1)=='+', paste0(css.ncol, substring(CSS[['css.ncol']],2)), CSS[['css.ncol']])
    if (!is.null(CSS[['css.skewcol']])) css.skewcol <- ifelse(substring(CSS[['css.skewcol']],1,1)=='+', paste0(css.skewcol, substring(CSS[['css.skewcol']],2)), CSS[['css.skewcol']])
    if (!is.null(CSS[['css.kurtcol']])) css.kurtcol <- ifelse(substring(CSS[['css.kurtcol']],1,1)=='+', paste0(css.kurtcol, substring(CSS[['css.kurtcol']],2)), CSS[['css.kurtcol']])
  }
  # ------------------------
  # set page style
  # ------------------------
  page.style <-  sprintf("<style>%s { %s }\n%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n</style>",
                         tag.table, css.table, tag.caption, css.caption,
                         tag.thead, css.thead, tag.tdata, css.tdata,
                         tag.firsttablecol, css.firsttablecol, tag.arc, css.arc,
                         tag.centeralign, css.centeralign, tag.ncol, css.ncol,
                         tag.summary, css.summary, tag.kurtcol, css.kurtcol,
                         tag.skewcol, css.skewcol)
  # ------------------------
  # start content
  # ------------------------
  toWrite <- paste0(toWrite, page.style)
  toWrite = paste(toWrite, "\n</head>\n<body>", "\n")
  # -------------------------------------
  # start table tag
  # -------------------------------------
  page.content <- "<table>\n"
  # -------------------------------------
  # table caption
  # -------------------------------------
  if (!is.null(title)) page.content <- paste(page.content, sprintf("  <caption>%s</caption>\n", title))
  # -------------------------------------
  # header row
  # -------------------------------------
  # write tr-tag
  page.content <- paste0(page.content, "  <tr>\n")
  # first column
  page.content <- paste0(page.content, "    <th class=\"thead\">&nbsp;</th>\n")
  # iterate columns
  for (i in 1:catcount) {
    page.content <- paste0(page.content, sprintf("    <th class=\"thead\">%s</th>\n", valuelabels[i]))
  }
  # add N column
  if (showTotalN) page.content <- paste0(page.content, "    <th class=\"thead ncol summary\">N</th>\n")
  # add skew column
  if (showSkew) page.content <- paste0(page.content, sprintf("    <th class=\"thead skewcol summary\">%s</th>\n", skewString))
  # add kurtosis column
  if (showKurtosis) page.content <- paste0(page.content, sprintf("    <th class=\"thead kurtcol summary\">%s</th>\n", kurtosisString))
  # close table row
  page.content <- paste0(page.content, "  </tr>\n")
  # -------------------------------------
  # data rows
  # -------------------------------------
  # iterate all rows of df
  for (i in 1:nrow(mat)) {
    # default row string for alternative row colors
    arcstring <- ""
    # if we have alternating row colors, set css
    if (alternateRowColors) arcstring <- ifelse(sjmisc::is_even(i), " arc", "")
    # write tr-tag
    page.content <- paste0(page.content, "  <tr>\n")
    # print first table cell
    page.content <- paste0(page.content, sprintf("    <td class=\"firsttablecol%s\">%s</td>\n", arcstring, varlabels[facord[i]]))
    # --------------------------------------------------------
    # iterate all columns
    # --------------------------------------------------------
    for (j in 1:ncol(mat)) {
      if (showN) {
        page.content <- paste0(page.content, sprintf("    <td class=\"tdata centeralign%s\">%i<br>(%.*f&nbsp;%%)</td>\n", arcstring, mat.n[facord[i], j], digits, 100 * mat[facord[i], j]))
      } else {
        page.content <- paste0(page.content, sprintf("    <td class=\"tdata centeralign%s\">%.*f&nbsp;%%</td>\n", arcstring, digits, 100 * mat[facord[i], j]))
      }
    }
    # add column with N's
    if (showTotalN) page.content <- paste0(page.content, sprintf("    <td class=\"tdata centeralign ncol summary%s\">%i</td>\n", arcstring, itemcount[facord[i]]))
    # add column with Skew's
    if (showSkew) page.content <- paste0(page.content, sprintf("    <td class=\"tdata centeralign skewcol summary%s\">%.*f</td>\n", arcstring, digits.stats, pstat$skew[facord[i]]))
    # add column with Kurtosis's
    if (showKurtosis) page.content <- paste0(page.content, sprintf("    <td class=\"tdata centeralign kurtcol summary%s\">%.*f</td>\n", arcstring, digits.stats, pstat$kurtosis[facord[i]]))
    # close row
    page.content <- paste0(page.content, "  </tr>\n")
  }
  # -------------------------------------
  # finish table
  # -------------------------------------
  page.content <- paste(page.content, "\n</table>")
  # -------------------------------------
  # finish html page
  # -------------------------------------
  toWrite <- paste(toWrite, page.content, "\n")
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
  knitr <- gsub("<caption", sprintf("<caption style=\"%s\"", css.caption), knitr, fixed = TRUE, useBytes = TRUE)
  # -------------------------------------
  # replace class-attributes with inline-style-definitions
  # -------------------------------------
  knitr <- gsub(tag.tdata, css.tdata, knitr, fixed = TRUE, useBytes = TRUE)
  knitr <- gsub(tag.thead, css.thead, knitr, fixed = TRUE, useBytes = TRUE)
  knitr <- gsub(tag.centeralign, css.centeralign, knitr, fixed = TRUE, useBytes = TRUE)
  knitr <- gsub(tag.firsttablecol, css.firsttablecol, knitr, fixed = TRUE, useBytes = TRUE)  
  knitr <- gsub(tag.ncol, css.ncol, knitr, fixed = TRUE, useBytes = TRUE)  
  knitr <- gsub(tag.skewcol, css.skewcol, knitr, fixed = TRUE, useBytes = TRUE)  
  knitr <- gsub(tag.kurtcol, css.kurtcol, knitr, fixed = TRUE, useBytes = TRUE)  
  knitr <- gsub(tag.summary, css.summary, knitr, fixed = TRUE, useBytes = TRUE)  
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
  invisible (structure(class = "sjtstackfrq",
                       list(page.style = page.style,
                            page.content = page.content,
                            output.complete = toWrite,
                            knitr = knitr)))
}
