#' @title Summary of stacked frequencies as HTML table
#' @name tab_stackfrq
#'
#' @description Shows the results of stacked frequencies (such as likert scales) as HTML table.
#'                This function is useful when several items with identical scale/categories
#'                should be printed as table to compare their distributions (e.g.
#'                when plotting scales like SF, Barthel-Index, Quality-of-Life-scales etc.).
#'
#' @param sort.frq logical, indicates whether the \code{items} should be ordered by
#'          by highest count of first or last category of \code{items}.
#'          \itemize{
#'            \item Use \code{"first.asc"} to order ascending by lowest count of first category,
#'            \item \code{"first.desc"} to order descending by lowest count of first category,
#'            \item \code{"last.asc"} to order ascending by lowest count of last category,
#'            \item \code{"last.desc"} to order descending by lowest count of last category,
#'            \item or \code{NULL} (default) for no sorting.
#'          }
#' @param show.total logical, if \code{TRUE}, an additional column with each
#'          item's total N is printed.
#' @param string.total label for the total N column.
#' @param string.na label for the missing column/row.
#' @param show.skew logical, if \code{TRUE}, an additional column with each item's skewness is printed.
#'          The skewness is retrieved from the \code{\link[psych]{describe}}-function
#'          of the \pkg{psych}-package.
#' @param digits.stats amount of digits for rounding the skewness and kurtosis valuess.
#'          Default is 2, i.e. skewness and kurtosis values have 2 digits after decimal point.
#'
#' @inheritParams tab_df
#' @inheritParams tab_itemscale
#' @inheritParams tab_xtab
#' @inheritParams plot_grpfrq
#' @inheritParams plot_stackfrq
#'
#' @return Invisibly returns
#'          \itemize{
#'            \item the web page style sheet (\code{page.style}),
#'            \item the web page content (\code{page.content}),
#'            \item the complete html-output (\code{page.complete}) and
#'            \item the html-table with inline-css for use with knitr (\code{knitr})
#'            }
#'            for further use.
#'
#' @examples
#' # -------------------------------
#' # random sample
#' # -------------------------------
#' # prepare data for 4-category likert scale, 5 items
#' likert_4 <- data.frame(
#'   as.factor(sample(1:4, 500, replace = TRUE, prob = c(0.2, 0.3, 0.1, 0.4))),
#'   as.factor(sample(1:4, 500, replace = TRUE, prob = c(0.5, 0.25, 0.15, 0.1))),
#'   as.factor(sample(1:4, 500, replace = TRUE, prob = c(0.25, 0.1, 0.4, 0.25))),
#'   as.factor(sample(1:4, 500, replace = TRUE, prob = c(0.1, 0.4, 0.4, 0.1))),
#'   as.factor(sample(1:4, 500, replace = TRUE, prob = c(0.35, 0.25, 0.15, 0.25)))
#' )
#'
#' # create labels
#' levels_4 <- c("Independent", "Slightly dependent",
#'               "Dependent", "Severely dependent")
#'
#' # create item labels
#' items <- c("Q1", "Q2", "Q3", "Q4", "Q5")
#'
#' # plot stacked frequencies of 5 (ordered) item-scales
#' \dontrun{
#' if (interactive()) {
#'   tab_stackfrq(likert_4, value.labels = levels_4, var.labels = items)
#'
#'   # -------------------------------
#'   # Data from the EUROFAMCARE sample dataset
#'   #  Auto-detection of labels
#'   # -------------------------------
#'   data(efc)
#'   # recveive first item of COPE-index scale
#'   start <- which(colnames(efc) == "c82cop1")
#'   # recveive first item of COPE-index scale
#'   end <- which(colnames(efc) == "c90cop9")
#'
#'   tab_stackfrq(efc[, c(start:end)], alternate.rows = TRUE)
#'
#'   tab_stackfrq(efc[, c(start:end)], alternate.rows = TRUE,
#'                show.n = TRUE, show.na = TRUE)
#'
#'   # --------------------------------
#'   # User defined style sheet
#'   # --------------------------------
#'   tab_stackfrq(efc[, c(start:end)], alternate.rows = TRUE,
#'                show.total = TRUE, show.skew = TRUE, show.kurtosis = TRUE,
#'                CSS = list(css.ncol = "border-left:1px dotted black;",
#'                           css.summary = "font-style:italic;"))
#' }
#' }
#' @export
tab_stackfrq <- function(items,
                         weight.by = NULL,
                         title = NULL,
                         var.labels = NULL,
                         value.labels = NULL,
                         wrap.labels = 20,
                         sort.frq = NULL,
                         alternate.rows = FALSE,
                         digits = 2,
                         string.total = "N",
                         string.na = "NA",
                         show.n = FALSE,
                         show.total = FALSE,
                         show.na = FALSE,
                         show.skew = FALSE,
                         show.kurtosis = FALSE,
                         digits.stats = 2,
                         file = NULL,
                         encoding = NULL,
                         CSS = NULL,
                         use.viewer = TRUE,
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
  if (is.null(value.labels)) {
    value.labels <- sjlabelled::get_labels(
      items[[1]],
      attr.only = F,
      values = "n",
      non.labelled = T
    )
  }

  if (is.null(var.labels)) {
    var.labels <- sjlabelled::get_label(items, def.value = colnames(items))
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
  if (is.null(value.labels)) value.labels <- as.character(minval:maxval)
  # check whether missings should be shown
  if (show.na) value.labels <- c(value.labels, `NA` = string.na)
  # save amolunt of values
  catcount <- length(value.labels)
  # check length of x-axis-labels and split longer strings at into new lines
  value.labels <- sjmisc::word_wrap(value.labels, wrap.labels, "<br>")
  # ----------------------------
  # if we have no variable labels, use row names
  # ----------------------------
  if (is.null(var.labels)) var.labels <- colnames(items)
  # check length of x-axis-labels and split longer strings at into new lines
  var.labels <- sjmisc::word_wrap(var.labels, wrap.labels, "<br>")
  # ----------------------------
  # additional statistics required from psych-package?
  # ----------------------------
  if (show.skew) pstat_skewness <- datawizard::skewness(items)
  if (show.kurtosis) pstat_kurtosis <- datawizard::kurtosis(items)
  if (is.null(weight.by)) {
    dummy <- sjmisc::frq(items, show.strings = TRUE, show.na = show.na)
  } else {
    items$weights <- weight.by
    dummy <- sjmisc::frq(items, weights = items$weights, show.strings = TRUE, show.na = show.na)
  }
  mat.n <- .transform_data(dummy, col = "frq")
  mat <- .transform_data(dummy, col = ifelse(isTRUE(show.na), "raw.prc", "valid.prc"))

  # ----------------------------
  # Check if ordering was requested
  # ----------------------------
  # default order
  facord <- seq_len(nrow(mat))
  if (!is.null(sort.frq)) {
    if (sort.frq == "first")
      facord <- order(mat.n$V1)
    else
      facord <- order(mat.n[, ncol(mat.n)])
  }

  # ----------------------------
  # reverse order
  # ----------------------------
  if (reverseOrder) facord <- rev(facord)
  # -------------------------------------
  # init header
  # -------------------------------------
  toWrite <- table.header <- sprintf("<html>\n<head>\n<meta http-equiv=\"Content-type\" content=\"text/html;charset=%s\">\n", encoding)
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
    if (!is.null(CSS[['css.table']])) css.table <- ifelse(substring(CSS[['css.table']],1,1) == '+', paste0(css.table, substring(CSS[['css.table']],2)), CSS[['css.table']])
    if (!is.null(CSS[['css.thead']])) css.thead <- ifelse(substring(CSS[['css.thead']],1,1) == '+', paste0(css.thead, substring(CSS[['css.thead']],2)), CSS[['css.thead']])
    if (!is.null(CSS[['css.caption']])) css.caption <- ifelse(substring(CSS[['css.caption']],1,1) == '+', paste0(css.caption, substring(CSS[['css.caption']],2)), CSS[['css.caption']])
    if (!is.null(CSS[['css.summary']])) css.summary <- ifelse(substring(CSS[['css.summary']],1,1) == '+', paste0(css.summary, substring(CSS[['css.summary']],2)), CSS[['css.summary']])
    if (!is.null(CSS[['css.arc']])) css.arc <- ifelse(substring(CSS[['css.arc']],1,1) == '+', paste0(css.arc, substring(CSS[['css.arc']],2)), CSS[['css.arc']])
    if (!is.null(CSS[['css.tdata']])) css.tdata <- ifelse(substring(CSS[['css.tdata']],1,1) == '+', paste0(css.tdata, substring(CSS[['css.tdata']],2)), CSS[['css.tdata']])
    if (!is.null(CSS[['css.centeralign']])) css.centeralign <- ifelse(substring(CSS[['css.centeralign']],1,1) == '+', paste0(css.centeralign, substring(CSS[['css.centeralign']],2)), CSS[['css.centeralign']])
    if (!is.null(CSS[['css.firsttablecol']])) css.firsttablecol <- ifelse(substring(CSS[['css.firsttablecol']],1,1) == '+', paste0(css.firsttablecol, substring(CSS[['css.firsttablecol']],2)), CSS[['css.firsttablecol']])
    if (!is.null(CSS[['css.ncol']])) css.ncol <- ifelse(substring(CSS[['css.ncol']],1,1) == '+', paste0(css.ncol, substring(CSS[['css.ncol']],2)), CSS[['css.ncol']])
    if (!is.null(CSS[['css.skewcol']])) css.skewcol <- ifelse(substring(CSS[['css.skewcol']],1,1) == '+', paste0(css.skewcol, substring(CSS[['css.skewcol']],2)), CSS[['css.skewcol']])
    if (!is.null(CSS[['css.kurtcol']])) css.kurtcol <- ifelse(substring(CSS[['css.kurtcol']],1,1) == '+', paste0(css.kurtcol, substring(CSS[['css.kurtcol']],2)), CSS[['css.kurtcol']])
  }
  # ------------------------
  # set page style
  # ------------------------
  page.style <-  sprintf("<style>\nhtml, body { background-color: white; }\n%s { %s }\n%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n</style>",
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
    page.content <- paste0(page.content, sprintf("    <th class=\"thead\">%s</th>\n", value.labels[i]))
  }
  # add N column
  if (show.total) page.content <- paste0(page.content, sprintf("    <th class=\"thead ncol summary\">%s</th>\n", string.total))
  # add skew column
  if (show.skew) page.content <- paste0(page.content, "    <th class=\"thead skewcol summary\">Skew</th>\n")
  # add kurtosis column
  if (show.kurtosis) page.content <- paste0(page.content, "    <th class=\"thead kurtcol summary\">Kurtosis</th>\n")
  # close table row
  page.content <- paste0(page.content, "  </tr>\n")
  # -------------------------------------
  # data rows
  # -------------------------------------
  # iterate all rows of df
  for (i in seq_len(nrow(mat))) {
    # default row string for alternative row colors
    arcstring <- ""
    # if we have alternating row colors, set css
    if (alternate.rows) arcstring <- ifelse(sjmisc::is_even(i), " arc", "")
    # write tr-tag
    page.content <- paste0(page.content, "  <tr>\n")
    # print first table cell
    page.content <- paste0(page.content, sprintf("    <td class=\"firsttablecol%s\">%s</td>\n", arcstring, var.labels[facord[i]]))
    # --------------------------------------------------------
    # iterate all columns
    # --------------------------------------------------------
    for (j in seq_len(ncol(mat))) {
      if (show.n) {
        page.content <- paste0(page.content, sprintf("    <td class=\"tdata centeralign%s\">%i<br>(%.*f&nbsp;%%)</td>\n", arcstring, as.integer(mat.n[facord[i], j]), digits, mat[facord[i], j]))
      } else {
        page.content <- paste0(page.content, sprintf("    <td class=\"tdata centeralign%s\">%.*f&nbsp;%%</td>\n", arcstring, digits, mat[facord[i], j]))
      }
    }
    # add column with N's
    if (show.total) page.content <- paste0(page.content, sprintf("    <td class=\"tdata centeralign ncol summary%s\">%i</td>\n", arcstring, as.integer(sum(mat.n[facord[i], ]))))
    # add column with Skew's
    if (show.skew) page.content <- paste0(page.content, sprintf("    <td class=\"tdata centeralign skewcol summary%s\">%.*f</td>\n", arcstring, digits.stats, pstat_skewness[facord[i]]))
    # add column with Kurtosis's
    if (show.kurtosis) page.content <- paste0(page.content, sprintf("    <td class=\"tdata centeralign kurtcol summary%s\">%.*f</td>\n", arcstring, digits.stats, pstat_kurtosis[facord[i]]))
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
  # return results
  # -------------------------------------
  structure(class = c("sjTable", "sjtstackfrq"),
                      list(page.style = page.style,
                           page.content = page.content,
                           page.complete = toWrite,
                           header = table.header,
                           knitr = knitr,
                           file = file,
                           viewer = use.viewer))
}



.transform_data <- function(x, col) {
  dat <- suppressWarnings(Reduce(function(x, y) merge(x, y, all = TRUE, sort = FALSE, by = "val"), x))
  if (is.factor(dat$val)) {
    reihe <- levels(dat$val)
    if (anyNA(dat$val))
      reihe <- c(reihe, NA)
    dat <- dat[order(dat$val, reihe), ]
  } else {
    dat <- dat[order(dat$val), ]
  }
  colnames(dat) <- make.names(colnames(dat), unique = TRUE)
  keep <- (colnames(dat) == "val") | grepl(paste0("^", col), colnames(dat))
  dat <- as.data.frame(t(dat[, keep, drop = FALSE]))
  dat <- as.data.frame(sapply(dat[-1, ], function(i) as.numeric(as.character(i))))
  dat[is.na(dat)] <- 0
  colnames(dat) <- sprintf("V%i", 1:ncol(dat))
  dat
}
