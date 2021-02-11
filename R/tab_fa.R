#' @title Summary of factor analysis as HTML table
#' @name tab_fa
#'
#' @description Performes a factor analysis on a data frame or matrix
#'                and displays the factors as HTML
#'                table, or saves them as file. \cr \cr In case a data frame is used as
#'                parameter, the Cronbach's Alpha value for each factor scale will be calculated,
#'                i.e. all variables with the highest loading for a factor are taken for the
#'                reliability test. The result is an alpha value for each factor dimension.
#'
#' @param show.comm Logical, if \code{TRUE}, show the communality column in the table.
#' @param method the factoring method to be used. \code{"ml"} will do a maximum likelihood factor analysis (default).
#'         \code{"minres"} will do a minimum residual (OLS),
#'         \code{"wls"} will do a weighted least squares (WLS) solution,
#'         \code{"gls"} does a generalized weighted least squares (GLS),
#'         \code{"pa"} will do the principal factor solution,
#'         \code{"minchi"} will minimize the sample size weighted chi square
#'         when treating pairwise correlations with different number of
#'         subjects per pair. \code{"minrank"} will do a minimum rank factor analysis.
#' @param sort logical, if \code{TRUE}, sort the loadings for each factors
#'   (items will be sorted in terms of their greatest loading, in descending
#'   order)
#'
#' @inheritParams tab_pca
#' @inheritParams tab_model
#' @inheritParams tab_df
#' @inheritParams tab_xtab
#' @inheritParams plot_grpfrq
#' @inheritParams tab_corr
#'
#' @return Invisibly returns
#'          \itemize{
#'            \item the web page style sheet (\code{page.style}),
#'            \item the web page content (\code{page.content}),
#'            \item the complete html-output (\code{page.complete}),
#'            \item the html-table with inline-css for use with knitr (\code{knitr}),
#'            \item the \code{factor.index}, i.e. the column index of each variable with the highest factor loading for each factor and
#'            \item the \code{removed.items}, i.e. which variables have been removed because they were outside of the \code{fctr.load.tlrn}'s range.
#'            }
#'            for further use.
#'
#' @note This method for factor analysis relies on the functions
#'       \code{\link[psych]{fa}} and \code{\link[psych]{fa.parallel}} from the psych package.
#'
#'
#' @examples
#' \dontrun{
#' # Data from the EUROFAMCARE sample dataset
#' library(sjmisc)
#' library(GPArotation)
#' data(efc)
#'
#' # recveive first item of COPE-index scale
#' start <- which(colnames(efc) == "c82cop1")
#' # recveive last item of COPE-index scale
#' end <- which(colnames(efc) == "c90cop9")
#' # auto-detection of labels
#' if (interactive()) {
#'   tab_fa(efc[, start:end])
#' }}
#' @export
tab_fa <- function(data,
                   rotation = c("promax", "varimax"),
                   method = c("ml", "minres", "wls", "gls", "pa", "minchi", "minrank"),
                   nmbr.fctr = NULL,
                   fctr.load.tlrn = 0.1,
                   sort = FALSE,
                   title = "Factor Analysis",
                   var.labels = NULL,
                   wrap.labels = 40,
                   show.cronb = TRUE,
                   show.comm = FALSE,
                   alternate.rows = FALSE,
                   digits = 2,
                   CSS = NULL,
                   encoding = NULL,
                   file = NULL,
                   use.viewer = TRUE,
                   remove.spaces = TRUE) {
  # -------------------------------------
  # check encoding
  # -------------------------------------
  encoding <- get.encoding(encoding, data)

  # check arguments
  rotation <- match.arg(rotation)
  method <- match.arg(method)

  if (!requireNamespace("psych", quietly = TRUE)) {
    stop("Package 'psych' required for this function to work. Please install it.", call. = FALSE)
  }

  # --------------------------------------------------------
  # try to automatically set labels is not passed as parameter
  # --------------------------------------------------------
  if (is.null(var.labels) && is.data.frame(data)) {
    var.labels <- sjlabelled::get_label(data, def.value = colnames(data))
  }
  # ----------------------------
  # check if user has passed a data frame
  # or a pca object
  # ----------------------------
  if (inherits(data, "fa")) {
    if (sort == TRUE) {
      fadata <- psych::fa.sort(data) #resort loadings
    } else {
      fadata <- data
    }
    dataframeparam <- FALSE
  } else if (is.data.frame(data)) {

    if (is.null(nmbr.fctr)) {
      nr_factors <- psych::fa.parallel(data, fa = 'fa', fm = method)$nfact
      dev.off()
      fadata <- psych::fa(data, nfactors = nr_factors, fm = method, rotate = rotation)
      if (sort == TRUE) {
        fadata <- psych::fa.sort(fadata) #resort loadings
      }
    }
    else {

      fadata <- psych::fa(data, nfactors = nmbr.fctr, fm = method, rotate = rotation)
      if (sort == TRUE) {
        fadata <- psych::fa.sort(fadata) #resort loadings
      }
    }
    dataframeparam <- TRUE
  }


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
  tag.centeralign <- "centeralign"
  tag.rightalign <- "rightalign"
  tag.cronbach <- "cronbach"
  tag.comm <- "comm"
  tag.rotation <- "rotation"
  tag.kmo <- "kmo"
  tag.arc <- "arc"
  tag.minval <- "minval"
  tag.removable <- "removable"
  tag.firsttablerow <- "firsttablerow"
  tag.firsttablecol <- "firsttablecol"
  css.table <- "border-collapse:collapse; border:none;"
  css.caption <- "font-weight: bold; text-align:left;"
  css.thead <- "border-top:double black; padding:0.2cm;"
  css.tdata <- "padding:0.2cm;"
  css.centeralign <- "text-align:center;"
  css.rightalign <- "text-align:right;"
  css.cronbach <- "font-style:italic; border-bottom:double;"
  css.comm <- "font-style:italic; color:#666666;"
  css.kmo <- "font-style:italic;"
  css.rotation <- "font-style:italic; font-size:0.9em;"
  css.minval <- "color:#cccccc;"
  css.arc <- "background-color:#eaeaea;"
  css.removable <- "background-color:#eacccc;"
  css.firsttablerow <- "border-top:1px solid black;"
  css.firsttablecol <- ""
  if (!show.comm && show.cronb) css.cronbach <- sprintf("%s border-bottom:double;", css.cronbach)

  # ------------------------
  # check user defined style sheets
  # ------------------------
  if (!is.null(CSS)) {
    if (!is.null(CSS[['css.table']])) css.table <- ifelse(substring(CSS[['css.table']], 1, 1) == '+', paste0(css.table, substring(CSS[['css.table']], 2)), CSS[['css.table']])
    if (!is.null(CSS[['css.thead']])) css.thead <- ifelse(substring(CSS[['css.thead']], 1, 1) == '+', paste0(css.thead, substring(CSS[['css.thead']], 2)), CSS[['css.thead']])
    if (!is.null(CSS[['css.tdata']])) css.tdata <- ifelse(substring(CSS[['css.tdata']], 1, 1) == '+', paste0(css.tdata, substring(CSS[['css.tdata']], 2)), CSS[['css.tdata']])
    if (!is.null(CSS[['css.caption']])) css.caption <- ifelse(substring(CSS[['css.caption']], 1, 1) == '+', paste0(css.caption, substring(CSS[['css.caption']], 2)), CSS[['css.caption']])
    if (!is.null(CSS[['css.centeralign']])) css.centeralign <- ifelse(substring(CSS[['css.centeralign']], 1, 1) == '+', paste0(css.centeralign, substring(CSS[['css.centeralign']], 2)), CSS[['css.centeralign']])
    if (!is.null(CSS[['css.rightalign']])) css.rightalign <- ifelse(substring(CSS[['css.rightalign']], 1, 1) == '+', paste0(css.rightalign, substring(CSS[['css.rightalign']], 2)), CSS[['css.rightalign']])
    if (!is.null(CSS[['css.arc']])) css.arc <- ifelse(substring(CSS[['css.arc']], 1, 1) == '+', paste0(css.arc, substring(CSS[['css.arc']], 2)), CSS[['css.arc']])
    if (!is.null(CSS[['css.firsttablerow']])) css.firsttablerow <- ifelse(substring(CSS[['css.firsttablerow']], 1, 1) == '+', paste0(css.firsttablerow, substring(CSS[['css.firsttablerow']], 2)), CSS[['css.firsttablerow']])
    if (!is.null(CSS[['css.firsttablecol']])) css.firsttablecol <- ifelse(substring(CSS[['css.firsttablecol']], 1, 1) == '+', paste0(css.firsttablecol, substring(CSS[['css.firsttablecol']], 2)), CSS[['css.firsttablecol']])
    if (!is.null(CSS[['css.cronbach']])) css.cronbach <- ifelse(substring(CSS[['css.cronbach']], 1, 1) == '+', paste0(css.cronbach, substring(CSS[['css.cronbach']], 2)), CSS[['css.cronbach']])
    if (!is.null(CSS[['css.comm']])) css.comm <- ifelse(substring(CSS[['css.comm']], 1, 1) == '+', paste0(css.comm, substring(CSS[['css.comm']], 2)), CSS[['css.comm']])
    if (!is.null(CSS[['css.kmo']])) css.kmo <- ifelse(substring(CSS[['css.kmo']], 1, 1) == '+', paste0(css.kmo, substring(CSS[['css.kmo']], 2)), CSS[['css.kmo']])
    if (!is.null(CSS[['css.rotation']])) css.rotation <- ifelse(substring(CSS[['css.rotation']], 1, 1) == '+', paste0(css.rotation, substring(CSS[['css.rotation']], 2)), CSS[['css.rotation']])
    if (!is.null(CSS[['css.minval']])) css.minval <- ifelse(substring(CSS[['css.minval']], 1, 1) == '+', paste0(css.minval, substring(CSS[['css.minval']], 2)), CSS[['css.minval']])
    if (!is.null(CSS[['css.removable']])) css.removable <- ifelse(substring(CSS[['css.removable']], 1, 1) == '+', paste0(css.removable, substring(CSS[['css.removable']], 2)), CSS[['css.removable']])
  }
  # ------------------------
  # set page style
  # ------------------------
  page.style <-  sprintf("<style>\nhtml, body { background-color: white; }\n%s { %s }\n%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s  { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s  { %s }\n</style>",
                         tag.table, css.table, tag.caption, css.caption, tag.thead, css.thead,
                         tag.tdata, css.tdata, tag.cronbach, css.cronbach, tag.minval, css.minval,
                         tag.removable, css.removable, tag.firsttablerow, css.firsttablerow,
                         tag.firsttablecol, css.firsttablecol, tag.centeralign, css.centeralign,
                         tag.rightalign, css.rightalign, tag.rotation, css.rotation,
                         tag.comm, css.comm, tag.kmo, css.kmo, tag.arc, css.arc)
  # ------------------------
  # start content
  # ------------------------
  toWrite <- paste0(toWrite, page.style)
  toWrite = paste(toWrite, "\n</head>\n<body>", "\n")


  # create data frame with factor loadings
  loadings <- fadata$loadings[]
  names <- rownames(fadata$loadings)
  df <- as.data.frame(loadings, row.names = names)

  # ----------------------------
  # check if user defined labels have been supplied
  # if not, use variable names from data frame
  # ----------------------------
  if (is.null(var.labels)) var.labels <- row.names(df)
  # ----------------------------
  # Prepare length of labels
  # ----------------------------
  if (!is.null(var.labels)) {
    # wrap long variable labels
    var.labels <- sjmisc::word_wrap(var.labels, wrap.labels, "<br>")
    # resort labels when sort == TRUE
    if (sort == TRUE) {
      var.labels <- var.labels[fadata$order]
    }
  }
  # --------------------------------------------------------
  # this function checks which items have unclear factor loadings,
  # i.e. which items do not strongly load on a single factor but
  # may load almost equally on several factors
  # --------------------------------------------------------
  getRemovableItems <- function(dataframe) {
    # clear vector
    removers <- c()
    # iterate each row of the data frame. each row represents
    # one item with its factor loadings
    for (i in seq_len(nrow(dataframe))) {
      # get factor loadings for each item
      rowval <- as.numeric(abs(df[i, ]))
      # retrieve highest loading
      maxload <- max(rowval)
      # retrieve 2. highest loading
      max2load <- sort(rowval, TRUE)[2]
      # check difference between both
      if (abs(maxload - max2load) < fctr.load.tlrn) {
        # if difference is below the tolerance,
        # remeber row-ID so we can remove that items
        # for further PCA with updated data frame
        removers <- c(removers, i)
      }
    }
    # return a vector with index numbers indicating which items
    # have unclear loadings
    return(removers)
  }
  # --------------------------------------------------------
  # this function retrieves a list with the column index ("factor" index)
  # where each case of the data frame has its highedt factor loading.
  # So we know to which "group" (factor dimension) each case of the
  # data frame belongs to according to the pca results
  # --------------------------------------------------------
  getItemLoadings <- function(dataframe) {
    # return a vector with index numbers indicating which items
    # loads the highest on which factor
    return(apply(dataframe, 1, function(x) which.max(abs(x))))
  }
  # --------------------------------------------------------
  # this function calculates the cronbach's alpha value for
  # each factor scale, i.e. all variables with the highest loading
  # for a factor are taken for the reliability test. The result is
  # an alpha value for each factor dimension
  # --------------------------------------------------------
  getCronbach <- function(dataframe, itemloadings) {
    # clear vector
    cbv <- c()
    # iterate all highest factor loadings of items
    for (n in seq_len(length(unique(itemloadings)))) {
      # calculate cronbach's alpha for those cases that all have the
      # highest loading on the same factor
      cbv <- c(cbv, performance::cronbachs_alpha(stats::na.omit(dataframe[, which(itemloadings == n)])))
    }
    # cbv now contains the factor numbers and the related alpha values
    # for each "factor dimension scale"
    return(cbv)
  }
  # ----------------------------------
  # Cronbach's Alpha can only be calculated when having a data frame
  # with each component / variable as column
  # ----------------------------------
  if (dataframeparam) {
    # get alpha values
    alphaValues <- getCronbach(data, getItemLoadings(df))
  } else {
    message("Cronbach's Alpha can only be calculated when having a data frame with each component / variable as column.")
    alphaValues <- NULL
    show.cronb <- FALSE
  }
  # -------------------------------------
  # retrieve those items that have unclear factor loadings, i.e.
  # which almost load equally on several factors. The tolerance
  # that indicates which difference between factor loadings is
  # considered as "equally" is defined via fctr.load.tlrn
  # -------------------------------------
  removableItems <- getRemovableItems(df)
  # -------------------------------------
  # retrieve kmo and msa for data set
  # -------------------------------------
  #kmo <- NULL # not implemented at the moment
  #if (show.msa) kmo <- psych::KMO(data)

  # -------------------------------------
  # convert data frame, add label names
  # -------------------------------------
  maxdf <- apply(df, 1, function(x) max(abs(x)))
  # -------------------------------------
  # start table tag
  # -------------------------------------
  page.content <- "<table>\n"
  # -------------------------------------
  # table caption, variable label
  # -------------------------------------
  if (!is.null(title)) page.content <- paste0(page.content, sprintf("  <caption>%s</caption>\n", title))
  # -------------------------------------
  # header row
  # -------------------------------------
  # write tr-tag
  page.content <- paste0(page.content, "  <tr>\n")
  # first column
  page.content <- paste0(page.content, "    <th class=\"thead\">&nbsp;</th>\n")
  # iterate columns
  for (i in seq_len(ncol(df))) {
    page.content <- paste0(page.content, sprintf("    <th class=\"thead\">Factor %i</th>\n", i))
  }
  # check if communality column should be shown
  if (show.comm) page.content <- paste0(page.content, "    <th class=\"thead comm\">Communality</th>\n")
  # close table row
  page.content <- paste0(page.content, "  </tr>\n")
  # -------------------------------------
  # data rows
  # -------------------------------------
  # iterate all rows of df
  for (i in seq_len(nrow(df))) {
    # start table row
    rowcss <- ""
    # check for removable items in first row
    if (i %in% removableItems && i == 1) rowcss <- " firsttablerow removable"
    # check for removable items in other rows
    if (i %in% removableItems && i != 1) rowcss <- " removable"
    # check for non-removable items in first row
    if (is.na(match(i, removableItems)) && i == 1) rowcss <- " firsttablerow"
    # default row string for alternative row colors
    arcstring <- ""
    # if we have alternating row colors, set css
    if (alternate.rows) arcstring <- ifelse(sjmisc::is_even(i), " arc", "")
    # write tr-tag with class-attributes
    page.content <- paste0(page.content, "  <tr>\n")
    # print first table cell
    page.content <- paste0(page.content, sprintf("    <td class=\"firsttablecol%s%s\">%s</td>\n",
                                                 arcstring, rowcss, var.labels[i]))
    # iterate all columns
    for (j in seq_len(ncol(df))) {
      # start table column
      colcss <- sprintf(" class=\"tdata centeralign%s%s\"", arcstring, rowcss)
      if (maxdf[[i]] != max(abs(df[i, j])))
        colcss <- sprintf(" class=\"tdata centeralign minval%s%s\"", arcstring, rowcss)
      page.content <- paste0(page.content, sprintf("    <td%s>%.*f</td>\n",
                                                   colcss, digits, df[i, j]))
    }
    # check if comm column should be shown
    if (show.comm) page.content <- paste0(page.content, sprintf("    <td class=\"tdata comm centeralign%s%s\">%.*f</td>\n",
                                                               arcstring,
                                                               rowcss,
                                                               digits,
                                                               fadata$communalities[[i]]))
    # close row
    page.content <- paste0(page.content, "  </tr>\n")
  }

  #
  #
  # # -------------------------------------
  # # Total Communalities   # not implemented at the moment
  # # -------------------------------------
  if (show.comm) {
    # write tr-tag with class-attributes
    page.content <- paste0(page.content, "  <tr>\n")
    page.content <- paste0(page.content, "    <td class=\"tdata kmo\">Total Communalities</td>\n")
    page.content <- paste0(page.content, sprintf("    <td class=\"tdata centeralign kmo\" colspan=\"%i\"></td>\n", ncol(df)))
    page.content <- paste0(page.content, sprintf("    <td class=\"tdata centeralign kmo\">%.*f</td>\n", digits, sum(fadata$communalities)))
    page.content <- paste0(page.content, "  </tr>\n")
  }


  # -------------------------------------
  # cronbach's alpha
  # -------------------------------------
  if (show.cronb && !is.null(alphaValues)) {
    # write tr-tag with class-attributes
    page.content <- paste0(page.content, "  <tr>\n")
    # first column
    page.content <- paste0(page.content, "    <td class=\"tdata cronbach\">Cronbach's &alpha;</td>\n")
    # iterate alpha-values
    for (i in seq_len(length(alphaValues))) {
      page.content <- paste0(page.content, sprintf("    <td class=\"tdata centeralign cronbach\">%.*f</td>\n",
                                                   digits,
                                                   alphaValues[i]))
    }
    # check if comm column should be shown
    if (show.comm) page.content <- paste0(page.content, "    <td class=\"tdata centeralign cronbach\"></td>\n")
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
  # create list with factor loadings that indicate
  # on which column inside the data frame the highest
  # loading is
  # -------------------------------------
  factorindex <- apply(df, 1, function(x) which.max(abs(x)))
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
  knitr <- gsub(tag.rightalign, css.rightalign, knitr, fixed = TRUE, useBytes = TRUE)
  knitr <- gsub(tag.cronbach, css.cronbach, knitr, fixed = TRUE, useBytes = TRUE)
  knitr <- gsub(tag.comm, css.comm, knitr, fixed = TRUE, useBytes = TRUE)
  knitr <- gsub(tag.arc, css.arc, knitr, fixed = TRUE, useBytes = TRUE)
  knitr <- gsub(tag.kmo, css.kmo, knitr, fixed = TRUE, useBytes = TRUE)
  knitr <- gsub(tag.rotation, css.rotation, knitr, fixed = TRUE, useBytes = TRUE)
  knitr <- gsub(tag.minval, css.minval, knitr, fixed = TRUE, useBytes = TRUE)
  knitr <- gsub(tag.removable, css.removable, knitr, fixed = TRUE, useBytes = TRUE)
  knitr <- gsub(tag.firsttablerow, css.firsttablerow, knitr, fixed = TRUE, useBytes = TRUE)
  knitr <- gsub(tag.firsttablecol, css.firsttablecol, knitr, fixed = TRUE, useBytes = TRUE)
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
  structure(
    class = c("sjTable", "sjtfa"),
    list(
      page.style = page.style,
      page.content = page.content,
      page.complete = toWrite,
      knitr = knitr,
      factor.index = factorindex,
      removed.items = removableItems,
      file = file,
      viewer = use.viewer
    )
  )
}
