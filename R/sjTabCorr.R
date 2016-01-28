#' @title Summary of correlations as HTML table
#' @name sjt.corr
#' 
#' @seealso \itemize{
#'           \item \href{http://www.strengejacke.de/sjPlot/sjt.corr}{sjPlot manual: sjt.corr}
#'           \item \code{\link{sjp.corr}}
#'          }
#' 
#' @description Shows the results of a computed correlation as HTML table. Requires either 
#'                a \code{\link{data.frame}} or a matrix with correlation coefficients 
#'                as returned by the \code{\link{cor}}-function.
#'                
#' @param data matrix with correlation coefficients as returned by the 
#'          \code{\link{cor}}-function, or a \code{\link{data.frame}} of variables that
#'          should be correlated.
#' @param missingDeletion indicates how missing values are treated. May be either
#'          \code{"listwise"} or \code{"pairwise"} (default).
#' @param corMethod indicates the correlation computation method. May be one of
#'          \code{"spearman"} (default), \code{"pearson"} or \code{"kendall"}.
#' @param showPValues logical, whether significance levels (p-values) of correlations should 
#'          be printed or not. See 'Note'.
#' @param pvaluesAsNumbers logical, if \code{TRUE}, the significance levels (p-values) are printed as numbers.
#'          if \code{FALSE} (default), asterisks are used. See 'Note'.
#' @param fade.ns logical, if \code{TRUE} (default), non-significant correlation-values appear faded (by using
#'          a lighter grey text color). See 'Note'.
#' @param varlabels character vector with item labels that are printed along the 
#'          first column/row. If no item labels are provided (default), the 
#'          data frame's column names are used. Item labels are detected automatically 
#'          if \code{data} is a \code{\link{data.frame}} where variables have
#'          label attributes (see \code{\link[sjmisc]{set_label}}) for details).
#' @param digits amount of digits used the values inside table cells.
#'          Default is 2.
#' @param triangle indicates whether only the upper right (use \code{"upper"}), lower left (use \code{"lower"})
#'          or both (use \code{"both"}) triangles of the correlation table is filled with values. Default
#'          is \code{"both"}. You can specifiy the inital letter only.
#' @param val.rm specify a number between 0 and 1 to suppress the output of correlation values 
#'          that are smaller than \code{val.rm}. The absolute correlation values are used, so
#'          a correlation value of \code{-.5} would be greater than \code{val.rm = .4} and thus not be
#'          omitted. By default, this argument is \code{NULL}, hence all values are shown in the table.
#'          If a correlation value is below the specified value of \code{val.rm}, it is still printed to
#'          the HTML table, but made "invisible" with white foreground color. You can use the \code{CSS}
#'          argument (\code{"css.valueremove"}) to change color and appearance of those correlation value that are smaller than
#'          the limit specified by \code{val.rm}. 
#' @param stringDiagonal a vector with string values of the same length as \code{ncol(data)} (number of
#'          correlated items) that can be used to display content in the diagonal cells
#'          where row and column item are identical (i.e. the "self-correlation"). By defauilt,
#'          this argument is \code{NULL} and the diagnal cells are empty.
#'          
#' @inheritParams sjt.frq
#' @inheritParams sjt.df
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
#' @note If \code{data} is a matrix with correlation coefficients as returned by 
#'       the \code{\link{cor}}-function, p-values can't be computed.
#'       Thus, \code{showPValues}, \code{pvaluesAsNumbers} and \code{fade.ns}
#'       only have an effect if \code{data} is a \code{\link{data.frame}}.
#'       \cr \cr
#'       Additionally, see 'Note' in \code{\link{sjt.frq}}.
#'  
#' @details See 'Details' in \code{\link{sjt.frq}}.
#' 
#' @examples
#' \dontrun{
#' # create data frame with 5 random variables
#' mydf <- data.frame(cbind(runif(10), 
#'                          runif(10), 
#'                          runif(10), 
#'                          runif(10), 
#'                          runif(10)))
#' 
#' # plot correlation matrix using circles
#' sjt.corr(mydf)
#' 
#' # -------------------------------
#' # Data from the EUROFAMCARE sample dataset
#' # -------------------------------
#' library(sjmisc)
#' data(efc)
#' 
#' # retrieve variable and value labels
#' varlabs <- get_label(efc)
#' 
#' # recveive first item of COPE-index scale
#' start <- which(colnames(efc) == "c83cop2")
#' # recveive last item of COPE-index scale
#' end <- which(colnames(efc) == "c88cop7")
#'  
#' # create data frame with COPE-index scale
#' mydf <- data.frame(efc[, c(start:end)])
#' colnames(mydf) <- varlabs[c(start:end)]
#'
#' # we have high correlations here, because all items
#' # belong to one factor. See example from "sjp.pca". 
#' sjt.corr(mydf, pvaluesAsNumbers = TRUE)
#' 
#' # -------------------------------
#' # auto-detection of labels, only lower triangle
#' # -------------------------------
#' sjt.corr(efc[, c(start:end)], triangle = "lower")
#' 
#' # -------------------------------
#' # auto-detection of labels, only lower triangle,
#' # all correlation values smaller than 0.3 are not
#' # shown in the table
#' # -------------------------------
#' sjt.corr(efc[, c(start:end)], 
#'          triangle = "lower", 
#'          val.rm = 0.3)
#' 
#' # -------------------------------
#' # auto-detection of labels, only lower triangle,
#' # all correlation values smaller than 0.3 are printed
#' # in blue
#' # -------------------------------
#' sjt.corr(efc[, c(start:end)], 
#'          triangle = "lower",
#'          val.rm = 0.3, 
#'          CSS = list(css.valueremove = 'color:blue;'))}
#' 
#' @importFrom stats na.omit
#' @export
sjt.corr <- function(data,
                     missingDeletion = "pairwise",
                     corMethod = "spearman",
                     title = NULL,
                     showPValues = TRUE,
                     pvaluesAsNumbers = FALSE,
                     fade.ns = TRUE,
                     file = NULL, 
                     varlabels = NULL,
                     breakLabelsAt = 40,
                     digits = 3,
                     triangle = "both",
                     val.rm = NULL,
                     stringDiagonal = NULL,
                     encoding = NULL,
                     CSS = NULL,
                     useViewer = TRUE,
                     no.output = FALSE,
                     remove.spaces = TRUE) {
  # --------------------------------------------------------
  # check p-value-style option
  # --------------------------------------------------------
  opt <- getOption("p_zero")
  if (is.null(opt) || opt == FALSE) {
    p_zero <- ""
  } else {
    p_zero <- "0"
  }
  # --------------------------------------------------------
  # check encoding
  # --------------------------------------------------------
  encoding <- get.encoding(encoding, data)
  # --------------------------------------------------------
  # argument check
  # --------------------------------------------------------
  if (is.null(triangle)) {
    triangle <- "both"
  } else if (triangle == "u" || triangle == "upper") {
    triangle <- "upper"
  } else if (triangle == "l" || triangle == "lower") {
    triangle <- "lower"
  } else triangle <- "both"
  # --------------------------------------------------------
  # try to automatically set labels is not passed as argument
  # --------------------------------------------------------
  if (is.null(varlabels) && is.data.frame(data)) {
    varlabels <- c()
    # if yes, iterate each variable
    for (i in 1:ncol(data)) {
      # retrieve variable name attribute
      vn <- sjmisc::get_label(data[[i]], def.value = colnames(data)[i])
      # if variable has attribute, add to variableLabel list
      if (!is.null(vn)) {
        varlabels <- c(varlabels, vn)
      } else {
        # else break out of loop
        varlabels <- NULL
        break
      }
    }
  }
  # ----------------------------
  # check for valid argument
  # ----------------------------
  if (corMethod != "pearson" && corMethod != "spearman" && corMethod != "kendall") {
    stop("argument 'corMethod' must be one of: pearson, spearman or kendall")
  }
  # ----------------------------
  # check if user has passed a data frame
  # or a pca object
  # ----------------------------
  if (class(data) == "matrix") {
    corr <- data
    cpvalues <- NULL
  } else {
    # missing deletion corresponds to
    # SPSS listwise
    if (missingDeletion == "listwise") {
      data <- stats::na.omit(data)
      corr <- cor(data, method = corMethod)
    } else {
      # missing deletion corresponds to
      # SPSS pairwise
      corr <- cor(data, 
                  method = corMethod, 
                  use = "pairwise.complete.obs")
    }
    #---------------------------------------
    # if we have a data frame as argument,
    # compute p-values of significances
    #---------------------------------------
    computePValues <- function(df) {
      cp <- c()
      for (i in 1:ncol(df)) {
        pv <- c()
        for (j in 1:ncol(df)) {
          test <- cor.test(df[[i]], 
                           df[[j]], 
                           alternative = "two.sided", 
                           method = corMethod)
          pv <- cbind(pv, round(test$p.value, 5))
        }
        cp <- rbind(cp, pv)
      }
      return(cp)
    }
    cpvalues <- computePValues(data)
  }
  # --------------------------------------------------------
  # save original p-values
  # --------------------------------------------------------
  cpv <- cpvalues
  # --------------------------------------------------------
  # add column with significance value
  # --------------------------------------------------------
  if (!is.null(cpvalues)) {
    if (!pvaluesAsNumbers) {
      # --------------------------------------------------------
      # prepare function for apply-function. replace sig. p
      # with asterisks
      # --------------------------------------------------------
      fun.star <- function(x) {
        x <- get_p_stars(x)
      }
    } else {
      # --------------------------------------------------------
      # prepare function for apply-function.
      # round p-values, keeping the numeric values
      # --------------------------------------------------------
      fun.star <- function(x) {
        round(x, digits)
      }
    }
    cpvalues <- apply(cpvalues, c(1,2), fun.star)
    if (pvaluesAsNumbers) {
      cpvalues <- apply(cpvalues, c(1,2), function(x) if (x < 0.001) 
                                                        x <- sprintf("&lt;%s.001", p_zero) 
                                                      else 
                                                        x <- sub("0", p_zero, sprintf("%.*f", digits, x)))
    }
  } else {
    showPValues <- FALSE
  }
  # ----------------------------
  # check if user defined labels have been supplied
  # if not, use variable names from data frame
  # ----------------------------
  if (is.null(varlabels)) {
    varlabels <- row.names(corr)
  }
  # check length of x-axis-labels and split longer strings at into new lines
  varlabels <- sjmisc::word_wrap(varlabels, breakLabelsAt, "<br>")
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
  tag.notsig <- "notsig"
  tag.pval <- "pval"
  tag.valueremove <- "valueremove"
  tag.summary <- "summary"
  tag.centeralign <- "centeralign"
  tag.firsttablecol <- "firsttablecol"
  css.table <- "border-collapse:collapse; border:none;"
  css.thead <- "font-style:italic; font-weight:normal; border-top:double black; border-bottom:1px solid black; padding:0.2cm;"
  css.tdata <- "padding:0.2cm;"
  css.caption <- "font-weight: bold; text-align:left;"
  css.valueremove <- "color:white;"
  css.centeralign <- "text-align:center;"
  css.firsttablecol <- "font-style:italic;"
  css.notsig <- "color:#999999;"
  css.summary <- "border-bottom:double black; border-top:1px solid black; font-style:italic; font-size:0.9em; text-align:right;"
  css.pval <- "vertical-align:super;font-size:0.8em;"
  if (pvaluesAsNumbers) css.pval <- "font-style:italic;"
  # ------------------------
  # check user defined style sheets
  # ------------------------
  if (!is.null(CSS)) {
    if (!is.null(CSS[['css.table']])) css.table <- ifelse(substring(CSS[['css.table']], 1, 1) == '+', paste0(css.table, substring(CSS[['css.table']], 2)), CSS[['css.table']])
    if (!is.null(CSS[['css.thead']])) css.thead <- ifelse(substring(CSS[['css.thead']], 1, 1) == '+', paste0(css.thead, substring(CSS[['css.thead']], 2)), CSS[['css.thead']])
    if (!is.null(CSS[['css.tdata']])) css.tdata <- ifelse(substring(CSS[['css.tdata']], 1, 1) == '+', paste0(css.tdata, substring(CSS[['css.tdata']], 2)), CSS[['css.tdata']])
    if (!is.null(CSS[['css.caption']])) css.caption <- ifelse(substring(CSS[['css.caption']], 1, 1) == '+', paste0(css.caption, substring(CSS[['css.caption']], 2)), CSS[['css.caption']])
    if (!is.null(CSS[['css.summary']])) css.summary <- ifelse(substring(CSS[['css.summary']], 1, 1) == '+', paste0(css.summary, substring(CSS[['css.summary']], 2)), CSS[['css.summary']])
    if (!is.null(CSS[['css.centeralign']])) css.centeralign <- ifelse(substring(CSS[['css.centeralign']], 1, 1) == '+', paste0(css.centeralign, substring(CSS[['css.centeralign']], 2)), CSS[['css.centeralign']])
    if (!is.null(CSS[['css.firsttablecol']])) css.firsttablecol <- ifelse(substring(CSS[['css.firsttablecol']], 1, 1) == '+', paste0(css.firsttablecol, substring(CSS[['css.firsttablecol']], 2)), CSS[['css.firsttablecol']])
    if (!is.null(CSS[['css.notsig']])) css.notsig <- ifelse(substring(CSS[['css.notsig']], 1, 1) == '+', paste0(css.notsig, substring(CSS[['css.notsig']], 2)), CSS[['css.notsig']])
    if (!is.null(CSS[['css.pval']])) css.pval <- ifelse(substring(CSS[['css.pval']], 1, 1) == '+', paste0(css.pval, substring(CSS[['css.pval']], 2)), CSS[['css.pval']])
    if (!is.null(CSS[['css.valueremove']])) css.valueremove <- ifelse(substring(CSS[['css.valueremove']], 1, 1) == '+', paste0(css.valueremove, substring(CSS[['css.valueremove']], 2)), CSS[['css.valueremove']])
  }
  # ------------------------
  # set page style
  # ------------------------
  page.style <-  sprintf("<style>%s { %s }\n%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n</style>",
                         tag.table, css.table, tag.caption, css.caption,
                         tag.thead, css.thead, tag.tdata, css.tdata,
                         tag.firsttablecol, css.firsttablecol, 
                         tag.centeralign, css.centeralign,
                         tag.notsig, css.notsig,
                         tag.pval, css.pval,
                         tag.summary, css.summary,
                         tag.valueremove, css.valueremove)
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
  for (i in 1:ncol(corr)) {
    page.content <- paste0(page.content, sprintf("    <th class=\"thead\">%s</th>\n", varlabels[i]))
  }
  # close table row
  page.content <- paste0(page.content, "  </tr>\n")
  # -------------------------------------
  # data rows
  # -------------------------------------
  # iterate all rows of df
  for (i in 1:nrow(corr)) {
    # write tr-tag
    page.content <- paste0(page.content, "  <tr>\n")
    # print first table cell
    page.content <- paste0(page.content, sprintf("    <td class=\"firsttablecol\">%s</td>\n", varlabels[i]))
    # --------------------------------------------------------
    # iterate all columns
    # --------------------------------------------------------
    for (j in 1:ncol(corr)) {
      # --------------------------------------------------------
      # leave out self-correlations
      # --------------------------------------------------------
      if (j == i) {
        if (is.null(stringDiagonal) || length(stringDiagonal) > ncol(corr)) {
          page.content <- paste0(page.content, "    <td class=\"tdata centeralign\">&nbsp;</td>\n")
        } else {
          page.content <- paste0(page.content, sprintf("    <td class=\"tdata centeralign\">%s</td>\n", 
                                                       stringDiagonal[j]))
        }
      } else {
        # --------------------------------------------------------
        # check whether only lower or upper triangle of correlation
        # table should be printed
        # --------------------------------------------------------
        if ((triangle == "upper" && j > i) || (triangle == "lower" && i > j) || triangle == "both") {
          # --------------------------------------------------------
          # print table-cell-data (cor-value)
          # --------------------------------------------------------
          cellval <- sprintf("%.*f", digits, corr[i, j])
          # --------------------------------------------------------
          # check whether we want to show P-Values
          # --------------------------------------------------------
          if (showPValues) {
            if (pvaluesAsNumbers) {
              # --------------------------------------------------------
              # if we have p-values as number, print them in new row
              # --------------------------------------------------------
              cellval <- sprintf("%s<br><span class=\"pval\">(%s)</span>", cellval, cpvalues[i, j])
            } else {
              # --------------------------------------------------------
              # if we have p-values as "*", add them
              # --------------------------------------------------------
              cellval <- sprintf("%s<span class=\"pval\">%s</span>", cellval, cpvalues[i, j])
            }
          }
          # --------------------------------------------------------
          # prepare css for not significant values
          # --------------------------------------------------------
          notsig <- ""
          # --------------------------------------------------------
          # check whether non significant values should be blurred
          # --------------------------------------------------------
          if (fade.ns && !is.null(cpv)) {
            # set css-class-attribute
            if (cpv[i, j] >= 0.05) notsig <- " notsig"
          }
          # --------------------------------------------------------
          # prepare css for values that shoould be removed due to low
          # correlation value
          # --------------------------------------------------------
          value.remove <- ""
          # --------------------------------------------------------
          # check whether correlation value is too small and should
          # be omitted
          # --------------------------------------------------------
          if (!is.null(val.rm) && abs(corr[i, j]) < abs(val.rm)) {
            value.remove <- " valueremove"            
          }
          page.content <- paste0(page.content, sprintf("    <td class=\"tdata centeralign%s%s\">%s</td>\n", 
                                                       notsig, 
                                                       value.remove, 
                                                       cellval))
        } else {
          page.content <- paste0(page.content, "    <td class=\"tdata centeralign\">&nbsp;</td>\n")
        }
      }
    }
    # close row
    page.content <- paste0(page.content, "  </tr>\n")
  }
  # -------------------------------------
  # feedback...
  # -------------------------------------
  page.content <- paste0(page.content, "  <tr>\n")
  page.content <- paste0(page.content, sprintf("    <td colspan=\"%i\" class=\"summary\">", ncol(corr) + 1))
  page.content <- paste0(page.content, sprintf("Computed correlation used %s-method with %s-deletion.", corMethod, missingDeletion))
  page.content <- paste0(page.content, "</td>\n  </tr>\n")
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
  knitr <- gsub(tag.notsig, css.notsig, knitr, fixed = TRUE, useBytes = TRUE)  
  knitr <- gsub(tag.pval, css.pval, knitr, fixed = TRUE, useBytes = TRUE)  
  knitr <- gsub(tag.summary, css.summary, knitr, fixed = TRUE, useBytes = TRUE)  
  knitr <- gsub(tag.firsttablecol, css.firsttablecol, knitr, fixed = TRUE, useBytes = TRUE)  
  knitr <- gsub(tag.valueremove, css.valueremove, knitr, fixed = TRUE, useBytes = TRUE)  
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
  invisible(structure(class = "sjtcorr",
                      list(page.style = page.style,
                           page.content = page.content,
                           output.complete = toWrite,
                           knitr = knitr)))
}
