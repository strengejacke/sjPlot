#' @title Show grouped means as HTML table
#' @name sjt.grpmean
#' 
#' @description Computes mean, sd and se for each sub-group (indicated by \code{varGrp})
#'                of \code{varCount} and prints the result as HTML table.
#'              
#' @seealso \code{\link{sjp.aov1}}  
#' 
#' @param varCount a numeric vector / variable. Mean, SD and SE for this variable are calculated.
#' @param varGrp a (numeric) vector with group indices, used to select sub-groups from \code{varCount}.
#' @param rowLabels a character vector of same length as \code{varGrp} unqiue values. In short: the
#'          value labels of \code{varGrp}. Used to name table rows. By default, row labels
#'          are automatically detected if set by \code{set_val_labels}.
#' @param digits amount of digits for table values.
#' @param digits.summary amount of digits for summary statistics (Anova).
#' @param file The destination file, which will be in html-format. If no filepath is specified,
#'          the file will be saved as temporary file and openend either in the RStudio View pane or
#'          in the default web browser.
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
#'          }
#'          See further examples below and the \href{http://www.strengejacke.de/sjPlot/sjtbasics}{sjPlot manual: sjt-basics}.
#' @param useViewer If \code{TRUE}, the function tries to show the HTML table in the IDE's viewer pane. If
#'          \code{FALSE} or no viewer available, the HTML table is opened in a web browser.
#' @param no.output If \code{TRUE}, the html-output is neither opened in a browser nor shown in
#'          the viewer pane and not even saved to file. This option is useful when the html output
#'          should be used in \code{knitr} documents. The html output can be accessed via the return
#'          value.
#' @param remove.spaces logical, if \code{TRUE}, leading spaces are removed from all lines in the final string
#'          that contains the html-data. Use this, if you want to remove parantheses for html-tags. The html-source
#'          may look less pretty, but it may help when exporting html-tables to office tools.
#' @return Invisibly returns a \code{\link{structure}} with
#'          \itemize{
#'            \item the data frame with the description information (\code{df}),
#'            \item the web page style sheet (\code{page.style}),
#'            \item the web page content (\code{page.content}),
#'            \item the complete html-output (\code{output.complete}) and
#'            \item the html-table with inlin-css for use with knitr (\code{knitr})
#'            }
#'            for further use.
#'
#' @examples
#' \dontrun{
#' data(efc)
#' sjt.grpmean(efc$c12hour,
#'             efc$e42dep)}
#'             
#' @export
sjt.grpmean <- function(varCount, 
                        varGrp, 
                        rowLabels=NULL, 
                        digits=2,
                        digits.summary = 3,
                        file=NULL,
                        encoding=NULL,
                        CSS=NULL,
                        useViewer=TRUE,
                        no.output=FALSE,
                        remove.spaces=TRUE) {
  # --------------------------------------------------------
  # check p-value-style option
  # --------------------------------------------------------
  opt <- getOption("p_zero")
  if (is.null(opt) || opt == FALSE) {
    p_zero <- ""
  }
  else {
    p_zero <- "0"
  }
  # --------------------------------------
  # set value and row labels
  # --------------------------------------
  if (is.null(rowLabels)) rowLabels <- autoSetValueLabels(varGrp)
  varGrpLabel <- autoSetVariableLabels(varGrp)
  varCountLabel <- autoSetVariableLabels(varCount)
  # --------------------------------------
  # handle NULL parameter
  # --------------------------------------
  if (is.null(varGrpLabel)) varGrpLabel <- ""
  if (is.null(varCountLabel)) varCountLabel <- "Compare means"
  # --------------------------------------
  # convert values to numeric
  # --------------------------------------
  varCount <- as.numeric(varCount)
  varGrp <- as.numeric(varGrp)
  # --------------------------------------
  # compute anova statistics for mean table
  # see below
  # --------------------------------------
  fit <- aov(varCount ~ as.factor(varGrp))
  # p-values of means
  means.p <- summary.lm(fit)$coefficients[, 4]
  pval <- c()
  # convert means to apa style
  for (i in 1:length(means.p)) {
    if (means.p[i] < 0.001) {
      pval <- c(pval, sprintf("&lt;%s.001", p_zero))
    }
    else {
      pval <- c(pval, sub("0", p_zero, sprintf("%.*f", digits, means.p[i])))
    }
  } 
  # --------------------------------------
  # retrieve group indices
  # --------------------------------------
  indices <- sort(unique(na.omit(varGrp)))
  df <- data.frame()
  # --------------------------------------
  # iterate all groups
  # --------------------------------------
  for (i in 1:length(indices)) {
    # --------------------------------------
    # add new row to data frame with
    # mean, N, sd and se of varCount for each
    # sub-group (indicated by indices)
    # --------------------------------------
    df <- rbind(df, 
                cbind(mean = sprintf("%.*f", digits, mean(varCount[varGrp == indices[i]], na.rm = TRUE)),
                      N = length(na.omit(varCount[varGrp == indices[i]])),
                      sd = sprintf("%.*f", digits, sd(varCount[varGrp == indices[i]], na.rm = TRUE)),
                      se = sprintf("%.*f", digits, std_e(varCount[varGrp == indices[i]])),
                      p = pval[i]))
  }
  # --------------------------------------
  # finally, add total-row
  # --------------------------------------
  df <- rbind(df, 
              cbind(mean = sprintf("%.*f", digits, mean(varCount, na.rm = TRUE)),
                    N = length(na.omit(varCount)),
                    sd = sprintf("%.*f", digits, sd(varCount, na.rm = TRUE)),
                    se = sprintf("%.*f", digits, std_e(varCount)),
                    p = ""))
  # --------------------------------------
  # fix row labels, if empty or NULL
  # --------------------------------------
  if (is.null(rowLabels) || length(rowLabels) < (nrow(df) - 1)) {
    rowLabels <- as.character(indices)
  }
  rownames(df) <- c(rowLabels, "Total")
  # --------------------------------------
  # get anova statistics for mean table
  # --------------------------------------
  # multiple r2
  r2 <- summary.lm(fit)$r.squared
  # adj. r2
  r2.adj <- summary.lm(fit)$adj.r.squared
  # get F-statistics
  fstat <- summary.lm(fit)$fstatistic[1]
  # p-value for F-test
  pval <- summary(fit)[[1]]['Pr(>F)'][1,1]
  pvalstring <- ifelse(pval < 0.001, sprintf("p&lt;%s.001", p_zero), sub("0", p_zero, sprintf("p=%.*f", digits.summary, pval)))
  # --------------------------------------
  # print data frame to html table
  # --------------------------------------
  html <- sjt.df(df, 
                 describe = F, 
                 title = varCountLabel, 
                 stringVariable = varGrpLabel,
                 showRowNames = T,
                 showCommentRow = T,
                 no.output = T,
                 CSS = CSS,
                 encoding = encoding,
                 hideProgressBar = TRUE,
                 commentString = gsub("0.", 
                                      paste0(".", p_zero), 
                                      sprintf("<strong>Anova:</strong> R<sup>2</sup>=%.*f &middot; adj. R<sup>2</sup>=%.*f &middot; F=%.*f &middot; %s",
                                              digits.summary, r2, digits.summary, r2.adj, digits.summary, fstat, pvalstring),
                                      fixed = TRUE),
                 remove.spaces = remove.spaces)
  # -------------------------------------
  # check if html-content should be printed
  # -------------------------------------
  out.html.table(no.output, file, html$knitr, html$output.complete, useViewer)  
  invisible (list(class="sjtgrpmean",
                  df=df,
                  page.style = html$page.style,
                  page.content = html$page.content,
                  knitr=html$knitr,
                  output.complete=html$output.complete))
}