#' @title Show linear regression as HTML table
#' @name sjt.lm
#' 
#' @description Shows (multiple) fitted linear models (beta coefficients, std. beta values etc.)
#'                as HTML table, or saves them as file. The fitted lm's should have the same predictor variables and
#'                differ only in their response (dependent variable).
#'                
#' @seealso \itemize{
#'            \item \code{\link{sjt.glm}}
#'            \item \code{\link{sjp.lm}}
#'            }
#' 
#' @param ... One or more fitted lm-objects.
#' @param file The destination file, which will be in html-format. If no filepath is specified,
#'          the file will be saved as temporary file and openend either in the RStudio View pane or
#'          in the default web browser.
#' @param labelPredictors Labels of the predictor variables, provided as char vector.
#' @param labelDependentVariables Labels of the dependent variables of all fitted models
#'          which have been used as first parameter(s), provided as char vector.
#' @param stringPredictors String constant used as headline for the predictor column.
#'          Default is \code{"Predictors"}.
#' @param stringDependentVariables String constant used as headline for the 
#'          dependent variable columns. Default is \code{"Dependent Variables"}.
#' @param showHeaderStrings If \code{TRUE}, the header strings \code{stringPredictors}
#'          and \code{stringDependentVariables} are shown. By default, they're hidden.
#' @param stringModel String constant used as headline for the model names in case no 
#'          labels for the dependent variables are provided (see labelDependentVariables).
#'          Default is \code{"Model"}.
#' @param stringIntercept String constant used as headline for the Intercept row
#'          default is \code{"Intercept"}.
#' @param stringObservations String constant used in the summary row for the count of observation
#'          (cases). Default is \code{"Observations"}.
#' @param stringB String used for the column heading of beta coefficients. Default is \code{"B"}.
#' @param stringSB String used for the column heading of standardized beta coefficients. Default is \code{"std. Beta"}.
#' @param stringCI String used for the column heading of confidence interval values. Default is \code{"CI"}.
#' @param stringSE String used for the column heading of standard error values. Default is \code{"std. Error"}.
#' @param stringP String used for the column heading of p values. Default is \code{"p"}.
#' @param showConfInt If \code{TRUE} (default), the confidence intervall is also printed to the table. Use
#'          \code{FALSE} to omit the CI in the table.
#' @param showStdBeta If \code{TRUE}, the standardized beta-coefficients are also printed.
#'          Default is \code{FALSE}.
#' @param showStdError If \code{TRUE}, the standard errors are also printed.
#'          Default is \code{FALSE}.
#' @param digits.est Amount of decimals for estimators.
#' @param digits.p Amount of decimals for p-values.
#' @param digits.ci Amount of decimals for confidence intervals.
#' @param digits.se Amount of decimals for standard error.
#' @param digits.sb Amount of decimals for standardized beta.
#' @param digits.summary Amount of decimals for values in model summary.
#' @param pvaluesAsNumbers If \code{TRUE}, p-values are shown as numbers. If \code{FALSE} (default),
#'          p-values are indicated by asterisks.
#' @param boldpvalues If \code{TRUE} (default), significant p-values are shown bold faced.
#' @param separateConfColumn if \code{TRUE}, the CI values are shown in a separate table column.
#'          Default is \code{FALSE}.
#' @param newLineConf If \code{TRUE} and \code{separateConfColumn} is \code{FALSE}, inserts a line break
#'          between B and CI values. If \code{FALSE}, CI values are printed in the same
#'          line with B values.
#' @param showAbbrHeadline If \code{TRUE} (default), the table data columns have a headline with 
#'          abbreviations for beta- and std. beta-values, confidence interval and p-values.
#' @param showR2 If \code{TRUE} (default), the R2 and adjusted R2 values for each model are printed
#'          in the model summary.
#' @param showFStat If \code{TRUE}, the F-statistics for each model is printed
#'          in the model summary. Default is \code{FALSE}.
#' @param showAIC If \code{TRUE}, the AIC value for each model is printed
#'          in the model summary. Default is \code{FALSE}.
#' @param cellSpacing The inner padding of table cells. By default, this value is 0.2 (measure is cm), which is
#'          suitable for viewing the table. Decrease this value (0.05 to 0.1) if you want to import the table
#'          into Office documents. This is a convenient parameter for the \code{CSS} parameter for changing
#'          cell spacing, which would be: \code{CSS=list(css.thead="padding:0.2cm;", css.tzdata="padding:0.2cm;")}.
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
#'            \item \code{css.lasttablerow='border-bottom: 1px dotted blue;'} for a blue dotted border of the last table row.
#'            \item \code{css.colnames='+color:green'} to add green color formatting to column names.
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
#' @note The HTML tables can either be saved as file and manually opened (specify parameter \code{file}) or
#'         they can be saved as temporary files and will be displayed in the RStudio Viewer pane (if working with RStudio)
#'         or opened with the default web browser. Displaying resp. opening a temporary file is the
#'         default behaviour (i.e. \code{file=NULL}).
#' 
#' @examples
#' # Now fit the models. Note that both models share the same predictors
#' # and only differ in their dependent variable
#' data(efc)
#' 
#' # fit first model
#' fit1 <- lm(barthtot ~ c160age + c12hour + c161sex + c172code, data=efc)
#' # fit second model
#' fit2 <- lm(neg_c_7 ~ c160age + c12hour + c161sex + c172code, data=efc)
#' 
#' # create and open HTML-table in RStudio Viewer Pane or web browser
#' \dontrun{
#' sjt.lm(fit1, 
#'        fit2, 
#'        labelDependentVariables = c("Barthel-Index",
#'                                    "Negative Impact"),
#'        labelPredictors = c("Carer's Age", 
#'                            "Hours of Care", 
#'                            "Carer's Sex", 
#'                            "Educational Status"))
#' 
#' # show HTML-table, indicating p-values as numbers
#' sjt.lm(fit1, 
#'        fit2, 
#'        labelDependentVariables = c("Barthel-Index", 
#'                                    "Negative Impact"),
#'        labelPredictors = c("Carer's Age", 
#'                            "Hours of Care", 
#'                            "Carer's Sex", 
#'                            "Educational Status"),
#'        showStdBeta = TRUE, 
#'        pvaluesAsNumbers = TRUE)
#' 
#' # create and open HTML-table in RStudio Viewer Pane or web browser,
#' # printing CI in a separate column
#' sjt.lm(fit1, 
#'        fit2, 
#'        labelDependentVariables = c("Barthel-Index", 
#'                                    "Negative Impact"),
#'        labelPredictors = c("Carer's Age", 
#'                            "Hours of Care", 
#'                            "Carer's Sex", 
#'                            "Educational Status"),
#'        separateConfColumn = TRUE)
#' 
#' # show HTML-table, indicating p-values as numbers
#' # and printing CI in a separate column
#' sjt.lm(fit1, 
#'        fit2, 
#'        labelDependentVariables = c("Barthel-Index", 
#'                                    "Negative Impact"),
#'        labelPredictors = c("Carer's Age", 
#'                            "Hours of Care", 
#'                            "Carer's Sex", 
#'                            "Educational Status"),
#'        showStdBeta = TRUE, 
#'        pvaluesAsNumbers = TRUE, 
#'        separateConfColumn = TRUE)
#' 
#' # ---------------------------------------------------------------- 
#' # connecting two html-tables
#' # ---------------------------------------------------------------- 
#' # fit two more models
#' fit3 <- lm(tot_sc_e ~ c160age + c12hour + c161sex + c172code, data=efc)
#' fit4 <- lm(e42dep ~ c160age + c12hour + c161sex + c172code, data=efc)
#' 
#' # create and save first HTML-table
#' part1 <- sjt.lm(fit1, 
#'                 fit2, 
#'                 labelDependentVariables = c("Barthel-Index", 
#'                                             "Negative Impact"),
#'                 labelPredictors = c("Carer's Age", 
#'                                     "Hours of Care",
#'                                     "Carer's Sex", 
#'                                     "Educational Status"))
#' # create and save second HTML-table
#' part2 <- sjt.lm(fit3, 
#'                 fit4, 
#'                 labelDependentVariables = c("Service Usage", 
#'                                             "Elder's Dependency"),
#'                 labelPredictors = c("Carer's Age", 
#'                                     "Hours of Care",
#'                                     "Carer's Sex", 
#'                                     "Educational Status"))
#' # browse temporary file
#' htmlFile <- tempfile(fileext=".html")
#' write(sprintf("<html><head>%s</head><body>%s<p></p>%s</body></html>",
#'               part1$page.style, 
#'               part1$page.content, 
#'               part2$page.content),
#'       file = htmlFile)
#' viewer <- getOption("viewer")
#' if (!is.null(viewer)) viewer(htmlFile) else utils::browseURL(htmlFile)
#' 
#' # ---------------------------------------------------------------- 
#' # User defined style sheet
#' # ---------------------------------------------------------------- 
#' sjt.lm(fit1, 
#'        fit2, 
#'        labelDependentVariables = c("Barthel-Index", "Negative Impact"),
#'        labelPredictors = c("Carer's Age", 
#'                            "Hours of Care", 
#'                            "Carer's Sex", 
#'                            "Educational Status"),
#'        CSS = list(css.table = "border: 2px solid;",
#'                   css.tdata = "border: 1px solid;",
#'                   css.depvarhead = "color:#003399;"))}
#'        
#' @export
sjt.lm <- function (..., 
                    file=NULL, 
                    labelPredictors=NULL, 
                    labelDependentVariables=NULL, 
                    stringPredictors="Predictors", 
                    stringDependentVariables="Dependent Variables", 
                    stringModel="Model",
                    showHeaderStrings=FALSE,
                    stringIntercept="(Intercept)",
                    stringObservations="Observations",
                    stringB="B",
                    stringSB="std. Beta",
                    stringCI="CI",
                    stringSE="std. Error",
                    stringP="p",
                    showConfInt=TRUE,
                    showStdBeta=FALSE,
                    showStdError=FALSE,
                    digits.est=2,
                    digits.p=3,
                    digits.ci=2,
                    digits.se=2,
                    digits.sb=2,
                    digits.summary=3,
                    pvaluesAsNumbers=FALSE,
                    boldpvalues=TRUE,
                    separateConfColumn=FALSE,
                    newLineConf=TRUE,
                    showAbbrHeadline=TRUE,
                    showR2=TRUE,
                    showFStat=FALSE,
                    showAIC=FALSE,
                    cellSpacing=0.2,
                    encoding=NULL,
                    CSS=NULL,
                    useViewer=TRUE,
                    no.output=FALSE) {
  # -------------------------------------
  # check encoding
  # -------------------------------------
  encoding <- get.encoding(encoding)
  # ------------------------
  # set page encoding
  # ------------------------
  toWrite <- sprintf("<html>\n<head>\n<meta http-equiv=\"Content-type\" content=\"text/html;charset=%s\">\n", encoding)
  # -------------------------------------
  # init style sheet and tags used for css-definitions
  # we can use these variables for string-replacement
  # later for return value
  # -------------------------------------
  tag.table <- "table"
  tag.thead <- "thead"
  tag.tdata <- "tdata"
  tag.summary <- "summary"
  tag.colnames <- "colnames"
  tag.firstsumrow <- "firstsumrow"
  tag.labelcellborder <- "labelcellborder"
  tag.lasttablerow <- "lasttablerow"
  tag.depvarhead <- "depvarhead"
  tag.topborder <- "topborder"
  tag.topcontentborder <- "topcontentborder"
  tag.annorow <- "annorow"
  tag.noannorow <- "noannorow"
  tag.annostyle <- "annostyle"
  tag.leftalign <- "leftalign"
  tag.centeralign <- "centeralign"
  css.table <- "border-collapse:collapse; border:none;"
  css.thead <- sprintf("border-bottom: 1px solid; padding:%.1fcm;", cellSpacing)
  css.tdata <- sprintf("padding:%.1fcm;", cellSpacing)
  css.summary <- "padding-top:0.1cm; padding-bottom:0.1cm;"
  css.colnames <- "font-style:italic;"
  css.firstsumrow <- "border-top:1px solid;"
  css.labelcellborder <- "border-bottom:1px solid;"
  css.lasttablerow <- "border-bottom: double;"
  css.topborder <- "border-top:double;"
  css.depvarhead <- "text-align:center; border-bottom:1px solid;"
  css.topcontentborder <- "border-top:2px solid;"
  css.annorow <- "border-top:2px solid;"
  css.noannorow <- "border-bottom:double;"
  css.annostyle <- "text-align:right;"
  css.leftalign <- "text-align:left;"
  css.centeralign <- "text-align:center;"
  # change table style if we have pvalues as numbers
  if (pvaluesAsNumbers) css.table <- sprintf("%s%s", css.table, css.noannorow)
  if (showHeaderStrings) css.labelcellborder <- ""
  # ------------------------
  # check user defined style sheets
  # ------------------------
  if (!is.null(CSS)) {
    if (!is.null(CSS[['css.table']])) css.table <- ifelse(substring(CSS[['css.table']],1,1)=='+', paste0(css.table, substring(CSS[['css.table']],2)), CSS[['css.table']])
    if (!is.null(CSS[['css.thead']])) css.thead <- ifelse(substring(CSS[['css.thead']],1,1)=='+', paste0(css.thead, substring(CSS[['css.thead']],2)), CSS[['css.thead']])
    if (!is.null(CSS[['css.tdata']])) css.tdata <- ifelse(substring(CSS[['css.tdata']],1,1)=='+', paste0(css.tdata, substring(CSS[['css.tdata']],2)), CSS[['css.tdata']])
    if (!is.null(CSS[['css.leftalign']])) css.leftalign <- ifelse(substring(CSS[['css.leftalign']],1,1)=='+', paste0(css.leftalign, substring(CSS[['css.leftalign']],2)), CSS[['css.leftalign']])
    if (!is.null(CSS[['css.centeralign']])) css.centeralign <- ifelse(substring(CSS[['css.centeralign']],1,1)=='+', paste0(css.centeralign, substring(CSS[['css.centeralign']],2)), CSS[['css.centeralign']])
    if (!is.null(CSS[['css.summary']])) css.summary <- ifelse(substring(CSS[['css.summary']],1,1)=='+', paste0(css.summary, substring(CSS[['css.summary']],2)), CSS[['css.summary']])
    if (!is.null(CSS[['css.lasttablerow']])) css.lasttablerow <- ifelse(substring(CSS[['css.lasttablerow']],1,1)=='+', paste0(css.lasttablerow, substring(CSS[['css.lasttablerow']],2)), CSS[['css.lasttablerow']])
    if (!is.null(CSS[['css.labelcellborder']])) css.labelcellborder <- ifelse(substring(CSS[['css.labelcellborder']],1,1)=='+', paste0(css.table, substring(CSS[['css.labelcellborder']],2)), CSS[['css.labelcellborder']])
    if (!is.null(CSS[['css.colnames']])) css.colnames <- ifelse(substring(CSS[['css.colnames']],1,1)=='+', paste0(css.colnames, substring(CSS[['css.colnames']],2)), CSS[['css.colnames']])
    if (!is.null(CSS[['css.firstsumrow']])) css.firstsumrow <- ifelse(substring(CSS[['css.firstsumrow']],1,1)=='+', paste0(css.firstsumrow, substring(CSS[['css.firstsumrow']],2)), CSS[['css.firstsumrow']])
    if (!is.null(CSS[['css.topborder']])) css.topborder <- ifelse(substring(CSS[['css.topborder']],1,1)=='+', paste0(css.topborder, substring(CSS[['css.topborder']],2)), CSS[['css.topborder']])
    if (!is.null(CSS[['css.depvarhead']])) css.depvarhead <- ifelse(substring(CSS[['css.depvarhead']],1,1)=='+', paste0(css.depvarhead, substring(CSS[['css.depvarhead']],2)), CSS[['css.depvarhead']])
    if (!is.null(CSS[['css.topcontentborder']])) css.topcontentborder <- ifelse(substring(CSS[['css.topcontentborder']],1,1)=='+', paste0(css.topcontentborder, substring(CSS[['css.topcontentborder']],2)), CSS[['css.topcontentborder']])
    if (!is.null(CSS[['css.annorow']])) css.annorow <- ifelse(substring(CSS[['css.annorow']],1,1)=='+', paste0(css.annorow, substring(CSS[['css.annorow']],2)), CSS[['css.annorow']])
    if (!is.null(CSS[['css.noannorow']])) css.noannorow <- ifelse(substring(CSS[['css.noannorow']],1,1)=='+', paste0(css.noannorow, substring(CSS[['css.noannorow']],2)), CSS[['css.noannorow']])
    if (!is.null(CSS[['css.annostyle']])) css.annostyle <- ifelse(substring(CSS[['css.annostyle']],1,1)=='+', paste0(css.annostyle, substring(CSS[['css.annostyle']],2)), CSS[['css.annostyle']])
  }
  # ------------------------
  # set page style
  # ------------------------
  page.style <-  sprintf("<style>%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n</style>",
                         tag.table, css.table, tag.thead, css.thead, tag.tdata, css.tdata,
                         tag.summary, css.summary, tag.colnames, css.colnames,
                         tag.firstsumrow, css.firstsumrow, tag.lasttablerow, css.lasttablerow,
                         tag.topborder, css.topborder, tag.depvarhead, css.depvarhead,
                         tag.topcontentborder, css.topcontentborder, tag.annorow, css.annorow, 
                         tag.noannorow, css.noannorow, tag.annostyle, css.annostyle,
                         tag.labelcellborder, css.labelcellborder,
                         tag.centeralign, css.centeralign, tag.leftalign, css.leftalign)
  # ------------------------
  # start content
  # ------------------------
  toWrite <- paste0(toWrite, page.style)
  toWrite <- paste0(toWrite, "\n</head>\n<body>\n")
  # ------------------------
  # retrieve fitted models
  # ------------------------
  input_list <- list(...)
  # -------------------------------------
  # if confidence interval should be omitted,
  # don't use separate column for CI!
  # -------------------------------------
  if (!showConfInt) {
    separateConfColumn <- FALSE
    showCIString <- stringB
  }
  else {
    showCIString <- sprintf("%s (%s)", stringB, stringCI)
  }
  # -------------------------------------
  # table headline
  # -------------------------------------
  headerColSpan <- length(input_list)
  headerColSpanFactor <- 1
  if (pvaluesAsNumbers) headerColSpanFactor <- headerColSpanFactor+1
  if (separateConfColumn) headerColSpanFactor <- headerColSpanFactor+1
  if (showStdBeta) headerColSpanFactor <- headerColSpanFactor+1
  if (showStdError) headerColSpanFactor <- headerColSpanFactor+1
  
  headerColSpan <- headerColSpanFactor * headerColSpan
  linebreakstring <- " "
  if (newLineConf) linebreakstring <- "<br>"
  # -------------------------------------
  # start table tag
  # -------------------------------------
  page.content <- "<table>"
  # -------------------------------------
  # check if we want to see header strings
  # -------------------------------------
  if (showHeaderStrings) {
    page.content <- paste0(page.content, sprintf("\n  <tr>\n    <td class=\"tdata topborder\" rowspan=\"2\"><em>%s</em></td>", stringPredictors))
    page.content <- paste0(page.content, sprintf("\n    <td colspan=\"%i\" class=\"tdata topborder depvarhead\"><em>%s</em></td>", headerColSpan, stringDependentVariables))
    page.content <- paste0(page.content, "\n  </tr>\n")
  }
  # -------------------------------------
  # table headline: label for dependent variables (model outcomes)
  # -------------------------------------
  page.content <- paste0(page.content, "<tr>")
  # -------------------------------------
  # If we don't show header strings, a rowspan-attribute is missing,
  # so we need to insert an empty cell here
  # -------------------------------------
  tcp <- ""
  if (!showHeaderStrings) {
    page.content <- paste0(page.content, "\n    <td class=\"tdata topborder\"></td>")
    tcp <- " topborder"
  }
  # -------------------------------------
  # continue with labels
  # -------------------------------------
  if (!is.null(labelDependentVariables)) {
    for (i in 1:length(labelDependentVariables)) {
      if (headerColSpanFactor>1) {
        page.content <- paste0(page.content, sprintf("\n    <td class=\"tdata centeralign labelcellborder%s\" colspan=\"%i\">%s</td>", tcp, headerColSpanFactor, labelDependentVariables[i]))
      }
      else {
        page.content <- paste0(page.content, sprintf("\n    <td class=\"tdata centeralign labelcellborder%s\">%s</td>", tcp, labelDependentVariables[i]))
      }
    }
    page.content <- paste0(page.content, "\n  </tr>")
  }
  else {
    for (i in 1:length(input_list)) {
      if (headerColSpanFactor>1) {
        page.content <- paste0(page.content, sprintf("\n    <td class=\"tdata centeralign labelcellborder%s\" colspan=\"%i\">%s %i</td>", tcp, headerColSpanFactor, stringModel, i))
      }
      else {
        page.content <- paste0(page.content, sprintf("\n    <td class=\"tdata centeralign labelcellborder%s\">%s %i</td>", tcp, stringModel, i))
      }
    }
    page.content <- paste0(page.content, "\n  </tr>")
  }
  # -------------------------------------
  # calculate coefficients and confidence intervalls
  # for all models
  # -------------------------------------
  coeffs <- c()
  confi_lower <- c()
  confi_higher <- c()
  pv <- c()
  se <- c()
  stdbv <- c()
  # -------------------------------------
  # retrieve data from fitted models
  # -------------------------------------
  for (i in 1:length(input_list)) {
    fit <- input_list[[i]]
    coeffs <- rbind(coeffs, coef(fit))
    confi_lower <- cbind(confi_lower, confint(fit)[,1])
    confi_higher <- cbind(confi_higher, confint(fit)[,2])
    pv <- cbind(pv, round(summary(fit)$coefficients[,4], digits.p))
    # standard error
    se <- cbind(se, round(summary(fit)$coefficients[,2], digits.se))
    # retrieve standardized betas
    stdbv <- cbind(stdbv, sprintf("%.*f", digits.sb, sjs.stdb(fit)))
  }
  # -------------------------------------
  # rotate coefficients
  # -------------------------------------
  coeffs <- t(coeffs)
  # -------------------------------------
  # set default predictor labels
  # -------------------------------------
  if (is.null(labelPredictors)) {
    fit <- input_list[[i]]
    labelPredictors <- c()
    # --------------------------------------------------------
    # auto-retrieve value labels
    # --------------------------------------------------------
    # iterate coefficients (1 is intercept or response)
    for (i in 2 : ncol(fit$model)) {
      # check if we hav label
      lab <- autoSetVariableLabels(fit$model[, i])
      # if not, use coefficient name
      if (is.null(lab)) {
        lab <- row.names(coeffs)[-1][i]
      }
      labelPredictors <- c(labelPredictors, lab)
    }
    # labelPredictors <- row.names(coeffs)[-1]
  }
  # --------------------------------------------------------
  # auto-retrieving variable labels does not work when we
  # have factors with different levels, which appear as 
  # "multiple predictors", but are only one variable
  # --------------------------------------------------------
  if (is.null(labelPredictors) || length(labelPredictors) < length(row.names(coeffs)[-1])) {
    labelPredictors <- row.names(coeffs)[-1]
  }
  # -------------------------------------
  # prepare p-values, either as * or as numbers
  # -------------------------------------
  if (!pvaluesAsNumbers) {
    pv <- apply(pv, c(1,2), function(x) {
      if (x>=0.05) x <- c("")
      else if (x>=0.01 && x<0.05) x <- c("*")
      else if (x>=0.001 && x<0.01) x <- c("**")
      else if (x<0.001) x <- c("***")
    })
  }
  else {
    pv <- apply(pv, c(1,2), function(x) {
      if (x <0.05 && boldpvalues) {
        x <- sprintf("<b>%.*f</b>", digits.p, x)      }
      else {
        x <- sprintf("%.*f", digits.p, x) 
      }
    })
  }
  # -------------------------------------
  # table header: or/ci and p-labels
  # -------------------------------------
  if (showAbbrHeadline) {
    page.content <- paste0(page.content, "\n  <tr>\n    <td class=\"tdata colnames\">&nbsp;</td>")
    colnr <- ifelse(is.null(labelDependentVariables), length(input_list), length(labelDependentVariables))
    for (i in 1:colnr) {
      # confidence interval in separate column
      if (separateConfColumn) {
        page.content <- paste0(page.content, sprintf("\n    <td class=\"tdata centeralign colnames\">%s</td>", stringB))
        if (showConfInt) page.content <- paste0(page.content, sprintf("<td class=\"tdata centeralign colnames\">%s</td>", stringCI))
      }
      else {
        # confidence interval in Beta-column
        page.content <- paste0(page.content, sprintf("\n    <td class=\"tdata centeralign colnames\">%s</td>", showCIString))
      }
      # show std. error
      if (showStdError) page.content <- paste0(page.content, sprintf("<td class=\"tdata centeralign colnames\">%s</td>", stringSE))
      # show std. beta
      if (showStdBeta) page.content <- paste0(page.content, sprintf("<td class=\"tdata centeralign colnames\">%s</td>", stringSB))
      # show p-values as numbers in separate column
      if (pvaluesAsNumbers) page.content <- paste0(page.content, sprintf("<td class=\"tdata centeralign colnames\">%s</td>", stringP))
    }
    page.content <- paste(page.content, "\n  </tr>\n")
  }
  # -------------------------------------
  # close table headline
  # -------------------------------------
  page.content <- paste0(page.content, "  <tr>\n")
  # -------------------------------------
  # 1. row: intercept
  # -------------------------------------
  page.content <- paste0(page.content, sprintf("    <td class=\"tdata leftalign topcontentborder\">%s</td>", stringIntercept))
  
  for (i in 1:ncol(coeffs)) {
    # confidence interval in separate column
    if (separateConfColumn) {
      # open table cell for Beta-coefficient
      page.content <- paste0(page.content, sprintf("\n    <td class=\"tdata centeralign topcontentborder\">%.*f", digits.est, coeffs[1,i]))
      # if p-values are not shown as numbers, insert them after beta-value
      if (!pvaluesAsNumbers) page.content <- paste0(page.content, sprintf(" %s", pv[1,i]))
      # if we have CI, start new table cell (CI in separate column)
      if (showConfInt) {
        page.content <- paste0(page.content, sprintf("</td><td class=\"tdata centeralign topcontentborder\">%.*f-%.*f</td>", digits.ci, confi_lower[1,i], digits.ci, confi_higher[1,i]))
      }
      else {
        page.content <- paste0(page.content, "</td>")
      }
    }
    else {
      # open table cell for Beta-coefficient
      page.content <- paste0(page.content, sprintf("\n    <td class=\"tdata centeralign topcontentborder\">%.*f", digits.est, coeffs[1,i]))
      # confidence interval in Beta-column
      if (showConfInt) page.content <- paste0(page.content, sprintf("%s(%.*f-%.*f)", linebreakstring, digits.ci, confi_lower[1,i], digits.ci, confi_higher[1,i]))
      # if p-values are not shown as numbers, insert them after beta-value
      if (!pvaluesAsNumbers) page.content <- paste0(page.content, sprintf(" %s", pv[1,i]))
      page.content <- paste0(page.content, "</td>")
    }
    # show std. error
    if (showStdError) page.content <- paste0(page.content, sprintf("<td class=\"tdata centeralign topcontentborder\">%.*f</td>", digits.se, se[1,i]))
    # show std. beta
    if (showStdBeta) page.content <- paste0(page.content, "<td class=\"tdata centeralign topcontentborder\"></td>")
    # show p-values as numbers in separate column
    if (pvaluesAsNumbers) page.content <- paste0(page.content, sprintf("<td class=\"tdata centeralign topcontentborder\">%s</td>", pv[1,i]))
  }
  page.content <- paste0(page.content, "\n  </tr>")  
  # -------------------------------------
  # subsequent rows: pedictors
  # -------------------------------------
  predlen <- length(labelPredictors)
  for (i in 1:predlen) {
    page.content <- paste0(page.content, "\n  <tr>\n", sprintf("    <td class=\"tdata leftalign\">%s</td>", labelPredictors[i]))
    for (j in 1:ncol(coeffs)) {
      # confidence interval in separate column
      if (separateConfColumn) {
        # open table cell for Beta-coefficient
        page.content <- paste0(page.content, sprintf("\n    <td class=\"tdata centeralign\">%.*f", digits.est, coeffs[i+1,j]))
        # if p-values are not shown as numbers, insert them after beta-value
        if (!pvaluesAsNumbers) page.content <- paste0(page.content, sprintf(" %s", pv[i+1,j]))
        # if we have CI, start new table cell (CI in separate column)
        if (showConfInt) {
          page.content <- paste0(page.content, sprintf("</td><td class=\"tdata centeralign\">%.*f-%.*f</td>", digits.ci, confi_lower[i+1,j], digits.ci, confi_higher[i+1,j]))
        }
        else {
          page.content <- paste0(page.content, "</td>")
        }
      }
      else {
        # open table cell for Beta-coefficient
        page.content <- paste0(page.content, sprintf("\n    <td class=\"tdata centeralign\">%.*f", digits.est, coeffs[i+1,j]))
        # confidence interval in Beta-column
        if (showConfInt) page.content <- paste0(page.content, sprintf("%s(%.*f-%.*f)", linebreakstring, digits.ci, confi_lower[i+1,j], digits.ci, confi_higher[i+1,j]))
        # if p-values are not shown as numbers, insert them after beta-value
        if (!pvaluesAsNumbers) page.content <- paste0(page.content, sprintf(" %s", pv[i+1,j]))
        page.content <- paste0(page.content, "</td>")
      }
      # show std. error
      if (showStdError) page.content <- paste0(page.content, sprintf("<td class=\"tdata centeralign\">%.*f</td>", digits.se, se[i+1,j]))
      # show std. beta
      if (showStdBeta) page.content <- paste0(page.content, sprintf("<td class=\"tdata centeralign\">%s</td>", stdbv[i,j]))
      # show p-values as numbers in separate column
      if (pvaluesAsNumbers) page.content <- paste0(page.content, sprintf("<td class=\"tdata centeralign\">%s</td>", pv[i+1,j]))
    }
    page.content <- paste0(page.content, "\n  </tr>")
  }
  # -------------------------------------
  # Model-Summary: N
  # -------------------------------------
  if (headerColSpanFactor>1) {
    colspanstring <- sprintf("<td class=\"tdata centeralign summary\" colspan=\"%i\">", headerColSpanFactor)
    colspanstringfirstrow <- sprintf("<td class=\"tdata summary centeralign firstsumrow\" colspan=\"%i\">", headerColSpanFactor)
  }
  else {
    colspanstring <- c("<td class=\"tdata centeralign summary\">")
    colspanstringfirstrow <- c("<td class=\"tdata summary centeralign firstsumrow\">")
  }
  page.content <- paste0(page.content, sprintf("\n  <tr>\n    <td class=\"tdata summary leftalign firstsumrow\">%s</td>\n", stringObservations))
  for (i in 1:length(input_list)) {
    page.content <- paste(page.content, sprintf("   %s%i</td>\n", colspanstringfirstrow, nobs(input_list[[i]])))
  }
  page.content <- paste0(page.content, "  </tr>\n")
  # -------------------------------------
  # Model-Summary: r2 and sdj. r2
  # -------------------------------------
  if (showR2) {
    page.content <- paste0(page.content, "  <tr>\n    <td class=\"tdata leftalign summary\">R<sup>2</sup> / adj. R<sup>2</sup></td>\n")
    for (i in 1:length(input_list)) {
      rsqu <- summary(input_list[[i]])$r.squared
      adjrsqu <- summary(input_list[[i]])$adj.r.squared
      page.content <- paste0(page.content, sprintf("    %s%.*f / %.*f</td>\n", colspanstring, digits.summary, rsqu, digits.summary, adjrsqu))
    }
    page.content <- paste(page.content, "  </tr>\n")
  }
  # -------------------------------------
  # Model-Summary: F-statistics
  # -------------------------------------
  if (showFStat) {
    page.content <- paste(page.content, "  <tr>\n     <td class=\"tdata leftalign summary\">F-statistics</td>\n")
    for (i in 1:length(input_list)) {
      fstat <- summary(input_list[[i]])$fstatistic
      # Calculate p-value for F-test
      pval <- pf(fstat[1], fstat[2], fstat[3],lower.tail = FALSE)
      # indicate significance level by stars
      pan <- c("")
      if (pval<=0.001) {
        pan <- c("***")
      }
      else  if (pval<=0.01) {
        pan <- c("**")
      }
      else  if (pval<=0.05) {
        pan <- c("*")
      }
      page.content <- paste(page.content, sprintf("    %s%.*f%s</td>\n", colspanstring, digits.summary, fstat[1], pan))
    }
    page.content <- paste(page.content, "  </tr>\n")
  }
  # -------------------------------------
  # Model-Summary: AIC
  # -------------------------------------
  if (showAIC) {
    page.content <- paste(page.content, "  <tr>\n     <td class=\"tdata leftalign summary\">AIC</td>\n")
    for (i in 1:length(input_list)) {
      page.content <- paste(page.content, sprintf("    %s%.*f</td>\n", colspanstring, digits.summary, AIC(input_list[[i]])))
    }
    page.content <- paste0(page.content, "  </tr>\n")
  }
  # -------------------------------------
  # table footnote
  # -------------------------------------
  if (!pvaluesAsNumbers) page.content <- paste(page.content, sprintf("  <tr class=\"tdata annorow\">\n    <td class=\"tdata\">Notes</td><td class=\"tdata annostyle\" colspan=\"%i\"><em>* p&lt;0.05&nbsp;&nbsp;&nbsp;** p&lt;0.01&nbsp;&nbsp;&nbsp;*** p&lt;0.001</em></td>\n  </tr>\n", headerColSpan), sep="")
  page.content <- paste0(page.content, "</table>\n")
  # -------------------------------------
  # finish table
  # -------------------------------------
  toWrite <- paste0(toWrite, page.content)
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
  knitr <- gsub("class=", "style=", knitr)
  knitr <- gsub("<table", sprintf("<table style=\"%s\"", css.table), knitr)
  # -------------------------------------
  # replace class-attributes with inline-style-definitions
  # -------------------------------------
  knitr <- gsub(tag.tdata, css.tdata, knitr)
  knitr <- gsub(tag.thead, css.thead, knitr)
  knitr <- gsub(tag.summary, css.summary, knitr)  
  knitr <- gsub(tag.colnames, css.colnames, knitr)
  knitr <- gsub(tag.leftalign, css.leftalign, knitr)
  knitr <- gsub(tag.centeralign, css.centeralign, knitr)
  knitr <- gsub(tag.firstsumrow, css.firstsumrow, knitr)
  knitr <- gsub(tag.lasttablerow, css.lasttablerow, knitr)  
  knitr <- gsub(tag.labelcellborder, css.labelcellborder, knitr)  
  knitr <- gsub(tag.topborder, css.topborder, knitr)  
  knitr <- gsub(tag.depvarhead, css.depvarhead, knitr)  
  knitr <- gsub(tag.topcontentborder, css.topcontentborder, knitr)  
  knitr <- gsub(tag.noannorow, css.noannorow, knitr)
  knitr <- gsub(tag.annorow, css.annorow, knitr)  
  knitr <- gsub(tag.annostyle, css.annostyle, knitr)  
  # -------------------------------------
  # check if html-content should be outputted
  # -------------------------------------
  out.html.table(no.output, file, knitr, toWrite, useViewer)  
  # -------------------------------------
  # return results
  # -------------------------------------
  invisible (structure(class = "sjtlm",
                       list(page.style = page.style,
                            page.content = page.content,
                            output.complete = toWrite,
                            knitr = knitr)))
}



# @title Show linear regression with mediator variable as HTML table
# @name sjt.mlm
# 
# @description Shows fitted linear models with mediator effects
#                as HTML table, or saves them as file.
#                
# @seealso \code{\link{sjt.glm}} \cr
#          \code{\link{sjp.lm}} \cr
#          \code{\link{sjp.lm1}} \cr
#          \code{\link{sjt.lm}}
#        
# @export
# sjt.mlm <- function(iv, dv, mv, controls=NULL, data) {
#   col.iv <- which(colnames(data)==iv)
#   col.dv <- which(colnames(data)==dv)
#   col.mv <- which(colnames(data)==mv)
#   fit <- lm(data[,col.iv]~data[,col.dv]+data[,col.mv], data, na.action="na.omit")
#   fit
# }

# sjt.mlm("sgb_all", "sf12_ps", "fab3er_1r", data=kn)
