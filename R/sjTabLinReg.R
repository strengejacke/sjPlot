# bind global variables
utils::globalVariables(c("starts_with"))


#' @title Summary of linear regression as HTML table
#' @name sjt.lm
#' 
#' @description Summarizes (multiple) fitted linear models (coefficients, std. beta values etc.)
#'                as HTML table, or saves them as file. The fitted models may have different predictors,
#'                e.g. when comparing different stepwise fitted models.
#'                
#' @seealso \href{http://strengejacke.de/sjPlot/sjt.lm/}{sjPlot manual: sjt.lm}
#' 
#' @param ... one or more fitted linear (mixed) models.
#' @param labelPredictors character vector with labels of predictor variables.
#'          If not \code{NULL}, \code{labelPredictors} will be used in the first
#'          table column with the predictors' names. See 'Examples'.
#' @param labelDependentVariables character vector with labels of dependent 
#'          variables of all fitted models. See 'Examples'.
#' @param stringPredictors string constant used as headline for the predictor column.
#'          Default is \code{"Predictors"}.
#' @param stringDependentVariables string constant used as headline for the 
#'          dependent variable columns. Default is \code{"Dependent Variables"}.
#' @param showHeaderStrings logical, if \code{TRUE}, the header strings \code{stringPredictors}
#'          and \code{stringDependentVariables} are shown. By default, they're hidden.
#' @param stringModel string constant used as headline for the model names in case no 
#'          labels for the dependent variables are provided (see \code{labelDependentVariables}).
#'          Default is \code{"Model"}.
#' @param stringIntercept string constant used as headline for the Intercept row.
#'          Default is \code{"Intercept"}.
#' @param stringObservations string constant used in the summary row for the count of observation
#'          (cases). Default is \code{"Observations"}.
#' @param stringB string used for the column heading of estimates. Default is \code{"B"}.
#' @param stringSB string used for the column heading of standardized beta coefficients. Default is \code{"std. Beta"}.
#' @param stringCI string used for the column heading of confidence interval values. Default is \code{"CI"}.
#' @param stringSE string used for the column heading of standard error values. Default is \code{"std. Error"}.
#' @param stringP string used for the column heading of p values. Default is \code{"p"}.
#' @param showEst logical, if \code{TRUE} (default), the estimates are printed.
#' @param showConfInt logical, if \code{TRUE} (default), the confidence intervall is also printed to the table. Use
#'          \code{FALSE} to omit the CI in the table.
#' @param showStdBeta indicates whether standardized beta-coefficients should 
#'          also printed, and if yes, which type of standardization is done.
#'          See 'Details'.
#' @param showStdError logical, if \code{TRUE}, the standard errors are also printed.
#'          Default is \code{FALSE}.
#' @param ci.hyphen string, indicating the hyphen for confidence interval range.
#'          May be an HTML entity. See 'Examples'.
#' @param minus.sign string, indicating the minus sign for negative numbers.
#'          May be an HTML entity. See 'Examples'.
#' @param digits.est amount of decimals for estimates
#' @param digits.p amount of decimals for p-values
#' @param digits.ci amount of decimals for confidence intervals
#' @param digits.se amount of decimals for standard error
#' @param digits.sb amount of decimals for standardized beta
#' @param digits.summary amount of decimals for values in model summary
#' @param pvaluesAsNumbers logical, if \code{TRUE}, p-values are shown as numbers. If \code{FALSE} (default),
#'          p-values are indicated by asterisks.
#' @param boldpvalues logical, if \code{TRUE} (default), significant p-values are shown bold faced.
#' @param separateConfColumn if \code{TRUE}, the CI values are shown in a separate table column.
#'          Default is \code{FALSE}.
#' @param newLineConf logical, if \code{TRUE} and \code{separateConfColumn = FALSE}, inserts a line break
#'          between B and CI values. If \code{FALSE}, CI values are printed in the same
#'          line as B values.
#' @param group.pred logical, if \code{TRUE} (default), automatically groups table rows with 
#'          factor levels of same factor, i.e. predictors of type \code{\link{factor}} will
#'          be grouped, if the factor has more than two levels. Grouping means that a separate headline
#'          row is inserted to the table just before the predictor values.
#' @param showAbbrHeadline logical, if \code{TRUE} (default), the table data columns have a headline with 
#'          abbreviations for estimates and std. beta-values, confidence interval and p-values.
#' @param showR2 logical, if \code{TRUE} (default), the R2 and adjusted R2 values for each model are printed
#'          in the model summary.
#' @param showFStat If \code{TRUE}, the F-statistics for each model is printed
#'          in the model summary. Default is \code{FALSE}.
#' @param showAIC logical, if \code{TRUE}, the AIC value for each model is printed
#'          in the model summary. Default is \code{FALSE}.
#' @param showAICc logical, if \code{TRUE}, the second-order AIC value for each model 
#'          is printed in the model summary. Default is \code{FALSE}.
#' @param remove.estimates numeric vector with indices (order equals to row index of \code{coef(fit)}) 
#'          or character vector with coefficient names that indicate which estimates should be removed
#'          from the table output. The first estimate is the intercept, followed by the model predictors.
#'          \emph{The intercept cannot be removed from the table output!} \code{remove.estimates = c(2:4)} 
#'          would remove the 2nd to the 4th estimate (1st to 3rd predictor after intercept) from the output. 
#'          \code{remove.estimates = "est_name"} would remove the estimate \emph{est_name}. Default 
#'          is \code{NULL}, i.e. all estimates are printed.
#' @param cellSpacing numeric, inner padding of table cells. By default, this value is 0.2 (unit is cm), which is
#'          suitable for viewing the table. Decrease this value (0.05 to 0.1) if you want to import the table
#'          into Office documents. This is a convenient argument for the \code{CSS} argument for changing
#'          cell spacing, which would be: \code{CSS = list(css.thead = "padding:0.2cm;", css.tdata = "padding:0.2cm;")}.
#' @param cellGroupIndent indent for table rows with grouped factor predictors. Only applies
#'          if \code{group.pred = TRUE}.
#'          
#' @inheritParams sjt.frq
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
#' @note See 'Note' in \code{\link{sjt.frq}}.
#'  
#' @details Concerning the \code{showStdBeta} argument, \code{showStdBeta = "std"}
#'            will print normal standardized estimates. \code{showStdBeta = "std2"},
#'            however, standardization of estimates follows 
#'            \href{http://www.stat.columbia.edu/~gelman/research/published/standardizing7.pdf}{Gelman's (2008)}
#'            suggestion, rescaling the estimates by dividing them by two standard 
#'            deviations instead of just one. Resulting coefficients are then 
#'            directly comparable for untransformed binary predictors. This type 
#'            of standardization uses the \code{\link[arm]{standardize}}-function.
#'            For backward compatibility reasons, \code{showStdBeta} also may be 
#'            a logical value; if \code{TRUE}, normal standardized estimates are 
#'            printed (same effect as \code{showStdBeta = "std"}). Use 
#'            \code{showStdBeta = NULL} (default) or \code{showStdBeta = FALSE},
#'            if standardized estimats should not be printed.
#'            \cr \cr
#'            Furthermore, see 'Details' in \code{\link{sjt.frq}}.
#' 
#' @examples
#' \dontrun{
#' # Now fit the models. Note that both models share the same predictors
#' # and only differ in their dependent variable. See examples of stepwise
#' # models below at the end.
#' library(sjmisc)
#' data(efc)
#' 
#' # fit first model
#' fit1 <- lm(barthtot ~ c160age + c12hour + c161sex + c172code, data = efc)
#' # fit second model
#' fit2 <- lm(neg_c_7 ~ c160age + c12hour + c161sex + c172code, data = efc)
#' 
#' # create and open HTML-table in RStudio Viewer Pane or web browser
#' # note that we don't need to specify labels for the predictors,
#' # because these are automatically read
#' sjt.lm(fit1, fit2)
#' 
#' # create and open HTML-table in RStudio Viewer Pane or web browser
#' # in the following examples, we set labels via argument
#' sjt.lm(fit1, 
#'        fit2, 
#'        labelDependentVariables = c("Barthel-Index",
#'                                    "Negative Impact"),
#'        labelPredictors = c("Carer's Age", 
#'                            "Hours of Care", 
#'                            "Carer's Sex", 
#'                            "Educational Status"))
#' 
#' # show HTML-table, indicating p-values as asterisks
#' sjt.lm(fit1, 
#'        fit2, 
#'        labelDependentVariables = c("Barthel-Index", 
#'                                    "Negative Impact"),
#'        labelPredictors = c("Carer's Age", 
#'                            "Hours of Care", 
#'                            "Carer's Sex", 
#'                            "Educational Status"),
#'        showStdBeta = TRUE, 
#'        pvaluesAsNumbers = FALSE)
#' 
#' # create and open HTML-table in RStudio Viewer Pane or web browser,
#' # integrate CI in estimate column
#' sjt.lm(fit1, 
#'        fit2, 
#'        labelDependentVariables = c("Barthel-Index", 
#'                                    "Negative Impact"),
#'        labelPredictors = c("Carer's Age", 
#'                            "Hours of Care", 
#'                            "Carer's Sex", 
#'                            "Educational Status"),
#'        separateConfColumn = FALSE)
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
#'        showStdBeta = TRUE)
#' 
#' # show HTML-table, indicating p-values as stars
#' # and integrate CI in estimate column
#' sjt.lm(fit1, 
#'        fit2, 
#'        labelDependentVariables = c("Barthel-Index", 
#'                                    "Negative Impact"),
#'        labelPredictors = c("Carer's Age", 
#'                            "Hours of Care", 
#'                            "Carer's Sex", 
#'                            "Educational Status"),
#'        showStdBeta = TRUE, 
#'        ci.hyphen = " to ",
#'        minus.sign = "&minus;",
#'        pvaluesAsNumbers = FALSE, 
#'        separateConfColumn = FALSE)
#' 
#' # ---------------------------------- 
#' # connecting two html-tables
#' # ---------------------------------- 
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
#' # ---------------------------------- 
#' # User defined style sheet
#' # ---------------------------------- 
#' sjt.lm(fit1, 
#'        fit2, 
#'        labelDependentVariables = c("Barthel-Index", "Negative Impact"),
#'        labelPredictors = c("Carer's Age", 
#'                            "Hours of Care", 
#'                            "Carer's Sex", 
#'                            "Educational Status"),
#'        CSS = list(css.table = "border: 2px solid;",
#'                   css.tdata = "border: 1px solid;",
#'                   css.depvarhead = "color:#003399;"))
#'                   
#' # ---------------------------------- 
#' # automatic grouping of predictors
#' # ---------------------------------- 
#' library(sjmisc)
#' data(efc)
#' 
#' # make education categorical
#' efc$c172code <- to_factor(efc$c172code)
#'     
#' # fit first model again (with c172code as factor)
#' fit1 <- lm(barthtot ~ c160age + c12hour + c172code + c161sex, data=efc)
#' # fit second model again (with c172code as factor)
#' fit2 <- lm(neg_c_7 ~ c160age + c12hour + c172code + c161sex, data=efc)
#' 
#' # plot models, but group by predictors
#' sjt.lm(fit1,
#'        fit2,
#'        group.pred = TRUE)
#'
#' # ---------------------------------------- 
#' # compare models with different predictors
#' # ---------------------------------------- 
#' library(sjmisc)
#' data(efc)
#' 
#' # make education categorical
#' efc$c172code <- to_factor(efc$c172code)
#' # make education categorical
#' efc$e42dep <- to_factor(efc$e42dep)
#' 
#' # fit first model
#' fit1 <- lm(neg_c_7 ~ c160age + c172code + c161sex, data = efc)
#' # fit second model
#' fit2 <- lm(neg_c_7 ~ c160age + c172code + c161sex + c12hour, data = efc)
#' # fit second model
#' fit3 <- lm(neg_c_7 ~ c160age + c172code + e42dep + tot_sc_e, data = efc)
#'
#' sjt.lm(fit1, fit2, fit3)
#'
#'
#' # ---------------------------------------- 
#' # compare models with different predictors
#' # and grouping
#' # ---------------------------------------- 
#' 
#' # make cope-index categorical
#' efc$c82cop1 <- to_fac(efc$c82cop1)
#' # fit another model
#' fit4 <- lm(neg_c_7 ~ c160age + c172code + e42dep + tot_sc_e + c82cop1, 
#'            data = efc)
#'
#' sjt.lm(fit1, fit2, fit4, fit3)
#' 
#' # show standardized beta only
#' sjt.lm(fit1, fit2, fit4, fit3,
#'        showEst = FALSE,
#'        showStdBeta = TRUE,
#'        showAIC = TRUE,
#'        showFStat = TRUE)
#'
#' # -----------------------------------------------------------
#' # color insanity. just to show that each column has an own
#' # CSS-tag, so - depending on the stats and values you show -
#' # you can define column spaces / margins, border etc. to
#' # visually separate your models in the table
#' # -----------------------------------------------------------
#' sjt.lm(fit1, fit2, fit4, fit3,
#'        showStdBeta = TRUE,
#'        showAIC = TRUE,
#'        showFStat = TRUE,
#'        showStdError = TRUE,
#'        CSS = list(css.modelcolumn1 = 'color:blue;',
#'                   css.modelcolumn2 = 'color:red;',
#'                   css.modelcolumn3 = 'color:green;',
#'                   css.modelcolumn4 = 'color:#ffff00;',
#'                   css.modelcolumn5 = 'color:#777777;',
#'                   css.modelcolumn6 = 'color:#3399cc;'))
#'
#' sjt.lm(fit1, fit2, fit4, fit3,
#'        showEst = FALSE,
#'        showStdBeta = TRUE,
#'        pvaluesAsNumbers = FALSE,
#'        group.pred = FALSE,
#'        CSS = list(css.modelcolumn4 = 'border-left:1px solid black;',
#'                   css.modelcolumn5 = 'padding-right:50px;'))}
#'                   
#' @importFrom dplyr full_join slice
#' @importFrom stats nobs AIC confint coef
#' @export
sjt.lm <- function(...,
                   file = NULL,
                   labelPredictors = NULL,
                   labelDependentVariables = NULL,
                   stringPredictors = "Predictors",
                   stringDependentVariables = "Dependent Variables",
                   stringModel = "Model",
                   showHeaderStrings = FALSE,
                   stringIntercept = "(Intercept)",
                   stringObservations = "Observations",
                   stringB = "B",
                   stringSB = "std. Beta",
                   stringCI = "CI",
                   stringSE = "std. Error",
                   stringP = "p",
                   showEst = TRUE,
                   showConfInt = TRUE,
                   showStdBeta = NULL,
                   showStdError = FALSE,
                   ci.hyphen = "&nbsp;&ndash;&nbsp;",
                   minus.sign = "&#45;",
                   digits.est = 2,
                   digits.p = 3,
                   digits.ci = 2,
                   digits.se = 2,
                   digits.sb = 2,
                   digits.summary = 3,
                   pvaluesAsNumbers = TRUE,
                   boldpvalues = TRUE,
                   separateConfColumn = TRUE,
                   newLineConf = TRUE,
                   group.pred = TRUE,
                   showAbbrHeadline = TRUE,
                   showR2 = TRUE,
                   showFStat = FALSE,
                   showAIC = FALSE,
                   showAICc = FALSE,
                   remove.estimates = NULL,
                   cellSpacing = 0.2,
                   cellGroupIndent = 0.6,
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
  
  # -------------------------------------
  # check arguments
  # -------------------------------------
  # check default for standardized beta valies
  if (is.null(showStdBeta) || showStdBeta == FALSE) 
    showStdBetaValues <- FALSE
  else
    showStdBetaValues <- TRUE
  # check if any estimates should be plotted?
  if (!showEst && !showStdBetaValues) {
    warning("Either estimates ('showEst') or standardized betas ('showStdBeta') must be 'TRUE' to show table. Setting 'showEst' to 'TRUE'.", call. = F)
    showEst <- TRUE
  }
  # check hyphen for ci-range
  if (is.null(ci.hyphen)) ci.hyphen <- "&nbsp;&ndash;&nbsp;"
  # replace space with protected space in ci-hyphen
  ci.hyphen <- gsub(" ", "&nbsp;", ci.hyphen, fixed = TRUE)
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
  tag.separatorcol <- "separatorcol"
  tag.summary <- "summary"
  tag.fixedparts <- "fixedparts"
  tag.randomparts <- "randomparts"
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
  tag.grouprow <- "grouprow"
  tag.tgrpdata <- "tgrpdata"
  tag.modelcolumn1 <- "modelcolumn1"
  tag.modelcolumn2 <- "modelcolumn2"
  tag.modelcolumn3 <- "modelcolumn3"
  tag.modelcolumn4 <- "modelcolumn4"
  tag.modelcolumn5 <- "modelcolumn5"
  tag.modelcolumn6 <- "modelcolumn6"
  css.table <- "border-collapse:collapse; border:none;"
  css.thead <- sprintf("border-bottom: 1px solid; padding:%.1fcm;", cellSpacing)
  css.tdata <- sprintf("padding:%.1fcm;", cellSpacing)
  css.separatorcol <- "padding-left:0.5em; padding-right:0.5em;"
  css.summary <- "padding-top:0.1cm; padding-bottom:0.1cm;"
  css.fixedparts <- "font-weight:bold; text-align:left;"
  css.randomparts <- "font-weight:bold; text-align:left; padding-top:0.5em;"
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
  css.grouprow <- sprintf("padding:%.1fcm;", cellSpacing)
  css.tgrpdata <- sprintf("font-style:italic; padding:%.1fcm; padding-left:%.1fcm;", cellSpacing, cellGroupIndent)
  css.modelcolumn1 <- ""
  css.modelcolumn2 <- ""
  css.modelcolumn3 <- ""
  css.modelcolumn4 <- ""
  css.modelcolumn5 <- ""
  css.modelcolumn6 <- ""
  # change table style if we have pvalues as numbers
  if (pvaluesAsNumbers) css.table <- sprintf("%s%s", css.table, css.noannorow)
  if (showHeaderStrings) css.labelcellborder <- ""
  # ------------------------
  # check user defined style sheets
  # ------------------------
  if (!is.null(CSS)) {
    if (!is.null(CSS[['css.table']])) css.table <- ifelse(substring(CSS[['css.table']], 1, 1) == '+', paste0(css.table, substring(CSS[['css.table']], 2)), CSS[['css.table']])
    if (!is.null(CSS[['css.thead']])) css.thead <- ifelse(substring(CSS[['css.thead']], 1, 1) == '+', paste0(css.thead, substring(CSS[['css.thead']], 2)), CSS[['css.thead']])
    if (!is.null(CSS[['css.tdata']])) css.tdata <- ifelse(substring(CSS[['css.tdata']], 1, 1) == '+', paste0(css.tdata, substring(CSS[['css.tdata']], 2)), CSS[['css.tdata']])
    if (!is.null(CSS[['css.separatorcol']])) css.separatorcol <- ifelse(substring(CSS[['css.separatorcol']], 1, 1) == '+', paste0(css.separatorcol, substring(CSS[['css.separatorcol']], 2)), CSS[['css.separatorcol']])
    if (!is.null(CSS[['css.leftalign']])) css.leftalign <- ifelse(substring(CSS[['css.leftalign']], 1, 1) == '+', paste0(css.leftalign, substring(CSS[['css.leftalign']], 2)), CSS[['css.leftalign']])
    if (!is.null(CSS[['css.centeralign']])) css.centeralign <- ifelse(substring(CSS[['css.centeralign']], 1, 1) == '+', paste0(css.centeralign, substring(CSS[['css.centeralign']], 2)), CSS[['css.centeralign']])
    if (!is.null(CSS[['css.summary']])) css.summary <- ifelse(substring(CSS[['css.summary']], 1, 1) == '+', paste0(css.summary, substring(CSS[['css.summary']], 2)), CSS[['css.summary']])
    if (!is.null(CSS[['css.fixedparts']])) css.fixedparts <- ifelse(substring(CSS[['css.fixedparts']], 1, 1) == '+', paste0(css.fixedparts, substring(CSS[['css.fixedparts']], 2)), CSS[['css.fixedparts']])
    if (!is.null(CSS[['css.randomparts']])) css.randomparts <- ifelse(substring(CSS[['css.randomparts']], 1, 1) == '+', paste0(css.randomparts, substring(CSS[['css.randomparts']], 2)), CSS[['css.randomparts']])
    if (!is.null(CSS[['css.lasttablerow']])) css.lasttablerow <- ifelse(substring(CSS[['css.lasttablerow']], 1, 1) == '+', paste0(css.lasttablerow, substring(CSS[['css.lasttablerow']], 2)), CSS[['css.lasttablerow']])
    if (!is.null(CSS[['css.labelcellborder']])) css.labelcellborder <- ifelse(substring(CSS[['css.labelcellborder']], 1, 1) == '+', paste0(css.table, substring(CSS[['css.labelcellborder']], 2)), CSS[['css.labelcellborder']])
    if (!is.null(CSS[['css.colnames']])) css.colnames <- ifelse(substring(CSS[['css.colnames']], 1, 1) == '+', paste0(css.colnames, substring(CSS[['css.colnames']], 2)), CSS[['css.colnames']])
    if (!is.null(CSS[['css.firstsumrow']])) css.firstsumrow <- ifelse(substring(CSS[['css.firstsumrow']], 1, 1) == '+', paste0(css.firstsumrow, substring(CSS[['css.firstsumrow']], 2)), CSS[['css.firstsumrow']])
    if (!is.null(CSS[['css.topborder']])) css.topborder <- ifelse(substring(CSS[['css.topborder']], 1, 1) == '+', paste0(css.topborder, substring(CSS[['css.topborder']], 2)), CSS[['css.topborder']])
    if (!is.null(CSS[['css.depvarhead']])) css.depvarhead <- ifelse(substring(CSS[['css.depvarhead']], 1, 1) == '+', paste0(css.depvarhead, substring(CSS[['css.depvarhead']], 2)), CSS[['css.depvarhead']])
    if (!is.null(CSS[['css.topcontentborder']])) css.topcontentborder <- ifelse(substring(CSS[['css.topcontentborder']], 1, 1) == '+', paste0(css.topcontentborder, substring(CSS[['css.topcontentborder']], 2)), CSS[['css.topcontentborder']])
    if (!is.null(CSS[['css.annorow']])) css.annorow <- ifelse(substring(CSS[['css.annorow']], 1, 1) == '+', paste0(css.annorow, substring(CSS[['css.annorow']], 2)), CSS[['css.annorow']])
    if (!is.null(CSS[['css.noannorow']])) css.noannorow <- ifelse(substring(CSS[['css.noannorow']], 1, 1) == '+', paste0(css.noannorow, substring(CSS[['css.noannorow']], 2)), CSS[['css.noannorow']])
    if (!is.null(CSS[['css.annostyle']])) css.annostyle <- ifelse(substring(CSS[['css.annostyle']], 1, 1) == '+', paste0(css.annostyle, substring(CSS[['css.annostyle']], 2)), CSS[['css.annostyle']])
    if (!is.null(CSS[['css.grouprow']])) css.grouprow <- ifelse(substring(CSS[['css.grouprow']], 1, 1) == '+', paste0(css.grouprow, substring(CSS[['css.grouprow']], 2)), CSS[['css.grouprow']])
    if (!is.null(CSS[['css.tgrpdata']])) css.tgrpdata <- ifelse(substring(CSS[['css.tgrpdata']], 1, 1) == '+', paste0(css.tgrpdata, substring(CSS[['css.tgrpdata']], 2)), CSS[['css.tgrpdata']])
    if (!is.null(CSS[['css.modelcolumn1']])) css.modelcolumn1 <- ifelse(substring(CSS[['css.modelcolumn1']], 1, 1) == '+', paste0(css.modelcolumn1, substring(CSS[['css.modelcolumn1']], 2)), CSS[['css.modelcolumn1']])
    if (!is.null(CSS[['css.modelcolumn2']])) css.modelcolumn2 <- ifelse(substring(CSS[['css.modelcolumn2']], 1, 1) == '+', paste0(css.modelcolumn2, substring(CSS[['css.modelcolumn2']], 2)), CSS[['css.modelcolumn2']])
    if (!is.null(CSS[['css.modelcolumn3']])) css.modelcolumn3 <- ifelse(substring(CSS[['css.modelcolumn3']], 1, 1) == '+', paste0(css.modelcolumn3, substring(CSS[['css.modelcolumn3']], 2)), CSS[['css.modelcolumn3']])
    if (!is.null(CSS[['css.modelcolumn4']])) css.modelcolumn4 <- ifelse(substring(CSS[['css.modelcolumn4']], 1, 1) == '+', paste0(css.modelcolumn4, substring(CSS[['css.modelcolumn4']], 2)), CSS[['css.modelcolumn4']])
    if (!is.null(CSS[['css.modelcolumn5']])) css.modelcolumn5 <- ifelse(substring(CSS[['css.modelcolumn5']], 1, 1) == '+', paste0(css.modelcolumn5, substring(CSS[['css.modelcolumn5']], 2)), CSS[['css.modelcolumn5']])
    if (!is.null(CSS[['css.modelcolumn6']])) css.modelcolumn6 <- ifelse(substring(CSS[['css.modelcolumn6']], 1, 1) == '+', paste0(css.modelcolumn6, substring(CSS[['css.modelcolumn6']], 2)), CSS[['css.modelcolumn6']])
  }
  # ------------------------
  # set page style
  # ------------------------
  page.style <-  sprintf("<style>%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n</style>",
                         tag.table, css.table, tag.thead, css.thead, tag.tdata, css.tdata,
                         tag.summary, css.summary, tag.colnames, css.colnames,
                         tag.firstsumrow, css.firstsumrow, tag.lasttablerow, css.lasttablerow,
                         tag.topborder, css.topborder, tag.depvarhead, css.depvarhead,
                         tag.topcontentborder, css.topcontentborder, tag.annorow, css.annorow, 
                         tag.noannorow, css.noannorow, tag.annostyle, css.annostyle,
                         tag.labelcellborder, css.labelcellborder,
                         tag.centeralign, css.centeralign, tag.leftalign, css.leftalign,
                         tag.grouprow, css.grouprow, tag.tgrpdata, css.tgrpdata,
                         tag.modelcolumn1, css.modelcolumn1,
                         tag.modelcolumn2, css.modelcolumn2,
                         tag.modelcolumn3, css.modelcolumn3,
                         tag.modelcolumn4, css.modelcolumn4,
                         tag.modelcolumn5, css.modelcolumn5,
                         tag.modelcolumn6, css.modelcolumn6,
                         tag.fixedparts, css.fixedparts,
                         tag.randomparts, css.randomparts,
                         tag.separatorcol, css.separatorcol)
  # ------------------------
  # start content
  # ------------------------
  toWrite <- paste0(toWrite, page.style)
  toWrite <- paste0(toWrite, "\n</head>\n<body>\n")
  # ------------------------
  # retrieve fitted models
  # ------------------------
  input_list <- list(...)
  # --------------------------------------------------------
  # check length. if we have a list of fitted model, 
  # we need to "unlist" them
  # --------------------------------------------------------
  if (class(input_list[[1]]) == "list") input_list <- lapply(input_list[[1]], function(x) x)
  # -----------------------------------------------------------
  # check argument. No model-summary supported for plm-objects
  # -----------------------------------------------------------
  if (any(class(input_list[[1]]) == "plm")) {
    # -----------------------------------------------------------
    # check package availability if fit is plm-object
    # -----------------------------------------------------------
    if (!"package:plm" %in% search()) {
      stop("Package 'plm' needs to be loaded for this function to work... Use 'library(plm)' and call this function again.", call. = FALSE)
    }
  }
  # ------------------------
  # do we have mixed models?
  # ------------------------
  lmerob <- any(class(input_list[[1]]) == "lmerMod") || any(class(input_list[[1]]) == "merModLmerTest")
  if (lmerob && !requireNamespace("lme4", quietly = TRUE)) {
    stop("Package 'lme4' needed for this function to work. Please install it.", call. = FALSE)
  }
  # ------------------------
  # should AICc be computed? Check for package
  # ------------------------
  if (showAICc && !requireNamespace("AICcmodavg", quietly = TRUE)) {
    warning("Package 'AICcmodavg' needed to show AICc. Argument 'showAICc' will be ignored.", call. = FALSE)
    showAICc <- FALSE
  }
  
  
  # ------------------------
  # check for stepwise models, when fitted models
  # are mixed effects models
  # ------------------------
  if (lmerob) {
    # "showICC" is not used in this function, so we
    # use "showR2" instead of showICC when calling this function
    # with mixed models.
    showICC <- showR2
    showR2 <- FALSE
    # check if we have different amount of coefficients
    # in fitted models - if yes, we have e.g. stepwise models
    sw.fit <- length(unique(sapply(input_list, function(x) length(lme4::fixef(x))))) > 1
    # if all fitted models have same amount of coefficients, check
    # whether all coefficients have same name. if not, we have models
    # with different predictors (e.g. stepwise comparison)
    if (sw.fit == FALSE) {
      all.coefs <- sapply(input_list, function(x) sort(names(lme4::fixef(x))))
      sw.fit <- any(apply(all.coefs, 1, function(x) length(unique(x))) > 1)
    }
  }
  # ------------------------
  # check for stepwise models, when fitted models
  # are simple linear models
  # ------------------------
  else {
    showICC <- FALSE
    # check if we have different amount of coefficients
    # in fitted models - if yes, we have e.g. stepwise models
    sw.fit <- length(unique(sapply(input_list, function(x) length(coef(x))))) > 1
    # if all fitted models have same amount of coefficients, check
    # whether all coefficients have same name. if not, we have models
    # with different predictors (e.g. stepwise comparison)
    if (sw.fit == FALSE) {
      all.coefs <- sapply(input_list, function(x) sort(names(coef(x))))
      sw.fit <- any(apply(all.coefs, 1, function(x) length(unique(x))) > 1)
    }
  }
  # -------------------------------------
  # prepare content, i.e. retrieve all
  # statistics from all fitted models
  # -------------------------------------
  df.fit <- list()
  # -------------------------------------
  # iterate all models
  # -------------------------------------
  for (i in 1:length(input_list)) {
    # -------------------------------------
    # retrieve model
    # -------------------------------------
    fit <- input_list[[i]]
    # -------------------------------------
    # retrieve ci for model
    # -------------------------------------
    if (lmerob) {
      # get cleaned CI
      confis <- get_cleaned_ciMerMod(fit, "lm", T)
      sbmer <- suppressWarnings(sjmisc::std_beta(fit)[-1, ])
      sbvals <- data.frame(beta = sbmer[, 1], 
                           ci.low = sbmer[, 1] - 1.96 * sbmer[, 2],
                           ci.hi = sbmer[, 1] + 1.96 * sbmer[, 2],
                           se = sbmer[, 2])
      coef.fit <- lme4::fixef(fit)
    } else {
      confis <- stats::confint(fit)
      if (!is.null(showStdBeta) && showStdBeta == "std2") 
        sbvals <- suppressWarnings(sjmisc::std_beta(fit, include.ci = T, type = "std2"))
      else
        sbvals <- suppressWarnings(sjmisc::std_beta(fit, include.ci = T))
      coef.fit <- stats::coef(fit)
    }
    # -------------------------------------
    # write data to data frame. we need names of
    # coefficients, estimated values, ci,
    # std. beta and p-values
    # -------------------------------------
    fit.df <- data.frame(names(coef.fit))
    fit.df$coeffs <- sprintf("%.*f", digits.est, coef.fit)
    fit.df$confi_lower <- sprintf("%.*f", digits.ci, confis[, 1])
    fit.df$confi_higher <- sprintf("%.*f", digits.ci, confis[, 2])
    # -------------------------------------
    # extracting p-values and se differs between
    # lmer and lm
    # -------------------------------------
    if (lmerob) {
      # p-values
      fit.df$pv <- round(get_lmerMod_pvalues(fit), digits.p)
      # standard error
      fit.df$se <- sprintf("%.*f", digits.se, coef(summary(fit))[, "Std. Error"])
    } else {
      # p-values
      fit.df$pv <- round(summary(fit)$coefficients[, 4], digits.p)
      # standard error
      fit.df$se <- sprintf("%.*f", digits.se, summary(fit)$coefficients[, 2])
    }
    # retrieve standardized betas and CI
    fit.df$stdbv <- c("", sprintf("%.*f", digits.sb, sbvals[, 1]))
    fit.df$stdbvci_lower <- c("", sprintf("%.*f", digits.ci, sbvals[, 2]))
    fit.df$stdbvci_higher <- c("", sprintf("%.*f", digits.ci, sbvals[, 3]))
    # -------------------------------------
    # prepare p-values, either as * or as numbers
    # -------------------------------------
    if (!pvaluesAsNumbers) {
      fit.df$pv <- sapply(fit.df$pv, function(x) x <- get_p_stars(x))
    } else {
      if (boldpvalues) {
        sb1 <- "<b>"
        sb2 <- "</b>"
      } else {
        sb1 <- sb2 <- ""
      }
      fit.df$pv <- sapply(fit.df$pv, function(x) {
        if (x < 0.05) {
          if (x < 0.001 && digits.p <= 3) {
            x <- sprintf("%s&lt;0.001%s", sb1, sb2)
          } else {
            x <- sprintf("%s%.*f%s", sb1, digits.p, x, sb2)
          }
        } else {
          x <- sprintf("%.*f", digits.p, x) 
        }
        # remove leading zero, APA style for p-value
        x <- sub("0", p_zero, x, fixed = TRUE)
      })
    }
    # -------------------------------------
    # set column names. we need the same name
    # for first column witrh coefficient names
    # and different column names for all model-statistics.
    # with this structure, we can join data frame with dplyr
    # in case we have fitted model with different predictors.
    # -------------------------------------
    colnames(fit.df) <- c("coef.name",
                          sprintf("estimate%i", i),
                          sprintf("ci.lo%i", i),
                          sprintf("ci.hi%i", i),
                          sprintf("p-value%i", i),
                          sprintf("se%i", i),
                          sprintf("std.beta%i", i),
                          sprintf("std.beta.ci.lo%i", i),
                          sprintf("std.beta.ci.hi%i", i))
    # -------------------------------------
    # add to df list
    # -------------------------------------
    df.fit[[length(df.fit) + 1]] <- fit.df
  }
  # -------------------------------------
  # join all data frame, i.e. "merge" all
  # model predictors into a single data frame
  # -------------------------------------
  joined.df <- df.fit[[1]]
  if (length(df.fit) > 1) {
    for (i in 2:length(df.fit)) {
      joined.df <- suppressWarnings(dplyr::full_join(joined.df, df.fit[[i]], "coef.name"))
    }
  }
  # -------------------------------------
  # replace NA, created by join, with empty string
  # -------------------------------------
  for (i in 1:ncol(joined.df)) {
    joined.df[, i] <- sapply(joined.df[, i], function(x) if (is.na(x)) x <- "" else x)
  }
  # -------------------------------------
  # remove estimates?
  # -------------------------------------
  keep.estimates <- NULL
  if (!is.null(remove.estimates)) {
    # do we have variable names instead of index numbers?
    if (!is.numeric(remove.estimates)) {
      # if so, retrieve index numbers
      tmp_re <- c()
      # iterate all var names
      for (re in 1:length(remove.estimates)) {
        # find row index by name
        tmp_re <- c(tmp_re, which(joined.df$coef.name == remove.estimates[re]))
      }
      # copy row numbers back
      remove.estimates <- tmp_re
    }
    # remove double indices and sort remaining indices
    remove.estimates <- sort(unique(remove.estimates))
    # check if intercept is in remove index, because intercept cannot be removed
    if (any(remove.estimates == 1)) {
      # remove intercept index
      remove.estimates <- remove.estimates[-which(remove.estimates == 1)]
      message("Intercept cannot be removed from table output. However, you may fake with style sheet, e.g. CSS = list(css.topcontentborder = \"+font-size: 0px;\").")
    }
    # create all row indices
    rowind <- c(1:nrow(joined.df))
    # "inverse" removable inices
    keep.estimates <- rowind[-remove.estimates]
    # select rows
    joined.df <- dplyr::slice(joined.df, keep.estimates)
  }
  # -------------------------------------
  # if confidence interval should be omitted,
  # don't use separate column for CI!
  # -------------------------------------
  if (!showConfInt) {
    separateConfColumn <- FALSE
    showCIString <- stringB
    showCIStringSB <- stringSB
  } else {
    showCIString <- sprintf("%s (%s)", stringB, stringCI)
    showCIStringSB <- sprintf("%s (%s)", stringSB, stringCI)
  }
  # -------------------------------------
  # table headline
  # -------------------------------------
  # headerColSpan indicates the column span over all model table columns,
  # i.e. the sum of all columns for each model
  headerColSpan <- length(input_list)
  # headerColSpanFactor indicates the column span per model,
  # i.e. the amount of table columns that are needed for each model
  # (B, p, CI, se...)
  headerColSpanFactor <- 1
  if (!showEst) headerColSpanFactor <- 0
  if (!showEst && separateConfColumn) headerColSpanFactor <- -1
  if (pvaluesAsNumbers) headerColSpanFactor <- headerColSpanFactor + 1
  if (separateConfColumn) headerColSpanFactor <- headerColSpanFactor + 1
  if (showStdBetaValues) headerColSpanFactor <- headerColSpanFactor + 1
  if (showStdBetaValues && separateConfColumn) headerColSpanFactor <- headerColSpanFactor + 1
  if (showStdError) headerColSpanFactor <- headerColSpanFactor + 1
  # now that we know how many columns each model needs,
  # we multiply columns per model with count of models, so we have
  # the column span over all models together; furthermore, we add
  # count of models to the overall column span, because
  # each model is separated with an empty table column
  headerColSpan <- headerColSpanFactor * headerColSpan + length(input_list)
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
    page.content <- paste0(page.content, "\n    <td class=\"tdata topborder\">&nbsp;</td>")
    tcp <- " topborder"
  }
  # -------------------------------------
  # continue with model-labels (dependent variables)
  # which are the heading for each model column
  # -------------------------------------
  if (!is.null(labelDependentVariables)) {
    for (i in 1:length(labelDependentVariables)) {
      # -------------------------
      # insert "separator column"
      # -------------------------
      page.content <- paste0(page.content, sprintf("\n    <td class=\"separatorcol%s\">&nbsp;</td>", tcp))
      if (headerColSpanFactor > 1) {
        page.content <- paste0(page.content, 
                               sprintf("\n    <td class=\"tdata centeralign labelcellborder%s\" colspan=\"%i\">%s</td>", 
                                       tcp, 
                                       headerColSpanFactor, 
                                       labelDependentVariables[i]))
      } else {
        page.content <- paste0(page.content, 
                               sprintf("\n    <td class=\"tdata centeralign labelcellborder%s\">%s</td>", 
                                       tcp, 
                                       labelDependentVariables[i]))
      }
    }
    page.content <- paste0(page.content, "\n  </tr>")
  } else {
    for (i in 1:length(input_list)) {
      # -------------------------
      # insert "separator column"
      # -------------------------
      page.content <- paste0(page.content, sprintf("\n    <td class=\"separatorcol%s\">&nbsp;</td>", tcp))
      if (headerColSpanFactor > 1) {
        page.content <- paste0(page.content, sprintf("\n    <td class=\"tdata centeralign labelcellborder%s\" colspan=\"%i\">%s %i</td>", 
                                                     tcp, 
                                                     headerColSpanFactor, 
                                                     stringModel, i))
      } else {
        page.content <- paste0(page.content, sprintf("\n    <td class=\"tdata centeralign labelcellborder%s\">%s %i</td>", 
                                                     tcp, 
                                                     stringModel, i))
      }
    }
    page.content <- paste0(page.content, "\n  </tr>")
  }
  # -------------------------------------
  # table header: or/ci and p-labels
  # -------------------------------------
  if (showAbbrHeadline) {
    page.content <- paste0(page.content, "\n  <tr>\n    <td class=\"tdata colnames\">&nbsp;</td>")
    colnr <- ifelse(is.null(labelDependentVariables), length(input_list), length(labelDependentVariables))
    for (i in 1:colnr) {
      # -------------------------
      # insert "separator column"
      # -------------------------
      page.content <- paste0(page.content, "<td class=\"separatorcol colnames\">&nbsp;</td>")
      # confidence interval in separate column
      if (showEst) {
        if (separateConfColumn) {
          page.content <- paste0(page.content, sprintf("\n    <td class=\"tdata centeralign colnames modelcolumn1\">%s</td>", stringB))
          if (showConfInt) page.content <- paste0(page.content, sprintf("<td class=\"tdata centeralign colnames modelcolumn2\">%s</td>", stringCI))
        } else {
          # confidence interval in Beta-column
          page.content <- paste0(page.content, sprintf("\n    <td class=\"tdata centeralign colnames modelcolumn1\">%s</td>", showCIString))
        }
      }
      # show std. error
      if (showStdError) page.content <- paste0(page.content, sprintf("<td class=\"tdata centeralign colnames modelcolumn3\">%s</td>", stringSE))
      # show std. beta
      if (showStdBetaValues) {
        # confidence interval in separate column
        if (separateConfColumn) {
          page.content <- paste0(page.content, sprintf("<td class=\"tdata centeralign colnames modelcolumn4\">%s</td>", stringSB))
          if (showConfInt) page.content <- paste0(page.content, sprintf("<td class=\"tdata centeralign colnames modelcolumn5\">%s</td>", stringCI))
        } else {
          # confidence interval in Beta-column
          page.content <- paste0(page.content, sprintf("\n    <td class=\"tdata centeralign colnames modelcolumn4\">%s</td>", showCIStringSB))
        }
      }
      # show p-values as numbers in separate column
      if (pvaluesAsNumbers) page.content <- paste0(page.content, sprintf("<td class=\"tdata centeralign colnames modelcolumn6\">%s</td>", stringP))
    }
    page.content <- paste(page.content, "\n  </tr>\n")
  }
  # -------------------------------------
  # set default predictor labels
  # -------------------------------------
  if (is.null(labelPredictors) && !sw.fit && !lmerob) {
    labelPredictors <- suppressWarnings(retrieveModelLabels(input_list))
  }
  # --------------------------------------------------------
  # auto-retrieving variable labels does not work when we
  # have factors with different levels, which appear as 
  # "multiple predictors", but are only one variable
  # --------------------------------------------------------
  if (is.null(labelPredictors) || length(labelPredictors) < length(joined.df[-1, 1])) {
    labelPredictors <- joined.df[-1, 1]
  }
  # -------------------------------------
  # should factor predictors be grouped?
  # -------------------------------------
  if (group.pred && !lmerob) {
    # get indices
    group.pred.list <- retrieveModelGroupIndices(input_list, remove.estimates)
    # append indices
    group.pred.rows <- group.pred.list[[1]]
    group.pred.span <- group.pred.list[[2]]
    group.pred.labs <- group.pred.list[[3]]
    # if we have also stepwise models, grouping may
    # not work properly
    if (sw.fit) message("Fitted models have different coefficients. Grouping may not work properly. Set 'group.pred = FALSE' if you encouter cluttered labelling.")
  } else {
    group.pred.rows <- group.pred.span <- group.pred.labs <- NULL
  }
  # -------------------------------------
  # 1. table part: "Fixed parts" - only used
  # for linear mixed models
  # -------------------------------------
  if (lmerob) {
    # css attribute "topcontentborder" already in this table row
    page.content <- paste0(page.content, sprintf("  <tr>\n    <td colspan=\"%i\" class=\"tdata leftalign topcontentborder fixedparts\">Fixed Parts</td>\n  </tr>\n", headerColSpan + 1))
    # so it's not needed for intercept row
    tcb_class <- ""
  } else {
    # for simple linear models, we need the
    # css attribute "topcontentborder" for the
    # intercept row
    tcb_class <- "topcontentborder "
  }
  # -------------------------------------
  # 1. row: intercept
  # -------------------------------------
  page.content <- paste0(page.content, sprintf("  <tr>\n    <td class=\"tdata %sleftalign\">%s</td>", 
                                               tcb_class, 
                                               stringIntercept))
  for (i in 1:length(input_list)) {
    # -------------------------
    # insert "separator column"
    # -------------------------
    page.content <- paste0(page.content, sprintf("<td class=\"separatorcol %s\">&nbsp;</td>", tcb_class))
    # show estimates?
    if (showEst) {
      # confidence interval in separate column
      if (separateConfColumn) {
        # open table cell for Beta-coefficient
        page.content <- paste0(page.content, sprintf("\n    <td class=\"tdata centeralign %smodelcolumn1\">%s", 
                                                     tcb_class, 
                                                     joined.df[1, (i - 1) * 8 + 2]))
        # if p-values are not shown as numbers, insert them after beta-value
        if (!pvaluesAsNumbers) page.content <- paste0(page.content, sprintf("&nbsp;%s", 
                                                                            joined.df[1, (i - 1) * 8 + 5]))
        # if we have CI, start new table cell (CI in separate column)
        if (showConfInt) {
          page.content <- paste0(page.content, sprintf("</td><td class=\"tdata centeralign %smodelcolumn2\">%s%s%s</td>", 
                                                       tcb_class, 
                                                       joined.df[1, (i - 1) * 8 + 3], 
                                                       ci.hyphen,
                                                       joined.df[1, (i - 1) * 8 + 4]))
        } else {
          page.content <- paste0(page.content, "</td>")
        }
      } else {
        # open table cell for Beta-coefficient
        page.content <- paste0(page.content, sprintf("\n    <td class=\"tdata centeralign %smodelcolumn1\">%s",
                                                     tcb_class, 
                                                     joined.df[1, (i - 1) * 8 + 2]))
        # confidence interval in Beta-column
        if (showConfInt) page.content <- paste0(page.content, sprintf("%s(%s%s%s)", 
                                                                      linebreakstring, 
                                                                      joined.df[1, (i - 1) * 8 + 3], 
                                                                      ci.hyphen,
                                                                      joined.df[1, (i - 1) * 8 + 4]))
        # if p-values are not shown as numbers, insert them after beta-value
        if (!pvaluesAsNumbers) page.content <- paste0(page.content, sprintf("&nbsp;%s", 
                                                                            joined.df[1, (i - 1) * 8 + 5]))
        page.content <- paste0(page.content, "</td>")
      }
    }
    # show std. error
    if (showStdError) page.content <- paste0(page.content, sprintf("<td class=\"tdata centeralign %smodelcolumn3\">%s</td>", 
                                                                   tcb_class, 
                                                                   joined.df[1, (i - 1) * 8 + 6]))
    # show std. beta
    if (showStdBetaValues) page.content <- paste0(page.content, 
                                            sprintf("<td class=\"tdata centeralign %smodelcolumn4\">&nbsp;</td>", 
                                                    tcb_class))
    # show std. beta
    if (showStdBetaValues && showConfInt && separateConfColumn) page.content <- paste0(page.content, sprintf("<td class=\"tdata centeralign %smodelcolumn5\">&nbsp;</td>", 
                                                                                                       tcb_class))
    # show p-values as numbers in separate column
    if (pvaluesAsNumbers) page.content <- paste0(page.content, sprintf("<td class=\"tdata centeralign %smodelcolumn6\">%s</td>", 
                                                                       tcb_class, 
                                                                       joined.df[1, (i - 1) * 8 + 5]))
  }
  page.content <- paste0(page.content, "\n  </tr>")  
  # -------------------------------------
  # subsequent rows: predictors
  # -------------------------------------
  for (i in 1:(nrow(joined.df) - 1)) {
    # -------------------------------------
    # do we need to insert a "factor grouping headline row"?
    # -------------------------------------
    if (!is.null(group.pred.rows) && any(group.pred.rows == i)) {
      page.content <- paste0(page.content, 
                             "\n  <tr>\n", 
                             sprintf("\n    <td class=\"grouprow\" colspan=\"%i\">%s</td>", 
                                     headerColSpan + 1, 
                                     group.pred.labs[which(group.pred.rows == i)]),
                             "\n  </tr>")
    }
    if (!is.null(group.pred.rows) && any(group.pred.span == i)) {
      indent.tag <- "tgrpdata"
    } else {
      indent.tag <- "tdata"
    }
    page.content <- paste0(page.content, "\n  <tr>\n", sprintf("    <td class=\"%s leftalign\">%s</td>", indent.tag, labelPredictors[i]))
    # ---------------------------------------
    # go through fitted model's statistics
    # ---------------------------------------
    for (j in 1:length(input_list)) {
      # -------------------------
      # insert "separator column"
      # -------------------------
      page.content <- paste0(page.content, "<td class=\"separatorcol\">&nbsp;</td>")
      # show estimates?
      if (showEst) {
        # retieve lower and upper ci
        ci.lo <- joined.df[i + 1, (j - 1) * 8 + 3]
        ci.hi <- joined.df[i + 1, (j - 1) * 8 + 4]
        # if we have empty cells (due to different predictors in models)
        # we don't print CI-separator strings and we don't print any esitmate
        # values - however, for proper display, we fill these values with "&nbsp;"
        ci.sep.string <- ifelse(sjmisc::is_empty(ci.lo), "&nbsp;", ci.hyphen)
        # replace empty beta, se and p-values with &nbsp;
        if (sjmisc::is_empty(joined.df[i + 1, (j - 1) * 8 + 2])) joined.df[i + 1, (j - 1) * 8 + 2] <- "&nbsp;"
        if (sjmisc::is_empty(joined.df[i + 1, (j - 1) * 8 + 5])) joined.df[i + 1, (j - 1) * 8 + 5] <- "&nbsp;"
        if (sjmisc::is_empty(joined.df[i + 1, (j - 1) * 8 + 6])) joined.df[i + 1, (j - 1) * 8 + 6] <- "&nbsp;"
        # confidence interval in separate column
        if (separateConfColumn) {
          # open table cell for Beta-coefficient
          page.content <- paste0(page.content, sprintf("\n    <td class=\"tdata centeralign modelcolumn1\">%s", 
                                                       joined.df[i + 1, (j - 1) * 8 + 2]))
          # if p-values are not shown as numbers, insert them after beta-value
          if (!pvaluesAsNumbers) page.content <- paste0(page.content, sprintf("&nbsp;%s", joined.df[i + 1, (j - 1) * 8 + 5]))
          # if we have CI, start new table cell (CI in separate column)
          if (showConfInt) {
            page.content <- paste0(page.content, sprintf("</td><td class=\"tdata centeralign modelcolumn2\">%s%s%s</td>", 
                                                         ci.lo, 
                                                         ci.sep.string, 
                                                         ci.hi))
          } else {
            page.content <- paste0(page.content, "</td>")
          }
        } else {
          # open table cell for Beta-coefficient
          page.content <- paste0(page.content, sprintf("\n    <td class=\"tdata centeralign modelcolumn1\">%s", 
                                                       joined.df[i + 1, (j - 1) * 8 + 2]))
          # confidence interval in Beta-column
          if (showConfInt && !sjmisc::is_empty(ci.lo)) page.content <- paste0(page.content, 
                                                                              sprintf("%s(%s%s%s)", 
                                                                                      linebreakstring, 
                                                                                      ci.lo, 
                                                                                      ci.sep.string, 
                                                                                      ci.hi))
          # if p-values are not shown as numbers, insert them after beta-value
          if (!pvaluesAsNumbers) page.content <- paste0(page.content, 
                                                        sprintf("&nbsp;%s", 
                                                                joined.df[i + 1, (j - 1) * 8 + 5]))
          page.content <- paste0(page.content, "</td>")
        }
      }
      # show std. error
      if (showStdError) page.content <- paste0(page.content, 
                                               sprintf("<td class=\"tdata centeralign modelcolumn3\">%s</td>", 
                                                       joined.df[i + 1, (j - 1) * 8 + 6]))
      # show std. beta
      if (showStdBetaValues) {
        # retieve lower and upper ci
        ci.lo <- joined.df[i + 1, (j - 1) * 8 + 8]
        ci.hi <- joined.df[i + 1, (j - 1) * 8 + 9]
        ci.sep.string <- ifelse(sjmisc::is_empty(ci.lo), "", "&nbsp;-&nbsp;")
        if (separateConfColumn) {
          # open table cell for Beta-coefficient
          page.content <- paste0(page.content, sprintf("<td class=\"tdata centeralign modelcolumn4\">%s", joined.df[i + 1, (j - 1) * 8 + 7]))
          # show pvalue stars, if no estimates are shown
          if (!pvaluesAsNumbers && !showEst) page.content <- paste0(page.content, sprintf("&nbsp;%s", joined.df[1, (i - 1) * 8 + 5]))
          # if we have CI, start new table cell (CI in separate column)
          if (showConfInt) {
            page.content <- paste0(page.content, sprintf("</td><td class=\"tdata centeralign modelcolumn5\">%s%s%s</td>", ci.lo, ci.sep.string, ci.hi))
          } else {
            page.content <- paste0(page.content, "</td>")
          }
        } else {
          # open table cell for Beta-coefficient
          page.content <- paste0(page.content, sprintf("<td class=\"tdata centeralign modelcolumn4\">%s", joined.df[i + 1, (j - 1) * 8 + 7]))
          # show pvalue stars, if no estimates are shown
          if (!pvaluesAsNumbers && !showEst) page.content <- paste0(page.content, sprintf("&nbsp;%s", joined.df[1, (i - 1) * 8 + 5]))
          # confidence interval in Beta-column
          if (showConfInt && !sjmisc::is_empty(ci.lo)) page.content <- paste0(page.content, sprintf("%s(%s%s%s)", linebreakstring, ci.lo, ci.sep.string, ci.hi))
          # if p-values are not shown as numbers, insert them after beta-value
          page.content <- paste0(page.content, "</td>")
        }
      }
      # show p-values as numbers in separate column
      if (pvaluesAsNumbers) page.content <- paste0(page.content, sprintf("<td class=\"tdata centeralign modelcolumn6\">%s</td>", joined.df[i + 1, (j - 1) * 8 + 5]))
    }
    page.content <- paste0(page.content, "\n  </tr>")
  }
  # -------------------------------------
  # Summary-row: column spans
  # -------------------------------------
  if (headerColSpanFactor > 1) {
    colspanstring <- sprintf("<td class=\"tdata centeralign summary\" colspan=\"%i\">", headerColSpanFactor)
    colspanstringfirstrow <- sprintf("<td class=\"tdata summary centeralign firstsumrow\" colspan=\"%i\">", headerColSpanFactor)
  } else {
    colspanstring <- c("<td class=\"tdata centeralign summary\">")
    colspanstringfirstrow <- c("<td class=\"tdata summary centeralign firstsumrow\">")
  }
  # -------------------------------------
  # Model-Summary: N of grouping levels
  # only applies to mixed models
  # -------------------------------------
  if (lmerob) {
    # css attribute "topcontentborder" already in this table row
    page.content <- paste0(page.content, sprintf("  <tr>\n    <td colspan=\"%i\" class=\"tdata summary leftalign randomparts\">Random Parts</td>\n  </tr>\n", headerColSpan + 1))
    # first models indicates grouping levels
    # we have to assume comparable models with same
    # random intercepts
    # count all random intercepts of all models
    all_mm_counts <- unlist(lapply(input_list, function(x) length(lme4::getME(x, "flist"))))
    # retrieve maximum random intercepts
    mmcount <- max(all_mm_counts)
    # get random intercepts from model with most intercepts
    mmgrps <- lme4::getME(input_list[[which.max(all_mm_counts)]], "flist")
    # iterate grouping levels
    for (gl in 1:mmcount) {
      page.content <- paste0(page.content, sprintf("\n  <tr>\n    <td class=\"tdata summary leftalign\">N<sub>%s</sub></td>\n", names(mmgrps[gl])))
      # iterate models
      for (i in 1:length(input_list)) {
        # -------------------------
        # insert "separator column"
        # -------------------------
        page.content <- paste0(page.content, "<td class=\"separatorcol\">&nbsp;</td>")
        # retrieve random intercepts of each model
        sub.mmgrps <- lme4::getME(input_list[[i]], "flist")
        # does model have enough random intercepts?
        # if yes, print
        if (length(sub.mmgrps) >= gl) {
          page.content <- paste(page.content, sprintf("   %s%i</td>\n", 
                                                      colspanstring, 
                                                      length(levels(sub.mmgrps[[gl]]))))
        } else {
          page.content <- paste(page.content, sprintf("   %s&nbsp;</td>\n", colspanstring))
        }
      }
      page.content <- paste0(page.content, "  </tr>\n")
    }
    # -------------------------------------
    # Model-Summary: icc
    # -------------------------------------
    if (showICC) {
      # get icc from models
      summary.icc <- sjmisc::icc(input_list[[which.max(all_mm_counts)]])
      # iterate icc's
      for (si in 1:mmcount) {
        page.content <- paste0(page.content, sprintf("  <tr>\n    <td class=\"tdata leftalign summary\">ICC<sub>%s</sub></td>\n", names(summary.icc[si])))
        # iterate models
        for (i in 1:length(input_list)) {
          # -------------------------
          # insert "separator column"
          # -------------------------
          page.content <- paste0(page.content, "<td class=\"separatorcol\">&nbsp;</td>")
          # get icc from models
          sub.summary.icc <- sjmisc::icc(input_list[[i]])
          # does model have enough icc values?
          # if yes, print
          if (length(sub.summary.icc) >= si) {
            sum.icc.str <- paste0(sprintf("%.*f", digits.summary, sub.summary.icc[si]), collapse = "")
            page.content <- paste0(page.content, colspanstring, sum.icc.str, "</td>\n")
          } else {
            page.content <- paste0(page.content, colspanstring, "&nbsp;</td>\n")
          }
        }
        page.content <- paste(page.content, "  </tr>\n")
      }
    }
  }
  # -------------------------------------
  # Model-Summary: N
  # -------------------------------------
  page.content <- paste0(page.content, sprintf("\n  <tr>\n    <td class=\"tdata summary leftalign firstsumrow\">%s</td>\n", stringObservations))
  for (i in 1:length(input_list)) {
    # -------------------------
    # insert "separator column"
    # -------------------------
    page.content <- paste0(page.content, "<td class=\"separatorcol firstsumrow\">&nbsp;</td>")
    # -------------------------------------
    # get number of observations
    # -------------------------------------
    if (any(class(input_list[[i]]) == "plm")) {
      # "plm" seems not to offer a "nobs" function
      n_of_obs <- nrow(input_list[[i]]$model)
    } else {
      n_of_obs <- stats::nobs(input_list[[i]])
    }
    page.content <- paste(page.content, sprintf("   %s%i</td>\n", colspanstringfirstrow, n_of_obs))
  }
  page.content <- paste0(page.content, "  </tr>\n")
  # -------------------------------------
  # Model-Summary: r2 and sdj. r2
  # -------------------------------------
  if (showR2) {
    page.content <- paste0(page.content, "  <tr>\n    <td class=\"tdata leftalign summary\">R<sup>2</sup> / adj. R<sup>2</sup></td>\n")
    for (i in 1:length(input_list)) {
      # -------------------------
      # insert "separator column"
      # -------------------------
      page.content <- paste0(page.content, "\n    <td class=\"separatorcol\">&nbsp;</td>")
      rsqu <- summary(input_list[[i]])$r.squared
      adjrsqu <- summary(input_list[[i]])$adj.r.squared
      page.content <- paste0(page.content, gsub("0.", 
                                                paste0(p_zero, "."), 
                                                sprintf("    %s%.*f / %.*f</td>\n", colspanstring, digits.summary, rsqu, digits.summary, adjrsqu),
                                                fixed = TRUE))
    }
    page.content <- paste(page.content, "  </tr>\n")
  }
  # -------------------------------------
  # Model-Summary: F-statistics
  # -------------------------------------
  if (showFStat) {
    page.content <- paste(page.content, "  <tr>\n     <td class=\"tdata leftalign summary\">F-statistics</td>\n")
    for (i in 1:length(input_list)) {
      # -------------------------
      # insert "separator column"
      # -------------------------
      page.content <- paste0(page.content, "\n    <td class=\"separatorcol\">&nbsp;</td>")
      fstat <- summary(input_list[[i]])$fstatistic
      # Calculate p-value for F-test
      pval <- pf(fstat[1], 
                 fstat[2], 
                 fstat[3],
                 lower.tail = FALSE)
      # indicate significance level by stars
      pan <- get_p_stars(pval)
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
      # -------------------------
      # insert "separator column"
      # -------------------------
      page.content <- paste0(page.content, "\n    <td class=\"separatorcol\">&nbsp;</td>")
      page.content <- paste(page.content, sprintf("    %s%.*f</td>\n", colspanstring, digits.summary, stats::AIC(input_list[[i]])))
    }
    page.content <- paste0(page.content, "  </tr>\n")
  }
  # -------------------------------------
  # Model-Summary: AICc
  # -------------------------------------
  if (showAICc) {
    page.content <- paste(page.content, "  <tr>\n     <td class=\"tdata leftalign summary\">AICc</td>\n")
    for (i in 1:length(input_list)) {
      # -------------------------
      # insert "separator column"
      # -------------------------
      page.content <- paste0(page.content, "\n    <td class=\"separatorcol\">&nbsp;</td>")
      page.content <- paste(page.content, sprintf("    %s%.*f</td>\n", colspanstring, digits.summary, AICcmodavg::AICc(input_list[[i]])))
    }
    page.content <- paste0(page.content, "  </tr>\n")
  }
  # -------------------------------------
  # table footnote
  # -------------------------------------
  if (!pvaluesAsNumbers) page.content <- paste(page.content, sprintf("  <tr class=\"tdata annorow\">\n    <td class=\"tdata\">Notes</td><td class=\"tdata annostyle\" colspan=\"%i\"><em>* p&lt;%s.05&nbsp;&nbsp;&nbsp;** p&lt;%s.01&nbsp;&nbsp;&nbsp;*** p&lt;%s.001</em></td>\n  </tr>\n", headerColSpan, p_zero, p_zero, p_zero), sep = "")
  page.content <- paste0(page.content, "</table>\n")
  # -------------------------------------
  # proper HTML-minus-signs
  # -------------------------------------
  page.content <- gsub("-", minus.sign, page.content, fixed = TRUE, useBytes = TRUE)
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
  knitr <- gsub("class=", "style=", knitr, fixed = TRUE, useBytes = TRUE)
  knitr <- gsub("<table", sprintf("<table style=\"%s\"", css.table), knitr, fixed = TRUE, useBytes = TRUE)
  # -------------------------------------
  # replace class-attributes with inline-style-definitions
  # -------------------------------------
  knitr <- gsub(tag.tdata, css.tdata, knitr, fixed = TRUE, useBytes = TRUE)
  knitr <- gsub(tag.thead, css.thead, knitr, fixed = TRUE, useBytes = TRUE)
  knitr <- gsub(tag.summary, css.summary, knitr, fixed = TRUE, useBytes = TRUE)  
  knitr <- gsub(tag.fixedparts, css.fixedparts, knitr, fixed = TRUE, useBytes = TRUE)  
  knitr <- gsub(tag.randomparts, css.randomparts, knitr, fixed = TRUE, useBytes = TRUE)
  knitr <- gsub(tag.separatorcol, css.separatorcol, knitr, fixed = TRUE, useBytes = TRUE)  
  knitr <- gsub(tag.colnames, css.colnames, knitr, fixed = TRUE, useBytes = TRUE)
  knitr <- gsub(tag.leftalign, css.leftalign, knitr, fixed = TRUE, useBytes = TRUE)
  knitr <- gsub(tag.centeralign, css.centeralign, knitr, fixed = TRUE, useBytes = TRUE)
  knitr <- gsub(tag.firstsumrow, css.firstsumrow, knitr, fixed = TRUE, useBytes = TRUE)
  knitr <- gsub(tag.lasttablerow, css.lasttablerow, knitr, fixed = TRUE, useBytes = TRUE)  
  knitr <- gsub(tag.labelcellborder, css.labelcellborder, knitr, fixed = TRUE, useBytes = TRUE)  
  knitr <- gsub(tag.topborder, css.topborder, knitr, fixed = TRUE, useBytes = TRUE)  
  knitr <- gsub(tag.depvarhead, css.depvarhead, knitr, fixed = TRUE, useBytes = TRUE)  
  knitr <- gsub(tag.topcontentborder, css.topcontentborder, knitr, fixed = TRUE, useBytes = TRUE)  
  knitr <- gsub(tag.noannorow, css.noannorow, knitr, fixed = TRUE, useBytes = TRUE)
  knitr <- gsub(tag.annorow, css.annorow, knitr, fixed = TRUE, useBytes = TRUE)  
  knitr <- gsub(tag.annostyle, css.annostyle, knitr, fixed = TRUE, useBytes = TRUE)  
  knitr <- gsub(tag.grouprow, css.grouprow, knitr, fixed = TRUE, useBytes = TRUE)
  knitr <- gsub(tag.tgrpdata, css.tgrpdata, knitr, fixed = TRUE, useBytes = TRUE)
  knitr <- gsub(tag.modelcolumn1, css.modelcolumn1, knitr, fixed = TRUE, useBytes = TRUE)
  knitr <- gsub(tag.modelcolumn2, css.modelcolumn2, knitr, fixed = TRUE, useBytes = TRUE)
  knitr <- gsub(tag.modelcolumn3, css.modelcolumn3, knitr, fixed = TRUE, useBytes = TRUE)
  knitr <- gsub(tag.modelcolumn4, css.modelcolumn4, knitr, fixed = TRUE, useBytes = TRUE)
  knitr <- gsub(tag.modelcolumn5, css.modelcolumn5, knitr, fixed = TRUE, useBytes = TRUE)
  knitr <- gsub(tag.modelcolumn6, css.modelcolumn6, knitr, fixed = TRUE, useBytes = TRUE)
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
  # replace &nbsp; (former NA), created by join, with empty string
  # -------------------------------------
  joined.df <- apply(joined.df, 1:2, function(x) if (x == "&nbsp;") "" else x)
  # -------------------------------------
  # return results
  # -------------------------------------
  invisible(structure(class = "sjtlm",
                      list(page.style = page.style,
                           page.content = page.content,
                           output.complete = toWrite,
                           knitr = knitr,
                           data = joined.df)))
}


#' @title Summary of linear mixed effects models as HTML table
#' @name sjt.lmer
#' 
#' @description Summarizes (multiple) fitted linear mixed effects models 
#'                (estimates, std. beta values etc.)  as HTML table, 
#'                or saves them as file. The fitted models may have different 
#'                predictors, e.g. when comparing different stepwise fitted models.
#'                
#' @seealso \href{http://strengejacke.de/sjPlot/sjt.lmer/}{sjPlot manual: sjt.lmer} 
#'            and \code{\link{sjt.lm}} for further examples.
#' 
#' @inheritParams sjt.lm
#' @inheritParams sjt.frq
#' 
#' @param showICC logical, if \code{TRUE}, the intra-class-correlation for each 
#'          model is printed in the model summary.
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
#' library(lme4)
#' library(sjmisc)
#' data(efc)
#' 
#' # prepare group variable
#' efc$grp = as.factor(efc$e15relat)
#' levels(x = efc$grp) <- get_labels(efc$e15relat)
#' efc$care.level <- as.factor(sjmisc::rec(efc$n4pstu, "0=0;1=1;2=2;3:4=4"))
#' levels(x = efc$care.level) <- c("none", "I", "II", "III")
#' 
#' # data frame for fitted model
#' mydf <- data.frame(neg_c_7 = as.numeric(efc$neg_c_7),
#'                    sex = as.factor(efc$c161sex),
#'                    c12hour = as.numeric(efc$c12hour),
#'                    barthel = as.numeric(efc$barthtot),
#'                    education = as.factor(efc$c172code),
#'                    grp = efc$grp,
#'                    carelevel = efc$care.level)
#'                    
#' # fit two sample models
#' fit1 <- lmer(neg_c_7 ~ sex + c12hour + barthel + (1|grp), data = mydf)
#' fit2 <- lmer(neg_c_7 ~ sex + c12hour + education + barthel + (1|grp), data = mydf)
#' fit3 <- lmer(neg_c_7 ~ sex + c12hour + education + barthel + 
#'               (1|grp) + 
#'               (1|carelevel), data = mydf)
#' 
#' # print summary table
#' sjt.lmer(fit1, fit2,
#'          ci.hyphen = " to ",
#'          minus.sign = "&minus;")
#' 
#' sjt.lmer(fit1, fit2,
#'          showAIC = TRUE,
#'          showConfInt = FALSE,
#'          showStdError = TRUE,
#'          pvaluesAsNumbers = FALSE)
#'            
#' sjt.lmer(fit1, fit2, fit3, 
#'          showAIC = TRUE,
#'          separateConfColumn = FALSE,
#'          newLineConf = FALSE)
#'          
#' sjt.lmer(fit1, fit2, fit3,
#'          labelPredictors = c("Elder's gender (female)",
#'                              "Hours of care per week",
#'                              "Barthel Index",
#'                              "Educational level (mid)",
#'                              "Educational level (high)"))}
#'                   
#' @export
sjt.lmer <- function(...,
                     file = NULL,
                     labelPredictors = NULL,
                     labelDependentVariables = NULL,
                     stringPredictors = "Predictors",
                     stringDependentVariables = "Dependent Variables",
                     stringModel = "Model",
                     showHeaderStrings = FALSE,
                     stringIntercept = "(Intercept)",
                     stringObservations = "Observations",
                     stringB = "B",
                     stringSB = "std. Beta",
                     stringCI = "CI",
                     stringSE = "std. Error",
                     stringP = "p",
                     showEst = TRUE,
                     showConfInt = TRUE,
                     showStdBeta = FALSE,
                     showStdError = FALSE,
                     ci.hyphen = "&nbsp;&ndash;&nbsp;",
                     minus.sign = "&#45;",
                     digits.est = 2,
                     digits.p = 3,
                     digits.ci = 2,
                     digits.se = 2,
                     digits.sb = 2,
                     digits.summary = 3,
                     pvaluesAsNumbers = TRUE,
                     boldpvalues = TRUE,
                     separateConfColumn = TRUE,
                     newLineConf = TRUE,
                     showAbbrHeadline = TRUE,
                     showICC = TRUE,
                     showAIC = FALSE,
                     showAICc = FALSE,
                     remove.estimates = NULL,
                     cellSpacing = 0.2,
                     encoding = NULL,
                     CSS = NULL,
                     useViewer = TRUE,
                     no.output = FALSE,
                     remove.spaces = TRUE) {
  input_list <- list(...)
  return(sjt.lm(input_list, file = file, labelPredictors = labelPredictors, 
                labelDependentVariables = labelDependentVariables, stringPredictors = stringPredictors, 
                stringDependentVariables = stringDependentVariables, stringModel = stringModel, 
                showHeaderStrings = showHeaderStrings, stringIntercept = stringIntercept,
                stringObservations = stringObservations, stringB = stringB, stringSB = stringSB, 
                stringCI = stringCI, stringSE = stringSE, stringP = stringP, showEst = showEst,
                showConfInt = showConfInt, showStdBeta = showStdBeta, showStdError = showStdError, 
                ci.hyphen = ci.hyphen, minus.sign = minus.sign,
                digits.est = digits.est, digits.p = digits.p, digits.ci = digits.ci,
                digits.se = digits.se, digits.sb = digits.sb, digits.summary = digits.summary, 
                pvaluesAsNumbers = pvaluesAsNumbers, boldpvalues = boldpvalues, 
                separateConfColumn = separateConfColumn, newLineConf = newLineConf, 
                group.pred = FALSE, showAbbrHeadline = showAbbrHeadline, showR2 = showICC, 
                showFStat = FALSE, showAIC = showAIC, showAICc = showAICc, remove.estimates = remove.estimates, 
                cellSpacing = cellSpacing, cellGroupIndent = 0, encoding = encoding, 
                CSS = CSS, useViewer = useViewer, no.output = no.output, remove.spaces = remove.spaces))
}