# bind global variables
utils::globalVariables(c("starts_with"))


#' @title Summary of generalized linear models as HTML table
#' @name sjt.glm
#' 
#' @description Summarizes (multiple) fitted generalized linear models (odds ratios, ci, p-values...)
#'                as HTML table, or saves them as file. The fitted models may have different predictors,
#'                e.g. when comparing different stepwise fitted models.
#' 
#' @param ... one or more fitted generalized linear (mixed) models.
#' @param stringOR string used for the column heading of odds ratio values. Default is \code{"OR"}.
#' @param exp.coef logical, if \code{TRUE} (default), regression coefficients and 
#'          confidence intervals are exponentiated. Use \code{FALSE} for 
#'          non-exponentiated coefficients (log-odds) as provided by 
#'          the \code{\link{summary}} function.
#' @param newLineConf logical, if \code{TRUE} and \code{separateConfColumn = FALSE}, inserts a line break
#'          between OR and CI values. If \code{FALSE}, CI values are printed in the same
#'          line with OR values.
#' @param showAbbrHeadline logical, if \code{TRUE} (default), the table data columns have a headline with 
#'          abbreviations for odds ratios, confidence interval and p-values.
#' @param showPseudoR logical, if \code{TRUE} (default), the pseudo R2 values for each model are printed
#'          in the model summary. R2cs is the Cox-Snell-pseudo R-square value, R2n is Nagelkerke's 
#'          pseudo R-square value and \code{D} is Tjur's Coefficient of Discrimination
#'          (see \code{\link[sjmisc]{cod}}).
#' @param showLogLik logical, if \code{TRUE}, the Log-Likelihood for each model is printed
#'          in the model summary. Default is \code{FALSE}.
#' @param showChi2 logical, if \code{TRUE}, the p-value of the chi-squared value for each 
#'          model's residual deviance against the null deviance is printed
#'          in the model summary. Default is \code{FALSE}. A well-fitting model
#'          with predictors should significantly differ from the null-model
#'          (without predictors), thus, a p-value less than 0.05 indicates a
#'          good model-fit.
#' @param showHosLem logical, if \code{TRUE}, a Hosmer-Lemeshow-Goodness-of-fit-test is
#'          performed. A well-fitting model shows no significant difference between 
#'          the model and the observed data, i.e. the reported p-values should be
#'          greater than 0.05.
#' @param showFamily logical, if \code{TRUE}, the family object and link function for each fitted model
#'          are printed. Can be used in case you want to compare models with different link functions
#'          and same predictors and response, to decide which model fits best. See \code{\link{family}}
#'          for more details. It is recommended to inspect the model \code{\link{AIC}} (see \code{showAIC}) to get a
#'          decision help for which model to choose.
#'          
#' @inheritParams sjt.frq
#' @inheritParams sjt.lm
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
#' # prepare dummy variables for binary logistic regression
#' y1 <- ifelse(swiss$Fertility < median(swiss$Fertility), 0, 1)
#' y2 <- ifelse(swiss$Infant.Mortality < median(swiss$Infant.Mortality), 0, 1)
#' y3 <- ifelse(swiss$Agriculture < median(swiss$Agriculture), 0, 1)
#' 
#' # Now fit the models. Note that both models share the same predictors
#' # and only differ in their dependent variable (y1, y2 and y3)
#' fitOR1 <- glm(y1 ~ swiss$Education + swiss$Examination+swiss$Catholic,
#'               family = binomial(link = "logit"))
#' fitOR2 <- glm(y2 ~ swiss$Education + swiss$Examination+swiss$Catholic,
#'               family = binomial(link = "logit"))
#' fitOR3 <- glm(y3 ~ swiss$Education + swiss$Examination+swiss$Catholic,
#'               family = binomial(link = "logit"))
#'
#' # open HTML-table in RStudio Viewer Pane or web browser
#' \dontrun{
#' sjt.glm(fitOR1, 
#'         fitOR2, 
#'         labelDependentVariables = c("Fertility", 
#'                                     "Infant Mortality"),
#'         labelPredictors = c("Education", 
#'                             "Examination", 
#'                             "Catholic"),
#'          ci.hyphen = " to ")
#' 
#' # open HTML-table in RStudio Viewer Pane or web browser,
#' # integrate CI in OR column
#' sjt.glm(fitOR1, fitOR2, fitOR3,
#'         labelDependentVariables = c("Fertility", 
#'                                     "Infant Mortality", 
#'                                     "Agriculture"),
#'         labelPredictors = c("Education", "Examination", "Catholic"),
#'         separateConfColumn = FALSE)
#' 
#' # open HTML-table in RStudio Viewer Pane or web browser,
#' # indicating p-values as numbers and printing CI in a separate column
#' sjt.glm(fitOR1, fitOR2, fitOR3,
#'         labelDependentVariables = c("Fertility", 
#'                                     "Infant Mortality", 
#'                                     "Agriculture"),
#'         labelPredictors = c("Education", "Examination", "Catholic"))
#' 
#' 
#' # -------------------------------------------- 
#' # User defined style sheet
#' # -------------------------------------------- 
#' sjt.glm(fitOR1, fitOR2, fitOR3,
#'         labelDependentVariables = c("Fertility", 
#'                                     "Infant Mortality", 
#'                                     "Agriculture"),
#'         labelPredictors = c("Education", "Examination", "Catholic"),
#'         showHeaderStrings = TRUE,
#'         CSS = list(css.table = "border: 2px solid;",
#'                    css.tdata = "border: 1px solid;",
#'                    css.depvarhead = "color:#003399;"))
#' 
#' 
#' # -------------------------------------------- 
#' # Compare models with different link functions, 
#' # but same predictors and response
#' # -------------------------------------------- 
#' library(sjmisc)
#' # load efc sample data
#' data(efc)
#' # dichtomozize service usage by "service usage yes/no"
#' efc$services <- sjmisc::dicho(efc$tot_sc_e, "v", 0, as.num = TRUE)
#' # fit 3 models with different link-functions
#' fit1 <- glm(services ~ neg_c_7 + c161sex + e42dep, 
#'             data=efc, 
#'             family=binomial(link="logit"))
#' fit2 <- glm(services ~ neg_c_7 + c161sex + e42dep, 
#'             data=efc, 
#'             family=binomial(link="probit"))
#' fit3 <- glm(services ~ neg_c_7 + c161sex + e42dep, 
#'             data=efc, 
#'             family=poisson(link="log"))
#'             
#' # compare models
#' sjt.glm(fit1, fit2, fit3, 
#'         showAIC = TRUE, 
#'         showFamily = TRUE)
#' 
#' 
#' # --------------------------------------------
#' # Change style of p-values and CI-appearance
#' # --------------------------------------------
#' # open HTML-table in RStudio Viewer Pane or web browser,
#' # table indicating p-values as stars
#' sjt.glm(fit1, fit2, fit3, 
#'         pvaluesAsNumbers = FALSE,
#'         showAIC = TRUE, 
#'         showFamily = TRUE)
#' 
#' # open HTML-table in RStudio Viewer Pane or web browser,
#' # indicating p-values as stars and integrate CI in OR column
#' sjt.glm(fit1, fit2, fit3, 
#'         pvaluesAsNumbers = FALSE,
#'         separateConfColumn = FALSE,
#'         showAIC = TRUE, 
#'         showFamily = TRUE,
#'         showPseudoR = TRUE)
#' 
#' # ---------------------------------- 
#' # automatic grouping of predictors
#' # ---------------------------------- 
#' library(sjmisc)
#' # load efc sample data
#' data(efc)
#' # dichtomozize service usage by "service usage yes/no"
#' efc$services <- sjmisc::dicho(efc$tot_sc_e, "v", 0, as.num = TRUE)
#' # make dependency categorical
#' efc$e42dep <- to_fac(efc$e42dep)
#' # fit model with "grouped" predictor
#' fit <- glm(services ~ neg_c_7 + c161sex + e42dep, data = efc)
#' 
#' # automatic grouping of categorical predictors
#' sjt.glm(fit)
#' 
#' 
#' # ---------------------------------- 
#' # compare models with different predictors
#' # ---------------------------------- 
#' fit2 <- glm(services ~ neg_c_7 + c161sex + e42dep + c12hour, data = efc)
#' fit3 <- glm(services ~ neg_c_7 + c161sex + e42dep + c12hour + c172code, 
#'             data = efc)
#' 
#' # print models with different predictors
#' sjt.glm(fit, fit2, fit3)
#' 
#' efc$c172code <- to_fac(efc$c172code)
#' fit2 <- glm(services ~ neg_c_7 + c161sex + c12hour, data = efc)
#' fit3 <- glm(services ~ neg_c_7 + c161sex + c172code, data = efc)
#' 
#' # print models with different predictors
#' sjt.glm(fit, fit2, fit3, group.pred = FALSE)}
#' 
#' @importFrom dplyr full_join slice
#' @importFrom stats nobs AIC confint coef logLik family
#' @export
sjt.glm <- function(...,
                    file = NULL,
                    labelPredictors = NULL,
                    labelDependentVariables = NULL,
                    stringPredictors = "Predictors",
                    stringDependentVariables = "Dependent Variables",
                    showHeaderStrings = FALSE,
                    stringModel = "Model",
                    stringIntercept = "(Intercept)",
                    stringObservations = "Observations",
                    stringOR = "OR",
                    stringCI = "CI",
                    stringSE = "std. Error",
                    stringP = "p",
                    digits.est = 2,
                    digits.p = 3,
                    digits.ci = 2,
                    digits.se = 2,
                    digits.summary = 3,
                    exp.coef = TRUE,
                    pvaluesAsNumbers = TRUE,
                    boldpvalues = TRUE,
                    showConfInt = TRUE,
                    showStdError = FALSE,
                    ci.hyphen = "&nbsp;&ndash;&nbsp;",
                    separateConfColumn = TRUE,
                    newLineConf = TRUE,
                    group.pred = TRUE,
                    showAbbrHeadline = TRUE,
                    showPseudoR = FALSE,
                    showLogLik = FALSE,
                    showAIC = FALSE,
                    showAICc = FALSE,
                    showChi2 = FALSE,
                    showHosLem = FALSE,
                    showFamily = FALSE,
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
  # check hyphen for ci-range
  if (is.null(ci.hyphen)) ci.hyphen <- "&nbsp;&ndash;&nbsp;"
  # replace space with protected space in ci-hyphen
  ci.hyphen <- gsub(" ", "&nbsp;", ci.hyphen, fixed = TRUE)
  # -------------------------------------
  # check encoding
  # -------------------------------------
  encoding <- get.encoding(encoding)
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
  }
  # ------------------------
  # set page style
  # ------------------------
  page.style <-  sprintf("<style>%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n</style>",
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
                         tag.fixedparts, css.fixedparts,
                         tag.randomparts, css.randomparts,
                         tag.separatorcol, css.separatorcol)
  # ------------------------
  # start content
  # ------------------------
  toWrite <- paste0(toWrite, page.style)
  toWrite = paste(toWrite, "\n</head>\n<body>", "\n")
  # ------------------------
  # retrieve fitted models
  # ------------------------
  input_list <- list(...)
  # --------------------------------------------------------
  # check length. if we have a list of fitted model, 
  # we need to "unlist" them
  # --------------------------------------------------------
  if (class(input_list[[1]])[1] == "list") input_list <- lapply(input_list[[1]], function(x) x)
  # ------------------------
  # do we have mixed models?
  # ------------------------
  lmerob <- any(class(input_list[[1]]) == "glmerMod")
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
    showICC <- showPseudoR
    showPseudoR <- FALSE
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
    sw.fit <- length(unique(sapply(input_list, function(x) length(stats::coef(x))))) > 1
    # if all fitted models have same amount of coefficients, check
    # whether all coefficients have same name. if not, we have models
    # with different predictors (e.g. stepwise comparison)
    if (sw.fit == FALSE) {
      all.coefs <- sapply(input_list, function(x) sort(names(stats::coef(x))))
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
      confis <- get_cleaned_ciMerMod(fit, "glm", T)
      coef.fit <- lme4::fixef(fit)
    } else {
      confis <- stats::confint(fit)
      coef.fit <- stats::coef(fit)
    }
    # -------------------------------------
    # write data to data frame. we need names of
    # coefficients, estimated values, ci,
    # std. beta and p-values
    # -------------------------------------
    fit.df <- data.frame(names(coef.fit))
    if (exp.coef) {
      fit.df$coeffs <- sprintf("%.*f", digits.est, exp(coef.fit))
      fit.df$confi_lower <- sprintf("%.*f", digits.ci, exp(confis[, 1]))
      fit.df$confi_higher <- sprintf("%.*f", digits.ci, exp(confis[, 2]))
    } else {
      fit.df$coeffs <- sprintf("%.*f", digits.est, coef.fit)
      fit.df$confi_lower <- sprintf("%.*f", digits.ci, confis[, 1])
      fit.df$confi_higher <- sprintf("%.*f", digits.ci, confis[, 2])
    }
    # -------------------------------------
    # extracting p-values and se differs between
    # lmer and lm
    # -------------------------------------
    if (lmerob) {
      # p-values
      fit.df$pv <- round(get_lmerMod_pvalues(fit), digits.p)
      # standard error
      fit.df$se <- sprintf("%.*f", digits.se, stats::coef(summary(fit))[, "Std. Error"])
    } else {
      # p-values
      fit.df$pv <- round(summary(fit)$coefficients[, 4], digits.p)
      # standard error
      fit.df$se <- sprintf("%.*f", digits.se, summary(fit)$coefficients[, 2])
    }
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
                          sprintf("se%i", i))
    # -------------------------------------
    # add to df list
    # -------------------------------------
    df.fit[[length(df.fit) + 1]] <- fit.df
  }
  # -------------------------------------
  # join all data frame
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
    showCIString <- stringOR
  } else {
    showCIString <- sprintf("%s (%s)", stringOR, stringCI)
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
  if (pvaluesAsNumbers) headerColSpanFactor <- headerColSpanFactor + 1
  if (separateConfColumn) headerColSpanFactor <- headerColSpanFactor + 1
  if (showStdError) headerColSpanFactor <- headerColSpanFactor + 1
  # now that we know how many columns each model needs,
  # we multiply columns per model with count of models, so we have
  # the column span over all models together; furthermore, we add
  # count of models  to the overall column span, because
  # each model is separated with an empty table column
  headerColSpan <- headerColSpanFactor * headerColSpan + length(input_list)
  linebreakstring <- " "
  if (newLineConf) linebreakstring <- "<br>"
  # -------------------------------------
  # start table
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
        page.content <- paste0(page.content, sprintf("\n    <td class=\"tdata centeralign labelcellborder%s\" colspan=\"%i\">%s</td>", 
                                                     tcp, 
                                                     headerColSpanFactor, 
                                                     labelDependentVariables[i]))
      } else {
        page.content <- paste0(page.content, sprintf("\n    <td class=\"tdata centeralign labelcellborder%s\">%s</td>", 
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
  # set default predictor labels
  # -------------------------------------
  if (is.null(labelPredictors) && !lmerob) {
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
  # table header: column labels or/ci and p-labels
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
      if (separateConfColumn) {
        page.content <- paste0(page.content, sprintf("\n    <td class=\"tdata centeralign colnames modelcolumn1\">%s</td>", stringOR))
        if (showConfInt) page.content <- paste0(page.content, sprintf("<td class=\"tdata centeralign colnames modelcolumn2\">%s</td>", stringCI))
      }
      else {
        # confidence interval in Beta-column
        page.content <- paste0(page.content, sprintf("\n    <td class=\"tdata centeralign colnames modelcolumn1\">%s</td>", showCIString))
      }
      # show std. error
      if (showStdError) page.content <- paste0(page.content, sprintf("<td class=\"tdata centeralign colnames modelcolumn3\">%s</td>", stringSE))
      # show p-values as numbers in separate column
      if (pvaluesAsNumbers) page.content <- paste0(page.content, sprintf("<td class=\"tdata centeralign colnames modelcolumn4\">%s</td>", stringP))
    }
    page.content <- paste(page.content, "\n  </tr>\n")
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
  page.content <- paste0(page.content, sprintf("  <tr>\n    <td class=\"tdata %sleftalign\">%s</td>", tcb_class, stringIntercept))
  for (i in 1:length(input_list)) {
    # -------------------------
    # insert "separator column"
    # -------------------------
    page.content <- paste0(page.content, sprintf("<td class=\"separatorcol %s\">&nbsp;</td>", tcb_class))
    # confidence interval in separate column
    if (separateConfColumn) {
      # open table cell for Beta-coefficient
      page.content <- paste0(page.content, sprintf("\n    <td class=\"tdata centeralign %smodelcolumn1\">%s", 
                                                   tcb_class, 
                                                   joined.df[1, (i - 1) * 5 + 2]))
      # if p-values are not shown as numbers, insert them after beta-value
      if (!pvaluesAsNumbers) page.content <- paste0(page.content, sprintf("&nbsp;%s", joined.df[1, (i - 1) * 5 + 5]))
      # if we have CI, start new table cell (CI in separate column)
      if (showConfInt) {
        page.content <- paste0(page.content, sprintf("</td><td class=\"tdata centeralign %smodelcolumn2\">%s%s%s</td>", 
                                                     tcb_class, 
                                                     joined.df[1, (i - 1) * 5 + 3], 
                                                     ci.hyphen,
                                                     joined.df[1, (i - 1) * 5 + 4]))
      } else {
        page.content <- paste0(page.content, "</td>")
      }
    } else {
      # open table cell for Beta-coefficient
      page.content <- paste0(page.content, sprintf("\n    <td class=\"tdata centeralign %smodelcolumn1\">%s", 
                                                   tcb_class, 
                                                   joined.df[1, (i - 1) * 5 + 2]))
      # confidence interval in Beta-column
      if (showConfInt) page.content <- paste0(page.content, sprintf("%s(%s%s%s)", 
                                                                    linebreakstring, 
                                                                    joined.df[1, (i - 1) * 5 + 3], 
                                                                    ci.hyphen,
                                                                    joined.df[1, (i - 1) * 5 + 4]))
      # if p-values are not shown as numbers, insert them after beta-value
      if (!pvaluesAsNumbers) page.content <- paste0(page.content, sprintf("&nbsp;%s", joined.df[1, (i - 1) * 5 + 5]))
      page.content <- paste0(page.content, "</td>")
    }
    # show std. error
    if (showStdError) page.content <- paste0(page.content, sprintf("<td class=\"tdata centeralign %smodelcolumn3\">%s</td>",
                                                                   tcb_class,
                                                                   joined.df[1, (i - 1) * 5 + 6]))
    # show p-values as numbers in separate column
    if (pvaluesAsNumbers) page.content <- paste0(page.content, sprintf("<td class=\"tdata centeralign %smodelcolumn4\">%s</td>", 
                                                                       tcb_class,
                                                                       joined.df[1, (i - 1) * 5 + 5]))
  }
  page.content <- paste0(page.content, "\n  </tr>")  
  # -------------------------------------
  # subsequent rows: pedictors
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
      # retieve lower and upper ci
      ci.lo <- joined.df[i + 1, (j - 1) * 5 + 3]
      ci.hi <- joined.df[i + 1, (j - 1) * 5 + 4]
      # if we have empry cells (due to different predictors in models)
      # we don't print CI-separator strings and we don't print any esitmate
      # values - however, for proper display, we fill these values with "&nbsp;"
      ci.sep.string <- ifelse(sjmisc::is_empty(ci.lo), "&nbsp;", ci.hyphen)
      # replace empty beta, se and p-values with &nbsp;
      if (sjmisc::is_empty(joined.df[i + 1, (j - 1) * 5 + 2])) joined.df[i + 1, (j - 1) * 5 + 2] <- "&nbsp;"
      if (sjmisc::is_empty(joined.df[i + 1, (j - 1) * 5 + 5])) joined.df[i + 1, (j - 1) * 5 + 5] <- "&nbsp;"
      if (sjmisc::is_empty(joined.df[i + 1, (j - 1) * 5 + 6])) joined.df[i + 1, (j - 1) * 5 + 6] <- "&nbsp;"
      # -------------------------
      # insert "separator column"
      # -------------------------
      page.content <- paste0(page.content, "<td class=\"separatorcol\">&nbsp;</td>")
      # confidence interval in separate column
      if (separateConfColumn) {
        # open table cell for Beta-coefficient
        page.content <- paste0(page.content, sprintf("\n    <td class=\"tdata centeralign modelcolumn1\">%s", 
                                                     joined.df[i + 1, (j - 1) * 5 + 2]))
        # if p-values are not shown as numbers, insert them after beta-value
        if (!pvaluesAsNumbers) page.content <- paste0(page.content, sprintf("&nbsp;%s", 
                                                                            joined.df[i + 1, (j - 1) * 5 + 5]))
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
                                                     joined.df[i + 1, (j - 1) * 5 + 2]))
        # confidence interval in Beta-column
        if (showConfInt && !sjmisc::is_empty(ci.lo)) page.content <- paste0(page.content, 
                                                                            sprintf("%s(%s%s%s)", 
                                                                                    linebreakstring, 
                                                                                    ci.lo, 
                                                                                    ci.sep.string, 
                                                                                    ci.hi))
        # if p-values are not shown as numbers, insert them after beta-value
        if (!pvaluesAsNumbers) page.content <- paste0(page.content, sprintf("&nbsp;%s", 
                                                                            joined.df[i + 1, (j - 1) * 5 + 5]))
        page.content <- paste0(page.content, "</td>")
      }
      # show std. error
      if (showStdError) page.content <- paste0(page.content, sprintf("<td class=\"tdata centeralign modelcolumn3\">%s</td>", 
                                                                     joined.df[i + 1, (j - 1) * 5 + 6]))
      # show p-values as numbers in separate column
      if (pvaluesAsNumbers) page.content <- paste0(page.content, sprintf("<td class=\"tdata centeralign modelcolumn4\">%s</td>", 
                                                                         joined.df[i + 1, (j - 1) * 5 + 5]))
    }
    page.content <- paste0(page.content, "\n  </tr>")
  }
  # -------------------------------------
  # Summary-row: column spans
  # -------------------------------------
  if (headerColSpanFactor > 1) {
    colspanstring <- sprintf("<td class=\"tdata centeralign summary\" colspan=\"%i\">", headerColSpanFactor)
    colspanstringfirstrow <- sprintf("<td class=\"tdata summary centeralign firstsumrow\" colspan=\"%i\">", headerColSpanFactor)
  }
  else {
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
      page.content <- paste0(page.content, sprintf("\n  <tr>\n    <td class=\"tdata summary leftalign\">N<sub>%s</sub></td>", names(mmgrps[gl])))
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
          page.content <- paste(page.content, sprintf("%s%i</td>", colspanstring, length(levels(sub.mmgrps[[gl]]))))
        } else {
          page.content <- paste(page.content, sprintf("%s&nbsp;</td>", colspanstring))
        }
      }
      page.content <- paste0(page.content, "\n  </tr>\n")
    }
    # -------------------------------------
    # Model-Summary: icc
    # -------------------------------------
    if (showICC) {
      # get icc from models
      summary.icc <- sjmisc::icc(input_list[[which.max(all_mm_counts)]])
      # iterate icc's
      for (si in 1:mmcount) {
        page.content <- paste0(page.content, sprintf("  <tr>\n    <td class=\"tdata leftalign summary\">ICC<sub>%s</sub></td>", names(summary.icc[si])))
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
            page.content <- paste0(page.content, colspanstring, sum.icc.str, "</td>")
          } else {
            page.content <- paste0(page.content, colspanstring, "&nbsp;</td>")
          }
        }
        page.content <- paste(page.content, "\n  </tr>\n")
      }
    }
  }
  # -------------------------------------
  # Model-Summary: N
  # -------------------------------------
  page.content <- paste0(page.content, sprintf("\n  <tr>\n    <td class=\"tdata summary leftalign firstsumrow\">%s</td>", stringObservations))
  for (i in 1:length(input_list)) {
    # -------------------------
    # insert "separator column"
    # -------------------------
    page.content <- paste0(page.content, "<td class=\"separatorcol firstsumrow\">&nbsp;</td>")
    page.content <- paste(page.content, sprintf("%s%i</td>", colspanstringfirstrow, stats::nobs(input_list[[i]])))
  }
  page.content <- paste0(page.content, "\n  </tr>\n")
  # -------------------------------------
  # Model-Summary: pseudo r2
  # -------------------------------------
  if (showPseudoR) {
    page.content <- paste0(page.content, "  <tr>\n    <td class=\"tdata leftalign summary\">Pseudo-R<sup>2</sup></td>")
    for (i in 1:length(input_list)) {
      # -------------------------
      # insert "separator column"
      # -------------------------
      page.content <- paste0(page.content, "\n    <td class=\"separatorcol\">&nbsp;</td>")
      psr <- sjmisc::pseudo_r2(input_list[[i]])
      tjur <- sjmisc::cod(input_list[[i]])
      page.content <- paste0(page.content, gsub("0.", 
                                                paste0(p_zero, "."),
                                                sprintf("%sR<sup>2</sup><sub>CS</sub> = %.*f<br>R<sup>2</sup><sub>N</sub> = %.*f<br>D = %.*f</td>", 
                                                        colspanstring, 
                                                        digits.summary, 
                                                        psr$CoxSnell,
                                                        digits.summary, 
                                                        psr$Nagelkerke,
                                                        digits.summary, 
                                                        tjur),
                                                fixed = TRUE))
    }
    page.content <- paste(page.content, "\n  </tr>\n")
  }
  # -------------------------------------
  # Model-Summary: log likelihood
  # -------------------------------------
  if (showLogLik) {
    page.content <- paste0(page.content, "  <tr>\n    <td class=\"tdata leftalign summary\">-2 Log-Likelihood</td>")
    for (i in 1:length(input_list)) {
      # -------------------------
      # insert "separator column"
      # -------------------------
      page.content <- paste0(page.content, "<td class=\"separatorcol\">&nbsp;</td>")
      page.content <- paste0(page.content, sprintf("%s%.*f</td>", colspanstring, digits.summary, -2 * as.vector(stats::logLik(input_list[[i]]))))
    }
    page.content <- paste0(page.content, "\n  </tr>\n")
  }
  # -------------------------------------
  # Model-Summary: AIC
  # -------------------------------------
  if (showAIC) {
    page.content <- paste0(page.content, "  <tr>\n    <td class=\"tdata leftalign summary\">AIC</td>")
    for (i in 1:length(input_list)) {
      # -------------------------
      # insert "separator column"
      # -------------------------
      page.content <- paste0(page.content, "<td class=\"separatorcol\">&nbsp;</td>")
      page.content <- paste0(page.content, sprintf("%s%.*f</td>", colspanstring, digits.summary, stats::AIC(input_list[[i]])))
    }
    page.content <- paste0(page.content, "\n  </tr>\n")
  }
  # -------------------------------------
  # Model-Summary: AICc
  # -------------------------------------
  if (showAICc) {
    page.content <- paste0(page.content, "  <tr>\n    <td class=\"tdata leftalign summary\">AICc</td>")
    for (i in 1:length(input_list)) {
      # -------------------------
      # insert "separator column"
      # -------------------------
      page.content <- paste0(page.content, "<td class=\"separatorcol\">&nbsp;</td>")
      page.content <- paste0(page.content, sprintf("%s%.*f</td>", colspanstring, digits.summary, AICcmodavg::AICc(input_list[[i]])))
    }
    page.content <- paste0(page.content, "\n  </tr>\n")
  }
  # -------------------------------------
  # Model-Summary: Chi2
  # -------------------------------------
  if (showChi2) {
    page.content <- paste0(page.content, "  <tr>\n    <td class=\"tdata leftalign summary\">&Chi;<sup>2</sup><sub>deviance</sub></td>")
    for (i in 1:length(input_list)) {
      # -------------------------
      # insert "separator column"
      # -------------------------
      page.content <- paste0(page.content, "<td class=\"separatorcol\">&nbsp;</td>")
      page.content <- paste0(page.content, gsub("0.",
                                                paste0(p_zero, "."),
                                                sprintf("%sp=%.*f</td>", 
                                                        colspanstring, 
                                                        digits.summary, 
                                                        Chisquare.glm(input_list[[i]])),
                                                fixed = TRUE))
    }
    page.content <- paste0(page.content, "\n  </tr>\n")
  }
  # -------------------------------------
  # Model-Summary: Chi-squared-GOF
  # -------------------------------------
  #   if (showGoF) {
  #     # -------------------------
  #     # not working for glmer
  #     # -------------------------
  #     if (lmerob) {
  #       warning("Chi-squared Goodness-of-Fit-test does not work for 'merMod'-objects.", call. = F)
  #     } else {
  #       page.content <- paste0(page.content, "  <tr>\n    <td class=\"tdata leftalign summary\">Pearson's &Chi;<sup>2</sup></td>")
  #       for (i in 1:length(input_list)) {
  #         # -------------------------
  #         # insert "separator column"
  #         # -------------------------
  #         page.content <- paste0(page.content, "<td class=\"separatorcol\">&nbsp;</td>")
  #         # -------------------------
  #         # compute Pearson's X2 GOF-test
  #         # -------------------------
  #         pgof <- sjmisc::chisq_gof(input_list[[i]])
  #         # -------------------------
  #         # print chisq and p
  #         # -------------------------
  #         page.content <- paste0(page.content, 
  #                                gsub("0.",
  #                                     paste0(p_zero, "."),
  #                                     sprintf("%s%.*f; p=%.*f</td>", 
  #                                             colspanstring, 
  #                                             digits.summary, 
  #                                             pgof$X2,
  #                                             digits.summary, 
  #                                             pgof$p.value),
  #                                     fixed = T))
  #       }
  #       page.content <- paste0(page.content, "\n  </tr>\n")
  #     }
  #   }
  # -------------------------------------
  # Model-Summary: Hosmer-Lemeshow-GOF
  # -------------------------------------
  if (showHosLem) {
    page.content <- paste0(page.content, "  <tr>\n    <td class=\"tdata leftalign summary\">Hosmer-Lemeshow-&Chi;<sup>2</sup></td>")
    for (i in 1:length(input_list)) {
      # -------------------------
      # insert "separator column"
      # -------------------------
      page.content <- paste0(page.content, "<td class=\"separatorcol\">&nbsp;</td>")
      # -------------------------
      # compute Hosmer-Lemeshow test
      # -------------------------
      hlgof <- sjmisc::hoslem_gof(input_list[[i]])
      # -------------------------
      # print chisq and p
      # -------------------------
      page.content <- paste0(page.content, 
                             gsub("0.",
                                  paste0(p_zero, "."),
                                  sprintf("%s%.*f; p=%.*f</td>", 
                                          colspanstring, 
                                          digits.summary, 
                                          unname(hlgof$chisq),
                                          digits.summary, 
                                          hlgof$p.value),
                                  fixed = T))
    }
    page.content <- paste0(page.content, "\n  </tr>\n")
  }
  # -------------------------------------
  # Model-Summary: Family
  # -------------------------------------
  if (showFamily) {
    page.content <- paste0(page.content, "  <tr>\n    <td class=\"tdata leftalign summary\">Family</td>")
    for (i in 1:length(input_list)) {
      # -------------------------
      # insert "separator column"
      # -------------------------
      page.content <- paste0(page.content, "<td class=\"separatorcol\">&nbsp;</td>")
      if (lmerob) {
        fam <- stats::family(input_list[[i]])
      } else {
        fam <- input_list[[i]]$family
      }
      page.content <- paste0(page.content, sprintf("%s%s (%s)</td>", colspanstring, fam$family, fam$link))
    }
    page.content <- paste0(page.content, "\n  </tr>\n")
  }
  # -------------------------------------
  # table footnote
  # -------------------------------------
  if (!pvaluesAsNumbers) page.content <- paste0(page.content, sprintf("  <tr>\n    <td class=\"tdata annorow\">Notes</td><td class=\"tdata annorow annostyle\" colspan=\"%i\"><em>* p&lt;%s.05&nbsp;&nbsp;&nbsp;** p&lt;%s.01&nbsp;&nbsp;&nbsp;*** p&lt;%s.001</em></td>\n  </tr>\n", headerColSpan, p_zero, p_zero, p_zero))
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
  invisible(structure(class = "sjtglm",
                      list(page.style = page.style,
                           page.content = page.content,
                           output.complete = toWrite,
                           knitr = knitr,
                           data = joined.df)))
}


#' @title Summary of generalized linear mixed models as HTML table
#' @name sjt.glmer
#' 
#' @description Summarizes (multiple) fitted generalized linear mixed models (odds ratios, ci, p-values...)
#'                as HTML table, or saves them as file. The fitted models may have different predictors,
#'                e.g. when comparing different stepwise fitted models.
#' 
#' @inheritParams sjt.glm
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
#' # create binary response
#' efc$hi_qol <- dicho(efc$quol_5)
#' # prepare group variable
#' efc$grp = as.factor(efc$e15relat)
#' levels(x = efc$grp) <- get_labels(efc$e15relat)
#' # data frame for fitted model
#' mydf <- data.frame(hi_qol = as.factor(efc$hi_qol),
#'                    sex = as.factor(efc$c161sex),
#'                    c12hour = as.numeric(efc$c12hour),
#'                    neg_c_7 = as.numeric(efc$neg_c_7),
#'                    education = as.factor(efc$c172code),
#'                    grp = efc$grp)
#'                    
#' # fit glmer
#' fit1 <- glmer(hi_qol ~ sex + c12hour + neg_c_7 + (1|grp),
#'               data = mydf,
#'               family = binomial("logit"))
#' fit2 <- glmer(hi_qol ~ sex + c12hour + neg_c_7 + education + (1|grp),
#'               data = mydf,
#'               family = binomial("logit"))
#'               
#' # print summary table
#' sjt.glmer(fit1, fit2,
#'           ci.hyphen = " to ")
#' 
#' # print summary table, using different table layout
#' sjt.glmer(fit1, fit2,
#'           showAIC = TRUE,
#'           showConfInt = FALSE,
#'           showStdError = TRUE,
#'           pvaluesAsNumbers = FALSE)
#'           
#' # print summary table
#' sjt.glmer(fit1, fit2,
#'           labelPredictors = c("Elder's gender (female)",
#'                               "Hours of care per week",
#'                               "Negative Impact",
#'                               "Educational level (mid)",
#'                               "Educational level (high)"))}
#' 
#' @export
sjt.glmer <- function(...,
                      file = NULL,
                      labelPredictors = NULL,
                      labelDependentVariables = NULL,
                      stringPredictors = "Predictors",
                      stringDependentVariables = "Dependent Variables",
                      showHeaderStrings = FALSE,
                      stringModel = "Model",
                      stringIntercept = "(Intercept)",
                      stringObservations = "Observations",
                      stringOR = "OR",
                      stringCI = "CI",
                      stringSE = "std. Error",
                      stringP = "p",
                      digits.est = 2,
                      digits.p = 3,
                      digits.ci = 2,
                      digits.se = 2,
                      digits.summary = 3,
                      exp.coef = TRUE,
                      pvaluesAsNumbers = TRUE,
                      boldpvalues = TRUE,
                      showConfInt = TRUE,
                      showStdError = FALSE,
                      ci.hyphen = "&nbsp;&ndash;&nbsp;",
                      separateConfColumn = TRUE,
                      newLineConf = TRUE,
                      showAbbrHeadline = TRUE,
                      showICC = TRUE,
                      showLogLik = FALSE,
                      showAIC = FALSE,
                      showAICc = FALSE,
                      showHosLem = FALSE,
                      showFamily = FALSE,
                      remove.estimates = NULL,
                      cellSpacing = 0.2,
                      encoding = NULL,
                      CSS = NULL,
                      useViewer = TRUE,
                      no.output = FALSE,
                      remove.spaces = TRUE) {
  
  input_list <- list(...)
  return(sjt.glm(input_list, file = file, labelPredictors = labelPredictors, 
                 labelDependentVariables = labelDependentVariables, stringPredictors = stringPredictors, 
                 stringDependentVariables = stringDependentVariables, showHeaderStrings = showHeaderStrings, 
                 stringModel = stringModel, stringIntercept = stringIntercept,
                 stringObservations = stringObservations, stringOR = stringOR,
                 stringCI = stringCI, stringSE = stringSE, stringP = stringP, 
                 digits.est = digits.est, digits.p = digits.p, digits.ci = digits.ci,
                 digits.se = digits.se, digits.summary = digits.summary, exp.coef = exp.coef,
                 pvaluesAsNumbers = pvaluesAsNumbers, boldpvalues = boldpvalues, 
                 showConfInt = showConfInt, showStdError = showStdError, 
                 ci.hyphen = ci.hyphen, separateConfColumn = separateConfColumn, newLineConf = newLineConf, 
                 group.pred = FALSE, showAbbrHeadline = showAbbrHeadline, showPseudoR = showICC, 
                 showLogLik = showLogLik, showAIC = showAIC, showAICc = showAICc, showChi2 = FALSE, 
                 showHosLem = showHosLem, showFamily = showFamily, remove.estimates = remove.estimates, 
                 cellSpacing = cellSpacing, cellGroupIndent = 0, encoding = encoding, 
                 CSS = CSS, useViewer = useViewer, no.output = no.output, remove.spaces = remove.spaces))
}