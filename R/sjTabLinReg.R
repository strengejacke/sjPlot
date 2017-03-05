#' @title Summary of linear regression as HTML table
#' @name sjt.lm
#'
#' @description Summarizes (multiple) fitted linear models (coefficients, std. beta values etc.)
#'                as HTML table, or saves them as file. The fitted models may have different predictors,
#'                e.g. when comparing different stepwise fitted models.
#'                This function also supports panel models fitted with the \code{plm}-function
#'                from the \pkg{plm}-package and generalized least squares models fitted with
#'                the \code{gls}-function from the \pkg{nlme}-package.
#'
#' @seealso \href{http://strengejacke.de/sjPlot/sjt.lm/}{sjPlot manual: sjt.lm}
#'
#' @param ... One or more fitted linear (mixed) models.
#' @param pred.labels Character vector with labels of predictor variables.
#'          If not \code{NULL}, \code{pred.labels} will be used in the first
#'          table column with the predictors' names. If \code{NULL}, variable
#'          labels are set based on label attributes (see \code{\link[sjmisc]{get_label}}).
#'          If \code{pred.labels = ""}, column names (vector names) are used
#'          as predictor labels. See 'Examples'.
#' @param depvar.labels Character vector with labels of dependent
#'          variables of all fitted models. See 'Examples'.
#' @param string.pred Character vector,used as headline for the predictor column.
#'          Default is \code{"Predictors"}.
#' @param string.dv Character vector, used as headline for the
#'          dependent variable columns. Default is \code{"Dependent Variables"}.
#' @param show.header Logical, if \code{TRUE}, the header strings \code{string.pred}
#'          and \code{string.dv} are shown. By default, they're hidden.
#' @param string.interc Character vector, used as headline for the Intercept row.
#'          Default is \code{"Intercept"}.
#' @param string.obs character vector, used in the summary row for the count of observation
#'          (cases). Default is \code{"Observations"}.
#' @param string.est Character vector, used for the column heading of estimates.
#' @param string.std Character vector, used for the column heading of standardized beta coefficients. Default is \code{"std. Beta"}.
#' @param string.ci Character vector, used for the column heading of confidence interval values. Default is \code{"CI"}.
#' @param string.se Character vector, used for the column heading of standard error values. Default is \code{"std. Error"}.
#' @param string.p Character vector, used for the column heading of p values. Default is \code{"p"}.
#' @param show.est Logical, if \code{TRUE} (default), the estimates are printed.
#' @param show.ci Logical, if \code{TRUE} (default), the confidence intervall is also printed to the table. Use
#'          \code{FALSE} to omit the CI in the table.
#' @param show.std Indicates whether standardized beta-coefficients should
#'          also printed, and if yes, which type of standardization is done.
#'          See 'Details'.
#' @param show.se Logical, if \code{TRUE}, the standard errors are also printed.
#'          Default is \code{FALSE}.
#' @param ci.hyphen Character vector, indicating the hyphen for confidence interval range.
#'          May be an HTML entity. See 'Examples'.
#' @param minus.sign string, indicating the minus sign for negative numbers.
#'          May be an HTML entity. See 'Examples'.
#' @param digits.est Amount of decimals for estimates
#' @param digits.p Amount of decimals for p-values
#' @param digits.ci Amount of decimals for confidence intervals
#' @param digits.se Amount of decimals for standard error
#' @param digits.std Amount of decimals for standardized beta
#' @param digits.summary Amount of decimals for values in model summary
#' @param emph.p Logical, if \code{TRUE}, significant p-values are shown bold faced.
#' @param p.zero logical, if \code{TRUE}, p-values have a leading 0 before the
#'          period (e.g. \emph{0.002}), else p-values start with a period and
#'          without a zero (e.g. \emph{.002}).
#' @param robust Logical, if \code{TRUE}, robust standard errors and confidence
#'          intervals will be reported. Computation of robust standard errors is
#'          based on the \code{\link[sjstats]{robust}}-function in the
#'          \pkg{sjstats}-package.
#' @param separate.ci.col Logical, if \code{TRUE}, the CI values are shown in a separate table column.
#'          Default is \code{FALSE}.
#' @param newline.ci Logical, if \code{TRUE} and \code{separate.ci.col = FALSE}, inserts a line break
#'          between estimate and CI values. If \code{FALSE}, CI values are printed in the same
#'          line as estimate values.
#' @param group.pred Logical, if \code{TRUE} (default), automatically groups table rows with
#'          factor levels of same factor, i.e. predictors of type \code{\link{factor}} will
#'          be grouped, if the factor has more than two levels. Grouping means that a separate headline
#'          row is inserted to the table just before the predictor values.
#' @param show.col.header Logical, if \code{TRUE} (default), the table data columns have a headline with
#'          abbreviations for estimates, std. beta-values, confidence interval and p-values.
#' @param show.r2 Logical, if \code{TRUE} (default), the R2 and adjusted R2 values for each model are printed
#'          in the model summary. For linear mixed models, the R2 and Omega-squared values are printed
#'          (see \code{\link[sjstats]{r2}} for details).
#' @param show.icc Logical, if \code{TRUE}, the intra-class-correlation for each
#'          model is printed in the model summary. Only applies to mixed models.
#' @param show.re.var Logical, if \code{TRUE}, the variance parameters for the random
#'          effects for each model are printed in the model summary. Only applies to mixed models.
#'          For details output, see 'Note' in \code{\link[sjstats]{icc}}.
#' @param show.fstat Logical, if \code{TRUE}, the F-statistics for each model is printed
#'          in the model summary. Default is \code{FALSE}. This argument does not apply to
#'          \code{\link{sjt.lmer}}.
#' @param show.aic Logical, if \code{TRUE}, the AIC value for each model is printed
#'          in the model summary. Default is \code{FALSE}.
#' @param show.aicc Logical, if \code{TRUE}, the second-order AIC value for each model
#'          is printed in the model summary. Default is \code{FALSE}.
#' @param show.dev Logical, if \code{TRUE}, the deviance for each model
#'          is printed in the model summary.
#' @param remove.estimates Numeric vector with indices (order equals to row index of \code{coef(fit)})
#'          or character vector with coefficient names that indicate which estimates should be removed
#'          from the table output. The first estimate is the intercept, followed by the model predictors.
#'          \emph{The intercept cannot be removed from the table output!} \code{remove.estimates = c(2:4)}
#'          would remove the 2nd to the 4th estimate (1st to 3rd predictor after intercept) from the output.
#'          \code{remove.estimates = "est_name"} would remove the estimate \emph{est_name}. Default
#'          is \code{NULL}, i.e. all estimates are printed.
#' @param cell.spacing Numeric, inner padding of table cells. By default, this value is 0.2 (unit is cm), which is
#'          suitable for viewing the table. Decrease this value (0.05 to 0.1) if you want to import the table
#'          into Office documents. This is a convenient argument for the \code{CSS} argument for changing
#'          cell spacing, which would be: \code{CSS = list(css.thead = "padding:0.2cm;", css.tdata = "padding:0.2cm;")}.
#' @param cell.gpr.indent Indent for table rows with grouped factor predictors. Only applies
#'          if \code{group.pred = TRUE}.
#' @param sep.column Logical, if \code{TRUE}, an empty table column is added after
#'          each model column, to add margins between model columns. By default, this
#'          column will be added to the output; however, when copying tables to
#'          office applications, it might be helpful not to add this separator column
#'          when modifying the table layout.
#'
#' @inheritParams sjt.frq
#' @inheritParams sjp.lmer
#' @inheritParams sjp.corr
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
#' @details Concerning the \code{show.std} argument, \code{show.std = "std"}
#'            will print normal standardized estimates. For \code{show.std = "std2"},
#'            however, standardization of estimates follows
#'            \href{http://www.stat.columbia.edu/~gelman/research/published/standardizing7.pdf}{Gelman's (2008)}
#'            suggestion, rescaling the estimates by dividing them by two standard
#'            deviations instead of just one. Resulting coefficients are then
#'            directly comparable for untransformed binary predictors. This type
#'            of standardization uses the \code{\link[arm]{standardize}}-function
#'            from the \pkg{arm}-package.
#'            For backward compatibility reasons, \code{show.std} also may be
#'            a logical value; if \code{TRUE}, normal standardized estimates are
#'            printed (same effect as \code{show.std = "std"}). Use
#'            \code{show.std = NULL} (default) or \code{show.std = FALSE},
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
#' sjt.lm(fit1, fit2,
#'        depvar.labels = c("Barthel-Index", "Negative Impact"),
#'        pred.labels = c("Carer's Age", "Hours of Care",
#'                        "Carer's Sex", "Educational Status"))
#'
#' # use vector names as labels
#' sjt.lm(fit1, fit2, pred.labels = "")
#'
#' # show HTML-table, indicating p-values as asterisks
#' sjt.lm(fit1, fit2, show.std = TRUE, p.numeric = FALSE)
#'
#' # create and open HTML-table in RStudio Viewer Pane or web browser,
#' # integrate CI in estimate column
#' sjt.lm(fit1, fit2, separate.ci.col = FALSE)
#'
#' # show HTML-table, indicating p-values as numbers
#' # and printing CI in a separate column
#' sjt.lm(fit1, fit2, show.std = TRUE)
#'
#' # show HTML-table, indicating p-values as stars
#' # and integrate CI in estimate column
#' sjt.lm(fit1, fit2, show.std = TRUE, ci.hyphen = " to ",
#'        minus.sign = "&minus;", p.numeric = FALSE,
#'        separate.ci.col = FALSE)
#'
#' # ----------------------------------
#' # connecting two html-tables
#' # ----------------------------------
#' # fit two more models
#' fit3 <- lm(tot_sc_e ~ c160age + c12hour + c161sex + c172code, data=efc)
#' fit4 <- lm(e42dep ~ c160age + c12hour + c161sex + c172code, data=efc)
#'
#' # create and save first HTML-table
#' part1 <- sjt.lm(fit1, fit2)
#'
#' # create and save second HTML-table
#' part2 <- sjt.lm(fit3, fit4)
#'
#' # browse temporary file
#' htmlFile <- tempfile(fileext=".html")
#' write(sprintf("<html><head>%s</head><body>%s<p></p>%s</body></html>",
#'               part1$page.style, part1$page.content, part2$page.content),
#'       file = htmlFile)
#' viewer <- getOption("viewer")
#' if (!is.null(viewer)) viewer(htmlFile) else utils::browseURL(htmlFile)
#'
#' # ----------------------------------
#' # User defined style sheet
#' # ----------------------------------
#' sjt.lm(fit1, fit2,
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
#' sjt.lm(fit1, fit2, group.pred = TRUE)
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
#' # ----------------------------------------
#' # compare models with different predictors
#' # and grouping
#' # ----------------------------------------
#' # make cope-index categorical
#' efc$c82cop1 <- to_factor(efc$c82cop1)
#' # fit another model
#' fit4 <- lm(neg_c_7 ~ c160age + c172code + e42dep + tot_sc_e + c82cop1,
#'            data = efc)
#'
#' sjt.lm(fit1, fit2, fit4, fit3)
#'
#' # show standardized beta only
#' sjt.lm(fit1, fit2, fit4, fit3, show.est = FALSE, show.std = TRUE,
#'        show.aic = TRUE, show.fstat = TRUE)
#'
#' # -----------------------------------------------------------
#' # color insanity. just to show that each column has an own
#' # CSS-tag, so - depending on the stats and values you show -
#' # you can define column spaces / margins, border etc. to
#' # visually separate your models in the table
#' # -----------------------------------------------------------
#' sjt.lm(fit1, fit2, fit4, fit3, show.std = TRUE, show.aic = TRUE,
#'        show.fstat = TRUE, show.se = TRUE,
#'        CSS = list(css.modelcolumn1 = 'color:blue;',
#'                   css.modelcolumn2 = 'color:red;',
#'                   css.modelcolumn3 = 'color:green;',
#'                   css.modelcolumn4 = 'color:#ffff00;',
#'                   css.modelcolumn5 = 'color:#777777;',
#'                   css.modelcolumn6 = 'color:#3399cc;',
#'                   css.modelcolumn7 = 'color:#cc9933;'))
#'
#' sjt.lm(fit1, fit2, fit4, fit3, show.est = FALSE, show.std = TRUE,
#'        p.numeric = FALSE, group.pred = FALSE,
#'        CSS = list(css.modelcolumn4 = 'border-left:1px solid black;',
#'                   css.modelcolumn5 = 'padding-right:50px;'))}
#'
#' @importFrom dplyr full_join slice bind_cols select_ rename_
#' @importFrom stats nobs AIC confint coef deviance
#' @importFrom lme4 VarCorr
#' @importFrom sjstats std_beta icc r2 cod chisq_gof hoslem_gof get_model_pval robust
#' @importFrom tibble lst add_row add_column
#' @importFrom broom tidy
#' @export
sjt.lm <- function(...,
                   pred.labels = NULL,
                   depvar.labels = NULL,
                   remove.estimates = NULL,
                   group.pred = TRUE,
                   p.numeric = TRUE,
                   emph.p = FALSE,
                   p.zero = FALSE,
                   p.kr = TRUE,
                   robust = FALSE,
                   separate.ci.col = TRUE,
                   newline.ci = TRUE,
                   show.est = TRUE,
                   show.std = NULL,
                   show.ci = TRUE,
                   show.se = FALSE,
                   show.header = FALSE,
                   show.col.header = TRUE,
                   show.r2 = TRUE,
                   show.icc = FALSE,
                   show.re.var = FALSE,
                   show.fstat = FALSE,
                   show.aic = FALSE,
                   show.aicc = FALSE,
                   show.dev = FALSE,
                   string.pred = "Predictors",
                   string.dv = "Dependent Variables",
                   string.interc = "(Intercept)",
                   string.obs = "Observations",
                   string.est = "B",
                   string.std = "std. Beta",
                   string.ci = "CI",
                   string.se = "std. Error",
                   string.p = "p",
                   ci.hyphen = "&nbsp;&ndash;&nbsp;",
                   minus.sign = "&#45;",
                   digits.est = 2,
                   digits.std = 2,
                   digits.p = 3,
                   digits.ci = 2,
                   digits.se = 2,
                   digits.summary = 3,
                   cell.spacing = 0.2,
                   cell.gpr.indent = 0.6,
                   sep.column = TRUE,
                   CSS = NULL,
                   encoding = NULL,
                   file = NULL,
                   use.viewer = TRUE,
                   no.output = FALSE,
                   remove.spaces = TRUE) {
  # --------------------------------------------------------
  # check p-value-style option
  # --------------------------------------------------------
  if (!p.zero)
    p_zero <- ""
  else
    p_zero <- "0"
  # -------------------------------------
  # check arguments
  # -------------------------------------
  if (is.null(show.std) || show.std == FALSE)
    showStdBetaValues <- FALSE
  else
    showStdBetaValues <- TRUE
  # if show.std is logical, replace with equivalent character value
  if (is.null(show.std) || show.std != "std2") show.std <- "std"
  # check if any estimates should be plotted?
  if (!show.est && !showStdBetaValues) {
    warning("Either estimates (`show.est`) or standardized betas (`show.std`) must be shown in table. Setting `show.est` to `TRUE`.", call. = F)
    show.est <- TRUE
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
  # get table header
  # ------------------------
  toWrite <- get_table_header(encoding, cell.spacing, cell.gpr.indent, p.numeric, show.header, CSS)
  # ------------------------
  # retrieve fitted models
  # ------------------------
  input_list <- tibble::lst(...)
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
      stop("Package `plm` needs to be loaded for this function to work. Use `library(plm)` and call this function again.", call. = FALSE)
    }
  }
  # ------------------------
  # do we have mixed models?
  # ------------------------
  lmerob <- inherits(input_list[[1]], c("lmerMod", "merModLmerTest"))
  # ------------------------
  # should AICc be computed? Check for package
  # ------------------------
  if (show.aicc && !requireNamespace("AICcmodavg", quietly = TRUE)) {
    warning("Package `AICcmodavg` needed to show AICc. Argument `show.aicc` will be ignored.", call. = FALSE)
    show.aicc <- FALSE
  }
  # ------------------------
  # check for stepwise models, when fitted models
  # are mixed effects models
  # ------------------------
  if (lmerob) {
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
    show.icc <- FALSE
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
  for (i in seq_len(length(input_list))) {
    # -------------------------------------
    # get tidy model summary
    # -------------------------------------
    if (robust) {
      fit.df <- sjstats::robust(input_list[[i]], conf.int = T) %>%
        dplyr::select_("-statistic")
    } else {
      fit.df <- broom::tidy(input_list[[i]], effects = "fixed", conf.int = T) %>%
        dplyr::select_("-statistic")
    }
    # -------------------------------------
    # check for p-value colum
    # -------------------------------------
    if (!sjmisc::str_contains(colnames(fit.df), "p.value")) {
      fit.df <- tibble::add_column(
        .data = fit.df,
        p.value = sjstats::get_model_pval(input_list[[i]], p.kr)[["p.value"]],
        .before = "conf.low"
      )
    }
    # -------------------------------------
    # get standardized values
    # -------------------------------------
    sbvals <- suppressWarnings(sjstats::std_beta(input_list[[i]], type = show.std))
    if (!lmerob) sbvals <- tibble::add_row(.data = sbvals, term = "(Intercept)", .before = 1)
    # -------------------------------------
    # bind std. values to data frame
    # -------------------------------------
    fit.df <- dplyr::bind_cols(
      fit.df,
      sbvals %>%
        dplyr::select_("-term") %>%
        dplyr::rename_("std.conf.low" = "conf.low",
                       "std.conf.high" = "conf.high",
                       "std.std.error" = "std.error")
      )
    # -------------------------------------
    # formate values
    # -------------------------------------
    fit.df$estimate <- sprintf("%.*f", digits.est, fit.df$estimate)
    fit.df$conf.low <- sprintf("%.*f", digits.ci, fit.df$conf.low)
    fit.df$conf.high <- sprintf("%.*f", digits.ci, fit.df$conf.high)
    fit.df$std.error <- sprintf("%.*f", digits.se, fit.df$std.error)
    fit.df$std.estimate <- sprintf("%.*f", digits.est, fit.df$std.estimate)
    fit.df$std.conf.low <- sprintf("%.*f", digits.ci, fit.df$std.conf.low)
    fit.df$std.conf.high <- sprintf("%.*f", digits.ci, fit.df$std.conf.high)
    fit.df$std.std.error <- sprintf("%.*f", digits.se, fit.df$std.std.error)
    # -------------------------------------
    # prepare p-values, either as * or as numbers
    # -------------------------------------
    if (!p.numeric) {
      fit.df$p.value <- sapply(fit.df$p.value, get_p_stars)
    } else {
      if (emph.p) {
        sb1 <- "<b>"
        sb2 <- "</b>"
      } else {
        sb1 <- sb2 <- ""
      }
      fit.df$p.value <- sapply(fit.df$p.value, function(x) {
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
                          sprintf("se%i", i),
                          sprintf("p-value%i", i),
                          sprintf("ci.lo%i", i),
                          sprintf("ci.hi%i", i),
                          sprintf("std.beta%i", i),
                          sprintf("std.se%i", i),
                          sprintf("std.beta.ci.lo%i", i),
                          sprintf("std.beta.ci.hi%i", i))
    # -------------------------------------
    # add to df list
    # -------------------------------------
    df.fit[[length(df.fit) + 1]] <- as.data.frame(fit.df)
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
  for (i in seq_len(ncol(joined.df))) {
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
      for (re in seq_len(length(remove.estimates))) {
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
    rowind <- seq_len(nrow(joined.df))
    # "inverse" removable inices
    keep.estimates <- rowind[-remove.estimates]
    # select rows
    joined.df <- dplyr::slice(joined.df, keep.estimates)
  }
  # -------------------------------------
  # if confidence interval should be omitted,
  # don't use separate column for CI!
  # -------------------------------------
  if (!show.ci) {
    separate.ci.col <- FALSE
    showCIString <- string.est
    showCIStringSB <- string.std
  } else {
    showCIString <- sprintf("%s (%s)", string.est, string.ci)
    showCIStringSB <- sprintf("%s (%s)", string.std, string.ci)
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
  if (!show.est) headerColSpanFactor <- 0
  if (!show.est && separate.ci.col) headerColSpanFactor <- -1
  if (p.numeric) headerColSpanFactor <- headerColSpanFactor + 1
  if (separate.ci.col) headerColSpanFactor <- headerColSpanFactor + 1
  if (showStdBetaValues) headerColSpanFactor <- headerColSpanFactor + 1
  if (showStdBetaValues && separate.ci.col) headerColSpanFactor <- headerColSpanFactor + 1
  if (show.est && show.se) headerColSpanFactor <- headerColSpanFactor + 1
  if (showStdBetaValues && show.se) headerColSpanFactor <- headerColSpanFactor + 1
  # now that we know how many columns each model needs,
  # we multiply columns per model with count of models, so we have
  # the column span over all models together
  headerColSpan <- headerColSpanFactor * headerColSpan
  # furthermore, we add count of models  to the overall column span, if
  # each model is separated with an empty table column
  if (sep.column) headerColSpan <- headerColSpan + length(input_list)
  linebreakstring <- " "
  if (newline.ci) linebreakstring <- "<br>"
  # -------------------------------------
  # start table tag
  # -------------------------------------
  page.content <- "<table>"
  # -------------------------------------
  # check if we want to see header strings
  # -------------------------------------
  if (show.header) {
    page.content <- paste0(page.content, sprintf("\n  <tr>\n    <td class=\"tdata topborder\" rowspan=\"2\"><em>%s</em></td>", string.pred))
    page.content <- paste0(page.content, sprintf("\n    <td colspan=\"%i\" class=\"tdata topborder depvarhead\"><em>%s</em></td>\n  </tr>\n", headerColSpan, string.dv))
  } else {
    # first column is empty
    page.content <- paste0(page.content,"\n    <td class=\"tdata topborder\">&nbsp;</td>")
  }
  # -------------------------------------
  # If we don't show header strings, a rowspan-attribute is missing,
  # so we need to insert an empty cell here
  # -------------------------------------
  if (show.header)
    tcp <- ""
  else
    tcp <- " topborder"
  # -------------------------------------
  # set default dependent var label
  # -------------------------------------
  gtrl <- get_table_response_label(page.content, depvar.labels, input_list, tcp, headerColSpanFactor, sep.column)
  page.content <- gtrl$page.content
  depvar.labels <- gtrl$depvar.labels
  # -------------------------------------
  # define column constants for joined.df
  # -------------------------------------
  COL_EST <- 2
  COL_SE <- 3
  COL_P <- 4
  COL_CI_LOW <- 5
  COL_CI_HIGH <- 6
  COL_STD_EST <- 7
  COL_STD_SE <- 8
  COL_STD_CI_LOW <- 9
  COL_STD_CI_HIGH <- 10
  # -------------------------------------
  # table header: or/ci and p-labels
  # -------------------------------------
  if (show.col.header) {
    page.content <- paste0(page.content, "\n  <tr>\n    <td class=\"tdata colnames\">&nbsp;</td>")
    colnr <- ifelse(is.null(depvar.labels), length(input_list), length(depvar.labels))
    for (i in seq_len(colnr)) {
      # -------------------------
      # insert "separator column"
      # -------------------------
      if (sep.column) page.content <- paste0(page.content, "\n    <td class=\"separatorcol colnames\">&nbsp;</td>")
      # confidence interval in separate column
      if (show.est) {
        if (separate.ci.col) {
          page.content <- paste0(page.content, sprintf("\n    <td class=\"tdata centeralign colnames modelcolumn1\">%s</td>", string.est))
          if (show.ci) page.content <- paste0(page.content, sprintf("\n    <td class=\"tdata centeralign colnames modelcolumn2\">%s</td>", string.ci))
        } else {
          # confidence interval in Beta-column
          page.content <- paste0(page.content, sprintf("\n    <td class=\"tdata centeralign colnames modelcolumn1\">%s</td>", showCIString))
        }
        # show std. error
        if (show.se) page.content <- paste0(page.content, sprintf("\n    <td class=\"tdata centeralign colnames modelcolumn3\">%s</td>", string.se))
      }
      # show std. beta
      if (showStdBetaValues) {
        # confidence interval in separate column
        if (separate.ci.col) {
          page.content <- paste0(page.content, sprintf("\n    <td class=\"tdata centeralign colnames modelcolumn4\">%s</td>", string.std))
          if (show.ci) page.content <- paste0(page.content, sprintf("\n    <td class=\"tdata centeralign colnames modelcolumn5\">%s</td>", string.ci))
        } else {
          # confidence interval in Beta-column
          page.content <- paste0(page.content, sprintf("\n    <td class=\"tdata centeralign colnames modelcolumn4\">%s</td>", showCIStringSB))
        }
        # show std. error
        if (show.se) page.content <- paste0(page.content, sprintf("\n    <td class=\"tdata centeralign colnames modelcolumn6\">%s</td>", string.se))
      }
      # show p-values as numbers in separate column
      if (p.numeric) page.content <- paste0(page.content, sprintf("\n    <td class=\"tdata centeralign colnames modelcolumn7\">%s</td>", string.p))
    }
    page.content <- paste(page.content, "\n  </tr>\n")
  }
  # -------------------------------------
  # set default predictor labels
  # -------------------------------------
  if (is.null(pred.labels)) {
    pred.labels <- suppressWarnings(retrieveModelLabels(input_list, group.pred = group.pred))
    # remove labels from removed estimates. we need "-1" here, because removed
    # estimates start counting with the intercept, while predictor label counting
    # starts with first estimate after intercept
    if (!is.null(keep.estimates)) pred.labels <- pred.labels[keep.estimates - 1]
  }
  # --------------------------------------------------------
  # auto-retrieving variable labels does not work when we
  # have factors with different levels, which appear as
  # "multiple predictors", but are only one variable
  # --------------------------------------------------------
  if (is.null(pred.labels) || length(pred.labels) < (nrow(joined.df) - 1)) {
    pred.labels <- joined.df[-1, 1]
  }
  # -------------------------------------
  # should factor predictors be grouped?
  # -------------------------------------
  if (group.pred) {
    # get indices
    group.pred.list <- retrieveModelGroupIndices(input_list, remove.estimates)
    # append indices
    group.pred.rows <- group.pred.list[[1]]
    group.pred.span <- group.pred.list[[2]]
    group.pred.labs <- group.pred.list[[3]]
    # if we have also stepwise models, grouping may
    # not work properly
    if (sw.fit) message("Fitted models have different coefficients. Grouping may not work properly. Set `group.pred = FALSE` if you encouter cluttered labelling.")
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
                                               tcb_class, string.interc))
  for (i in seq_len(length(input_list))) {
    # -------------------------
    # insert "separator column"
    # -------------------------
    if (sep.column) page.content <- paste0(page.content, sprintf("\n    <td class=\"separatorcol %s\">&nbsp;</td>", tcb_class))
    # show estimates?
    if (show.est) {
      # open table cell for Beta-coefficient
      page.content <- paste0(page.content, sprintf("\n    <td class=\"tdata centeralign %smodelcolumn1\">%s",
                                                   tcb_class, joined.df[1, (i - 1) * 9 + COL_EST]))
      # confidence interval in separate column
      if (separate.ci.col) {
        # if p-values are not shown as numbers, insert them after beta-value
        if (!p.numeric) page.content <- paste0(page.content, sprintf("&nbsp;%s", joined.df[1, (i - 1) * 9 + COL_P]))
        # if we have CI, start new table cell (CI in separate column)
        if (show.ci) {
          page.content <- table_cell_string(page.content, "</td>\n    ", tcb_class, 2,
                                            paste(c(joined.df[1, (i - 1) * 9 + COL_CI_LOW],
                                                    ci.hyphen, joined.df[1, (i - 1) * 9 + COL_CI_HIGH]), collapse = ""))
        } else {
          page.content <- paste0(page.content, "</td>")
        }
      } else {
        # confidence interval in Beta-column
        if (show.ci) page.content <- paste0(page.content, sprintf("%s(%s%s%s)", linebreakstring, joined.df[1, (i - 1) * 9 + COL_CI_LOW],
                                                                      ci.hyphen,joined.df[1, (i - 1) * 9 + COL_CI_HIGH]))
        # if p-values are not shown as numbers, insert them after beta-value
        if (!p.numeric) page.content <- paste0(page.content, sprintf("&nbsp;%s", joined.df[1, (i - 1) * 9 + COL_P]))
        page.content <- paste0(page.content, "</td>")
      }
      # show std. error
      if (show.se) page.content <- table_cell_string(page.content, "\n    ", tcb_class, 3, joined.df[1, (i - 1) * 9 + COL_SE])
    }
    # show std. beta
    if (showStdBetaValues) page.content <- table_cell_string(page.content, "\n    ", tcb_class, 4, "&nbsp;")
    # show std. beta
    if (showStdBetaValues && show.ci && separate.ci.col) page.content <- table_cell_string(page.content, "\n    ", tcb_class, 5, "&nbsp;")
    # show std. beta std. error
    if (showStdBetaValues && show.se) page.content <- table_cell_string(page.content, "\n    ", tcb_class, 6, "&nbsp;")
    # show p-values as numbers in separate column
    if (p.numeric)
      # if we don't have estimates, intercept is not available. so don't show p-value here
      page.content <- table_cell_string(page.content, "\n    ", tcb_class, 7, ifelse(isTRUE(show.est), joined.df[1, (i - 1) * 9 + COL_P], "&nbsp;"))
  }
  page.content <- paste0(page.content, "\n  </tr>")
  # -------------------------------------
  # subsequent rows: predictors
  # -------------------------------------
  for (i in seq_len((nrow(joined.df) - 1))) {
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
    page.content <- paste0(page.content, "\n  <tr>\n", sprintf("    <td class=\"%s leftalign\">%s</td>", indent.tag, pred.labels[i]))
    # ---------------------------------------
    # go through fitted model's statistics
    # ---------------------------------------
    for (j in seq_len(length(input_list))) {
      # -------------------------
      # insert "separator column"
      # -------------------------
      if (sep.column) page.content <- paste0(page.content, "\n    <td class=\"separatorcol\">&nbsp;</td>")
      # show estimates?
      if (show.est) {
        # retieve lower and upper ci
        ci.lo <- joined.df[i + 1, (j - 1) * 9 + COL_CI_LOW]
        ci.hi <- joined.df[i + 1, (j - 1) * 9 + COL_CI_HIGH]
        # if we have empty cells (due to different predictors in models)
        # we don't print CI-separator strings and we don't print any esitmate
        # values - however, for proper display, we fill these values with "&nbsp;"
        ci.sep.string <- ifelse(sjmisc::is_empty(ci.lo), "&nbsp;", ci.hyphen)
        # open table cell for Beta-coefficient
        page.content <- paste0(page.content, sprintf("\n    <td class=\"tdata centeralign modelcolumn1\">%s",
                                                     joined.df[i + 1, (j - 1) * 9 + COL_EST]))
        # confidence interval in separate column
        if (separate.ci.col) {
          # if p-values are not shown as numbers, insert them after beta-value
          if (!p.numeric) page.content <- paste0(page.content, sprintf("&nbsp;%s", joined.df[i + 1, (j - 1) * 9 + COL_P]))
          # if we have CI, start new table cell (CI in separate column)
          if (show.ci) {
            page.content <- paste0(page.content, sprintf("</td>\n    <td class=\"tdata centeralign modelcolumn2\">%s%s%s</td>",
                                                         ci.lo, ci.sep.string, ci.hi))
          } else {
            page.content <- paste0(page.content, "</td>")
          }
        } else {
          # confidence interval in Beta-column
          if (show.ci && !sjmisc::is_empty(ci.lo)) page.content <- paste0(page.content,
                                                                              sprintf("%s(%s%s%s)",
                                                                                      linebreakstring,
                                                                                      ci.lo,
                                                                                      ci.sep.string,
                                                                                      ci.hi))
          # if p-values are not shown as numbers, insert them after beta-value
          if (!p.numeric) page.content <- paste0(page.content,
                                                        sprintf("&nbsp;%s",
                                                                joined.df[i + 1, (j - 1) * 9 + COL_P]))
          page.content <- paste0(page.content, "</td>")
        }
        # show std. error
        if (show.se) page.content <- table_cell_string(page.content, "\n    ", "", 3, joined.df[i + 1, (j - 1) * 9 + COL_SE])
      }
      # show std. beta
      if (showStdBetaValues) {
        # retieve lower and upper ci
        ci.lo <- joined.df[i + 1, (j - 1) * 9 + COL_STD_CI_LOW]
        ci.hi <- joined.df[i + 1, (j - 1) * 9 + COL_STD_CI_HIGH]
        # if we have empty cells (due to different predictors in models)
        # we don't print CI-separator strings and we don't print any esitmate
        # values - however, for proper display, we fill these values with "&nbsp;"
        ci.sep.string <- ifelse(sjmisc::is_empty(ci.lo), "&nbsp;", ci.hyphen)
        # open table cell for Std. Beta-coefficient
        page.content <- paste0(page.content, sprintf("\n    <td class=\"tdata centeralign modelcolumn4\">%s", joined.df[i + 1, (j - 1) * 9 + COL_STD_EST]))
        if (separate.ci.col) {
          # show pvalue stars, if no estimates are shown
          if (!p.numeric && !show.est) page.content <- paste0(page.content, sprintf("&nbsp;%s", joined.df[i + 1, (j - 1) * 9 + COL_P]))
          # if we have CI, start new table cell (CI in separate column)
          if (show.ci) {
            page.content <- paste0(page.content, sprintf("</td>\n    <td class=\"tdata centeralign modelcolumn5\">%s%s%s</td>",
                                                         ci.lo,
                                                         ci.sep.string,
                                                         ci.hi))
          } else {
            page.content <- paste0(page.content, "</td>")
          }
        } else {
          # show pvalue stars, if no estimates are shown
          if (!p.numeric && !show.est) page.content <- paste0(page.content, sprintf("&nbsp;%s", joined.df[i + 1, (j - 1) * 9 + COL_P]))
          # confidence interval in Beta-column
          if (show.ci && !sjmisc::is_empty(ci.lo)) page.content <- paste0(page.content, sprintf("%s(%s%s%s)",
                                                                                                    linebreakstring,
                                                                                                    ci.lo,
                                                                                                    ci.sep.string,
                                                                                                    ci.hi))
          # if p-values are not shown as numbers, insert them after beta-value
          page.content <- paste0(page.content, "</td>")
        }
        # show std. error
        if (show.se) page.content <- table_cell_string(page.content, "\n    ", "", 6, joined.df[i + 1, (j - 1) * 9 + COL_STD_SE])
      }
      # show p-values as numbers in separate column
      if (p.numeric) page.content <- table_cell_string(page.content, "\n    ", "", 7, joined.df[i + 1, (j - 1) * 9 + COL_P])
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
    # -------------------------------------
    # we need to know max amount of groups
    # -------------------------------------
    all_mm_counts <- unlist(lapply(input_list, function(x) length(lme4::getME(x, "flist"))))
    # retrieve maximum random intercepts
    mmcount <- max(all_mm_counts)
    # get random intercepts from model with most intercepts
    mmgrps <- lme4::getME(input_list[[which.max(all_mm_counts)]], "flist")
    # get icc for all models
    all_icc <- lapply(input_list, sjstats::icc)
    # -------------------------------------
    # show variance components?
    # -------------------------------------
    if (show.re.var) {
      # -------------------------------------
      # lets check which mdoels have random slopes, needed later
      # -------------------------------------
      has_rnd_slope <- unlist(lapply(input_list, function(mo) {
        lapply(lme4::VarCorr(mo), function(x) dim(attr(x, "correlation"))[1] > 1)
      }))
      # -------------------------
      # first, within-group variance
      # -------------------------
      page.content <- paste0(page.content, "\n  <tr>\n    <td class=\"tdata summary leftalign\">&sigma;<sup>2</sup></td>\n")
      # iterate models
      for (i in seq_len(length(input_list))) {
        # -------------------------
        # insert "separator column"
        # -------------------------
        if (sep.column) page.content <- paste0(page.content, "<td class=\"separatorcol\">&nbsp;</td>")
        page.content <- paste0(page.content, colspanstring, sprintf("%.*f", digits.summary, attr(all_icc[[i]], "sigma_2", exact = T)), "</td>\n")
      }
      page.content <- paste0(page.content, "  </tr>\n")
      # -------------------------
      # next, between-group variance
      # -------------------------
      # first models indicates grouping levels. we have to assume comparable models
      # with same random intercepts.
      for (gl in seq_len(mmcount)) {
        page.content <- paste0(page.content,
                               sprintf("\n  <tr>\n    <td class=\"tdata summary leftalign\">&tau;<sub>00, %s</sub></td>\n",
                                       names(mmgrps[gl])))
        # iterate models
        for (i in seq_len(length(input_list))) {
          # -------------------------
          # insert "separator column"
          # -------------------------
          if (sep.column) page.content <- paste0(page.content, "<td class=\"separatorcol\">&nbsp;</td>")
          # get random intercept variance
          tau.00 <- attr(all_icc[[i]], "tau.00", exact = T)
          if (length(tau.00) >= gl) {
            rand.int.var <- paste0(sprintf("%.*f", digits.summary, tau.00[gl], collapse = ""))
            page.content <- paste0(page.content, colspanstring, rand.int.var, "</td>\n")

          } else {
            page.content <- paste(page.content, sprintf("   %s&nbsp;</td>\n", colspanstring))
          }
        }
        page.content <- paste0(page.content, "  </tr>\n")
      }
      # -------------------------
      # finally, random slope intercept correlation
      # -------------------------
      if (any(has_rnd_slope)) {
        # iterate final models
        page.content <- paste0(page.content, "\n  <tr>\n    <td class=\"tdata summary leftalign\">&rho;<sub>01</sub></td>\n")
        # iterate models
        for (i in seq_len(length(input_list))) {
          # -------------------------
          # insert "separator column"
          # -------------------------
          if (sep.column) page.content <- paste0(page.content, "<td class=\"separatorcol\">&nbsp;</td>")
          # does model have random slope?
          if (has_rnd_slope[i]) {
            # get slope-intercept correlation
            page.content <- paste0(page.content, colspanstring, sprintf("%.*f", digits.summary, attr(all_icc[[i]], "rho.01", exact = T)), "</td>\n")
          } else {
            page.content <- paste(page.content, sprintf("   %s&nbsp;</td>\n", colspanstring))
          }
        }
        page.content <- paste0(page.content, "  </tr>\n")
      }
    }
    # -------------------------------------
    # N of grouping levels
    # -------------------------------------
    # first models indicates grouping levels. we have to assume comparable models
    # with same random intercepts.
    for (gl in seq_len(mmcount)) {
      page.content <- paste0(page.content, sprintf("\n  <tr>\n    <td class=\"tdata summary leftalign\">N<sub>%s</sub></td>\n", names(mmgrps[gl])))
      # iterate models
      for (i in 1:length(input_list)) {
        # -------------------------
        # insert "separator column"
        # -------------------------
        if (sep.column) page.content <- paste0(page.content, "<td class=\"separatorcol\">&nbsp;</td>")
        # retrieve random intercepts of each model
        sub.mmgrps <- lme4::getME(input_list[[i]], "flist")
        # does model have enough random intercepts?
        # if yes, print
        if (length(sub.mmgrps) >= gl) {
          page.content <- paste(page.content, sprintf("   %s%i</td>\n",
                                                      colspanstring,
                                                      nlevels(sub.mmgrps[[gl]])))
        } else {
          page.content <- paste(page.content, sprintf("   %s&nbsp;</td>\n", colspanstring))
        }
      }
      page.content <- paste0(page.content, "  </tr>\n")
    }
    # -------------------------------------
    # Model-Summary: icc
    # -------------------------------------
    if (show.icc) {
      # get icc from models
      summary.icc <- all_icc[[which.max(all_mm_counts)]]
      # iterate icc's
      for (si in seq_len(mmcount)) {
        page.content <- paste0(page.content, sprintf("  <tr>\n    <td class=\"tdata leftalign summary\">ICC<sub>%s</sub></td>\n", names(summary.icc[si])))
        # iterate models
        for (i in 1:length(input_list)) {
          # -------------------------
          # insert "separator column"
          # -------------------------
          if (sep.column) page.content <- paste0(page.content, "<td class=\"separatorcol\">&nbsp;</td>")
          # get icc from models
          sub.summary.icc <- all_icc[[i]]
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
  page.content <- paste0(page.content, sprintf("\n  <tr>\n    <td class=\"tdata summary leftalign firstsumrow\">%s</td>\n", string.obs))
  for (i in 1:length(input_list)) {
    # -------------------------
    # insert "separator column"
    # -------------------------
    if (sep.column) page.content <- paste0(page.content, "<td class=\"separatorcol firstsumrow\">&nbsp;</td>")
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
  if (show.r2) {
    # first, we need the correct description for 2nd r2-value
    if (lmerob)
      r2string <- "&Omega;<sub>0</sub><sup>2</sup>"
    else
      r2string <- "adj. R<sup>2</sup>"

    page.content <- paste0(page.content, sprintf("  <tr>\n    <td class=\"tdata leftalign summary\">R<sup>2</sup> / %s</td>\n", r2string))
    for (i in 1:length(input_list)) {
      # -------------------------
      # insert "separator column"
      # -------------------------
      if (sep.column) page.content <- paste0(page.content, "\n    <td class=\"separatorcol\">&nbsp;</td>")
      # -------------------------
      # no R2 for GLS
      # -------------------------
      if (any(class(input_list[[i]]) == "gls")) {
        page.content <- paste0(page.content, sprintf("    %sNA / NA</td>\n", colspanstring))
      } else {
        # get r2 values
        r2vals <- sjstats::r2(input_list[[i]])
        page.content <- paste0(page.content, gsub("0.", paste0(p_zero, "."),
                                                  sprintf("    %s%.*f / %.*f</td>\n", colspanstring, digits.summary,
                                                          r2vals[[1]], digits.summary, r2vals[[2]]),
                                                  fixed = TRUE))
      }
    }
    page.content <- paste(page.content, "  </tr>\n")
  }
  # -------------------------------------
  # Model-Summary: F-statistics
  # -------------------------------------
  if (show.fstat) {
    page.content <- paste(page.content, "  <tr>\n     <td class=\"tdata leftalign summary\">F-statistics</td>\n")
    for (i in 1:length(input_list)) {
      # -------------------------
      # insert "separator column"
      # -------------------------
      if (sep.column) page.content <- paste0(page.content, "\n    <td class=\"separatorcol\">&nbsp;</td>")
      # -------------------------
      # no F-Statistics for GLS
      # -------------------------
      if (any(class(input_list[[i]]) == "gls")) {
        page.content <- paste0(page.content, sprintf("    %sNA</td>\n", colspanstring))
      } else {
        fstat <- summary(input_list[[i]])$fstatistic
        # Calculate p-value for F-test
        pval <- pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE)
        # indicate significance level by stars
        pan <- get_p_stars(pval)
        page.content <- paste(page.content, sprintf("    %s%.*f%s</td>\n",
                                                    colspanstring,
                                                    digits.summary,
                                                    fstat[1],
                                                    pan))
      }
    }
    page.content <- paste(page.content, "  </tr>\n")
  }
  # -------------------------------------
  # Model-Summary: AIC
  # -------------------------------------
  if (show.aic) {
    # -------------------------------------
    # Check whether we have mixed models, and fitted with REML.
    # In this case, comparison of AIC does not make sense -
    # user need to refit models with REML = FALSE
    # -------------------------------------
    if (lmerob && length(input_list) > 1) {
      # check whether we have mixed models fitted with REML
      models.reml <- vapply(input_list, function(x) is(x, "merMod") && lme4::isREML(x), NA)
      if (any(models.reml)) warning("Some models were fit with REML. To get meaningful AIC values for comparison, refit models with ML (`REML = FALSE`).", call. = F)
    }
    page.content <- paste(page.content, "  <tr>\n     <td class=\"tdata leftalign summary\">AIC</td>\n")
    for (i in 1:length(input_list)) {
      # -------------------------
      # insert "separator column"
      # -------------------------
      if (sep.column) page.content <- paste0(page.content, "\n    <td class=\"separatorcol\">&nbsp;</td>")
      page.content <- paste(page.content, sprintf("    %s%.*f</td>\n", colspanstring, digits.summary, stats::AIC(input_list[[i]])))
    }
    page.content <- paste0(page.content, "  </tr>\n")
  }
  # -------------------------------------
  # Model-Summary: AICc
  # -------------------------------------
  if (show.aicc) {
    page.content <- paste(page.content, "  <tr>\n     <td class=\"tdata leftalign summary\">AICc</td>\n")
    for (i in 1:length(input_list)) {
      # -------------------------
      # insert "separator column"
      # -------------------------
      if (sep.column) page.content <- paste0(page.content, "\n    <td class=\"separatorcol\">&nbsp;</td>")
      page.content <- paste(page.content, sprintf("    %s%.*f</td>\n", colspanstring, digits.summary, AICcmodavg::AICc(input_list[[i]])))
    }
    page.content <- paste0(page.content, "  </tr>\n")
  }
  # -------------------------------------
  # Model-Summary: deviance
  # -------------------------------------
  if (show.dev) {
    page.content <- paste0(page.content, "  <tr>\n    <td class=\"tdata leftalign summary\">Deviance</td>")
    for (i in 1:length(input_list)) {
      # -------------------------
      # insert "separator column"
      # -------------------------
      if (sep.column) page.content <- paste0(page.content, "<td class=\"separatorcol\">&nbsp;</td>")
      page.content <- paste0(page.content, sprintf("%s%.*f</td>", colspanstring, digits.summary, stats::deviance(input_list[[i]], REML = FALSE)))
    }
    page.content <- paste0(page.content, "\n  </tr>\n")
  }
  # -------------------------------------
  # table footnote
  # -------------------------------------
  if (!p.numeric) page.content <- paste(page.content, sprintf("  <tr class=\"tdata annorow\">\n    <td class=\"tdata\">Notes</td><td class=\"tdata annostyle\" colspan=\"%i\"><em>* p&lt;%s.05&nbsp;&nbsp;&nbsp;** p&lt;%s.01&nbsp;&nbsp;&nbsp;*** p&lt;%s.001</em></td>\n  </tr>\n", headerColSpan, p_zero, p_zero, p_zero), sep = "")
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
  knitr <- replace_css_styles(page.content, cell.spacing, cell.gpr.indent,
                              p.numeric, show.header, CSS)
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
  #out.html.table(no.output, file, knitr, toWrite, use.viewer)
  # -------------------------------------
  # replace &nbsp; (former NA), created by join, with empty string
  # -------------------------------------
  joined.df <- apply(joined.df, 1:2, function(x) if (x == "&nbsp;") "" else x)
  # -------------------------------------
  # return results
  # -------------------------------------

  structure(
    class = c("sjTable", "sjtfrq"),
    list(
      page.style = get_table_css_styles(cell.spacing, cell.gpr.indent,
                                        p.numeric, show.header, CSS),
      page.content = page.content,
      output.complete = toWrite,
      knitr = knitr,
      header = NULL,
      file = file,
      show = !no.output,
      use.viewer = use.viewer,
      data = joined.df
    )
  )
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
#' @inheritParams sjp.corr
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
#' @note The variance components of the random parts (see \code{show.re.var}) are
#'         denoted like:
#'         \itemize{
#'          \item within-group variance: sigma-squared
#'          \item between-group-variance: tau-zero-zero
#'          \item random-slope-intercept-correlation: rho-zero-one
#'          }
#'
#' @details Concerning the \code{show.std} argument, \code{show.std = "std"}
#'            will print normal standardized estimates. For \code{show.std = "std2"},
#'            however, standardization of estimates follows
#'            \href{http://www.stat.columbia.edu/~gelman/research/published/standardizing7.pdf}{Gelman's (2008)}
#'            suggestion, rescaling the estimates by dividing them by two standard
#'            deviations instead of just one. Resulting coefficients are then
#'            directly comparable for untransformed binary predictors. This type
#'            of standardization uses the \code{\link[arm]{standardize}}-function
#'            from the \pkg{arm}-package.
#'            For backward compatibility reasons, \code{show.std} also may be
#'            a logical value; if \code{TRUE}, normal standardized estimates are
#'            printed (same effect as \code{show.std = "std"}). Use
#'            \code{show.std = NULL} (default) or \code{show.std = FALSE},
#'            if standardized estimats should not be printed.
#'            \cr \cr
#'            Computation of p-values (if necessary and if \code{p.kr = TRUE}) are based
#'            on conditional F-tests with Kenward-Roger approximation for the df, using
#'            the \pkg{pbkrtest}-package. If \pkg{pbkrtest} is not available or
#'            \code{p.kr = FALSE}, computation of p-values is based
#'            on normal-distribution assumption, treating the t-statistics as Wald
#'            z-statistics. See 'Details' in \code{\link[sjstats]{get_model_pval}}.
#'            \cr \cr
#'            The confidence intervals stem from \pkg{broom}'s
#'            \code{\link[broom]{tidy}}-function. For linear mixed models, the computation
#'            method is "Wald" (\code{lme4::confint.merMod(fit, method = "Wald")}).
#'            \cr \cr
#'            Furthermore, see 'Details' in \code{\link{sjt.frq}}.
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
#' efc$care.level <- sjmisc::rec(efc$n4pstu,
#'                               recodes = "0=0;1=1;2=2;3:4=3",
#'                               as.num = FALSE)
#' levels(x = efc$care.level) <- c("none", "I", "II", "III")
#'
#' # data frame for fitted model
#' mydf <- data.frame(neg_c_7 = efc$neg_c_7,
#'                    sex = efc$c161sex,
#'                    c12hour = efc$c12hour,
#'                    barthel = efc$barthtot,
#'                    education = to_factor(efc$c172code),
#'                    grp = efc$grp,
#'                    carelevel = efc$care.level)
#'
#' # fit three sample models
#' fit1 <- lmer(neg_c_7 ~ sex + c12hour + barthel + (1|grp), data = mydf)
#' fit2 <- lmer(neg_c_7 ~ sex + c12hour + education + barthel + (1|grp), data = mydf)
#' fit3 <- lmer(neg_c_7 ~ sex + c12hour + education + barthel +
#'               (1|grp) + (1|carelevel), data = mydf)
#'
#' # print summary table... automatic grouping does not work here,
#' # barthel-index is printed as category of education (values are
#' # correct, however, indentation is wrong)
#' sjt.lmer(fit1, fit2, ci.hyphen = " to ", group.pred = TRUE)
#'
#' # either change order of models
#' sjt.lmer(fit2, fit1, group.pred = TRUE)
#' # or turn off automatic grouping of categorical predictors
#' sjt.lmer(fit1, fit2, group.pred = FALSE)
#'
#' # print table, using vector names as labels
#' sjt.lmer(fit1, fit2, fit3, pred.labels = "")
#'
#' # show other statistics
#' sjt.lmer(fit1, fit2, show.aic = TRUE, show.ci = FALSE,
#'          show.se = TRUE, p.numeric = FALSE)
#'
#' sjt.lmer(fit1, fit2, fit3, show.aic = TRUE,
#'          separate.ci.col = FALSE, newline.ci = FALSE)
#'
#' # user defined predictor labels
#' sjt.lmer(fit1, fit2, fit3, pred.labels = c("Elder's gender (female)",
#'          "Hours of care per week", "Barthel Index", "Educational level (mid)",
#'          "Educational level (high)"))}
#'
#' @export
sjt.lmer <- function(...,
                     pred.labels = NULL,
                     depvar.labels = NULL,
                     remove.estimates = NULL,
                     group.pred = FALSE,
                     p.numeric = TRUE,
                     emph.p = FALSE,
                     p.zero = FALSE,
                     p.kr = TRUE,
                     separate.ci.col = TRUE,
                     newline.ci = TRUE,
                     show.est = TRUE,
                     show.std = NULL,
                     show.ci = TRUE,
                     show.se = FALSE,
                     show.header = FALSE,
                     show.col.header = TRUE,
                     show.r2 = TRUE,
                     show.icc = TRUE,
                     show.re.var = TRUE,
                     show.fstat = FALSE,
                     show.aic = FALSE,
                     show.aicc = FALSE,
                     show.dev = FALSE,
                     string.pred = "Predictors",
                     string.dv = "Dependent Variables",
                     string.interc = "(Intercept)",
                     string.obs = "Observations",
                     string.est = "B",
                     string.std = "std. Beta",
                     string.ci = "CI",
                     string.se = "std. Error",
                     string.p = "p",
                     ci.hyphen = "&nbsp;&ndash;&nbsp;",
                     minus.sign = "&#45;",
                     digits.est = 2,
                     digits.std = 2,
                     digits.p = 3,
                     digits.ci = 2,
                     digits.se = 2,
                     digits.summary = 3,
                     cell.spacing = 0.2,
                     cell.gpr.indent = 0.6,
                     sep.column = TRUE,
                     CSS = NULL,
                     encoding = NULL,
                     file = NULL,
                     use.viewer = TRUE,
                     no.output = FALSE,
                     remove.spaces = TRUE) {
  input_list <- tibble::lst(...)
  # -------------------------------------
  # check arguments
  # -------------------------------------
  if (!is.null(show.std) && show.std == "std2") show.std <- "std"

  return(sjt.lm(input_list, file = file, pred.labels = pred.labels,
                depvar.labels = depvar.labels, string.pred = string.pred,
                string.dv = string.dv,
                show.header = show.header, string.interc = string.interc,
                string.obs = string.obs, string.est = string.est, string.std = string.std,
                string.ci = string.ci, string.se = string.se, string.p = string.p, show.est = show.est,
                show.ci = show.ci, show.std = show.std, show.se = show.se,
                ci.hyphen = ci.hyphen, minus.sign = minus.sign,
                digits.est = digits.est, digits.p = digits.p, digits.ci = digits.ci,
                digits.se = digits.se, digits.std = digits.std, digits.summary = digits.summary,
                p.numeric = p.numeric, emph.p = emph.p, p.zero = p.zero, p.kr = p.kr,
                robust = FALSE, separate.ci.col = separate.ci.col, newline.ci = newline.ci,
                group.pred = group.pred, show.col.header = show.col.header, show.r2 = show.r2, show.icc = show.icc,
                show.re.var = show.re.var, show.fstat = FALSE, show.aic = show.aic, show.aicc = show.aicc, show.dev = show.dev,
                remove.estimates = remove.estimates, cell.spacing = cell.spacing, cell.gpr.indent = cell.gpr.indent,
                sep.column = sep.column, encoding = encoding,
                CSS = CSS, use.viewer = use.viewer, no.output = no.output, remove.spaces = remove.spaces))
}
