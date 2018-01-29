#' @title Summary of generalized linear models as HTML table
#' @name sjt.glm
#'
#' @description Summarizes (multiple) fitted generalized linear models (odds ratios, ci, p-values...)
#'                as HTML table, or saves them as file. The fitted models may have different predictors,
#'                e.g. when comparing different stepwise fitted models.
#'
#' @param ... One or more fitted generalized linear (mixed) models.
#' @param exp.coef Logical, if \code{TRUE} (default), regression coefficients and
#'          confidence intervals are exponentiated. Use \code{FALSE} for
#'          non-exponentiated coefficients (log-odds) as provided by
#'          the \code{\link{summary}} function.
#' @param show.r2 Logical, if \code{TRUE} (default), the pseudo R2 values for each model are printed
#'          in the model summary. R2cs is the Cox-Snell-pseudo R-squared value, R2n is Nagelkerke's
#'          pseudo R-squared value and \code{D} is Tjur's Coefficient of Discrimination
#'          (see \code{\link[sjstats]{cod}}).
#' @param show.loglik Logical, if \code{TRUE}, the Log-Likelihood for each model is printed
#'          in the model summary. Default is \code{FALSE}.
#' @param show.chi2 Logical, if \code{TRUE}, the p-value of the chi-squared value for each
#'          model's residual deviance against the null deviance is printed
#'          in the model summary. Default is \code{FALSE}. A well-fitting model
#'          with predictors should significantly differ from the null-model
#'          (without predictors), thus, a p-value less than 0.05 indicates a
#'          good model-fit.
#' @param show.hoslem Logical, if \code{TRUE}, a Hosmer-Lemeshow-Goodness-of-fit-test is
#'          performed. A well-fitting model shows no significant difference between
#'          the model and the observed data, i.e. the reported p-values should be
#'          greater than 0.05.
#' @param show.family Logical, if \code{TRUE}, the family object and link function for each fitted model
#'          are printed. Can be used in case you want to compare models with different link functions
#'          and same predictors and response, to decide which model fits best. See \code{\link{family}}
#'          for more details. It is recommended to inspect the model \code{\link{AIC}} (see \code{show.aic}) to get a
#'          decision help for which model to choose.
#'
#' @inheritParams sjt.frq
#' @inheritParams sjt.lm
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
#' @note If \code{exp.coef = TRUE} and Odds Ratios are reported, standard errors
#'       for generalized linear (mixed) models are \emph{not} on the untransformed
#'       scale, as shown in the \code{summary()}-method. Rather, \code{sjt.glm()}
#'       uses adjustments according to the delta method for approximating standard
#'       errors of transformed regression parameters (see \code{\link[sjstats]{se}}).
#'       If \code{exp.coef = FALSE} and log-Odds Ratios are reported, the standard
#'       errors are untransformed.
#'       \cr \cr Futhermore, see 'Notes' in \code{\link{sjt.frq}}.
#'
#' @details See 'Details' in \code{\link{sjt.frq}}.
#'
#' @examples
#' # prepare dummy variables for binary logistic regression
#' swiss$y1 <- ifelse(swiss$Fertility < median(swiss$Fertility), 0, 1)
#' swiss$y2 <- ifelse(swiss$Infant.Mortality < median(swiss$Infant.Mortality), 0, 1)
#' swiss$y3 <- ifelse(swiss$Agriculture < median(swiss$Agriculture), 0, 1)
#'
#' # Now fit the models. Note that both models share the same predictors
#' # and only differ in their dependent variable (y1, y2 and y3)
#' fitOR1 <- glm(y1 ~ Education + Examination + Catholic, data = swiss,
#'               family = binomial(link = "logit"))
#' fitOR2 <- glm(y2 ~ Education + Examination + Catholic, data = swiss,
#'               family = binomial(link = "logit"))
#' fitOR3 <- glm(y3 ~ Education + Examination + Catholic, data = swiss,
#'               family = binomial(link = "logit"))
#'
#' \dontrun{
#' # open HTML-table in RStudio Viewer Pane or web browser
#' sjt.glm(fitOR1, fitOR2,
#'         depvar.labels = c("Fertility", "Infant Mortality"),
#'         pred.labels = c("Education", "Examination", "Catholic"),
#'         ci.hyphen = " to ")
#'
#' # open HTML-table in RStudio Viewer Pane or web browser,
#' # integrate CI in OR column
#' sjt.glm(fitOR1, fitOR2, fitOR3,
#'         pred.labels = c("Education", "Examination", "Catholic"),
#'         separate.ci.col = FALSE)
#'
#' # open HTML-table in RStudio Viewer Pane or web browser,
#' # indicating p-values as numbers and printing CI in a separate column
#' sjt.glm(fitOR1, fitOR2, fitOR3,
#'         depvar.labels = c("Fertility", "Infant Mortality", "Agriculture"),
#'         pred.labels = c("Education", "Examination", "Catholic"))
#'
#' # --------------------------------------------
#' # User defined style sheet
#' # --------------------------------------------
#' sjt.glm(fitOR1, fitOR2, fitOR3,
#'         depvar.labels = c("Fertility", "Infant Mortality", "Agriculture"),
#'         pred.labels = c("Education", "Examination", "Catholic"),
#'         show.header = TRUE,
#'         CSS = list(css.table = "border: 2px solid;",
#'                    css.tdata = "border: 1px solid;",
#'                    css.depvarhead = "color:#003399;"))
#'
#' # --------------------------------------------
#' # Compare models with different link functions,
#' # but same predictors and response
#' # --------------------------------------------
#' library(sjmisc)
#' # load efc sample data
#' data(efc)
#' # dichtomozize service usage by "service usage yes/no"
#' efc$services <- sjmisc::dicho(efc$tot_sc_e, dich.by = 0, as.num = TRUE)
#' # fit 3 models with different link-functions
#' fit1 <- glm(services ~ neg_c_7 + c161sex + e42dep,
#'             data = efc, family = binomial(link = "logit"))
#' fit2 <- glm(services ~ neg_c_7 + c161sex + e42dep,
#'             data = efc, family = binomial(link = "probit"))
#' fit3 <- glm(services ~ neg_c_7 + c161sex + e42dep,
#'             data = efc, family = poisson(link = "log"))
#'
#' # compare models
#' sjt.glm(fit1, fit2, fit3, string.est = "Estimate",
#'         show.aic = TRUE, show.family = TRUE)
#'
#' # --------------------------------------------
#' # Change style of p-values and CI-appearance
#' # --------------------------------------------
#' # open HTML-table in RStudio Viewer Pane or web browser,
#' # table indicating p-values as stars
#' sjt.glm(fit1, fit2, fit3, p.numeric = FALSE,
#'         show.aic = TRUE, show.family = TRUE)
#'
#' # open HTML-table in RStudio Viewer Pane or web browser,
#' # indicating p-values as stars and integrate CI in OR column
#' sjt.glm(fit1, fit2, fit3, p.numeric = FALSE, separate.ci.col = FALSE,
#'         show.aic = TRUE, show.family = TRUE, show.r2 = TRUE)
#'
#' # ----------------------------------
#' # automatic grouping of predictors
#' # ----------------------------------
#' library(sjmisc)
#' # load efc sample data
#' data(efc)
#' # dichtomozize service usage by "service usage yes/no"
#' efc$services <- sjmisc::dicho(efc$tot_sc_e, dich.by = 0, as.num = TRUE)
#' # make dependency categorical
#' efc$e42dep <- to_factor(efc$e42dep)
#' # fit model with "grouped" predictor
#' fit <- glm(services ~ neg_c_7 + c161sex + e42dep, data = efc)
#'
#' # automatic grouping of categorical predictors
#' sjt.glm(fit)
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
#' efc$c172code <- to_factor(efc$c172code)
#' fit2 <- glm(services ~ neg_c_7 + c161sex + c12hour, data = efc)
#' fit3 <- glm(services ~ neg_c_7 + c161sex + c172code, data = efc)
#'
#' # print models with different predictors
#' sjt.glm(fit, fit2, fit3, group.pred = FALSE)}
#'
#' @importFrom dplyr full_join slice mutate if_else
#' @importFrom stats nobs AIC confint coef logLik family deviance
#' @importFrom sjstats std_beta icc r2 cod chisq_gof hoslem_gof se
#' @importFrom tibble lst
#' @importFrom broom tidy
#' @export
sjt.glm <- function(...,
                    pred.labels = NULL,
                    depvar.labels = NULL,
                    remove.estimates = NULL,
                    group.pred = TRUE,
                    exp.coef = TRUE,
                    p.numeric = TRUE,
                    emph.p = FALSE,
                    p.zero = FALSE,
                    robust = FALSE,
                    separate.ci.col = TRUE,
                    newline.ci = TRUE,
                    show.ci = TRUE,
                    show.se = FALSE,
                    show.header = FALSE,
                    show.col.header = TRUE,
                    show.r2 = FALSE,
                    show.icc = FALSE,
                    show.re.var = FALSE,
                    show.loglik = FALSE,
                    show.aic = FALSE,
                    show.aicc = FALSE,
                    show.dev = FALSE,
                    show.hoslem = FALSE,
                    show.family = FALSE,
                    show.chi2 = FALSE,
                    string.pred = "Predictors",
                    string.dv = "Dependent Variables",
                    string.interc = "(Intercept)",
                    string.obs = "Observations",
                    string.est = NULL,
                    string.ci = "CI",
                    string.se = "std. Error",
                    string.p = "p",
                    ci.hyphen = "&nbsp;&ndash;&nbsp;",
                    digits.est = 2,
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
  toWrite <- get_table_header(encoding, cell.spacing, cell.gpr.indent, p.numeric, show.header, CSS)
  # ------------------------
  # retrieve fitted models
  # ------------------------
  input_list <- tibble::lst(...)
  # --------------------------------------------------------
  # check length. if we have a list of fitted model,
  # we need to "unlist" them
  # --------------------------------------------------------
  if (class(input_list[[1]])[1] == "list") input_list <- lapply(input_list[[1]], function(x) x)
  # ------------------------
  # do we have mixed models?
  # ------------------------
  lmerob <- any(class(input_list[[1]]) == "glmerMod")
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
    # retrieve model
    # -------------------------------------
    fit <- input_list[[i]]
    # -------------------------------------
    # get tidy model summary
    # -------------------------------------
    if (robust) {
      fit.df <- sjstats::robust(fit, conf.int = T, exponentiate = F) %>%
        dplyr::select_("-statistic")
    } else {
      # get tidy output
      fit.df <- broom::tidy(fit, effects = "fixed", conf.int = T)

      # check if coefficients should be exponentiated - if yes,
      # also retrieve adjusted standard errors
      if (exp.coef) {
        fit.df <- fit.df %>%
          # remove non-transformed standard error
          dplyr::select_("-statistic", "-std.error") %>%
          # and add adjusted standard errors
          dplyr::mutate(std.error = sjstats::se(fit)[["std.error"]])

        # reorder df
        fit.df <- fit.df[, c(1:2, 6, 3:5)]
      } else {
        # just remove test statistics
        fit.df <- dplyr::select_(fit.df, "-statistic")
      }
    }
    # -------------------------------------
    # write data to data frame. we need names of
    # coefficients, estimated values, ci,
    # std. beta and p-values
    # -------------------------------------
    if (exp.coef) {
      fit.df$estimate <- exp(fit.df$estimate)
      fit.df$conf.low <- exp(fit.df$conf.low)
      fit.df$conf.high <- exp(fit.df$conf.high)
    }
    # -------------------------------------
    # format values
    # -------------------------------------
    fit.df$estimate <- sprintf("%.*f", digits.est, fit.df$estimate)
    fit.df$conf.low <- sprintf("%.*f", digits.ci, fit.df$conf.low)
    fit.df$conf.high <- sprintf("%.*f", digits.ci, fit.df$conf.high)
    fit.df$std.error <- sprintf("%.*f", digits.se, fit.df$std.error)
    # -------------------------------------
    # prepare p-values, either as * or as numbers
    # -------------------------------------
    if (!p.numeric) {
      fit.df$p.value <- sapply(fit.df$p.value, function(x) x <- get_p_stars(x))
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
                          sprintf("ci.hi%i", i))
    fit.df <- fit.df[, c(1:2, 5:6, 4, 3)]
    # -------------------------------------
    # add to df list
    # -------------------------------------
    df.fit[[length(df.fit) + 1]] <- as.data.frame(fit.df)
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


  # select correct column heading, depending on model family and link function
  if (is.null(string.est)) {
    # get family
    fitfam <- get_glm_family(input_list[[1]])
    # check if we have a binomial model
    if (fitfam$is_bin) {
      # here we gor for logistic regression
      # estimate is "Odds Ratio"
      if (fitfam$is_logit) {
        string.est <-
          dplyr::if_else(isTRUE(exp.coef),
                         true = "Odds Ratio",
                         false = "Log-Odds",
                         missing = "Estimate")
      } else {
        # estimate is "Risk Ratio"
        string.est <-
          dplyr::if_else(isTRUE(exp.coef),
                         true = "Risk Ratio",
                         false = "Log-Risk",
                         missing = "Estimate")
      }
    } else if (fitfam$is_pois) {
      string.est <- dplyr::if_else(isTRUE(exp.coef),
                                   true = "IRR",
                                   false = "Log-Mean",
                                   missing = "Estimate")
    } else {
      string.est <- "Estimate"
    }
  }

  # -------------------------------------
  # if confidence interval should be omitted,
  # don't use separate column for CI!
  # -------------------------------------
  if (!show.ci) {
    separate.ci.col <- FALSE
    showCIString <- string.est
  } else {
    showCIString <- sprintf("%s (%s)", string.est, string.ci)
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
  if (p.numeric) headerColSpanFactor <- headerColSpanFactor + 1
  if (separate.ci.col) headerColSpanFactor <- headerColSpanFactor + 1
  if (show.se) headerColSpanFactor <- headerColSpanFactor + 1
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
  # start table
  # -------------------------------------
  page.content <- "<table>"
  # -------------------------------------
  # check if we want to see header strings
  # -------------------------------------
  if (show.header) {
    page.content <- paste0(page.content, sprintf("\n  <tr>\n    <td class=\"tdata topborder\" rowspan=\"2\"><em>%s</em></td>", string.pred))
    page.content <- paste0(page.content, sprintf("\n    <td colspan=\"%i\" class=\"tdata topborder depvarhead\"><em>%s</em></td>", headerColSpan, string.dv))
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
  if (!show.header) {
    page.content <- paste0(page.content, "\n    <td class=\"tdata topborder\">&nbsp;</td>")
    tcp <- " topborder"
  }
  # -------------------------------------
  # set default dependent var label
  # -------------------------------------
  gtrl <- get_table_response_label(page.content, depvar.labels, input_list, tcp, headerColSpanFactor, sep.column)
  page.content <- gtrl$page.content
  depvar.labels <- gtrl$depvar.labels
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
  if (show.col.header) {
    page.content <- paste0(page.content, "\n  <tr>\n    <td class=\"tdata colnames\">&nbsp;</td>")
    colnr <- ifelse(is.null(depvar.labels), length(input_list), length(depvar.labels))
    for (i in seq_len(colnr)) {
      # -------------------------
      # insert "separator column"
      # -------------------------
      if (sep.column) page.content <- paste0(page.content, "<td class=\"separatorcol colnames\">&nbsp;</td>")
      # confidence interval in separate column
      if (separate.ci.col) {
        page.content <- paste0(page.content, sprintf("\n    <td class=\"tdata centeralign colnames modelcolumn1\">%s</td>", string.est))
        if (show.ci) page.content <- paste0(page.content, sprintf("<td class=\"tdata centeralign colnames modelcolumn2\">%s</td>", string.ci))
      }
      else {
        # confidence interval in Beta-column
        page.content <- paste0(page.content, sprintf("\n    <td class=\"tdata centeralign colnames modelcolumn1\">%s</td>", showCIString))
      }
      # show std. error
      if (show.se) page.content <- paste0(page.content, sprintf("<td class=\"tdata centeralign colnames modelcolumn3\">%s</td>", string.se))
      # show p-values as numbers in separate column
      if (p.numeric) page.content <- paste0(page.content, sprintf("<td class=\"tdata centeralign colnames modelcolumn4\">%s</td>", string.p))
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
  page.content <- paste0(page.content, sprintf("  <tr>\n    <td class=\"tdata %sleftalign\">%s</td>", tcb_class, string.interc))
  for (i in seq_len(length(input_list))) {
    # -------------------------
    # insert "separator column"
    # -------------------------
    if (sep.column) page.content <- paste0(page.content, sprintf("<td class=\"separatorcol %s\">&nbsp;</td>", tcb_class))
    # confidence interval in separate column
    if (separate.ci.col) {
      # open table cell for Beta-coefficient
      page.content <- paste0(page.content, sprintf("\n    <td class=\"tdata centeralign %smodelcolumn1\">%s",
                                                   tcb_class,
                                                   joined.df[1, (i - 1) * 5 + 2]))
      # if p-values are not shown as numbers, insert them after beta-value
      if (!p.numeric) page.content <- paste0(page.content, sprintf("&nbsp;%s", joined.df[1, (i - 1) * 5 + 5]))
      # if we have CI, start new table cell (CI in separate column)
      if (show.ci) {
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
      if (show.ci) page.content <- paste0(page.content, sprintf("%s(%s%s%s)",
                                                                    linebreakstring,
                                                                    joined.df[1, (i - 1) * 5 + 3],
                                                                    ci.hyphen,
                                                                    joined.df[1, (i - 1) * 5 + 4]))
      # if p-values are not shown as numbers, insert them after beta-value
      if (!p.numeric) page.content <- paste0(page.content, sprintf("&nbsp;%s", joined.df[1, (i - 1) * 5 + 5]))
      page.content <- paste0(page.content, "</td>")
    }
    # show std. error
    if (show.se) page.content <- paste0(page.content, sprintf("<td class=\"tdata centeralign %smodelcolumn3\">%s</td>",
                                                                   tcb_class,
                                                                   joined.df[1, (i - 1) * 5 + 6]))
    # show p-values as numbers in separate column
    if (p.numeric) page.content <- paste0(page.content, sprintf("<td class=\"tdata centeralign %smodelcolumn4\">%s</td>",
                                                                       tcb_class,
                                                                       joined.df[1, (i - 1) * 5 + 5]))
  }
  page.content <- paste0(page.content, "\n  </tr>")
  # -------------------------------------
  # subsequent rows: pedictors
  # -------------------------------------
  for (i in seq_len(nrow(joined.df) - 1)) {
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
      if (sep.column) page.content <- paste0(page.content, "<td class=\"separatorcol\">&nbsp;</td>")
      # confidence interval in separate column
      if (separate.ci.col) {
        # open table cell for Beta-coefficient
        page.content <- paste0(page.content, sprintf("\n    <td class=\"tdata centeralign modelcolumn1\">%s",
                                                     joined.df[i + 1, (j - 1) * 5 + 2]))
        # if p-values are not shown as numbers, insert them after beta-value
        if (!p.numeric) page.content <- paste0(page.content, sprintf("&nbsp;%s",
                                                                            joined.df[i + 1, (j - 1) * 5 + 5]))
        # if we have CI, start new table cell (CI in separate column)
        if (show.ci) {
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
        if (show.ci && !sjmisc::is_empty(ci.lo)) page.content <- paste0(page.content,
                                                                            sprintf("%s(%s%s%s)",
                                                                                    linebreakstring,
                                                                                    ci.lo,
                                                                                    ci.sep.string,
                                                                                    ci.hi))
        # if p-values are not shown as numbers, insert them after beta-value
        if (!p.numeric) page.content <- paste0(page.content, sprintf("&nbsp;%s",
                                                                            joined.df[i + 1, (j - 1) * 5 + 5]))
        page.content <- paste0(page.content, "</td>")
      }
      # show std. error
      if (show.se) page.content <- paste0(page.content, sprintf("<td class=\"tdata centeralign modelcolumn3\">%s</td>",
                                                                     joined.df[i + 1, (j - 1) * 5 + 6]))
      # show p-values as numbers in separate column
      if (p.numeric) page.content <- paste0(page.content, sprintf("<td class=\"tdata centeralign modelcolumn4\">%s</td>",
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
    # -------------------------------------
    # we need to know max amount of groups
    # -------------------------------------
    all_mm_counts <- unlist(lapply(input_list, function(x) length(lme4::getME(x, "flist"))))
    # retrieve maximum random intercepts
    mmcount <- max(all_mm_counts)
    # get random intercepts from model with most intercepts
    mmgrps <- lme4::getME(input_list[[which.max(all_mm_counts)]], "flist")
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
      # between-group variance
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
          reva <- lme4::VarCorr(input_list[[i]])
          vars <- lapply(reva, function(x) x[[1]])
          tau.00 <- sapply(vars, function(x) x[1])
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
            reva <- lme4::VarCorr(input_list[[i]])
            cor_ <- unlist(lapply(reva, function(x) attr(x, "correlation")[1, 2]))
            rho.01 <- paste0(sprintf("%.*f", digits.summary, cor_[1], collapse = ""))
            page.content <- paste0(page.content, colspanstring, rho.01, "</td>\n")
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
      page.content <- paste0(page.content, sprintf("\n  <tr>\n    <td class=\"tdata summary leftalign\">N<sub>%s</sub></td>", names(mmgrps[gl])))
      # iterate models
      for (i in seq_len(length(input_list))) {
        # -------------------------
        # insert "separator column"
        # -------------------------
        if (sep.column) page.content <- paste0(page.content, "<td class=\"separatorcol\">&nbsp;</td>")
        # retrieve random intercepts of each model
        sub.mmgrps <- lme4::getME(input_list[[i]], "flist")
        # does model have enough random intercepts?
        # if yes, print
        if (length(sub.mmgrps) >= gl) {
          page.content <- paste(page.content, sprintf("%s%i</td>", colspanstring, nlevels(sub.mmgrps[[gl]])))
        } else {
          page.content <- paste(page.content, sprintf("%s&nbsp;</td>", colspanstring))
        }
      }
      page.content <- paste0(page.content, "\n  </tr>\n")
    }
    # -------------------------------------
    # Model-Summary: icc
    # -------------------------------------
    if (show.icc) {
      # get icc from models
      summary.icc <- sjstats::icc(input_list[[which.max(all_mm_counts)]])
      # iterate icc's
      for (si in seq_len(mmcount)) {
        page.content <- paste0(page.content, sprintf("  <tr>\n    <td class=\"tdata leftalign summary\">ICC<sub>%s</sub></td>", names(summary.icc[si])))
        # iterate models
        for (i in seq_len(length(input_list))) {
          # -------------------------
          # insert "separator column"
          # -------------------------
          if (sep.column) page.content <- paste0(page.content, "<td class=\"separatorcol\">&nbsp;</td>")
          # get icc from models
          sub.summary.icc <- sjstats::icc(input_list[[i]])
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
  page.content <- paste0(page.content, sprintf("\n  <tr>\n    <td class=\"tdata summary leftalign firstsumrow\">%s</td>", string.obs))
  for (i in seq_len(length(input_list))) {
    # -------------------------
    # insert "separator column"
    # -------------------------
    if (sep.column) page.content <- paste0(page.content, "<td class=\"separatorcol firstsumrow\">&nbsp;</td>")
    page.content <- paste(page.content, sprintf("%s%i</td>", colspanstringfirstrow, stats::nobs(input_list[[i]])))
  }
  page.content <- paste0(page.content, "\n  </tr>\n")
  # -------------------------------------
  # Model-Summary: pseudo r2
  # -------------------------------------
  if (show.r2) {
    # first, we need the correct description for 2nd r2-value
    if (lmerob)
      r2string <- "Tjur's D"
    else
      r2string <- "Pseudo-R<sup>2</sup>"

    page.content <- paste0(page.content, sprintf("  <tr>\n    <td class=\"tdata leftalign summary\">%s</td>", r2string))
    for (i in seq_len(length(input_list))) {
      # -------------------------
      # insert "separator column"
      # -------------------------
      if (sep.column) page.content <- paste0(page.content, "\n    <td class=\"separatorcol\">&nbsp;</td>")
      psr <- sjstats::r2(input_list[[i]])
      tjur <- sjstats::cod(input_list[[i]])
      if (lmerob) {
        page.content <- paste0(page.content, gsub("0.",
                                                  paste0(p_zero, "."),
                                                  sprintf("%s%.*f",
                                                          colspanstring,
                                                          digits.summary,
                                                          tjur),
                                                  fixed = TRUE))
      } else {
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
    }
    page.content <- paste(page.content, "\n  </tr>\n")
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
      models.reml <- purrr::map_lgl(input_list, ~ is_merMod(.x) && lme4::isREML(.x))
      if (any(models.reml)) warning("Some models were fit with REML. To get meaningful AIC values for comparison, refit models with ML (`REML = FALSE`).", call. = F)
    }
    page.content <- paste0(page.content, "  <tr>\n    <td class=\"tdata leftalign summary\">AIC</td>")
    for (i in seq_len(length(input_list))) {
      # -------------------------
      # insert "separator column"
      # -------------------------
      if (sep.column) page.content <- paste0(page.content, "<td class=\"separatorcol\">&nbsp;</td>")
      page.content <- paste0(page.content, sprintf("%s%.*f</td>", colspanstring, digits.summary, stats::AIC(input_list[[i]])))
    }
    page.content <- paste0(page.content, "\n  </tr>\n")
  }
  # -------------------------------------
  # Model-Summary: AICc
  # -------------------------------------
  if (show.aicc) {
    page.content <- paste0(page.content, "  <tr>\n    <td class=\"tdata leftalign summary\">AICc</td>")
    for (i in seq_len(length(input_list))) {
      # -------------------------
      # insert "separator column"
      # -------------------------
      if (sep.column) page.content <- paste0(page.content, "<td class=\"separatorcol\">&nbsp;</td>")
      page.content <- paste0(page.content, sprintf("%s%.*f</td>", colspanstring, digits.summary, AICcmodavg::AICc(input_list[[i]])))
    }
    page.content <- paste0(page.content, "\n  </tr>\n")
  }
  # -------------------------------------
  # Model-Summary: log likelihood
  # -------------------------------------
  if (show.loglik) {
    page.content <- paste0(page.content, "  <tr>\n    <td class=\"tdata leftalign summary\">-2 Log-Likelihood</td>")
    for (i in seq_len(length(input_list))) {
      # -------------------------
      # insert "separator column"
      # -------------------------
      if (sep.column) page.content <- paste0(page.content, "<td class=\"separatorcol\">&nbsp;</td>")
      page.content <- paste0(page.content, sprintf("%s%.*f</td>", colspanstring, digits.summary, -2 * as.vector(stats::logLik(input_list[[i]]))))
    }
    page.content <- paste0(page.content, "\n  </tr>\n")
  }
  # -------------------------------------
  # Model-Summary: deviance
  # -------------------------------------
  if (show.dev) {
    page.content <- paste0(page.content, "  <tr>\n    <td class=\"tdata leftalign summary\">Deviance</td>")
    for (i in seq_len(length(input_list))) {
      # -------------------------
      # insert "separator column"
      # -------------------------
      if (sep.column) page.content <- paste0(page.content, "<td class=\"separatorcol\">&nbsp;</td>")
      page.content <- paste0(page.content, sprintf("%s%.*f</td>", colspanstring, digits.summary, stats::deviance(input_list[[i]], REML = FALSE)))
    }
    page.content <- paste0(page.content, "\n  </tr>\n")
  }
  # -------------------------------------
  # Model-Summary: Chi2
  # -------------------------------------
  if (show.chi2) {
    page.content <- paste0(page.content, "  <tr>\n    <td class=\"tdata leftalign summary\">&Chi;<sup>2</sup><sub>deviance</sub></td>")
    for (i in seq_len(length(input_list))) {
      # -------------------------
      # insert "separator column"
      # -------------------------
      if (sep.column) page.content <- paste0(page.content, "<td class=\"separatorcol\">&nbsp;</td>")
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
  # Model-Summary: Hosmer-Lemeshow-GOF
  # -------------------------------------
  if (show.hoslem) {
    page.content <- paste0(page.content, "  <tr>\n    <td class=\"tdata leftalign summary\">Hosmer-Lemeshow-&Chi;<sup>2</sup></td>")
    for (i in seq_len(length(input_list))) {
      # -------------------------
      # insert "separator column"
      # -------------------------
      if (sep.column) page.content <- paste0(page.content, "<td class=\"separatorcol\">&nbsp;</td>")
      # -------------------------
      # compute Hosmer-Lemeshow test
      # -------------------------
      hlgof <- sjstats::hoslem_gof(input_list[[i]])
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
  if (show.family) {
    page.content <- paste0(page.content, "  <tr>\n    <td class=\"tdata leftalign summary\">Family</td>")
    for (i in seq_len(length(input_list))) {
      # -------------------------
      # insert "separator column"
      # -------------------------
      if (sep.column) page.content <- paste0(page.content, "<td class=\"separatorcol\">&nbsp;</td>")
      fam <- stats::family(input_list[[i]])
      page.content <- paste0(page.content, sprintf("%s%s (%s)</td>",
                                                   colspanstring,
                                                   fam$family,
                                                   fam$link))
    }
    page.content <- paste0(page.content, "\n  </tr>\n")
  }
  # -------------------------------------
  # table footnote
  # -------------------------------------
  if (!p.numeric) page.content <- paste0(page.content, sprintf("  <tr>\n    <td class=\"tdata annorow\">Notes</td><td class=\"tdata annorow annostyle\" colspan=\"%i\"><em>* p&lt;%s.05&nbsp;&nbsp;&nbsp;** p&lt;%s.01&nbsp;&nbsp;&nbsp;*** p&lt;%s.001</em></td>\n  </tr>\n", headerColSpan, p_zero, p_zero, p_zero))
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
 structure(class = c("sjTable", "sjtglm"),
                      list(page.style = get_table_css_styles(cell.spacing, cell.gpr.indent,
                                                             p.numeric, show.header, CSS),
                           page.content = page.content,
                           #page.content.list = page.content.list,
                           output.complete = toWrite,
                           knitr = knitr,
                           file = file,
                           header = NULL,
                           show = !no.output,
                           use.viewer = use.viewer,
                           data = joined.df))
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
#' @note Computation of p-values (if necessary) is based on normal-distribution
#'         assumption, treating the t-statistics as Wald z-statistics.
#'         \cr \cr
#'         The variance components of the random parts (see \code{show.re.var}) are
#'         denoted like:
#'         \itemize{
#'          \item between-group-variance: tau-zero-zero
#'          \item random-slope-intercept-correlation: rho-zero-one
#'          }
#'       Standard errors for generalized linear (mixed) models are \emph{not}
#'       the regular standard errors on the untransformed scale, as shown in the
#'       \code{summary()}-method. Rather, \code{sjt.glmer()} uses adjustments
#'       according to the delta method for approximating standard errors of
#'       transformed regression parameters (see \code{\link[sjstats]{se}}).
#'       \cr \cr Futhermore, see 'Notes' in \code{\link{sjt.frq}}.
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
#' mydf <- data.frame(hi_qol = to_factor(efc$hi_qol),
#'                    sex = to_factor(efc$c161sex),
#'                    c12hour = efc$c12hour,
#'                    neg_c_7 = efc$neg_c_7,
#'                    education = to_factor(efc$c172code),
#'                    grp = efc$grp)
#'
#' # fit glmer
#' fit1 <- glmer(hi_qol ~ sex + c12hour + neg_c_7 + (1|grp),
#'               data = mydf, family = binomial("logit"))
#' fit2 <- glmer(hi_qol ~ sex + c12hour + neg_c_7 + education + (1|grp),
#'               data = mydf, family = binomial("logit"))
#'
#' # print summary table
#' sjt.glmer(fit1, fit2, ci.hyphen = " to ")
#'
#' # print summary table, using different table layout
#' sjt.glmer(fit1, fit2, show.aic = TRUE, show.ci = FALSE,
#'           show.se = TRUE, p.numeric = FALSE)
#'
#' # print summary table
#' sjt.glmer(fit1, fit2, pred.labels = c("Elder's gender (female)",
#'             "Hours of care per week", "Negative Impact",
#'             "Educational level (mid)", "Educational level (high)"))
#'
#' # use vector names as predictor labels
#' sjt.glmer(fit1, fit2, pred.labels = "")}
#'
#'
#' @export
sjt.glmer <- function(...,
                      pred.labels = NULL,
                      depvar.labels = NULL,
                      remove.estimates = NULL,
                      group.pred = FALSE,
                      exp.coef = TRUE,
                      p.numeric = TRUE,
                      emph.p = FALSE,
                      p.zero = FALSE,
                      separate.ci.col = TRUE,
                      newline.ci = TRUE,
                      show.ci = TRUE,
                      show.se = FALSE,
                      show.header = FALSE,
                      show.col.header = TRUE,
                      show.r2 = FALSE,
                      show.icc = TRUE,
                      show.re.var = TRUE,
                      show.loglik = FALSE,
                      show.aic = FALSE,
                      show.aicc = FALSE,
                      show.dev = TRUE,
                      show.hoslem = FALSE,
                      show.family = FALSE,
                      string.pred = "Predictors",
                      string.dv = "Dependent Variables",
                      string.interc = "(Intercept)",
                      string.obs = "Observations",
                      string.est = NULL,
                      string.ci = "CI",
                      string.se = "std. Error",
                      string.p = "p",
                      ci.hyphen = "&nbsp;&ndash;&nbsp;",
                      digits.est = 2,
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
  return(sjt.glm(input_list, file = file, pred.labels = pred.labels,
                 depvar.labels = depvar.labels, string.pred = string.pred,
                 string.dv = string.dv, show.header = show.header,
                 string.interc = string.interc,
                 string.obs = string.obs, string.est = string.est,
                 string.ci = string.ci, string.se = string.se, string.p = string.p,
                 digits.est = digits.est, digits.p = digits.p, digits.ci = digits.ci,
                 digits.se = digits.se, digits.summary = digits.summary, exp.coef = exp.coef,
                 p.numeric = p.numeric, emph.p = emph.p, p.zero = p.zero, robust = FALSE,
                 show.ci = show.ci, show.se = show.se,
                 ci.hyphen = ci.hyphen, separate.ci.col = separate.ci.col, newline.ci = newline.ci,
                 group.pred = group.pred, show.col.header = show.col.header, show.r2 = show.r2, show.icc = show.icc,
                 show.re.var = show.re.var, show.loglik = show.loglik, show.aic = show.aic, show.aicc = show.aicc, show.dev = show.dev,
                 show.chi2 = FALSE, show.hoslem = show.hoslem, show.family = show.family, remove.estimates = remove.estimates,
                 cell.spacing = cell.spacing, cell.gpr.indent = cell.gpr.indent, sep.column = sep.column,
                 encoding = encoding, CSS = CSS, use.viewer = use.viewer, no.output = no.output, remove.spaces = remove.spaces))
}
