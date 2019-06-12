#' @title Print regression models as HTML table
#' @name tab_model
#'
#' @description
#'   \code{tab_model()} creates HTML tables from regression models.
#'
#' @param title String, will be used as table caption.
#' @param terms Character vector with names of those terms (variables) that should
#'    be printed in the table. All other terms are removed from the output. If
#'    \code{NULL}, all terms are printed. Note that the term names must match
#'    the names of the model's coefficients. For factors, this means that
#'    the variable name is suffixed with the related factor level, and each
#'    category counts as one term. E.g. \code{rm.terms = "t_name [2,3]"}
#'    would remove the terms \code{"t_name2"} and \code{"t_name3"} (assuming
#'    that the variable \code{t_name} is categorical and has at least
#'    the factor levels \code{2} and \code{3}). Another example for the
#'    \emph{iris}-dataset: \code{terms = "Species"} would not work, instead
#'    use \code{terms = "Species [versicolor,virginica]"}.
#' @param rm.terms Character vector with names that indicate which terms should
#'    be removed from the output Counterpart to \code{terms}. \code{rm.terms =
#'    "t_name"} would remove the term \emph{t_name}. Default is \code{NULL}, i.e.
#'    all terms are used. For factors, levels that should be removed from the plot
#'    need to be explicitely indicated in square brackets, and match the model's
#'    coefficient names, e.g. \code{rm.terms = "t_name [2,3]"} would remove the terms
#'    \code{"t_name2"} and \code{"t_name3"} (assuming that the variable \code{t_name}
#'    was categorical and has at least the factor levels \code{2} and \code{3}).
#' @param pred.labels Character vector with labels of predictor variables.
#'    If not \code{NULL}, \code{pred.labels} will be used in the first
#'    table column with the predictors' names. By default, if \code{auto.label = TRUE}
#'    and \code{\link[sjlabelled]{get_term_labels}} is called to retrieve the labels
#'    of the coefficients, which will be used as predictor labels.
#'    If \code{pred.labels = ""} or \code{auto.label = FALSE}, the raw
#'    variable names as used in the model formula are used as predictor
#'    labels. If \code{pred.labels} is a named vector, predictor labels (by
#'    default, the names of the model's coefficients) will be matched with the
#'    names of \code{pred.labels}. This ensures that labels always match the
#'    related predictor in the table, no matter in which way the predictors
#'    are sorted. See 'Examples'.
#' @param dv.labels Character vector with labels of dependent variables of all
#'    fitted models. See 'Examples'.
#' @param show.intercept Logical, if \code{TRUE}, the intercepts are printed.
#' @param show.est Logical, if \code{TRUE}, the estimates are printed.
#' @param show.zeroinf Logical, if \code{TRUE} and model has a zero-inflated
#'    model part, this is also printed to the table.
#' @param show.re.var Logical, if \code{TRUE}, prints the random effect variances
#'    for mixed models. See \code{\link[insight]{get_variance}} for details.
#' @param show.icc Logical, if \code{TRUE}, prints the intraclass correlation
#'    coefficient for mixed models. See \code{\link[performance]{icc}} for details.
#' @param show.ngroups Logical, if \code{TRUE}, shows number of random effects groups
#'    for mixed models.
#' @param show.dev Logical, if \code{TRUE}, shows the deviance of the model.
#' @param show.loglik Logical, if \code{TRUE}, shows the log-Likelihood of the model.
#' @param show.ci Either logical, and if \code{TRUE}, the confidence intervals
#'    is printed to the table; if \code{FALSE}, confidence intervals are
#'    omitted. Or numeric, between 0 and 1, indicating the range of the
#'    confidence intervals.
#' @param show.std Indicates whether standardized beta-coefficients should
#'    also printed, and if yes, which type of standardization is done.
#'    See 'Details'.
#' @param show.p Logical, if \code{TRUE}, p-values are also printed.
#' @param show.se Logical, if \code{TRUE}, the standard errors are
#'   also printed. If robust standard errors are required, use arguments
#'   \code{vcov.fun}, \code{vcov.type} and \code{vcov.args} (see
#'   \code{\link[sjstats]{robust}} for details).
#' @param show.r2 Logical, if \code{TRUE}, the r-squared value is also printed.
#'    Depending on the model, these might be pseudo-r-squared values, or Bayesian
#'    r-squared etc. See \code{\link[performance]{r2}} for details.
#' @param show.stat Logical, if \code{TRUE}, the coefficients' test statistic
#'    is also printed.
#' @param show.df Logical, if \code{TRUE} and \code{p.val = "kr"}, the p-values
#'    for linear mixed models are based on df with Kenward-Rogers approximation.
#'    These df-values are printed. See \code{\link[sjstats]{p_value}} for details.
#' @param string.pred Character vector,used as headline for the predictor column.
#'    Default is \code{"Predictors"}.
#' @param string.est Character vector, used for the column heading of coefficients.
#'    Default is based on the response scale, e.g. for logistic regression models,
#'    \code{"Odds Ratios"} will be chosen, while for Poisson models it is
#'    \code{"Incidence Rate Ratios"} etc. Default if not specified is \code{"Estimate"}.
#' @param string.std Character vector, used for the column heading of standardized beta coefficients. Default is \code{"std. Beta"}.
#' @param string.ci Character vector, used for the column heading of confidence interval values. Default is \code{"CI"}.
#' @param string.se Character vector, used for the column heading of standard error values. Default is \code{"std. Error"}.
#' @param string.p Character vector, used for the column heading of p values. Default is \code{"p"}.
#' @param string.df Character vector, used for the column heading of degrees of freedom. Default is \code{"df"}.
#' @param string.stat Character vector, used for the test statistic. Default is \code{"Statistic"}.
#' @param string.resp Character vector, used for the column heading of of the response level for multinominal or categorical models. Default is \code{"Response"}.
#' @param string.intercept Character vector, used as name for the intercept parameter. Default is \code{"(Intercept)"}.
#' @param strings Named character vector, as alternative to arguments like \code{string.ci}
#'    or \code{string.p} etc. The name (lhs) must be one of the string-indicator from
#'    the forementioned arguments, while the value (rhs) is the string that is used
#'    as column heading. E.g., \code{strings = c(ci = "Conf.Int.", se = "std. Err")}
#'    would be equivalent to setting \code{string.ci = "Conf.Int.", string.se = "std. Err"}.
#' @param ci.hyphen Character vector, indicating the hyphen for confidence interval range.
#'    May be an HTML entity. See 'Examples'.
#' @param minus.sign string, indicating the minus sign for negative numbers.
#'    May be an HTML entity. See 'Examples'.
#' @param emph.p Logical, if \code{TRUE}, significant p-values are shown bold faced.
#' @param digits Amount of decimals for estimates
#' @param digits.p Amount of decimals for p-values
#' @param collapse.ci Logical, if \code{FALSE}, the CI values are shown in
#'    a separate table column.
#' @param collapse.se Logical, if \code{FALSE}, the SE values are shown in
#'    a separate table column.
#' @param linebreak Logical, if \code{TRUE} and \code{collapse.ci = FALSE} or
#'    \code{collapse.se = FALSE}, inserts a line break between estimate and
#'    CI resp. SE values. If \code{FALSE}, values are printed in the same line
#'    as estimate values.
#' @param group.terms Logical, if \code{TRUE} (default), automatically groups table rows with
#'    factor levels of same factor, i.e. predictors of type \code{\link{factor}} will
#'    be grouped, if the factor has more than two levels. Grouping means that a separate headline
#'    row is inserted to the table just before the predictor values.
#' @param show.ci50 Logical, if \code{TRUE}, for Bayesian models, a second
#'    credible interval is added to the table output.
#' @param show.fstat Logical, if \code{TRUE}, the F-statistics for each model is
#'    printed in the table summary. This option is not supported by all model types.
#' @param show.aic Logical, if \code{TRUE}, the AIC value for each model is printed
#'    in the table summary.
#' @param show.aicc Logical, if \code{TRUE}, the second-order AIC value for each model
#'    is printed in the table summary.
#' @param show.obs Logical, if \code{TRUE}, the number of observations per model is
#'    printed in the table summary.
#' @param col.order Character vector, indicating which columns should be printed
#'    and in which order. Column names that are excluded from \code{col.order}
#'    are not shown in the table output. However, column names that are included,
#'    are only shown in the table when the related argument (like \code{show.est}
#'    for \code{"estimate"}) is set to \code{TRUE} or another valid value.
#'    Table columns are printed in the order as they appear in \code{col.order}.
#' @param p.val Character, for mixed models, indicates how p-values are computed.
#'   Use \code{p.val = "wald"} for a faster, but less precise computation. For
#'   \code{p.val = "kr"}, computation of p-values is based on conditional F-tests
#'   with Kenward-Roger approximation for the degrees of freedom, using the
#'   \pkg{pbkrtest}-package. In this case, use \code{show.df = TRUE} to show
#'   the approximated degrees of freedom for each coefficient.
#' @param p.style Character, indicating if p-values should be printed as
#'   numeric value (\code{"numeric"}), as asterisks (\code{"asterisk"})
#'   or both (\code{"both"}). May be abbreviated.
#' @param CSS A \code{\link{list}} with user-defined style-sheet-definitions,
#'    according to the \href{http://www.w3.org/Style/CSS/}{official CSS syntax}.
#'    See 'Details' or \href{../doc/table_css.html}{this package-vignette}.
#' @param file Destination file, if the output should be saved as file.
#'    If \code{NULL} (default), the output will be saved as temporary file and
#'    openend either in the IDE's viewer pane or the default web browser.
#' @param use.viewer Logical, if \code{TRUE}, the HTML table is shown in the IDE's
#'    viewer pane. If \code{FALSE} or no viewer available, the HTML table is
#'    opened in a web browser.
#'
#' @inheritParams plot_models
#' @inheritParams plot_model
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
#' @note The HTML tables can either be saved as file and manually opened (use argument \code{file}) or
#'         they can be saved as temporary files and will be displayed in the RStudio Viewer pane (if working with RStudio)
#'         or opened with the default web browser. Displaying resp. opening a temporary file is the
#'         default behaviour (i.e. \code{file = NULL}).
#'         \cr \cr
#'         Examples are shown in these three vignettes:
#'         \href{../doc/tab_model_estimates.html}{Summary of Regression Models as HTML Table},
#'         \href{../doc/tab_mixed.html}{Summary of Mixed Models as HTML Table} and
#'         \href{../doc/tab_bayes.html}{Summary of Bayesian Models as HTML Table}.
#'
#' @details \strong{Standardized Estimates}
#'    \cr \cr
#'    Concerning the \code{show.std} argument, \code{show.std = "std"}
#'    will print normal standardized estimates. For \code{show.std = "std2"},
#'    however, standardization of estimates follows
#'    \href{http://www.stat.columbia.edu/~gelman/research/published/standardizing7.pdf}{Gelman's (2008)}
#'    suggestion, rescaling the estimates by dividing them by two standard
#'    deviations instead of just one. Resulting coefficients are then
#'    directly comparable for untransformed binary predictors.
#'    For backward compatibility reasons, \code{show.std} also may be
#'    a logical value; if \code{TRUE}, normal standardized estimates are
#'    printed (same effect as \code{show.std = "std"}). Use
#'    \code{show.std = NULL} (default) or \code{show.std = FALSE},
#'    if standardized estimats should not be printed.
#'    \cr \cr
#'    \strong{How do I use \code{CSS}-argument?}
#'    \cr \cr
#'    With the \code{CSS}-argument, the visual appearance of the tables
#'    can be modified. To get an overview of all style-sheet-classnames
#'    that are used in this function, see return value \code{page.style} for details.
#'    Arguments for this list have following syntax:
#'    \enumerate{
#'      \item the class-names with \code{"css."}-prefix as argument name and
#'      \item each style-definition must end with a semicolon
#'    }
#'    You can add style information to the default styles by using a + (plus-sign) as
#'    initial character for the argument attributes. Examples:
#'    \itemize{
#'      \item \code{css.table = 'border:2px solid red;'} for a solid 2-pixel table border in red.
#'      \item \code{css.summary = 'font-weight:bold;'} for a bold fontweight in the summary row.
#'      \item \code{css.lasttablerow = 'border-bottom: 1px dotted blue;'} for a blue dotted border of the last table row.
#'      \item \code{css.colnames = '+color:green'} to add green color formatting to column names.
#'      \item \code{css.arc = 'color:blue;'} for a blue text color each 2nd row.
#'      \item \code{css.caption = '+color:red;'} to add red font-color to the default table caption style.
#'    }
#
#' @importFrom dplyr full_join select if_else mutate
#' @importFrom purrr reduce map2 map_if map_df compact map_lgl map_chr flatten_chr
#' @importFrom sjlabelled get_dv_labels get_term_labels
#' @importFrom sjmisc word_wrap var_rename add_columns add_case
#' @importFrom sjstats std_beta
#' @importFrom insight model_info is_multivariate find_random get_data
#' @importFrom performance r2 icc
#' @importFrom stats nobs
#' @importFrom rlang .data
#' @export
tab_model <- function(
  ...,
  transform,

  show.intercept = TRUE,
  show.est = TRUE,
  show.ci = .95,
  show.ci50 = TRUE,
  show.se = NULL,
  show.std = NULL,
  show.p = TRUE,
  show.stat = FALSE,
  show.df = FALSE,

  show.zeroinf = TRUE,
  show.r2 = TRUE,
  show.icc = TRUE,
  show.re.var = TRUE,
  show.ngroups = TRUE,
  show.fstat = FALSE,
  show.aic = FALSE,
  show.aicc = FALSE,
  show.dev = FALSE,
  show.loglik = FALSE,
  show.obs = TRUE,

  terms = NULL,
  rm.terms = NULL,
  group.terms = TRUE,
  order.terms = NULL,

  title = NULL,
  pred.labels = NULL,
  dv.labels = NULL,
  wrap.labels = 25,

  vcov.fun = NULL,
  vcov.type = c("HC3", "const", "HC", "HC0", "HC1", "HC2", "HC4", "HC4m", "HC5"),
  vcov.args = NULL,

  string.pred = "Predictors",
  string.est = "Estimate",
  string.std = "std. Beta",
  string.ci = "CI",
  string.se = "std. Error",
  string.p = "p",
  string.df = "df",
  string.stat = "Statistic",
  string.resp = "Response",
  string.intercept = "(Intercept)",
  strings = NULL,
  ci.hyphen = "&nbsp;&ndash;&nbsp;",
  minus.sign = "&#45;",

  collapse.ci = FALSE,
  collapse.se = FALSE,
  linebreak = TRUE,

  col.order = c(
    "est",
    "se",
    "std.est",
    "std.se",
    "ci",
    "std.ci",
    "ci.inner",
    "ci.outer",
    "stat",
    "p",
    "df",
    "response.level"
  ),

  digits = 2,
  digits.p = 3,
  emph.p = TRUE,
  p.val = c("wald", "kr"),
  p.style = c("numeric", "asterisk", "both"),
  p.threshold = c(0.05, 0.01, 0.001),

  case = "parsed",
  auto.label = TRUE,
  prefix.labels = c("none", "varname", "label"),
  bpe = "median",
  CSS = css_theme("regression"),
  file = NULL,
  use.viewer = TRUE
) {

  p.val <- match.arg(p.val)
  p.style <- match.arg(p.style)
  prefix.labels <- match.arg(prefix.labels)
  vcov.type <- match.arg(vcov.type)

  change_string_est <- !missing(string.est)

  # if we prefix labels, use different default for case conversion,
  # else the separating white spaces after colon are removed.
  if (missing(case)) {
    if (prefix.labels == "none")
      case <- "parsed"
    else
      case <- NULL
  }

  if (p.style == "asterisk") show.p <- FALSE

  # check se-argument
  vcov.fun <- check_se_argument(se = vcov.fun, type = NULL)


  models <- list(...)

  if (length(class(models[[1]])) == 1 && class(models[[1]]) == "list")
    models <- lapply(models[[1]], function(x) x)

  names(models) <- unlist(lapply(
    match.call(expand.dots = F)$`...`,
    function(.x) deparse(.x, width.cutoff = 500L))
  )

  auto.transform <- missing(transform)
  ci.lvl <- ifelse(is.null(show.ci), .95, show.ci)

  copos <- which("est" == col.order)
  if (!sjmisc::is_empty(copos)) col.order[copos] <- "estimate"

  copos <- which("se" == col.order)
  if (!sjmisc::is_empty(copos)) col.order[copos] <- "std.error"

  copos <- which("ci" == col.order)
  if (!sjmisc::is_empty(copos)) col.order[copos] <- "conf.int"

  copos <- which("std.est" == col.order)
  if (!sjmisc::is_empty(copos)) col.order[copos] <- "std.estimate"

  copos <- which("std.ci" == col.order)
  if (!sjmisc::is_empty(copos)) col.order[copos] <- "std.conf.int"

  copos <- which("p" == col.order)
  if (!sjmisc::is_empty(copos)) col.order[copos] <- "p.value"

  copos <- which("stat" == col.order)
  if (!sjmisc::is_empty(copos)) col.order[copos] <- "statistic"


  # match strings, to label the default strings in the table,
  # like "Estimate", "CI" etc.
  if (!sjmisc::is_empty(strings) && !is.null(names(strings))) {
    s.names <- names(strings)
    if ("pred" %in% s.names) string.pred <- strings[["pred"]]
    if ("est" %in% s.names) string.est <- strings[["est"]]
    if ("std" %in% s.names) string.std <- strings[["std"]]
    if ("ci" %in% s.names) string.ci <- strings[["ci"]]
    if ("se" %in% s.names) string.se <- strings[["se"]]
    if ("p" %in% s.names) string.p <- strings[["p"]]
    if ("df" %in% s.names) string.df <- strings[["df"]]
    if ("stat" %in% s.names) string.stat <- strings[["stat"]]
    if ("resp" %in% s.names) string.resp <- strings[["resp"]]
    if ("intercept" %in% s.names) string.intercept <- strings[["intercept"]]
  }

  model.list <- purrr::map2(
    models,
    1:length(models),
    function(model, i) {

      # get info on model family
      fam.info <- insight::model_info(model)

      if (insight::is_multivariate(model))
        fam.info <- fam.info[[1]]

      # check whether estimates should be transformed or not

      if (auto.transform) {
        if (fam.info$is_linear)
          transform <- NULL
        else
          transform <- "exp"
      }

      # get tidy output of summary ----

      dat <- tidy_model(
        model,
        ci.lvl = ci.lvl,
        transform,
        type = "est",
        bpe,
        se = show.se,
        robust = list(vcov.fun, vcov.type, vcov.args),
        facets = FALSE,
        show.zeroinf = show.zeroinf,
        p.val = p.val
      )


      # transform estimates

      if (!is.stan(model) && !is.null(transform)) {
        funtrans <- match.fun(transform)
        dat[["estimate"]] <- funtrans(dat[["estimate"]])
        dat[["conf.low"]] <- funtrans(dat[["conf.low"]])
        dat[["conf.high"]] <- funtrans(dat[["conf.high"]])
      }


      # merge CI columns

      dat <- dat %>%
        dplyr::mutate(conf.int = sprintf(
          "%.*f%s%.*f",
          digits,
          .data$conf.low,
          ci.hyphen,
          digits,
          .data$conf.high
        )) %>%
        dplyr::select(-.data$conf.low, -.data$conf.high) %>%
        dplyr::mutate(
          p.stars = get_p_stars(.data$p.value, p.threshold),
          p.sig = .data$p.value < .05,
          p.value = sprintf("%.*f", digits.p, .data$p.value)
        )


      # emphasize p-values ----

      if (emph.p && !all(dat$p.value == "NA")) dat$p.value[dat$p.sig] <- sprintf("<strong>%s</strong>", dat$p.value[dat$p.sig])
      dat <- dplyr::select(dat, -.data$p.sig)


      # get inner probability (i.e. 2nd CI for Stan-models) ----

      if (is.stan(model)) {
        dat <- dat %>%
          sjmisc::var_rename(conf.int = "ci.outer") %>%
          dplyr::mutate(ci.inner = sprintf(
            "%.*f%s%.*f",
            digits,
            .data$conf.low50,
            ci.hyphen,
            digits,
            .data$conf.high50
          )) %>%
            dplyr::select(-.data$conf.low50, -.data$conf.high50)
      }


      # indicate p <0.001 ----

      pv <- paste0("0.", paste(rep("0", digits.p), collapse = ""))
      dat$p.value[dat$p.value == pv] <- "&lt;0.001"

      pv <- paste0("<strong>0.", paste(rep("0", digits.p), collapse = ""), "</strong>")
      dat$p.value[dat$p.value == pv] <- "<strong>&lt;0.001"


      # tidy output of standardized values ----

      if (!is.null(show.std) && fam.info$is_linear && !is.stan(model)) {
        dat <- model %>%
          sjstats::std_beta(type = show.std, ci.lvl = ci.lvl) %>%
          sjmisc::var_rename(
            std.error = "std.se",
            conf.low = "std.conf.low",
            conf.high = "std.conf.high"
          ) %>%
          sjmisc::add_case(.after = -1) %>%
          dplyr::select(-1) %>%
          sjmisc::add_columns(dat) %>%
          dplyr::mutate(std.conf.int = sprintf(
            "%.*f%s%.*f",
            digits,
            .data$std.conf.low,
            ci.hyphen,
            digits,
            .data$std.conf.high
          )) %>%
          dplyr::select(-.data$std.conf.low, -.data$std.conf.high)
      }


      # add asterisks to estimates ----

      if (p.style %in% c("asterisk", "both")) {
        if (obj_has_name(dat, "estimate"))
          dat$estimate <- sprintf("%.*f <sup>%s</sup>", digits, dat$estimate, dat$p.stars)
        if (!show.est && obj_has_name(dat, "std.estimate"))
          dat$std.estimate <- sprintf("%.*f <sup>%s</sup>", digits, dat$std.estimate, dat$p.stars)
      }

      dat <- dplyr::select(dat, -.data$p.stars)


      # switch column for p-value and conf. int. ----

      dat <- dat[, sort_columns(colnames(dat), is.stan(model), col.order)]


      # add suffix to column names, so we can distinguish models later

      cn <- colnames(dat)[2:ncol(dat)]
      colnames(dat)[2:ncol(dat)] <- sprintf("%s_%i", cn, i)


      # for HTML, convert numerics to character ----

      dat <- dat %>%
        purrr::map_if(is.numeric, ~ sprintf("%.*f", digits, .x)) %>%
        as.data.frame(stringsAsFactors = FALSE)


      # remove 2nd HDI if requested ----

      if (!show.ci50)
        dat <- dplyr::select(dat, -string_starts_with("ci.inner", colnames(dat)))


      ## TODO optionally insert linebreak for new-line-CI / SE

      # merge estimates and CI / SE columns, if requested ----

      if (collapse.ci) {

        if (linebreak)
          lb <- "<br>"
        else
          lb <- " "

        est.cols <- string_starts_with("estimate", x = colnames(dat))
        dat[[est.cols]] <- sprintf("%s%s(%s)", dat[[est.cols]], lb, dat[[est.cols + 2]])

        # for stan models, we also have 50% HDI
        if (!sjmisc::is_empty(string_starts_with("ci", x = colnames(dat)))) {
          dat <- dplyr::select(dat, -string_starts_with("ci.outer", x = colnames(dat)))
          dat[[est.cols]] <- sprintf("%s%s(%s)", dat[[est.cols]], lb, dat[[est.cols + 2]])
          dat <- dplyr::select(dat, -string_starts_with("ci.inner", x = colnames(dat)))
        } else {
          dat <- dplyr::select(dat, -string_starts_with("conf.int", x = colnames(dat)))
        }

        std.cols <- string_starts_with("std.estimate", x = colnames(dat))
        if (!sjmisc::is_empty(std.cols)) {
          dat[[std.cols]] <- sprintf("%s%s(%s)", dat[[std.cols]], lb, dat[[std.cols + 2]])
          dat <- dplyr::select(dat, -string_starts_with("std.conf.int", x = colnames(dat)))
        }
      }

      if (collapse.se) {

        if (linebreak)
          lb <- "<br>"
        else
          lb <- " "

        est.cols <- string_starts_with("estimate", x = colnames(dat))
        dat[[est.cols]] <- sprintf("%s%s(%s)", dat[[est.cols]], lb, dat[[est.cols + 1]])
        dat <- dplyr::select(dat, -string_starts_with("std.error", x = colnames(dat)))

        std.cols <- string_starts_with("std.estimate", x = colnames(dat))
        if (!sjmisc::is_empty(std.cols)) {
          dat[[std.cols]] <- sprintf("%s%s(%s)", dat[[std.cols]], lb, dat[[std.cols + 1]])
          dat <- dplyr::select(dat, -string_starts_with("std.se", x = colnames(dat)))
        }
      }


      # handle zero-inflation part ----

      zidat <- NULL
      wf <- string_starts_with("wrap.facet", x = colnames(dat))

      if (!sjmisc::is_empty(wf)) {
        zi <- which(dat[[wf]] %in% c("Zero-Inflated Model", "Zero Inflation Model"))

        if (show.zeroinf && !sjmisc::is_empty(zi)) {
          zidat <- dat %>%
            dplyr::slice(!! zi) %>%
            dplyr::select(!! -wf)
        }

        if (!sjmisc::is_empty(zi)) dat <- dplyr::slice(dat, !! -zi)
        dat <- dplyr::select(dat, !! -wf)
      }


      # Add no of observations statistic ----

      n_obs <- NULL

      if (show.obs) {
        n_obs <- get_observations(model)
      }


      vars <- NULL

      # extract variance components ----

      if ((show.icc || show.re.var || show.r2) && is_mixed_model(model)) {
        if (inherits(model, "brmsfit")) {
          vars_brms <- performance::icc(model)
          vars$var.intercept <- attr(vars_brms, "var_rand_intercept")
          vars$var.residual <- attr(vars_brms, "var_residual")
        } else {
          vars <- insight::get_variance(model)
        }
      } else {
        vars <- NULL
      }

      # Add ICC statistic ----

      icc <- NULL

      if (show.icc && is_mixed_model(model) && !is.null(vars) && !all(is.na(vars))) {
        if (inherits(model, "brmsfit")) {
          icc <- list(icc.adjusted = vars_brms$ICC_decomposed)
        } else {
          icc <- list(icc.adjusted = vars$var.random / (vars$var.random + vars$var.residual))
        }
      }

      # Add r-squared statistic ----

      rsq <- NULL

      if (show.r2 && !insight::is_multivariate(model)) {
        # if marginal and conditional r-squared already have been computed
        # via adjusted ICC, use these results and avoid time consuming
        # multiple computation
        if (is_mixed_model(model)) {
          if (inherits(model, "brmsfit")) {
            rsqdummy <- tryCatch(
              {
                suppressWarnings(performance::r2(model))
              },
              error = function(x) { NULL }
            )
            if (!is.null(rsqdummy)) {
              rsq <- list(
                `Marginal R2` = rsqdummy$R2_Bayes_marginal,
                `Conditional R2` = rsqdummy$R2_Bayes
              )
            }
          } else if (!is.null(vars) && !all(is.na(vars))) {
            rsq <- list(
              `Marginal R2` = vars$var.fixed / (vars$var.fixed + vars$var.random + vars$var.residual),
              `Conditional R2` = (vars$var.fixed + vars$var.random) / (vars$var.fixed + vars$var.random + vars$var.residual)
            )
          }
        } else {
          rsq <- tryCatch(
            {
              suppressWarnings(performance::r2(model))
            },
            error = function(x) { NULL }
          )

          # fix names of r-squared values

          if (!is.null(rsq)) {
            rnames <- sub("_", " ", names(rsq))
            names(rsq) <- rnames
          }
        }
      }


      # Add number of random effect groups ----

      n_re_grps <- NULL

      if (show.ngroups && is_mixed_model(model)) {
        rand_eff <- insight::get_data(model)[, insight::find_random(model, split_nested = TRUE, flatten = TRUE), drop = FALSE]
        n_re_grps <- sapply(rand_eff, function(.i) length(unique(.i, na.rm = TRUE)))
        names(n_re_grps) <- sprintf("ngrps.%s", names(n_re_grps))
      }


      # Add deviance and AIC statistic ----

      dev <- NULL
      if (show.dev) dev <- model_deviance(model)

      aic <- NULL
      if (show.aic) aic <- model_aic(model)

      aicc <- NULL
      if (show.aicc) aic <- model_aicc(model)

      loglik <- NULL
      if (show.loglik) loglik <- model_loglik(model)


      ## TODO add F-Statistic


      # fix brms coefficient names

      if (inherits(model, "brmsfit")) {
        dat$term <- gsub("^b_", "", dat$term)
        if (!is.null(zidat)) zidat$term <- gsub("^b_", "", zidat$term)
      }


      # check if Intercept should be renamed...

      if (string.intercept != "(Intercept)") {
        intercepts <- which(dat$term == "(Intercept)")
        if (!sjmisc::is_empty(intercepts)) {
          dat$term[intercepts] <- string.intercept
        }
        if (!is.null(zidat)) {
          intercepts <- which(zidat$term == "(Intercept)")
          if (!sjmisc::is_empty(intercepts)) {
            zidat$term[intercepts] <- string.intercept
          }
        }
      }


      list(
        dat = dat,
        transform = transform,
        zeroinf = zidat,
        rsq = rsq,
        n_obs = n_obs,
        icc = icc,
        dev = dev,
        aic = aic,
        variances = vars,
        n_re_grps = n_re_grps,
        loglik = loglik
      )
    }
  )


  # join all model data frames and convert to character ----

  na.vals <- c(
    "NA",
    sprintf("NA%sNA", ci.hyphen),
    sprintf("NA (NA%sNA)", ci.hyphen),
    sprintf("NA (NA%sNA) (NA)", ci.hyphen)
  )

  # we have data for fixed effects and zero inflation part as
  # well as transformation of coefficients in a list, so separate
  # them out into own objects

  model.data <- purrr::map(model.list, ~.x[[1]])
  transform.data <- purrr::map(model.list, ~.x[[2]])
  zeroinf.data <- purrr::map(model.list, ~.x[[3]])
  rsq.data <- purrr::map(model.list, ~.x[[4]])
  n_obs.data <- purrr::map(model.list, ~.x[[5]])
  icc.data <- purrr::map(model.list, ~.x[[6]])
  dev.data <- purrr::map(model.list, ~.x[[7]])
  aic.data <- purrr::map(model.list, ~.x[[8]])
  variance.data <- purrr::map(model.list, ~.x[[9]])
  ngrps.data <- purrr::map(model.list, ~.x[[10]])
  loglik.data <- purrr::map(model.list, ~.x[[11]])
  is.zeroinf <- purrr::map_lgl(model.list, ~ !is.null(.x[[3]]))

  zeroinf.data <- purrr::compact(zeroinf.data)


  # make sure we don't have zi-data if not wanted

  if (!show.zeroinf) zeroinf.data <- NULL


  # sort multivariate response models by response level

  model.data <- purrr::map(model.data, function(.x) {
    resp.col <- string_starts_with("response.level", x = colnames(.x))
    if (!sjmisc::is_empty(resp.col))
      .x[order(match(.x[[resp.col]], unique(.x[[resp.col]]))), ]
    else
      .x
  })


  # if only one multivariate response model, split data
  # to print models side by side, and update labels of
  # dependent variables

  show.response <- TRUE

  if (length(model.data) == 1) {
    fi <- insight::model_info(models[[1]])

    if (insight::is_multivariate(models[[1]]))
      fi <- fi[[1]]

    if (insight::is_multivariate(models[[1]]) || fi$is_categorical) {

      show.response <- FALSE

      if (fi$is_categorical) {
        dv.labels <- sprintf(
          "%s: %s",
          insight::find_response(models[[1]]),
          unique(model.data[[1]][["response.level_1"]])
        )

        model.data <- split(model.data[[1]], model.data[[1]]["response.level_1"])
      } else {
        dv.labels <- sjmisc::word_wrap(
          sjlabelled::get_dv_labels(models, mv = TRUE, case = case),
          wrap = wrap.labels,
          linesep = "<br>"
        )

        if (sjmisc::is_empty(dv.labels) || !isTRUE(auto.label))
          dv.labels <- insight::find_response(models[[1]])

        model.data <- split(model.data[[1]], model.data[[1]]["response.level_1"])
        dv.labels <- dv.labels[match(names(dv.labels), names(model.data))]
      }

      model.data <- purrr::map2(model.data, 1:length(model.data), function(x, y) {
        colnames(x) <- gsub(
          pattern = "_1",
          replacement = sprintf("_%i", y),
          x = colnames(x)
        )
        x
      })
    }
  }


  # Join all models into one data frame, and replace NA by empty strings

  dat <- model.data %>%
    purrr::reduce(~ dplyr::full_join(.x, .y, by = "term")) %>%
    purrr::map_df(~ dplyr::if_else(.x %in% na.vals | is.na(.x), "", .x))

  # remove unwanted columns and rows ----

  dat <-
    remove_unwanted(
      dat,
      show.intercept,
      show.est,
      show.std,
      show.ci,
      show.se,
      show.stat,
      show.p,
      show.df,
      show.response,
      terms,
      rm.terms
    )


  # same for zero-inflated parts ----

  zeroinf <- NULL
  if (!sjmisc::is_empty(zeroinf.data)) {
    zeroinf <- zeroinf.data %>%
      purrr::reduce(~ dplyr::full_join(.x, .y, by = "term")) %>%
      purrr::map_df(~ dplyr::if_else(.x %in% na.vals | is.na(.x), "", .x))

    zeroinf <-
      remove_unwanted(
        zeroinf,
        show.intercept,
        show.est,
        show.std,
        show.ci,
        show.se,
        show.stat,
        show.p,
        show.df,
        show.response,
        terms,
        rm.terms
      )
  }


  # get default labels for dv and terms ----

  if (isTRUE(auto.label) && sjmisc::is_empty(pred.labels)) {
    pred.labels <- sjlabelled::get_term_labels(models, case = case, mark.cat = TRUE, prefix = prefix.labels)
    no.dupes <- !duplicated(names(pred.labels))
    pred.labels <- prepare.labels(pred.labels[no.dupes], grp = group.terms)
  } else {
    # no automatic grouping of table rows for categorical variables
    # when user supplies own labels
    group.terms <- FALSE
  }


  # to insert "header" rows for categorical variables, we need to
  # save the original term names first.

  # remember.terms <- dat$term


  # named vector for predictor labels means we try to match labels
  # with model terms

  if (!sjmisc::is_empty(pred.labels)) {

    if (!is.null(names(pred.labels))) {
      labs <- sjmisc::word_wrap(pred.labels, wrap = wrap.labels, linesep = "<br>")
      # some labels may not match. in this case, we only need to replace those
      # elements in the vector that match a specific label, but
      # at the correct position inside "dat$term"
      tr <- 1:nrow(dat)
      find.matches <- match(dat$term, names(pred.labels))
      find.na <- which(is.na(find.matches))
      if (!sjmisc::is_empty(find.na)) tr <- tr[-find.na]
      rp <- as.vector(stats::na.omit(find.matches))

      dat$term[tr] <- unname(labs[rp])

      # also label zero-inflated part

      if (!is.null(zeroinf)) {
        tr <- 1:nrow(zeroinf)
        find.matches <- match(zeroinf$term, names(pred.labels))
        find.na <- which(is.na(find.matches))
        if (!sjmisc::is_empty(find.na)) tr <- tr[-find.na]
        rp <- as.vector(stats::na.omit(find.matches))

        zeroinf$term[tr] <- unname(labs[rp])
      }

    } else {
      if (length(pred.labels) == nrow(dat))
        dat$term <- pred.labels
      else
        message("Length of `pred.labels` does not equal number of predictors, no labelling applied.")
    }
  }


  if (isTRUE(auto.label) && sjmisc::is_empty(dv.labels)) {
    dv.labels <- sjmisc::word_wrap(
      sjlabelled::get_dv_labels(models, case = case),
      wrap = wrap.labels,
      linesep = "<br>"
    )
  } else if (sjmisc::is_empty(dv.labels)) {
    dv.labels <- purrr::map(models, insight::find_response) %>% purrr::flatten_chr()
  }


  # group terms ----

  # if (group.terms) {
  #   ## TODO group terms by variables, so category values of factors are "grouped"
  #   remember.terms[attr(pred.labels, "category.value")]
  # }


  # does user want a specific order for terms?

  if (!is.null(order.terms)) {
    if (length(order.terms) == nrow(dat)) {
      dat <- dat[order.terms, ]
    } else {
      message("Number of values in `order.terms` does not match number of terms. Terms are not sorted.")
    }
  }


  # get proper column header labels ----

  col.header <- purrr::map_chr(colnames(dat), function(x) {
    pos <- grep("^estimate_", x)

    if (!sjmisc::is_empty(pos)) {
      i <- as.numeric(sub("estimate_", "", x = x, fixed = T))

      if (insight::is_multivariate(models[[1]]))
        mr <- i
      else
        mr <- NULL

      if (change_string_est && !sjmisc::is_empty(string.est)) {
        x <- string.est
      } else if (i <= length(models)) {
        x <- estimate_axis_title(
          models[[i]],
          axis.title = NULL,
          type = "est",
          transform = transform.data[[i]],
          multi.resp = mr,
          include.zeroinf = FALSE
        )
      } else if (length(models) == 1) {
        x <- estimate_axis_title(
          models[[1]],
          axis.title = NULL,
          type = "est",
          transform = transform.data[[1]],
          multi.resp = mr,
          include.zeroinf = FALSE
        )
      } else {
        x <- string.est
      }
    }


    pos <- grep("^term", x)
    if (!sjmisc::is_empty(pos)) x <- string.pred

    pos <- grep("^conf.int", x)
    if (!sjmisc::is_empty(pos)) x <- string.ci

    pos <- grep("^std.error", x)
    if (!sjmisc::is_empty(pos)) x <- string.se

    pos <- grep("^std.estimate", x)
    if (!sjmisc::is_empty(pos)) x <- string.std

    pos <- grep("^std.se", x)
    if (!sjmisc::is_empty(pos)) x <- paste("standardized", string.se)

    pos <- grep("^std.conf.int", x)
    if (!sjmisc::is_empty(pos)) x <- paste("standardized", string.ci)

    pos <- grep("^p.value", x)
    if (!sjmisc::is_empty(pos)) x <- string.p

    pos <- grep("^df", x)
    if (!sjmisc::is_empty(pos)) x <- string.df

    pos <- grep("^statistic", x)
    if (!sjmisc::is_empty(pos)) x <- string.stat

    pos <- grep("^response.level", x)
    if (!sjmisc::is_empty(pos)) x <- string.resp

    pos <- grep("^ci.inner", x)
    if (!sjmisc::is_empty(pos)) x <- "CI (50%)"

    pos <- grep("^ci.outer", x)
    if (!sjmisc::is_empty(pos)) x <- sprintf("CI (%i%%)", round(100 * show.ci))

    x
  })


  if (p.style %in% c("asterisk", "both"))
    footnote <- sprintf(
      "* p&lt;%s&nbsp;&nbsp;&nbsp;** p&lt;%s&nbsp;&nbsp;&nbsp;*** p&lt;%s",
      format(p.threshold[1]),
      format(p.threshold[2]),
      format(p.threshold[3])
    )
  else
    footnote <- NULL


  tab_model_df(
    x = dat,
    zeroinf = zeroinf,
    is.zeroinf = is.zeroinf,
    title = title,
    col.header = col.header,
    dv.labels = dv.labels,
    rsq.list = rsq.data,
    n_obs.list = n_obs.data,
    icc.list = icc.data,
    dev.list = dev.data,
    aic.list = aic.data,
    variance.list = variance.data,
    ngrps.list = ngrps.data,
    loglik.list = loglik.data,
    n.models = length(model.list),
    show.re.var = show.re.var,
    show.icc = show.icc,
    CSS = CSS,
    file = file,
    use.viewer = use.viewer,
    footnote = footnote
  )
}


#' @importFrom stats na.omit
sort_columns <- function(x, is.stan, col.order) {
  ## TODO check code for multiple response models
  ## TODO allow custom sorting

  reihe <- c(
    "term",
    "estimate",
    "std.error",
    "std.estimate",
    "std.se",
    "conf.int",
    "std.conf.int",
    "ci.inner",
    "ci.outer",
    "statistic",
    "p.value",
    "df",
    "wrap.facet",
    "response.level"
  )

  # fix args
  if (sjmisc::is_empty(col.order)) col.order <- reihe
  if (col.order[1] != "term") col.order <- c("term", col.order)
  if (!("wrap.facet" %in% col.order)) col.order <- c(col.order, "wrap.facet")

  if (is.stan) {
    pcol <- which(col.order == "p.value")
    if (!sjmisc::is_empty(pcol))
      col.order <- col.order[-pcol]
  }

  as.vector(stats::na.omit(match(col.order, x)))
}


#' @importFrom dplyr select slice
remove_unwanted <- function(dat, show.intercept, show.est, show.std, show.ci, show.se, show.stat, show.p, show.df, show.response, terms, rm.terms) {
  if (!show.intercept) {
    ints1 <- string_contains("(Intercept", x = dat$term)
    ints2 <- string_contains("b_Intercept", x = dat$term)
    ints3 <- string_contains("b_zi_Intercept", x = dat$term)
    ints4 <- which(dat$term %in% "Intercept")

    ints <- c(ints1, ints2, ints3, ints4)

    if (!sjmisc::is_empty(ints))
      dat <- dplyr::slice(dat, !! -ints)
  }

  if (show.est == FALSE) {
    dat <- dplyr::select(
      dat,
      -string_starts_with("estimate", x = colnames(dat)),
      -string_starts_with("conf", x = colnames(dat)),
      -string_starts_with("std.error", x = colnames(dat))
    )
  }

  if (is.null(show.std) || show.std == FALSE) {
    dat <- dplyr::select(dat, -string_starts_with("std.estimate", x = colnames(dat)))
  }

  if (is.null(show.ci) || show.ci == FALSE) {
    dat <- dplyr::select(
      dat,
      -string_starts_with("conf", x = colnames(dat)),
      -string_starts_with("std.conf", x = colnames(dat)),
      -string_starts_with("ci", x = colnames(dat))
    )
  }

  if (is.null(show.se) || show.se == FALSE) {
    dat <- dplyr::select(
      dat,
      -string_starts_with("std.error", x = colnames(dat)),
      -string_starts_with("std.se", x = colnames(dat))
    )
  }

  if (show.stat == FALSE) {
    dat <- dplyr::select(dat, -string_starts_with("statistic", x = colnames(dat)))
  }

  if (show.response == FALSE) {
    dat <- dplyr::select(dat, -string_starts_with("response.level", x = colnames(dat)))
  }

  if (show.p == FALSE) {
    dat <- dplyr::select(dat, -string_starts_with("p.value", x = colnames(dat)))
  }

  if (show.df == FALSE) {
    dat <- dplyr::select(dat, -string_starts_with("df", x = colnames(dat)))
  }

  if (!is.null(terms)) {
    terms <- parse_terms(terms)
    keep <- which(dat$term %in% terms)
    dat <- dplyr::slice(dat, !! keep)
  }

  if (!is.null(rm.terms)) {
    rm.terms <- parse_terms(rm.terms)
    keep <- which(!(dat$term %in% rm.terms))
    dat <- dplyr::slice(dat, !! keep)
  }

  dat
}


prepare.labels <- function(x, grp) {
  x_var <- names(x[attr(x, "category.value") == FALSE])
  x_val <- names(x[attr(x, "category.value") == TRUE])

  for (i in x_var) {
    pos <- string_starts_with(i, x = x_val)

    if (!grp || (length(pos) > 0 && length(pos) < 3)) {
      match.vals <- x_val[pos]
      x[match.vals] <- sprintf("%s: %s", x[i], x[match.vals])
    }
  }

  x
}
