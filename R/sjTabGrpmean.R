#' @title Summary of grouped means as HTML table
#' @name sjt.grpmean
#' 
#' @description Computes mean, sd and se for each sub-group (indicated by \code{varGrp})
#'                of \code{varCount} and prints the result as HTML table.
#'              
#' @seealso \code{\link{sjp.aov1}}  
#' 
#' @param varCount a numeric vector / variable. Mean, SD and SE for this variable are calculated.
#' @param varGrp a (numeric) vector with group indices, used to select sub-groups from \code{varCount}.
#' @param weightBy A weight factor that will be applied to weight all cases.
#'          Must be a vector of same length as \code{varCount}. Default is \code{NULL}, 
#'          so no weights are used.
#' @param rowLabels a character vector of same length as \code{varGrp} unqiue values. In short: the
#'          value labels of \code{varGrp}. Used to name table rows. By default, row labels
#'          are automatically detected if set by \code{\link[sjmisc]{set_labels}}.
#' @param digits amount of digits for table values.
#' @param digits.summary amount of digits for summary statistics (Anova).
#' 
#' @inheritParams sjt.frq
#' 
#' @return Invisibly returns a \code{\link{list}} with
#'          \itemize{
#'            \item the data frame with the description information (\code{df}),
#'            \item the web page style sheet (\code{page.style}),
#'            \item the web page content (\code{page.content}),
#'            \item the complete html-output (\code{output.complete}) and
#'            \item the html-table with inlin-css for use with knitr (\code{knitr})
#'            }
#'            for further use.
#'
#' @note See 'Notes' in \code{\link{sjt.frq}}.
#'  
#' @details This function performs a One-Way-Anova with \code{varCount} as dependent
#'            and \code{varGrp} as independent variable, by calling
#'            \code{lm(varCount ~ as.factor(varGrp))}, to get p-values for each
#'            sub-group and the complete "model". Thus, p-values indicate whether
#'            each group-mean is significantly different from the reference 
#'            group (reference level of \code{varGrp}). Statistics like mean values are
#'            based on subsetted groups (i.e. \code{varCount} is divided into sub-groups
#'            indicated by \code{varGrp}).
#'            \cr \cr
#'            Furthermore, see 'Details' in \code{\link{sjt.frq}}.
#'
#' @examples
#' \dontrun{
#' library(sjmisc)
#' data(efc)
#' sjt.grpmean(efc$c12hour,
#'             efc$e42dep)}
#'             
#' @import sjmisc
#' @importFrom stats na.omit lm
#' @export
sjt.grpmean <- function(varCount,
                        varGrp,
                        weightBy = NULL,
                        rowLabels = NULL,
                        digits = 2,
                        digits.summary = 3,
                        file = NULL,
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
  # --------------------------------------
  # set value and row labels
  # --------------------------------------
  if (is.null(rowLabels)) rowLabels <- sjmisc::get_labels(varGrp,
                                                          attr.only = F,
                                                          include.values = NULL,
                                                          include.non.labelled = T)
  varGrpLabel <- sjmisc::get_label(varGrp, def.value = get_var_name(deparse(substitute(varGrp))))
  varCountLabel <- sjmisc::get_label(varCount, def.value = get_var_name(deparse(substitute(varCount))))
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
  if (!is.null(weightBy)) {
    fit <- stats::lm(varCount ~ as.factor(varGrp), weights = weightBy)
  } else {
    fit <- stats::lm(varCount ~ as.factor(varGrp))
  }
  # get model summary
  sum.fit <- summary(fit)
  # p-values of means
  means.p <- sum.fit$coefficients[, 4]
  pval <- c()
  # convert means to apa style
  for (i in 1:length(means.p)) {
    if (means.p[i] < 0.001) {
      pval <- c(pval, sprintf("&lt;%s.001", p_zero))
    } else {
      pval <- c(pval, sub("0", p_zero, sprintf("%.*f", digits, means.p[i]), fixed = T))
    }
  } 
  # --------------------------------------
  # retrieve group indices
  # --------------------------------------
  indices <- sort(unique(stats::na.omit(varGrp)))
  df <- data.frame()
  # --------------------------------------
  # iterate all groups
  # --------------------------------------
  for (i in 1:length(indices)) {
    # --------------------------------------
    # do we have weighted means?
    # --------------------------------------
    if (!is.null(weightBy)) {
      mw <- weighted.mean(varCount[varGrp == indices[i]], 
                          w = weightBy[varGrp == indices[i]],
                          na.rm = TRUE)
    } else {
      mw <- mean(varCount[varGrp == indices[i]], na.rm = TRUE)
    }
    # --------------------------------------
    # add new row to data frame with
    # mean, N, sd and se of varCount for each
    # sub-group (indicated by indices)
    # --------------------------------------
    df <- rbind(df, 
                cbind(mean = sprintf("%.*f", digits, mw),
                      N = length(stats::na.omit(varCount[varGrp == indices[i]])),
                      sd = sprintf("%.*f", digits, sd(varCount[varGrp == indices[i]], na.rm = TRUE)),
                      se = sprintf("%.*f", digits, sjmisc::se(varCount[varGrp == indices[i]])),
                      p = pval[i]))
  }
  # --------------------------------------
  # do we have weighted means?
  # --------------------------------------
  if (!is.null(weightBy)) {
    mw <- weighted.mean(varCount, 
                        w = weightBy,
                        na.rm = TRUE)
  } else {
    mw <- mean(varCount, na.rm = TRUE)
  }
  # --------------------------------------
  # finally, add total-row
  # --------------------------------------
  df <- rbind(df, 
              cbind(mean = sprintf("%.*f", digits, mw),
                    N = length(stats::na.omit(varCount)),
                    sd = sprintf("%.*f", digits, sd(varCount, na.rm = TRUE)),
                    se = sprintf("%.*f", digits, sjmisc::se(varCount)),
                    p = ""))
  # --------------------------------------
  # fix row labels, if empty or NULL
  # --------------------------------------
  if (is.null(rowLabels) || length(rowLabels) < (nrow(df) - 1)) rowLabels <- as.character(indices)
  rownames(df) <- c(rowLabels, "Total")
  # --------------------------------------
  # get anova statistics for mean table
  # --------------------------------------
  # multiple r2
  r2 <- sum.fit$r.squared
  # adj. r2
  r2.adj <- sum.fit$adj.r.squared
  # get F-statistics
  fstat <- sum.fit$fstatistic[1]
  # p-value for F-test
  pval <- sjmisc:::lm_pval_fstat(fit)
  pvalstring <- ifelse(pval < 0.001, 
                       sprintf("p&lt;%s.001", p_zero), 
                       sub("0", p_zero, sprintf("p=%.*f", digits.summary, pval)))
  eta <- sub("0", p_zero, sprintf("&eta;=%.*f", digits.summary, sqrt(r2)))
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
                 commentString = gsub("=0.", 
                                      paste0("=", p_zero, "."), 
                                      sprintf("<strong>Anova:</strong> R<sup>2</sup>=%.*f &middot; adj. R<sup>2</sup>=%.*f &middot; %s &middot; F=%.*f &middot; %s",
                                              digits.summary, r2, digits.summary, r2.adj, eta, digits.summary, fstat, pvalstring),
                                      fixed = TRUE),
                 remove.spaces = remove.spaces)
  # -------------------------------------
  # check if html-content should be printed
  # -------------------------------------
  out.html.table(no.output, file, html$knitr, html$output.complete, useViewer)  
  invisible(list(class = c("sjTable", "sjtgrpmean"),
                 df = df, 
                 page.style = html$page.style,
                 page.content = html$page.content,
                 knitr = html$knitr,
                 output.complete = html$output.complete))
}
