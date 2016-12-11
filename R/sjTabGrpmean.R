#' @title Summary of grouped means as HTML table
#' @name sjt.grpmean
#' 
#' @description Computes mean, sd and se for each sub-group (indicated by \code{var.grp})
#'                of \code{var.cnt} and prints the result as HTML table.
#'              
#' @seealso \code{\link{sjp.aov1}}  
#' 
#' @param digits.summary amount of digits for summary statistics (Anova).
#' 
#' @inheritParams sjt.frq
#' @inheritParams sjp.glmer
#' @inheritParams sjp.grpfrq
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
#' @details This function performs a One-Way-Anova with \code{var.cnt} as dependent
#'            and \code{var.grp} as independent variable, by calling
#'            \code{lm(var.cnt ~ as.factor(var.grp))}, to get p-values for each
#'            sub-group and the complete "model". Thus, p-values indicate whether
#'            each group-mean is significantly different from the reference 
#'            group (reference level of \code{var.grp}). Statistics like mean values are
#'            based on subsetted groups (i.e. \code{var.cnt} is divided into sub-groups
#'            indicated by \code{var.grp}).
#'            \cr \cr
#'            Furthermore, see 'Details' in \code{\link{sjt.frq}}.
#'
#' @examples
#' \dontrun{
#' library(sjmisc)
#' data(efc)
#' sjt.grpmean(efc$c12hour, efc$e42dep)}
#'             
#' @importFrom stats na.omit lm
#' @export
sjt.grpmean <- function(var.cnt,
                        var.grp,
                        weight.by = NULL,
                        value.labels = NULL,
                        digits = 2,
                        digits.summary = 3,
                        CSS = NULL,
                        encoding = NULL,
                        file = NULL,
                        use.viewer = TRUE,
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
  if (is.null(value.labels)) {
    # first, drop unused labels
    var.grp <- sjmisc::drop_labels(var.grp, drop.na = TRUE)
    # now get valid value labels
    value.labels <- sjmisc::get_labels(
      var.grp, attr.only = F, include.values = NULL, include.non.labelled = T
    )
  }
  varGrpLabel <- sjmisc::get_label(var.grp, def.value = get_var_name(deparse(substitute(var.grp))))
  varCountLabel <- sjmisc::get_label(var.cnt, def.value = get_var_name(deparse(substitute(var.cnt))))
  # --------------------------------------
  # handle NULL parameter
  # --------------------------------------
  if (is.null(varGrpLabel)) varGrpLabel <- ""
  if (is.null(varCountLabel)) varCountLabel <- "Compare means"
  # --------------------------------------
  # convert values to numeric
  # --------------------------------------
  var.cnt <- as.numeric(var.cnt)
  var.grp <- as.numeric(var.grp)
  # --------------------------------------
  # compute anova statistics for mean table
  # see below
  # --------------------------------------
  if (!is.null(weight.by)) {
    fit <- stats::lm(var.cnt ~ as.factor(var.grp), weights = weight.by)
  } else {
    fit <- stats::lm(var.cnt ~ as.factor(var.grp))
  }
  # get model summary
  sum.fit <- summary(fit)
  # p-values of means
  means.p <- sum.fit$coefficients[, 4]
  pval <- c()
  # convert means to apa style
  for (i in seq_len(length(means.p))) {
    if (means.p[i] < 0.001) {
      pval <- c(pval, sprintf("&lt;%s.001", p_zero))
    } else {
      pval <- c(pval, sub("0", p_zero, sprintf("%.*f", digits, means.p[i]), fixed = T))
    }
  } 
  # --------------------------------------
  # retrieve group indices
  # --------------------------------------
  indices <- sort(unique(stats::na.omit(var.grp)))
  df <- data.frame()
  # --------------------------------------
  # iterate all groups
  # --------------------------------------
  for (i in seq_len(length(indices))) {
    # --------------------------------------
    # do we have weighted means?
    # --------------------------------------
    if (!is.null(weight.by)) {
      mw <- weighted.mean(var.cnt[var.grp == indices[i]], 
                          w = weight.by[var.grp == indices[i]],
                          na.rm = TRUE)
    } else {
      mw <- mean(var.cnt[var.grp == indices[i]], na.rm = TRUE)
    }
    # --------------------------------------
    # add new row to data frame with
    # mean, N, sd and se of var.cnt for each
    # sub-group (indicated by indices)
    # --------------------------------------
    df <- rbind(df, 
                cbind(mean = sprintf("%.*f", digits, mw),
                      N = length(stats::na.omit(var.cnt[var.grp == indices[i]])),
                      sd = sprintf("%.*f", digits, sd(var.cnt[var.grp == indices[i]], na.rm = TRUE)),
                      se = sprintf("%.*f", digits, sjstats::se(var.cnt[var.grp == indices[i]])),
                      p = pval[i]))
  }
  # --------------------------------------
  # do we have weighted means?
  # --------------------------------------
  if (!is.null(weight.by)) {
    mw <- weighted.mean(var.cnt, w = weight.by, na.rm = TRUE)
  } else {
    mw <- mean(var.cnt, na.rm = TRUE)
  }
  # --------------------------------------
  # finally, add total-row
  # --------------------------------------
  df <- rbind(df, 
              cbind(mean = sprintf("%.*f", digits, mw),
                    N = length(stats::na.omit(var.cnt)),
                    sd = sprintf("%.*f", digits, sd(var.cnt, na.rm = TRUE)),
                    se = sprintf("%.*f", digits, sjstats::se(var.cnt)),
                    p = ""))
  # --------------------------------------
  # fix row labels, if empty or NULL
  # --------------------------------------
  if (is.null(value.labels) || length(value.labels) < (nrow(df) - 1)) value.labels <- as.character(indices)
  rownames(df) <- c(value.labels, "Total")
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
  pval <- sjstats:::lm_pval_fstat(fit)
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
                 string.var = varGrpLabel,
                 show.rownames = T,
                 show.cmmn.row = T,
                 no.output = T,
                 CSS = CSS,
                 encoding = encoding,
                 hide.progress = TRUE,
                 string.cmmn = gsub("=0.", paste0("=", p_zero, "."), 
                                    sprintf("<strong>Anova:</strong> R<sup>2</sup>=%.*f &middot; adj. R<sup>2</sup>=%.*f &middot; %s &middot; F=%.*f &middot; %s",
                                            digits.summary, r2, digits.summary, r2.adj, eta, digits.summary, fstat, pvalstring),
                                    fixed = TRUE),
                 remove.spaces = remove.spaces)
  # -------------------------------------
  # check if html-content should be printed
  # -------------------------------------
  out.html.table(no.output, file, html$knitr, html$output.complete, use.viewer)  
  invisible(list(class = c("sjTable", "sjtgrpmean"),
                 df = df, 
                 page.style = html$page.style,
                 page.content = html$page.content,
                 knitr = html$knitr,
                 output.complete = html$output.complete))
}
