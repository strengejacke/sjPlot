#' @title Summary of grouped means as HTML table
#' @name sjt.grpmean
#'
#' @description Computes mean, sd and se for each sub-group (indicated by \code{var.grp})
#'                of \code{var.cnt} and prints the result as HTML table.
#'
#' @seealso \code{\link{sjp.aov1}}
#'
#' @param digits.summary Amount of digits for summary statistics (Anova).
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
#' @importFrom sjstats eta_sq grpmean
#' @importFrom tibble column_to_rownames
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

  # set default for weights
  if (is.null(weight.by)) weight.by <- 1

  # create data frame
  mydf <-
    data.frame(
      dv = var.cnt,
      grp = sjlabelled::as_factor(var.grp),
      weight.by = weight.by
    )

  # call sjstats::grpmean, to create data frame
  dat <-
    sjstats::grpmean(
      x = mydf,
      dv = "dv",
      grp = "grp",
      weight.by = "weight.by",
      digits = digits
    )

  # get statistics
  eta <- sub("0", p_zero, sprintf("&eta;=%.*f", digits.summary, sqrt(attr(dat, "r2"))))

  pval <- attr(dat, "p.value")
  pvalstring <-
    ifelse(pval < 0.001,
           sprintf("p&lt;%s.001", p_zero),
           sub("0", p_zero, sprintf("p=%.*f", digits.summary, pval)))


  # print data frame to html table
  html <- sjt.df(
    suppressWarnings(tibble::column_to_rownames(dat, var = "term")),
    describe = F,
    title = attr(dat, "dv.label"),
    string.var = attr(dat, "grp.label"),
    show.rownames = T,
    show.cmmn.row = T,
    no.output = T,
    CSS = CSS,
    encoding = encoding,
    hide.progress = TRUE,
    string.cmmn = gsub(
      "=0.",
      paste0("=", p_zero, "."),
      sprintf(
        "<strong>Anova:</strong> R<sup>2</sup>=%.*f &middot; adj. R<sup>2</sup>=%.*f &middot; %s &middot; F=%.*f &middot; %s",
        digits.summary,
        attr(dat, "r2"),
        digits.summary,
        attr(dat, "adj.r2"),
        eta,
        digits.summary,
        attr(dat, "fstat"),
        pvalstring
      ),
      fixed = TRUE
    ),
    remove.spaces = remove.spaces)

  structure(
    class = c("sjTable", "sjtgrpmean"),
    list(
      output.complete = html$output.complete,
      knitr = html$knitr,
      file = file,
      page.style = html$page.style,
      page.content = html$page.content,
      header = html$header,
      show = !no.output,
      use.viewer = use.viewer
    )
  )
}
