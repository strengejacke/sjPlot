#' @title Show Mann-Whitney-Test as HTML table
#' @name sjt.mwu
#' 
#' @description Shows the results of a Mann-Whitney-U-test as HTML table. The results
#'                from the Mann-Whitney-test are obtained by the \code{\link[sjmisc]{mwu}}
#'                function from the \code{sjmisc} package.
#'
#' @param x Results of a Mann-Whitney-U test, provided by \code{\link[sjmisc]{mwu}}. See examples.
#' @param alternateRowColors If \code{TRUE}, alternating rows are highlighted with a light gray
#'          background color.
#' @param title A table caption. By default, \code{title} is \code{NULL}, hence no title will be used.
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
#'            \item \code{css.arc='color:blue;'} for a blue text color each 2nd row.
#'            \item \code{css.caption='+color:red;'} to add red font-color to the default table caption style.
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
#' @return Invisibly returns a \code{\link{list}} with
#'          \itemize{
#'            \item the data frame with the description information (\code{data}),
#'            \item the web page style sheet (\code{page.style}),
#'            \item the web page content (\code{page.content}),
#'            \item the complete html-output (\code{output.complete}) and
#'            \item the html-table with inline-css for use with knitr (\code{knitr})
#'            }
#'            for further use.
#'
#' @examples 
#' \dontrun{
#' library(sjmisc)
#' data(efc)
#' sjt.mwu(mwu(efc$e17age, efc$e42dep))}
#'
#' @export
sjt.mwu <- function(x, 
                    title = NULL, 
                    alternateRowColors = TRUE, 
                    file=NULL,
                    encoding=NULL,
                    CSS=NULL,
                    useViewer = TRUE, 
                    no.output = FALSE,
                    remove.spaces = TRUE) {
  # --------------------------------------------------------
  # check correct class
  # --------------------------------------------------------
  if (class(x) != "mwu") {
    stop("'x' must be of class 'mwu', as returned by the 'mwu'-function of the sjmisc-package. See ?sjt.mwu for details.", call. = F)
  }
  # --------------------------------------------------------
  # check p-value-style option
  # --------------------------------------------------------
  opt <- getOption("p_zero")
  if (is.null(opt) || opt == FALSE) {
    p_zero <- ""
  } else {
    p_zero <- "0"
  }
  # --------------------------------------------------------
  # fix p- ans r-values
  # --------------------------------------------------------
  x$tab.df$p <- sub("0", p_zero, x$tab.df$p, fixed = T)
  x$tab.df$Effect.Size <- sub("0", p_zero, x$tab.df$Effect.Size, fixed = T)
  # --------------------------------------------------------
  # print table and return results
  # --------------------------------------------------------
  html <- sjt.df(x$tab.df, 
                 title = title,
                 describe = F, 
                 showRowNames = F, 
                 alternateRowColors = alternateRowColors,
                 CSS = CSS,
                 no.output = T,
                 encoding = encoding,
                 hideProgressBar = T,
                 remove.spaces = remove.spaces)
  # -------------------------------------
  # check if html-content should be printed
  # -------------------------------------
  out.html.table(no.output, file, html$knitr, html$output.complete, useViewer)  
  invisible (list(class = "sjtmwu",
                  df = x$tab.df, 
                  page.style = html$page.style,
                  page.content = html$page.content,
                  knitr = html$knitr,
                  output.complete = html$output.complete))
}
