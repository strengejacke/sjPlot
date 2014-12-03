#' @docType data
#' @title Sample dataset from the EUROFAMCARE project
#' @name efc
#' @keywords data
#' 
#' @references \url{http://www.uke.de/eurofamcare/}
#' 
#' @seealso \itemize{
#'            \item \code{\link{sji.SPSS}}
#'            \item \code{\link{sji.viewSPSS}}
#'            \item \code{\link{sjt.df}}
#'            \item \code{\link{sji.getValueLabels}}
#'            \item \code{\link{sji.getVariableLabels}}
#'            \item \code{\link{sji.convertToLabel}}
#'            \item \code{\link{sjp.frq}}
#'            \item \href{http://www.strengejacke.de/sjPlot/datainit/}{sjPlot manual: data init}
#'          }
#'          
#' @examples
#' # Attach EFC-data
#' data(efc)
#' 
#' # Show structure
#' str(efc)
#' 
#' # show first rows
#' head(efc)
#' 
#' # show variables
#' \dontrun{
#' sji.viewSPSS(efc)
#' 
#' # show variable labels
#' sji.getVariableLabels(efc)
#' 
#' # plot efc-data frame summary
#' sjt.df(efc, alternateRowColor=TRUE)}
#' 
NULL