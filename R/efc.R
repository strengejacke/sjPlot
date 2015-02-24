#' @docType data
#' @title Sample dataset from the EUROFAMCARE project
#' @name efc
#' @keywords data
#' 
#' @references \url{http://www.uke.de/eurofamcare/}
#' 
#' @seealso \itemize{
#'            \item \code{\link{read_spss}}
#'            \item \code{\link{view_spss}}
#'            \item \code{\link{get_val_labels}}
#'            \item \code{\link{get_var_labels}}
#'            \item \code{\link{to_label}}
#'            \item \code{\link{sjt.df}}
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
#' view_spss(efc)
#' 
#' # show variable labels
#' get_var_label(efc)
#' 
#' # plot efc-data frame summary
#' sjt.df(efc, alternateRowColor=TRUE)}
#' 
NULL