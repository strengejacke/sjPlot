#' @docType data
#' @title Sample dataset from the EUROFAMCARE project
#' @name efc
#' @keywords data
#' 
#' @references \url{http://www.uke.de/eurofamcare/}
#' 
#' @seealso \itemize{
#'            \item \href{http://www.strengejacke.de/sjPlot/datainit/}{sjPlot manual: data init}
#'            \item \code{\link{read_spss}}
#'            \item \code{\link{view_spss}}
#'            \item \code{\link{get_val_labels}}
#'            \item \code{\link{get_var_labels}}
#'            \item \code{\link{to_label}}
#'            \item \code{\link{sjt.df}}
#'          }
#' 
#' @note There are two further datasets, \code{efc2} and \code{efc3}, which
#'         slightly differ in their structure. \code{efc2} has already attached
#'         variable label attributes to each variable. In \code{efc3}, categorical
#'         variables have been converted to labelled factors, i.e. value labels 
#'         are set as factor levels. However, factors in \code{efc3} no longer
#'         have variable label attributes.
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


#' @docType data
#' @title Sample dataset from the EUROFAMCARE project
#' @name efc2
#' @keywords data
#' 
#' @references \url{http://www.uke.de/eurofamcare/}
#' 
#' @seealso \itemize{
#'            \item \href{http://www.strengejacke.de/sjPlot/datainit/}{sjPlot manual: data init}
#'            \item \code{\link{read_spss}}
#'            \item \code{\link{view_spss}}
#'            \item \code{\link{get_val_labels}}
#'            \item \code{\link{get_var_labels}}
#'            \item \code{\link{to_label}}
#'            \item \code{\link{sjt.df}}
#'          }
#' 
#' @examples
#' # Attach EFC-data
#' data(efc2)
#' 
#' \dontrun{
#' # show variables
#' view_spss(efc2)
#' 
#' # print frq of dependency
#' sjt.frq(efc2$e42dep)}
#' 
NULL


#' @docType data
#' @title Sample dataset from the EUROFAMCARE project
#' @name efc3
#' @keywords data
#' 
#' @references \url{http://www.uke.de/eurofamcare/}
#' 
#' @seealso \itemize{
#'            \item \href{http://www.strengejacke.de/sjPlot/datainit/}{sjPlot manual: data init}
#'            \item \code{\link{read_spss}}
#'            \item \code{\link{view_spss}}
#'            \item \code{\link{get_val_labels}}
#'            \item \code{\link{get_var_labels}}
#'            \item \code{\link{to_label}}
#'            \item \code{\link{sjt.df}}
#'          }
#' 
#' @examples
#' # Attach EFC-data
#' data(efc3)
#' 
#' str(efc3$e15relat)
#' table(efc3$e15relat)
#' 
#' \dontrun{
#' # print frq of relationships
#' sjt.frq(efc3$e15relat)}
#' 
NULL
