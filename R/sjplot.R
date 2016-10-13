#' @title Wrapper to create plots and tables within a pipe-workflow
#' @name sjplot
#'
#' @description This function has a pipe-friendly argument-structure, with the
#'              first argument always being the data, followed by variables that
#'              should be plotted or printed as table. The function then transforms 
#'              the input and calls the requested sjp.xy- resp. sjt.xy-function 
#'              to create a plot or table.
#'
#' @param .data A data frame.
#' @param ... Names of variables that should be plotted, and also further 
#'          arguments passed down to the \pkg{sjPlot}-functions. See 'Examples'.
#' @param fun Plotting function. Refers to the function name of \pkg{sjPlot}-functions.
#'          See 'Details' and 'Examples'.
#' 
#' @return NULL.
#' 
#' @note The \code{...}-argument is used, first, to specify the variables from \code{.data}
#'       that should be plotted, and, second, to name further arguments that are
#'       used in the subsequent plotting functions. Refer to the online-help of
#'       supported plotting-functions to see valid arguments.
#' 
#' @details Following \code{fun}-values are currently supported:
#'          \describe{
#'             \item{\code{"frq"}}{calls \code{\link{sjp.frq}}. If \code{.data} 
#'             has more than one variable, a plot for each variable in \code{.data} 
#'             is plotted.
#'             }
#'             \item{\code{"grpfrq"}}{calls \code{\link{sjp.grpfrq}}. The first 
#'             two variables in \code{.data} are used (and required) to create the plot.
#'             }
#'             \item{\code{"xtab"}}{calls \code{\link{sjp.xtab}}. The first 
#'             two variables in \code{.data} are used (and required) to create the plot.
#'             }
#'             \item{\code{"gpt"}}{calls \code{\link{sjp.gpt}}. The first 
#'             three variables in \code{.data} are used (and required) to create the plot.
#'             }
#'             \item{\code{"scatter"}}{calls \code{\link{sjp.scatter}}. The first 
#'             two variables in \code{.data} are used (and required) to create the plot;
#'             if \code{.data} also has a third variable, this is used as grouping-
#'             variable in \code{sjp.scatter}.
#'             }
#'          }
#' 
#' @examples
#' library(sjmisc)
#' library(dplyr)
#' data(efc)
#' 
#' # Frequency plot
#' sjplot(efc, e42dep, c172code, fun = "frq")
#' 
#' # Grouped frequencies
#' efc %>% sjplot(e42dep, c172code, fun = "grpfrq")
#' 
#' # Grouped frequencies, as box plots
#' efc %>% sjplot(e17age, c172code, fun = "grpfrq", 
#'                type = "box", geom.colors = "Set1")
#' 
#' # scatter plot, grouped
#' efc %>% 
#'   select(e42dep, c172code, c161sex) %>% 
#'   sjplot(fun = "scatter")
#'
#'
#' @importFrom sjmisc is_empty
#' @export
sjplot <- function(.data, ..., fun = c("frq", "grpfrq", "xtab", "gpt", "scatter")) {
  # check if x is a data frame
  if (!is.data.frame(.data)) stop("`x` must be a data frame.", call. = F)
  
  # match fun-arguments
  fun <- match.arg(fun)
  
  # evaluate arguments, generate data
  x <- get_dot_data(.data, match.call(expand.dots = FALSE)$`...`)
  
  # check remaining arguments
  args <- match.call(expand.dots = FALSE)$`...`
  args <- args[names(args) != ""]
  
  # choose plottype, and call plot-function with or w/o additional arguments
  if (sjmisc::is_empty(args)) {
    if (fun == "frq") {
      for (i in seq_len(ncol(x))) sjp.frq(x[[i]])
    } else if (fun  == "grpfrq") {
      sjp.grpfrq(x[[1]], x[[2]])
    } else if (fun  == "xtab") {
      sjp.xtab(x[[1]], x[[2]])
    } else if (fun  == "gpt") {
      sjp.gpt(x[[1]], x[[2]], x[[3]])
    } else if (fun  == "scatter") {
      sjp.scatter(x[[1]], x[[2]], x[[3]])
    }
  } else {
    if (fun == "frq") {
      for (i in seq_len(ncol(x))) do.call(sjp.frq, args = c(list(var.cnt = x[[i]]), args))
    } else if (fun  == "grpfrq") {
      do.call(sjp.grpfrq, args = c(list(var.cnt = x[[1]], var.grp = x[[2]]), args))
    } else if (fun  == "xtab") {
      do.call(sjp.xtab, args = c(list(x = x[[1]], grp = x[[2]]), args))
    } else if (fun  == "gpt") {
      do.call(sjp.gpt, args = c(list(x = x[[1]], y = x[[2]], groups = x[[3]]), args))
    } else if (fun  == "scatter") {
      do.call(sjp.scatter, args = c(list(x = x[[1]], y = x[[2]], grp = x[[3]]), args))
    }
  }
}


#' @rdname sjplot
#' @export
sjtab <- function(.data, ..., fun = c("frq", "xtab")) {
  # check if x is a data frame
  if (!is.data.frame(.data)) stop("`x` must be a data frame.", call. = F)
  
  # match fun-arguments
  fun <- match.arg(fun)
  
  # evaluate arguments, generate data
  x <- get_dot_data(.data, match.call(expand.dots = FALSE)$`...`)
  
  # check remaining arguments
  args <- match.call(expand.dots = FALSE)$`...`
  args <- args[names(args) != ""]
  
  # choose plottype, and call plot-function with or w/o additional arguments
  if (sjmisc::is_empty(args)) {
    if (fun == "frq") {
      sjt.frq(x)
    } else if (fun  == "xtab") {
      sjt.xtab(x[[1]], x[[2]])
    }
  } else {
    if (fun == "frq") {
      do.call(sjt.frq, args = c(list(data = x), args))
    } else if (fun  == "xtab") {
      do.call(sjp.xtab, args = c(list(var.row = x[[1]], var.col = x[[2]]), args))
    }
  }
}
