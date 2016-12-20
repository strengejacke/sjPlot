#' @title Wrapper to create plots and tables within a pipe-workflow
#' @name sjplot
#'
#' @description This function has a pipe-friendly argument-structure, with the
#'              first argument always being the data, followed by variables that
#'              should be plotted or printed as table. The function then transforms 
#'              the input and calls the requested sjp.- resp. sjt.-function 
#'              to create a plot or table. \cr \cr
#'              Both \code{sjplot()} and \code{sjtab()} support grouped data frames.
#'
#' @param data A data frame. May also be a grouped data frame (see 'Note' and
#'          'Examples').
#' @param ... Names of variables that should be plotted, and also further 
#'          arguments passed down to the \pkg{sjPlot}-functions. See 'Examples'.
#' @param fun Plotting function. Refers to the function name of \pkg{sjPlot}-functions.
#'          See 'Details' and 'Examples'.
#'          
#' @return See related sjp. and sjt.-functions.
#' 
#' @note The \code{...}-argument is used, first, to specify the variables from \code{data}
#'       that should be plotted, and, second, to name further arguments that are
#'       used in the subsequent plotting functions. Refer to the online-help of
#'       supported plotting-functions to see valid arguments.
#'       \cr \cr
#'       \code{data} may also be a grouped data frame (see \code{\link[dplyr]{group_by}})
#'       with up to two grouping variables. Plots are created for each subgroup then.
#' 
#' @details Following \code{fun}-values are currently supported:
#'          \describe{
#'             \item{\code{"aov1"}}{calls \code{\link{sjp.aov1}}. The first 
#'             two variables in \code{data} are used (and required) to create the plot.
#'             }
#'             \item{\code{"frq"}}{calls \code{\link{sjp.frq}} or \code{\link{sjt.frq}}.
#'             If \code{data} has more than one variable, a plot for each 
#'             variable in \code{data} is plotted.
#'             }
#'             \item{\code{"gpt"}}{calls \code{\link{sjp.gpt}}. The first 
#'             three variables in \code{data} are used (and required) to create the plot.
#'             }
#'             \item{\code{"grpfrq"}}{calls \code{\link{sjp.grpfrq}}. The first 
#'             two variables in \code{data} are used (and required) to create the plot.
#'             }
#'             \item{\code{"grpmean"}}{calls \code{\link{sjt.grpmean}}.
#'             The first two variables in \code{data} are used (and required) 
#'             to create the table.
#'             }
#'             \item{\code{"likert"}}{calls \code{\link{sjp.likert}}. \code{data} 
#'             must be a data frame with items to plot.
#'             }
#'             \item{\code{"scatter"}}{calls \code{\link{sjp.scatter}}. The first 
#'             two variables in \code{data} are used (and required) to create the plot;
#'             if \code{data} also has a third variable, this is used as grouping-
#'             variable in \code{sjp.scatter}.
#'             }
#'             \item{\code{"stackfrq"}}{calls \code{\link{sjp.stackfrq}} or \code{\link{sjt.stackfrq}}.
#'             \code{data} must be a data frame with items to create the plot or table.
#'             }
#'             \item{\code{"xtab"}}{calls \code{\link{sjp.xtab}} or \code{\link{sjt.xtab}}.
#'             The first two variables in \code{data} are used (and required) 
#'             to create the plot or table.
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
#' # frequencies, as plot grid
#' efc %>% 
#'   select(e42dep, c172code, e16sex, c161sex) %>% 
#'   sjplot() %>% 
#'   plot_grid()
#'
#' # plot grouped data frame
#' efc %>% 
#'   group_by(e16sex, c172code) %>% 
#'   select(e42dep, e16sex, c172code) %>% 
#'   sjplot(wrap.title = 100) # no line break for subtitles
#'
#' \dontrun{
#' # table output of grouped data frame
#' efc %>% 
#'   group_by(e16sex, c172code) %>% 
#'   select(e42dep, n4pstu, e16sex, c172code) %>% 
#'   sjtab(fun = "xtab", use.viewer = FALSE) # open all tables in browser}
#'
#' @importFrom sjmisc is_empty copy_labels get_label get_labels
#' @importFrom dplyr select_ filter
#' @importFrom tidyr nest
#' @importFrom stats complete.cases
#' @export
sjplot <- function(data, ..., fun = c("frq", "grpfrq", "xtab", "gpt", "scatter", 
                                      "aov1", "likert", "stackfrq")) {
  # check if x is a data frame
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = F)
  
  # match arguments
  fun <- match.arg(fun)

  # evaluate arguments, generate data
  x <- get_dot_data(data, match.call(expand.dots = FALSE)$`...`)
  
  # check remaining arguments
  args <- match.call(expand.dots = FALSE)$`...`
  args <- args[names(args) != ""]
  
  p <- NULL
  pl <- NULL
  
  # do we have a grouped data frame?
  if (inherits(x, "grouped_df")) {
    # get grouped data
    grps <- get_grouped_data(x)
    
    # now plot everything
    for (i in seq_len(nrow(grps))) {
      # copy back labels to grouped data frame
      tmp <- sjmisc::copy_labels(grps$data[[i]], x)
      
      # prepare argument list, including title
      tmp.args <- get_grouped_title(x, grps, args, i, sep = "\n")
      
      # plot
      plots <- plot_sj(tmp, fun, tmp.args)
      
      # add plots, check for NULL results
      if (!is.null(plots$p)) pl <- c(pl, list(plots$p))
      if (!is.null(plots$pl)) pl <- c(pl, plots$pl)
    }
  } else {
    # plot
    plots <- plot_sj(x, fun, args)
    # we only have one plot call
    p <- plots$p
    pl <- plots$pl
  }

  # print all plots
  if (!is.null(pl)) {
    for (p in pl) suppressWarnings(graphics::plot(p))
    invisible(pl)
  } else {
    suppressWarnings(graphics::plot(p))
    invisible(p)
  }
}


#' @rdname sjplot
#' @export
sjtab <- function(data, ..., fun = c("frq", "xtab", "grpmean", "stackfrq")) {
  # check if x is a data frame
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = F)
  
  # match fun-arguments
  fun <- match.arg(fun)
  
  # evaluate arguments, generate data
  x <- get_dot_data(data, match.call(expand.dots = FALSE)$`...`)
  
  # check remaining arguments
  args <- match.call(expand.dots = FALSE)$`...`
  args <- args[names(args) != ""]
  
  # do we have a grouped data frame?
  if (inherits(x, "grouped_df")) {
    # get grouped data
    grps <- get_grouped_data(x)
    
    # now plot everything
    for (i in seq_len(nrow(grps))) {
      # copy back labels to grouped data frame
      tmp <- sjmisc::copy_labels(grps$data[[i]], x)
      
      # prepare argument list, including title
      tmp.args <- get_grouped_title(x, grps, args, i, sep = "<br>")
      
      # table
      tab_sj(tmp, fun, tmp.args)
    }
  } else {
    # plot
    tab_sj(x, fun, args)
  }
}


get_grouped_title <- function(x, grps, args, i, sep = "\n") {
  # prepare title for group
  var.name <- colnames(grps)[1]
  t1 <- sjmisc::get_label(x[[var.name]], def.value = var.name)
  t2 <- sjmisc::get_labels(x[[var.name]])[grps[[var.name]][i]]
  title <- sprintf("%s: %s", t1, t2)
  
  # do we have another groupng variable?
  if (length(attr(x, "vars", exact = T)) > 1) {
    # prepare title for group
    var.name <- colnames(grps)[2]
    t1 <- sjmisc::get_label(x[[var.name]], def.value = var.name)
    t2 <- sjmisc::get_labels(x[[var.name]])[grps[[var.name]][i]]
    title <- sprintf("%s%s%s: %s", title, sep, t1, t2)
  }
  
  # add title argument to argument list
  c(args, `title` = title)
}


get_grouped_data <- function(x) {
  # nest data frame
  grps <- tidyr::nest(x)
  
  # remove NA category
  cc <- grps %>% 
    dplyr::select_("-data") %>% 
    stats::complete.cases()
  # select only complete cases
  grps <- grps %>% dplyr::filter(cc)
  
  # arrange data
  if (length(attr(x, "vars", exact = T)) == 1)
    reihe <- order(grps[[1]])
  else
    reihe <- order(grps[[1]], grps[[2]])
  grps <- grps[reihe, ]
  
  grps
}


plot_sj <- function(x, fun, args) {
  p <- NULL
  pl <- NULL
  
  # choose plottype, and call plot-function with or w/o additional arguments
  if (sjmisc::is_empty(args)) {
    if (fun == "frq") {
      pl <- list()
      for (i in seq_len(ncol(x))) {
        pl[[length(pl) + 1]] <- sjp.frq(x[[i]], prnt.plot = F)$plot
      }
    } else if (fun  == "grpfrq") {
      p <- sjp.grpfrq(x[[1]], x[[2]], prnt.plot = F)$plot
    } else if (fun  == "likert") {
      p <- sjp.likert(x, prnt.plot = F)$plot
    } else if (fun  == "stackfrq") {
      p <- sjp.stackfrq(x, prnt.plot = F)$plot
    } else if (fun  == "xtab") {
      p <- sjp.xtab(x[[1]], x[[2]], prnt.plot = F)$plot
    } else if (fun  == "gpt") {
      p <- sjp.gpt(x[[1]], x[[2]], x[[3]], prnt.plot = F)$plot
    } else if (fun  == "scatter") {
      if (ncol(x) >= 3)
        p <- sjp.scatter(x[[1]], x[[2]], x[[3]], prnt.plot = F)$plot
      else
        p <- sjp.scatter(x[[1]], x[[2]], prnt.plot = F)$plot
    } else if (fun  == "aov1") {
      p <- sjp.aov1(x[[1]], x[[2]], prnt.plot = F)$plot
    }
  } else {
    if (fun == "frq") {
      pl <- list()
      for (i in seq_len(ncol(x))) {
        pl[[length(pl) + 1]] <- do.call(sjp.frq, args = c(list(var.cnt = x[[i]], prnt.plot = F), args))$plot
      }
    } else if (fun  == "grpfrq") {
      p <- do.call(sjp.grpfrq, args = c(list(var.cnt = x[[1]], var.grp = x[[2]], prnt.plot = F), args))$plot
    } else if (fun  == "likert") {
      p <- do.call(sjp.likert, args = c(list(items = x, prnt.plot = F), args))$plot
    } else if (fun  == "stackfrq") {
      p <- do.call(sjp.stackfrq, args = c(list(items = x, prnt.plot = F), args))$plot
    } else if (fun  == "xtab") {
      p <- do.call(sjp.xtab, args = c(list(x = x[[1]], grp = x[[2]], prnt.plot = F), args))$plot
    } else if (fun  == "gpt") {
      p <- do.call(sjp.gpt, args = c(list(x = x[[1]], y = x[[2]], groups = x[[3]], prnt.plot = F), args))$plot
    } else if (fun  == "scatter") {
      if (ncol(x) >= 3)
        p <- do.call(sjp.scatter, args = c(list(x = x[[1]], y = x[[2]], grp = x[[3]], prnt.plot = F), args))$plot
      else
        p <- do.call(sjp.scatter, args = c(list(x = x[[1]], y = x[[2]], prnt.plot = F), args))$plot
    } else if (fun  == "aov1") {
      p <- do.call(sjp.aov1, args = c(list(var.dep = x[[1]], var.grp = x[[2]], prnt.plot = F), args))$plot
    }
  }
  
  list(p = p, pl = pl)
}


tab_sj <- function(x, fun, args) {
  
  # choose plottype, and call plot-function with or w/o additional arguments
  if (sjmisc::is_empty(args)) {
    if (fun == "frq") {
      sjt.frq(x)
    } else if (fun  == "xtab") {
      sjt.xtab(x[[1]], x[[2]])
    } else if (fun  == "stackfrq") {
      sjt.stackfrq(x)
    } else if (fun  == "grpmean") {
      sjt.grpmean(x[[1]], x[[2]])
    }
  } else {
    if (fun == "frq") {
      do.call(sjt.frq, args = c(list(data = x), args))
    } else if (fun  == "stackfrq") {
      do.call(sjt.stackfrq, args = c(list(items = x), args))
    } else if (fun  == "xtab") {
      do.call(sjt.xtab, args = c(list(var.row = x[[1]], var.col = x[[2]]), args))
    } else if (fun  == "grpmean") {
      do.call(sjt.grpmean, args = c(list(var.cnt = x[[1]], var.grp = x[[2]]), args))
    }
  }
}