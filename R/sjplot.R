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
#'             \item{\code{"grpfrq"}}{calls \code{\link{plot_grpfrq}}. The first
#'             two variables in \code{data} are used (and required) to create the plot.
#'             }
#'             \item{\code{"likert"}}{calls \code{\link{plot_likert}}. \code{data}
#'             must be a data frame with items to plot.
#'             }
#'             \item{\code{"stackfrq"}}{calls \code{\link{tab_stackfrq}}.
#'             \code{data} must be a data frame with items to create the table.
#'             }
#'             \item{\code{"xtab"}}{calls \code{\link{plot_xtab}} or \code{\link{tab_xtab}}.
#'             The first two variables in \code{data} are used (and required)
#'             to create the plot or table.
#'             }
#'          }
#'
#' @examples
#' library(dplyr)
#' data(efc)
#'
#' # Grouped frequencies
#' efc %>% sjplot(e42dep, c172code, fun = "grpfrq")
#'
#' # Grouped frequencies, as box plots
#' efc %>% sjplot(e17age, c172code, fun = "grpfrq",
#'                type = "box", geom.colors = "Set1")
#'
#' \dontrun{
#' # table output of grouped data frame
#' efc %>%
#'   group_by(e16sex, c172code) %>%
#'   select(e42dep, n4pstu, e16sex, c172code) %>%
#'   sjtab(fun = "xtab", use.viewer = FALSE) # open all tables in browser}
#'
#' @importFrom sjmisc is_empty
#' @importFrom sjlabelled copy_labels get_label get_labels
#' @importFrom dplyr filter
#' @importFrom tidyr nest
#' @importFrom stats complete.cases
#' @export
sjplot <- function(data, ..., fun = c("grpfrq", "xtab", "aov1", "likert")) {
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
      tmp <- sjlabelled::copy_labels(grps$data[[i]], x)

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
sjtab <- function(data, ..., fun = c("xtab", "stackfrq")) {
  # check if x is a data frame
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = F)

  # match fun-arguments
  fun <- match.arg(fun)

  # evaluate arguments, generate data
  x <- get_dot_data(data, match.call(expand.dots = FALSE)$`...`)

  tabs.list <- list()

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
      tmp <- sjlabelled::copy_labels(grps$data[[i]], x)

      # prepare argument list, including title
      tmp.args <- get_grouped_title(x, grps, args, i, sep = "<br>")

      # table
      tl <- tab_sj(tmp, fun, tmp.args)

      # save list
      tabs.list[[length(tabs.list) + 1]] <- tl
    }

    final.table <- paste0(
      tl$header,
      tl$page.style,
      "\n</head>\n<body>\n"
    )

    final.knitr <- ""

    # iterate table list
    for (i in seq_len(length(tabs.list))) {
      final.table <- paste0(final.table, tabs.list[[i]]$page.content, sep = "\n<p>&nbsp;</p>\n")
      final.knitr <- paste0(final.knitr, tabs.list[[i]]$knitr, sep = "\n<p>&nbsp;</p>\n")
    }

    # close html tags
    final.table <- paste0(final.table, "\n</body>\n</html>")

    # return all tables
    return(structure(
      class = c("sjTable", "sjtab"),
      list(
        page.style = tl$page.style,
        header = tl$header,
        page.content = final.table,
        page.complete = final.table,
        knitr = final.knitr,
        file = eval(args[["file"]]),
        viewer = if (is.null(args[["use.viewer"]])) TRUE else eval(args[["use.viewer"]])
      )
    ))
  } else {
    # plot
    tab_sj(x, fun, args)
  }
}


get_grouped_plottitle <- function(x, grps, i, sep = "\n") {
  # prepare title for group
  tp <- get_title_part(x, grps, 1, i)
  title <- sprintf("%s: %s", tp[1], tp[2])

  # do we have another groupng variable?
  if (length(dplyr::group_vars(x)) > 1) {
    # prepare title for group
    tp <- get_title_part(x, grps, 2, i)
    title <- sprintf("%s%s%s: %s", title, sep, tp[1], tp[2])
  }

  title
}


get_grouped_title <- function(x, grps, args, i, sep = "\n") {
  # prepare title for group
  tp <- get_title_part(x, grps, 1, i)
  title <- sprintf("%s: %s", tp[1], tp[2])

  # do we have another groupng variable?
  if (length(dplyr::group_vars(x)) > 1) {
    # prepare title for group
    tp <- get_title_part(x, grps, 2, i)
    title <- sprintf("%s%s%s: %s", title, sep, tp[1], tp[2])
  }

  # add title argument to argument list
  c(args, `title` = title)
}


#' @importFrom sjlabelled get_values get_label get_labels
get_title_part <- function(x, grps, level, i) {
  # prepare title for group
  var.name <- colnames(grps)[level]

  # get values from value labels
  vals <- sjlabelled::get_values(x[[var.name]])
  # if we have no value labels, get values directly
  if (is.null(vals)) vals <- unique(x[[var.name]])
  # find position of value labels for current group
  lab.pos <- which(vals == grps[[var.name]][i])

  # get variable and value labels
  t1 <- sjlabelled::get_label(x[[var.name]], def.value = var.name)
  t2 <- sjlabelled::get_labels(x[[var.name]])[lab.pos]

  # if we have no value label, use value instead
  if (is.null(t2)) t2 <- vals[lab.pos]

  # generate title
  c(t1, t2)
}


#' @importFrom rlang .data
#' @importFrom dplyr select filter group_modify group_vars
#' @importFrom stats complete.cases
#'
get_grouped_data <- function(x) {
  # retain observations that are complete wrt grouping vars, then nest
  grps <- x %>%
    dplyr::group_modify(~ dplyr::filter(.x, stats::complete.cases(.y))) %>%
    tidyr::nest()

  # arrange data
  if (length(dplyr::group_vars(x)) == 1)
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
    if (fun  == "grpfrq") {
      p <- plot_grpfrq(x[[1]], x[[2]])
    } else if (fun  == "likert") {
      p <- plot_likert(x)
    } else if (fun  == "xtab") {
      p <- plot_xtab(x[[1]], x[[2]])
    } else if (fun  == "aov1") {
      p <- sjp.aov1(x[[1]], x[[2]])
    }
  } else {
    if (fun  == "grpfrq") {
      p <- do.call(plot_grpfrq, args = c(list(var.cnt = x[[1]], var.grp = x[[2]]), args))
    } else if (fun  == "likert") {
      p <- do.call(plot_likert, args = c(list(items = x), args))
    } else if (fun  == "xtab") {
      p <- do.call(plot_xtab, args = c(list(x = x[[1]], grp = x[[2]]), args))
    } else if (fun  == "aov1") {
      p <- do.call(sjp.aov1, args = c(list(var.dep = x[[1]], var.grp = x[[2]]), args))
    }
  }

  list(p = p, pl = pl)
}


tab_sj <- function(x, fun, args) {

  # choose plottype, and call plot-function with or w/o additional arguments
  if (sjmisc::is_empty(args)) {
    if (fun  == "xtab") {
      tab_xtab(x[[1]], x[[2]])
    } else if (fun  == "stackfrq") {
      tab_stackfrq(x)
    }
  } else {
    if (fun  == "stackfrq") {
      do.call(tab_stackfrq, args = c(list(items = x), args))
    } else if (fun  == "xtab") {
      do.call(tab_xtab, args = c(list(var.row = x[[1]], var.col = x[[2]]), args))
    }
  }
}
