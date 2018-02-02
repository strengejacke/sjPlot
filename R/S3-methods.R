# display html-content in viewer pane or write it to file
#' @importFrom utils browseURL
#' @export
print.sjTable <- function(x, ...) {
  if (x$show) {
    # -------------------------------------
    # check if we have filename specified
    # -------------------------------------
    if (!is.null(x$file)) {
      # write file
      write(x$knitr, file = x$file)
    } else {
      # -------------------------------------
      # else open in viewer pane
      # -------------------------------------
      # create and browse temporary file
      htmlFile <- tempfile(fileext = ".html")
      write(x$page.complete, file = htmlFile)
      # check whether we have RStudio Viewer
      viewer <- getOption("viewer")
      if (x$viewer && !is.null(viewer)) {
        viewer(htmlFile)
      } else {
        utils::browseURL(htmlFile)
      }
      # delete temp file
      # unlink(htmlFile)
    }
  }
}


#' @importFrom knitr knit_print asis_output
#' @export
knit_print.sjTable <-  function(input, ...) {
  knitr::asis_output(input$knitr)
}


# HTMl table method for grpmean() ----

#' @export
print.sjt_grpmean <- function(x, ...) {
  title <- sprintf(
    "Mean for %s by %s",
    attr(x, "dv.label", exact = TRUE),
    attr(x, "grp.label", exact = TRUE)
  )

  footnote <- sprintf(
    "Anova: R<sup>2</sup>=%.3f; adj.R<sup>2</sup>=%.3f; F=%.3f; p=%.3f",
    attr(x, "r2", exact = TRUE),
    attr(x, "adj.r2", exact = TRUE),
    attr(x, "fstat", exact = TRUE),
    attr(x, "p.value", exact = TRUE)
  )

  tab <- tab_df(
    x = x,
    title = title,
    footnote = footnote,
    col.header = NULL,
    show.type = FALSE,
    show.rownames = FALSE,
    show.footnote = TRUE,
    alternate.rows = FALSE,
    encoding = "UTF-8",
    CSS = list(css.firsttablecol = '+text-align:left;'),
    file = NULL,
    use.viewer = attr(x, "print", exact = TRUE) == "viewer",
    ...
  )

  print(tab, ...)
}


# HTMl table method for grouped grpmean() ----

#' @importFrom purrr map_chr
#' @export
print.sjt_grpmeans <- function(x, ...) {

  uv <- attr(x, "print", exact = TRUE) == "viewer"

  titles <- purrr::map_chr(x, ~ sprintf(
      "Mean for %s by %s<br><span class=\"subtitle\">grouped by %s</span>",
      attr(.x, "dv.label", exact = TRUE),
      attr(.x, "grp.label", exact = TRUE),
      gsub(pattern = "\n", replacement = "<br>", attr(.x, "group", exact = TRUE), fixed = T)
    ))

  footnotes <- purrr::map_chr(x, ~ sprintf(
      "Anova: R<sup>2</sup>=%.3f; adj.R<sup>2</sup>=%.3f; F=%.3f; p=%.3f",
      attr(.x, "r2", exact = TRUE),
      attr(.x, "adj.r2", exact = TRUE),
      attr(.x, "fstat", exact = TRUE),
      attr(.x, "p.value", exact = TRUE)
    ))


  tabs <- tab_dfs(
    x = x,
    titles = titles,
    footnotes = footnotes,
    col.header = NULL,
    show.type = FALSE,
    show.rownames = FALSE,
    show.footnote = TRUE,
    alternate.rows = FALSE,
    encoding = "UTF-8",
    CSS = list(css.firsttablecol = '+text-align:left;'),
    file = NULL,
    use.viewer = uv,
    ...
  )

  print(tabs, ...)
}


# HTMl table method for reliab_test() ----

#' @export
print.sjt_reliab <- function(x, ...) {

  chead <- c(
    "Variable",
    "&alpha; if deleted",
    "Item Discrimination"
  )

  tab <- tab_df(
    x = x,
    title = "Reliability Test",
    footnote = NULL,
    col.header = chead,
    show.type = FALSE,
    show.rownames = FALSE,
    show.footnote = FALSE,
    alternate.rows = FALSE,
    encoding = "UTF-8",
    CSS = list(
      css.firsttablecol = '+text-align:left;',
      css.lasttablerow = 'border-bottom: 1px solid;'
    ),
    file = NULL,
    use.viewer = attr(x, "print", exact = TRUE) == "viewer",
    ...
  )

  print(tab, ...)
}


# HTMl table method for descr() ----

#' @importFrom purrr map_if
#' @importFrom tibble as_tibble
#' @importFrom sjmisc is_float
#' @export
print.sjt_descr <- function(x, ...) {

  digits <- 2

  # do we have digits argument?
  add.args <- lapply(match.call(expand.dots = F)$`...`, function(x) x)
  if ("digits" %in% names(add.args)) digits <- eval(add.args[["digits"]])

  uv <- attr(x, "print", exact = TRUE) == "viewer"

  chead <- c(
    "Variable",
    "Type",
    "Label",
    "N",
    "Missings (%)",
    "Mean",
    "SD",
    "SE",
    "Median",
    "Trimmed Mean",
    "Min",
    "Max",
    "Range",
    "Skewness",
    "Kurtosis"
  )


  x <- x %>%
    purrr::map_if(sjmisc::is_float, ~ round(.x, digits)) %>%
    tibble::as_tibble()

  tab <- tab_df(
    x = x,
    title = "Basic descriptive statistics",
    footnote = NULL,
    col.header = chead,
    show.type = FALSE,
    show.rownames = FALSE,
    show.footnote = FALSE,
    alternate.rows = TRUE,
    encoding = "UTF-8",
    CSS = list(
      css.firsttablecol = '+text-align:left;',
      css.lasttablerow = 'border-bottom: 1px solid;'
    ),
    file = NULL,
    use.viewer = uv,
    ...
  )

  print(tab, ...)
}


#' @importFrom purrr map_if
#' @importFrom tibble as_tibble
#' @importFrom sjmisc is_float
#' @export
print.sjt_grpdescr <- function(x, ...) {

  ## TODO

  digits <- 2

  # do we have digits argument?
  add.args <- lapply(match.call(expand.dots = F)$`...`, function(x) x)
  if ("digits" %in% names(add.args)) digits <- eval(add.args[["digits"]])

  uv <- attr(x, "print", exact = TRUE) == "viewer"

  chead <- c(
    "Variable",
    "Type",
    "Label",
    "N",
    "Missings (%)",
    "Mean",
    "SD",
    "SE",
    "Median",
    "Trimmed Mean",
    "Min",
    "Max",
    "Range",
    "Skewness",
    "Kurtosis"
  )


  x <- x %>%
    purrr::map_if(sjmisc::is_float, ~ round(.x, digits)) %>%
    tibble::as_tibble()

  tab <- tab_df(
    x = x,
    title = "Basic descriptive statistics",
    footnote = NULL,
    col.header = chead,
    show.type = FALSE,
    show.rownames = FALSE,
    show.footnote = FALSE,
    alternate.rows = TRUE,
    encoding = "UTF-8",
    CSS = list(
      css.firsttablecol = '+text-align:left;',
      css.lasttablerow = 'border-bottom: 1px solid;'
    ),
    file = NULL,
    use.viewer = uv,
    ...
  )

  print(tab, ...)
}
