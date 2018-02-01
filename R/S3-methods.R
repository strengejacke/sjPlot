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


#' @export
print.sjt_grpmean <- function(x, ...) {
  title <- sprintf(
    "Grouped Means for %s by %s",
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
    show.type = FALSE,
    show.rownames = FALSE,
    show.footnote = TRUE,
    alternate.rows = FALSE,
    footnote = footnote,
    encoding = "UTF-8",
    CSS = list(css.firsttablecol = '+text-align:left;'),
    file = NULL,
    use.viewer = attr(x, "print", exact = TRUE) == "viewer",
    ...
  )

  print(tab, ...)
}


#' @importFrom purrr map
#' @export
print.sjt_grpmeans <- function(x, ...) {

  uv <- attr(x, "print", exact = TRUE) == "viewer"

  x <- purrr::map(x, function(dat) {

    title <- sprintf(
      "Grouped Means for %s by %s<br>grouped by %s",
      attr(dat, "dv.label", exact = TRUE),
      attr(dat, "grp.label", exact = TRUE),
      attr(dat, "group", exact = T)
    )

    footnote <- sprintf(
      "Anova: R<sup>2</sup>=%.3f; adj.R<sup>2</sup>=%.3f; F=%.3f; p=%.3f",
      attr(dat, "r2", exact = TRUE),
      attr(dat, "adj.r2", exact = TRUE),
      attr(dat, "fstat", exact = TRUE),
      attr(dat, "p.value", exact = TRUE)
    )

    attr(dat, "title") <- title
    attr(dat, "footnote") <- footnote

    dat
  })

  tabs <- tab_dfs(
    x = x,
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
