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
      write(x$output.complete, file = htmlFile)
      # check whether we have RStudio Viewer
      viewer <- getOption("viewer")
      if (x$use.viewer && !is.null(viewer)) {
        viewer(htmlFile)
      } else {
        utils::browseURL(htmlFile)
      }
      # delete temp file
      # unlink(htmlFile)
    }
  }
}

#' @importFrom knitr asis_output
#' @export
knit_print.sjTable <-  function(input, ...) {

 if (!requireNamespace("knitr", quietly = TRUE)) {
   stop("Package `knitr` needed to print tables inside knitr-documents. Please install it.", call. = F)

 }
  knitr::asis_output(input$knitr)
}
