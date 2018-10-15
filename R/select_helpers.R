string_starts_with <- function(pattern, x) {
  pattern <- paste0("^\\Q", pattern, "\\E")
  grep(pattern, x, perl = TRUE)
}

string_contains <- function(pattern, x) {
  pattern <- paste0("\\Q", pattern, "\\E")
  grep(pattern, x, perl = TRUE)
}

string_ends_with <- function(pattern, x) {
  pattern <- paste0("\\Q", pattern, "\\E$")
  grep(pattern, x, perl = TRUE)
}

#' @importFrom purrr map
string_one_of <- function(pattern, x) {
  m <- unlist(purrr::map(pattern, ~ grep(., x, fixed = TRUE, useBytes = TRUE)))
  x[m]
}

rownames_as_column <- function(x, var = "rowname") {
  rn <- data.frame(rn = rownames(x), stringsAsFactors = FALSE)
  x <- cbind(rn, x)
  colnames(x)[1] <- var
  rownames(x) <- NULL
  x
}

obj_has_name <- function(x, name) {
  name %in% names(x)
}

obj_has_rownames <- function(x) {
  !identical(as.character(1:nrow(x)), rownames(x))
}

#' @importFrom dplyr select
add_cases <- function(data, ..., .after = -1, .before = NULL) {

  dat <- data.frame(..., stringsAsFactors = FALSE)
  x <- rbind(data, NA)
  last.row <- nrow(x)

  if (!is.null(.before))
    .after <- .before - 1

  for (.x in colnames(dat)) {
    x[last.row, .x] <- dat[[.x]]
  }

  if (.after < 1)
    o <- c(last.row, 1:(last.row - 1))
  else if (is.infinite(.after))
    o <- 1:last.row
  else
    o <- c(1:.after, last.row, (.after + 1):(last.row - 1))

  x[o, , drop = FALSE]
}
