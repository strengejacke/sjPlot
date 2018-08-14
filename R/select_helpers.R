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
  rn <- rownames(x)
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

#' @importFrom dplyr select bind_cols
add_cols <- function(data, ..., .after = 1) {
  c1 <- 1:.after
  c2 <- (.after + 1):ncol(data)

  x1 <- dplyr::select(data, !! c1)
  x2 <- dplyr::select(data, !! c2)

  dat <- data.frame(...)
  dplyr::bind_cols(x1, dat, x2)
}
