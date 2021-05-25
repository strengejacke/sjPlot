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

rownames_as_column <- function(x, var = "rowname", rownames = NULL) {
  if (is.null(rownames)) {
    rn <- rownames(x)
  } else {
    rn <- rownames
  }
  rn <- data.frame(rn = rn, stringsAsFactors = FALSE)
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



#' @importFrom stats reshape
#' @keywords internal
.gather <- function(x, names_to = "key", values_to = "value", columns = colnames(x)) {
  if (is.numeric(columns)) columns <- colnames(x)[columns]
  dat <- stats::reshape(
    x,
    idvar = "id",
    ids = row.names(x),
    times = columns,
    timevar = names_to,
    v.names = values_to,
    varying = list(columns),
    direction = "long"
  )

  if (is.factor(dat[[values_to]]))
    dat[[values_to]] <- as.character(dat[[values_to]])

  dat[, 1:(ncol(dat) - 1), drop = FALSE]
}
