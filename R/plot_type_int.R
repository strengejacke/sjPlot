plot_type_int <- function(model) {
  # find right hand side of formula, to extract interaction terms
  rhs <- labels(stats::terms(model))

  # interaction terms are separated with ":"
  int.terms <- rhs[sjmisc::str_contains(":", rhs, switch = TRUE)]

  # split at colon. we than have all interaction terms as
  # list, each list-element indicating an interaction
  int.terms <- strsplit(int.terms, ":")

  p
}
