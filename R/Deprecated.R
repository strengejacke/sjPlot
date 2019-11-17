#' @title Deprecated functions
#' @name sjc.qclus
#' @description A list of deprecated functions.
#'
#' @param x An object.
#' @param ... Currently not used.
#'
#' @return Nothing.
#'
#' @export
sjc.qclus <- function(x, ...) {
  .Deprecated("parameters::cluster_analysis()")
}

#' @rdname sjc.qclus
#' @export
sjc.cluster <- function(x, ...) {
  .Deprecated("parameters::cluster_analysis()")
}

#' @rdname sjc.qclus
#' @export
sjc.grpdisc <- function(x, ...) {
  .Deprecated("parameters::cluster_discrimination()")
}

#' @rdname sjc.qclus
#' @export
sjc.elbow <- function(x, ...) {
  .Deprecated("parameters::n_clusters()")
}

#' @rdname sjc.qclus
#' @export
sjc.kgap <- function(x, ...) {
  .Deprecated("parameters::n_clusters()")
}
