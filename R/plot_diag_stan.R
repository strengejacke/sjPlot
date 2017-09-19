#' @importFrom stats update
#' @importFrom dplyr bind_rows
#' @importFrom tidyr gather
plot_diag_stan <- function(model, geom.colors, facets, ...) {

  # check if rstanarm can be loaded
  if (!requireNamespace("rstanarm", quietly = TRUE))
    stop("Package `rstanarm` needs to be loaded first!", call. = F)

  # check some defaults
  if (missing(facets)) facets <- TRUE

  alpha <- .3
  scale <- .9


  # get samples from posterior and prior

  prior <- suppressWarnings(
    stats::update(model, prior_PD = TRUE, refresh = -1, iter = 2000, chains = 2)
  )

  d1 <- as.data.frame(model)
  d2 <- as.data.frame(prior)


  # remove intercept from output for ridgeline plot.
  # this would increase the range of the scale too much

  if (!facets) {
    d1 <- dplyr::select(d1, -.data$`(Intercept)`)
    d2 <- dplyr::select(d2, -.data$`(Intercept)`)
  }


  # grouping variable

  d1$Sample <- "Posterior"
  d2$Sample <- "Prior"

  gather.cols <- 1:(ncol(d1) - 1)

  # join data frames and convert to long format

  pp <- dplyr::bind_rows(d1, d2) %>%
    tidyr::gather(key = "Term", value = "Estimate", !! gather.cols)


  # additional arguments?
  add.args <- lapply(match.call(expand.dots = F)$`...`, function(x) x)
  if ("alpha" %in% names(add.args)) alpha <- add.args[["alpha"]]
  if ("scale" %in% names(add.args)) scale <- add.args[["scale"]]


  if (!facets && requireNamespace("ggridges", quietly = TRUE)) {
    p <- ggplot(pp, aes_string(y = "Term", x = "Estimate", fill = "Sample")) +
      ggridges::geom_density_ridges2(alpha = alpha, rel_min_height = .005, scale = scale) +
      scale_fill_manual(values = col_check2(geom.colors, 2))
  } else {
    p <- ggplot(pp, aes_string(x = "Estimate", fill = "Sample")) +
      geom_density(alpha = alpha) +
      facet_wrap(~Term, scales = "free") +
      scale_fill_manual(values = col_check2(geom.colors, 2))
  }


  p + xlab("Distribution")
}
