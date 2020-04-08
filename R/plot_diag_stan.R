#' @importFrom stats update
#' @importFrom dplyr bind_rows select mutate
#' @importFrom tidyr gather
plot_diag_stan <- function(model, geom.colors, axis.lim, facets, axis.labels, ...) {

  # check some defaults
  if (missing(facets)) facets <- TRUE

  alpha <- .3
  scale <- .9

  if (inherits(model, "brmsfit")) {

    # check if brms can be loaded

    if (!requireNamespace("brms", quietly = TRUE))
      stop("Package `brms` needs to be loaded first!", call. = F)


    # get samples from posterior and prior

    d1 <- brms::posterior_samples(model)

    d1 <- dplyr::select(
      d1,
      string_starts_with("b_", colnames(d1)),
      -string_starts_with("b_Intercept", colnames(d1))
    )


    # check if prior sample are available

    d2 <- brms::prior_samples(model, pars=colnames(d1))

    if (is.null(d2))
      stop("No prior-samples found. Please use option `sample_prior = TRUE` when fitting the model.", call. = FALSE)

  } else if (inherits(model, c("stanreg", "stanfit"))) {

    # check if rstanarm can be loaded
    if (!requireNamespace("rstanarm", quietly = TRUE))
      stop("Package `rstanarm` needs to be loaded first!", call. = F)


    # get samples from posterior and prior

    prior <- suppressWarnings(
      stats::update(model, prior_PD = TRUE, refresh = -1, iter = 2000, chains = 2)
    )

    d1 <- as.data.frame(model)
    d2 <- as.data.frame(prior)


    # remove intercept from output for ridgeline plot.
    # this would increase the range of the scale too much

    if (obj_has_name(d1, "(Intercept)"))
      d1 <- dplyr::select(d1, -.data$`(Intercept)`)

    if (obj_has_name(d2, "(Intercept)"))
      d2 <- dplyr::select(d2, -.data$`(Intercept)`)

    if (obj_has_name(d1, "sigma"))
      d1 <- dplyr::select(d1, -.data$sigma)

    if (obj_has_name(d2, "sigma"))
      d2 <- dplyr::select(d2, -.data$sigma)

    d1 <- dplyr::select(d1, -string_starts_with("b[(Intercept)", colnames(d1)))
    d2 <- dplyr::select(d2, -string_starts_with("b[(Intercept)", colnames(d2)))
    d1 <- dplyr::select(d1, -string_starts_with("Sigma[", colnames(d1)))
    d2 <- dplyr::select(d2, -string_starts_with("Sigma[", colnames(d2)))
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
  if ("alpha" %in% names(add.args)) alpha <- eval(add.args[["alpha"]])
  if ("scale" %in% names(add.args)) scale <- eval(add.args[["scale"]])


  if (!facets && requireNamespace("ggridges", quietly = TRUE)) {
    p <- ggplot(pp, aes_string(y = "Term", x = "Estimate", fill = "Sample")) +
      ggridges::geom_density_ridges2(alpha = alpha, rel_min_height = .005, scale = scale) +
      scale_fill_manual(values = col_check2(geom.colors, 2))
  } else {

    p <- ggplot(pp, aes_string(x = "Estimate", fill = "Sample")) +
      geom_density(alpha = alpha) +
      scale_fill_manual(values = col_check2(geom.colors, 2))

    if (!is.null(axis.labels) && !is.null(names(axis.labels))) {
      p <- p + facet_wrap(~Term, scales = "free", labeller = labeller(.default = label_value, Term = axis.labels))
    } else {
      p <- p + facet_wrap(~Term, scales = "free")
    }
  }


  if (!is.null(axis.lim))
    p <- p + scale_x_continuous(limits = axis.lim)


  p + xlab("Distribution")
}

