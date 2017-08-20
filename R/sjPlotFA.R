#' @title Plot FA results
#' @name sjp.fa
#'
#' @description Performes a maximum likelihood factor analysis on a data frame or matrix
#'                and plots the factor solution as ellipses or tiles. \cr
#'                In case a data frame is used as argument, the cronbach's alpha value for
#'                each factor scale will be calculated, i.e. all variables with the highest
#'                loading for a factor are taken for the reliability test. The result is
#'                an alpha value for each factor dimension.
#'
#' @seealso \itemize{
#'            \item \href{http://www.strengejacke.de/sjPlot/sjp.pca/}{sjPlot manual: sjp.pca}
#'            \item \code{\link{sjt.pca}}
#'            }
#' @param data A data frame that should be used to compute a FA, or a \code{\link[psych]{fa}} object.
#' @param rotation Rotation of the factor loadings. May be \code{"varimax"} for orthogonal rotation
#'          or \code{"promax"} for oblique transformation (default). Requires the \code{"GPArotation"} package.
#' @param nmbr.fctr Number of factors used for calculating the rotation. By
#'          default, this value is \code{NULL} and the amount of factors is
#'          calculated according to a parallel analysis.
#' @param method the factoring method to be used. \code{"ml"} will do a maximum likelihood factor analysis (default).
#'         \code{"minres"} will do a minimum residual (OLS),
#'         \code{"wls"} will do a weighted least squares (WLS) solution,
#'         \code{"gls"} does a generalized weighted least squares (GLS),
#'         \code{"pa"} will do the principal factor solution,
#'         \code{"minchi"} will minimize the sample size weighted chi square
#'         when treating pairwise correlations with different number of
#'         subjects per pair. \code{"minrank"} will do a minimum rank factor analysis.
#'
#' @return (Invisibly) returns a \code{\link{structure}} with
#'          \itemize{
#'            \item the rotated factor loading matrix (\code{rotate})
#'            \item the column indices of removed variables (for more details see next list item) (\code{removed.colindex})
#'            \item an updated data frame containing all factors that have a clear loading on a specific scale in case \code{data} was a data frame (See argument \code{fctr.load.tlrn} for more details) (\code{removed.df})
#'            \item the \code{factor.index}, i.e. the column index of each variable with the highest factor loading for each factor,
#'            \item the ggplot-object (\code{plot}),
#'            \item the data frame that was used for setting up the ggplot-object (\code{df}).
#'            }
#'
#' @inheritParams sjp.pca
#' @inheritParams sjt.pca
#' @inheritParams sjp.grpfrq
#' @inheritParams sjp.glmer
#'
#' @note This method for factor analysis relies on the functions
#'       \code{\link[psych]{fa}} and \code{\link[psych]{fa.parallel}}
#'       from the psych package.
#'
#' @examples
#' library(sjmisc)
#' library(GPArotation)
#' data(efc)
#' # recveive first item of COPE-index scale
#' start <- which(colnames(efc) == "c82cop1")
#' # recveive last item of COPE-index scale
#' end <- which(colnames(efc) == "c90cop9")
#'
#' # use data frame as argument, let sjp.fa() compute FA
#' sjp.fa(efc[, start:end])
#' sjp.fa(efc[, start:end], type = "tile")
#'
#' @import ggplot2
#' @importFrom tidyr gather
#' @importFrom scales brewer_pal grey_pal
#' @importFrom stats na.omit prcomp
#' @importFrom sjstats cronb
#' @importFrom psych fa fa.parallel
#' @importFrom grDevices dev.off
#' @export
sjp.fa <- function(data,
                    rotation = c("promax", "varimax"),
                    method = c("ml", "minres", "wls", "gls", "pa", "minchi", "minrank"),
                    nmbr.fctr = NULL,
                    fctr.load.tlrn = 0.1,
                    digits = 2,
                    title = NULL,
                    axis.labels = NULL,
                    type = c("bar", "circle", "tile"),
                    geom.size = .6,
                    geom.colors = "RdBu",
                    wrap.title = 50,
                    wrap.labels = 30,
                    show.values = TRUE,
                    show.cronb = TRUE,
                    prnt.plot = TRUE) {
  # --------------------------------------------------------
  # check arguments
  # --------------------------------------------------------
  type <- match.arg(type)
  rotation <- match.arg(rotation)
  method <- match.arg(method)
  # --------------------------------------------------------
  # try to automatically set labels is not passed as argument
  # --------------------------------------------------------
  if (is.null(axis.labels) && is.data.frame(data)) {
    axis.labels <- unname(sjlabelled::get_label(data, def.value = colnames(data)))
  }
  # ----------------------------
  # set color palette
  # ----------------------------
  if (is.brewer.pal(geom.colors[1])) {
    geom.colors <- scales::brewer_pal(palette = geom.colors[1])(5)
  } else if (geom.colors[1] == "gs") {
    geom.colors <- scales::grey_pal()(5)
  }
  # ----------------------------
  # check if user has passed a data frame
  # or a pca object
  # ----------------------------
  if (inherits(data, "fa")) {
    fadata <- data
    dataframeparam <- FALSE
  } else if (is.data.frame(data)) {

    if (is.null(nmbr.fctr)) {
      nr_factors <- psych::fa.parallel(data, fa = 'fa', fm = method)$nfact
      grDevices::dev.off()
      fadata <- psych::fa(data, nfactors = nr_factors, fm = method, rotate = rotation)
    }
     else {

    fadata <- psych::fa(data, nfactors = nmbr.fctr, fm = method, rotate = rotation)

     }
    dataframeparam <- TRUE
  }



  # create data frame with factor loadings
  loadings <- fadata$loadings[]
  names <- rownames(fadata$loadings)


  df <- as.data.frame(loadings, row.names = names)


  # ----------------------------
  # check if user defined labels have been supplied
  # if not, use variable names from data frame
  # ----------------------------
  if (is.null(axis.labels)) axis.labels <- row.names(df)
  # ----------------------------
  # Prepare length of title and labels
  # ----------------------------
  # check length of diagram title and split longer string at into new lines
  if (!is.null(title)) title <- sjmisc::word_wrap(title, wrap.title)
  # check length of x-axis-labels and split longer strings at into new lines
  if (!is.null(axis.labels)) axis.labels <- sjmisc::word_wrap(axis.labels, wrap.labels)
  # --------------------------------------------------------
  # this function checks which items have unclear factor loadings,
  # i.e. which items do not strongly load on a single factor but
  # may load almost equally on several factors
  # --------------------------------------------------------
  getRemovableItems <- function(dataframe) {
    # clear vector
    removers <- c()
    # iterate each row of the data frame. each row represents
    # one item with its factor loadings
    for (i in seq_len(nrow(dataframe))) {
      # get factor loadings for each item
      rowval <- as.numeric(abs(df[i, ]))
      # retrieve highest loading
      maxload <- max(rowval)
      # retrieve 2. highest loading
      max2load <- sort(rowval, TRUE)[2]
      # check difference between both
      if (abs(maxload - max2load) < fctr.load.tlrn) {
        # if difference is below the tolerance,
        # remember row-ID so we can remove that items
        # for further FA with updated data frame
        removers <- c(removers, i)
      }
    }
    # return a vector with index numbers indicating which items
    # have unclear loadings
    return(removers)
  }
  # --------------------------------------------------------
  # this function retrieves a list with the column index ("factor" index)
  # where each case of the data frame has its highedt factor loading.
  # So we know to which "group" (factor dimension) each case of the
  # data frame belongs to according to the pca results
  # --------------------------------------------------------
  getItemLoadings <- function(dataframe) {
    # return a vector with index numbers indicating which items
    # loads the highest on which factor
    return(apply(dataframe, 1, function(x) which.max(abs(x))))
  }
  # --------------------------------------------------------
  # this function calculates the cronbach's alpha value for
  # each factor scale, i.e. all variables with the highest loading
  # for a factor are taken for the reliability test. The result is
  # an alpha value for each factor dimension
  # --------------------------------------------------------
  getCronbach <- function(dataframe, itemloadings) {
    # clear vector
    cbv <- c()
    # iterate all highest factor loadings of items
    for (n in seq_len(length(unique(itemloadings)))) {
      # calculate cronbach's alpha for those cases that all have the
      # highest loading on the same factor
      cbv <- as.data.frame(rbind(cbv, cbind(nr = n, sjstats::cronb(stats::na.omit(dataframe[, which(itemloadings == n)])))))
    }
    # just for vertical position adjustment when we print the alpha values
    vpos <- rep(c(-0.25, -1), nrow(cbv))
    cbv <- cbind(cbv, vpos[seq_len(nrow(cbv))])
    names(cbv) <- c("nr", "alpha", "vpos")
    # cbv now contains the factor numbers and the related alpha values
    # for each "factor dimension scale"
    return(cbv)
  }
  # ----------------------------------
  # Cronbach's Alpha can only be calculated when having a data frame
  # with each component / variable as column
  # ----------------------------------
  if (dataframeparam) {
    # get alpha values
    alphaValues <- getCronbach(data, getItemLoadings(df))
  } else {
    message("Cronbach's Alpha can only be calculated when having a data frame with each component / variable as column.")
    show.cronb <- FALSE
  }
  # -------------------------------------
  # create list with factor loadings that indicate
  # on which column inside the data frame the highest
  # loading is
  # -------------------------------------
  factorindex <- getItemLoadings(df)
  # retrieve those items that have unclear factor loadings, i.e.
  # which almost load equally on several factors. The tolerance
  # that indicates which difference between factor loadings is
  # considered as "equally" is defined via fctr.load.tlrn
  removableItems <- getRemovableItems(df)
  # rename columns, so we have numbers on x axis
  names(df) <- seq_len(ncol(df))
  # convert to long data
  df <- tidyr::gather(df, "xpos", "value", !! seq_len(ncol(df)), factor_key = TRUE)
  # we need new columns for y-positions and point sizes
  df <- cbind(df, ypos = seq_len(nrow(loadings)), psize = exp(abs(df$value)) * geom.size)
  if (!show.values) {
    valueLabels <- ""
  } else {
    valueLabels <- sprintf("%.*f", digits, df$value)
  }
  # --------------------------------------------------------
  # start with base plot object here
  # --------------------------------------------------------
  if (type == "bar") {
    heatmap <- ggplot(df, aes(x = rev(factor(ypos)), y = abs(value), fill = value))
  } else {
    heatmap <- ggplot(data = df, aes(x = xpos, y = ypos, fill = value))
  }
  # --------------------------------------------------------
  # determine the geom type, either points when "type" is "circles"
  # --------------------------------------------------------
  if (type == "circle") {
    geo <- geom_point(shape = 21, size = df$psize)
  } else if (type == "tile") {
    # ----------------------------------------
    # or boxes / tiles when "type" is "tile"
    # ----------------------------------------
    geo <- geom_tile()
  } else {
    # ----------
    # or bars
    # ----------
    geo <- geom_bar(stat = "identity", width = geom.size)
  }
  heatmap <- heatmap + geo +
    # --------------------------------------------------------
  # fill gradient colour from distinct color brewer palette.
  # negative correlations are dark red, positive corr. are dark blue,
  # and they become lighter the closer they are to a correlation
  # coefficient of zero
  # --------------------------------------------------------
  scale_fill_gradientn(colours = geom.colors, limits = c(-1, 1)) +
    labs(title = title, x = NULL, y = NULL, fill = NULL) +
    guides(fill = FALSE)
  # --------------------------------------------------------
  # facet bars, and flip coordinates
  # --------------------------------------------------------
  if (type == "bar") {
    heatmap <- heatmap +
      scale_x_discrete(labels = rev(axis.labels)) +
      scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, .2)) +
      facet_grid(~xpos) +
      geom_text(label = valueLabels, hjust = -0.2) +
      coord_flip()
  } else {
    heatmap <- heatmap +
      geom_text(label = valueLabels) +
      scale_y_reverse(breaks = seq(1, length(axis.labels), by = 1),
                      labels = axis.labels)
    # --------------------------------------------------------
    # show cronbach's alpha value for each scale
    # --------------------------------------------------------
    if (show.cronb) {
      heatmap <- heatmap +
        annotate("text", x = alphaValues$nr, y = Inf, parse = TRUE,
                 label = sprintf("alpha == %.*f", digits, alphaValues$alpha),
                 vjust = -0.5)
    }
  }
  # --------------------------------------------------------
  # print plot
  # --------------------------------------------------------
  if (prnt.plot) graphics::plot(heatmap)
  # --------------------------------------------------------
  # if we have a data frame, all factors which do not clearly
  # load on a specific dimension (see patameter "fctr.load.tlrn")
  # will be removed and the updated data frame will be returned.
  # the user may calculate another FA with the updated data frame
  # in order to get more clearly factor loadings
  # --------------------------------------------------------
  remdf <- NULL
  if (any(class(data) == "data.frame")) {
    message("Following items have no clear factor loading:")
    if (!is.null(removableItems)) {
      message(colnames(data)[removableItems])
      remdf <- data[, -removableItems]
    } else {
      message("none.")
    }
  }
  # --------------------------------------------------------
  # return structure with various results
  # --------------------------------------------------------
  invisible(structure(class = "sjpfa",
                      list(rotation = rotation,
                           removed.colindex = removableItems,
                           removed.df = remdf,
                           factor.index = factorindex,
                           plot = heatmap,
                           df = df)))
}
