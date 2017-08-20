# bind global variables
utils::globalVariables(c("xpos", "value", "Var2", "grp", "prc", "fg", "cprc", "se", "group", "var", "kmeans"))


#' @title Compute quick cluster analysis
#' @name sjc.qclus
#' @description Compute a quick kmeans or hierarchical cluster analysis and displays "cluster characteristics"
#'                as plot.
#'
#' @references Maechler M, Rousseeuw P, Struyf A, Hubert M, Hornik K (2014) cluster: Cluster Analysis Basics and Extensions. R package.
#'
#' @param data A data frame with variables that should be used for the
#'          cluster analysis.
#' @param groupcount Amount of groups (clusters) used for the cluster solution. May also be
#'          a set of initial (distinct) cluster centres, in case \code{method = "kmeans"}
#'          (see \code{\link{kmeans}} for details on \code{centers} argument).
#'          If \code{groupcount = NULL} and \code{method = "kmeans"}, the optimal
#'          amount of clusters is calculated using the gap statistics (see
#'          \code{\link{sjc.kgap}}). For \code{method = "hclust"}, \code{groupcount}
#'          needs to be specified. Following functions may be helpful for estimating
#'          the amount of clusters:
#'          \itemize{
#'            \item Use \code{\link{sjc.elbow}} to determine the group-count depending on the elbow-criterion.
#'            \item If \code{method = "kmeans"}, use \code{\link{sjc.kgap}} to determine the group-count according to the gap-statistic.
#'            \item If \code{method = "hclust"} (hierarchical clustering, default), use \code{\link{sjc.dend}} to inspect different cluster group solutions.
#'            \item Use \code{\link{sjc.grpdisc}} to inspect the goodness of grouping (accuracy of classification).
#'            }
#' @param groups Optional, by default, this argument is \code{NULL} and will be
#'          ignored. However, to plot existing cluster groups, specify \code{groupcount}
#'          and \code{groups}. \code{groups} is a vector of same length as
#'          \code{nrow(data)} and indicates the group classification of the cluster
#'          analysis. The group classification can be computed with the
#'          \code{\link{sjc.cluster}} function. See 'Examples'.
#' @param method Method for computing the cluster analysis. By default (\code{"kmeans"}), a
#'          kmeans cluster analysis will be computed. Use \code{"hclust"} to
#'          compute a hierarchical cluster analysis. You can specify the
#'          initial letters only.
#' @param distance Distance measure to be used when \code{method = "hclust"} (for hierarchical
#'          clustering). Must be one of \code{"euclidean"}, \code{"maximum"}, \code{"manhattan"},
#'          \code{"canberra"}, \code{"binary"} or \code{"minkowski"}. See \code{\link{dist}}.
#'          If is \code{method = "kmeans"} this argument will be ignored.
#' @param agglomeration Agglomeration method to be used when \code{method = "hclust"} (for hierarchical
#'          clustering). This should be one of \code{"ward"}, \code{"single"}, \code{"complete"}, \code{"average"},
#'          \code{"mcquitty"}, \code{"median"} or \code{"centroid"}. Default is \code{"ward"} (see \code{\link{hclust}}).
#'          If \code{method = "kmeans"} this argument will be ignored. See 'Note'.
#' @param iter.max Maximum number of iterations allowed. Only applies, if
#'          \code{method = "kmeans"}. See \code{\link{kmeans}} for details on this argument.
#' @param algorithm Algorithm used for calculating kmeans cluster. Only applies, if
#'          \code{method = "kmeans"}. May be one of \code{"Hartigan-Wong"} (default),
#'          \code{"Lloyd"} (used by SPSS), or \code{"MacQueen"}. See \code{\link{kmeans}}
#'          for details on this argument.
#' @param show.accuracy Logical, if \code{TRUE}, the \code{\link{sjc.grpdisc}} function will be called,
#'          which computes a linear discriminant analysis on the classified cluster groups and plots a
#'          bar graph indicating the goodness of classification for each group.
#' @param show.grpcnt Logical, if \code{TRUE} (default), the count within each cluster group is added to the
#'          legend labels (e.g. \code{"Group 1 (n=87)"}).
#' @param reverse.axis Logical, if \code{TRUE}, the values on the x-axis are reversed.
#'
#' @inheritParams sjp.grpfrq
#'
#' @return (Invisibly) returns an object with
#'           \itemize{
#'            \item \code{data}: the used data frame for plotting,
#'            \item \code{plot}: the ggplot object,
#'            \item \code{groupcount}: the number of found cluster (as calculated by \code{\link{sjc.kgap}})
#'            \item \code{classification}: the group classification (as calculated by \code{\link{sjc.cluster}}), including missing values, so this vector can be appended to the original data frame.
#'            \item \code{accuracy}: the accuracy of group classification (as calculated by \code{\link{sjc.grpdisc}}).
#'           }
#'
#' @details Following steps are computed in this function:
#'          \enumerate{
#'            \item If \code{method = "kmeans"}, this function first determines the optimal group count via gap statistics (unless argument \code{groupcount} is specified), using the \code{\link{sjc.kgap}} function.
#'            \item A cluster analysis is performed by running the \code{\link{sjc.cluster}} function to determine the cluster groups.
#'            \item Then, all variables in \code{data} are scaled and centered. The mean value of these z-scores within each cluster group is calculated to see how certain characteristics (variables) in a cluster group differ in relation to other cluster groups.
#'            \item These results are plotted as graph.
#'          }
#'          This method can also be used to plot existing cluster solution as graph witouth computing
#'          a new cluster analysis. See argument \code{groups} for more details.
#'
#' @note See 'Note' in \code{\link{sjc.cluster}}
#'
#' @examples
#' \dontrun{
#' # k-means clustering of mtcars-dataset
#' sjc.qclus(mtcars)
#'
#' # k-means clustering of mtcars-dataset with 4 pre-defined
#' # groups in a faceted panel
#' sjc.qclus(airquality, groupcount = 4, facet.grid = TRUE)}
#'
#' # k-means clustering of airquality data
#' # and saving the results. most likely, 3 cluster
#' # groups have been found (see below).
#' airgrp <- sjc.qclus(airquality)
#'
#' # "re-plot" cluster groups, without computing
#' # new k-means cluster analysis.
#' sjc.qclus(airquality, groupcount = 3, groups = airgrp$classification)
#'
#' @import ggplot2
#' @importFrom stats na.omit
#' @importFrom graphics plot
#' @importFrom sjmisc std
#' @export
sjc.qclus <- function(data,
                      groupcount = NULL,
                      groups = NULL,
                      method = c("kmeans", "hclust"),
                      distance = c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski"),
                      agglomeration = c("ward", "ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid"),
                      iter.max = 20,
                      algorithm = c("Hartigan-Wong", "Lloyd", "MacQueen"),
                      show.accuracy = FALSE,
                      title = NULL,
                      axis.labels = NULL,
                      wrap.title = 40,
                      wrap.labels = 20,
                      wrap.legend.title = 20,
                      wrap.legend.labels = 20,
                      facet.grid = FALSE,
                      geom.colors = "Paired",
                      geom.size = 0.5,
                      geom.spacing = 0.1,
                      show.legend = TRUE,
                      show.grpcnt = TRUE,
                      legend.title = NULL,
                      legend.labels = NULL,
                      coord.flip = FALSE,
                      reverse.axis = FALSE,
                      prnt.plot = TRUE) {
  # --------------------------------------------------------
  # match arguments
  # --------------------------------------------------------
  distance <- match.arg(distance)
  method <- match.arg(method)
  agglomeration <- match.arg(agglomeration)
  algorithm <- match.arg(algorithm)
  # --------------------------------------------------------
  # save original data frame
  # --------------------------------------------------------
  rownames(data) <- seq_len(nrow(data))
  data.origin <- data
  # remove missings
  data <- stats::na.omit(data)
  # check for valid argument
  if (is.null(axis.labels)) axis.labels <- colnames(data)
  # --------------------------------------------------------
  # Trim labels and title to appropriate size
  # --------------------------------------------------------
  # check length of diagram title and split longer string at into new lines
  if (!is.null(title)) title <- sjmisc::word_wrap(title, wrap.title)
  # check length of legend title and split longer string at into new lines
  if (!is.null(legend.title)) legend.title <- sjmisc::word_wrap(title, wrap.legend.title)
  # check length of y-axis title and split longer string at into new lines
  if (!is.null(legend.labels)) legend.labels <- sjmisc::word_wrap(legend.labels, wrap.legend.labels)
  # check length of x-axis-labels and split longer strings at into new lines
  # every 10 chars, so labels don't overlap
  axis.labels <- sjmisc::word_wrap(axis.labels, wrap.labels)
  # ---------------------------------------------
  # check for auto-groupcount
  # ---------------------------------------------
  if (is.null(groupcount)) {
    # ------------------------
    # check if suggested package is available
    # ------------------------
    if (!requireNamespace("cluster", quietly = TRUE)) {
      stop("Package `cluster` needed for this function to work. Please install it.", call. = FALSE)
    }
    # check whether method is kmeans. hierarchical clustering
    # requires a specified groupcount
    if (method != "kmeans") {
      message("Cannot compute hierarchical cluster analysis when `groupcount` is NULL. Using kmeans clustering instead.")
      method <- "kmeans"
    }
    # retrieve optimal group count via gap statistics
    kgap <- sjc.kgap(data, plotResults = F)
    # save group counts
    groupcount <- kgap$solution
  }
  # ---------------------------------------------
  # run cluster analysis with claculated group count
  # ---------------------------------------------
  if (is.null(groups)) {
    # check for argument and R version
    if (agglomeration == "ward") agglomeration <- "ward.D2"
    grp.class <- grp <- sjc.cluster(data.origin, groupcount, method, distance, agglomeration, iter.max, algorithm)
  } else {
    grp.class <- grp <- groups
  }
  # remove missings
  grp <- stats::na.omit(grp)
  # ---------------------------------------------
  # check whether groupcount was matrix or not
  # ---------------------------------------------
  if (is.matrix(groupcount)) groupcount <- length(unique(grp))
  # ---------------------------------------------
  # auto-set legend labels
  # ---------------------------------------------
  if (is.null(legend.labels)) legend.labels <- sprintf("Group %i", seq_len(groupcount))
  # --------------------------------------------------------
  # show goodness of classification
  # --------------------------------------------------------
  grp.accuracy <- sjc.grpdisc(data,
                              groups = grp,
                              groupcount = groupcount,
                              prnt.plot = show.accuracy)
  # ---------------------------------------------
  # Add group count to legend labels
  # ---------------------------------------------
  if (show.grpcnt || show.accuracy) {
    # iterate legend labels
    for (i in seq_len(length(legend.labels))) {
      # label string for group count
      gcnt.label <- sprintf("n=%i", length(which(grp == i)))
      # label string for accuracy
      acc.label <- sprintf("accuracy=%.2f%%", 100 * grp.accuracy$accuracy[i])
      # prepare legend label
      legend.labels[i] <- paste0(legend.labels[i], " (")
      # add group count to each label
      if (show.grpcnt) legend.labels[i] <- paste0(legend.labels[i], gcnt.label)
      if (show.grpcnt && show.accuracy) legend.labels[i] <- paste0(legend.labels[i], ", ")
      if (show.accuracy) legend.labels[i] <- paste0(legend.labels[i], acc.label)
      legend.labels[i] <- paste0(legend.labels[i], ")")
    }
  }
  # scale data
  z <- sjmisc::std(data)
  # retrieve column count
  colnr <- ncol(data)
  # init data frame
  df <- data.frame()
  # ---------------------------------------------
  # iterate all columns to calculate group means
  # ---------------------------------------------
  for (cnt in seq_len(colnr)) {
    # retrieve column data
    coldat <- z[[cnt]]
    # ---------------------------------------------
    # iterate groups
    # ---------------------------------------------
    for (i in seq_len(groupcount)) {
      # retrieve column values for each group
      grpvalues <- coldat[which(grp == i)]
      # calculate mean
      mw <- mean(grpvalues, na.rm = TRUE)
      df <- rbind(df, cbind(x = cnt, y = mw, group = i))
    }
    # df %.% group_by(grp) %.% summarise(mean(a))
  }
  # --------------------------------------------------------
  # factor for discrete scale
  # --------------------------------------------------------
  df$group <- as.factor(df$group)
  # --------------------------------------------------------
  # create plot
  # --------------------------------------------------------
  if (reverse.axis) {
    gp <- ggplot(df, aes(x = rev(x), y = y, fill = group))
    axis.labels <- rev(axis.labels)
  } else {
    gp <- ggplot(df, aes_string(x = "x", y = "y", fill = "group"))
  }
  gp <- gp +
    geom_bar(stat = "identity",
             position = position_dodge(geom.size + geom.spacing),
             width = geom.size) +
    scale_x_discrete(breaks = seq_len(colnr),
                     limits = seq_len(colnr),
                     labels = axis.labels) +
    labs(title = title, x = "Cluster group characteristics", y = "Mean of z-scores", fill = legend.title)
  # --------------------------------------------------------
  # check whether coordinates should be flipped, i.e.
  # swap x and y axis
  # --------------------------------------------------------
  if (coord.flip) gp <- gp + coord_flip()
  # --------------------------------------------------------
  # use facets
  # --------------------------------------------------------
  if (facet.grid) gp <- gp + facet_wrap(~group)
  # ---------------------------------------------------------
  # set geom colors
  # ---------------------------------------------------------
  gp <- sj.setGeomColors(gp,
                         geom.colors,
                         length(legend.labels),
                         show.legend,
                         legend.labels)
  # --------------------------------------------------------
  # plot
  # --------------------------------------------------------
  if (prnt.plot) graphics::plot(gp)
  # --------------------------------------------------------
  # return values
  # --------------------------------------------------------
  invisible(structure(class = "sjcqclus",
                      list(data = df,
                           groupcount = groupcount,
                           classification = grp.class,
                           accuracy = grp.accuracy$accuracy,
                           plot = gp)))
}


#' @title Compute hierarchical or kmeans cluster analysis
#' @name sjc.cluster
#' @description Compute hierarchical or kmeans cluster analysis and return the group
#'                association for each observation as vector.
#'
#' @references Maechler M, Rousseeuw P, Struyf A, Hubert M, Hornik K (2014) cluster: Cluster Analysis Basics and Extensions. R package.
#'
#' @inheritParams sjc.qclus
#'
#' @return The group classification for each observation as vector. This group
#'           classification can be used for \code{\link{sjc.grpdisc}}-function to
#'           check the goodness of classification.
#'           The returned vector includes missing values, so it can be appended
#'           to the original data frame \code{data}.
#'
#' @note Since R version > 3.0.3, the \code{"ward"} option has been replaced by
#'        either \code{"ward.D"} or \code{"ward.D2"}, so you may use one of
#'        these values. When using \code{"ward"}, it will be replaced by \code{"ward.D2"}.
#'        \cr \cr
#'        To get similar results as in SPSS Quick Cluster function, following points
#'        have to be considered:
#'        \enumerate{
#'          \item Use the \code{/PRINT INITIAL} option for SPSS Quick Cluster to get a table with initial cluster centers.
#'          \item Create a \code{\link{matrix}} of this table, by consecutively copying the values, one row after another, from the SPSS output into a matrix and specify \code{nrow} and \code{ncol} arguments.
#'          \item Use \code{algorithm="Lloyd"}.
#'          \item Use the same amount of \code{iter.max} both in SPSS and this \code{sjc.qclus}.
#'        }
#'        This ensures a fixed initial set of cluster centers (as in SPSS), while \code{\link{kmeans}} in R
#'        always selects initial cluster sets randomly.
#'
#' @examples
#' # Hierarchical clustering of mtcars-dataset
#' groups <- sjc.cluster(mtcars, 5)
#'
#' # K-means clustering of mtcars-dataset
#' groups <- sjc.cluster(mtcars, 5, method="k")
#'
#' @import ggplot2
#' @importFrom stats dist na.omit hclust kmeans cutree
#' @export
sjc.cluster <- function(data,
                        groupcount = NULL,
                        method = c("hclust", "kmeans"),
                        distance = c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski"),
                        agglomeration = c("ward", "ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid"),
                        iter.max = 20,
                        algorithm = c("Hartigan-Wong", "Lloyd", "MacQueen")) {
  # --------------------------------------------------------
  # match arguments
  # --------------------------------------------------------
  distance <- match.arg(distance)
  method <- match.arg(method)
  agglomeration <- match.arg(agglomeration)
  algorithm <- match.arg(algorithm)
  # --------------------------------------------------------
  # save original data frame
  # --------------------------------------------------------
  data.origin <- data
  # create id with index numbers for rows
  data.origin$sj.grp.id <- seq_len(nrow(data.origin))
  # create NA-vector of same length as data frame
  complete.groups <- rep(NA, times = nrow(data.origin))
  # Prepare Data
  # listwise deletion of missing
  data <- stats::na.omit(data)
  # remove missings
  data.origin <- stats::na.omit(data.origin)
  # ---------------------------------------------
  # check for auto-groupcount
  # ---------------------------------------------
  if (is.null(groupcount)) {
    # ------------------------
    # check if suggested package is available
    # ------------------------
    if (!requireNamespace("cluster", quietly = TRUE)) {
      stop("Package `cluster` needed for this function to work. Please install it.", call. = FALSE)
    }
    # check whether method is kmeans. hierarchical clustering
    # requires a specified groupcount
    if (method != "kmeans") {
      message("Cannot compute hierarchical cluster analysis when `groupcount` is NULL. Using kmeans clustering instead.")
      method <- "kmeans"
    }
    # retrieve optimal group count via gap statistics
    kgap <- sjc.kgap(data, plotResults = F)
    # save group counts
    groupcount <- kgap$solution
  }
  # --------------------------------------------------
  # Ward Hierarchical Clustering
  # --------------------------------------------------
  if (method == "hclust") {
    # check for argument and R version
    if (agglomeration == "ward") agglomeration <- "ward.D2"
    # distance matrix
    d <- stats::dist(data, method = distance)
    # hierarchical clustering, using ward
    hc <- stats::hclust(d, method = agglomeration)
    # cut tree into x clusters
    groups <- stats::cutree(hc, k = groupcount)
  } else {
    km <- stats::kmeans(data, centers = groupcount, iter.max = iter.max, algorithm = algorithm)
    # return cluster assignment
    groups <- km$cluster
  }
  # -----------------------------------
  # create vector with cluster group classification,
  # including missings
  # -----------------------------------
  # assign valid group values
  complete.groups[data.origin$sj.grp.id] <- groups
  # return group assignment
  return(complete.groups)
}


#' @title Compute hierarchical cluster analysis and visualize group classification
#' @name sjc.dend
#' @description Computes a hierarchical cluster analysis and plots a hierarchical
#'                dendrogram with highlighted rectangles around the classified groups.
#'                Can be used, for instance, as visual tool to verify the elbow-criterion
#'                (see \code{\link{sjc.elbow}}).
#'
#' @param groupcount The amount of groups (clusters) that should be used.
#'          \itemize{
#'            \item Use \code{\link{sjc.elbow}}-function to determine the group-count depending on the elbow-criterion.
#'            \item Use \code{\link{sjc.grpdisc}}-function to inspect the goodness of grouping (accuracy of classification).
#'          }
#'          Solutions for multiple cluster groups can be plotted, for instance with \code{"groupcount = c(3:6)"}.
#'
#' @inheritParams sjc.qclus
#'
#' @note Since R version > 3.0.3, the \code{"ward"} option has
#'          been replaced by either \code{"ward.D"} or \code{"ward.D2"},
#'          so you may use one of these values. When using \code{"ward"},
#'          it will be replaced by \code{"ward.D2"}.
#'
#' @examples
#' # Plot dendrogram of hierarchical clustering of mtcars-dataset
#' # and show group classification
#' sjc.dend(mtcars, 5)
#'
#' # Plot dendrogram of hierarchical clustering of mtcars-dataset
#' # and show group classification for 2 to 4 groups
#' sjc.dend(mtcars, 2:4)
#'
#' @importFrom stats dist hclust cutree na.omit rect.hclust
#' @importFrom scales brewer_pal
#' @importFrom graphics plot rect par
#' @export
sjc.dend <- function(data, groupcount, distance = "euclidean", agglomeration = "ward") {
  # Prepare Data
  # listwise deletion of missing
  data <- stats::na.omit(data)
  # --------------------------------------------------
  # Ward Hierarchical Clustering
  # --------------------------------------------------
  # distance matrix
  d <- stats::dist(data, method = distance)
  # check for argument and R version
  if (agglomeration == "ward") agglomeration <- "ward.D2"
  # hierarchical clustering, using ward
  hc <- stats::hclust(d, method = agglomeration)
  # display simple dendrogram
  graphics::plot(hc,
                 main = "Cluster Dendrogramm",
                 xlab = sprintf("Hierarchical Cluster Analysis, %s-Method",
                                agglomeration))
  # now plot overlaying rectangles, depending on the amount of groupcounts
  gl <- length(groupcount)
  if (gl > 1) {
    # retrieve different colors
    color <- scales::brewer_pal("qual", "Set1")(gl)
    # iterate all groupcounts
    for (cnt in seq_len(gl)) {
      k <- groupcount[cnt]
      # retrieve cluster
      cluster <- stats::cutree(hc, k = k)
      # create table with cluster groups
      clustab <- table(cluster)[unique(cluster[hc$order])]
      m <- c(0, cumsum(clustab))
      which <- 1L:k
      # draw dendrogram with red borders around the clusters
      # source code taken from "rect.hclust" from base-package
      for (n in seq_along(which)) {
        graphics::rect(m[which[n]] + 0.46 + (cnt * 0.2),
                       graphics::par("usr")[3L],
                       m[which[n] + 1] + 0.53 - (cnt * 0.2),
                       mean(rev(hc$height)[(k - 1):k]),
                       border = color[cnt],
                       lwd = 2)
      }
    }
  } else {
    # draw dendrogram with red borders around the clusters
    stats::rect.hclust(hc, k = groupcount, border = "red")
  }
}


#' @title Compute a linear discriminant analysis on classified cluster groups
#' @name sjc.grpdisc
#' @description Computes linear discriminant analysis on classified cluster groups.
#'                This function plots a bar graph indicating the goodness of classification
#'                for each group.
#'
#' @param groups group classification of the cluster analysis that was returned
#'          from the \code{\link{sjc.cluster}}-function
#' @param groupcount amount of groups (clusters) that should be used. Use
#'          \code{\link{sjc.elbow}} to determine the group-count depending
#'          on the elbow-criterion.
#' @param clss.fit logical, if \code{TRUE} (default), a vertical line indicating the
#'          overall goodness of classification is added to the plot, so one can see
#'          whether a certain group is below or above the average classification goodness.
#'
#' @inheritParams sjc.cluster
#' @inheritParams sjp.grpfrq
#'
#' @return (Invisibly) returns an object with
#'           \itemize{
#'            \item \code{data}: the used data frame for plotting,
#'            \item \code{plot}: the ggplot object,
#'            \item \code{accuracy}: a vector with the accuracy of classification for each group,
#'            \item \code{total.accuracy}: the total accuracy of group classification.
#'           }
#'
#' @examples
#' # retrieve group classification from hierarchical cluster analysis
#' # on the mtcars data set (5 groups)
#' groups <- sjc.cluster(mtcars, 5)
#'
#' # plot goodness of group classificatoin
#' sjc.grpdisc(mtcars, groups, 5)
#'
#' @importFrom stats na.omit
#' @importFrom graphics plot
#' @importFrom MASS lda
#' @import ggplot2
#' @export
sjc.grpdisc <- function(data, groups, groupcount, clss.fit = TRUE, prnt.plot = TRUE) {
  # Prepare Data
  # listwise deletion of missing
  data <- stats::na.omit(data)
  groups <- stats::na.omit(groups)
  # ---------------------------------------------------------------
  # compute discriminant analysis of groups on original data frame
  # ---------------------------------------------------------------
  disc <- MASS::lda(groups ~ ., data = data, na.action = "na.omit", CV = TRUE)
  # ---------------------------------------------------------------
  # Assess the accuracy of the prediction
  # percent correct for each category of groups
  # ---------------------------------------------------------------
  ct <- table(groups, disc$class)
  dg <- diag(prop.table(ct, 1))
  # ---------------------------------------------------------------
  # print barplot for correct percentage for each category of groups
  # ---------------------------------------------------------------
  perc <- round(100 * dg, 2)
  percrest <- round(100 - perc, 2)
  # total correct percentage
  totalcorrect <- sum(diag(prop.table(ct)))
  # round total percentages and transform to percent value
  totalcorrect <- round(100 * totalcorrect, 2)
  # create three data columns for data frame which is
  # needed to plot the barchart with ggplot
  newdat <- NULL
  tmpdat <- NULL
  filldat <- NULL
  labeldat <- NULL
  # data frame has flexible count of rows, depending on
  # the amount of groups in the lda
  for (i in seq_len(groupcount)) {
    # first columns indicates the two parts of each group
    # (correct percentage and remaining percentage untill 100%)
    newdat <- rbind(newdat, paste("g", i, sep = ""))
    newdat <- rbind(newdat, paste("g", i, sep = ""))
    # second columns contains the percentage of lda
    # followed by the remaining percentage to 100%
    tmpdat <- rbind(tmpdat, perc[i])
    tmpdat <- rbind(tmpdat, percrest[i])
    # third columns indicates both which data row contains
    # the lda-percentage and which one the remaining percentage
    filldat <- rbind(filldat, "1")
    filldat <- rbind(filldat, "2")
    # last column is created for printing the label-values
    # we need only on percentage value, otherwise double labels are
    # printed
    labeldat <- rbind(labeldat, perc[i])
    labeldat <- rbind(labeldat, 0)
  }
  # create data frame
  mydat <- data.frame(filldat, newdat, tmpdat, labeldat)
  # name columns
  names(mydat) <- c("fg", "grp", "prc", "cprc")
  # fillgroup-indicator ($fg) needs to be a factor
  mydat$fg <- factor(mydat$fg)
  # plot bar charts, stacked proportional
  # this works, because we always have two "values" (variables)
  # for the X-axis in the $grp-columns indicating a group
  classplot <- ggplot(mydat, aes_string(x = "grp", y = "prc", fill = "fg")) +
    # use stat identity to show value, not count of $prc-variable
    # draw no legend!
    geom_bar(stat = "identity", colour = "black", show.legend = FALSE) +
    # fill bars
    scale_fill_manual(values = c("#235a80", "#80acc8")) +
    # give chart and X-axis a title
    labs(title = "Accuracy of cluster group classification (in %)",
         x = "cluster groups",
         y = NULL) +
    # print value labels into bar chart
    geom_text(aes_string(label = "cprc", y = "cprc"),
              vjust = 1.2,
              colour = "white") +
    # larger font size for axes
    theme(axis.line = element_line(colour = "gray"),
          axis.text = element_text(size = rel(1.2)),
          axis.title = element_text(size = rel(1.2))) +
    # set ticks
    scale_y_continuous(breaks = seq(0, 100, 10)) +
    # change range on x-axis, so the text annotation is visible and
    # beside the bars and not printed into them
    coord_cartesian(ylim = c(0, 100),
                    xlim = c(-0.5, groupcount + 1))
  if (clss.fit) {
    classplot <- classplot +
    # set line across all bars which indicates the total percentage of
    # correct assigned cases
    geom_hline(yintercept = totalcorrect,
               linetype = 2,
               colour = "#333333") +
      # print text annotation
      annotate("text",
               x = 0,
               y = totalcorrect,
               vjust = 1.2,
               label = paste("overall", c(totalcorrect), sep = "\n"),
               size = 5,
               colour = "#333333")
  }
  # --------------------------------------------------------
  # plot
  # --------------------------------------------------------
  if (prnt.plot) graphics::plot(classplot)
  # --------------------------------------------------------
  # return values
  # --------------------------------------------------------
  invisible(structure(class = "sjcgrpdisc",
                      list(data = mydat,
                           accuracy = as.vector(dg),
                           total.accuracy = totalcorrect,
                           plot = classplot)))
}


#' @title Compute elbow values of a k-means cluster analysis
#' @name sjc.elbow
#' @description Plot elbow values of a k-means cluster analysis. This function
#'                computes a k-means cluster analysis on the provided data frame
#'                and produces two plots: one with the different elbow values
#'                and a second plot that maps the differences between each
#'                "step" (i.e. between elbow values) on the y-axis. An
#'                increase in the second plot may indicate the elbow criterion.
#'
#' @param data data frame containing all variables that should be used for
#'          determining the elbow criteria
#' @param steps maximum group-count for the k-means cluster analysis for
#'          which the elbow-criterion should be displayed. Default is \code{15}.
#' @param show.diff logical, if \code{TRUE}, an additional plot with the differences between
#'          each fusion step of the Elbow criterion calculation is shown. This plot
#'          may help identifying the "elbow". Default for this argument is \code{FALSE}.
#'
#' @examples
#' # plot elbow values of mtcars dataset
#' sjc.elbow(mtcars)
#'
#' @import ggplot2
#' @importFrom tidyr gather
#' @importFrom grDevices rgb
#' @importFrom stats na.omit
#' @importFrom graphics plot
#' @importFrom rlang .data
#' @export
sjc.elbow <- function(data, steps = 15, show.diff = FALSE) {
  # Prepare Data
  # listwise deletion of missing
  data <- stats::na.omit(data)
  # define line linecolor
  lcol <- grDevices::rgb(128, 172, 200, maxColorValue = 255)
  # calculate elbow values (sum of squares)
  wss <- (nrow(data) - 1) * sum(apply(data, 2, var))
  for (i in 2:steps) wss[i] <- sum(kmeans(data, centers = i)$withinss)
  # round and print elbow values
  wssround <- round(wss, 0)
  dfElbowValues <- as.data.frame(wssround)
  dfElbowValues <- cbind(dfElbowValues, xpos = seq_len(nrow(dfElbowValues)))
  # calculate differences between each step
  diff <- c()
  for (i in 2:steps) diff <- cbind(diff,wssround[i - 1] - wssround[i])
  dfElbowDiff <- tidyr::gather(as.data.frame(diff),
                               "Var2",
                               "value",
                               !! seq_len(ncol(diff)),
                               factor_key = TRUE)
  # --------------------------------------------------
  # Plot diagram with sum of squares
  # all pointes are connected with a line
  # a bend the in curve progression might indicate elbow
  # --------------------------------------------------
  graphics::plot(ggplot(dfElbowValues, aes_string(x = "xpos", y = "wssround", label = "wssround")) +
                   geom_line(colour = lcol) +
                   geom_point(colour = lcol, size = 3) +
                   geom_text(hjust = -0.3) +
                   labs(title = "Elbow criterion (sum of squares)",
                        x = "Number of clusters",
                        y = "elbow value"))
  # --------------------------------------------------
  # Plot diagram with differences between each step
  # increasing y-value on x-axis (compared to former y-values)
  # might indicate elbow
  # --------------------------------------------------
  if (show.diff) {
    graphics::plot(ggplot(dfElbowDiff, aes_string(x = "Var2", y = "value", label = "value")) +
                     geom_line(colour = lcol) +
                     geom_point(colour = lcol, size = 3) +
                     geom_text(hjust = -0.3) +
                     labs(title = "Elbow criterion (differences between sum of squares)",
                          x = "difference to previews cluster",
                          y = "delta"))
  }
}


#' @title Compute gap statistics for k-means-cluster
#' @name sjc.kgap
#' @description An implementation of the gap statistic algorithm from Tibshirani, Walther, and Hastie's
#'                "Estimating the number of clusters in a data set via the gap statistic".
#'                This function calls the \code{\link[cluster]{clusGap}}-function of the
#'                \pkg{cluster}-package to calculate the data for the plot.
#'
#' @seealso \code{\link{sjc.elbow}}
#'
#' @param x matrix, where rows are observations and columns are individual dimensions,
#'          to compute and plot the gap statistic (according to a uniform reference distribution).
#' @param max maximum number of clusters to consider, must be at least two. Default
#'          is 10.
#' @param B integer, number of Monte Carlo ("bootstrap") samples. Default is 100.
#' @param SE.factor [When \code{method} contains "SE"] Determining the optimal
#'          number of clusters, Tibshirani et al. proposed the "1 S.E."-rule.
#'          Using an SE.factor f, the "f S.E."-rule is used, more generally.
#' @param method character string indicating how the "optimal" number of clusters,
#'          k^, is computed from the gap statistics (and their standard deviations),
#'          or more generally how the location k^ of the maximum of f[k] should be
#'          determined. Default is \code{"Tibs2001SEmax"}. Possible value are:
#'          \describe{
#'            \item{\code{"globalmax"}}{simply corresponds to the global maximum, i.e., is which.max(f).}
#'            \item{\code{"firstmax"}}{gives the location of the first local maximum.}
#'            \item{\code{"Tibs2001SEmax"}}{uses the criterion, Tibshirani et al(2001) proposed: "the smallest k such that f(k) >= f(k+1) - s_{k+1}". Note that this chooses k = 1 when all standard deviations are larger than the differences f(k+1) - f(k).}
#'            \item{\code{"firstSEmax"}}{is the location of the first f() value which is not larger than the first local maximum minus SE.factor * SE.f[], i.e, within an "f S.E." range of that maximum (see also SE.factor).}
#'            \item{\code{"globalSEmax"}}{(used in Dudoit and Fridlyand (2002), supposedly following Tibshirani's proposition) is the location of the first f() value which is not larger than the global maximum minus SE.factor * SE.f[], i.e, within an "f S.E." range of that maximum (see also SE.factor).}
#'            }
#' @param plotResults logical, if \code{TRUE} (default), a graph visualiting the gap statistic will
#'          be plotted. Use \code{FALSE} to omit the plot.
#'
#' @return An object containing the used data frame for plotting, the ggplot object
#'           and the number of found cluster.
#'
#' @references \itemize{
#'              \item Tibshirani R, Walther G, Hastie T (2001) Estimating the number of clusters in a data set via gap statistic. J. R. Statist. Soc. B, 63, Part 2, pp. 411-423
#'              \item Maechler, M., Rousseeuw, P., Struyf, A., Hubert, M., Hornik, K.(2013). cluster: Cluster Analysis Basics and Extensions. R package version 1.14.4. (\href{https://cran.r-project.org/package=cluster}{web})
#'             }
#'
#' @examples
#' \dontrun{
#' # plot gap statistic and determine best number of clusters
#' # in mtcars dataset
#' sjc.kgap(mtcars)
#'
#' # and in iris dataset
#' sjc.kgap(iris[,1:4])}
#'
#' @import ggplot2
#' @importFrom stats na.omit
#' @importFrom graphics plot
#' @export
sjc.kgap <- function(x,
                     max = 10,
                     B = 100,
                     SE.factor = 1,
                     method = "Tibs2001SEmax",
                     plotResults = TRUE) {
  # ------------------------
  # check if suggested package is available
  # ------------------------
  if (!requireNamespace("cluster", quietly = TRUE)) {
    stop("Package 'cluster' needed for this function to work. Please install it.", call. = FALSE)
  }
  # Prepare Data
  # listwise deletion of missing
  x <- stats::na.omit(x)
  # Gap Statistic for Estimating the Number of Clusters
  gap <- cluster::clusGap(x, kmeans, max, B)

  stopifnot((K <- nrow(T <- gap$Tab)) >= 1, SE.factor >= 0)
  message("Clustering Gap statistic [\"clusGap\"].\n",
          sprintf("B=%d simulated reference sets, k = 1..%d\n", gap$B, K),
          sep = "")
  # Gap Statistic for Estimating the Number of Clusters
  nc <- cluster::maxSE(f = T[, "gap"],
                       SE.f = T[, "SE.sim"],
                       method = method,
                       SE.factor = SE.factor)
  message(sprintf(" --> Number of clusters (method '%s'%s): %d\n",
                  method,
                  if (grepl("SE", method, fixed = T)) sprintf(", SE.factor=%g", SE.factor) else "",
                  nc))
  # point size for cluster solution
  nclus <- rep(2, max)
  nclus[nc] <- 4
  # coliur  for cluster solution
  cclus <- rep("black", max)
  cclus[nc] <- "#cc3366"
  # create data frame
  df <- data.frame(x = seq_len(max),
                   y = gap$Tab[, 'gap'],
                   se = gap$Tab[, 'SE.sim'],
                   psize = nclus,
                   pcol = cclus)
  # plot cluster solution
  gp <- ggplot(df, aes_string(x = "x", y = "y")) +
    geom_errorbar(aes(ymin = y - se, ymax = y + se),
                  width = 0,
                  size = 0.5,
                  colour = "#3366cc") +
    geom_line(colour = "gray50") +
    geom_point(colour = df$pcol, size = df$psize) +
    scale_x_discrete(breaks = seq_len(nrow(df))) +
    labs(x = "Number of clusters",
         y = "Gap",
         title = sprintf("Estimation of clusters (gap statistics)\n%i-cluster solution found", nc)) +
    theme_classic()
  if (plotResults) graphics::plot(gp)
  # return value
  invisible(structure(class = "sjckgap",
                      list(data = df,
                           plot = gp,
                           solution = nc)))
}
