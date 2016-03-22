# bind global variables
utils::globalVariables(c("xpos", "value", "Var2", "grp", "prc", "fg", "cprc", "se", "group", "var", "kmeans"))


#' @title Compute quick cluster analysis
#' @name sjc.qclus
#' @description Compute a quick kmeans or hierarchical cluster analysis and displays "cluster characteristics"
#'                as graph.
#'                \enumerate{
#'                \item If \code{method = "kmeans"}, this function first determines the optimal group count via gap statistics (unless argument \code{groupcount} is specified), using the \code{\link{sjc.kgap}} function.
#'                \item A cluster analysis is performed by running the \code{\link{sjc.cluster}} function to determine the cluster groups.
#'                \item Then, all variables in \code{data} are scaled and centered. The mean value of these z-scores within each cluster group is calculated to see how certain characteristics (variables) in a cluster group differ in relation to other cluster groups.
#'                \item These results are plotted as graph.
#'                }
#'                This method can also be used to plot existing cluster solution as graph witouth computing
#'                a new cluster analysis. See argument \code{groups} for more details.
#'                
#' @references Maechler M, Rousseeuw P, Struyf A, Hubert M, Hornik K (2014) cluster: Cluster Analysis Basics and Extensions. R package.
#' 
#' @param data data frame containing all variables that should be used for the
#'          cluster analysis.
#' @param groupcount amount of groups (clusters) that should be retrieved. May also be
#'          a set of initial (distinct) cluster centres, in case \code{method = "kmeans"}
#'          (see \code{\link{kmeans}} for details on \code{centers} argument). 
#'          If \code{groupcount = NULL}, the optimal amount of clusters is calculated using the 
#'          gap statistics (see \code{\link{sjc.kgap}}. However, this works only 
#'          with \code{method = "kmeans"}. If \code{method = "hclust"}, you have 
#'          to specify \code{groupcount}. Following functions 
#'          may be helpful for estimating the amount of clusters:
#'          \itemize{
#'            \item Use \code{\link{sjc.elbow}} to determine the group-count depending on the elbow-criterion.
#'            \item If \code{method = "kmeans"}, use \code{\link{sjc.kgap}} to determine the group-count according to the gap-statistic.
#'            \item If \code{method = "hclust"} (hierarchical clustering, default), use \code{\link{sjc.dend}} to inspect different cluster group solutions.
#'            \item Use \code{\link{sjc.grpdisc}} to inspect the goodness of grouping (accuracy of classification).
#'            }
#' @param groups optional, by default, this argument is \code{NULL} and will be 
#'          ignored. However, to plot existing cluster groups, specify \code{groupcount}
#'          and \code{groups}. \code{groups} is a vector of same length as 
#'          \code{nrow(data)} and indicates the group classification of the cluster 
#'          analysis. The group classification can be computed with the
#'          \code{\link{sjc.cluster}} function. See 'Examples'.
#' @param method method for computing the cluster analysis. By default (\code{"kmeans"}), a
#'          kmeans cluster analysis will be computed. Use \code{"hclust"} to 
#'          compute a hierarchical cluster analysis. You can specify the 
#'          initial letters only.
#' @param distance distance measure to be used when \code{method = "hclust"} (for hierarchical
#'          clustering). Must be one of \code{"euclidean"}, \code{"maximum"}, \code{"manhattan"}, 
#'          \code{"canberra"}, \code{"binary"} or \code{"minkowski"}. See \code{\link{dist}}.
#'          If is \code{method = "kmeans"} this argument will be ignored.
#' @param agglomeration agglomeration method to be used when \code{method = "hclust"} (for hierarchical
#'          clustering). This should be one of \code{"ward"}, \code{"single"}, \code{"complete"}, \code{"average"}, 
#'          \code{"mcquitty"}, \code{"median"} or \code{"centroid"}. Default is \code{"ward"} (see \code{\link{hclust}}).
#'          If \code{method = "kmeans"} this argument will be ignored. See 'Note'.
#' @param iter.max maximum number of iterations allowed. Only applies, if 
#'          \code{method = "kmeans"}. See \code{\link{kmeans}} for details on this argument.
#' @param algorithm algorithm used for calculating kmeans cluster. Only applies, if 
#'          \code{method = "kmeans"}. May be one of \code{"Hartigan-Wong"} (default), 
#'          \code{"Lloyd"} (used by SPSS), or \code{"MacQueen"}. See \code{\link{kmeans}} 
#'          for details on this argument.
#' @param showAccuracy logical, if \code{TRUE}, the \code{\link{sjc.grpdisc}} function will be called,
#'          which computes a linear discriminant analysis on the classified cluster groups and plots a 
#'          bar graph indicating the goodness of classification for each group.
#' @param axisTitle.x title for the x-axis.
#' @param axisTitle.y title for the y-axis.
#' @param breakTitleAt Determines how many chars of the title are displayed in 
#'          one line and when a line break is inserted into the title.
#' @param facetCluster If \code{TRUE}, each cluster group will be represented by an own panel.
#'          Default is \code{FALSE}, thus all cluster groups are plotted in a single graph.
#' @param geom.colors user defined color for bars. See 'Note' in \code{\link{sjp.grpfrq}}.
#' @param geom.size width of bars. Recommended values for this argument are from 0.4 to 1.5
#' @param showAxisLabels.x whether x axis labels (cluster variables) should be shown or not.
#' @param showAxisLabels.y whether y axis labels (z scores) should be shown or not.
#' @param showGroupCount if \code{TRUE} (default), the count within each cluster group is added to the 
#'          legend labels (e.g. \code{"Group 1 (n=87)"}).
#' @param showAccuracyLabels if \code{TRUE}, the accuracy-values for each cluster group is added to the 
#'          legend labels (e.g. \code{"Group 1 (n=87, accuracy=95.3)"}). Accuracy is calculated by \code{\link{sjc.grpdisc}}.
#' @param legendLabels labels for the guide/legend. If \code{legendLabels = NULL}
#'          (default), the standard string \code{"Group <nr>"} will be used.
#' @param reverseAxis.x if \code{TRUE}, the values on the x-axis are reversed.
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
#' @note See 'Note' in \code{\link{sjc.cluster}}
#' 
#' @examples
#' \dontrun{
#' # k-means clustering of mtcars-dataset
#' sjc.qclus(mtcars)
#' 
#' # k-means clustering of mtcars-dataset with 4 pre-defined
#' # groups in a faceted panel
#' sjc.qclus(airquality, 
#'           groupcount = 4, 
#'           facetCluster = TRUE)}
#'           
#' # k-means clustering of airquality data
#' # and saving the results. most likely, 3 cluster
#' # groups have been found (see below).
#' airgrp <- sjc.qclus(airquality)
#' 
#' # "re-plot" cluster groups, without computing
#' # new k-means cluster analysis.
#' sjc.qclus(airquality,
#'           groupcount = 3,
#'           groups = airgrp$classification)
#' 
#' @import ggplot2
#' @import sjmisc
#' @importFrom stats na.omit
#' @importFrom graphics plot
#' @export
sjc.qclus <- function(data,
                      groupcount = NULL,
                      groups = NULL,
                      method = "k",
                      distance = "euclidean",
                      agglomeration = "ward",
                      iter.max = 20,
                      algorithm = "Hartigan-Wong",
                      showAccuracy = FALSE,
                      title = NULL,
                      axisLabels.x = NULL,
                      axisTitle.x = "Cluster group characteristics",
                      axisTitle.y = "Mean of z-scores",
                      breakTitleAt = 40,
                      breakLabelsAt = 20,
                      breakLegendTitleAt = 20,
                      breakLegendLabelsAt = 20,
                      facetCluster = FALSE,
                      geom.colors = "Paired",
                      geom.size = 0.5,
                      geom.spacing = 0.1,
                      hideLegend = FALSE,
                      showAxisLabels.x = TRUE,
                      showAxisLabels.y = TRUE,
                      showGroupCount = TRUE,
                      showAccuracyLabels = FALSE,
                      legendTitle = NULL,
                      legendLabels = NULL,
                      coord.flip = FALSE,
                      reverseAxis.x = FALSE,
                      printPlot = TRUE) {
  # --------------------------------------------------------
  # check for abbreviations
  # --------------------------------------------------------
  if (method == "kmeans") method <- "k"
  if (method == "hclust") method <- "h"
  # --------------------------------------------------------
  # save original data frame
  # --------------------------------------------------------
  rownames(data) <- c(1:nrow(data))
  data.origin <- data
  # remove missings
  data <- stats::na.omit(data)
  # check for valid argument
  if (is.null(axisLabels.x)) axisLabels.x <- colnames(data)
  # --------------------------------------------------------
  # Trim labels and title to appropriate size
  # --------------------------------------------------------
  # check length of diagram title and split longer string at into new lines
  if (!is.null(title)) title <- sjmisc::word_wrap(title, breakTitleAt)    
  # check length of x-axis title and split longer string at into new lines
  if (!is.null(axisTitle.x)) axisTitle.x <- sjmisc::word_wrap(axisTitle.x, breakTitleAt)    
  # check length of y-axis title and split longer string at into new lines
  if (!is.null(axisTitle.y)) axisTitle.y <- sjmisc::word_wrap(axisTitle.y, breakTitleAt)    
  # check length of legend title and split longer string at into new lines
  if (!is.null(legendTitle)) legendTitle <- sjmisc::word_wrap(title, breakLegendTitleAt)    
  # check length of y-axis title and split longer string at into new lines
  if (!is.null(legendLabels)) legendLabels <- sjmisc::word_wrap(legendLabels, breakLegendLabelsAt)
  # check length of x-axis-labels and split longer strings at into new lines
  # every 10 chars, so labels don't overlap
  axisLabels.x <- sjmisc::word_wrap(axisLabels.x, breakLabelsAt)
  # ---------------------------------------------
  # check for auto-groupcount
  # ---------------------------------------------
  if (is.null(groupcount)) {
    # ------------------------
    # check if suggested package is available
    # ------------------------
    if (!requireNamespace("cluster", quietly = TRUE)) {
      stop("Package 'cluster' needed for this function to work. Please install it.", call. = FALSE)
    }
    # check whether method is kmeans. hierarchical clustering
    # requires a specified groupcount
    if (method != "k") {
      message("Cannot compute hierarchical cluster analysis when 'groupcount' is NULL. Using kmeans clustering instead.")
      method <- "k"
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
  if (is.null(legendLabels)) legendLabels <- sprintf("Group %i", c(1:groupcount))
  # --------------------------------------------------------
  # show goodness of classification
  # --------------------------------------------------------
  grp.accuracy <- sjc.grpdisc(data, 
                              groups = grp, 
                              groupcount = groupcount, 
                              printPlot = showAccuracy)
  # ---------------------------------------------
  # Add group count to legend labels
  # ---------------------------------------------
  if (showGroupCount || showAccuracyLabels) {
    # iterate legend labels
    for (i in 1:length(legendLabels)) {
      # label string for group count
      gcnt.label <- sprintf("n=%i", length(which(grp == i)))
      # label string for accuracy
      acc.label <- sprintf("accuracy=%.2f%%", 100 * grp.accuracy$accuracy[i])
      # prepare legend label
      legendLabels[i] <- paste0(legendLabels[i], " (")
      # add group count to each label
      if (showGroupCount) legendLabels[i] <- paste0(legendLabels[i], gcnt.label)
      if (showGroupCount && showAccuracyLabels) legendLabels[i] <- paste0(legendLabels[i], ", ")
      if (showAccuracyLabels) legendLabels[i] <- paste0(legendLabels[i], acc.label)
      legendLabels[i] <- paste0(legendLabels[i], ")")
    }
  }
  # scale data
  z <- scale(data)
  # retrieve column count
  colnr <- ncol(data)
  # init data frame
  df <- data.frame()
  # ---------------------------------------------
  # iterate all columns to calculate group means
  # ---------------------------------------------
  for (cnt in 1:colnr) {
    # retrieve column data
    coldat <- z[, cnt]
    # ---------------------------------------------
    # iterate groups
    # ---------------------------------------------
    for (i in 1:groupcount) {
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
  # Hide or show Axis Labels (x axis text) 
  # --------------------------------------------------------
  if (!showAxisLabels.x) axisLabels.x <- NULL
  # --------------------------------------------------------
  # create plot
  # --------------------------------------------------------
  if (reverseAxis.x) {
    gp <- ggplot(df, aes(x = rev(x), y = y, fill = group))
    axisLabels.x <- rev(axisLabels.x)
  } else {
    gp <- ggplot(df, aes(x = x, y = y, fill = group))
  }
  gp <- gp +
    geom_bar(stat = "identity", 
             position = position_dodge(geom.size + geom.spacing), 
             width = geom.size) +
    scale_x_discrete(breaks = c(1:colnr), 
                     limits = c(1:colnr), 
                     labels = axisLabels.x) +
    labs(title = title, x = axisTitle.x, y = axisTitle.y, fill = legendTitle)
  # --------------------------------------------------------
  # hide y-axis labels
  # --------------------------------------------------------
  if (!showAxisLabels.y) gp <- gp + scale_y_continuous(labels = NULL)    
  # --------------------------------------------------------
  # check whether coordinates should be flipped, i.e.
  # swap x and y axis
  # --------------------------------------------------------
  if (coord.flip) gp <- gp + coord_flip()
  # --------------------------------------------------------
  # use facets
  # --------------------------------------------------------
  if (facetCluster) gp <- gp + facet_wrap(~group)
  # ---------------------------------------------------------
  # set geom colors
  # ---------------------------------------------------------
  gp <- sj.setGeomColors(gp, 
                         geom.colors, 
                         length(legendLabels), 
                         ifelse(isTRUE(hideLegend), FALSE, TRUE), 
                         legendLabels)  
  # --------------------------------------------------------
  # plot
  # --------------------------------------------------------
  if (printPlot) graphics::plot(gp)
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
#' @param method Indicates the clustering method. If \code{"hclust"} (default), a hierachical 
#'          clustering using the ward method is computed. Use \code{"kmeans"} to compute a k-means clustering.
#'          You can specifiy inital letters only.
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
#'          \item Create a \code{\link{matrix}} of this table, by consecutively copying the values, one row after another, from the SPSS output into a matrix and specifying \code{nrow} and \code{ncol} arguments.
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
                        method = "h",
                        distance = "euclidean",
                        agglomeration = "ward",
                        iter.max = 20,
                        algorithm = "Hartigan-Wong") {
  # --------------------------------------------------------
  # check for abbreviations
  # --------------------------------------------------------
  if (method == "kmeans") method <- "k"
  if (method == "hclust") method <- "h"
  # --------------------------------------------------------
  # save original data frame
  # --------------------------------------------------------
  data.origin <- data
  # create id with index numbers for rows
  data.origin$sj.grp.id <- c(1:nrow(data.origin))
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
      stop("Package 'cluster' needed for this function to work. Please install it.", call. = FALSE)
    }
    # check whether method is kmeans. hierarchical clustering
    # requires a specified groupcount
    if (method != "k") {
      message("Cannot compute hierarchical cluster analysis when 'groupcount' is NULL. Using kmeans clustering instead.")
      method <- "k"
    }
    # retrieve optimal group count via gap statistics
    kgap <- sjc.kgap(data, plotResults = F)
    # save group counts
    groupcount <- kgap$solution
  }
  # --------------------------------------------------
  # Ward Hierarchical Clustering
  # --------------------------------------------------
  if (method == "h") {
    # check for argument and R version
    if (agglomeration == "ward") agglomeration <- "ward.D2"
    # distance matrix
    d <- stats::dist(data, method = distance)
    # hierarchical clustering, using ward
    hc <- stats::hclust(d, method = agglomeration) 
    # cut tree into x clusters
    groups <- stats::cutree(hc, k = groupcount)
  } else {
    km <- stats::kmeans(data, 
                        centers = groupcount, 
                        iter.max = iter.max, 
                        algorithm = algorithm)
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
#' @param data data frame containing all variables that should be used for the
#'          cluster analysis.
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
    for (cnt in 1:gl) {
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
#' @param data data frame containing all variables that should be used for the
#'          check for goodness of classification of a cluster analysis
#' @param groups group classification of the cluster analysis that was returned
#'          from the \code{\link{sjc.cluster}}-function
#' @param groupcount amount of groups (clusters) that should be used. Use
#'          \code{\link{sjc.elbow}} to determine the group-count depending
#'          on the elbow-criterion.
#' @param showTotalCorrect logical, if \code{TRUE} (default), a vertical line indicating the
#'          overall goodness of classification is added to the plot, so one can see
#'          whether a certain group is below or above the average classification goodness.
#' @param printPlot logical, if \code{TRUE} (default), plots the results as graph. Use \code{FALSE} if you don't
#'          want to plot any graphs. In either case, the ggplot-object will be returned as value.
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
#' @importFrom MASS lda
#' @importFrom stats na.omit
#' @importFrom graphics plot
#' @import ggplot2
#' @export
sjc.grpdisc <- function(data, groups, groupcount, showTotalCorrect = TRUE, printPlot = TRUE) {
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
  for (i in 1:groupcount) {
    # first columns indicates the two parts of each group
    # (correct percentage and remaining percentage untill 100%)
    newdat <- rbind(newdat, c(paste("g", i, sep = "")))
    newdat <- rbind(newdat, c(paste("g", i, sep = "")))
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
  classplot <- ggplot(mydat, aes(x = grp, y = prc, fill = fg)) +
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
    geom_text(aes(label = cprc, y = cprc), 
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
  if (showTotalCorrect) {
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
  if (printPlot) graphics::plot(classplot)
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
#' @param showDiff Deprecated; use \code{show.diff} instead.
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
#' @export
sjc.elbow <- function(data, steps = 15, show.diff = FALSE, showDiff = FALSE) {
  # -----------------------------------
  # warn, if deprecated param is used
  # -----------------------------------
  if (!missing(showDiff)) {
    warning("argument 'showDiff' is deprecated; please use 'show.diff' instead.")
    show.diff <- showDiff
  }
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
  dfElbowValues <- cbind(dfElbowValues, xpos = 1:nrow(dfElbowValues))
  # calculate differences between each step
  diff <- c()
  for (i in 2:steps) diff <- cbind(diff,wssround[i - 1] - wssround[i])
  dfElbowDiff <- tidyr::gather(as.data.frame(diff), 
                               "Var2", 
                               "value", 
                               1:ncol(diff),
                               factor_key = TRUE)
  # --------------------------------------------------
  # Plot diagram with sum of squares
  # all pointes are connected with a line
  # a bend the in curve progression might indicate elbow
  # --------------------------------------------------
  graphics::plot(ggplot(dfElbowValues, aes(x = xpos, y = wssround, label = wssround)) + 
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
    graphics::plot(ggplot(dfElbowDiff, aes(x = Var2, y = value, label = value)) + 
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
#'              \item Maechler, M., Rousseeuw, P., Struyf, A., Hubert, M., Hornik, K.(2013). cluster: Cluster Analysis Basics and Extensions. R package version 1.14.4. (\href{http://cran.r-project.org/package=cluster}{web})
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
  df <- data.frame(x = 1:max, 
                   y = gap$Tab[, 'gap'], 
                   se = gap$Tab[, 'SE.sim'], 
                   psize = nclus, 
                   pcol = cclus)
  # plot cluster solution
  gp <- ggplot(df, aes(x = x, y = y)) + 
    geom_errorbar(aes(ymin = y - se, ymax = y + se), 
                  width = 0, 
                  size = 0.5, 
                  colour = "#3366cc") +
    geom_line(colour = "gray50") +
    geom_point(colour = df$pcol, size = df$psize) +
    scale_x_discrete(breaks = c(1:nrow(df))) +
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
