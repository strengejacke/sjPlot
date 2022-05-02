#' @title Summary of item analysis of an item scale as HTML table
#' @name tab_itemscale
#'
#' @description This function performs an item analysis with certain statistics that are
#'                useful for scale or index development. The resulting tables are shown in the
#'                viewer pane resp. webbrowser or can be saved as file. Following statistics are
#'                computed for each item of a data frame:
#'                \itemize{
#'                  \item percentage of missing values
#'                  \item mean value
#'                  \item standard deviation
#'                  \item skew
#'                  \item item difficulty
#'                  \item item discrimination
#'                  \item Cronbach's Alpha if item was removed from scale
#'                  \item mean (or average) inter-item-correlation
#'                }
#'                Optional, following statistics can be computed as well:
#'                \itemize{
#'                  \item kurstosis
#'                  \item Shapiro-Wilk Normality Test
#'                }
#'                If \code{factor.groups} is not \code{NULL}, the data frame \code{df} will be
#'                splitted into groups, assuming that \code{factor.groups} indicate those columns
#'                of the data frame that belong to a certain factor (see return value of function \code{\link{tab_pca}}
#'                as example for retrieving factor groups for a scale and see examples for more details).
#'
#' @param df A data frame with items.
#' @param factor.groups If not \code{NULL}, \code{df} will be splitted into sub-groups,
#'          where the item analysis is carried out for each of these groups. Must be a vector of same
#'          length as \code{ncol(df)}, where each item in this vector represents the group number of
#'          the related columns of \code{df}. If \code{factor.groups = "auto"}, a principal
#'          component analysis with Varimax rotation is performed, and the resulting
#'          groups for the components are used as group index. See 'Examples'.
#' @param factor.groups.titles Titles for each factor group that will be used as table caption for each
#'          component-table. Must be a character vector of same length as \code{length(unique(factor.groups))}.
#'          Default is \code{"auto"}, which means that each table has a standard caption \emph{Component x}.
#'          Use \code{NULL} to suppress table captions.
#' @param scale Logical, if \code{TRUE}, the data frame's vectors will be scaled when calculating the
#'          Cronbach's Alpha value (see \code{\link[performance]{item_reliability}}). Recommended, when
#'          the variables have different measures / scales.
#' @param min.valid.rowmean Minimum amount of valid values to compute row means for index scores.
#'          Default is 2, i.e. the return values \code{index.scores} and \code{df.index.scores} are
#'          computed for those items that have at least \code{min.valid.rowmean} per case (observation, or
#'          technically, row). See \code{mean_n} for details.
#' @param show.shapiro Logical, if \code{TRUE}, a Shapiro-Wilk normality test is computed for each item.
#'          See \code{\link{shapiro.test}} for details.
#' @param show.kurtosis Logical, if \code{TRUE}, the kurtosis for each item will also be shown (see \code{\link[psych]{kurtosi}}
#'          and \code{\link[psych]{describe}} in the \code{psych}-package for more details.
#' @param show.corr.matrix Logical, if \code{TRUE} (default), a correlation matrix of each component's
#'          index score is shown. Only applies if \code{factor.groups} is not \code{NULL} and \code{df} has
#'          more than one group. First, for each case (df's row), the sum of all variables (df's columns) is
#'          scaled (using the \code{\link{scale}}-function) and represents a "total score" for
#'          each component (a component is represented by each group of \code{factor.groups}).
#'          After that, each case (df's row) has a scales sum score for each component.
#'          Finally, a correlation of these "scale sum scores" is computed.
#'
#' @inheritParams tab_model
#' @inheritParams view_df
#' @inheritParams tab_xtab
#' @inheritParams tab_df
#'
#' @return Invisibly returns
#'         \itemize{
#'          \item \code{df.list}: List of data frames with the item analysis for each sub.group (or complete, if \code{factor.groups} was \code{NULL})
#'          \item \code{index.scores}: A data frame with of standardized scale / index scores for each case (mean value of all scale items for each case) for each sub-group.
#'          \item \code{ideal.item.diff}: List of vectors that indicate the ideal item difficulty for each item in each sub-group. Item difficulty only differs when items have different levels.
#'          \item \code{cronbach.values}: List of Cronbach's Alpha values for the overall item scale for each sub-group.
#'          \item \code{knitr.list}: List of html-tables with inline-css for use with knitr for each table (sub-group)
#'          \item \code{knitr}: html-table of all complete output with inline-css for use with knitr
#'          \item \code{complete.page}: Complete html-output.
#'          }
#'          If \code{factor.groups = NULL}, each list contains only one elment, since just one
#'          table is printed for the complete scale indicated by \code{df}. If \code{factor.groups}
#'          is a vector of group-index-values, the lists contain elements for each sub-group.
#'
#' @note \itemize{
#'          \item The \emph{Shapiro-Wilk Normality Test} (see column \code{W(p)}) tests if an item has a distribution that is significantly different from normal.
#'          \item \emph{Item difficulty} should range between 0.2 and 0.8. Ideal value is \code{p+(1-p)/2} (which mostly is between 0.5 and 0.8).
#'          \item For \emph{item discrimination}, acceptable values are 0.20 or higher; the closer to 1.00 the better. See \code{\link[performance]{item_reliability}} for more details.
#'          \item In case the total \emph{Cronbach's Alpha} value is below the acceptable cut-off of 0.7 (mostly if an index has few items), the \emph{mean inter-item-correlation} is an alternative measure to indicate acceptability. Satisfactory range lies between 0.2 and 0.4. See also \code{\link[performance]{item_intercor}}.
#'        }
#'
#' @references \itemize{
#'              \item Jorion N, Self B, James K, Schroeder L, DiBello L, Pellegrino J (2013) Classical Test Theory Analysis of the Dynamics Concept Inventory. (\href{https://www.academia.edu/4104752/Classical_Test_Theory_Analysis_of_the_Dynamics_Concept_Inventory}{web})
#'              \item Briggs SR, Cheek JM (1986) The role of factor analysis in the development and evaluation of personality scales. Journal of Personality, 54(1), 106-148. doi: 10.1111/j.1467-6494.1986.tb00391.x
#'              \item McLean S et al. (2013) Stigmatizing attitudes and beliefs about bulimia nervosa: Gender, age, education and income variability in a community sample. International Journal of Eating Disorders. doi: 10.1002/eat.22227
#'              \item Trochim WMK (2008) Types of Reliability. (\href{https://conjointly.com/kb/types-of-reliability/}{web})
#'             }
#'
#' @examples
#' # Data from the EUROFAMCARE sample dataset
#' library(sjmisc)
#' library(sjlabelled)
#' data(efc)
#'
#' # retrieve variable and value labels
#' varlabs <- get_label(efc)
#'
#' # recveive first item of COPE-index scale
#' start <- which(colnames(efc) == "c82cop1")
#' # recveive last item of COPE-index scale
#' end <- which(colnames(efc) == "c90cop9")
#'
#' # create data frame with COPE-index scale
#' mydf <- data.frame(efc[, start:end])
#' colnames(mydf) <- varlabs[start:end]
#'
#' \dontrun{
#' if (interactive()) {
#'   tab_itemscale(mydf)
#'
#'   # auto-detection of labels
#'   tab_itemscale(efc[, start:end])
#'
#'   # Compute PCA on Cope-Index, and perform a
#'   # item analysis for each extracted factor.
#'   indices <- tab_pca(mydf)$factor.index
#'   tab_itemscale(mydf, factor.groups = indices)
#'
#'   # or, equivalent
#'   tab_itemscale(mydf, factor.groups = "auto")
#' }}
#' @export
tab_itemscale <- function(df,
                             factor.groups = NULL,
                             factor.groups.titles = "auto",
                             scale = FALSE,
                             min.valid.rowmean = 2,
                             alternate.rows = TRUE,
                             sort.column = NULL,
                             show.shapiro = FALSE,
                             show.kurtosis = FALSE,
                             show.corr.matrix = TRUE,
                             CSS = NULL,
                             encoding = NULL,
                             file = NULL,
                             use.viewer = TRUE,
                             remove.spaces = TRUE) {
  # check encoding
  encoding <- get.encoding(encoding, df)

  # convert ordered factors to numeric
  ordered_vars <- sapply(df, is.ordered)
  if (any(ordered_vars)) df[ordered_vars] <- sjlabelled::as_numeric(df[ordered_vars])

  # Warn if factors are used
  factor_vars <- sapply(df, is.factor)
  if (any(factor_vars)) {
    df[factor_vars] <- sjlabelled::as_numeric(df[factor_vars])
    warning("At least one variable is of type factor, please check if the factor levels are ordered correctly.")
  }

  # auto-detect variable labels
  varlabels <- sjlabelled::get_label(df, def.value = colnames(df))
  colnames(df) <- varlabels

  # check whether we have (factor) groups
  # for data frame
  if (is.null(factor.groups))
    factor.groups <- rep(1, length.out = ncol(df))
  else if (inherits(factor.groups, "parameters_pca"))
    factor.groups <- parameters::closest_component(factor.groups)
  else if (length(factor.groups) == 1 && factor.groups == "auto") {
    pr <- parameters::principal_components(df, rotation = "varimax")
    factor.groups <- parameters::closest_component(pr)
  }

  # data frame with data from item-analysis-output-table
  df.ia <- list()

  # component's correlation matrix
  df.comcor <- list()
  diff.ideal.list <- list()
  index.scores <- list()

  # cronbach's alpha values
  cronbach.total <- list()

  # mean inter-item-correlation values
  mic.total <- list()

  # retrieve unique factor / group index values
  findex <- sort(unique(factor.groups))

  # set titles
  if (!is.null(factor.groups.titles) && (factor.groups.titles[1] == "auto" || length(factor.groups.titles) != length(findex))) {
    factor.groups.titles <- sprintf("Component %i", seq_along(findex))
  }

  # iterate all sub-scales (groups)
  for (i in seq_len(length(findex))) {

    # retrieve sub-scale
    df.sub <- subset(df, select = which(factor.groups == findex[i]))

    # remember item (column) names for return value
    # return value gets column names of initial data frame
    df.names <- colnames(df)[which(factor.groups == findex[i])]

    # retrieve missing percentage for each item
    missings.prz <- apply(df.sub, 2, function(x) round(100 * sum(is.na(x)) / length(x), 2))

    # item difficulty
    difficulty <- apply(df.sub, 2, function(x) {
      x <- stats::na.omit(x)
      round(sum(x) / (max(x) * length(x)), 2)
      })

    # ideal item difficulty
    fun.diff.ideal <- function(x) {
      p <- 1 / max(x, na.rm = TRUE)
      round(p + (1 - p) / 2, 2)
    }

    diff.ideal <- apply(df.sub, 2, fun.diff.ideal)

    # get statistics
    dstat <- sjmisc::descr(df.sub)
    reli <- performance::item_reliability(df.sub, standardize = scale)

    # get index score value, by retrieving the row mean
    item.score <- sjstats::mean_n(df.sub, min.valid.rowmean)

    # store scaled values of each item's total score
    # to compute correlation coefficients between identified components
    df.subcc <- subset(df, select = which(factor.groups == findex[i]))
    comcor <- sjmisc::std(rowSums(df.subcc), append = FALSE)

    # check if we have valid return values from reliability test.
    # In case df had less than 3 columns, NULL is returned
    if (!is.null(reli)) {
      alpha <- reli$alpha_if_deleted
      itemdis <- reli$item_discrimination
    } else {
      alpha <- as.factor(NA)
      itemdis <- as.factor(NA)
    }

    # create dummy data frame
    df.dummy <- data_frame(cbind(
      sprintf("%.2f %%", missings.prz),
      round(dstat$mean, 2),
      round(dstat$sd, 2),
      round(dstat$skew, 2)
    ))

    df.colnames <- c("Missings", "Mean", "SD", "Skew")

    # include kurtosis statistics
    if (show.kurtosis) {
      df.dummy <- data_frame(cbind(df.dummy, round(as.numeric(datawizard::kurtosis(df.sub)), 2)))
      df.colnames <- c(df.colnames, "Kurtosis")
    }

    # include shapiro-wilk normality test
    if (show.shapiro) {
      shaptest.w <- apply(df.sub, 2, function(x) stats::shapiro.test(x)$statistic)
      shaptest.p <- apply(df.sub, 2, function(x) stats::shapiro.test(x)$p.value)
      df.dummy <- data_frame(cbind(df.dummy, sprintf("%.2f (%.3f)", shaptest.w, shaptest.p)))
      df.colnames <- c(df.colnames, "W(p)")
    }

    df.dummy <- data.frame(cbind(df.dummy, difficulty, itemdis, alpha))
    df.colnames <- c(df.colnames, "Item Difficulty", "Item Discrimination", "&alpha; if deleted")

    # set names of data frame
    colnames(df.dummy) <- df.colnames
    rownames(df.dummy) <- df.names

    # add results to return list
    df.ia[[length(df.ia) + 1]] <- df.dummy
    diff.ideal.list[[length(diff.ideal.list) + 1]] <- diff.ideal
    index.scores[[length(index.scores) + 1]] <- item.score
    cronbach.total[[length(cronbach.total) + 1]] <- performance::cronbachs_alpha(df.sub)
    df.comcor[[length(df.comcor) + 1]] <- comcor

    # Mean-interitem-corelation
    mic.total[[length(mic.total) + 1]] <- performance::item_intercor(df.sub)
  }

  # create data frame with index scores,
  # including missings
  df.index.scores <- as.data.frame(index.scores)

  # proper col names
  colnames(df.index.scores) <- sprintf("Score%i", seq_len(ncol(df.index.scores)))

  footns <- purrr::map2_chr(mic.total, cronbach.total, ~ sprintf(
      "Mean inter-item-correlation=%.3f &middot; Cronbach's &alpha;=%.3f", .x, .y
    ))

  if (is.null(CSS)) CSS <- list(css.firsttablecol = '+text-align:left;')

  # get html-table from data frame
  html <- tab_dfs(
    x = df.ia,
    titles = factor.groups.titles,
    col.header = NULL,
    alternate.rows = alternate.rows,
    CSS = CSS,
    sort.column = sort.column,
    show.type = FALSE,
    show.rownames = TRUE,
    use.viewer = TRUE,
    encoding = encoding,
    show.footnote = TRUE,
    footnotes = footns,
    file = file
  )

  html2 <- NULL

  # show component correlation table
  if (show.corr.matrix) {

    # check if we have enough components
    if (length(df.comcor) > 1) {

      # copy all component correlation values to a data frame
      df.cc <- data.frame(matrix(unlist(df.comcor),
                                 nrow = nrow(df),
                                 byrow = FALSE))

      # give proper columm names
      colnames(df.cc) <- sprintf("Component %i", seq_len(ncol(df.cc)))

      # compute correlation table, store html result
      html2 <- tab_corr(
        df.cc,
        na.deletion = "listwise",
        p.numeric = TRUE,
        triangle = "lower",
        string.diag = sprintf("&alpha;=%.3f", unlist(cronbach.total)),
        encoding = encoding
      )

    }
  }

  if (!is.null(html2)) {
    html$knitr <- paste0(html$knitr, "<p>&nbsp;</p>", html2$knitr)
    html$page.content <- paste0(html$page.content, "<p>&nbsp;</p>", html2$page.content)
    html$page.style <- paste0(html$page.style, html2$page.style)
    html$page.complete <-
      sprintf(
        "<html>\n<head>\n<meta http-equiv=\"Content-type\" content=\"text/html;charset=%s\">\n%s\n</head>\n<body>\n%s\n</body></html>",
        encoding,
        html$page.style,
        html$page.content
      )
  }

  html$df.list <- df.ia
  html$index.scores <- df.index.scores
  html$cronbach.values <- cronbach.total
  html$ideal.item.diff <- diff.ideal.list

  sjlabelled::set_label(html$index.scores) <- purrr::map2_chr(mic.total, cronbach.total, ~ sprintf(
    "Mean icc=%.3f; Cronbach's Alpha=%.3f", .x, .y
  ))

  html
}


#' @rdname tab_itemscale
#' @export
sjt.itemanalysis <- tab_itemscale
