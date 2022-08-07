# Help-functions

# evaluates arguments
get_dot_data <- function(data, dots) {
  # any dots?
  if (length(dots) > 0)
    # get variable names
    vars <- dot_names(dots)
  else
    vars <- NULL

  # check if data is a data frame
  if (is.data.frame(data)) {
    # get valid variable names
    vars <- vars[vars %in% colnames(data)]
    vars.is.empty <- sjmisc::is_empty(vars)
    if (!is.null(vars) && !vars.is.empty)
      # select variables, if any
      x <- data[, vars, drop = FALSE]
    else
      # else return complete data frame
      x <- data
  }

  x
}

# return names of objects passed as ellipses argument
dot_names <- function(dots) unname(unlist(lapply(dots, as.character)))


get_dplyr_dot_data <- function(x, qs) {
  if (sjmisc::is_empty(qs))
    x
  else
    suppressMessages(dplyr::select(x, !!!qs))
}


# add annotations with table summary
# here we print out total N of cases, chi-square and significance of the table
print.table.summary <- function(baseplot,
                                modsum,
                                summary.pos = "r") {
  if (!is.null(modsum)) {
    # add annotations with table summary
    # here we print out total N of cases, chi-square and significance of the table
    if (summary.pos == "r") {
      t.hjust <- "top"
      x.x <- Inf
    } else {
      t.hjust <- "bottom"
      x.x <- -Inf
    }
    baseplot <- baseplot +
      annotate(
        "text",
        label = modsum,
        parse = TRUE,
        x = x.x,
        y = Inf,
        vjust = "top",
        hjust = t.hjust
      )
  }

  baseplot
}


get_var_name <- function(x) {
  if (is.null(x)) return(NULL)
  # remove "data frame name"
  dollar_pos <- regexpr("$", x, fixed = TRUE)[1]
  if (dollar_pos != -1)
    x <- substr(x, start = dollar_pos + 1, stop = nchar(x))

  x
}


# Create frequency data frame of a variable
# for sjp and sjt frq functions
#' @importFrom stats na.omit ftable na.pass
#' @importFrom tidyr spread
create.xtab.df <- function(x,
                           grp,
                           round.prz = 2,
                           na.rm = FALSE,
                           weight.by = NULL) {
  # ------------------------------
  # convert to labels
  # ------------------------------
  x_full <- suppressWarnings(sjmisc::to_label(x, add.non.labelled = TRUE))
  grp_full <- suppressWarnings(sjmisc::to_label(grp, add.non.labelled = TRUE))
  # ------------------------------
  # create frequency crosstable. we need to convert
  # vector to labelled factor first.
  # ------------------------------
  if (is.null(weight.by)) {
    if (na.rm) {
      mydat <- stats::ftable(table(x_full, grp_full))
    } else {
      mydat <- stats::ftable(table(x_full, grp_full, useNA = "always"))
    }
  } else {
    if (na.rm)
      mydat <- stats::ftable(round(stats::xtabs(weight.by ~ x_full + grp_full)), 0)
    else
      mydat <- stats::ftable(round(stats::xtabs(weight.by ~ x_full + grp_full,
                                                exclude = NULL,
                                                na.action = stats::na.pass)), 0)
  }

  # create proportional tables, cell values
  ori.cell.values <- 100 * prop.table(mydat)
  proptab.cell <- round(100 * prop.table(mydat), round.prz)

  # create proportional tables, row percentages, including total row
  proptab.row <- rbind(
    as.data.frame(as.matrix(round(100 * prop.table(mydat, 1), round.prz))),
    round(colSums(ori.cell.values), round.prz)
  )

  rownames(proptab.row)[nrow(proptab.row)] <- "total"
  proptab.row <- as.data.frame(apply(proptab.row, c(1, 2), function(x) if (is.na(x)) x <- 0 else x))

  # create proportional tables, column  percentages, including total row
  proptab.col <- cbind(
    as.data.frame(as.matrix(round(100 * prop.table(mydat, 2), round.prz))),
    round(rowSums(ori.cell.values), round.prz)
  )

  colnames(proptab.col)[ncol(proptab.col)] <- "total"
  proptab.col <- as.data.frame(apply(proptab.col, c(1, 2), function(x) if (is.na(x)) x <- 0 else x))

  # add total row and column to cell percentages afterwards
  proptab.cell <- rbind(
    as.data.frame(as.matrix(proptab.cell)),
    round(colSums(ori.cell.values), round.prz)
  )

  proptab.cell <- cbind(
    as.data.frame(as.matrix(proptab.cell)),
    rowSums(proptab.cell)
  )

  # due to roundings, total might differ from 100%, so clean this here
  proptab.cell[nrow(proptab.cell), ncol(proptab.cell)] <- 100
  colnames(proptab.cell)[ncol(proptab.cell)] <- "total"
  rownames(proptab.cell)[nrow(proptab.cell)] <- "total"

  # convert to data frame
  mydat <- data.frame(mydat)
  colnames(mydat)[2] <- "Var2"

  # spread variables back, so we have a table again
  mydat <- tidyr::spread(mydat, .data$Var2, .data$Freq)

  # rename column names
  colnames(mydat)[1] <- "label"
  colnames(mydat)[is.na(colnames(mydat))] <- "NA"
  colnames(mydat)[colnames(mydat) == "<NA>"] <- "NA"

  # label must be character
  mydat$label <- as.character(mydat$label)
  mydat$label[is.na(mydat$label)] <- "NA"

  # save labels to extra vector
  labels.cnt <- mydat$label
  labels.grp <- colnames(mydat)[-1]

  # return result
  invisible(structure(list(mydat = mydat,
                           proptab.cell = proptab.cell,
                           proptab.col = proptab.col,
                           proptab.row = proptab.row,
                           labels.cnt = labels.cnt,
                           labels.grp = labels.grp)))
}


# check character encoding for HTML-tables
# (sjt-functions)
get.encoding <- function(encoding, data = NULL) {
  if (is.null(encoding)) {
    if (!is.null(data) && is.data.frame(data)) {
      # get variable label
      labs <- sjlabelled::get_label(data[[1]])
      # check if vectors of data frame have
      # any valid label. else, default to utf-8
      if (!is.null(labs) && is.character(labs)) {
        encoding <- Encoding(sjlabelled::get_label(data[[1]]))
      } else {
        encoding <- "UTF-8"
      }
      # unknown encoding? default to utf-8
      if (encoding == "unknown") {
        encoding <- "UTF-8"
      }
    } else if (.Platform$OS.type == "unix") {
      encoding <- "UTF-8"
    } else {
      encoding <- "Windows-1252"
    }
  }
  return(encoding)
}


# Calculate statistics of cross tabs
#' @importFrom sjstats cramer phi table_values
#' @importFrom stats chisq.test fisher.test xtabs
crosstabsum <- function(x, grp, weight.by) {
  # --------------------------------------------------------
  # check p-value-style option
  # --------------------------------------------------------
  opt <- getOption("p_zero")
  if (is.null(opt) || opt == FALSE) {
    p_zero <- ""
  } else {
    p_zero <- "0"
  }
  if (is.null(weight.by)) {
    ftab <- table(x, grp)
  } else {
    ftab <- round(stats::xtabs(weight.by ~ x + grp), 0)
  }
  # calculate chi square value
  chsq <- stats::chisq.test(ftab)
  p.value <- chsq$p.value
  tab <- sjstats::table_values(ftab)
  # do we have cells with less than 5 observations?
  if (min(tab$expected) < 5 || (min(tab$expected) < 10 && chsq$parameter == 1)) {
    fish <- stats::fisher.test(ftab, simulate.p.value = (nrow(ftab) > 2 || ncol(ftab) > 2))
    p.value <- fish$p.value
  } else {
    fish <- NULL
  }
  # pvalue in string
  if (p.value < 0.001)
    pvas <- sprintf("%s.001", p_zero)
  else
    pvas <- sub("0", p_zero, sprintf("%.3f", p.value))
  # check whether variables are dichotome or if they have more
  # than two categories. if they have more, use Cramer's V to calculate
  # the contingency coefficient
  if (nrow(ftab) > 2 || ncol(ftab) > 2) {
    # check whether fisher's test or chi-squared should be printed
    if (is.null(fish)) {
      modsum <- as.character(as.expression(
        substitute("N" == tn * "," ~~ chi^2 == c2 * "," ~~ "df" == dft * "," ~~ phi[c] == kook * "," ~~ "p" == pva,
                   list(tn = summary(ftab)$n.cases,
                        c2 = sprintf("%.2f", chsq$statistic),
                        dft = c(chsq$parameter),
                        kook = sprintf("%.2f", sjstats::cramer(ftab)),
                        pva = pvas))))
    } else {
      modsum <- as.character(as.expression(
        substitute("N" == tn * "," ~~ "df" == dft * "," ~~ phi[c] == kook * "," ~~ "Fisher's p" == pva,
                   list(tn = summary(ftab)$n.cases,
                        dft = c(chsq$parameter),
                        kook = sprintf("%.2f", sjstats::cramer(ftab)),
                        pva = pvas))))
    }
  # if variables have two categories (2x2 table), use phi to calculate
  # the degree of association
  } else {
    # check whether fisher's test or chi-squared should be printed
    if (is.null(fish)) {
      modsum <- as.character(as.expression(
        substitute("N" == tn * "," ~~ chi^2 == c2 * "," ~~ "df" == dft * "," ~~ phi == kook * "," ~~ "p" == pva,
                   list(tn = summary(ftab)$n.cases,
                        c2 = sprintf("%.2f", chsq$statistic),
                        dft = c(chsq$parameter),
                        kook = sprintf("%.2f", sjstats::phi(ftab)),
                        pva = pvas))))
    } else {
      modsum <- as.character(as.expression(
        substitute("N" == tn * "," ~~ "df" == dft * "," ~~ phi == kook * "," ~~ "Fisher's p" == pva,
                   list(tn = summary(ftab)$n.cases,
                        dft = c(chsq$parameter),
                        kook = sprintf("%.2f", sjstats::phi(ftab)),
                        pva = pvas))))
    }
  }
  return(modsum)
}


# Erzeugt eine rotierte Faktorladungen einer Hauptkomponentenanalyse
# (Paramter "data") mit einer bestimmten Anzahl an Faktoren (Parameter "factors")
# auf Grundlage der Varimax-Rotation
#
# Parameter:
# - data: the results (object) from a principal component analysis
#         (prcomp(myData...))
# - factors: the amount of factors. can be calculated from the
#            below function "factorcount"
varimaxrota <- function(data, factors) {
  # Faktorladungen berechnen
  # Die Faktorladungen erhält man durch Multiplikation der Eigenvektoren
  # mit der Diagonalmatrix der ausgewiesenen Standardabweichungen
  ladungen <- data$rotation %*% diag(data$sdev)
  # Zur Durchführung der VARIMAX-Rotation erzeugen wir eine Matrix
  # mit den Faktorladungen der ausgewählten Faktoren (Anzahl = Parameter "factors")
  # Varimax Rotation durchführen
  varib <- stats::varimax(ladungen[, seq_len(factors)])
  varib
}


# unlist labels
# Help function that unlists a list into a vector
unlistlabels <- function(lab) {
  dummy <- unlist(lab)
  labels <- c()
  labels <- c(labels, as.character(dummy))
  return(labels)
}


sju.rmspc <- function(html.table) {
  cleaned <- gsub("      <", "<", html.table, fixed = TRUE, useBytes = TRUE)
  cleaned <- gsub("    <", "<", cleaned, fixed = TRUE, useBytes = TRUE)
  cleaned <- gsub("  <", "<", cleaned, fixed = TRUE, useBytes = TRUE)
  return(cleaned)
}

.is_false <- function(x) {
  is.logical(x) && length(x) == 1L && !is.na(x) && !x
}
