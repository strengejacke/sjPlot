# bind global variables
utils::globalVariables(c("Freq", "vif"))

# Help-functions


# get additional arguments for geoms
get_dot_args <- function(x) {
  # ---------------------------------------
  # get ...-argument, and check if it was "width"
  # ---------------------------------------
  eb.width <- x[["width"]]
  if (is.null(eb.width)) eb.width <- 0
  # get ...-argument, and check if it was "alpha"
  ci.alpha <- x[["alpha"]]
  if (is.null(ci.alpha)) ci.alpha <- .15
  # get ...-argument, and check if it was "level"
  ci.lvl <- x[["level"]]
  if (is.null(ci.lvl)) ci.lvl <- .95

  list(eb.width = eb.width,
       ci.alpha = ci.alpha,
       ci.lvl = ci.lvl)
}

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


# function to create pretty breaks
# for log-scales
#' @importFrom grDevices axisTicks
base_breaks <- function(n = 10) {
  function(x) {
    grDevices::axisTicks(log10(range(x, na.rm = TRUE)), log = TRUE, nint = n)
  }
}


#' @importFrom sjmisc get_label get_labels str_contains to_label to_value replace_na word_wrap
#' @importFrom sjstats resp_val resp_var
get_lm_data <- function(fit) {
  if (inherits(fit, "plm")) {
    # plm objects have different structure than (g)lm
    fit_x <- data.frame(cbind(as.vector(fit$model[, 1]), stats::model.matrix(fit)))
    # retrieve response vector
    resp <- sjstats::resp_val(fit)
    depvar.label <- sjstats::resp_var(fit)
  } else if (inherits(fit, "pggls")) {
    # plm objects have different structure than (g)lm
    fit_x <- data.frame(fit$model)
    depvar.label <- attr(attr(attr(fit$model, "terms"), "dataClasses"), "names")[1]
    # retrieve response vector
    resp <- as.vector(fit$model[, 1])
  } else if (is_merMod(fit)) {
    fit_x <- data.frame(stats::model.matrix(fit))
    # retrieve response vector
    resp <- stats::model.frame(fit)[[1]]
    depvar.label <- sjstats::resp_var(fit)
  } else if (inherits(fit, "gls")) {
    fit_x <- data.frame(stats::model.matrix(fit))
    resp <- nlme::getResponse(fit)
    depvar.label <- attr(resp, "label")
  } else {
    fit_x <- data.frame(stats::model.matrix(fit))
    # retrieve response vector
    resp <- stats::model.frame(fit)[[1]]
    depvar.label <- sjstats::resp_var(fit)
  }
  # get variable label label
  depvar.label <- unname(sjmisc::get_label(x = resp, def.value = depvar.label))
  return(list(matrix = fit_x, resp.label = depvar.label, resp = resp))
}


get_glm_family <- function(fit) {
  c.f <- class(fit)
  # ------------------------
  # do we have glm? if so, get link family. make exceptions
  # for specific models that don't have family function
  # ------------------------
  if (any(c.f %in% c("lme", "plm"))) {
    fitfam <- ""
    logit_link <- FALSE
  } else {
    fitfam <- stats::family(fit)$family
    logit_link <- stats::family(fit)$link == "logit"
  }
  # --------------------------------------------------------
  # create logical for family
  # --------------------------------------------------------
  binom_fam <- fitfam %in% c("binomial", "quasibinomial")
  poisson_fam <- fitfam %in% c("poisson", "quasipoisson") ||
    sjmisc::str_contains(fitfam, "negative binomial", ignore.case = T)
  return(list(is_bin = binom_fam, is_pois = poisson_fam, is_logit = logit_link))
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
      annotate("text",
               label = modsum,
               parse = TRUE,
               x = x.x,
               y = Inf,
               vjust = "top",
               hjust = t.hjust)
  }
  return(baseplot)
}


# display html-content in viewer pane
# or write it to file
out.html.table <- function(no.output, file, knitr, toWrite, use.viewer) {
  if (!no.output) {
    # -------------------------------------
    # check if we have filename specified
    # -------------------------------------
    if (!is.null(file)) {
      # write file
      write(knitr, file = file)
    } else {
      # -------------------------------------
      # else open in viewer pane
      # -------------------------------------
      # create and browse temporary file
      htmlFile <- tempfile(fileext = ".html")
      write(toWrite, file = htmlFile)
      # check whether we have RStudio Viewer
      viewer <- getOption("viewer")
      if (use.viewer && !is.null(viewer)) {
        viewer(htmlFile)
      } else {
        utils::browseURL(htmlFile)
      }
      # delete temp file
      # unlink(htmlFile)
    }
  }
}


get_var_name <- function(x) {
  # remove "data frame name"
  dollar_pos <- regexpr("$", x, fixed = T)[1]
  if (dollar_pos != -1)
    x <-
    substr(x, start = dollar_pos + 1, stop = nchar(x))
  return(x)
}


# Create frequency data frame of a variable
# for sjp and sjt frq functions
#' @importFrom sjstats weight table_values cramer phi
#' @importFrom stats na.omit
#' @importFrom dplyr full_join
#' @importFrom sjmisc zap_inf
create.frq.df <- function(x,
                          wrap.labels = Inf,
                          order.frq = "none",
                          round.prz = 2,
                          na.rm = FALSE,
                          weight.by = NULL) {
  #---------------------------------------------------
  # zap inf and NaN
  #---------------------------------------------------
  x <- sjmisc::zap_inf(x)
  #---------------------------------------------------
  # variable with only mising?
  #---------------------------------------------------
  if (length(stats::na.omit(x)) == 0) {
    mydat <- data.frame(val = NA,
                        label = NA,
                        frq = NA,
                        raw.prc = NA,
                        valid.prc = NA,
                        cum.perc = NA,
                        upper.ci = NA,
                        lower.ci = NA)
    return(invisible(structure(list(mydat = mydat))))
  }
  #---------------------------------------------------
  # get value labels (if any)
  #---------------------------------------------------
  labels <- sjmisc::get_labels(
    x,
    attr.only = T,
    include.values = "n",
    include.non.labelled = T
  )
  #---------------------------------------------------
  # weight variable
  #---------------------------------------------------
  if (!is.null(weight.by)) x <- sjstats::weight(x, weight.by)
  #---------------------------------------------------
  # do we have a labelled vector?
  #---------------------------------------------------
  if (!is.null(labels)) {
    # add rownames and values as columns
    dat <- data.frame(n = names(labels), v = as.character(labels), stringsAsFactors = FALSE)
    colnames(dat) <- c("val", "label")
    # character vectors need to be converted with to_value
    # to avoid NAs, but only if character is non-numeric
    if (is.character(dat$val) && anyNA(suppressWarnings(as.numeric(dat$val))))
      dat$val <- sjmisc::to_value(dat$val, keep.labels = F)
    else
      dat$val <- as.numeric(dat$val)
    # create frequency table
    dat2 <- data.frame(table(x, useNA = "always"))
    colnames(dat2) <- c("val", "frq")
    dat2$val <- sjmisc::to_value(dat2$val, keep.labels = F)
    # join frq table and label columns
    mydat <- suppressMessages(dplyr::full_join(dat, dat2))
    # replace NA with 0, for proper percentages, i.e.
    # missing values don't appear (zero counts)
    mydat$frq <- suppressMessages(sjmisc::replace_na(mydat$frq, value = 0))
  } else {
    # if we have no labels, do simple frq table
    mydat <- data.frame(table(x, useNA = "always"))
    colnames(mydat) <- c("val", "frq")
    # add values as label
    mydat$label <- labels <- as.character(mydat$val)
  }
  #---------------------------------------------------
  # need numeric
  #---------------------------------------------------
  if (is.factor(x) || is.character(x)) {
    x <- sjmisc::to_value(x, keep.labels = F)
  }
  # valid values are one row less, because last row is NA row
  valid.vals <- nrow(mydat) - 1
  # total sum of variable, for confindence intervals
  total_n = sum(mydat$frq)
  rel_frq <- as.numeric(mydat$frq / total_n)
  ci <- 1.96 * suppressWarnings(sqrt(rel_frq * (1 - rel_frq) / total_n))
  mydat$upper.ci <- total_n * (rel_frq + ci)
  mydat$lower.ci <- total_n * (rel_frq - ci)
  mydat$rel.upper.ci <- rel_frq + ci
  mydat$rel.lower.ci <- rel_frq - ci
  # --------------------------------------------------------
  # Order categories ascending or descending
  # --------------------------------------------------------
  if (!is.null(order.frq) && (order.frq == "asc" || order.frq == "desc")) {
    ord <- order(mydat$frq[seq_len(valid.vals)], decreasing = (order.frq == "desc"))
    mydat <- mydat[c(ord, valid.vals + 1), ]
    labels <- labels[ord]
  }
  # raw percentages
  mydat$raw.prc <- mydat$frq / sum(mydat$frq)
  # compute valud and cumulative percentages
  mydat$valid.prc <- c(mydat$frq[seq_len(valid.vals)] / length(stats::na.omit(x)), NA)
  mydat$cum.prc <- c(cumsum(mydat$valid.prc[seq_len(valid.vals)]), NA)
  # proper rounding
  mydat$raw.prc <- 100 * round(mydat$raw.prc, round.prz + 2)
  mydat$cum.prc <- 100 * round(mydat$cum.prc, round.prz + 2)
  mydat$valid.prc <- 100 * round(mydat$valid.prc, round.prz + 2)
  # -------------------------------------
  # remove na?
  # -------------------------------------
  if (na.rm) {
    mydat <- mydat[1:valid.vals, ]
  }
  # -------------------------------------
  # "rename" NA values
  # -------------------------------------
  if (!is.null(mydat$label)) {
    mydat$label[is.na(mydat$label)] <- "NA"
    mydat$label[mydat$label == "<NA>"] <- "NA"
  }
  mydat$val <- suppressMessages(sjmisc::replace_na(mydat$val, value = max(sjmisc::to_value(mydat$val), na.rm = T) + 1))
  # save original order
  mydat$order <- sjmisc::to_value(mydat$val, keep.labels = F)
  # sort for x-axis
  mydat$val <- sort(mydat$order)
  # -------------------------------------
  # wrap labels?
  # -------------------------------------
  if (!is.infinite(wrap.labels) && !is.null(labels)) {
    if (anyNA(labels)) labels <- na.omit(labels)
    labels <- sjmisc::word_wrap(labels, wrap.labels)
  }
  # -------------------------------------
  # return results
  # -------------------------------------
  class(mydat) <- "data.frame"
  invisible(structure(list(mydat = mydat,
                           labels = labels,
                           minval = min(sjmisc::to_value(mydat$val, keep.labels = FALSE), na.rm = TRUE))))
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
  x_full <- suppressWarnings(sjmisc::to_label(x, add.non.labelled = T))
  grp_full <- suppressWarnings(sjmisc::to_label(grp, add.non.labelled = T))
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
  mydat <- tidyr::spread(mydat, Var2, Freq)
  
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
      labs <- sjmisc::get_label(data[[1]])
      # check if vectors of data frame have
      # any valid label. else, default to utf-8
      if (!is.null(labs) && is.character(labs))
        encoding <- Encoding(sjmisc::get_label(data[[1]]))
      else
        encoding <- "UTF-8"
      # unknown encoding? default to utf-8
      if (encoding == "unknown") encoding <- "UTF-8"
    } else if (.Platform$OS.type == "unix")
      encoding <- "UTF-8"
    else
      encoding <- "Windows-1252"
  }
  return(encoding)
}


# check whether a color value is indicating
# a color brewer palette
is.brewer.pal <- function(pal) {
  bp.seq <- c("BuGn", "BuPu", "GnBu", "OrRd", "PuBu", "PuBuGn", "PuRd", "RdPu",
              "YlGn", "YlGnBu", "YlOrBr", "YlOrRd", "Blues", "Greens", "Greys",
              "Oranges", "Purples", "Reds")
  bp.div <- c("BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu",
              "RdYlGn", "Spectral")
  bp.qul <- c("Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1",
              "Set2", "Set3")
  bp <- c(bp.seq, bp.div, bp.qul)
  return(any(bp == pal))
}


# Calculate statistics of cross tabs
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
        substitute("N" == tn * "," ~~ chi^2 == c2 * "," ~~ "df" == dft * "," ~~ phi[c] == kook * "," ~~ "p" < pva,
                   list(tn = summary(ftab)$n.cases,
                        c2 = sprintf("%.2f", chsq$statistic),
                        dft = c(chsq$parameter),
                        kook = sprintf("%.2f", sjstats::cramer(ftab)),
                        pva = pvas))))
    } else {
      modsum <- as.character(as.expression(
        substitute("N" == tn * "," ~~ "df" == dft * "," ~~ phi[c] == kook * "," ~~ "Fisher's p" < pva,
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


# checks at which position in fitted models factors with
# more than two levels are located.
#' @importFrom stats model.frame
retrieveModelGroupIndices <- function(models, rem_rows = NULL) {
  # init group-row-indices
  group.pred.rows <- c()
  group.pred.labs <- c()
  group.pred.span <- c()
  found.factors <- c()
  add.index <- 0
  # ------------------------
  # retrieve fitted models
  # ------------------------
  # go through fitted models
  for (k in seq_len(length(models))) {
    # get model
    fit <- models[[k]]
    # copy model frame
    if (is_merMod(fit))
      fmodel <- stats::model.frame(fit, fixed.only = T)
    else
      fmodel <- stats::model.frame(fit)
    # retrieve all factors from model
    for (grp.cnt in seq_len(ncol(fmodel))) {
      # get variable
      fit.var <- fmodel[, grp.cnt]
      # is factor? and has more than two levels?
      # (otherwise, only one category would appear in
      # coefficients, so no grouping needed anyway)
      if (is.factor(fit.var) && nlevels(fit.var) > 2) {
        # get factor name
        fac.name <- colnames(fmodel)[grp.cnt]
        # check whether we already have this factor
        if (!any(found.factors == fac.name)) {
          # if not, save found factor variable name
          found.factors <- c(found.factors, fac.name)
          # save factor name
          lab <- unname(sjmisc::get_label(fit.var, def.value = fac.name))
          # determins startindex
          index <- grp.cnt + add.index - 1
          index.add <- nlevels(fit.var) - 2
          # save row index, so we know where to start group
          group.pred.rows <- c(group.pred.rows, index)
          group.pred.span <- c(group.pred.span, index:(index + index.add))
          group.pred.labs <- c(group.pred.labs, lab)
          # increase add.index by amount of factor levels (minus reference cat.)
          add.index <- add.index + index.add
        } else {
          add.index <- add.index + nlevels(fit.var) - 2
        }
      }
    }
  }
  # have any groups? if not, reset row-index-counter
  if (length(group.pred.rows) < 1) {
    group.pred.rows <- NULL
    group.pred.labs <- NULL
    group.pred.span <- NULL
  }
  # do we have any rows removed?
  else if (!is.null(rem_rows)) {
    # any non-computed row-indices left?
    while (length(rem_rows) > 0) {
      # take care, while loop!
      any.found <- FALSE
      # if yes, go through all grouping row indices
      for (i in seq_len(length(group.pred.rows))) {
        # if yes, check if removed row was before
        # grouped row indes
        if (length(rem_rows) > 0 && rem_rows[1] <= group.pred.rows[i]) {
          # if yes, iterate all remaining group indices
          for (j in i:length(group.pred.rows)) {
            # and reduce index number (because of removed rows)
            group.pred.rows[j] <- group.pred.rows[j] - 1
          }
          # where does span for grouping start?
          start <- min(which(group.pred.span >= rem_rows[1]))
          for (j in start:length(group.pred.span)) {
            # and reduce index number (because of removed rows)
            group.pred.span[j] <- group.pred.span[j] - 1
          }
          # reduce indices
          rem_rows <- rem_rows - 1
          # remove computed row-index
          rem_rows <- rem_rows[-1]
          # found something!
          any.found <- TRUE
        }
      }
      # removed any index? if not, break loop
      if (!any.found) break
    }
  }
  return(list(group.pred.rows,
              group.pred.span,
              group.pred.labs))
}


is_merMod <- function(fit) {
  return(inherits(fit, c("lmerMod", "glmerMod", "nlmerMod", "merModLmerTest")))
}


# automatically retrieve predictor labels
# of fitted (g)lm
#' @importFrom stats formula terms
retrieveModelLabels <- function(models, group.pred) {
  fit.labels <- c()
  for (k in seq_len(length(models))) {
    # get model
    fit <- models[[k]]
    # any valid model?
    if (inherits(fit, c("plm", "ppgls"))) return(NULL)
    # get model coefficients' names
    if (is_merMod(fit)) {
      coef_names <- names(lme4::fixef(fit))
      # get model frame
      m_f <- stats::model.frame(fit, fixed.only = TRUE)
    } else {
      coef_names <- names(stats::coef(fit))
      # get model frame
      m_f <- stats::model.frame(fit)
    }
    # for NULL-models, we just have one column in model frame
    if (ncol(m_f) > 1) {
      # iterate coefficients (1 is intercept or response)
      for (i in 2:ncol(m_f)) {
        # check bounds
        if (i <= length(coef_names)) {
          # get predictor
          pvar <- m_f[, i]
          # check if we have a variable label
          lab <- sjmisc::get_label(pvar, def.value = colnames(m_f)[i])
          # get model coefficients' names
          coef_name <- coef_names[i]
          # is predictor a factor?
          # if yes, we have this variable multiple
          # times, so manually set value labels
          if (is.factor(pvar)) {
            # get amount of levels
            pvar.len <- nlevels(pvar)
            # get value labels, if any
            pvar.lab <- sjmisc::get_labels(pvar)
            # have any labels, and have we same amount of labels
            # as factor levels?
            if (!is.null(pvar.lab) && length(pvar.lab) == pvar.len) {
              # create labels
              if (group.pred && pvar.len > 2) {
                # if predictor grouping is enabled, don't use variable labels again
                labels.to.add <- pvar.lab[2:pvar.len]
              } else {
                # else, if we have not grouped predictors, we have no headin
                # with variable label, hence, factor levels may not be intuitiv.
                # thus, add variable label so values have a meaning
                labels.to.add <- sprintf("%s (%s)", lab, pvar.lab[2:pvar.len])
              }
              fit.labels <- c(fit.labels, labels.to.add)
            } else {
              fit.labels <- c(fit.labels, coef_name)
            }
          } else {
            if (!any(fit.labels == lab)) fit.labels <- c(fit.labels, lab)
          }
        }
      }
    }
  }
  return(unique(fit.labels))
}


# compute chi-square for glm
Chisquare.glm <- function(rr, digits = 3) {
  return(with(rr, pchisq(null.deviance - deviance,
                         df.null - df.residual,
                         lower.tail = FALSE), digits = digits))
}


# compute model statistics for lm
#' @importFrom stats pf AIC
sju.modsum.lm <- function(fit) {
  # get F-statistics
  fstat <- summary(fit)$fstatistic
  # Calculate p-value for F-test
  pval <- stats::pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE)
  # indicate significance level by stars
  pan <- get_p_stars(pval)
  # create mathematical term
  modsum <- as.character(as.expression(
    substitute(beta[0] == a * "," ~~ R^2 == r2 * "," ~~ "adj. " * R^2 == ar2 * "," ~~ "F" == f*panval * "," ~~ "AIC" == aic,
               list(a = format(coef(fit)[1], digits = 3),
                    r2 = format(summary(fit)$r.squared, digits = 3),
                    ar2 = format(summary(fit)$adj.r.squared, digits = 3),
                    f = sprintf("%.2f", fstat[1]),
                    panval = pan,
                    aic = sprintf("%.2f", stats::AIC(fit))))))
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
#' @importFrom stats varimax
varimaxrota <- function(data, factors) {
  # Faktorladungen berechnen
  # Die Faktorladungen erhält man durch Multiplikation der Eigenvektoren
  # mit der Diagonalmatrix der ausgewiesenen Standardabweichungen
  ladungen <- data$rotation %*% diag(data$sdev)
  # Zur Durchführung der VARIMAX-Rotation erzeugen wir eine Matrix
  # mit den Faktorladungen der ausgewählten Faktoren (Anzahl = Parameter "factors")
  # Varimax Rotation durchführen
  varib <- stats::varimax(ladungen[, seq_len(factors)])
  return(varib)
}


# unlist labels
# Help function that unlists a list into a vector
unlistlabels <- function(lab) {
  dummy <- unlist(lab)
  labels <- c()
  labels <- c(labels, as.character(dummy))
  return(labels)
}


get_model_response_label <- function(fit) {
  m_f <- stats::model.frame(fit)
  unname(sjmisc::get_label(m_f[[1]], def.value = colnames(m_f)[1]))
}



#' @importFrom stats reorder
sjp.vif <- function(fit) {
  # -----------------------------------
  # check package availability
  # -----------------------------------
  if (!requireNamespace("car", quietly = TRUE)) {
    stop("Package `car` needed for this function to work. Please install it.", call. = F)
  }
  vifval <- NULL
  vifplot <- NULL
  mydat <- NULL
  # check if we have more than 1 term
  if (length(coef(fit)) > 2) {
    # variance inflation factor
    # claculate VIF
    vifval <- car::vif(fit)
    if (is.matrix(vifval)) {
      val <- vifval[, 1]
    } else {
      val <- vifval
    }
    # retrieve highest VIF-value to determine y-axis range
    maxval <- val[which.max(val)]
    # determine upper limit of y-axis
    upperLimit <- 10
    # check whether maxval exceeds the critical VIF-Limit
    # of 10. If so, set upper limit to max. value
    if (maxval >= upperLimit) upperLimit <- ceiling(maxval)
    mydat <- data.frame(vif = round(val, 2))
    # Neue Variable erstellen, damit die Ergebnisse sortiert werden
    # können (siehe reorder in ggplot-Funktion)
    mydat$vars <- row.names(mydat)
    # die variablenlabel sollen noch mal sortiert werden, nach
    # VIF-Werten aufsteigend. Dies ist für die X-Achsenbeschriftung
    # nötig, da diese sonst nicht mehr mit den sortierten VIF-Werten
    # (Balkenreihenfolge auf X-Achse) übereinstimmt
    mydat <- cbind(mydat, mydat[order(val), 2])
    # Spalten sollen Namen kriegen
    names(mydat) <- c("vif", "vars", "label")
    # grafik ausgeben, dabei die variablen der X-Achse nach aufsteigenden
    # VIF-Werten ordnen
    vifplot <- ggplot(mydat, aes(x = stats::reorder(vars, vif), y = vif)) +
      # Balken zeichnen. Stat=identity heißt, dass nicht die counts, sondern
      # die tatsächlichen Zahlenwerte (VIF-Werte) abgebildet werden sollen
      geom_bar(stat = "identity", width = 0.7, fill = "#80acc8") +
      # grüne Linie zeichnen, die den guten Bereich anzeigt (VIF < 5)
      geom_hline(yintercept = 5, linetype = 2, colour = "darkgreen", alpha = 0.7) +
      # rote  Linie zeichnen, die den tolerablen Bereich anzeigt (VIF < 10)
      geom_hline(yintercept = 10, linetype = 2, colour = "darkred", alpha = 0.7) +
      # grüne und rote Line beschriften
      annotate("text", x = 1, y = 4.7, label = "good", size = 4, colour = "darkgreen") +
      annotate("text", x = 1, y = 9.7, label = "tolerable", size = 4, colour = "darkred") +
      # als X-Achsenbeschriftung die Variablennamen setzen
      scale_x_discrete(labels = mydat$label) +
      # Keine weiteren Titel an X- und Y-Achse angeben
      labs(title = "Variance Inflation Factors (multicollinearity)",
           x = NULL,
           y = NULL) +
      # maximale Obergrenze der Y-Achse setzen
      scale_y_continuous(limits = c(0, upperLimit), expand = c(0, 0)) +
      # Beschriftung der X-Achse (Variablenlabel) in 45-Grad-Winkel setzen
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, size = rel(1.2)))
    print(vifplot)
  }
  invisible(structure(class = "sjpvif",
                      list(plot = vifplot,
                           df = mydat,
                           vifval = vifval)))
}


sju.rmspc <- function(html.table) {
  cleaned <- gsub("      <", "<", html.table, fixed = TRUE, useBytes = TRUE)
  cleaned <- gsub("    <", "<", cleaned, fixed = TRUE, useBytes = TRUE)
  cleaned <- gsub("  <", "<", cleaned, fixed = TRUE, useBytes = TRUE)
  return(cleaned)
}


get_p_stars <- function(pval) {
  pan <- ""
  if (is.na(pval))
    pan <- ""
  else if (pval < 0.001)
    pan <- "***"
  else if (pval < 0.01)
    pan <- "**"
  else if (pval < 0.05)
    pan <- "*"
  return(invisible(pan))
}
