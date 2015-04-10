# Help-functions


# function to create pretty breaks
# for log-scales
base_breaks <- function(n = 10) {
  function(x) {
    axisTicks(log10(range(x, na.rm = TRUE)), log = TRUE, nint = n)
  }
}


# add annotations with table summary
# here we print out total N of cases, chi-square and significance of the table
print.table.summary <- function(baseplot,
                                modsum,
                                tableSummaryPos = "r") {
  if (!is.null(modsum)) {
    # add annotations with table summary
    # here we print out total N of cases, chi-square and significance of the table
    if (tableSummaryPos == "r") {
      t.hjust <- 1.05
      x.x <- Inf
    } else {
      t.hjust <- -0.05
      x.x <- -Inf
    }
    baseplot <- baseplot +
      annotate("text",
               label = modsum,
               parse = TRUE,
               x = x.x,
               y = Inf,
               vjust = 1.1,
               hjust = t.hjust)
  }
  return (baseplot)
}


# helper function to calculate probabilities for odds ratios
# see http://stats.stackexchange.com/questions/89474/interpretation-of-ordinal-logistic-regression,
# http://stats.stackexchange.com/questions/26288/understanding-odds-ratios-in-logistic-regression
# and http://pages.uoregon.edu/aarong/teaching/G4075_Outline/node16.html
odds.to.prob <- function(x) {
  # formular: probality = odds divided by (1+odds),
  # where odds = exp(logit)
  # x = log-odds (logits)
  return (1 / (1 + exp(-x)))
}


# display html-content in viewer pane
# or write it to file
out.html.table <- function(no.output, file, knitr, toWrite, useViewer) {
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
      if (useViewer && !is.null(viewer)) {
        viewer(htmlFile)
      } else {
        utils::browseURL(htmlFile)
      }
      # delete temp file
      # unlink(htmlFile)
    }
  }
}


# Create frequency data frame of a variable
# for sjp and sjt frq functions
create.frq.df <- function(varCount,
                          labels,
                          breakLabelsAt,
                          order.frq = "none",
                          round.prz = 4,
                          na.rm = FALSE,
                          startAxisAt = "auto",
                          weightBy = NULL) {
  #---------------------------------------------------
  # weight variable
  #---------------------------------------------------
  if (!is.null(weightBy)) varCount <- sjmisc::weight(varCount, weightBy)
  #---------------------------------------------------
  # create frequency data frame
  #---------------------------------------------------
  df <- as.data.frame(table(varCount))
  names(df) <- c("y", "Freq")
  # --------------------------------------------------------
  # Define amount of category, include zero counts
  # --------------------------------------------------------
  # Zero counts of categories are not plotted by default just becaus
  # these categories don't appear in the data. If we assume a
  # "quasi-continuous" scale (categories from 1 to 4 etc.), we now
  # identify the zero counts and add / insert them into the data frame.
  # This enables us to plot zero counts as well.
  # We guess the maximum amount of categories either by the amount
  # of supplied category labels. If no category labels were passed
  # as parameter, we assume that the maximum value found in the category
  # columns represents the highest category number
  catcount <- 0
  catmin <- minval <- min(varCount, na.rm = TRUE)
  # ----------------------------------------------
  # check for axis start, depending on lowest value
  # ----------------------------------------------
  if (startAxisAt == "auto") {
    startAxisAt <- as.numeric(catmin)
    if (startAxisAt == 0) startAxisAt <- 1
  }
  # Factors have to be transformed into numeric values
  # for continiuos x-axis-scale
  df$y <- sjmisc::to_value(df$y, keep.labels = F)
  # if categories start with zero, fix this here
  if (min(df$y) == 0 && startAxisAt > 0) df$y <- df$y + 1
  # get the highest answer category of "y", so we know where the
  # range of the x-axis ends
  if (!is.null(labels)) {
    catcount <- startAxisAt + length(labels) - 1
  } else {
    # determine maximum values
    # first, check the total amount of different factor levels
    catcount_1 <- length(unique(na.omit(varCount)))
    # second, check the maximum factor level
    catcount_2 <- max(varCount, na.rm = TRUE)
    # if categories start with zero, fix this here
    if (min(varCount, na.rm = TRUE) == 0) catcount_2 <- catcount_2 + 1
    # catcount should contain the higher values, i.e. the maximum count of
    # categories (factor levels) corresponds either to the highest factor level
    # value or to the amount of different factor levels, depending on which one
    # is larger
    catcount <- ifelse (catcount_1 > catcount_2, catcount_1, catcount_2)
  }
  # Create a vector of zeros
  frq <- rep(0, catcount)
  # Replace the values in freq for those indices which equal dummyf$xa
  # by dummyf$ya so that remaining indices are ones which you
  # intended to insert
  frq[df$y] <- df$Freq
  # create new data frame. We now have a data frame with all
  # variable categories abd their related counts, including
  # zero counts, but no(!) missings!
  mydat <- as.data.frame(cbind(var = startAxisAt:catcount,
                               frq = frq[startAxisAt:catcount]))
  # caculate missings here
  missingcount <- length(which(is.na(varCount)))
  if (!is.null(labels)) {
    labels <- sjmisc::word_wrap(labels, breakLabelsAt)
  } else {
    # If axisLabels.x were not defined, simply set numbers from 1 to
    # amount of categories (=number of rows) in dataframe instead
    if (is.null(labels)) labels <- c(startAxisAt:(nrow(mydat) + startAxisAt - 1))
  }
  # --------------------------------------------------------
  # Handle missings
  # --------------------------------------------------------
  # If missings are not removed, add an
  # "NA" to labels and a new row to data frame which contains the missings
  if (!na.rm) {
    labels  <- c(labels, "NA")
    mydat <- rbind(mydat, c(catcount + 1, missingcount))
    # also add a columns with percentage values of count distribution
    mydat <- data.frame(cbind(mydat, prz = c(round(100 * mydat$frq / length(varCount), round.prz))))
  } else {
    # also add a columns with percentage values of count distribution
    mydat <- data.frame(cbind(mydat, prz = c(round(100 * mydat$frq / length(na.omit(varCount)), round.prz))))
  }
  # --------------------------------------------------------
  # Order categories ascending or descending
  # --------------------------------------------------------
  if (!is.null(order.frq) && (order.frq == "asc" || order.frq == "desc")) {
    ord <- order(mydat$frq, decreasing = (order.frq == "desc"))
    mydat$frq <- mydat$frq[ord]
    mydat$prz <- mydat$prz[ord]
    labels <- labels[ord]
  }
  # --------------------------------------------------------
  # add valid and cumulative percentages
  # --------------------------------------------------------
  mydat$valid <- c(round(100 * mydat$frq / length(na.omit(varCount)), round.prz))
  mydat$cumperc <- cumsum(mydat$valid)
  # --------------------------------------------------------
  # check if all categories are in table. if first category does not
  # start with 1, insert a row at beginning. but only do this, if we have
  # more value labels than data frame rows (i.e. more categories are expected
  # than appear in the data frame)
  # --------------------------------------------------------
  dfc <- 1
  while (length(labels) > nrow(mydat) && as.numeric(mydat$var[dfc]) > dfc) {
    # insert "first" row which seems to be missing
    mydat <- rbind(rep(0, ncol(mydat)), mydat)
    # increase counter
    dfc <- dfc + 1
  }
  # check if we modified mydat
  if (dfc > 1) {
    # set var
    mydat$var <- c(1:nrow(mydat))
    if (catmin != min(as.numeric(mydat$var), na.rm = T)) {
      catmin <- min(as.numeric(mydat$var), na.rm = T)
    }
  }
  # -------------------------------------
  # return results
  # -------------------------------------
  invisible (structure(list(mydat = mydat,
                            labels = labels,
                            catmin = catmin,
                            minval = minval)))
}


# check character encoding for HTML-tables
# (sjt-functions)
get.encoding <- function(encoding) {
  if (is.null(encoding)) {
    if (.Platform$OS.type == "unix")
      encoding <- "UTF-8"
    else
      encoding <- "Windows-1252"
  }
  return (encoding)
}


# check whether a color value is indicating
# a color brewer palette
is.brewer.pal <- function(pal) {
  bp.seq <- c("BuGn", "BuPu", "GnBu", "OrRd", "PuBu", "PuBuGn", "PuRd", "RdPu",
              "YlGn", "YlGnBu", "YlOrBr", "YlOrRd", "Blues", "Greens", "Greys",
              "Oranges", "Purples", "Reds")
  bp.div <- c("BrBG", "PiYg", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu",
              "RdYlGn", "Spectral")
  bp.qul <- c("Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1",
              "Set2", "Set3")
  bp <- c(bp.seq, bp.div, bp.qul)
  return (any(bp == pal))
}


# Calculate statistics of cross tabs
crosstabsum <- function(ftab) {
  # --------------------------------------------------------
  # check p-value-style option
  # --------------------------------------------------------
  opt <- getOption("p_zero")
  if (is.null(opt) || opt == FALSE) {
    p_zero <- ""
  } else {
    p_zero <- "0"
  }
  # calculate chi square value
  chsq <- chisq.test(ftab)
  tab <- sjmisc::table_values(ftab)
  fish <- NULL
  # check whether variables are dichotome or if they have more
  # than two categories. if they have more, use Cramer's V to calculate
  # the contingency coefficient
  if (nrow(ftab) > 2 || ncol(ftab) > 2) {
    # if minimum expected values below 5, compute fisher's exact test
    if(min(tab$expected) < 5 || (min(tab$expected) < 10 && chsq$parameter == 1)) fish <- fisher.test(ftab, simulate.p.value = TRUE)
    # check whether fisher's test or chi-squared should be printed
    if (is.null(fish)) {
      if (chsq$p.value < 0.001) {
        modsum <- as.character(as.expression(
          substitute("N" == tn * "," ~~ chi^2 == c2 * "," ~~ "df" == dft * "," ~~ phi[c] == kook * "," ~~ "p" < pva,
                     list(tn = summary(ftab)$n.cases,
                          c2 = sprintf("%.2f", chsq$statistic),
                          dft = c(chsq$parameter),
                          kook = sprintf("%.2f", sjmisc::cramer(ftab)),
                          pva = sprintf("%s.001", p_zero)))))
      } else {
        modsum <- as.character(as.expression(
          substitute("N" == tn * "," ~~ chi^2 == c2 * "," ~~ "df" == dft * "," ~~ phi[c] == kook * "," ~~ "p" == pva,
                     list(tn = summary(ftab)$n.cases,
                          c2 = sprintf("%.2f", chsq$statistic),
                          dft = c(chsq$parameter),
                          kook = sprintf("%.2f", sjmisc::cramer(ftab)),
                          pva = sub("0", p_zero, sprintf("%.3f", chsq$p.value))))))
      }
    } else {
      if (fish$p.value < 0.001) {
        modsum <- as.character(as.expression(
          substitute("N" == tn * "," ~~ "df" == dft * "," ~~ phi[c] == kook * "," ~~ "Fisher's p" < pva,
                     list(tn = summary(ftab)$n.cases,
                          dft = c(chsq$parameter),
                          kook = sprintf("%.2f", sjmisc::cramer(ftab)),
                          pva = sprintf("%s.001", p_zero)))))
      } else {
        modsum <- as.character(as.expression(
          substitute("N" == tn * "," ~~ "df" == dft * "," ~~ phi[c] == kook * "," ~~ "Fisher's p" == pva,
                     list(tn = summary(ftab)$n.cases,
                          dft = c(chsq$parameter),
                          kook = sprintf("%.2f", sjmisc::cramer(ftab)),
                          pva = sub("0", p_zero, sprintf("%.3f", fish$p.value))))))
      }
    }
  # if variables have two categories (2x2 table), use phi to calculate
  # the degree of association
  } else {
    # if minimum expected values below 5, compute fisher's exact test
    if(min(tab$expected) < 5 || (min(tab$expected) < 10 && chsq$parameter == 1)) fish <- fisher.test(ftab)
    # check whether fisher's test or chi-squared should be printed
    if (is.null(fish)) {
      modsum <- as.character(as.expression(
        substitute("N" == tn * "," ~~ chi^2 == c2 * "," ~~ "df" == dft * "," ~~ phi == kook * "," ~~ "p" == pva,
                   list(tn = summary(ftab)$n.cases,
                        c2 = sprintf("%.2f", chsq$statistic),
                        dft = c(chsq$parameter),
                        kook = sprintf("%.2f", sjmisc::phi(ftab)),
                        pva = sub("0", p_zero, sprintf("%.3f", chsq$p.value))))))
    } else {
      modsum <- as.character(as.expression(
        substitute("N" == tn * "," ~~ "df" == dft * "," ~~ phi == kook * "," ~~ "Fisher's p" == pva,
                   list(tn = summary(ftab)$n.cases,
                        dft = c(chsq$parameter),
                        kook = sprintf("%.2f", sjmisc::phi(ftab)),
                        pva = sub("0", p_zero, sprintf("%.3f", fish$p.value))))))
    }
  }
  return (modsum)
}


# checks at which position in fitted models factors with
# more than two levels are located.
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
  for (k in 1:length(models)) {
    # get model
    fit <- models[[k]]
    # retrieve all factors from model
    for (grp.cnt in 1:ncol(fit$model)) {
      # get variable
      fit.var <- fit$model[, grp.cnt]
      # is factor? and has more than two levels?
      # (otherwise, only one category would appear in
      # coefficients, so no grouping needed anyway)
      if (is.factor(fit.var) && length(levels(fit.var)) > 2) {
        # get factor name
        fac.name <- colnames(fit$model)[grp.cnt]
        # check whether we already have this factor
        if (!any(found.factors == fac.name)) {
          # if not, save found factor variable name
          found.factors <- c(found.factors, fac.name)
          # save factor name
          lab <- unname(sjmisc::get_var_labels(fit.var))
          # any label?
          if (is.null(lab)) lab <- colnames(fit$model)[grp.cnt]
          # determins startindex
          index <- grp.cnt + add.index - 1
          index.add <- length(levels(fit.var)) - 2
          # save row index, so we know where to start group
          group.pred.rows <- c(group.pred.rows, index)
          group.pred.span <- c(group.pred.span, index:(index + index.add))
          group.pred.labs <- c(group.pred.labs, lab)
          # increase add.index by amount of factor levels (minus reference cat.)
          add.index <- add.index + index.add
        } else {
          add.index <- add.index + length(levels(fit.var)) - 2
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
      for (i in 1:length(group.pred.rows)) {
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
  return (list(group.pred.rows,
               group.pred.span,
               group.pred.labs))
}


# automatically retrieve predictor labels
# of fitted (g)lm
retrieveModelLabels <- function(models) {
  # do we have global options?
  opt <- getOption("autoSetVariableLabels")
  if (is.null(opt) || opt == TRUE) {
    fit.labels <- c()
    for (k in 1:length(models)) {
      # get model
      fit <- models[[k]]
      # iterate coefficients (1 is intercept or response)
      for (i in 2:ncol(fit$model)) {
        # is predictor a factor?
        pvar <- fit$model[, i]
        # if yes, we have this variable multiple
        # times, so manually set value labels
        if (is.factor(pvar)) {
          # get amount of levels
          pvar.len <- length(levels(pvar))
          # get value labels, if any
          pvar.lab <- sjmisc::get_val_labels(pvar)
          # have any labels, and have we same amount of labels
          # as factor levels?
          if (!is.null(pvar.lab) && length(pvar.lab) == pvar.len) {
            # add labels
            if (!any(fit.labels == pvar.lab[2:pvar.len])) {
              fit.labels <- c(fit.labels, pvar.lab[2:pvar.len])
            }
          } else {
            # add labels
            if (!any(fit.labels == attr(fit$coefficients[i], "names"))) {
              fit.labels <- c(fit.labels, attr(fit$coefficients[i], "names"))
            }
          }
        } else {
          # check if we hav label
          lab <- sjmisc:::autoSetVariableLabels(fit$model[, i])
          # if not, use coefficient name
          if (is.null(lab)) {
            lab <- attr(fit$coefficients[i], "names")
          }
          if (!any(fit.labels == lab)) fit.labels <- c(fit.labels, lab)
        }
      }
    }
    return (fit.labels)
  }
  return (NULL)
}


# compute pseudo r-square for glm
PseudoR2 <- function(rr) { # rr must be the result of lm/glm
  n <- nrow(rr$model)
  COX <- (1 - exp((rr$deviance - rr$null) / n))
  NR <- COX / (1 - exp(-rr$null / n))
  RVAL <- c(N = n, CoxSnell = COX, Nagelkerke = NR)
  return(RVAL)
}


# compute chi-square for glm
Chisquare.glm <- function(rr, digits=3) {
  return (with(rr, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE), digits = digits))
}


# compute model statistics for lm
sju.modsum.lm <- function(fit) {
  # get F-statistics
  fstat <- summary(fit)$fstatistic
  # Calculate p-value for F-test
  pval <- pf(fstat[1], fstat[2], fstat[3],lower.tail = FALSE)
  # indicate significance level by stars
  pan <- c("")
  if (pval < 0.001) {
    pan <- c("***")
  } else  if (pval < 0.01) {
    pan <- c("**")
  } else  if (pval < 0.05) {
    pan <- c("*")
  }
  # create mathematical term
  modsum <- as.character(as.expression(
    substitute(beta[0] == a * "," ~~ R^2 == r2 * "," ~~ "adj. " * R^2 == ar2 * "," ~~ "F" == f*panval * "," ~~ "AIC" == aic,
               list(a=format(coef(fit)[1], digits=3),
                    r2=format(summary(fit)$r.squared, digits=3),
                    ar2=format(summary(fit)$adj.r.squared, digits=3),
                    f=sprintf("%.2f", fstat[1]),
                    panval=pan,
                    aic=sprintf("%.2f", AIC(fit))))))
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
  ladb <- c()
  for (i in 1:factors) {
    ladb <- cbind(ladb, ladungen[, i])
  }
  # Varimax Rotation durchführen
  varib <- varimax(ladb)
  return (varib)
}


# unlist labels
# Help function that unlists a list into a vector
unlistlabels <- function(lab) {
  dummy <- unlist(lab)
  labels <- c()
  labels <- c(labels, as.character(dummy))
  return (labels)
}


#' @title Adjust y range of ggplot-objects
#' @name adjust_plot_range
#'
#' @description This method adjusts the y-range of a ggplot-object, which is useful when
#'                value labels are outside of the plot region. A modified ggplot-object will
#'                be returned with adjusted y-range so everything should be visible.
#'                Note that this function only works on \code{scale_y_continuous}.
#'
#' @note Note that this function only works on \code{scale_y_continuous}.
#'
#' @references \href{http://www.r-bloggers.com/setting-axis-limits-on-ggplot-charts/}{r-bloggers.com}
#'
#' @param gp A ggplot-object. Usually, this will be returned by most of this
#'          package's plotting functions.
#' @param upperMargin Defines the margin of the upper y bound of the plot. This value will
#'          be multiplied with the total y range. Default is 1.05, which means that the upper
#'          margin of the plot is about 5 percent of the "visible" plot area (i.e. the y-range
#'          is 105 percent of the actual needed range to make all object visible).
#' @return The same ggplot-object, with adjusted y-range, so all graphics and labels
#'          should be visible.
#'
#' @examples
#' # sample data set
#' library(sjmisc)
#' data(efc)
#' # show frequencies of relationship-variable and
#' # retrieve plot object
#' gp <- sjp.frq(efc$e15relat, printPlot=FALSE)
#' # show current plot
#' plot(gp$plot)
#' # show adjusted plot
#' adjust_plot_range(gp$plot)
#'
#' @import ggplot2
#' @export
adjust_plot_range <- function(gp, upperMargin=1.05) {
  # retrieve y-range of original plot
  gp <- gp + scale_y_continuous(limits = NULL)
  # build ggplot object
  gy <- ggplot_build(gp)
  # calculate new limit
  ylo <- abs(gy$panel$ranges[[1]]$y.range[1])
  yhi <- abs(gy$panel$ranges[[1]]$y.range[2] * upperMargin)
  # change y scale
  gp <- gp + scale_y_continuous(expand = c(0, 0),
                                limits = c(0, ylo + yhi))
  # return plot
  return(gp)
}


sjp.vif <- function(fit) {
  vifval <- NULL
  # check if we have more than 1 term
  if (length(coef(fit)) > 2) {
    # variance inflation factor
    # claculate VIF
    vifval <- vif(fit)
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
    mydat <- data.frame(cbind(round(val, 2)))
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
    plot(ggplot(mydat, aes(x = reorder(vars, vif), y = vif)) +
           # Balken zeichnen. Stat=identity heißt, dass nicht die counts, sondern
           # die tatsächlichen Zahlenwerte (VIF-Werte) abgebildet werden sollen
           geom_bar(stat="identity", width=0.7, fill="#80acc8") +
           # grüne Linie zeichnen, die den guten Bereich anzeigt (VIF < 5)
           geom_hline(yintercept=5, linetype=2, colour="darkgreen", alpha=0.7) +
           # rote  Linie zeichnen, die den tolerablen Bereich anzeigt (VIF < 10)
           geom_hline(yintercept=10, linetype=2, colour="darkred", alpha=0.7) +
           # grüne und rote Line beschriften
           annotate("text", x=1, y=4.7, label="good", size=4, colour="darkgreen") +
           annotate("text", x=1, y=9.7, label="tolerable", size=4, colour="darkred") +
           # als X-Achsenbeschriftung die Variablennamen setzen
           scale_x_discrete(labels=mydat$label) +
           # Keine weiteren Titel an X- und Y-Achse angeben
           labs(title="Variance Inflation Factors (multicollinearity)", x=NULL, y=NULL) +
           # maximale Obergrenze der Y-Achse setzen
           scale_y_continuous(limits=c(0, upperLimit), expand=c(0,0)) +
           # Beschriftung der X-Achse (Variablenlabel) in 45-Grad-Winkel setzen
           theme(axis.text.x=element_text(angle=45, vjust=0.5, size=rel(1.2))))
  }
  invisible(vifval)
}


# helper function to compute absolute and relative
# confidence intervals
sjs.frqci <- function(x) {
  ft <- as.numeric(unname(table(x)))
  n <- sum(ft, na.rm = T)
  rel_frq <- as.numeric(ft/n)
  ci <- 1.96 * sqrt(rel_frq * (1 - rel_frq)/n)
  ci.u <- n * (rel_frq + ci)
  ci.l <- n * (rel_frq - ci)
  rel.ci.u <- rel_frq + ci
  rel.ci.l <- rel_frq - ci
  mydat.frq <- data.frame(frq = ft,
                          lower.ci = ci.l,
                          upper.ci = ci.u)
  mydat.rel <- data.frame(rel.frq = rel_frq,
                          rel.lower.ci = rel.ci.l,
                          rel.upper.ci = rel.ci.u)

  invisible (structure(class = "sjs.frqci",
                       list(mydat.frq = mydat.frq,
                            mydat.rel = mydat.rel)))
}


sju.rmspc <- function(html.table) {
  cleaned <- gsub("      <", "<", html.table, fixed = TRUE)
  cleaned <- gsub("    <", "<", cleaned, fixed = TRUE)
  cleaned <- gsub("  <", "<", cleaned, fixed = TRUE)
  return (cleaned)
}
