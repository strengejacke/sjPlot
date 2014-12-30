# -------------------------------------
# Help-functions
# -------------------------------------


# -------------------------------------
# function to create pretty breaks
# for log-scales
# -------------------------------------
base_breaks <- function(n = 10) {
  function(x) {
    axisTicks(log10(range(x, na.rm = TRUE)), log = TRUE, nint = n)
  }
}


# -------------------------------------
# add annotations with table summary
# here we print out total N of cases, chi-square and significance of the table
# -------------------------------------
print.table.summary <- function(baseplot, 
                                modsum,
                                tableSummaryPos = "r") {
  if (!is.null(modsum)) {
    # add annotations with table summary
    # here we print out total N of cases, chi-square and significance of the table
    if (tableSummaryPos == "r") {
      t.hjust <- 1.05
      x.x <- Inf
    }
    else {
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


# -------------------------------------
# helper function to calculate probabilities for odds ratios
# see http://stats.stackexchange.com/questions/89474/interpretation-of-ordinal-logistic-regression,
# http://stats.stackexchange.com/questions/26288/understanding-odds-ratios-in-logistic-regression
# and http://pages.uoregon.edu/aarong/teaching/G4075_Outline/node16.html
# -------------------------------------
odds.to.prob <- function(x) {
  # formular: probality = odds divided by (1+odds),
  # where odds = exp(logit)
  # return(exp(x)/(1+exp(x)))
  return (1/(1+exp(-x)))
}


# -------------------------------------
# display html-content in viewer pane
# or write it to file
# -------------------------------------
out.html.table <- function(no.output, file, knitr, toWrite, useViewer) {
  if (!no.output) {
    # -------------------------------------
    # check if we have filename specified
    # -------------------------------------
    if (!is.null(file)) {
      # write file
      write(knitr, file = file)
    }
    # -------------------------------------
    # else open in viewer pane
    # -------------------------------------
    else {
      # else create and browse temporary file
      htmlFile <- tempfile(fileext = ".html")
      write(toWrite, file = htmlFile)
      # check whether we have RStudio Viewer
      viewer <- getOption("viewer")
      if (useViewer && !is.null(viewer)) {
        viewer(htmlFile)
      }
      else {
        utils::browseURL(htmlFile)    
      }
      # delete temp file
      # unlink(htmlFile)
    }
  }
}
# -------------------------------------
# Create frequency data frame of a variable
# for sjp and sjt frq functions
# -------------------------------------
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
  if (!is.null(weightBy)) {
    varCount <- sju.weight(varCount, weightBy)
  }
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
  catmin <- minval <- min(varCount, na.rm=TRUE)
  # ----------------------------------------------
  # check for axis start, depending on lowest value
  # ----------------------------------------------
  if (startAxisAt=="auto") {
    startAxisAt <- as.numeric(catmin)
    if (startAxisAt==0) startAxisAt <- 1
  }
  # Factors have to be transformed into numeric values
  # for continiuos x-axis-scale
  df$y <- as.numeric(as.character(df$y))
  # if categories start with zero, fix this here
  if (min(df$y)==0 && startAxisAt>0) {
    df$y <- df$y+1
  }
  # get the highest answer category of "y", so we know where the
  # range of the x-axis ends
  if (!is.null(labels)) {
    catcount <- startAxisAt + length(labels) - 1
  }
  else {
    # determine maximum values
    # first, check the total amount of different factor levels
    catcount_1 <- length(unique(na.omit(varCount)))
    # second, check the maximum factor level
    catcount_2 <- max(varCount, na.rm=TRUE)
    # if categories start with zero, fix this here
    if (min(varCount, na.rm=TRUE)==0) {
      catcount_2 <- catcount_2+1
    }
    # catcount should contain the higher values, i.e. the maximum count of
    # categories (factor levels) corresponds either to the highest factor level
    # value or to the amount of different factor levels, depending on which one
    # is larger
    catcount <- ifelse (catcount_1 > catcount_2, catcount_1, catcount_2)
  }
  # Create a vector of zeros 
  frq <- rep(0,catcount)
  # Replace the values in freq for those indices which equal dummyf$xa
  # by dummyf$ya so that remaining indices are ones which you 
  # intended to insert 
  frq[df$y] <- df$Freq
  # create new data frame. We now have a data frame with all
  # variable categories abd their related counts, including
  # zero counts, but no(!) missings!
  mydat <- as.data.frame(cbind(var=startAxisAt:catcount, frq=frq[startAxisAt:catcount]))
  # caculate missings here
  missingcount <- length(which(is.na(varCount)))
  if (!is.null(labels)) {
    labels <- sju.wordwrap(labels, breakLabelsAt)    
  }
  # If axisLabels.x were not defined, simply set numbers from 1 to
  # amount of categories (=number of rows) in dataframe instead
  else  {
    if (is.null(labels)) labels <- c(startAxisAt:(nrow(mydat)+startAxisAt-1))
  }
  # --------------------------------------------------------
  # Handle missings
  # --------------------------------------------------------
  # If missings are not removed, add an
  # "NA" to labels and a new row to data frame which contains the missings
  if (!na.rm) {
    labels  <- c(labels, "NA")
    mydat <- rbind(mydat, c(catcount+1, missingcount))
    # also add a columns with percentage values of count distribution
    mydat <- data.frame(cbind(mydat, prz = c(round(100*mydat$frq/length(varCount),round.prz))))
  }
  else {
    # also add a columns with percentage values of count distribution
    mydat <- data.frame(cbind(mydat, prz = c(round(100*mydat$frq/length(na.omit(varCount)),round.prz))))
  }
  # --------------------------------------------------------
  # Order categories ascending or descending
  # --------------------------------------------------------
  if (!is.null(order.frq) && (order.frq=="asc" || order.frq=="desc")) {
    ord <- order(mydat$frq, decreasing=(order.frq=="desc"))
    mydat$frq <- mydat$frq[ord]
    mydat$prz <- mydat$prz[ord]
    labels <- labels[ord]
  }
  # --------------------------------------------------------
  # add valid and cumulative percentages
  # --------------------------------------------------------
  mydat$valid <- c(round(100*mydat$frq/length(na.omit(varCount)),round.prz))
  mydat$cumperc <- cumsum(mydat$valid)
  # --------------------------------------------------------
  # check if all categories are in table. if first category does not
  # start with 1, insert a row at beginning. but only do this, if we have
  # more value labels than data frame rows (i.e. more categories are expected
  # than appear in the data frame)
  # --------------------------------------------------------
  dfc <- 1
  while (length(labels)>nrow(mydat) && as.numeric(mydat$var[dfc])>dfc) {
    # insert "first" row which seems to be missing
    mydat <- rbind(rep(0, ncol(mydat)), mydat)
    # increase counter
    dfc <- dfc+1
  }
  # check if we modified ,ydat
  if (dfc>1) {
    # set var
    mydat$var <- c(1:nrow(mydat))
    if (catmin!=min(as.numeric(mydat$var), na.rm=T)) {
      catmin <- min(as.numeric(mydat$var), na.rm=T)
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


# -------------------------------------
# check character encoding for HTML-tables
# (sjt-functions)
# -------------------------------------
get.encoding <- function(encoding) {
  if (is.null(encoding)) {
    if (.Platform$OS.type=="unix") encoding <- "UTF-8" else encoding <- "Windows-1252"
  }
  return (encoding)
}


# -------------------------------------
# check whether a color value is indicating
# a color brewer palette
# -------------------------------------
is.brewer.pal <- function(pal) {
  bp.seq <- c("BuGn", "BuPu", "GnBu", "OrRd", "PuBu", "PuBuGn", "PuRd", "RdPu", 
              "YlGn", "YlGnBu", "YlOrBr", "YlOrRd", "Blues", "Greens", "Greys",
              "Oranges", "Purples", "Reds")
  bp.div <- c("BrBG", "PiYg", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", 
              "RdYlGn", "Spectral")
  bp.qul <- c("Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1",
              "Set2", "Set3")
  
  bp <- c(bp.seq, bp.div, bp.qul)
  
  return (any(bp==pal))
}

# -------------------------------------
# Calculate statistics of cross tabs
# -------------------------------------
crosstabsum <- function(ftab) {
  # calculate chi square value
  chsq <- chisq.test(ftab)
  tab <- sjs.table.values(ftab)
  fish <- NULL
  # check whether variables are dichotome or if they have more
  # than two categories. if they have more, use Cramer's V to calculate
  # the contingency coefficient
  if (nrow(ftab)>2 || ncol(ftab)>2) {
    # if minimum expected values below 5, compute fisher's exact test
    if(min(tab$expected)<5 || (min(tab$expected)<10 && chsq$parameter==1)) fish <- fisher.test(ftab, simulate.p.value=TRUE)
    # check whether fisher's test or chi-squared should be printed
    if (is.null(fish)) {
      if (chsq$p.value < 0.001) {
        modsum <- as.character(as.expression(
          substitute("N" == tn * "," ~~ chi^2 == c2 * "," ~~ "df" == dft * "," ~~ phi[c] == kook * "," ~~ "p" < pva,
                     list(tn=summary(ftab)$n.cases,
                          c2=sprintf("%.2f", chsq$statistic),
                          dft=c(chsq$parameter),
                          kook=sprintf("%.2f", sjs.cramer(ftab)),
                          pva=0.001))))
      }
      else {
        modsum <- as.character(as.expression(
          substitute("N" == tn * "," ~~ chi^2 == c2 * "," ~~ "df" == dft * "," ~~ phi[c] == kook * "," ~~ "p" == pva,
                     list(tn=summary(ftab)$n.cases,
                          c2=sprintf("%.2f", chsq$statistic),
                          dft=c(chsq$parameter),
                          kook=sprintf("%.2f", sjs.cramer(ftab)),
                          pva=sprintf("%.3f", chsq$p.value)))))
      }
    }
    else {
      if (fish$p.value < 0.001) {
        modsum <- as.character(as.expression(
          substitute("N" == tn * "," ~~ "df" == dft * "," ~~ phi[c] == kook * "," ~~ "Fisher's p" < pva,
                     list(tn=summary(ftab)$n.cases,
                          dft=c(chsq$parameter),
                          kook=sprintf("%.2f", sjs.cramer(ftab)),
                          pva=0.001))))
      }
      else {
        modsum <- as.character(as.expression(
          substitute("N" == tn * "," ~~ "df" == dft * "," ~~ phi[c] == kook * "," ~~ "Fisher's p" == pva,
                     list(tn=summary(ftab)$n.cases,
                          dft=c(chsq$parameter),
                          kook=sprintf("%.2f", sjs.cramer(ftab)),
                          pva=sprintf("%.3f", fish$p.value)))))
      }
    }
  }
  # if variables have two categories (2x2 table), use phi to calculate
  # the degree of association
  else {
    # if minimum expected values below 5, compute fisher's exact test
    if(min(tab$expected)<5 || (min(tab$expected)<10 && chsq$parameter==1)) fish <- fisher.test(ftab)
    # check whether fisher's test or chi-squared should be printed
    if (is.null(fish)) {
      modsum <- as.character(as.expression(
        substitute("N" == tn * "," ~~ chi^2 == c2 * "," ~~ "df" == dft * "," ~~ phi == kook * "," ~~ "p" == pva,
                   list(tn=summary(ftab)$n.cases,
                        c2=sprintf("%.2f", chsq$statistic),
                        dft=c(chsq$parameter),
                        kook=sprintf("%.2f", sjs.phi(ftab)),
                        pva=sprintf("%.3f", chsq$p.value)))))
    }
    else {
      modsum <- as.character(as.expression(
        substitute("N" == tn * "," ~~ "df" == dft * "," ~~ phi == kook * "," ~~ "Fisher's p" == pva,
                   list(tn=summary(ftab)$n.cases,
                        dft=c(chsq$parameter),
                        kook=sprintf("%.2f", sjs.phi(ftab)),
                        pva=sprintf("%.3f", fish$p.value)))))
    }
  }  
  return (modsum)
}


# -------------------------------------
# automatically set labels of values,
# if attributes are present
# -------------------------------------
autoSetValueLabels <- function(x) {
  # check if we have value label attribut
  vl <- attr(x, "value.labels")
  lv <- levels(x)
  label <- NULL
  # check  if we have value labels
  if (!is.null(vl) && length(vl)>0) {
    label <- rev(names(vl))
  }
  # check  if we have factor levels
  else if (!is.null(lv)) {
    label <- lv
  }
  # if we have string values, copy them as labels
  else if (is.character(x)) {
    label <- unique(x)
  }
  return(label)
}


# -------------------------------------
# automatically set labels of variables,
# if attributes are present
# -------------------------------------
autoSetVariableLabels <- function(x) {
  # check if we have variable label attribut
  vl <- as.vector(attr(x, "variable.label"))
  label <- NULL
  # check if we have variable labels
  if (!is.null(vl) && length(vl)>0) {
    label <- vl
  }
  return(label)
}


# -------------------------------------
# compute pseudo r-square for glm
# -------------------------------------
PseudoR2 <- function(rr) { # rr must be the result of lm/glm
  n <- nrow(rr$model)
  COX <- (1-exp((rr$deviance-rr$null)/n))
  NR <- COX/(1-exp(-rr$null/n))
  RVAL <- c(N=n, CoxSnell=COX, Nagelkerke=NR)
  return(RVAL)
}


# -------------------------------------
# compute chi-square for glm
# -------------------------------------
Chisquare.glm <- function(rr, digits=3) {
  return (with(rr, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE), digits=digits))
}


# -------------------------------------
# compute model statistics for lm
# -------------------------------------
sju.modsum.lm <- function(fit) {
  # get F-statistics
  fstat <- summary(fit)$fstatistic
  # Calculate p-value for F-test
  pval <- pf(fstat[1], fstat[2], fstat[3],lower.tail = FALSE)
  # indicate significance level by stars
  pan <- c("")
  if (pval<=0.001) {
    pan <- c("***")
  }
  else  if (pval<=0.01) {
    pan <- c("**")
  }
  else  if (pval<=0.05) {
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
# --------------------------------------------------------
# Erzeugt eine rotierte Faktorladungen einer Hauptkomponentenanalyse
# (Paramter "data") mit einer bestimmten Anzahl an Faktoren (Parameter "factors")
# auf Grundlage der Varimax-Rotation
#
# Parameter:
# - data: the results (object) from a principal component analysis
#         (prcomp(myData...))
# - factors: the amount of factors. can be calculated from the
#            below function "factorcount"
# --------------------------------------------------------
varimaxrota <- function(data, factors) {
  # Faktorladungen berechnen
  # Die Faktorladungen erhält man durch Multiplikation der Eigenvektoren
  # mit der Diagonalmatrix der ausgewiesenen Standardabweichungen
  ladungen <- data$rotation%*%diag(data$sdev)
  # Zur Durchführung der VARIMAX-Rotation erzeugen wir eine Matrix
  # mit den Faktorladungen der ausgewählten Faktoren (Anzahl = Parameter "factors")
  ladb <- c()
  for (i in 1:factors) {
    ladb <- cbind(ladb, ladungen[,i])
  }
  # Varimax Rotation durchführen
  varib <- varimax(ladb)
  return (varib)
}


# --------------------------------------------------------
# unlist labels
# Help function that unlists a list into a vector
# --------------------------------------------------------
unlistlabels <- function(lab) {
  dummy <- unlist(lab)
  labels <- c()
  for (i in 1:length(dummy)) {
    labels <- c(labels, as.character(dummy[i]))
  }
  return (labels)
}
