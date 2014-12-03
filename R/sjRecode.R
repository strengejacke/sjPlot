#' @title Dichotomize variables
#' @name sju.dicho
#' 
#' @description Dichotomizes variables into dummy variables (0/1). Dichotomization is
#'                either done by median or mean (see \code{dichBy}).
#'
#' @param var The variable that should be dichotomized.
#' @param dichBy Indicates the split criterion where the variable is dichotomized. By default,
#'          \code{var} is split into two groups at the median (\code{dichBy="median"} or
#'          \code{dichBy="md"}). Further values for \code{dichBy} are \code{"mean"} (or \code{"m"}),
#'          which splits into groups at the mean of \code{var}; and \code{"value"} (or \code{"v"}).
#'          In the latter case, you have to specifiy \code{dichVal}.
#' @param dichVal Indicates a value where \code{var} is dichotomized when \code{dichBy="value"}.
#'          Note that \code{dichVal} is inclusive, i.e. \code{dichVal=10} will split \code{var}
#'          into one group with values from lowest to 10 and another group with values greater
#'          than 10.
#' @param asNum If \code{TRUE}, return value will be numeric, not a factor.
#' @return A dichotomized factor (or numeric, if \code{asNum = TRUE} variable (0/1-coded).
#' 
#' @examples
#' data(efc)
#' summary(efc$c12hour)
#' table(sju.dicho(efc$c12hour))
#' table(sju.dicho(efc$c12hour, "mean"))
#' table(sju.dicho(efc$c12hour, "value", 30))
#'  
#' @export
sju.dicho <- function(var, dichBy="median", dichVal=-1, asNum = FALSE) {
  # check abbreviations
  if (dichBy=="md") dichBy <- "median"
  if (dichBy=="m") dichBy <- "mean"
  if (dichBy=="v") dichBy <- "value"
  # check if factor
  if (is.factor(var)) {
    # try to convert to numeric
    var <- as.numeric(as.character(var))
  }
  # check for correct dichotome types
  if (dichBy!="median" && dichBy!="mean" && dichBy!="value") {
    stop("Parameter \"dichBy\" must either be \"median\", \"mean\" or \"value\"..." , call.=FALSE)
  }
  if (dichBy=="median") {
    var <- ifelse(var<=median(var, na.rm=T),0,1)
  }
  else if (dichBy=="mean") {
    var <- ifelse(var<=mean(var, na.rm=T),0,1)
  }
  else {
    var <- ifelse(var<=dichVal,0,1)
  }
  if (!asNum) var <- as.factor(var)
  return(var)
}


#' @title Recode count variables into grouped factors
#' @name sju.groupVar
#' 
#' @description Recode count variables into grouped factors.
#' 
#' @seealso \code{\link{sju.groupVarLabels}}
#'
#' @param var The count variable, which should recoded into groups.
#' @param groupsize The group-size, i.e. the range for grouping. By default, for each 5 categories 
#'          a new group is defined, i.e. \code{groupsize=5}. Use \code{groupsize="auto"} to automatically
#'          resize a variable into a maximum of 30 groups (which is the ggplot-default grouping when
#'          plotting histograms). Use \code{autoGroupCount} to determin the amount of groups.
#' @param asNumeric If \code{TRUE} (default), the recoded variable will be returned as numeric vector.
#'          If \code{FALSE}, a factor is returned.
#' @param rightInterval If \code{TRUE}, grouping starts with the lower bound of \code{groupsize}. In this
#'          case, groups cover the ranges from 50-54, 55-59, 60-64 etc. \cr
#'          If \code{FALSE} (default), grouping starts with the upper bound of \code{groupsize}. In this
#'          case, groups cover the ranges from 51-55, 56-60, 61-65 etc.
#' @param autoGroupCount Sets the maximum number of groups that are defined when auto-grouping is on
#'          (\code{groupsize="auto"}). Default is 30. If \code{groupsize} is not set to \code{"auto"},
#'          this parameter will be ignored.

#' @return A grouped variable, either as numeric or as factor (see paramter \code{asNumeric}).
#' 
#' @examples
#' age <- abs(round(rnorm(100, 65, 20)))
#' age.grp <- sju.groupVar(age, 10)
#' hist(age)
#' hist(age.grp)
#' 
#' # histogram with EUROFAMCARE sample dataset
#' # variable not grouped
#' data(efc)
#' efc.val <- sji.getValueLabels(efc)
#' efc.var <- sji.getVariableLabels(efc)
#' sjp.frq(efc$e17age,
#'         title=efc.var[['e17age']],
#'         type="h",
#'         showValueLabels=FALSE)
#' 
#' # bar plot with EUROFAMCARE sample dataset
#' # grouped variable
#' data(efc)
#' efc.val <- sji.getValueLabels(efc)
#' efc.var <- sji.getVariableLabels(efc)
#' ageGrp <- sju.groupVar(efc$e17age)
#' ageGrpLab <- sju.groupVarLabels(efc$e17age)
#' sjp.frq(ageGrp,
#'         title=efc.var[['e17age']],
#'         axisLabels.x=ageGrpLab,
#'         maxYlim=FALSE)
#'  
#' @export
sju.groupVar <- function(var, groupsize=5, asNumeric=TRUE, rightInterval=FALSE, autoGroupCount=30) {
  # minimum range. will be changed when autogrouping
  minval <- 0
  multip <- 2
  # check for auto-grouping
  if (groupsize=="auto") {
    # determine groupsize, which is 1/30 of range
    size <- ceiling((max(var, na.rm=TRUE)-min(var, na.rm=TRUE))/autoGroupCount)
    # reset groupsize var
    groupsize <- as.numeric(size)
    # change minvalue
    minval <- min(var, na.rm=TRUE)
    multip <- 1
  }
  # Einteilung der Variablen in Gruppen. Dabei werden unbenutzte Faktoren gleich entfernt
  var <- droplevels(cut(var, breaks=c(seq(minval, max(var, na.rm=TRUE)+multip*groupsize, by=groupsize)), right=rightInterval))
  # Die Level der Gruppierung wird neu erstellt
  levels(var) <- c(1:length(levels(var)))
  # in numerisch umwandeln
  if (asNumeric) {
    var <- as.numeric(as.character(var))  
  }
  return (var)
}

#' @title Create labels for recoded groups
#' @name sju.groupVarLabels
#' 
#' @description Creates the related labels for the grouped variable created by
#'                the \code{\link{sju.groupVar}} function.
#'                
#' @seealso \code{\link{sju.groupVar}}
#' 
#' @note Usually you should use the same values for \code{groupsize} and
#'         \code{rightInterval} as used in the \code{\link{sju.groupVar}} function
#'         if you want to create labels for the related recoded variable.
#'         
#' @param var The scale variable, which should recoded into groups.
#' @param groupsize The group-size, i.e. the range for grouping. By default, for each 5 categories 
#'          new group is built, i.e. \code{groupsize=5}. Use \code{groupsize="auto"} to automatically
#'          resize a variable into a maximum of 30 groups (which is the ggplot-default grouping when
#'          plotting histograms). Use parameter \code{autoGroupCount} to define the amount of groups.
#' @param rightInterval If \code{TRUE}, grouping starts with the lower bound of \code{groupsize}. In this
#'          case, groups cover the ranges from 50-54, 55-59, 60-64 etc. \cr
#'          If \code{FALSE} (default), grouping starts with the upper bound of \code{groupsize}. In this
#'          case, groups cover the ranges from 51-55, 56-60, 61-65 etc.
#' @param autoGroupCount Sets the maximum number of groups that are built when auto-grouping is on
#'          (\code{groupsize="auto"}). Default is 30. If \code{groupsize} is not set to \code{"auto"},
#'          this parameter will be ignored.
#' 
#' @return A string vector containing labels based on the grouped counts of \code{var},
#'           formatted as "from lower bound to upper bound", e.g. \code{"10-19"  "20-29"  "30-39"} etc.
#'           See example below.
#' 
#' @examples
#' age <- abs(round(rnorm(100, 65, 20)))
#' age.grp <- sju.groupVar(age, 10)
#' hist(age)
#' hist(age.grp)
#' 
#' age.grpvar <- sju.groupVarLabels(age, 10)
#' table(age.grp)
#' print(age.grpvar)
#' 
#' # histogram with EUROFAMCARE sample dataset
#' # variable not grouped
#' data(efc)
#' efc.val <- sji.getValueLabels(efc)
#' efc.var <- sji.getVariableLabels(efc)
#' sjp.frq(efc$e17age,
#'         title=efc.var[['e17age']],
#'         type="h",
#'         showValueLabels=FALSE)
#' 
#' # bar plot with EUROFAMCARE sample dataset
#' # grouped variable
#' data(efc)
#' efc.val <- sji.getValueLabels(efc)
#' efc.var <- sji.getVariableLabels(efc)
#' ageGrp <- sju.groupVar(efc$e17age)
#' ageGrpLab <- sju.groupVarLabels(efc$e17age)
#' sjp.frq(ageGrp,
#'         title=efc.var[['e17age']],
#'         axisLabels.x=ageGrpLab,
#'         maxYlim=FALSE)
#' 
#' @export
sju.groupVarLabels <- function(var, groupsize=5, rightInterval=FALSE, autoGroupCount=30) {
  # minimum range. will be changed when autogrouping
  minval <- 0
  multip <- 2
  # check for auto-grouping
  if (groupsize=="auto") {
    # determine groupsize, which is 1/30 of range
    size <- ceiling((max(var, na.rm=TRUE)-min(var, na.rm=TRUE))/autoGroupCount)
    # reset groupsize var
    groupsize <- as.numeric(size)
    # change minvalue
    minval <- min(var, na.rm=TRUE)
    multip <- 1
  }
  # Einteilung der Variablen in Gruppen. Dabei werden unbenutzte Faktoren gleich entfernt
  var <- droplevels(cut(var,breaks=c(seq(minval, max(var, na.rm=TRUE)+multip*groupsize, by=groupsize)), right=rightInterval))
  # Gruppen holen
  lvl <- levels(var) 
  # rückgabewert init
  retval <- rep(c(""), length(lvl))
  # alle Gruppierungen durchgehen
  for (i in 1:length(lvl)) {
    # Länge jedes Labels der Gruppeneinteilungen auslesen
    sublength <- nchar(lvl[i])
    # "(" und "]", das bei "cut"-Funktion automatisch erstellt wird, aus dem Label entfernen
    lvlstr <- substr(lvl[i], 2, sublength-1)
    # Unter- und Obergrenze in jeweils einem string
    subs <- strsplit(lvlstr, ",")
    # Untergrenze als Zahlenwert
    lower <- as.numeric(subs[[1]][1])
    # Obergrenze als Zahlenwert
    upper <- as.numeric(subs[[1]][2])
    # Prüfen, welche Intervallgrenze ein- und welche ausgeschlossen werden soll
    if(rightInterval) {
      lower <- lower+1
    }
    else {
      upper <- upper-1
    }
    # Rückgabe des Strings
    retval[i] <- c(paste(c(lower), "-", c(upper), sep=""))
  }
  return (c(retval))
}


#' @title Adjust y range of ggplot-objects
#' @name sju.adjustPlotRange.y
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
#' data(efc)
#' # show frequencies of relationship-variable and
#' # retrieve plot object
#' gp <- sjp.frq(efc$e15relat, printPlot=FALSE)
#' # show current plot
#' plot(gp$plot)
#' # show adjusted plot
#' sju.adjustPlotRange.y(gp$plot)
#' 
#' @export
sju.adjustPlotRange.y <- function(gp, upperMargin=1.05) {
  # retrieve y-range of original plot
  gp <- gp + scale_y_continuous(limits=NULL)
  # build ggplot object
  gy <- ggplot_build(gp)
  # calculate new limit
  ylo <- abs(gy$panel$ranges[[1]]$y.range[1])
  yhi <- abs(gy$panel$ranges[[1]]$y.range[2]*upperMargin)
  # change y scale
  gp <- gp + scale_y_continuous(expand=c(0,0), 
                                limits=c(0,ylo+yhi))
  # return plot
  return(gp)
}


#' @title Insert line breaks in long labels
#' @name sju.wordwrap
#' 
#' @description Insert line breaks in long character strings. Useful if you want to wordwrap
#'                plot labels.
#'
#' @param labels The label(s) (i.e. character string). You can also pass several strings as vector
#'          (e.g. \code{labels=c("first long string", "second long string")})
#' @param wrap The amount of chars per line (i.e. line length)
#' @param linesep By default, this parameter is \code{NULL} and a regular new line
#'          string is used. For HTML-needs, for instance, \code{linesep} could be \code{"<br>"}.
#' @return New label(s) with line breaks inserted at every \code{wrap}'s position.
#' 
#' @examples
#' sju.wordwrap(c("A very long string", "And another even longer string!"), 10)
#' 
#' @export
sju.wordwrap <- function(labels, wrap, linesep=NULL) {
  # check for valid value
  if (is.null(labels) || length(labels)==0) {
    return(NULL)
  }
  # default line separator is \n
  if (is.null(linesep)) {
    linesep <- '\\1\n'
    lsub <- 0
    ori.linesep <- '\n'
  }
  else {
    # however, for html-function we can use "<br>"
    # as parameter
    lsub <- nchar(linesep)-1
    ori.linesep <- linesep
    linesep <- sprintf("\\1%s", linesep)
  }
  # create regex pattern for line break
  pattern <- c(paste('(.{1,', wrap, '})(\\s|$)', sep=""))
  # iterate all labels
  for (n in 1:length(labels)) {
    # check if wrap exceeds lengths of labels
    if (wrap>0 && nchar(labels[n])>wrap) {
      # insert line breaks
      labels[n] <- gsub(pattern, linesep, labels[n])
      # -----------------------
      # in case label was short enough, we still have a line break
      # at the end of the label. here we remove any trailing line breaks
      # -----------------------
      # get length of label
      l <- nchar(labels[n])
      # get last char
      lc <- substr(labels[n], l-lsub, l)
      # check if line break
      if (lc==ori.linesep) {
        # if yes, remove it
        labels[n] <- substr(labels[n], 0, l-(lsub+1))
      }
    }
  }
  return(labels)
}


#' @title Recode variable categories into new values.
#' @name sju.recodeTo
#' 
#' @description Recodes the categories of a variables \code{var} into new category values, beginning
#'                with the lowest value specified by parameter \code{lowest}. Useful if you want
#'                to recode dummy variables with 1/2 coding to 0/1 coding, or recoding scales from
#'                1-4 to 0-3 etc.
#'
#' @param var The variable (vector) that should be recoded.
#' @param lowest Indicating the lowest category value after recoding. Default is 0, so the new
#'          variable starts with the category value 0.
#' @param highest If specified and larger than \code{lowest}, all category values larger than
#'          \code{highest} will be set to \code{NA}. Default is \code{-1}, i.e. this parameter is ignored
#'          and no NA's will be produced.
#' @return A new variable with recoded category values, where \code{lowest} indicates the lowest
#'           value.
#' 
#' @examples
#' # recode 1-4 to 0-3
#' dummy <- sample(1:4, 10, replace=TRUE)
#' sju.recodeTo(dummy)
#' 
#' # recode 3-6 to 0-3
#' # note that numeric type is returned
#' dummy <- as.factor(3:6)
#' sju.recodeTo(dummy) 
#' 
#' # lowest value starting with 1
#' dummy <- sample(11:15, 10, replace=TRUE)
#' sju.recodeTo(dummy, 1) 
#'
#' # lowest value starting with 1, highest with 3
#' # all others set to NA
#' dummy <- sample(11:15, 10, replace=TRUE)
#' sju.recodeTo(dummy, 1, 3) 
#' 
#' @export
sju.recodeTo <- function(var, lowest=0, highest=-1) {
  # check if factor
  if (is.factor(var)) {
    # try to convert to numeric
    var <- as.numeric(as.character(var))
  }
  # retrieve lowest category
  minval <- min(var, na.rm=TRUE)
  # check substraction difference between current lowest value
  # and requested lowest value
  downsize <- minval-lowest
  var <- sapply(var, function(x) x-downsize)
  # check for highest range
  if (highest>lowest) {
    # set NA to all values out of range
    var[var>highest] <- NA
  }
  # return recoded var
  return(var)
}


#' @title Recode variable values.
#' @name sju.recode
#' 
#' @description Recodes the categories of a variables. Wrapper function that calls
#'                the \code{\link{recode}} function from the \code{car} package.
#'
#' @param ... parameters, see \code{\link{recode}} function from the \code{car} package.
#' @return A variable with recoded values.
#' 
#' @examples
#' data(efc)
#' table(efc$e42dep)
#' table(sju.recode(efc$e42dep, "1:2=1;3:4=2"))
#'
#' @importFrom car recode
#' @export
sju.recode <- function(...) {
  # return recoded var
  return(recode(...))
}


#' @title Plot Variance Inflation Factors of linear models
#' @name sjp.vif
#' 
#' @description Plots the Variance Inflation Factors (check for multicollinearity) of 
#'                (generalized) linear models. Values below 5 are good and indicating no
#'                multicollinearity, values between 5 and 10 may be tolerable. Values 
#'                greater than 10 are not acceptable and indicate multicollinearity
#'                between model's predictors.
#'
#' @param fit The fitted (generalized) linear model which should be checked for
#'          multicollinearity.
#' @return (invisibly) returns the VIF values.
#' 
#' @examples
#' # fit linear model
#' fit <- lm(airquality$Ozone ~ airquality$Wind + airquality$Temp + airquality$Solar.R)
#' # plot VIF values
#' sjp.vif(fit)
#' 
#' @importFrom car vif
#' @export
sjp.vif <- function(fit) {
  # check if we have more than 1 term
  if (length(coef(fit))>2) {
    # variance inflation factor
    # claculate VIF
    vifval <- vif(fit)
    if (is.matrix(vifval)) {
      val <- vifval[,1]
    }
    else {
      val <- vifval
    }
    # retrieve highest VIF-value to determine y-axis range
    maxval <- val[which.max(val)]
    # determine upper limit of y-axis
    upperLimit <-10
    # check whether maxval exceeds the critical VIF-Limit
    # of 10. If so, set upper limit to max. value
    if (maxval >= upperLimit) {
      upperLimit <- ceiling(maxval)
    }
    mydat <- data.frame(cbind(round(val,2)))
    # Neue Variable erstellen, damit die Ergebnisse sortiert werden
    # können (siehe reorder in ggplot-Funktion)
    mydat$vars<-row.names(mydat)
    # die variablenlabel sollen noch mal sortiert werden, nach 
    # VIF-Werten aufsteigend. Dies ist für die X-Achsenbeschriftung
    # nötig, da diese sonst nicht mehr mit den sortierten VIF-Werten
    # (Balkenreihenfolge auf X-Achse) übereinstimmt
    mydat <- cbind(mydat, mydat[order(val),2])
    # Spalten sollen Namen kriegen
    names(mydat)<-c("vif", "vars", "label")
    # grafik ausgeben, dabei die variablen der X-Achse nach aufsteigenden
    # VIF-Werten ordnen
    plot(ggplot(mydat, aes(x=reorder(vars, vif), y=vif)) +
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


#' @title Set NA for specific variable values
#' @name sju.setNA
#' 
#' @description This function sets specific values of a variable \code{var}
#'                as missings (\code{NA}).
#'
#' @param var The variable where new missing values should be defined.
#' @param values A numeric vector with values that should be replaced with \code{\link{NA}}'s.
#' 
#' @return The \code{var} where each value of \code{values} replaced by an \code{NA}.
#' 
#' @examples
#' # create random variable
#' dummy <- sample(1:8, 100, replace=TRUE)
#' # show value distribution
#' table(dummy)
#' # set value 1 and 8 as missings
#' dummy <- sju.setNA(dummy, c(1,8))
#' # show value distribution, including missings
#' table(dummy, exclude=NULL)
#' 
#' @export
sju.setNA <- function(var, values) {
  # retrieve value labels
  vl <- attr(var, "value.labels")
  # retrieve label names
  ln <- names(vl)
  # iterate all values that should be 
  # replaced by NA's
  for (i in seq_along(values)) {
    # find associated values in var
    # and set them to NA
    var[var==values[i]] <- NA
    # check if value labels exist, and if yes, remove them
    labelpos <- which(as.numeric(vl)==values[i])
    if (length(labelpos > 0)) {
      # remove NA label
      vl <- vl[-labelpos]
    }
  }
  # set back updated label attribute
  attr(var, "value.labels") <- vl
  return(var)
}


#' @title Weight a variable
#' @name sju.weight2
#' 
#' @description This function weights the variable \code{var} by
#'                a specific vector of \code{weights}. It's an 
#'                alternative weight calculation to \code{\link{sju.weight}},
#'                though \code{\link{sju.weight}} usage is recommended.
#'                This function sums up all \code{weights} values of the associated
#'                categories of \code{var}, whereas the \code{\link{sju.weight}} function
#'                uses a \code{\link{xtabs}} formula to weight cases. Thus, this function
#'                may return a value with a different length than that from \code{var}.
#'
#' @seealso \code{\link{sju.weight}}
#'
#' @param var The (unweighted) variable 
#' @param weights A vector with same length as \code{var}, which
#'          contains weight factors. Each value of \code{var} has a
#'          specific assigned weight in \code{weights}.
#' 
#' @return The weighted \code{var}.
#' 
#' @note The values of the returned vector are in sorted order, whereas the categories
#'        of the original \code{var} may be spread randomly. Hence, \code{var} can't be
#'        used, for instance, for further cross tabulation. In case you want to have
#'        weighted contingency tables or (grouped) box plots etc., use the \code{weightBy}
#'        parameter of most functions (like in \code{\link{sjt.xtab}} or \code{\link{sjp.grpfrq}}).
#' 
#' @examples
#' v <- sample(1:4, 20, TRUE)
#' table(v)
#' w <- abs(rnorm(20))
#' table(sju.weight2(v,w))
#' 
#' @export
sju.weight2 <- function(var, weights) {
  items <- unique(var)
  newvar <- c()
  for (i in 1:length(items)) {
    newcount = round(sum(weights[which(var==items[i])]))
    newvar <- c(newvar, rep(items[i], newcount))
  }
  return (newvar)
}


#' @title Weight a variable
#' @name sju.weight
#' @description This function weights the variable \code{var} by
#'                a specific vector of \code{weights}.
#'
#' @seealso \code{\link{sju.weight2}}
#' 
#' @param var The (unweighted) variable 
#' @param weights A vector with same length as \code{var}, which
#'          contains weight factors. Each value of \code{var} has a
#'          specific assigned weight in \code{weights}.
#' 
#' @return The weighted \code{var}.
#' 
#' @note The values of the returned vector are in sorted order, whereas the categories
#'        of the original \code{var} may be spread randomly. Hence, \code{var} can't be
#'        used, for instance, for further cross tabulation. In case you want to have
#'        weighted contingency tables or (grouped) box plots etc., use the \code{weightBy}
#'        parameter of most functions (like in \code{\link{sjt.xtab}} or \code{\link{sjp.grpfrq}}).
#' 
#' @examples
#' v <- sample(1:4, 20, TRUE)
#' table(v)
#' w <- abs(rnorm(20))
#' table(sju.weight(v,w))
#' 
#' @export
sju.weight <- function(var, weights) {
  # init values
  weightedvar <- c()
  wtab <- round(xtabs(weights ~ var, data=data.frame(cbind(weights=weights,var=var))))
  # iterate all table values
  for (w in 1:length(wtab)) {
    # retrieve count of each table cell
    w_count <- wtab[[w]]
    # retrieve "cell name" which is identical to the variable value
    w_value <- as.numeric(names(wtab[w]))
    # append variable value, repeating it "w_count" times.
    weightedvar <- c(weightedvar, rep(w_value, w_count))
  }
  return(weightedvar)
}


#' @title Group near elements of string vectors
#' @name sju.groupString
#' 
#' @seealso \code{\link{sju.strpos}}
#' 
#' @description This function groups elements of a string vector (character or string variable) according
#'                to the element's distance. The more similar two string elements are, the higher is the
#'                chance to be combined into a group.
#'
#' @param strings a character vector with string elements
#' @param maxdist the maximum distance between two string elements, which is allowed to treat two
#'          elements as similar or equal.
#' @param method Method for distance calculation. The default is \code{"lv"}. See \code{stringdist} package for details.
#' @param strict if \code{TRUE}, value matching is more strictly. See examples for details.
#' @param trim.whitespace if \code{TRUE} (default), leading and trailing white spaces will
#'          be removed from string values.
#' @param remove.empty if \code{TRUE} (default), empty string values will be removed from the
#'          character vector \code{strings}.
#' @param showProgressBar If \code{TRUE}, the progress bar is displayed when computing the distance matrix.
#'          Default in \code{FALSE}, hence the bar is hidden.
#' 
#' @return A character vector where similar string elements (values) are recoded into a new, single value.
#' 
#' @examples
#' \dontrun{
#' oldstring <- c("Hello", "Helo", "Hole", "Apple", "Ape", "New", "Old", "System", "Systemic")
#' newstring <- sju.groupString(oldstring)
#' sjt.frq(data.frame(oldstring, newstring), removeStringVectors = FALSE, autoGroupStrings = FALSE)
#' 
#' newstring <- sju.groupString(oldstring, strict = TRUE)
#' sjt.frq(data.frame(oldstring, newstring), removeStringVectors = FALSE, autoGroupStrings = FALSE)}
#' 
#' @export
sju.groupString <- function(strings, maxdist = 3, method = "lv", strict = FALSE, trim.whitespace = TRUE, remove.empty = TRUE, showProgressBar = FALSE) {
  # -------------------------------------
  # check if required package is available
  # -------------------------------------
  if (!requireNamespace("stringdist", quietly = TRUE)) {
    stop("Package 'stringdist' needed for this function to work. Please install it.", call. = FALSE)
  }
  # -------------------------------------
  # coerce to character, if necessary
  # -------------------------------------
  if (!is.character(strings)) strings <- as.character(strings)
  # -------------------------------------
  # helper function to trim white spaces
  # -------------------------------------
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  # -------------------------------------
  # trim white spaces
  # -------------------------------------
  if (trim.whitespace) {
    for (i in 1:length(strings)) {
      strings[i] <- trim(strings[i])
    }
  }
  # -------------------------------------
  # remove empty values
  # -------------------------------------
  if (remove.empty) {
    removers <- c()
    for (i in 1:length(strings)) {
      if (0==nchar(strings[i])) {
        removers <- c(removers, i)
      }
    }
    if (length(removers)>0) strings <- strings[-removers]
  }
  # -------------------------------------
  # create matrix from string values of variable
  # -------------------------------------
  m <- stringdist::stringdistmatrix(strings, strings, method = method)
  colnames(m) <- strings
  rownames(m) <- strings
  # -------------------------------------
  # init variable that contains "close" pairs
  # -------------------------------------
  pairs <- list()
  # -------------------------------------
  # helper function that finds elements in
  # final list of grouped elements
  # -------------------------------------
  findInPairs <- function(curel) {
    elfound <- FALSE
    if (length(pairs)>0) {
      for (ll in 1:length(pairs)) {
        pel <- pairs[[ll]]
        if (any(pel==curel)) elfound <- TRUE
      }
    }
    return (elfound)
  }
  # -------------------------------------
  # create progress bar
  # -------------------------------------
  if (showProgressBar) pb <- txtProgressBar(min=0, max=ncol(m), style=3)
  # -------------------------------------
  # iterate matrix
  # -------------------------------------
  for (i in 1:nrow(m)) {
    # update progress bar
    if (showProgressBar) setTxtProgressBar(pb, i)
    # -------------------------------------
    # check if current element is already grouped
    # -------------------------------------
    if (!findInPairs(rownames(m)[i])) {
      # -------------------------------------
      # current row element has not been grouped
      # yet, so go on...
      # -------------------------------------
      pairvector <- c()
      for (j in 1:ncol(m)) {
        # -------------------------------------
        # check if we found a pair's distance that
        # is within the maximum requested distance
        # i.e. which are "close" enough
        # -------------------------------------
        if (m[i,j] <= maxdist) {
          # -------------------------------------
          # go through all rows of this column and 
          # check if there's a better match for the
          # currently compared token
          # -------------------------------------
          foundBetterToken <- !strict
          for (cnt in 1:nrow(m)) {
            if (strict) {
              if (m[cnt,j] > 0 && m[cnt,j] < m[i,j]) foundBetterToken <- TRUE
            }
            else {
              if (m[cnt,j] <= maxdist && m[i,cnt] <= maxdist) foundBetterToken <- FALSE
            }
          }
          # -------------------------------------
          # in the current column, there's no better
          # matching of strings, so we pick this values
          # and add it to our results
          # -------------------------------------
          if (!foundBetterToken) {
            # -------------------------------------
            # remember string value
            # -------------------------------------
            token <- colnames(m)[j]
            # -------------------------------------
            # check if we already found a string value
            # within this column
            # -------------------------------------
            if (!any(pairvector==token) && !findInPairs(token)) {
              # -------------------------------------
              # if not, add string values to "close" pairs
              # of this column
              # -------------------------------------
              pairvector <- c(pairvector, token)
            }
          }
        }
      }
      # -------------------------------------
      # now we have a vector with all "close" string values
      # from the current row's value
      # -------------------------------------
      pairvector <- sort(pairvector)
      # -------------------------------------
      # check if we already have saved these values to our list
      # -------------------------------------
      if (!any(unlist(lapply(pairs, function(x) length(x)==length(pairvector) && any(x==pairvector))))) {
        # -------------------------------------
        # if not, add "close" values as new list element
        # -------------------------------------
        pairs <- c(pairs, list(pairvector))
      }
    }
  }
  # -------------------------------------
  # we now have a list, where each list element
  # is a vector of "close" string values
  # -------------------------------------
  strings.new <- c()
  # -------------------------------------
  # go through each list element
  # -------------------------------------
  for (i in 1:length(pairs)) {
    r <- pairs[[i]]
    # -------------------------------------
    # find vector indices of "close" values in
    # original string
    # -------------------------------------
    indices <- unlist(lapply(r, function(x) which(strings==x)))  
    newvalue <- r[1]
    count <- 2
    # -------------------------------------
    # "merge" each close values into one
    # single value that combines all close values
    # -------------------------------------
    while (count <= length(r)) {
      newvalue <- paste0(newvalue, ", ", r[count])
      count <- count+1
    }
    strings.new[indices] <- newvalue
  }
  if (showProgressBar) close(pb)
  # -------------------------------------
  # return new vector, where all single "close"
  # values are replaced by the group of closed values.
  # e.g. the three values "hello", "holle" and "hole"
  # will be "recoded" into on value "hello, holle, hole"
  # -------------------------------------
  return (strings.new)
}


#' @title Find partial matching and close distance elements in strings
#' @name sju.strpos
#' @description This function finds the element indices of partial matching or similar strings 
#'                in a character vector. Can be used to find exact or slightly mistyped elements
#'                in a string vector.
#'
#' @seealso \code{\link{sju.groupString}}
#'
#' @param searchString a character vector with string elements
#' @param findTerm the string that should be matched against the elements of \code{searchString}.
#' @param maxdist the maximum distance between two string elements, which is allowed to treat them
#'          as similar or equal.
#' @param part.dist.match activates similar matching (close distance strings) for parts (substrings)
#'          of the \code{searchString}. Following values are accepted:
#'          \itemize{
#'            \item 0 for no partial distance matching
#'            \item 1 for one-step matching, which means, only substrings of same length as \code{findTerm} are extracted from \code{searchString} matching
#'            \item 2 for two-step matching, which means, substrings of same length as \code{findTerm} as well as strings with a slightly wider range are extracted from \code{searchString} matching
#'          }
#'          Default value is 0.
#' @param showProgressBar If \code{TRUE}, the progress bar is displayed when computing the distance matrix.
#'          Default in \code{FALSE}, hence the bar is hidden.
#'          
#' @return A numeric vector with index position of elements in \code{searchString} that 
#'           partially match or are similar to \code{findTerm}. Returns \code{-1} if no
#'           match was found.
#' 
#' @note this function does \emph{not} return the position of a matching string \emph{inside}
#'         another string, but the element's index of the \code{searchString} vector, where
#'         a (partial) match with \code{findTerm} was found. Thus, searching for "abc" in
#'         a string "this is abc" will not return 9 (the start position of the substring),
#'         but 1 (the element index, which is always 1 if \code{searchString} only has one element).
#' 
#' @examples
#' \dontrun{
#' string <- c("Hello", "Helo", "Hole", "Apple", "Ape", "New", "Old", "System", "Systemic")
#' sju.strpos(string, "hel")   # partial match
#' sju.strpos(string, "stem")  # partial match
#' sju.strpos(string, "R")     # no match
#' sju.strpos(string, "saste") # similarity to "System"
#' 
#' # finds two indices, because partial matching now 
#' # also applies to "Systemic"
#' sju.strpos(string, 
#'            "sytsme", 
#'            part.dist.match = 1)
#' 
#' # finds nothing
#' sju.strpos("We are Sex Pistols!", "postils")
#' # finds partial matching of similarity
#' sju.strpos("We are Sex Pistols!", "postils", part.dist.match = 1)}
#' 
#' @export
sju.strpos <- function(searchString, findTerm, maxdist = 3, part.dist.match = 0, showProgressBar = FALSE) {
  # -------------------------------------
  # init return value
  # -------------------------------------
  indices <- c()
  # -------------------------------------
  # find element indices from partial matching of string and find term
  # -------------------------------------
  pos <- as.numeric(grep(findTerm, searchString, ignore.case = T))
  if (length(pos)>0) indices <- c(indices, pos)
  # -------------------------------------
  # check if required package is available
  # -------------------------------------
  if (!requireNamespace("stringdist", quietly = TRUE)) {
    cat("Package 'stringdist' needed for this function to fully work. Please install it. Only partial matching indices are returned.")
    return (indices)
  }
  # -------------------------------------
  # find element indices from similar strings
  # -------------------------------------
  pos <- which(stringdist::stringdist(tolower(findTerm), tolower(searchString)) <= maxdist)
  if (length(pos)>0) indices <- c(indices, pos)
  # -------------------------------------
  # find element indices from partial similar (distance) 
  # string matching
  # -------------------------------------
  if (part.dist.match>0) {
    # -------------------------------------
    # helper function to trim white spaces
    # -------------------------------------
    trim <- function (x) gsub("^\\s+|\\s+$", "", x)
    ftlength <- nchar(findTerm)
    # -------------------------------------
    # create progress bar
    # -------------------------------------
    if (showProgressBar) pb <- txtProgressBar(min=0, max=length(searchString), style=3)
    # -------------------------------------
    # iterate search string vector
    # -------------------------------------
    for (ssl in 1:length(searchString)) {
      # -------------------------------------
      # retrieve each element of search string vector
      # we do this step by step instead of vectorizing
      # due to the substring approach
      # -------------------------------------
      sst <- searchString[ssl]
      # -------------------------------------
      # we extract substrings of same length as findTerm
      # starting from first char of searchString until end
      # and try to find similar matches
      # -------------------------------------
      steps <- nchar(sst) - ftlength + 1
      for (pi in 1:steps) {
        # -------------------------------------
        # retrieve substring
        # -------------------------------------
        sust <- trim(substr(sst, pi, pi+ftlength-1))
        # -------------------------------------
        # find element indices from similar substrings
        # -------------------------------------
        pos <- which(stringdist::stringdist(tolower(findTerm), tolower(sust)) <= maxdist)
        if (length(pos)>0) indices <- c(indices, ssl)
      }
      if (part.dist.match>1) {
        # -------------------------------------
        # 2nd loop picks longer substrings, because similarity
        # may also be present if length of strings differ
        # (e.g. "app" and "apple")
        # -------------------------------------
        steps <- nchar(sst) - ftlength
        if (steps>1) {
          for (pi in 2:steps) {
            # -------------------------------------
            # retrieve substring
            # -------------------------------------
            sust <- trim(substr(sst, pi-1, pi+ftlength))
            # -------------------------------------
            # find element indices from similar substrings
            # -------------------------------------
            pos <- which(stringdist::stringdist(tolower(findTerm), tolower(sust)) <= maxdist)
            if (length(pos)>0) indices <- c(indices, ssl)
          }
        }
      }
      # update progress bar
      if (showProgressBar) setTxtProgressBar(pb, ssl)
    }
  }
  if (showProgressBar) close(pb)
  # -------------------------------------
  # return result
  # -------------------------------------
  if (length(indices) > 0) {
    return (sort(unique(indices)))
  }
  return (-1)
}


#' @title Compute row means with min amount of valid values
#' @name sju.mean.n
#' @description This function is similar to the SPSS \code{MEAN.n} function and computes
#'                row means from a \link{data.frame} or \link{matrix} if at least \code{n}
#'                values of a row a valid (and not \link{NA}).
#'
#' @param df a \link{data.frame} with at least two columns, where row means are applied.
#' @param n the amount of valid values per row to calculate the row mean. If a row's valid
#'          values is smaller than \code{n}, \link{NA} will be returned as row mean value.
#' 
#' @return A vector with row mean values of \code{df} for those rows with at least \code{n}
#'           valid values. Else, \link{NA} is returned.
#' 
#' @references \itemize{
#'              \item \href{http://candrea.ch/blog/compute-spss-like-mean-index-variables/}{candrea's blog}
#'              \item \href{http://r4stats.com/2014/09/03/adding-the-spss-mean-n-function-to-r/}{r4stats.com}
#'              }
#' 
#' @examples
#' df <- data.frame(a=c(1,2,NA,4), b=c(NA,2,NA,5), c=c(NA,4,NA,NA), d=c(2,3,7,8))
#' sju.mean.n(df, 4) # 1 valid return value
#' sju.mean.n(df, 3) # 2 valid return values
#' sju.mean.n(df, 2)
#' sju.mean.n(df, 1) # all means are shown
#' 
#' @export
sju.mean.n <- function(df, n) {
  # ---------------------------------------
  # coerce matrix to data frame
  # ---------------------------------------
  if (is.matrix(df)) df <- as.data.frame(df)
  # ---------------------------------------
  # check if we have a data framme with at least two columns
  # ---------------------------------------
  if (!is.data.frame(df) || ncol(df) < 2) {
    warning("'df' must be a data.frame with at least two columns.")
    return (NA)
  }
  # ---------------------------------------
  # n may not be larger as df's amount of columns
  # ---------------------------------------
  if (ncol(df) < n) {
    warning("'n' must be smaller or equal to data.frame's amount of columns.")
    return (NA)
  }
  apply(df, 1, function(x) ifelse(sum(!is.na(x)) >= n, mean(x, na.rm=TRUE), NA))
}
