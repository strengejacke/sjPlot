#' @title Import SPSS dataset as data frame into R
#' @name sji.SPSS
#' 
#' @description Import data from SPSS, including NA's, value and variable labels.
#' 
#' @seealso \itemize{
#'            \item \href{http://www.strengejacke.de/sjPlot/datainit/}{sjPlot manual: data initialization}
#'            \item \href{http://www.strengejacke.de/sjPlot/sji.viewSPSS/}{sjPlot manual: inspecting (SPSS imported) data frames}
#'            \item \code{\link{sji.getValueLabels}}
#'            \item \code{\link{sji.getVariableLabels}}
#'            \item \code{\link{sji.convertToLabel}}
#'            \item \code{\link{sji.convertToValue}}
#'            \item \code{\link{sji.viewSPSS}}
#'            }
#'          
#' @param path The file path to the SPSS dataset.
#' @param enc The file encoding of the SPSS dataset.
#' @param autoAttachVarLabels if \code{TRUE}, variable labels will automatically be
#'          attached to each variable as \code{"variable.label"} attribute.
#'          See \code{\link{sji.setVariableLabels}} for details.
#' @return A data frame containing the SPSS data. retrieve value labels with \code{\link{sji.getValueLabels}}
#'   and variable labels with \code{\link{sji.getVariableLabels}}.
#'   
#' @note This is a wrapper function for \code{\link{read.spss}} of the
#'         \code{foreign} package, using convenient parameter default
#'         settings.
#' 
#' @examples
#' # import SPSS data set
#' # mydat <- sji.SPSS("my_spss_data.sav", enc="UTF-8")
#' 
#' # retrieve variable labels
#' # mydat.var <- sji.getVariableLabels(mydat)
#' 
#' # retrieve value labels
#' # mydat.val <- sji.getValueLabels(mydat)
#' 
#' @importFrom foreign read.spss
#' @export
sji.SPSS <- function(path, enc=NA, autoAttachVarLabels=FALSE) {
  # import data as data frame
  data.spss <- read.spss(path, to.data.frame=TRUE, use.value.labels=FALSE, reencode=enc)
  # auto attach labels
  if (autoAttachVarLabels) {
    message("Attaching variable labels. Please wait...\n")
    data.spss <- sji.setVariableLabels(data.spss, sji.getVariableLabels(data.spss))
  }
  # return data frame
  return(data.spss)
}


#' @title Retrieve value labels of a variable or an SPSS-imported data frame
#' @name sji.getValueLabels
#' @description This function retrieves the value labels of an imported
#'                SPSS data set and returns the result as list or of a variable and returns
#'                the label as string.
#' 
#' @seealso \itemize{
#'            \item \href{http://www.strengejacke.de/sjPlot/datainit/}{sjPlot manual: data initialization}
#'            \item \href{http://www.strengejacke.de/sjPlot/sji.viewSPSS/}{sjPlot manual: inspecting (SPSS imported) data frames}
#'            \item \code{\link{sji.SPSS}}
#'            \item \code{\link{sji.getVariableLabels}}
#'            \item \code{\link{sji.convertToLabel}}
#'            \item \code{\link{sji.convertToValue}}
#'            \item \code{\link{sji.setValueLabels}}
#'            }
#'
#' @param x a data frame with variables that have attached value labels (e.g.
#'          from an imported SPSS data (see \code{\link{sji.SPSS}})) or a variable
#'          with attached value labels.
#' @return Either a list with all value labels from the data frame's variables,
#'           or a string with the value labels, if \code{x} is a variable.
#' 
#' @examples
#' # import SPSS data set
#' # mydat <- sji.SPSS("my_spss_data.sav", enc="UTF-8")
#' 
#' # retrieve variable labels
#' # mydat.var <- sji.getVariableLabels(mydat)
#' 
#' # retrieve value labels
#' # mydat.val <- sji.getValueLabels(mydat)
#' 
#' data(efc)
#' sji.getValueLabels(efc$e42dep)
#' 
#' @export
sji.getValueLabels <- function(x) {
  if (is.data.frame(x) || is.matrix(x)) {
    a <- lapply(x, FUN = sji.getValueLabel)
  }
  else {
    a <- sji.getValueLabel(x)
  }
  
  return (a)
}
sji.getValueLabel <- function(x) {
  return (rev(names(attr(x, "value.labels"))))
}


#' @title Attach value labels to a variable or vector
#' @name sji.setValueLabels
#' @description This function attaches character labels as \code{"value.labels"} attribute
#'                to a variable or vector \code{"x"}, resp. to all variables of a data frame
#'                if \code{"x"} is a \code{\link{data.frame}}. These value labels will be accessed
#'                by most of this package's functions, in order to automatically set values
#'                or legend labels.
#'                
#' @seealso \itemize{
#'            \item \href{http://www.strengejacke.de/sjPlot/datainit/}{sjPlot manual: data initialization}
#'            \item \href{http://www.strengejacke.de/sjPlot/sji.viewSPSS/}{sjPlot manual: inspecting (SPSS imported) data frames}
#'            \item \code{\link{sji.SPSS}}
#'            \item \code{\link{sji.getVariableLabels}}
#'            \item \code{\link{sji.convertToLabel}}
#'            \item \code{\link{sji.convertToValue}}
#'            \item \code{\link{sji.getValueLabels}}
#'            }
#' 
#' @param x a variable (vector) or a data frame where labels should be attached. Replaces former value labels.
#' @param labels a character vector of labels that will be attached to \code{"x"} by setting
#'          the \code{"value.labels"} attribute. The length of this character vector must equal
#'          the value range of \code{"x"}, i.e. if \code{"x"} has values from 1 to 3,
#'          \code{"labels"} should have a length of 3. 
#'          If \code{"x"} is a data frame, \code{labels} may also be a \code{\link{list}} of
#'          character vectors. If \code{labels} is a list, it must have the same length as
#'          number of columns of \code{x}. If \code{labels} is a vector and \code{x} is a data frame,
#'          the \code{labels} will be applied to each column of \code{x}.
#' @return \code{"x"} with attached value labels.
#' 
#' @examples
#' dummy <- sample(1:4, 40, replace=TRUE)
#' sjp.frq(dummy)
#' 
#' dummy <- sji.setValueLabels(dummy, c("very low", "low", "mid", "hi"))
#' sjp.frq(dummy)
#' 
#' @export
sji.setValueLabels <- function(x, labels) {
  if (is.vector(x) || is.atomic(x)) {
    return (sji.setValueLabels.vector(x, labels))
  }
  else if (is.data.frame(x) || is.matrix(x)) {
    for (i in 1:ncol(x)) {
      if (is.vector(labels)) {
        x[,i] <- sji.setValueLabels.vector(x[,i], labels)
      }
      else if (is.list(labels)) {
        x[,i] <- sji.setValueLabels.vector(x[,i], labels[[i]])
      }
      else {
        warning("'labels' must be a list of same length as 'ncol(x)' or a vector.", call. = F)
      }
    }
    return (x)
  }
}
sji.setValueLabels.vector <- function(var, labels) {
  # check for null
  if (!is.null(labels)) {
    if (is.null(var) || is.character(var)) {
      warning("Can't attach labels to string or NULL vectors.\n")
    }
    else {
      # check if var is a factor
      if (is.factor(var)) {
        # retrieve levels
        minval <- 1
        maxval <- length(levels(var))
      }
      else {
        # retrieve values
        minval <- min(var, na.rm=TRUE)
        maxval <- max(var, na.rm=TRUE)
      }
      # check for unlisting
      if (is.list(labels)) {
        labels <- as.vector(unlist(labels))
      }
      lablen <- length(labels)
      valrange <- maxval-minval+1
      if (is.infinite(valrange)) {
        warning("Can't set value labels. Infinite value range.\n")
      }
      # check for valid length of labels
      else if (valrange<lablen) {
        message(sprintf("More labels than values of \"var\". Using first %i labels.\n", valrange))
        attr(var, "value.labels") <- c(as.character(c(minval:maxval)))
        names(attr(var, "value.labels")) <- rev(labels[1:valrange])
      }
      else if (valrange>lablen) {
        warning("Can't set value labels. Value range of \"var\" is longer than length of \"labels\".\n")
      }
      else {
        attr(var, "value.labels") <- c(as.character(c(minval:maxval)))
        names(attr(var, "value.labels")) <- rev(labels)
      }
    }
  }
  return (var)
}


#' @title Retrieve variable labels of (an SPSS-imported) data frame or of a specific variable
#' @name sji.getVariableLabels
#' 
#' @description This function retrieves the variable labels of an imported
#'                SPSS data set and returns the result as list or the variable 
#'                label of a specific vector / variable and returns it as string.
#' 
#' @seealso \itemize{
#'            \item \href{http://www.strengejacke.de/sjPlot/datainit/}{sjPlot manual: data initialization}
#'            \item \href{http://www.strengejacke.de/sjPlot/sji.viewSPSS/}{sjPlot manual: inspecting (SPSS imported) data frames}
#'            \item \code{\link{sji.SPSS}}
#'            \item \code{\link{sji.getValueLabels}}
#'            \item \code{\link{sji.convertToLabel}}
#'            \item \code{\link{sji.convertToValue}}
#'            \item \code{\link{sji.setValueLabels}}
#'            }
#' 
#' @param x A data frame (containing imported SPSS data or with attached variable labels) or
#'          a vector with \code{"variable.label"} attribute.
#' 
#' @return A list with all variable labels from the SPSS dataset,
#'           or a string with the variable label, if \code{x} is a variable.
#' 
#' @examples
#' # import SPSS data set
#' # mydat <- sji.SPSS("my_spss_data.sav", enc="UTF-8")
#' 
#' # retrieve variable labels
#' # mydat.var <- sji.getVariableLabels(mydat)
#' 
#' # retrieve value labels
#' # mydat.val <- sji.getValueLabels(mydat)
#' 
#' data(efc)
#' sji.getVariableLabels(efc$e42dep)
#' 
#' @export
sji.getVariableLabels <- function(x) {
  if (is.data.frame(x) || is.matrix(x)) {
    return(attr(x, "variable.labels"))
  }
  else {
    return(attr(x, "variable.label"))
  }
}


#' @title Set variable label(s) to a single variable or data frame
#' @name sji.setVariableLabels
#' @description This function sets variable labels to a single variable or to
#'                a set of variables in a data frame. To each variable, the
#'                attribute \code{"variable.label"} with the related variable
#'                name is attached. Most of this package's functions can automatically
#'                retrieve the variable name to use it as axis labels or plot title.
#' 
#' @seealso \itemize{
#'            \item \href{http://www.strengejacke.de/sjPlot/datainit/}{sjPlot manual: data initialization}
#'            \item \href{http://www.strengejacke.de/sjPlot/sji.viewSPSS/}{sjPlot manual: inspecting (SPSS imported) data frames}
#'            \item \code{\link{sji.SPSS}}
#'            \item \code{\link{sji.getVariableLabels}}
#'            \item \code{\link{sji.convertToLabel}}
#'            \item \code{\link{sji.convertToValue}}
#'            \item \code{\link{sji.getValueLabels}}
#'            \item \code{\link{sji.setValueLabels}}
#'            }
#' 
#' @param x A single variable (vector) or data frame with variables.
#' @param lab If \code{x} is a vector (single variable), use a single character string with 
#'          the variable label for \code{x}. If \code{x} is a \code{\link{data.frame}}, use a
#'          vector with character labels of same length as \code{ncol(x)}.
#' @return \code{x}, with attached \code{"variable.label"} attribute(s), which contains the
#'           variable name(s).
#' 
#' @examples
#' # sample data set, imported from SPSS. Variable labels are attached
#' # as attribute to the data frame (so variables currently don't have this attribute)
#' data(efc)
#' # get variable labels
#' variable.labels <- sji.getVariableLabels(efc)
#' # set variable labels as attribute to each single variable of data frame
#' efc <- sji.setVariableLabels(efc, variable.labels)
#' 
#' \dontrun{
#' sjt.frq(efc$e42dep)
#' sjt.frq(data.frame(efc$e42dep, efc$e16sex))}
#' 
#' # ---------------------------------------------
#' # manually set value and variable labels
#' # ---------------------------------------------
#' dummy <- sample(1:4, 40, replace=TRUE)
#' dummy <- sji.setValueLabels(dummy, c("very low", "low", "mid", "hi"))
#' dummy <- sji.setVariableLabels(dummy, "Dummy-variable")
#' # auto-detection of value labels by default, auto-detection of
#' # variable labels if parameter "title" set to "auto"
#' sjp.frq(dummy, title="auto")
#' 
#' @export
sji.setVariableLabels <- function(x, lab) {
  if (!is.null(lab) && !is.null(x)) {
    if (is.data.frame(x)) {
      if (ncol(x)!=length(lab)) {
        cat("Parameter \"lab\" must be of same length as numbers of columns in \"x\".\n")
      }
      else {
        # -------------------------------------
        # create progress bar
        # -------------------------------------
        pb <- txtProgressBar(min=0, max=ncol(x), style=3)
        for (i in 1:ncol(x)) {
          attr(x[,i], "variable.label") <- lab[i]
          # update progress bar
          setTxtProgressBar(pb, i)
        }
        close(pb)
      }
      # attach also all labels to df
      attr(x, "variable.labels") <- lab
      # and name attribute
      attr(attr(x, "variable.labels"), "names") <- colnames(x)
    }
    else {
      attr(x, "variable.label") <- lab
    }
  }
  return (x)
}

#' @title Replaces variable values with their associated value labels
#' @name sji.convertToLabel
#' 
#' @description This function converts (replaces) variable values (of factors) with their
#' associated value labels. Might be helpful for factor variables.
#' For instance, if you have a Gender variable with 0/1, and associated
#' labels are male/female, this function would convert all 0 to male and
#' all 1 to female in the data frame.
#' 
#' @seealso \itemize{
#'            \item \code{\link{sji.convertToValue}}
#'            \item \code{\link{sji.getValueLabels}}
#'            \item \code{\link{sji.getVariableLabels}}
#'            \item \code{\link{sji.SPSS}}
#'            }
#' 
#' @param variable A (factor) variable.
#' @return A factor variable containing with the replaced value labels.
#' 
#' @examples
#' data(efc)
#' print(sji.getValueLabels(efc)['c161sex'])
#' head(efc$c161sex)
#' head(sji.convertToLabel(efc$c161sex))
#' 
#' print(sji.getValueLabels(efc)['e42dep'])
#' table(efc$e42dep)
#' table(sji.convertToLabel(efc$e42dep))
#' 
#' @export
sji.convertToLabel <- function(variable) {
  vl <- rev(names(attr(variable, "value.labels")))
  vn <- sort(unique(na.omit(variable)))
  
  for (i in 1:length(vl)) {
    variable[variable==vn[i]] <- vl[i]
  }
  return (as.factor(variable))
}


#' @title Converts factors to numeric variables
#' @name sji.convertToValue
#' 
#' @description This function converts (replaces) factor values with the
#' related factor level index number, thus the factor is converted to 
#' a numeric variable.
#' 
#' @seealso \itemize{
#'            \item \code{\link{sji.convertToLabel}}
#'            \item \code{\link{sji.getValueLabels}}
#'            \item \code{\link{sji.getVariableLabels}}
#'            \item \code{\link{sji.SPSS}}
#'            }
#'            
#' @param fac A (factor) variable.
#' @param startAt the starting index, i.e. numeric value of the variable.
#' @return A numeric variable with values ranging from \code{startAt} to
#'           \code{startAt} + length of factor levels.
#' 
#' @examples
#' data(efc)
#' test <- sji.convertToLabel(efc$e42dep)
#' table(test)
#' 
#' table(sji.convertToValue(test))
#' hist(sji.convertToValue(test,0))
#' 
#' @export
sji.convertToValue <- function(fac, startAt=1) {
  l <- length(levels(fac))
  end <- startAt+l-1
  levels(fac) <- c(startAt:end)
  return (as.numeric(as.character(fac)))
}