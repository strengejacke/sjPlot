#' @title Import SPSS dataset as data frame into R
#' @name read_spss
#' 
#' @description Import data from SPSS, including NA's, value and variable labels.
#' 
#' @seealso \itemize{
#'            \item \href{http://www.strengejacke.de/sjPlot/datainit/}{sjPlot manual: data initialization}
#'            \item \href{http://www.strengejacke.de/sjPlot/view_spss/}{sjPlot manual: inspecting (SPSS imported) data frames}
#'            \item \code{\link{get_val_labels}}
#'            \item \code{\link{get_var_labels}}
#'            \item \code{\link{to_label}}
#'            \item \code{\link{to_value}}
#'            \item \code{\link{view_spss}}
#'            }
#'          
#' @param path The file path to the SPSS dataset.
#' @param enc The file encoding of the SPSS dataset.
#' @param autoAttachVarLabels if \code{TRUE}, variable labels will automatically be
#'          attached to each variable as \code{"variable.label"} attribute.
#'          See \code{\link{set_var_labels}} for details.
#' @param atomic.to.fac Logical, if \code{TRUE}, factor variables imported from
#'          SPSS (which are imported as \code{\link{atomic}}) will be converted
#'          to \code{\link{factor}}s.
#' @return A data frame containing the SPSS data. retrieve value labels with \code{\link{get_val_labels}}
#'   and variable labels with \code{\link{get_var_labels}}.
#'   
#' @note This is a wrapper function for \code{\link{read.spss}} of the
#'         \code{foreign} package, using convenient parameter default
#'         settings. This function attaches value and variable
#'         labels to the imported variables of the data frame. \cr \cr
#'        With attached value and variable labels, most functions of this package
#'        automatically detect labels and uses them as axis, legend or title labels
#'        in plots (\code{sjp.}-functions) respectively as column or row headers 
#'        in table outputs (\code{sjt.}-functions). Use \code{options(autoSetValueLabels = FALSE)}
#'        and \code{options(autoSetVariableLabels = FALSE)} to turn off automatic
#'        label detection.
#' 
#' @examples
#' # import SPSS data set
#' # mydat <- read_spss("my_spss_data.sav", enc="UTF-8")
#' 
#' # retrieve variable labels
#' # mydat.var <- get_var_labels(mydat)
#' 
#' # retrieve value labels
#' # mydat.val <- get_val_labels(mydat)
#' 
#' @importFrom foreign read.spss
#' @export
read_spss <- function(path, 
                     enc=NA, 
                     autoAttachVarLabels=FALSE,
                     atomic.to.fac=FALSE) {
  # import data as data frame
  data.spss <- suppressWarnings(read.spss(path, to.data.frame=TRUE, use.value.labels=FALSE, reencode=enc))
  # convert atomic values to factors
  if (atomic.to.fac) {
    # -------------------------------------
    # create progress bar
    # -------------------------------------
    pb <- txtProgressBar(min = 0, 
                         max = ncol(data.spss), 
                         style = 3)
    # tell user...
    message("Converting atomic to factors. Please wait...\n")
    # iterate all columns
    for (i in 1:ncol(data.spss)) {
      # copy column to vector
      x <- data.spss[, i]
      # is atomic, which was factor in SPSS?
      if (is.atomic(x) && !is.null(attr(x, "value.labels"))) {
        # so we have value labels (only typical for factors, not
        # continuous variables) and a variable of type "atomic" (SPSS
        # continuous variables would be imported as numeric) - this
        # indicates we have a factor variable. now we convert to 
        # factor, but need to capture labels attribute first
        labs <- attr(x, "value.labels")
        # to factor
        x <- as.factor(x)
        # set back labels attribute
        attr(x, "value.labels") <- labs
        # copy vector back to data frame
        data.spss[, i] <- x
      }
      # update progress bar
      setTxtProgressBar(pb, i)
    }
    close(pb)
  }
  # auto attach labels
  if (autoAttachVarLabels) {
    message("Attaching variable labels. Please wait...\n")
    data.spss <- set_var_labels(data.spss, get_var_labels(data.spss))
  }
  # return data frame
  return(data.spss)
}


#' @title Retrieve value labels of a variable or an SPSS-imported data frame
#' @name get_val_labels
#' @description This function retrieves the value labels of an imported
#'                SPSS data set and returns the result as list or of a variable and returns
#'                the label as string.
#' 
#' @seealso \itemize{
#'            \item \href{http://www.strengejacke.de/sjPlot/datainit/}{sjPlot manual: data initialization}
#'            \item \href{http://www.strengejacke.de/sjPlot/view_spss/}{sjPlot manual: inspecting (SPSS imported) data frames}
#'            \item \code{\link{read_spss}}
#'            \item \code{\link{get_var_labels}}
#'            \item \code{\link{to_label}}
#'            \item \code{\link{to_value}}
#'            \item \code{\link{set_val_labels}}
#'            }
#'
#' @param x a data frame with variables that have attached value labels (e.g.
#'          from an imported SPSS data (see \code{\link{read_spss}})) or a variable
#'          with attached value labels.
#' @return Either a list with all value labels from the data frame's variables,
#'           or a string with the value labels, if \code{x} is a variable.
#' 
#' @note This function only works with vectors that have value and variable
#'        labels attached. This is automatically done by importing SPSS data sets
#'        with the \code{\link{read_spss}} function and can manually be applied
#'        with the \code{\link{set_val_labels}} and \code{\link{set_var_labels}}
#'        functions. \cr \cr
#'        With attached value and variable labels, most functions of this package
#'        automatically detect labels and uses them as axis, legend or title labels
#'        in plots (\code{sjp.}-functions) respectively as column or row headers 
#'        in table outputs (\code{sjt.}-functions). Use \code{options(autoSetValueLabels = FALSE)}
#'        and \code{options(autoSetVariableLabels = FALSE)} to turn off automatic
#'        label detection.
#'        
#' @examples
#' # import SPSS data set
#' # mydat <- read_spss("my_spss_data.sav", enc="UTF-8")
#' 
#' # retrieve variable labels
#' # mydat.var <- get_var_labels(mydat)
#' 
#' # retrieve value labels
#' # mydat.val <- get_val_labels(mydat)
#' 
#' data(efc)
#' get_val_labels(efc$e42dep)
#' 
#' @export
get_val_labels <- function(x) {
  if (is.data.frame(x) || is.matrix(x)) {
    a <- lapply(x, FUN = sji.getValueLabel)
  }
  else {
    a <- sji.getValueLabel(x)
  }
  
  return (a)
}
sji.getValueLabel <- function(x) {
  # retrieve named labels
  lab <- attr(x, "value.labels")
  # retrieve order of value labels
  reihenfolge <- order(as.numeric(unname(lab)))
  # retrieve label values in correct order
  labels <- names(lab)[reihenfolge]
  # return them
  return (labels)
}


#' @title Attach value labels to a variable or vector
#' @name set_val_labels
#' @description This function attaches character labels as \code{"value.labels"} attribute
#'                to a variable or vector \code{"x"}, resp. to all variables of a data frame
#'                if \code{"x"} is a \code{\link{data.frame}}. These value labels will be accessed
#'                by most of this package's functions, in order to automatically set values
#'                or legend labels.
#'                
#' @seealso \itemize{
#'            \item \href{http://www.strengejacke.de/sjPlot/datainit/}{sjPlot manual: data initialization}
#'            \item \href{http://www.strengejacke.de/sjPlot/view_spss/}{sjPlot manual: inspecting (SPSS imported) data frames}
#'            \item \code{\link{read_spss}}
#'            \item \code{\link{get_var_labels}}
#'            \item \code{\link{to_label}}
#'            \item \code{\link{to_value}}
#'            \item \code{\link{get_val_labels}}
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
#' @note With attached value and variable labels, most functions of this package
#'       automatically detect labels and uses them as axis, legend or title labels
#'       in plots (\code{sjp.}-functions) respectively as column or row headers 
#'       in table outputs (\code{sjt.}-functions). Use \code{options(autoSetValueLabels = FALSE)}
#'       and \code{options(autoSetVariableLabels = FALSE)} to turn off automatic
#'       label detection.
#'       
#' @examples
#' dummy <- sample(1:4, 40, replace=TRUE)
#' sjp.frq(dummy)
#' 
#' dummy <- set_val_labels(dummy, c("very low", "low", "mid", "hi"))
#' sjp.frq(dummy)
#' 
#' @export
set_val_labels <- function(x, labels) {
  if (is.vector(x) || is.atomic(x)) {
    return (sji.setValueLabel.vector(x, labels))
  }
  else if (is.data.frame(x) || is.matrix(x)) {
    for (i in 1:ncol(x)) {
      if (is.list(labels)) {
        x[,i] <- sji.setValueLabel.vector(x[,i], labels[[i]])
      }
      else if (is.vector(labels)) {
        x[,i] <- sji.setValueLabel.vector(x[,i], labels)
      }
      else {
        warning("'labels' must be a list of same length as 'ncol(x)' or a vector.", call. = F)
      }
    }
    return (x)
  }
}
sji.setValueLabel.vector <- function(var, labels) {
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
        names(attr(var, "value.labels")) <- labels[1:valrange]
      }
      else if (valrange>lablen) {
        warning("Can't set value labels. Value range of \"var\" is longer than length of \"labels\".\n")
      }
      else {
        attr(var, "value.labels") <- c(as.character(c(minval:maxval)))
        names(attr(var, "value.labels")) <- labels
      }
    }
  }
  return (var)
}


#' @title Retrieve variable labels of (an SPSS-imported) data frame or of a specific variable
#' @name get_var_labels
#' 
#' @description This function retrieves the variable labels of an imported
#'                SPSS data set and returns the result as list or the variable 
#'                label of a specific vector / variable and returns it as string.
#' 
#' @seealso \itemize{
#'            \item \href{http://www.strengejacke.de/sjPlot/datainit/}{sjPlot manual: data initialization}
#'            \item \href{http://www.strengejacke.de/sjPlot/view_spss/}{sjPlot manual: inspecting (SPSS imported) data frames}
#'            \item \code{\link{read_spss}}
#'            \item \code{\link{get_val_labels}}
#'            \item \code{\link{to_label}}
#'            \item \code{\link{to_value}}
#'            \item \code{\link{set_val_labels}}
#'            }
#' 
#' @param x A data frame (containing imported SPSS data or with attached variable labels) or
#'          a vector with \code{"variable.label"} attribute.
#' 
#' @return A list with all variable labels from the SPSS dataset,
#'           or a string with the variable label, if \code{x} is a variable.
#' 
#' @note This function only works with vectors that have value and variable
#'        labels attached. This is automatically done by importing SPSS data sets
#'        with the \code{\link{read_spss}} function and can manually be applied
#'        with the \code{\link{set_val_labels}} and \code{\link{set_var_labels}}
#'        functions. \cr \cr
#'        With attached value and variable labels, most functions of this package
#'        automatically detect labels and uses them as axis, legend or title labels
#'        in plots (\code{sjp.}-functions) respectively as column or row headers 
#'        in table outputs (\code{sjt.}-functions). Use \code{options(autoSetValueLabels = FALSE)}
#'        and \code{options(autoSetVariableLabels = FALSE)} to turn off automatic
#'        label detection.
#' 
#' @examples
#' # import SPSS data set
#' # mydat <- read_spss("my_spss_data.sav", enc="UTF-8")
#' 
#' # retrieve variable labels
#' # mydat.var <- get_var_labels(mydat)
#' 
#' # retrieve value labels
#' # mydat.val <- get_val_labels(mydat)
#' 
#' data(efc)
#' get_var_labels(efc$e42dep)
#' 
#' @export
get_var_labels <- function(x) {
  if (is.data.frame(x) || is.matrix(x)) {
    return(attr(x, "variable.labels"))
  }
  else {
    return(attr(x, "variable.label"))
  }
}


#' @title Attach variable label(s) to a single variable or data frame
#' @name set_var_labels
#' @description This function sets variable labels to a single variable or to
#'                a set of variables in a data frame. To each variable, the
#'                attribute \code{"variable.label"} with the related variable
#'                name is attached. Most of this package's functions can automatically
#'                retrieve the variable name to use it as axis labels or plot title.
#' 
#' @seealso \itemize{
#'            \item \href{http://www.strengejacke.de/sjPlot/datainit/}{sjPlot manual: data initialization}
#'            \item \href{http://www.strengejacke.de/sjPlot/view_spss/}{sjPlot manual: inspecting (SPSS imported) data frames}
#'            \item \code{\link{read_spss}}
#'            \item \code{\link{get_var_labels}}
#'            \item \code{\link{to_label}}
#'            \item \code{\link{to_value}}
#'            \item \code{\link{get_val_labels}}
#'            \item \code{\link{set_val_labels}}
#'            }
#' 
#' @param x A single variable (vector) or data frame with variables.
#' @param lab If \code{x} is a vector (single variable), use a single character string with 
#'          the variable label for \code{x}. If \code{x} is a \code{\link{data.frame}}, use a
#'          vector with character labels of same length as \code{ncol(x)}.
#' @return \code{x}, with attached \code{"variable.label"} attribute(s), which contains the
#'           variable name(s).
#' 
#' @note With attached value and variable labels, most functions of this package
#'       automatically detect labels and uses them as axis, legend or title labels
#'       in plots (\code{sjp.}-functions) respectively as column or row headers 
#'       in table outputs (\code{sjt.}-functions). Use \code{options(autoSetValueLabels = FALSE)}
#'       and \code{options(autoSetVariableLabels = FALSE)} to turn off automatic
#'       label detection.
#'        
#' @examples
#' # sample data set, imported from SPSS. Variable labels are attached
#' # as attribute to the data frame (so variables currently don't have this attribute)
#' data(efc)
#' # get variable labels
#' variable.labels <- get_var_labels(efc)
#' # set variable labels as attribute to each single variable of data frame
#' efc <- set_var_labels(efc, variable.labels)
#' 
#' \dontrun{
#' sjt.frq(efc$e42dep)
#' sjt.frq(data.frame(efc$e42dep, efc$e16sex))}
#' 
#' # ---------------------------------------------
#' # manually set value and variable labels
#' # ---------------------------------------------
#' dummy <- sample(1:4, 40, replace=TRUE)
#' dummy <- set_val_labels(dummy, c("very low", "low", "mid", "hi"))
#' dummy <- set_var_labels(dummy, "Dummy-variable")
#' # auto-detection of value labels by default, auto-detection of
#' # variable labels if parameter "title" set to "auto"
#' sjp.frq(dummy, title="auto")
#' 
#' @export
set_var_labels <- function(x, lab) {
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
#' @name to_label
#' 
#' @description This function converts (replaces) variable values (of factors) with their
#' associated value labels. Might be helpful for factor variables.
#' For instance, if you have a Gender variable with 0/1, and associated
#' labels are male/female, this function would convert all 0 to male and
#' all 1 to female in the data frame.
#' 
#' @seealso \itemize{
#'            \item \code{\link{to_fac}}
#'            \item \code{\link{to_value}}
#'            \item \code{\link{get_val_labels}}
#'            \item \code{\link{get_var_labels}}
#'            \item \code{\link{read_spss}}
#'            }
#' 
#' @param variable A (factor) variable.
#' @return A factor variable containing with the replaced value labels.
#' 
#' @examples
#' data(efc)
#' print(get_val_labels(efc)['c161sex'])
#' head(efc$c161sex)
#' head(to_label(efc$c161sex))
#' 
#' print(get_val_labels(efc)['e42dep'])
#' table(efc$e42dep)
#' table(to_label(efc$e42dep))
#' 
#' @export
to_label <- function(variable) {
  vl <- rev(names(attr(variable, "value.labels")))
  vn <- sort(unique(na.omit(variable)))
  
  for (i in 1:length(vl)) {
    variable[variable==vn[i]] <- vl[i]
  }
  return (as.factor(variable))
}


#' @title Convert variable into factor and keep value labels
#' @name to_fac
#' 
#' @description This function converts a variable into a factor, but keeps
#'                variable and value labels, if these are attached as attributes
#'                to the variale \code{var}. See examples.
#' 
#' @seealso \itemize{
#'            \item \code{\link{to_value}}
#'            \item \code{\link{to_label}}
#'            \item \code{\link{get_val_labels}}
#'            \item \code{\link{get_var_labels}}
#'            \item \code{\link{read_spss}}
#'            }
#' 
#' @param var A (numeric or atomic) variable.
#' @return A factor variable, including variable and value labels.
#' 
#' @note This function only works with vectors that have value and variable
#'        labels attached. This is automatically done by importing SPSS data sets
#'        with the \code{\link{read_spss}} function and can manually be applied
#'        with the \code{\link{set_val_labels}} and \code{\link{set_var_labels}}
#'        functions.
#' 
#' @examples
#' \dontrun{
#' data(efc)
#' # normal factor conversion, loses value attributes
#' efc$e42dep <- as.factor(efc$e42dep)
#' sjt.frq(efc$e42dep)
#' 
#' data(efc)
#' # factor conversion, which keeps value attributes
#' efc$e42dep <- to_fac(efc$e42dep)
#' sjt.frq(efc$e42dep)}
#' 
#' @export
to_fac <- function(var) {
  # retrieve value labels
  lab <- get_val_labels(var)
  # retrieve variable labels
  varlab <- get_var_labels(var)
  # convert variable to factor
  var <- as.factor(var)
  # set back value labels
  var <- set_val_labels(var, lab)
  # set back variable labels
  var <- set_var_labels(var, varlab)
  return (var)
}


#' @title Converts factors to numeric variables
#' @name to_value
#' 
#' @description This function converts (replaces) factor values with the
#' related factor level index number, thus the factor is converted to 
#' a numeric variable.
#' 
#' @seealso \itemize{
#'            \item \code{\link{to_fac}}
#'            \item \code{\link{to_label}}
#'            \item \code{\link{get_val_labels}}
#'            \item \code{\link{get_var_labels}}
#'            \item \code{\link{read_spss}}
#'            }
#'            
#' @param fac A (factor) variable.
#' @param startAt the starting index, i.e. numeric value of the variable.
#' @return A numeric variable with values ranging from \code{startAt} to
#'           \code{startAt} + length of factor levels.
#' 
#' @examples
#' data(efc)
#' test <- to_label(efc$e42dep)
#' table(test)
#' 
#' table(to_value(test))
#' hist(to_value(test, 0))
#' 
#' @export
to_value <- function(fac, startAt=1) {
  l <- length(levels(fac))
  end <- startAt+l-1
  levels(fac) <- c(startAt:end)
  return (as.numeric(as.character(fac)))
}
