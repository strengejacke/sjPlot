get_table_header <- function(encoding, cell.spacing, cell.gpr.indent, p.numeric, show.header, CSS) {
  return(table_style_worker(NULL, encoding, cell.spacing, cell.gpr.indent, p.numeric, show.header, CSS, option = 1))
}


table_cell_string <- function(page.content, cell.prefix, cell.class, col.nr, cell.value) {
  return(paste0(page.content, sprintf("%s<td class=\"tdata centeralign %smodelcolumn%i\">%s</td>",
                                      cell.prefix, cell.class, col.nr, cell.value)))
}


get_table_response_label <- function(page.content, depvar.labels, input_list, tcp, headerColSpanFactor, sep.column) {
  # -------------------------------------
  # set default dependent var label
  # -------------------------------------
  if (is.null(depvar.labels)) {
    # here we try to find variable labels for each
    # response vector. if we found a label for *all*
    # responses, we use these as labels for the dependent
    # variables, in the table column headers
    depvar.labels <- c()
    # iterate models
    for (i in seq_len(length(input_list))) {
      # get model data
      m_d <- get_lm_data(input_list[[i]])
      # get model response
      resp_vec <- m_d$resp
      resp_name <- m_d$resp.label
      # get label
      depvar.labels <- c(depvar.labels,
                         sjlabelled::get_label(resp_vec, def.value = resp_name))
    }
  }
  # -------------------------------------
  # continue with model-labels (dependent variables)
  # which are the heading for each model column
  # -------------------------------------
  for (i in seq_len(length(depvar.labels))) {
    # -------------------------
    # insert "separator column"
    # -------------------------
    if (sep.column) page.content <- paste0(page.content, sprintf("\n    <td class=\"labelcellborder separatorcol%s\">&nbsp;</td>", tcp))
    if (headerColSpanFactor > 1) {
      page.content <- paste0(page.content,
                             sprintf("\n    <td class=\"tdata centeralign labelcellborder%s\" colspan=\"%i\">%s</td>",
                                     tcp,
                                     headerColSpanFactor,
                                     depvar.labels[i]))
    } else {
      page.content <- paste0(page.content,
                             sprintf("\n    <td class=\"tdata centeralign labelcellborder%s\">%s</td>",
                                     tcp,
                                     depvar.labels[i]))
    }
  }
  page.content <- paste0(page.content, "\n  </tr>")

  return(list(page.content = page.content, depvar.labels = depvar.labels))
}




replace_css_styles <- function(page.content, cell.spacing, cell.gpr.indent, p.numeric, show.header, CSS) {
  return(table_style_worker(page.content, NULL, cell.spacing, cell.gpr.indent, p.numeric, show.header, CSS, option = 2))
}


get_table_css_styles <- function(cell.spacing, cell.gpr.indent, p.numeric, show.header, CSS) {
  return(table_style_worker(NULL, NULL, cell.spacing, cell.gpr.indent, p.numeric, show.header, CSS, option = 3))
}

table_style_worker <- function(page.content, encoding, cell.spacing, cell.gpr.indent,
                               p.numeric, show.header, CSS, option) {
  # -------------------------------------
  # init style sheet and tags used for css-definitions
  # we can use these variables for string-replacement
  # later for return value
  # -------------------------------------
  tag.table <- "table"
  tag.thead <- "thead"
  tag.tdata <- "tdata"
  tag.separatorcol <- "separatorcol"
  tag.summary <- "summary"
  tag.fixedparts <- "fixedparts"
  tag.randomparts <- "randomparts"
  tag.colnames <- "colnames"
  tag.firstsumrow <- "firstsumrow"
  tag.labelcellborder <- "labelcellborder"
  tag.lasttablerow <- "lasttablerow"
  tag.depvarhead <- "depvarhead"
  tag.topborder <- "topborder"
  tag.topcontentborder <- "topcontentborder"
  tag.annorow <- "annorow"
  tag.noannorow <- "noannorow"
  tag.annostyle <- "annostyle"
  tag.leftalign <- "leftalign"
  tag.centeralign <- "centeralign"
  tag.grouprow <- "grouprow"
  tag.tgrpdata <- "tgrpdata"
  tag.modelcolumn1 <- "modelcolumn1"
  tag.modelcolumn2 <- "modelcolumn2"
  tag.modelcolumn3 <- "modelcolumn3"
  tag.modelcolumn4 <- "modelcolumn4"
  tag.modelcolumn5 <- "modelcolumn5"
  tag.modelcolumn6 <- "modelcolumn6"
  tag.modelcolumn7 <- "modelcolumn7"
  css.table <- "border-collapse:collapse; border:none;"
  css.thead <- sprintf("border-bottom: 1px solid; padding:%.1fcm;", cell.spacing)
  css.tdata <- sprintf("padding:%.1fcm;", cell.spacing)
  css.separatorcol <- "padding-left:0.5em; padding-right:0.5em;"
  css.summary <- "padding-top:0.1cm; padding-bottom:0.1cm;"
  css.fixedparts <- "font-weight:bold; text-align:left;"
  css.randomparts <- "font-weight:bold; text-align:left; padding-top:0.5em;"
  css.colnames <- "font-style:italic;"
  css.firstsumrow <- "border-top:1px solid;"
  css.labelcellborder <- "border-bottom:1px solid;"
  css.lasttablerow <- "border-bottom: double;"
  css.topborder <- "border-top:double;"
  css.depvarhead <- "text-align:center; border-bottom:1px solid;"
  css.topcontentborder <- "border-top:1px solid;"
  css.annorow <- "border-top:1px solid;"
  css.noannorow <- "border-bottom:double;"
  css.annostyle <- "text-align:right;"
  css.leftalign <- "text-align:left;"
  css.centeralign <- "text-align:center;"
  css.grouprow <- sprintf("padding:%.1fcm;", cell.spacing)
  css.tgrpdata <- sprintf("font-style:italic; padding:%.1fcm; padding-left:%.1fcm;", cell.spacing, cell.gpr.indent)
  css.modelcolumn1 <- ""
  css.modelcolumn2 <- ""
  css.modelcolumn3 <- ""
  css.modelcolumn4 <- ""
  css.modelcolumn5 <- ""
  css.modelcolumn6 <- ""
  css.modelcolumn7 <- ""
  # change table style if we have pvalues as numbers
  if (p.numeric) css.table <- sprintf("%s%s", css.table, css.noannorow)
  if (show.header) css.labelcellborder <- ""
  # ------------------------
  # check user defined style sheets
  # ------------------------
  if (!is.null(CSS)) {
    if (!is.null(CSS[['css.table']])) css.table <- ifelse(substring(CSS[['css.table']], 1, 1) == '+', paste0(css.table, substring(CSS[['css.table']], 2)), CSS[['css.table']])
    if (!is.null(CSS[['css.thead']])) css.thead <- ifelse(substring(CSS[['css.thead']], 1, 1) == '+', paste0(css.thead, substring(CSS[['css.thead']], 2)), CSS[['css.thead']])
    if (!is.null(CSS[['css.tdata']])) css.tdata <- ifelse(substring(CSS[['css.tdata']], 1, 1) == '+', paste0(css.tdata, substring(CSS[['css.tdata']], 2)), CSS[['css.tdata']])
    if (!is.null(CSS[['css.separatorcol']])) css.separatorcol <- ifelse(substring(CSS[['css.separatorcol']], 1, 1) == '+', paste0(css.separatorcol, substring(CSS[['css.separatorcol']], 2)), CSS[['css.separatorcol']])
    if (!is.null(CSS[['css.leftalign']])) css.leftalign <- ifelse(substring(CSS[['css.leftalign']], 1, 1) == '+', paste0(css.leftalign, substring(CSS[['css.leftalign']], 2)), CSS[['css.leftalign']])
    if (!is.null(CSS[['css.centeralign']])) css.centeralign <- ifelse(substring(CSS[['css.centeralign']], 1, 1) == '+', paste0(css.centeralign, substring(CSS[['css.centeralign']], 2)), CSS[['css.centeralign']])
    if (!is.null(CSS[['css.summary']])) css.summary <- ifelse(substring(CSS[['css.summary']], 1, 1) == '+', paste0(css.summary, substring(CSS[['css.summary']], 2)), CSS[['css.summary']])
    if (!is.null(CSS[['css.fixedparts']])) css.fixedparts <- ifelse(substring(CSS[['css.fixedparts']], 1, 1) == '+', paste0(css.fixedparts, substring(CSS[['css.fixedparts']], 2)), CSS[['css.fixedparts']])
    if (!is.null(CSS[['css.randomparts']])) css.randomparts <- ifelse(substring(CSS[['css.randomparts']], 1, 1) == '+', paste0(css.randomparts, substring(CSS[['css.randomparts']], 2)), CSS[['css.randomparts']])
    if (!is.null(CSS[['css.lasttablerow']])) css.lasttablerow <- ifelse(substring(CSS[['css.lasttablerow']], 1, 1) == '+', paste0(css.lasttablerow, substring(CSS[['css.lasttablerow']], 2)), CSS[['css.lasttablerow']])
    if (!is.null(CSS[['css.labelcellborder']])) css.labelcellborder <- ifelse(substring(CSS[['css.labelcellborder']], 1, 1) == '+', paste0(css.table, substring(CSS[['css.labelcellborder']], 2)), CSS[['css.labelcellborder']])
    if (!is.null(CSS[['css.colnames']])) css.colnames <- ifelse(substring(CSS[['css.colnames']], 1, 1) == '+', paste0(css.colnames, substring(CSS[['css.colnames']], 2)), CSS[['css.colnames']])
    if (!is.null(CSS[['css.firstsumrow']])) css.firstsumrow <- ifelse(substring(CSS[['css.firstsumrow']], 1, 1) == '+', paste0(css.firstsumrow, substring(CSS[['css.firstsumrow']], 2)), CSS[['css.firstsumrow']])
    if (!is.null(CSS[['css.topborder']])) css.topborder <- ifelse(substring(CSS[['css.topborder']], 1, 1) == '+', paste0(css.topborder, substring(CSS[['css.topborder']], 2)), CSS[['css.topborder']])
    if (!is.null(CSS[['css.depvarhead']])) css.depvarhead <- ifelse(substring(CSS[['css.depvarhead']], 1, 1) == '+', paste0(css.depvarhead, substring(CSS[['css.depvarhead']], 2)), CSS[['css.depvarhead']])
    if (!is.null(CSS[['css.topcontentborder']])) css.topcontentborder <- ifelse(substring(CSS[['css.topcontentborder']], 1, 1) == '+', paste0(css.topcontentborder, substring(CSS[['css.topcontentborder']], 2)), CSS[['css.topcontentborder']])
    if (!is.null(CSS[['css.annorow']])) css.annorow <- ifelse(substring(CSS[['css.annorow']], 1, 1) == '+', paste0(css.annorow, substring(CSS[['css.annorow']], 2)), CSS[['css.annorow']])
    if (!is.null(CSS[['css.noannorow']])) css.noannorow <- ifelse(substring(CSS[['css.noannorow']], 1, 1) == '+', paste0(css.noannorow, substring(CSS[['css.noannorow']], 2)), CSS[['css.noannorow']])
    if (!is.null(CSS[['css.annostyle']])) css.annostyle <- ifelse(substring(CSS[['css.annostyle']], 1, 1) == '+', paste0(css.annostyle, substring(CSS[['css.annostyle']], 2)), CSS[['css.annostyle']])
    if (!is.null(CSS[['css.grouprow']])) css.grouprow <- ifelse(substring(CSS[['css.grouprow']], 1, 1) == '+', paste0(css.grouprow, substring(CSS[['css.grouprow']], 2)), CSS[['css.grouprow']])
    if (!is.null(CSS[['css.tgrpdata']])) css.tgrpdata <- ifelse(substring(CSS[['css.tgrpdata']], 1, 1) == '+', paste0(css.tgrpdata, substring(CSS[['css.tgrpdata']], 2)), CSS[['css.tgrpdata']])
    if (!is.null(CSS[['css.modelcolumn1']])) css.modelcolumn1 <- ifelse(substring(CSS[['css.modelcolumn1']], 1, 1) == '+', paste0(css.modelcolumn1, substring(CSS[['css.modelcolumn1']], 2)), CSS[['css.modelcolumn1']])
    if (!is.null(CSS[['css.modelcolumn2']])) css.modelcolumn2 <- ifelse(substring(CSS[['css.modelcolumn2']], 1, 1) == '+', paste0(css.modelcolumn2, substring(CSS[['css.modelcolumn2']], 2)), CSS[['css.modelcolumn2']])
    if (!is.null(CSS[['css.modelcolumn3']])) css.modelcolumn3 <- ifelse(substring(CSS[['css.modelcolumn3']], 1, 1) == '+', paste0(css.modelcolumn3, substring(CSS[['css.modelcolumn3']], 2)), CSS[['css.modelcolumn3']])
    if (!is.null(CSS[['css.modelcolumn4']])) css.modelcolumn4 <- ifelse(substring(CSS[['css.modelcolumn4']], 1, 1) == '+', paste0(css.modelcolumn4, substring(CSS[['css.modelcolumn4']], 2)), CSS[['css.modelcolumn4']])
    if (!is.null(CSS[['css.modelcolumn5']])) css.modelcolumn5 <- ifelse(substring(CSS[['css.modelcolumn5']], 1, 1) == '+', paste0(css.modelcolumn5, substring(CSS[['css.modelcolumn5']], 2)), CSS[['css.modelcolumn5']])
    if (!is.null(CSS[['css.modelcolumn6']])) css.modelcolumn6 <- ifelse(substring(CSS[['css.modelcolumn6']], 1, 1) == '+', paste0(css.modelcolumn6, substring(CSS[['css.modelcolumn6']], 2)), CSS[['css.modelcolumn6']])
    if (!is.null(CSS[['css.modelcolumn7']])) css.modelcolumn7 <- ifelse(substring(CSS[['css.modelcolumn7']], 1, 1) == '+', paste0(css.modelcolumn7, substring(CSS[['css.modelcolumn7']], 2)), CSS[['css.modelcolumn7']])
  }
  # ------------------------
  # set page style
  # ------------------------
  page.style <-  sprintf("<style>\nhtml, body { background-color: white; }\n%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n</style>",
                         tag.table, css.table, tag.thead, css.thead, tag.tdata, css.tdata,
                         tag.summary, css.summary, tag.colnames, css.colnames,
                         tag.firstsumrow, css.firstsumrow, tag.lasttablerow, css.lasttablerow,
                         tag.topborder, css.topborder, tag.depvarhead, css.depvarhead,
                         tag.topcontentborder, css.topcontentborder, tag.annorow, css.annorow,
                         tag.noannorow, css.noannorow, tag.annostyle, css.annostyle,
                         tag.labelcellborder, css.labelcellborder,
                         tag.centeralign, css.centeralign, tag.leftalign, css.leftalign,
                         tag.grouprow, css.grouprow, tag.tgrpdata, css.tgrpdata,
                         tag.modelcolumn1, css.modelcolumn1,
                         tag.modelcolumn2, css.modelcolumn2,
                         tag.modelcolumn3, css.modelcolumn3,
                         tag.modelcolumn4, css.modelcolumn4,
                         tag.modelcolumn5, css.modelcolumn5,
                         tag.modelcolumn6, css.modelcolumn6,
                         tag.modelcolumn7, css.modelcolumn7,
                         tag.fixedparts, css.fixedparts,
                         tag.randomparts, css.randomparts,
                         tag.separatorcol, css.separatorcol)


  if (option == 1) {
    # ------------------------
    # set page encoding
    # ------------------------
    toWrite <- sprintf("<html>\n<head>\n<meta http-equiv=\"Content-type\" content=\"text/html;charset=%s\">\n", encoding)
    # ------------------------
    # start content
    # ------------------------
    toWrite <- paste0(toWrite, page.style)
    toWrite <- paste0(toWrite, "\n</head>\n<body>\n")

    return(toWrite)
  } else if (option == 2) {
    # -------------------------------------
    # replace class attributes with inline style,
    # useful for knitr
    # -------------------------------------
    # copy page content
    # -------------------------------------
    knitr <- page.content
    # -------------------------------------
    # set style attributes for main table tags
    # -------------------------------------
    knitr <- gsub("class=", "style=", knitr, fixed = TRUE, useBytes = TRUE)
    knitr <- gsub("<table", sprintf("<table style=\"%s\"", css.table), knitr, fixed = TRUE, useBytes = TRUE)
    # -------------------------------------
    # replace class-attributes with inline-style-definitions
    # -------------------------------------
    knitr <- gsub(tag.tdata, css.tdata, knitr, fixed = TRUE, useBytes = TRUE)
    knitr <- gsub(tag.thead, css.thead, knitr, fixed = TRUE, useBytes = TRUE)
    knitr <- gsub(tag.summary, css.summary, knitr, fixed = TRUE, useBytes = TRUE)
    knitr <- gsub(tag.fixedparts, css.fixedparts, knitr, fixed = TRUE, useBytes = TRUE)
    knitr <- gsub(tag.randomparts, css.randomparts, knitr, fixed = TRUE, useBytes = TRUE)
    knitr <- gsub(tag.separatorcol, css.separatorcol, knitr, fixed = TRUE, useBytes = TRUE)
    knitr <- gsub(tag.colnames, css.colnames, knitr, fixed = TRUE, useBytes = TRUE)
    knitr <- gsub(tag.leftalign, css.leftalign, knitr, fixed = TRUE, useBytes = TRUE)
    knitr <- gsub(tag.centeralign, css.centeralign, knitr, fixed = TRUE, useBytes = TRUE)
    knitr <- gsub(tag.firstsumrow, css.firstsumrow, knitr, fixed = TRUE, useBytes = TRUE)
    knitr <- gsub(tag.lasttablerow, css.lasttablerow, knitr, fixed = TRUE, useBytes = TRUE)
    knitr <- gsub(tag.labelcellborder, css.labelcellborder, knitr, fixed = TRUE, useBytes = TRUE)
    knitr <- gsub(tag.topborder, css.topborder, knitr, fixed = TRUE, useBytes = TRUE)
    knitr <- gsub(tag.depvarhead, css.depvarhead, knitr, fixed = TRUE, useBytes = TRUE)
    knitr <- gsub(tag.topcontentborder, css.topcontentborder, knitr, fixed = TRUE, useBytes = TRUE)
    knitr <- gsub(tag.noannorow, css.noannorow, knitr, fixed = TRUE, useBytes = TRUE)
    knitr <- gsub(tag.annorow, css.annorow, knitr, fixed = TRUE, useBytes = TRUE)
    knitr <- gsub(tag.annostyle, css.annostyle, knitr, fixed = TRUE, useBytes = TRUE)
    knitr <- gsub(tag.grouprow, css.grouprow, knitr, fixed = TRUE, useBytes = TRUE)
    knitr <- gsub(tag.tgrpdata, css.tgrpdata, knitr, fixed = TRUE, useBytes = TRUE)
    knitr <- gsub(tag.modelcolumn1, css.modelcolumn1, knitr, fixed = TRUE, useBytes = TRUE)
    knitr <- gsub(tag.modelcolumn2, css.modelcolumn2, knitr, fixed = TRUE, useBytes = TRUE)
    knitr <- gsub(tag.modelcolumn3, css.modelcolumn3, knitr, fixed = TRUE, useBytes = TRUE)
    knitr <- gsub(tag.modelcolumn4, css.modelcolumn4, knitr, fixed = TRUE, useBytes = TRUE)
    knitr <- gsub(tag.modelcolumn5, css.modelcolumn5, knitr, fixed = TRUE, useBytes = TRUE)
    knitr <- gsub(tag.modelcolumn6, css.modelcolumn6, knitr, fixed = TRUE, useBytes = TRUE)
    knitr <- gsub(tag.modelcolumn7, css.modelcolumn7, knitr, fixed = TRUE, useBytes = TRUE)

    return(knitr)
  } else if (option == 3) {
    # ------------------------
    # set page style
    # ------------------------
    return(page.style)
  }
}
