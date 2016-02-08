context("sjt.xtab")

test_that("Simple xtab", {
  skip_on_cran()
  sjt.xtab(efc$e42dep, efc$e16sex)
})

test_that("xtab", {
  skip_on_cran()
  sjt.xtab(efc$e42dep, 
           efc$e16sex,
           digits = 2,
           showCellPerc = T,
           showNA = T,
           showRowPerc = T,
           showColPerc = T,
           showExpected = T,
           useViewer = F)
})

test_that("xtab", {
  skip_on_cran()
  sjt.xtab(efc$e42dep, 
           efc$e16sex,
           showHorizontalLine = T)
})

efc.labels <- get_labels(efc)

test_that("Manual labels", {
  skip_on_cran()
  # print cross table with manually set
  # labels and expected values
  sjt.xtab(efc$e16sex, 
           efc$e42dep, 
           variableLabels = c("Elder's gender", 
                              "Elder's dependency"),
           valueLabels = list(efc.labels[['e16sex']], 
                              efc.labels[['e42dep']]),
           showExpected = TRUE,
           useViewer = F)
})

test_that("xtab", {
  skip_on_cran()
  # print minimal cross table with labels, total col/row highlighted
  sjt.xtab(efc$e16sex, efc$e42dep, 
           variableLabels = c("Gender", "Dependency"),
           valueLabels = list(c("m", "w"),
                              c("none", "slight", "moderate", "severe")),
           CSS = list(css.table = "border: 1px solid green",
                      css.thead = "border: 1px solid red",
                      css.tdata = "border: 1px dotted blue",
                      css.secondtablerow = "background-color: #cccccc",
                      css.lasttablerow = "background-color: #ffaaaa"),
           useViewer = F)
})

test_that("xtab", {
  skip_on_cran()
  # print minimal cross table with labels, total col/row highlighted
  sjt.xtab(efc$e16sex, efc$e42dep, 
           variableLabels = c("Gender", "Dependency"),
           valueLabels = list(c("m", "w"),
                              c("none", "slight", "moderate", "severe")),
           showCellPerc = T,
           showExpected = T,
           showColPerc = T,
           showRowPerc = T,
           showSummary = T,
           CSS = list(css.table = "border: 1px solid green",
                      css.thead = "border: 1px solid red",
                      css.tdata = "border: 1px dotted blue",
                      css.secondtablerow = "background-color: #cccccc",
                      css.lasttablerow = "background-color: #ffaaaa"),
           useViewer = F)
})

test_that("xtab", {
  skip_on_cran()
  # print minimal cross table with labels, total col/row highlighted
  sjt.xtab(efc$e16sex, efc$e42dep, 
           showHorizontalLine = FALSE,
           showCellPerc = FALSE,
           highlightTotal = TRUE,
           useViewer = F)
})


test_that("xtab", {
  skip_on_cran()
  # ---------------------------------------------------------------- 
  # User defined style sheet
  # ---------------------------------------------------------------- 
  sjt.xtab(efc$e16sex, efc$e42dep, 
           CSS = list(css.table = "border: 2px solid;",
                      css.tdata = "border: 1px solid;",
                      css.horline = "border-bottom: double blue;"),
           useViewer = F)
})