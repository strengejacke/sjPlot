context("sjt.xtab")

test_that("Simple xtab", {
  sjt.xtab(efc$e42dep, efc$e16sex)
})

test_that("xtab", {
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
  sjt.xtab(efc$e42dep, 
           efc$e16sex,
           showHorizontalLine = T)
})

test_that("Manual labels", {
  efc.labels <- get_labels(efc)
  # print cross table with manually set
  # labels and expected values
  sjt.xtab(efc$e16sex, 
           efc$e42dep, 
           variableLabels = c("Elder's gender", 
                              "Elder's dependency"),
           valueLabels = list(efc.labels[['e16sex']], 
                              efc.labels[['e42dep']]),
           showExpected = TRUE)
})

test_that("xtab", {
  # print minimal cross table with labels, total col/row highlighted
  sjt.xtab(efc$e16sex, efc$e42dep, 
           showHorizontalLine = FALSE,
           showCellPerc = FALSE,
           highlightTotal = TRUE)
})

test_that("xtab", {
  # ---------------------------------------------------------------- 
  # User defined style sheet
  # ---------------------------------------------------------------- 
  sjt.xtab(efc$e16sex, efc$e42dep, 
           CSS = list(css.table = "border: 2px solid;",
                      css.tdata = "border: 1px solid;",
                      css.horline = "border-bottom: double blue;"))
})