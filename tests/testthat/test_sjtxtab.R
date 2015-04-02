test_that("Check tab crosstabs", { 
  skip_on_cran()
  library(sjmisc)
  data(efc2)
  
  sjt.xtab(efc2$e42dep, efc2$e16sex)

  sjt.xtab(efc2$e42dep, 
           efc2$e16sex,
           efc2$c172code)
  
  sjt.xtab(efc2$e42dep, 
           efc2$e16sex,
           efc2$c172code,
           digits = 2,
           showCellPerc = T,
           showNA = T,
           showTotalN = T,
           showRowPerc = T,
           showColPerc = T,
           showExpected = T,
           useViewer = F)

  sjt.xtab(efc2$e42dep, 
           efc2$e16sex,
           showHorizontalLine = T)
  
})