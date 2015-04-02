test_that("Check plot grouped frq", { 
  skip_on_cran()
  library(sjmisc)
  data(efc)
  
  sjp.grpfrq(efc$e42dep, efc$e16sex)

  sjp.grpfrq(efc$c12hour, 
             efc$e16sex,
             interactionVar = efc$c172code,
             type = "v",
             innerBoxPlotWidth = .05,
             innerBoxPlotDotSize = 2)

  sjp.grpfrq(efc$e17age, 
             efc$e16sex,
             type = "lines",
             smoothLines = T,
             showValueLabels = F)


  sjp.grpfrq(efc$e17age, 
             efc$e42dep,
             type = "hist",
             facet.grid = T,
             showMeanIntercept = T,
             showValueLabels = F,
             showMeanValue = T,
             showStandardDeviation = T,
             showGroupCount = T,
             meanInterceptLineType = 3,
             meanInterceptLineSize = 2)  

  sjp.grpfrq(efc$e17age, 
             efc$e42dep,
             type = "hist",
             facet.grid = T,
             showMeanIntercept = T,
             showStandardDeviation = T,
             showValueLabels = F,
             showMeanValue = F)  
  
})