test_that("Check plot frq", { 
  skip_on_cran()
  library(sjmisc)
  data(efc)
  
  sjp.frq(efc$e42dep)
  
  sjp.frq(efc$e15relat,
          title = "Relationship",
          sort.frq = "desc",
          geom.size = .4,
          geom.colors = "#0088ff",
          axisLabels.x = get_val_labels(efc$e15relat))

  sjp.frq(efc$e17age, interactionVar = efc$c161sex)
          
  sjp.frq(efc$e17age, 
          interactionVar = efc$c161sex,
          type = "box",
          axisTitle.x = get_var_labels(efc$e17age),
          geom.colors = "yellow")  

  sjp.frq(efc$c12hour, 
          type = "violin",
          innerBoxPlotWidth = .1,
          innerBoxPlotDotSize = 2)  
  
  sjp.frq(efc$c160age,
          type = "h",
          showMeanIntercept = T,
          showMeanValue = T,
          showNormalCurve = T,
          showStandardDeviation = T,
          showStandardNormalCurve = T,
          normalCurveColor = "blue",
          normalCurveSize = 3)

  sjp.frq(efc$c160age,
          type = "dens",
          showMeanIntercept = T,
          showMeanValue = T,
          showNormalCurve = T,
          showStandardDeviation = T,
          showStandardNormalCurve = T,
          normalCurveColor = "blue",
          normalCurveSize = 3)

  sjp.frq(efc$e42dep,
          type = "dots",
          showCI = T,
          error.bar.color = "blue",
          coord.flip = T,
          geom.size = 3)
  
  sjp.frq(efc$e42dep,
          showCI = T,
          error.bar.color = "blue")

})