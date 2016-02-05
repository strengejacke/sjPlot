test_that("Simple frequencies", {
  sjp.frq(efc$e42dep)
})

test_that("Frequencies, sorted", {
sjp.frq(efc$e15relat,
        title = "Relationship",
        sort.frq = "desc",
        geom.size = .4,
        geom.colors = "#0088ff",
        axisLabels.x = get_labels(efc$e15relat))
})
  
test_that("Simple frequencies", {
  # Expect Warning
  expect_warning(sjp.frq(efc$e17age, interactionVar = efc$c161sex))
})
        
test_that("Boxplot with interaction", {
  sjp.frq(efc$e17age, 
          interactionVar = efc$c161sex,
          type = "box",
          axisTitle.x = get_label(efc$e17age),
          geom.colors = "yellow")  
})

test_that("Violin plot", {
  sjp.frq(efc$c12hour, 
          type = "violin",
          innerBoxPlotWidth = .1,
          innerBoxPlotDotSize = 2)  
})

test_that("Histogram with norm curves", {
  sjp.frq(efc$c160age,
          type = "h",
          showMeanIntercept = T,
          showMeanValue = T,
          showNormalCurve = T,
          showStandardDeviation = T,
          showStandardNormalCurve = T,
          normalCurveColor = "blue",
          normalCurveSize = 3,
          axisLimits.y = c(0,50))
})

test_that("Density with norm curves", {
  sjp.frq(efc$c160age,
          type = "dens",
          showMeanIntercept = T,
          showMeanValue = T,
          showNormalCurve = T,
          showStandardDeviation = T,
          showStandardNormalCurve = T,
          normalCurveColor = "blue",
          normalCurveSize = 3)
})

test_that("Dots, flipped", {
  sjp.frq(efc$e42dep,
          type = "dots",
          showCI = T,
          error.bar.color = "blue",
          coord.flip = T,
          geom.size = 3)
})

test_that("Bars and SE", {
  sjp.frq(efc$e42dep,
          showCI = T,
          error.bar.color = "blue")
})
