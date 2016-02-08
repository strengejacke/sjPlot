context("sjp.frq")

test_that("Simple frequencies", {
  skip_on_cran()
  sjp.frq(efc$e42dep)
})

test_that("Frequencies, sorted", {
  skip_on_cran()
  sjp.frq(efc$e15relat,
        title = "Relationship",
        sort.frq = "desc",
        geom.size = .4,
        geom.colors = "#0088ff",
        axisLabels.x = get_labels(efc$e15relat))
})
  
test_that("Simple frequencies", {
  skip_on_cran()
  # Expect Warning
  expect_warning(sjp.frq(efc$e17age, interactionVar = efc$c161sex))
})
        
test_that("Boxplot with interaction", {
  skip_on_cran()
  sjp.frq(efc$e17age, 
          interactionVar = efc$c161sex,
          type = "box",
          axisTitle.x = get_label(efc$e17age),
          geom.colors = "yellow")  
})

test_that("Violin plot", {
  skip_on_cran()
  sjp.frq(efc$c12hour, 
          type = "violin",
          innerBoxPlotWidth = .1,
          innerBoxPlotDotSize = 2)  
})

test_that("Histogram with norm curves", {
  skip_on_cran()
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
  skip_on_cran()
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
  skip_on_cran()
  sjp.frq(efc$e42dep,
          type = "dots",
          showCI = T,
          error.bar.color = "blue",
          coord.flip = T,
          geom.size = 3)
})

test_that("Bars and SE", {
  skip_on_cran()
  sjp.frq(efc$e42dep,
          showCI = T,
          error.bar.color = "blue")
})

test_that("Boxplot", {
  skip_on_cran()
  # boxplot
  sjp.frq(ChickWeight$weight, type = "box")
})

test_that("Histogram", {
  skip_on_cran()
  # histogram
  sjp.frq(discoveries, type = "hist", showMeanIntercept = TRUE)
})

test_that("Violin plot", {
  skip_on_cran()
  # violin plot
  sjp.frq(ChickWeight$weight, type = "v")
})

test_that("Barplot", {
  skip_on_cran()
  # bar plot
  sjp.frq(ChickWeight$Diet)
})

test_that("Grouped bars", {
  skip_on_cran()
  # bar plot with EUROFAMCARE sample dataset
  # grouped variable
  ageGrp <- group_var(efc$e17age)
  ageGrpLab <- group_labels(efc$e17age)
  sjp.frq(ageGrp,
          title = get_label(efc$e17age),
          axisLabels.x = ageGrpLab)
})

test_that("Interaction", {
  skip_on_cran()
  # box plots with interaction variable
  # the following example is equal to the function call
  # sjp.grpfrq(efc$e17age, efc$e16sex, type = "box")
  sjp.frq(efc$e17age,
          title = paste(get_label(efc$e17age), 
                        "by", 
                        get_label(efc$e16sex),
                        interactionVar = efc$e16sex,
                        interactionVarLabels = get_labels(efc$e16sex),
                        type = "box"))
})

test_that("Dotplot", {
  skip_on_cran()
  # plotting confidence intervals
  sjp.frq(efc$e15relat,
          type = "dots",
          showCI = TRUE,
          sort.frq = "desc",
          coord.flip = TRUE,
          expand.grid = TRUE, # for text labels
          vjust = "bottom",   # for text labels
          hjust = "left")     # for text labels
})
