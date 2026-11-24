test_that("view_df preserves labels while applying inline table styles", {
  data <- data.frame(group = c("a", "b"), march = 1:2, stringsAsFactors = FALSE)
  attr(data$march, "label") <- "Search in March class=tdata"
  attr(data$march, "labels") <- c(March = 1L, April = 2L)
  comment(data$march) <- "March class=thead"

  result <- view_df(data, show.values = FALSE, use.viewer = FALSE)

  expect_match(result$knitr, ">Search in March class=tdata</td>", fixed = TRUE)
  expect_match(result$knitr, ">March<br>April</td>", fixed = TRUE)
  expect_match(result$knitr, 'title="March class=thead">march</td>', fixed = TRUE)
  expect_match(result$knitr, 'background-color:#eeeeee"', fixed = TRUE)
  expect_match(result$page.content, '<td class="tdata arc"', fixed = TRUE)
})

test_that("view_df honors custom inline styles for striped and omitted cells", {
  data <- data.frame(id = 1:2)
  data$group <- list("a", "b")
  result <- view_df(
    data,
    show.na = TRUE,
    CSS = list(css.tdata = "padding:1px;", css.arc = "background-color:blue;", css.omit = "color:red;"),
    use.viewer = FALSE
  )

  expect_match(result$knitr, '<td style="padding:1px;">', fixed = TRUE)
  expect_match(result$knitr, '<td style="padding:1px; background-color:blue;">', fixed = TRUE)
  expect_match(result$knitr, '<span style="color:red;"', fixed = TRUE)
})
