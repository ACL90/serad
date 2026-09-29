test_that("format_niv formate les niveaux et les différences", {

  nbsp <- "\u00a0"
  moins <- getOption("serad")$moins

  expect_equal(
    format_niv(365484, detail = -2),
    paste0("365", nbsp, "500")
  )

  expect_equal(
    format_niv(365484 - 300000, signe = TRUE, detail = -2),
    paste0("+65", nbsp, "500")
  )

  expect_equal(
    format_niv(300000 - 365484, signe = TRUE, detail = -2),
    paste0(moins, "65", nbsp, "500")
  )

})
