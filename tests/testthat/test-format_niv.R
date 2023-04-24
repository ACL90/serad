test_that("ex1", {
  expect_equal(format_niv(365484,detail=-2), paste0("365","\ua0","500"))
})
