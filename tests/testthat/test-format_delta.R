test_that("ex1", {
  expect_equal( format_delta(365484), paste0("+365","\ua0","500"))
})

test_that("ex2", {
  expect_equal( format_delta(365484,0), paste0("365","\ua0","500"))
})

test_that("ex3", {
  expect_equal( format_delta(-365484), paste0("−365","\ua0","500"))
})

test_that("ex4", {
  expect_equal( format_delta(-365484,0,-1), paste0("365","\ua0","480"))
})



#' format_delta(365484)       #+365 500
#' format_delta(365484,0)     #365 500
#' format_delta(-365484)      #−365 500
#' format_delta(-365484,0,-1) #365 480
