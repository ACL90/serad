test_that("format_g", {
  expect_equal(format_g(5.3654,signe=0),paste0("5,4","\ua0","%"))
  expect_equal(format_g(5.3654,1),paste0("+5,4","\ua0","%"))
  expect_equal(format_g(-5.3654,0),paste0("5,4","\ua0","%"))
  expect_equal(format_g(-5.3654,1),paste0("−5,4","\ua0","%"))
  expect_equal(format_g(-5.3654),paste0("−5,4","\ua0","%"))
  expect_equal(format_g(-5.3654,detail = 2),paste0("−5,37","\ua0","%"))
  expect_equal(format_g(0.35),paste0("+0,4","\ua0","%"))

})

