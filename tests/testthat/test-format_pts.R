test_that("format_pts", {
  expect_equal(format_pts(5.3654,signe=0),paste0("5,4","\ua0","points"))
  expect_equal(format_pts(1.3654,signe=0),paste0("1,4","\ua0","point"))
  expect_equal(format_pts(5.3654,abrev=1),paste0("+5,4","\ua0","pts"))
  expect_equal(format_pts(5.3654,1),paste0("+5,4","\ua0","points"))
  expect_equal(format_pts(-5.3654,0),paste0("5,4","\ua0","points"))
  expect_equal(format_pts(-5.3654,1),paste0("−5,4","\ua0","points"))
  expect_equal(format_pts(-5.3654),paste0("−5,4","\ua0","points"))
  expect_equal(format_pts(-5.3654,detail = 2),paste0("−5,37","\ua0","points"))
  expect_equal(format_pts(0.35),paste0("+0,4","\ua0","point"))

})
