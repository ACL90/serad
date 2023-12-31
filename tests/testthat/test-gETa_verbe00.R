test_that("gETa_verbe00", {
  expect_equal(gETa_verbe00(0.049,0.049) , "A")
  expect_equal(gETa_verbe00(0.049,2) , "B")
  expect_equal(gETa_verbe00(10,1) , "E")
  expect_equal(gETa_verbe00(4,1) , "E")
  expect_equal(gETa_verbe00(1,1) , "G")
  expect_equal(gETa_verbe00(0.3,1) , "F")
  expect_equal(gETa_verbe00(0.1,-1) , "C")
  expect_equal(gETa_verbe00(-0.1,-1) , "K")
  expect_equal(gETa_verbe00(-0.3,-1) , "K")
  expect_equal(gETa_verbe00(-1,-1) , "K")
  expect_equal(gETa_verbe00(-4,-1) , "J")
  expect_equal(gETa_verbe00(-4,1) , "H")
  expect_equal(gETa_verbe00(-20,1) , "M")
  expect_equal(gETa_verbe00(-21,1) , "L")
})

