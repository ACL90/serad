test_that("ex1", {
  expect_equal(a(4,2,1), 0)
})

test_that("ex2", {
  expect_equal(a(6,2,1), 100)
})


test_that("ex3", {
  #it depend on the default value of serad0$eps=0.00000001
  #expect_equal(a(2,1,1), 1e+12)
  expect_warning(a(2,1,1),"division par 0 dans serad::g()")
})

