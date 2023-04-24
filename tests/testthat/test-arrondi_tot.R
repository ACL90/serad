test_that("ex1", {
  expect_equal(arrondi_tot(1877.85,digit=0), 1878)
})

test_that("ex2", {
  expect_equal(arrondi_tot(1877.85,digit=1), 1877.9)
})


test_that("ex3", {
  expect_equal(arrondi_tot(1877.85,digit=2), 1877.85)
})


test_that("ex4", {
  expect_equal(arrondi_tot(1877.85,digit=-1), 1880)
})


test_that("ex5", {
  expect_equal(arrondi_tot(1877.85,digit=-2), 1900)
})
