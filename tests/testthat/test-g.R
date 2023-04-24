test_that("ex1", {
  expect_equal(g(2,1), 100)
})


test_that("ex3", {
  #it depend on the default value of serad0$eps=0.00000001
  #expect_equal(g(2,0), 2e+10)
  expect_warning(g(2,0),"division par 0 dans serad::g()")
})

#' g(2,1)  #100
#' g(2,0)  #2e+10 et message d'avis
