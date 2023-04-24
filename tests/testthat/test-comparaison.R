#comparaison
test_that("ex1", {
  expect_equal(comparaison(1.04,1,"augmente","reste stable","diminue"), "augmente")
})

test_that("ex2", {
  expect_equal(comparaison(0.9991,1,"augmente","reste stable","diminue"), "reste stable")
})

test_that("ex3", {
  expect_equal(comparaison(0.999,1,"augmente","reste stable","diminue"), "diminue")
})

test_that("ex4", {
  expect_equal(comparaison(0.9991,1,"augmente","reste stable","diminue",seuil = 0), "diminue")
})

test_that("ex5", {
  expect_equal(comparaison(1,1,"augmente","reste stable","diminue",seuil = 0), "augmente")
})

#audessus
test_that("ex1", {
  expect_equal(audessus(1.04,1)    , "au-dessus")
})
test_that("ex2", {
  expect_equal(audessus(1.04,1.04)    , "au-dessus")
})
test_that("ex3", {
  expect_equal(audessus(0.96,1)    , "en dessous")
})

#alahausse
test_that("ex1", {
  expect_equal(alahausse(1.004,1)    , "à la hausse")
})
test_that("ex2", {
  expect_equal(alahausse(0.996,1)    , "à la baisse")
})
test_that("ex3", {
  expect_equal(alahausse(1,1.0004)    , "inchangé")
})

#davantage
test_that("ex1", {
  expect_equal(davantage(1.04,1)    , "davantage")
})
test_that("ex2", {
  expect_equal(davantage(1.04,1.04) , "davantage")
})
test_that("ex3", {
  expect_equal(davantage(0.96,1)    , "moins")
})


#depasse
test_that("ex1", {
  expect_equal(depasse(1.04,1)    , "excèdent")
})
test_that("ex2", {
  expect_equal(depasse(1.04,1,sing=1)    , "excède")
})
test_that("ex3", {
  expect_equal(depasse(0.96,1)     , "sont en dessous de")
})
test_that("ex4", {
  expect_equal(depasse(0.9991,1)     , "sont au niveau de")
})

#g_nom_simple
test_that("ex1", {
  expect_equal(g_nom_simple(3,1)    , "en hausse de 200,0\ua0%")
})
test_that("ex1", {
  expect_equal(g_nom_simple(3,5)    , "en baisse de 40,0\ua0%")
})

