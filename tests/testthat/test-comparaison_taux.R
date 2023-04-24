test_that("ex", {
  expect_equal(comparaison_taux(5,"augmente","reste stable","diminue"), "augmente")
})

test_that("ex", {
  expect_equal(comparaison_taux(0.05,"augmente","reste stable","diminue"), "reste stable")
})

test_that("ex", {
  expect_equal(comparaison_taux(0.05,"augmente","reste stable","diminue",seuil=0), "augmente")
})

test_that("ex", {
  expect_equal(comparaison_taux(0,"augmente","reste stable","diminue",seuil=0), "augmente")
})

test_that("ex", {
  expect_equal(comparaison_taux(0,"as","bs","cs",seuil=0,param=1,hausse1="a",egalite1="b",baisse1="c"),
               "a")
})

test_that("ex", {
  expect_equal(comparaison_taux(0,"as","bs","cs",seuil=0,param=0,hausse1="a",egalite1="b",baisse1="c"),
               "as")
})

test_that("ex", {
  expect_equal(comparaison_taux(0,"as","bs","cs",seuil=0,hausse1="a",egalite1="b",baisse1="c"),
               "as")
})


