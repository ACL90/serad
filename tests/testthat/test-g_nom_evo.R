test_that("g_nom_evo - classification des niveaux", {

  expect_equal(g_nom_evo(8),   "une forte hausse")
  expect_equal(g_nom_evo(4),   "une hausse")
  expect_equal(g_nom_evo(0.4, titre = TRUE), "Légère hausse")
  expect_equal(g_nom_evo(0.1), "une stabilité")

  expect_equal(g_nom_evo(-0.3), "une légère baisse")
  expect_equal(g_nom_evo(-1),   "une baisse")
  expect_equal(g_nom_evo(-4, titre = TRUE), "Baisse")
  expect_equal(g_nom_evo(-7),   "une forte baisse")

})
