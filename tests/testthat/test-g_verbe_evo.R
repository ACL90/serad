test_that("g_verbe_evo - classification complète", {

  # ---- Hausse ----
  expect_equal(g_verbe_evo(10, stable_sans_valeur = FALSE),
               "augmente fortement de 10,0\ua0%")

  expect_equal(g_verbe_evo(4),
               "augmente de 4,0\ua0%")

  expect_equal(g_verbe_evo(1, sing = FALSE),
               "augmentent de 1,0\ua0%")

  expect_equal(g_verbe_evo(0.3),
               "augmente légèrement de 0,3\ua0%")

  # ---- Stabilité ----
  expect_equal(g_verbe_evo(-0.1, stable_sans_valeur = FALSE),
               "est stable à -0,1\ua0%")

  expect_equal(g_verbe_evo(-0.1, stable_sans_valeur = TRUE),
               "est stable")

  # ---- Baisse ----
  expect_equal(g_verbe_evo(-0.3),
               "baisse légèrement de 0,3\ua0%")

  expect_equal(g_verbe_evo(-4),
               "baisse de 4,0\ua0%")

  expect_equal(g_verbe_evo(-20),
               "baisse fortement de 20,0\ua0%")

})
