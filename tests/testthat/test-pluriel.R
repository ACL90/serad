test_that("s - gestion singulier/pluriel", {

  # ---- Cas simples ----
  expect_equal(pluriel(-7.5), "s")
  expect_equal(pluriel(-2),   "s")
  expect_equal(pluriel(1.97), "")

  # ---- Formes personnalisées ----
  expect_equal(
    pluriel(1.4, "chat parle", "chats parlent"),
    "chat parle"
  )

  expect_equal(
    pluriel(-2, "chat parle", "chats parlent"),
    "chats parlent"
  )

  # ---- Interaction avec arrondi_tot ----
  expect_equal(
    pluriel(arrondi_tot(1.97)),
    "s"
  )
})
