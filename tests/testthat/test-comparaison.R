# ===============================
# comparaison
# ===============================

test_that("comparaison - cas hausse", {
  expect_equal(
    comparaison(1.04, 1, "augmente", "reste stable", "diminue"),
    "augmente"
  )
})

test_that("comparaison - cas egalite", {
  expect_equal(
    comparaison(0.9991, 1, "augmente", "reste stable", "diminue"),
    "reste stable"
  )
})

test_that("comparaison - cas baisse", {
  expect_equal(
    comparaison(0.999, 1, "augmente", "reste stable", "diminue"),
    "diminue"
  )
})

test_that("comparaison - seuil nul", {
  expect_equal(
    comparaison(0.9991, 1, "augmente", "reste stable", "diminue", seuil = 0),
    "diminue"
  )

  expect_equal(
    comparaison(1, 1, "augmente", "reste stable", "diminue", seuil = 0),
    "augmente"
  )
})
