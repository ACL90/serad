test_that("les mois sont correctement affiches en francais", {

  expect_equal(
    libelle_periode(
      1,
      2026,
      periode = "mois",
      lang = "fr"
    ),
    "janvier 2026"
  )

  expect_equal(
    libelle_periode(
      2,
      2026,
      periode = "mois",
      lang = "fr"
    ),
    "f\u00E9vrier 2026"
  )

  expect_equal(
    libelle_periode(
      8,
      2026,
      periode = "mois",
      lang = "fr"
    ),
    "ao\u00FBt 2026"
  )

  expect_equal(
    libelle_periode(
      12,
      2026,
      periode = "mois",
      lang = "fr"
    ),
    "d\u00E9cembre 2026"
  )
})


test_that("les mois sont correctement affiches en anglais", {

  expect_equal(
    libelle_periode(
      1,
      2026,
      periode = "mois",
      lang = "en"
    ),
    "January 2026"
  )

  expect_equal(
    libelle_periode(
      8,
      2026,
      periode = "mois",
      lang = "en"
    ),
    "August 2026"
  )
})


test_that("les trimestres sont affiches en lettres", {

  expect_equal(
    libelle_periode(
      1,
      2026,
      periode = "trimestre",
      format = "lettres",
      lang = "fr"
    ),
    "premier trimestre 2026"
  )

  expect_equal(
    libelle_periode(
      3,
      2026,
      periode = "trimestre",
      format = "lettres",
      lang = "fr"
    ),
    "troisi\u00E8me trimestre 2026"
  )

  expect_equal(
    libelle_periode(
      4,
      2026,
      periode = "trimestre",
      format = "lettres",
      lang = "en"
    ),
    "fourth quarter 2026"
  )
})


test_that("les trimestres sont affiches en chiffres", {

  expect_equal(
    libelle_periode(
      1,
      2026,
      periode = "trimestre",
      format = "chiffres",
      lang = "fr"
    ),
    "1^er^ trimestre 2026"
  )

  expect_equal(
    libelle_periode(
      3,
      2026,
      periode = "trimestre",
      format = "chiffres",
      lang = "fr"
    ),
    "3^e^ trimestre 2026"
  )

  expect_equal(
    libelle_periode(
      1,
      2026,
      periode = "trimestre",
      format = "chiffres",
      lang = "en"
    ),
    "1^st^ quarter 2026"
  )

  expect_equal(
    libelle_periode(
      4,
      2026,
      periode = "trimestre",
      format = "chiffres",
      lang = "en"
    ),
    "4^th^ quarter 2026"
  )
})


test_that("le decalage des mois gere les changements d'annee", {

  expect_equal(
    libelle_periode(
      12,
      2026,
      periode = "mois",
      decalage = 1,
      lang = "fr"
    ),
    "janvier 2027"
  )

  expect_equal(
    libelle_periode(
      1,
      2026,
      periode = "mois",
      decalage = -1,
      lang = "fr"
    ),
    "d\u00E9cembre 2025"
  )

  expect_equal(
    libelle_periode(
      8,
      2026,
      periode = "mois",
      decalage = 23,
      lang = "fr"
    ),
    "juillet 2028"
  )

  expect_equal(
    libelle_periode(
      8,
      2026,
      periode = "mois",
      decalage = -23,
      lang = "fr"
    ),
    "septembre 2024"
  )
})


test_that("le decalage des trimestres gere les changements d'annee", {

  expect_equal(
    libelle_periode(
      4,
      2026,
      periode = "trimestre",
      decalage = 1,
      lang = "fr"
    ),
    "premier trimestre 2027"
  )

  expect_equal(
    libelle_periode(
      1,
      2026,
      periode = "trimestre",
      decalage = -1,
      lang = "fr"
    ),
    "quatri\u00E8me trimestre 2025"
  )

  expect_equal(
    libelle_periode(
      1,
      2026,
      periode = "trimestre",
      decalage = 23,
      lang = "fr"
    ),
    "quatri\u00E8me trimestre 2031"
  )

  expect_equal(
    libelle_periode(
      4,
      2026,
      periode = "trimestre",
      decalage = -23,
      lang = "fr"
    ),
    "premier trimestre 2021"
  )
})


test_that("l'annee peut etre retiree du libelle", {

  expect_equal(
    libelle_periode(
      8,
      2026,
      periode = "mois",
      avec_annee = FALSE,
      lang = "fr"
    ),
    "ao\u00FBt"
  )

  expect_equal(
    libelle_periode(
      3,
      2026,
      periode = "trimestre",
      avec_annee = FALSE,
      lang = "fr"
    ),
    "troisi\u00E8me trimestre"
  )

  expect_equal(
    libelle_periode(
      2,
      2026,
      periode = "trimestre",
      format = "chiffres",
      avec_annee = FALSE,
      lang = "en"
    ),
    "2^nd^ quarter"
  )
})


test_that("les valeurs incorrectes de numero sont refusees", {

  expect_error(
    libelle_periode(
      0,
      2026,
      periode = "mois",
      lang = "fr"
    ),
    "`numero` doit \u00EAtre compris entre 1 et 12"
  )

  expect_error(
    libelle_periode(
      13,
      2026,
      periode = "mois",
      lang = "fr"
    ),
    "`numero` doit \u00EAtre compris entre 1 et 12"
  )

  expect_error(
    libelle_periode(
      5,
      2026,
      periode = "trimestre",
      lang = "fr"
    ),
    "`numero` doit \u00EAtre compris entre 1 et 4"
  )

  expect_error(
    libelle_periode(
      1.5,
      2026,
      periode = "mois",
      lang = "fr"
    ),
    "`numero` doit \u00EAtre un entier"
  )

  expect_error(
    libelle_periode(
      NA_real_,
      2026,
      periode = "mois",
      lang = "fr"
    ),
    "`numero` doit \u00EAtre un entier"
  )
})


test_that("les valeurs incorrectes d'annee sont refusees", {

  expect_error(
    libelle_periode(
      1,
      NA_real_,
      periode = "mois",
      lang = "fr"
    ),
    "`annee` doit \u00EAtre un entier"
  )

  expect_error(
    libelle_periode(
      1,
      "2026",
      periode = "mois",
      lang = "fr"
    ),
    "`annee` doit \u00EAtre un entier"
  )
})


test_that("les decalages incorrects sont refuses", {

  expect_error(
    libelle_periode(
      1,
      2026,
      periode = "mois",
      decalage = 1.5,
      lang = "fr"
    ),
    "`decalage` doit \u00EAtre un entier"
  )

  expect_error(
    libelle_periode(
      1,
      2026,
      periode = "mois",
      decalage = NA_real_,
      lang = "fr"
    ),
    "`decalage` doit \u00EAtre un entier"
  )
})


test_that("avec_annee accepte uniquement un booleen", {

  expect_error(
    libelle_periode(
      1,
      2026,
      periode = "mois",
      avec_annee = NA,
      lang = "fr"
    ),
    "`avec_annee` doit \u00EAtre"
  )

  expect_error(
    libelle_periode(
      1,
      2026,
      periode = "mois",
      avec_annee = "oui",
      lang = "fr"
    ),
    "`avec_annee` doit \u00EAtre"
  )
})


test_that("les choix d'arguments incorrects sont refuses", {

  expect_error(
    libelle_periode(
      1,
      2026,
      periode = "semaine",
      lang = "fr"
    )
  )

  expect_error(
    libelle_periode(
      1,
      2026,
      periode = "trimestre",
      format = "court",
      lang = "fr"
    )
  )

  expect_error(
    libelle_periode(
      1,
      2026,
      periode = "mois",
      lang = "de"
    )
  )
})
