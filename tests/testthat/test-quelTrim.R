test_that("quelTrim works", {
  expect_equal(quelTrim(3,2023),                  "troisième trimestre 2023")
  expect_equal(quelTrim(3,2023,majuscule=1),      "Troisième trimestre 2023")
  expect_equal(quelTrim(3,2023,type="chiffres"), "3e trimestre 2023")
  expect_equal(quelTrim(1,2023,type="chiffres"), "1er trimestre 2023")
  expect_equal(prevTrim(1,2023) , "quatrième trimestre 2022")
  expect_equal(nextTrim(3,2023) , "quatrième trimestre 2023")
  expect_equal(nextTrim(4,2023) , "premier trimestre 2024")

})
