test_that("multiplication works", {
  expect_equal(gETa_verbe_taux(0.049,0.049), "reste stable")
  expect_equal(gETa_verbe_taux(0.049,2), "se stabilise")
  expect_equal(gETa_verbe_taux(10,1), "accélère")
  expect_equal(gETa_verbe_taux(4,1,0), "accélèrent")
  expect_equal(gETa_verbe_taux(1,1), "poursuit sa progression")
  #expect_equal(gETa_verbe_taux(0.3,1), "") ralentit // se modère
  #expect_equal(gETa_verbe_taux(0.1,-1), "") repart à la hausse // se redresse
  #expect_equal(gETa_verbe_taux(-0.1,-1), "") poursuit sa baisse // continue à baisser
  #expect_equal(gETa_verbe_taux(-0.3,-1), "") poursuit sa baisse // continue à baisser
  #expect_equal(gETa_verbe_taux(-1,-1), "") poursuit sa baisse // continue à baisser
  #expect_equal(gETa_verbe_taux(-4,-1), "") recule à nouveau // poursuit son recul
  #expect_equal(gETa_verbe_taux(-4,1), "") recule //se replie
  expect_equal(gETa_verbe_taux(-20,1), "se replie fortement")
  expect_equal(gETa_verbe_taux(-21,1), "chute")
})
