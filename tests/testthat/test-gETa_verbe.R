test_that("gETa_verbe", {
  #je commente la ou il y a de l aleatoire
  expect_equal(gETa_verbe(1.00049,1,0.9996), "reste stable")
  expect_equal(gETa_verbe(1.00049,1,0.981), "se stabilise")
  expect_equal(gETa_verbe(1.1,1,0.99), "accélère")
  expect_equal(gETa_verbe(1.1,1,0.99,0), "accélèrent")
  expect_equal(gETa_verbe(1.04,1,0.99), "accélère")
  expect_equal(gETa_verbe(1.01,1,0.99), "poursuit sa progression")
  #expect_equal(gETa_verbe(1.003,1,0.99,0), "ralentissent") se modèrent/ralentissent
  #expect_equal(gETa_verbe(1.001,1,1.01), "repart à la hausse") repart à la hausse//se redresse
  #expect_equal(gETa_verbe(0.999,1,1.01), "continue à baisser") poursuit sa baisse//continue à baisser
  #expect_equal(gETa_verbe(0.99,1,1.01), "") #poursuit sa baisse // continue à baisser
  #expect_equal(gETa_verbe(0.96,1,1.01), "") #poursuit son recul // recule à nouveau
  #expect_equal(gETa_verbe(0.96,1,0.99), "recule") se replie//recule
  expect_equal(gETa_verbe(0.8,1,0.99), "se replie fortement")
  expect_equal(gETa_verbe(0.79,1,0.99), "chute")
})

