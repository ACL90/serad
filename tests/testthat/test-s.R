test_that("multiplication works", {
  expect_equal(s(-7.5), "s")
  expect_equal(s(-2), "s")
  expect_equal(s(1.4,"chat parle", "chats parlent"), "chat parle")
  expect_equal(s(-2,"chat parle", "chats parlent"), "chats parlent")
  expect_equal(s(1.97), "")
  expect_equal(s(arrondi_tot(1.97)), "s")
  expect_equal(s(1.97, seuil=1.95), "s")
})


