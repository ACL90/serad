test_that("multiplication works", {

  col0 = c("Y1T1", "Y1T2", "Y1trim3", "Y1T4","Y2T1","Y2-T2")
  col1 = c( 12,     11,      7,         6,     9,     10)
  col2 = c( 12,     11,      7,         6,     9,     14)
  col3 = c( 12,     11,      3,         6,     9,     4)
  col4 = c( 12,     11,      7,         6,     9,     4)
  col5 = c( 12,     11,      7,         3,     9,     4)
  df1 = data.frame(col0,col1,col2,col3,col4,col5)

  expect_equal(plushautniveau(df1),
               paste0("C'est le plus haut niveau depuis Y1T2."))
  expect_equal(plushautniveau(df1,nbperiode = 5),
               paste0(""))
  expect_equal(plushautniveau(df1,vary="col2"),
               paste0("C'est le plus haut niveau depuis le d\u00e9but de la s\u00e9rie."))
  expect_equal(plushautniveau(df1,vary="col3"),
               paste0("C'est le plus bas niveau depuis Y1trim3."))
  expect_equal(plushautniveau(df1,vary="col4"),
               paste0("C'est le plus bas niveau depuis le d\u00e9but de la s\u00e9rie."))


  }
  )
