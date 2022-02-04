test_that("parameter value encoding (unquoted)", {
  expect_equal(parameter_value_encode("Not quoted."), "=\"Not quoted.\"")
})

test_that("parameter value encoding (quoted)", {
  expect_equal(parameter_value_encode('"Quoted."'), "*=utf-8''%22Quoted.%22")
})

test_that("parameter value encoding (non-ASCII)", {
  expect_match(parameter_value_encode(ACCENTED_NAME), "*=utf-8''se(%C3%B1|%F1)or-gonz(%C3%A1|%E1)lez.csv")

  msg <- envelope() %>% attachment(ACCENTED_PATH)

  expect_match(
    msg %>% as.character(),
    "name\\*=utf-8''se(%C3%B1|%F1)or-gonz(%C3%A1|%E1)lez.csv"
  )
  expect_match(
    msg %>% as.character(),
    "filename\\*=utf-8''se(%C3%B1|%F1)or-gonz(%C3%A1|%E1)lez.csv"
  )
})
