test_that("parameter value encoding (unquoted)", {
  expect_equal(parameter_value_encode("Not quoted."), "=\"Not quoted.\"")
})

test_that("parameter value encoding (quoted)", {
  expect_equal(parameter_value_encode('"Quoted."'), "*=utf-8''%22Quoted.%22")
})

test_that("parameter value encoding (non-ASCII)", {
  expect_equal(parameter_value_encode(ACCENTED_NAME), "*=utf-8''se%C3%B1or-gonz%C3%A1lez.csv")

  msg <- envelope() %>% attachment(ACCENTED_PATH)

  expect_match(
    msg %>% as.character(),
    "name*=utf-8''se%C3%B1or-gonz%C3%A1lez.csv",
    fixed = TRUE
  )
  expect_match(
    msg %>% as.character(),
    "filename*=utf-8''se%C3%B1or-gonz%C3%A1lez.csv",
    fixed = TRUE
  )
})
