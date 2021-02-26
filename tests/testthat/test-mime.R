test_that("parameter value encoding (unquoted)", {
  expect_equal(parameter_value_encode("I'm not quoted.csv"), "=\"I'm not quoted.csv\"")
})

test_that("parameter value encoding (quoted)", {
  expect_equal(parameter_value_encode("\"I'm quoted\".csv"), "*=utf-8''%22I'm%20quoted%22.csv")
})

test_that("parameter value encoding (non-ASCII)", {
  expect_equal(parameter_value_encode("señor.csv"), "*=utf-8''se%C3%B1or.csv")
})
