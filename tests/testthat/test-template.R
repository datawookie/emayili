test_that("template in system folder", {
  expect_error(envelope() %>% template("newsletter"), NA)
})

test_that("template in relative path", {
  expect_error(envelope() %>% template(TEMPLATE_NAME, name = "Bob"), NA)
})

test_that("template in absolute path", {
  expect_error(envelope() %>% template(file.path(getwd(), TEMPLATE_NAME), name = "Bob"), NA)
})

test_that("missing template", {
  expect_error(envelope() %>% template("missing"), "Unable to find '.*' template.")
})
