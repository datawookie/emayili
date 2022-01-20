test_that("template in system folder", {
  expect_error(envelope() %>% template("newsletter"), NA)
})

test_that("template in relative path", {
  expect_error(envelope() %>% template(TEMPLATE_HTML, name = "Bob"), NA)
})

test_that("template in absolute path", {
  expect_error(envelope() %>% template(file.path(getwd(), TEMPLATE_HTML), name = "Bob"), NA)
})

test_that("missing template", {
  expect_error(envelope() %>% template("missing"), "Unable to find '.*' template.")
})

test_that("unable to find HTML or text template", {
  expect_error(envelope() %>% template(TEMPLATE_NONE))
})

test_that("text template", {
  expect_match(
    envelope() %>% template(file.path(getwd(), TEMPLATE_TEXT), name = "Bob") %>% as.character(),
    "Hello Bob!$"
  )
})

test_that("HTML & text template", {
  expect_match(
    envelope() %>% template(file.path(getwd(), TEMPLATE_BOTH), name = "Bob") %>% as.character(),
    "<p>Hello Bob!</p>"
  )
})
