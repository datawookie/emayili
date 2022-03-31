test_that("content-language field", {
  expect_match(
    envelope() %>% text("Hello!", language = "en-GB") %>% as.character(),
    "Content-Language: +en-GB"
  )
  expect_match(
    envelope() %>% html(HTMLCONTENT, language = "en-GB") %>% as.character(),
    "Content-Language: +en-GB"
  )
  expect_match(
    envelope() %>% render(RMD_TEMPLATE, language = "en-GB", include_css = FALSE) %>% as.character(),
    "Content-Language: +en-GB"
  )
})

test_that("detect language", {
  skip_if_neither_installed(c("cld3", "cld2"))
  expect_no_match(
    envelope() %>% text("Hello!", language = FALSE) %>% as.character(),
    "Content-Language:"
  )
  expect_match(
    envelope() %>%
      html(
        "<p>Hij heeft de klok horen luiden maar weet niet waar de klepel hangt.</p>",
        language = TRUE
      ) %>%
      as.character(),
    "Content-Language: +nl"
  )
})
