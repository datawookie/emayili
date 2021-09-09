test_that("render plain markdown from string", {
  msg <-

    expect_match(
      envelope() %>%
        md("[This](https://www.google.com) is a link.") %>%
        as.character(),
      "<a href=3D\"https://www.google.com\">"
    )
})

test_that("render R markdown from file", {
    expect_match(
      envelope() %>%
        rmd(FILE_RMD) %>%
        as.character(),
      "This is an R Markdown format used for publishing markdown documents to GitHub."
    )
})


