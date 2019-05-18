test_that("server type", {
  PASSWORD = Sys.getenv("PASSWORD")
  USERNAME = Sys.getenv("USERNAME")

  smtp <- server(host = "smtp.gmail.com", port = 465, username = USERNAME, password = PASSWORD)

  expect_type(smtp, "closure")
})
