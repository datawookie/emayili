SMTP_PASSWORD = Sys.getenv("SMTP_PASSWORD")
SMTP_USERNAME = Sys.getenv("SMTP_USERNAME")

smtp <- server(host = "smtp.gmail.com", port = 587, username = SMTP_USERNAME, password = SMTP_PASSWORD)

msg <- envelope() %>%
  from(SMTP_USERNAME) %>%
  to(SMTP_USERNAME) %>%
  subject("Text body") %>%
  text("Hello, World!")

test_that("server type", {
  expect_type(smtp, "closure")
})

test_that("sends text message", {
  expect_error(smtp(msg), NA)
})
