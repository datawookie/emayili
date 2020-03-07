SMTP_USERNAME = Sys.getenv("SMTP_USERNAME")

# Using fake SMTP server.
#
# - https://mailtrap.io/
# - https://www.smtpbucket.com/
#
SMTP_SERVER   = "mail.smtpbucket.com"
SMTP_PORT     = 8025

smtp <- server(host = SMTP_SERVER, port = SMTP_PORT, username = SMTP_USERNAME)
smtp_insecure <- server(host = SMTP_SERVER, port = SMTP_PORT, username = SMTP_USERNAME, insecure = TRUE)

smtp_gmail <- server(
  host = "smtp.gmail.com",
  port = 587,
  username = SMTP_USERNAME,
  password = Sys.getenv("SMTP_PASSWORD")
)

msg <- envelope() %>%
  from(SMTP_USERNAME) %>%
  to(SMTP_USERNAME)

test_that("server type", {
  expect_type(smtp, "closure")
})

test_that("sends text message", {
  msg <- msg %>%
    subject("Text body") %>%
    text("Hello, World!")

  skip_on_cran()
  expect_error(smtp(msg), NA)
})

test_that("sends message with insecure = TRUE", {
  skip_on_cran()
  expect_error(smtp_insecure(msg), NA)
})

test_that("sends with SSL", {
  skip_on_cran()
  expect_error(smtp_gmail(msg %>% subject("{emayili} test")), NA)
})

test_that("sends HTML message", {
  msg <- msg %>%
    subject("HTML body") %>%
    html("<p><strong>Hello</strong>, <em>World</em>! You can also <u>underline</u> text.</p>")

  skip_on_cran()
  expect_error(smtp(msg), NA)
})

test_that("sends message with text attachment", {
  msg <- msg %>%
    attachment(TXTPATH)

  skip_on_cran()
  expect_error(smtp(msg), NA)
})

test_that("sends message with image attachment", {
  msg <- msg %>%
    attachment(PNGPATH)

  skip_on_cran()
  expect_error(smtp(msg), NA)
})

test_that("sends message with image attachment (using CID)", {
  msg <- msg %>%
    html('<img src="cid:r-logo"/>') %>%
    attachment(PNGPATH, cid = "r-logo", type = "image/png")

  skip_on_cran()
  expect_error(smtp(msg), NA)
})

test_that("sends with verbose output", {
  skip_on_cran()
  output <- capture.output(smtp(msg, verbose = TRUE), type = "message") %>%
    paste(collapse = "\n")

  expect_match(output, "^Sending email to")
})
