SMTP_USERNAME = Sys.getenv("SMTP_USERNAME")

# Using Gmail SMTP server.
#
# SMTP_PASSWORD = Sys.getenv("SMTP_PASSWORD")
# SMTP_SERVER   = "smtp.gmail.com"
# SMTP_PORT     = 587

# Using fake SMTP server.
#
# - https://mailtrap.io/
# - https://www.smtpbucket.com/
#
SMTP_PASSWORD = NULL
SMTP_SERVER   = "mail.smtpbucket.com"
SMTP_PORT     = 8025

smtp <- server(host = SMTP_SERVER, port = SMTP_PORT, username = SMTP_USERNAME, password = SMTP_PASSWORD)

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

  expect_error(smtp(msg), NA)
})

test_that("sends HTML message", {
  msg <- msg %>%
    subject("HTML body") %>%
    html("<p><strong>Hello</strong>, <em>World</em>! You can also <u>underline</u> text.</p>")

  expect_error(smtp(msg), NA)
})

test_that("sends message with text attachment", {
  msg <- msg %>%
    attachment(TXTPATH)

  expect_error(smtp(msg), NA)
})

test_that("sends message with image attachment", {
  msg <- msg %>%
    attachment(PNGPATH)

  expect_error(smtp(msg), NA)
})

test_that("sends message with image attachment (using CID)", {
  msg <- msg %>%
    html('<img src="cid:r-logo"/>') %>%
    attachment(PNGPATH, cid = "r-logo", type = "image/png")

  expect_error(smtp(msg), NA)
})
