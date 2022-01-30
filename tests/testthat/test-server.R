msg <- envelope() %>%
  from(EMAIL_FROM) %>%
  to(EMAIL_TO)

msg_no_recipient <- envelope() %>%
  from(EMAIL_FROM)

msg_no_sender <- envelope() %>%
  to(EMAIL_TO)

test_that("server type", {
  expect_type(smtp, "closure")
  expect_type(smtp_sendgrid, "closure")
  expect_type(smtp_mailgun, "closure")
  expect_type(smtp_sendinblue, "closure")
  expect_type(smtp_mailersend, "closure")
  expect_type(smtp_smtpbucket, "closure")

  skip_if(suppressWarnings(is.na(smtp_gmail)))
  expect_type(smtp_gmail, "closure")
})

test_that("test server connection", {
  skip_if(suppressWarnings(is.na(smtp_gmail)))

  expect_type(
    gmail(
      username = SMTP_USERNAME_GMAIL,
      password = SMTP_PASSWORD_GMAIL,
      test = TRUE
    ),
    "closure"
  )
  expect_error(
    gmail(
      username = SMTP_USERNAME_GMAIL,
      password = NULL,
      test = TRUE,
      max_times = 1
    ),
    "Login denied"
  )
})

test_that("error if sender missing", {
  skip_on_cran()
  expect_error(smtp(msg_no_sender), "Must specify who the email is from.")
})

test_that("error if recipient missing", {
  skip_on_cran()
  expect_error(smtp(msg_no_recipient), "Must specify at least one email recipient.")
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
  skip_on_ci()
  skip_if(suppressWarnings(is.na(smtp_gmail)))
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
  expect_error(smtp(msg %>% subject("{emayili} test: Text attachment")), NA)
})

test_that("sends message with image attachment", {
  msg <- msg %>%
    attachment(JPGPATH)

  skip_on_cran()
  expect_error(smtp(msg %>% subject("{emayili} test: Image attachment")), NA)
})

test_that("sends message with image attachment (using CID)", {
  msg <- msg %>%
    html('<img src="cid:r-logo"/>') %>%
    attachment(JPGPATH, cid = "r-logo", type = "image/jpg")

  skip_on_cran()
  expect_error(smtp(msg %>% subject("{emayili} test: Image attachment (using CID)")), NA)
})

test_that("verbose output", {
  skip_on_cran()

  expect_match(
    capture.output(smtp(msg, verbose = TRUE), type = "message") %>%
      paste(collapse = "\n"),
    "250 Message accepted",
    fixed = TRUE
  )

  expect_length(capture.output(smtp(msg), type = "message"), 0)
})

test_that("replace bare line feeds", {
  msg <- envelope() %>% render("Hello!")

  expect_false(as.character(msg) %>% str_detect(REGEX_BARE_LINEFEED))
})
