skip_on_ci()
skip_on_cran()
skip_if_not(
  require(Microsoft365R, quietly=TRUE),
  "{Microsoft365R} tests skipped (package not available)."
)

# To run these tests you'll need to first authenticate. This is done by running
# the following command in an interactive session.
#
# It might also help to login to a Microsoft account (https://login.live.com/).

outlook <- get_personal_outlook()

from_addr <- "anne@example.com"
to_addr <- "bob@example.com"
cc_addr <- "jane@example.com"
bcc_addr <- "rex@example.com"

msg <- envelope(
  to = to_addr,
  cc = cc_addr,
  bcc = bcc_addr,
  subject = SUBJECT,
  html = HTMLCONTENT
)
msg_outlook <- outlook$create_email(msg)

test_that("body is correct", {
  expect_identical(msg_outlook$properties$body$contentType, "html")
  expect_true(grepl(HTMLCONTENT, msg_outlook$properties$body$content))
})

test_that("subject is correct", {
  expect_identical(msg_outlook$properties$subject, SUBJECT)
})

test_that("parties are correct", {
  expect_identical(msg_outlook$properties$toRecipients[[1]]$emailAddress$address, as.character(to(msg)))
  expect_identical(msg_outlook$properties$ccRecipients[[1]]$emailAddress$address, as.character(cc(msg)))
  expect_identical(msg_outlook$properties$bccRecipients[[1]]$emailAddress$address, as.character(bcc(msg)))
})
