skip_on_ci()
# skip_if_not(
#   interactive(),
#   "{Microsoft365R} tests skipped (not interactive session)."
# )
skip_if_not(
  require(Microsoft365R, quietly=TRUE),
  "{Microsoft365R} tests skipped (package not available)."
)

from_addr <- "anne@example.com"
to_addr <- "bob@example.com"
cc_addr <- "jane@example.com"
bcc_addr <- "rex@example.com"

# require(AzureAuth)
# scopes <- c(
#   file.path("https://graph.microsoft.com", c("Mail.Send", "Mail.ReadWrite", "User.Read")),
#   "openid",
#   "offline_access"
# )
# token <- get_azure_token(
#   scopes,
#   "consumers",
#   "d44a05d5-c6a5-4bbb-82d2-443123722380",
#   version = 2
# )
# outlook <- get_personal_outlook(token = token)

outlook <- get_personal_outlook()

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
