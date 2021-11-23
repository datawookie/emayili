skip_if_not(
  require(Microsoft365R, quietly=TRUE),
  "{Microsoft365R} tests skipped (package not available)."
)
require(AzureAuth)

tenant <- "consumers"
app <- "d44a05d5-c6a5-4bbb-82d2-443123722380"
from_addr <- "anne@example.com"
to_addr <- "bob@example.com"
cc_addr <- "jane@example.com"
bcc_addr <- "rex@example.com"

scopes <- c(
  file.path("https://graph.microsoft.com", c("Mail.Send", "Mail.ReadWrite", "User.Read")),
  "openid",
  "offline_access"
)
token <- get_azure_token(
  scopes,
  tenant,
  app,
  version = 2
)

outlook <- get_personal_outlook(token = token)

folder <- outlook$create_folder(rndchar())

msg <- envelope(
  to = to_addr,
  cc = cc_addr,
  bcc = bcc_addr,
  subject = SUBJECT,
  html = HTMLCONTENT
)
msg <- folder$create_email(msg)

expect_identical(msg$properties$body$contentType, "html")
expect_true(grepl(HTMLCONTENT, msg$properties$body$content))
expect_identical(msg$properties$subject, SUBJECT)
expect_identical(msg$properties$toRecipients[[1]]$emailAddress$address, to_addr)
expect_identical(msg$properties$ccRecipients[[1]]$emailAddress$address, cc_addr)
expect_identical(msg$properties$bccRecipients[[1]]$emailAddress$address, bcc_addr)
