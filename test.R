library(dplyr)
library(emayili)

SMTP_USERNAME = Sys.getenv("SMTP_USER")
SMTP_PASSWORD = Sys.getenv("SMTP_PASSWORD")
SMTP_SERVER = Sys.getenv("SMTP_SERVER")
SMTP_PORT = Sys.getenv("SMTP_PORT")

if (SMTP_SERVER == "") {
  SMTP_USERNAME = NULL
  SMTP_PASSWORD = NULL
  SMTP_SERVER = "mail.smtpbucket.com"
  SMTP_PORT = 8025
}

smtp <- server(host = SMTP_SERVER, port = SMTP_PORT, username = SMTP_USERNAME, password = SMTP_PASSWORD)

LINK = "https://website.com/XY_stuff?Q_DL=2a94hds4QmHP_9NaQ3odv_MLRP_9GNlEdOXL2q9&Q_CHL=gl"

email <- envelope() %>%
  # from(SMTP_USERNAME) %>%
  # to(SMTP_USERNAME) %>%
  subject("subject") %>%
  # body("Hello!") %>%
  # body(paste0("LINK: ", LINK), type = "html")
# body(paste0("LINK: ", LINK))
# body(
# paste0("LINK: ", LINK, "\n<a href='", LINK, "'>foo</a>"), type = "html"
# ) %>%
  attachment("README.md", cid = "readme")
  # attachment("attachment-spreadsheet.xlsx") %>%
  # attachment("attachment-image.jpg", cid = "image", type = "image/jpeg")

smtp(email, verbose = TRUE)

cat(emayili:::message(email))
