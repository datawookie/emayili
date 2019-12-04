library(dplyr)
library(emayili)

SMTP_USERNAME = Sys.getenv("SMTP_USER")
SMTP_PASSWORD = Sys.getenv("SMTP_PASSWORD")
SMTP_SERVER = Sys.getenv("SMTP_SERVER")
SMTP_PORT = Sys.getenv("SMTP_PORT")

smtp <- server(host = SMTP_SERVER, port = SMTP_PORT, username = SMTP_USERNAME, password = SMTP_PASSWORD)

email <- envelope() %>%
  from(SMTP_USERNAME) %>%
  to(SMTP_USERNAME) %>%
  subject("subject") %>%
  body("Hello!") %>%
  attachment("README.md", cid = "readme") %>%
  attachment("attachment-spreadsheet.xlsx") %>%
  attachment("attachment-image.jpg", cid = "image")

smtp(email, verbose = TRUE)

cat(emayili:::message(email))
