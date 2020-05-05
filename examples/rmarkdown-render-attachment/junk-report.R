library(emayili)
library(rmarkdown)
library(dplyr)

# This script will render a Rmd to HTML and attach it to an email.
#
# NOTE: Images in the HTML will not appear in the attachment.

render(
  input = "junk-report.Rmd" ,
  output_format = "html_document",
  output_file = "junk-report.html"
)

SMTP_USERNAME = Sys.getenv("SMTP_USERNAME")

smtp <- server(
  host = Sys.getenv("SMTP_SERVER"),
  port = Sys.getenv("SMTP_PORT"),
  username = SMTP_USERNAME,
  password = Sys.getenv("SMTP_PASSWORD")
)

email <- envelope() %>%
  from(SMTP_USERNAME) %>%
  to(SMTP_USERNAME) %>%
  subject("Junk Report")%>%
  attachment("junk-report.html", disposition = "attachment")

smtp(email, verbose = TRUE)
