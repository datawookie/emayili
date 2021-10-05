library(here)
library(logger)

log_threshold(ERROR)

TXTPATH <- tempfile(fileext = ".txt")
CSSPATH <- tempfile(fileext = ".css")
HTMLPATH <- "hello.html"
RMD_TEMPLATE <- "vignette.Rmd"

# This file comes from https://bit.ly/2P4LUO8 (cat poster on WikiPedia).
#
JPGPATH <- system.file("cats.jpg", package = "emayili", mustWork = TRUE)
# If have not installed yet.
if (!file.exists(JPGPATH)) JPGPATH <- here("inst", "cats.jpg")

TXTCONTENT <- "Some random text."

PLAIN_MARKDOWN <- "## Section\n[link](https://www.google.com)"
PLAIN_MARKDOWN_INTERPOLATE <- "Hi {{name}}!"

COLOUR_GLAUCOUS = "#6082b6"

# Start with a blank slate.
#
source("teardown-files.R", local = TRUE)

# The , sep = "" prevents it from writing an "\n" at the end of the line.
writeLines(TXTCONTENT, TXTPATH, sep = "")
TXTCONTENT_ENCODED <- emayili:::mime_base64encode(charToRaw(TXTCONTENT))

writeLines(
  paste0("body {color: ", COLOUR_GLAUCOUS," !important;}"),
  CSSPATH
)

HTMLCONTENT <- "<p>Hello there, stranger!</p>"
#
writeLines(HTMLCONTENT, HTMLPATH)

# SERVER -----------------------------------------------------------------------

# Using fake SMTP server.
#
# - https://mailtrap.io/
# - https://www.smtpbucket.com/
#
SMTP_SERVER   = "mail.smtpbucket.com"
SMTP_PORT     = 8025
SMTP_USERNAME <- Sys.getenv("SMTP_USERNAME")
SMTP_PASSWORD <- Sys.getenv("SMTP_PASSWORD")

smtp <- server(
  host = SMTP_SERVER,
  port = SMTP_PORT,
  username = SMTP_USERNAME
)
smtp_verbose <- server(
  host = SMTP_SERVER,
  port = SMTP_PORT,
  username = SMTP_USERNAME
)
smtp_insecure <- server(
  host = SMTP_SERVER,
  port = SMTP_PORT,
  username = SMTP_USERNAME,
  insecure = TRUE
)

smtp_gmail <- server(
  host = "smtp.gmail.com",
  port = 587,
  username = SMTP_USERNAME,
  password = SMTP_PASSWORD
)

# R MARKDOWN FILE --------------------------------------------------------------

# Create an Rmd document from template.
rmarkdown::draft(
  RMD_TEMPLATE,
  template = "html_vignette",
  package = "rmarkdown",
  edit = FALSE
)
