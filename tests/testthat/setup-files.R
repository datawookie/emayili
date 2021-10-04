library(logger)

log_threshold(ERROR)

TXTPATH <- tempfile(fileext = ".txt")
PNGPATH <- tempfile(fileext = ".png")
CSSPATH <- tempfile(fileext = ".css")
HTMLPATH <- "hello.html"
RMD_TEMPLATE <- "vignette.Rmd"

TXTCONTENT <- "Some random text."

PLAIN_MARKDOWN <- "## Section\n[link](https://www.google.com)"
PLAIN_MARKDOWN_INTERPOLATE <- "Hi {{name}}!"

COLOUR_GLAUCOUS = "#6082b6"

# Start with a blank slate.
#
source("teardown-files.R", local = TRUE)

writeLines(TXTCONTENT, TXTPATH)

writeLines(
  paste0("body {color: ", COLOUR_GLAUCOUS," !important;}"),
  CSSPATH
)

png(PNGPATH, width=600, height=350)
hist(mtcars$disp)
dev.off()

HTMLCONTENT <- "<p>Hello there, stranger!</p>"
#
writeLines(HTMLCONTENT, HTMLPATH)

# This file comes from https://bit.ly/2P4LUO8 (cat poster on WikiPedia).
#
JPGPATH <- here::here("inst", "cats.jpg")

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
