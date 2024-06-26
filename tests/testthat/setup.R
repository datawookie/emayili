suppressPackageStartupMessages({
  library(here)
  library(dplyr)
  library(base64enc)
  library(logger)
  require(htmltools, quietly = TRUE)
})

log_threshold(FATAL)

# Generate random folder name.
#
rndchar <- function(n = 24) {
  stringi::stri_rand_strings(1, n)
}

TXTPATH <- tempfile(fileext = ".txt")
CSSPATH <- tempfile(fileext = ".css")
HTMLPATH <- "hello.html"
RMD_TEMPLATE <- "vignette.Rmd"

TEMPLATE_HTML <- "letter-html"
TEMPLATE_TEXT <- "letter-text"
TEMPLATE_BOTH <- "letter-both"
TEMPLATE_NONE <- "letter-none"

IMG_URL <- "https://cran.r-project.org/Rlogo.svg"

# This file comes from https://bit.ly/2P4LUO8 (cat poster on WikiPedia).
#
JPGPATH <- system.file(
  "cats.jpg",
  package = "emayili",
  mustWork = TRUE
)
# If have not installed yet.
if (!file.exists(JPGPATH)) JPGPATH <- here("inst", "cats.jpg")

TXTCONTENT <- "Some random text."

PLAIN_MARKDOWN <- "## Section\n[link](https://www.google.com)"
PLAIN_MARKDOWN_INTERPOLATE <- "Hi {{name}}!"

COLOUR_GLAUCOUS <- "#6082b6"

ACCENTED_NAME <- "señor-gonzález.csv"
ACCENTED_PATH <- file.path(tempdir(), ACCENTED_NAME)

# Start with a blank slate.
#
source("teardown.R", local = TRUE)

# The , sep = "" prevents it from writing an "\n" at the end of the line.
writeLines(TXTCONTENT, TXTPATH, sep = "")
TXTCONTENT_ENCODED <- emayili:::mime_base64encode(charToRaw(TXTCONTENT))

writeLines(
  paste0("body {color: ", COLOUR_GLAUCOUS, " !important;}"),
  CSSPATH
)

HTMLCONTENT <- "<p>Hello there, stranger!</p>"
#
writeLines(HTMLCONTENT, HTMLPATH)

SUBJECT <- rndchar(36)

file.create(ACCENTED_PATH)

# SERVER -----------------------------------------------------------------------

# Using fake SMTP server.
#
# - https://mailtrap.io/
# - https://www.smtpbucket.com/
#
SMTP_SERVER <- "mail.smtpbucket.com"
SMTP_PORT <- 8025
SMTP_USERNAME_GMAIL <- Sys.getenv("GMAIL_USERNAME", NA)
SMTP_PASSWORD_GMAIL <- Sys.getenv("GMAIL_PASSWORD", NA)
SMTP_PASSWORD_SENDGRID <- Sys.getenv("SENDGRID_API_KEY")
SMTP_USERNAME_MAILGUN <- Sys.getenv("MAILGUN_SMTP_USERNAME")
SMTP_PASSWORD_MAILGUN <- Sys.getenv("MAILGUN_SMTP_PASSWORD")
SMTP_USERNAME_SENDINBLUE <- Sys.getenv("SENDINBLUE_SMTP_USERNAME")
SMTP_PASSWORD_SENDINBLUE <- Sys.getenv("SENDINBLUE_SMTP_PASSWORD")
SMTP_USERNAME_MAILERSEND <- Sys.getenv("MAILERSEND_SMTP_USERNAME")
SMTP_PASSWORD_MAILERSEND <- Sys.getenv("MAILERSEND_SMTP_PASSWORD")
SMTP_USERNAME_MAILTRAP <- Sys.getenv("MAILTRAP_SMTP_USERNAME")
SMTP_PASSWORD_MAILTRAP <- Sys.getenv("MAILTRAP_SMTP_PASSWORD")

EMAIL_FROM <- ifelse(is.na(SMTP_USERNAME_GMAIL), "alice@gmail.com", SMTP_USERNAME_GMAIL)
EMAIL_TO <- ifelse(is.na(SMTP_USERNAME_GMAIL), "bob@yahoo.com", SMTP_USERNAME_GMAIL)

smtp <- server(
  host = SMTP_SERVER,
  port = SMTP_PORT
)
smtp_verbose <- server(
  host = SMTP_SERVER,
  port = SMTP_PORT,
  username = SMTP_USERNAME_GMAIL
)
smtp_insecure <- server(
  host = SMTP_SERVER,
  port = SMTP_PORT,
  username = SMTP_USERNAME_GMAIL,
  insecure = TRUE
)

if (is.na(SMTP_USERNAME_GMAIL) || is.na(SMTP_PASSWORD_GMAIL)) {
  smtp_gmail <- NA
} else {
  smtp_gmail <- gmail(
    username = SMTP_USERNAME_GMAIL,
    password = SMTP_PASSWORD_GMAIL
  )
}

smtp_sendgrid <- sendgrid(
  password = SMTP_PASSWORD_SENDGRID
)

smtp_mailgun <- mailgun(
  username = SMTP_USERNAME_MAILGUN,
  password = SMTP_PASSWORD_MAILGUN
)

smtp_sendinblue <- sendinblue(
  username = SMTP_USERNAME_SENDINBLUE,
  password = SMTP_PASSWORD_SENDINBLUE
)

smtp_mailersend <- mailersend(
  username = SMTP_USERNAME_MAILERSEND,
  password = SMTP_PASSWORD_MAILERSEND
)

smtp_smtpbucket <- smtpbucket()

smtp_mailtrap <- mailtrap(
  sandbox = TRUE,
  username = SMTP_USERNAME_MAILTRAP,
  password = SMTP_PASSWORD_MAILTRAP
)

# TEMPLATE ---------------------------------------------------------------------

TEMPLATE_HTML_CONTENT <- "<p>Hello {{ name }}!</p>"
TEMPLATE_TEXT_CONTENT <- "Hello {{ name }}!"

for (dir in c(TEMPLATE_HTML, TEMPLATE_TEXT, TEMPLATE_BOTH, TEMPLATE_NONE)) {
  dir.create(dir)
}

for (dir in c(TEMPLATE_HTML, TEMPLATE_BOTH)) {
  writeLines(TEMPLATE_HTML_CONTENT, file.path(dir, "template.html"))
}
for (dir in c(TEMPLATE_TEXT, TEMPLATE_BOTH)) {
  writeLines(TEMPLATE_TEXT_CONTENT, file.path(dir, "template.txt"))
}

# R MARKDOWN FILE --------------------------------------------------------------

# Create an Rmd document from template.
rmarkdown::draft(
  RMD_TEMPLATE,
  template = "html_vignette",
  package = "rmarkdown",
  edit = FALSE
)

# ------------------------------------------------------------------------------

ADDRESS_HANS_DISPLAY <- "Hansjörg Müller"
ADDRESS_HANS <- address(email = "hans@gmail.com", display = ADDRESS_HANS_DISPLAY)

ADDRESS_BETHANY_DISPLAY <- "Betânia Guaraná"
ADDRESS_BETHANY <- address(email = "bethany@gmail.com", display = ADDRESS_BETHANY_DISPLAY)

SUBJECT_FURNITURE_FEET <- encodable("Möbelträgerfüße")

# UTILITY ----------------------------------------------------------------------

skip_if_neither_installed <- function(paks) {
  are_there <- vapply(
    paks,
    requireNamespace,
    FUN.VALUE = logical(1),
    quietly = TRUE
  )
  if (!any(are_there)) {
    testthat::skip(
      sprintf(
        "None of {%s} are installed",
        paste(paks, collapse = "} or {")
      )
    )
  }
}

with_tz <- function(code) {
  old_tz <- Sys.getenv("TZ")
  on.exit(
    Sys.setenv("TZ" = old_tz)
  )
  Sys.setenv(TZ = "UTC")
  force(code)
}
