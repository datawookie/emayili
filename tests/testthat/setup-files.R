library(logger)

log_threshold(ERROR)

TXTPATH <- tempfile(fileext = ".txt")
#
writeLines("Some random text.", TXTPATH)

PNGPATH <- tempfile(fileext = ".png")
#
png(PNGPATH, width=600, height=350)
hist(mtcars$disp)
dev.off()

HTMLPATH <- "hello.html"
HTMLCONTENT <- "<p>Hello there, stranger!</p>"
#
writeLines(HTMLCONTENT, HTMLPATH)

# This file comes from https://bit.ly/2P4LUO8 (cat poster on WikiPedia).
#
JPGPATH <- here::here("inst", "cats.jpg")

# R MARKDOWN FILE --------------------------------------------------------------

FILE_RMD <- "message.Rmd"

# Create an Rmd document from template.
rmarkdown::draft(
  FILE_RMD,
  template = "github_document",
  package = "rmarkdown",
  edit = FALSE
)

# ------------------------------------------------------------------------------

SMTP_USERNAME <- Sys.getenv("SMTP_USERNAME")
SMTP_PASSWORD <- Sys.getenv("SMTP_PASSWORD")
