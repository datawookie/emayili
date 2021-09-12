library(logger)

log_threshold(ERROR)

TXTPATH <- tempfile(fileext = ".txt")
PNGPATH <- tempfile(fileext = ".png")
HTMLPATH <- "hello.html"
RMD_TEMPLATE <- "message.Rmd"
RMD_EXAMPLE <- "example.Rmd"

# Start with a blank slate.
#
source("teardown-files.R", local = TRUE)

writeLines("Some random text.", TXTPATH)

png(PNGPATH, width=600, height=350)
hist(mtcars$disp)
dev.off()

HTMLCONTENT <- "<p>Hello there, stranger!</p>"
#
writeLines(HTMLCONTENT, HTMLPATH)

# This file comes from https://bit.ly/2P4LUO8 (cat poster on WikiPedia).
#
JPGPATH <- here::here("inst", "cats.jpg")

# R MARKDOWN FILE --------------------------------------------------------------

# Create an Rmd document from template.
rmarkdown::draft(
  RMD_TEMPLATE,
  template = "github_document",
  package = "rmarkdown",
  edit = FALSE
)

file.copy(
  system.file("examples/rmd", "example.Rmd", package = "servr"),
  RMD_EXAMPLE
)

# ------------------------------------------------------------------------------

SMTP_USERNAME <- Sys.getenv("SMTP_USERNAME")
SMTP_PASSWORD <- Sys.getenv("SMTP_PASSWORD")
