TXTPATH <- tempfile(fileext = ".txt")
#
writeLines("Some random text.", TXTPATH)

PNGPATH <- tempfile(fileext = ".png")
#
png(PNGPATH, width=600, height=350)
hist(mtcars$disp)
dev.off()

# This file comes from https://bit.ly/2P4LUO8 (cat poster on WikiPedia).
#
JPGPATH <- here::here("inst", "cats.jpg")

SMTP_USERNAME = Sys.getenv("SMTP_USERNAME")
