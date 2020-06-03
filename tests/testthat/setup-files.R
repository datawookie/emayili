TXTPATH <- tempfile(fileext = ".txt")
#
writeLines("Some random text.", TXTPATH)

PNGPATH <- tempfile(fileext = ".png")
#
png(PNGPATH, width=600, height=350)
hist(mtcars$disp)
dev.off()

JPGPATH <- tempfile(fileext = ".png")
#
download.file("https://bit.ly/2P4LUO8", JPGPATH, quiet = TRUE)

SMTP_USERNAME = Sys.getenv("SMTP_USERNAME")
