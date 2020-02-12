TXTPATH <- tempfile(fileext = ".txt")
#
writeLines("Some random text.", TXTPATH)

PNGPATH <- tempfile(fileext = ".png")
#
png(PNGPATH, width=600, height=350)
hist(mtcars$disp)
dev.off()
