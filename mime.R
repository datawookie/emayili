library(xml2)

read_file <- function(path) {
  readChar(path, file.info(path)$size)
}

msg <- envelope() %>%
  from("andrew@fathomdata.dev") %>%
  # to(c("andrew@fathomdata.dev", "matt@fathomdata.dev"))
  to("andrew@fathomdata.dev")

# email <- email %>% render("test.Rmd", quiet = FALSE)

smtp <- server(
  host = Sys.getenv("SMTP_SERVER"),
  port = Sys.getenv("SMTP_PORT"),
  username = Sys.getenv("SMTP_USERNAME"),
  password = Sys.getenv("SMTP_PASSWORD"),
  max_times = 1
)

# /tmp/Rtmp3yy0bw/file244fb5f65ba66_files/figure-html/
# /tmp/Rtmp3yy0bw/file244fb5f65ba66_files/figure-html

input <- "untitled.Rmd"
output <- tempfile(fileext = ".html")
images <- file.path(sub(".html", "_files", output), "figure-html")

markdown <- read_file(input)

cat(markdown, file = input)

rmarkdown::render(
  input,
  output_file = output,
  quiet = TRUE,
  output_options = list(self_contained = FALSE)
)
output = read_file(output)

# Use Content-ID (CID) for images.
#
# print(xml_find_all(xml, "//img"))

# Strip out <script> tags. These don't work in email, right?
#
xml <- read_html(output)

xml_find_all(xml, "//script") %>% xml_remove()
#
# Don't actually want to strip out all <link> tags because one of them has
# important CSS, but this is just to get things working in GMail web client.
#
xml_find_all(xml, "//link") %>% xml_remove()

for (img in xml_find_all(xml, "//img")) {
  src <- xml_attr(img, "src")
  src <- paste0('cid:', hexkey(basename(src)))
  xml_attr(img, "src") <- src
}

msg <- msg %>% html(as.character(xml))

for (image in list.files(images, full.names = TRUE)) {
  msg <- msg %>%
    attachment(path = image, cid = hexkey(basename(image)))
}

smtp(msg, verbose = FALSE)
