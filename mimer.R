library(emayili)

x <- emayili:::MIME()
y0 <- emayili:::multipart_related()
y1 <- emayili:::multipart_related()
y2 <- emayili:::multipart_related()
y3 <- emayili:::multipart_related()
y4 <- emayili:::multipart_related()
z0 <- emayili:::multipart_mixed(children = y0)
z2 <- emayili:::multipart_mixed(children = list(y0, y1))

smtp <- server(
  host = Sys.getenv("SMTP_SERVER"),
  port = Sys.getenv("SMTP_PORT"),
  username = Sys.getenv("SMTP_USERNAME"),
  password = Sys.getenv("SMTP_PASSWORD"),
  max_times = 1
)

# print(x)
# print(y0)
# print(z0)
# print(z2)
#
# z2 <- append(z2, y2)
# z2 <- append(z2, y3)
# z2 <- append(z2, y4)
# # append(z2, y4)
# # print(z2)
# print(z2)

# t1 <- emayili:::text_plain("FOO")
# h1 <- emayili:::text_html("<p>BAR</p>")
#
# t1
# h1
#
msg <- envelope() %>%
  to("andrew@fathomdata.dev") %>%
  from("andrew@fathomdata.dev")

# msg %>%
#   to("andrew@fathomdata.dev") %>%
#   from("andrew@fathomdata.dev") %>%
#   subject("TEST TEXT") %>%
#   text("Hello, World!") %T>%
#   print(details = TRUE) %>%
#   smtp(verbose = TRUE)
#
# msg %>%
#   to("andrew@fathomdata.dev") %>%
#   from("andrew@fathomdata.dev") %>%
#   subject("TEST HTML") %>%
#   html("<p>Farewell, <strong>cruel</strong> World!</p>") %T>%
#   print(details = TRUE) %>%
#   smtp(verbose = TRUE)

msg %>%
  to("andrew@fathomdata.dev") %>%
  from("andrew@fathomdata.dev") %>%
  subject("TEST ATTACHMENT") %>%
  html("<p>Farewell, <strong>cruel</strong> World!</p>") %>%
  attachment("/home/wookie/Downloads/testfile.jpg") %>%
  print(details = TRUE)

  # html("<p>Hello, World! <strong>BOOM</strong></p>")
#
# print(msg, details = TRUE)
#

# emayili::other("/etc/passwd")
#
# emayili::other("/home/wookie/Downloads/Untitled.png")
# emayili::other("/home/wookie/Downloads/maxresdefault.jpg")

# msg <- msg %>%
#   attachment("/etc/passwd", type = "text/plain") %>%
#   attachment("/home/wookie/Downloads/Untitled.png")

msg1 <- msg %>%
  text("untitled.Rmd")
print(msg1, details = TRUE)

msg2 <- msg %>%
  rmd("untitled.Rmd")
print(msg2, details = TRUE)

smtp(msg, verbose = TRUE)


related <- emayili:::multipart_related()
related <- append(related,  emayili:::text_html("<p>BAR</p>"))

mixed <- emayili:::multipart_mixed()
mixed <- append(mixed,  emayili:::text_html("<p>BAR</p>"))
