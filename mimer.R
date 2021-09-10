x <- MIME()
y0 <- emayili:::multipart_related()
y1 <- emayili:::multipart_related()
y2 <- emayili:::multipart_related()
y3 <- emayili:::multipart_related()
y4 <- emayili:::multipart_related()
z0 <- emayili:::multipart_mixed(children = y0)
z2 <- emayili:::multipart_mixed(children = list(y0, y1))


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

t1 <- emayili:::text_plain("FOO")
h1 <- emayili:::text_html("<p>BAR</p>")

t1
h1
