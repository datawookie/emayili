# Example text from https://en.wikipedia.org/wiki/Quoted-printable.
#
# nolint start
encoded <- "J'interdis aux marchands de vanter trop leurs marchandises. Car ils se font=\n vite p=C3=A9dagogues et t'enseignent comme but ce qui n'est par essence qu=\n'un moyen, et te trompant ainsi sur la route =C3=A0 suivre les voil=C3=A0 b=\nient=C3=B4t qui te d=C3=A9gradent, car si leur musique est vulgaire ils te =\nfabriquent pour te la vendre une =C3=A2me vulgaire."
decoded <- "J'interdis aux marchands de vanter trop leurs marchandises. Car ils se font vite pédagogues et t'enseignent comme but ce qui n'est par essence qu'un moyen, et te trompant ainsi sur la route à suivre les voilà bientôt qui te dégradent, car si leur musique est vulgaire ils te fabriquent pour te la vendre une âme vulgaire."
# nolint end

# Can generate other test text at https://www.webatic.com/quoted-printable-convertor.

test_that("(en|de)code text", {
  local_reproducible_output(unicode = TRUE)

  expect_equal(qp_encode(decoded), encoded)
  expect_equal(qp_decode(encoded), decoded)
})

test_that("(en|de)code '='", {
  expect_equal(qp_encode("= =="), "=3D =3D=3D")
  expect_equal(qp_decode("=3D =3D=3D"), "= ==")
})

test_that("don't break lines in token", {
  expect_equal(
    qp_encode("========================="),
    "=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D"
  )
  expect_equal(
    qp_encode(" ========================="),
    " =3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=\n=3D"
  )
  expect_equal(
    qp_encode("  ========================="),
    "  =3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=\n=3D"
  )
  expect_equal(
    qp_encode("   ========================="),
    "   =3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=\n=3D"
  )
  expect_equal(
    qp_encode("    ========================="),
    "    =3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=\n=3D=3D"
  )
})

test_that("unicode", {
  local_reproducible_output(unicode = TRUE)

  expect_equal(Encoding(qp_decode("=E3=83=84")), Encoding("ツ"))
  expect_equal(qp_decode("=E3=83=84"), "ツ")
})

test_that("display name not encoded in console", {
  expect_match(as.character(ADDRESS_HANS), "Hansjörg Müller")
})

test_that("subject not encoded in console", {
  expect_match(as.character(SUBJECT_FURNITURE_FEET), "Möbelträgerfüße")
})

test_that("display name encoded in message", {
  msg <- envelope(to = c(ADDRESS_HANS, ADDRESS_BETHANY))

  expect_match(msg %>% as.character(encode = FALSE), "Hansjörg Müller")
  expect_match(msg %>% as.character(encode = FALSE), "Betânia Guaraná")
  expect_match(
    msg %>% as.character(encode = TRUE),
    "=?UTF-8?B?SGFuc2rDtnJnIE3DvGxsZXI=?=",
    fixed = TRUE
  )
  expect_match(
    msg %>% as.character(encode = TRUE),
    "=?UTF-8?B?QmV0w6JuaWEgR3VhcmFuw6E=?=",
    fixed = TRUE
  )
})

test_that("subject encoded in message", {
  expect_match(
    envelope(subject = SUBJECT_FURNITURE_FEET) %>% as.character(encode = FALSE),
    "Möbelträgerfüße"
  )
  expect_match(
    envelope(subject = SUBJECT_FURNITURE_FEET) %>% as.character(encode = TRUE),
    "=?UTF-8?B?TcO2YmVsdHLDpGdlcmbDvMOfZQ==?=",
    fixed = TRUE
  )
})

test_that("accented characters in Rmd file", {
  ACCENTS <- "öäå"
  rmd <- tempfile(fileext = ".Rmd")
  writeLines(ACCENTS, rmd)
  expect_match(
    envelope() %>% render(rmd) %>% as.character(),
    ACCENTS
  )
  unlink(rmd)
})
