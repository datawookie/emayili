# Example text from https://en.wikipedia.org/wiki/Quoted-printable.
#
encoded <- "J'interdis aux marchands de vanter trop leurs marchandises. Car ils se font=\n vite p=C3=A9dagogues et t'enseignent comme but ce qui n'est par essence qu=\n'un moyen, et te trompant ainsi sur la route =C3=A0 suivre les voil=C3=A0 b=\nient=C3=B4t qui te d=C3=A9gradent, car si leur musique est vulgaire ils te =\nfabriquent pour te la vendre une =C3=A2me vulgaire."
decoded <- "J'interdis aux marchands de vanter trop leurs marchandises. Car ils se font vite pédagogues et t'enseignent comme but ce qui n'est par essence qu'un moyen, et te trompant ainsi sur la route à suivre les voilà bientôt qui te dégradent, car si leur musique est vulgaire ils te fabriquent pour te la vendre une âme vulgaire."

test_that("(en|de)code '='", {
  expect_equal(qp_encode("= =="), "=3D =3D=3D")
  expect_equal(qp_decode("=3D =3D=3D"), "= ==")
  expect_equal(qp_encode(decoded), encoded)
  expect_equal(qp_decode(encoded), decoded)
})
