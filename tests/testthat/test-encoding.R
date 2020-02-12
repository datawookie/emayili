test_that("(en|de)code '='", {
  expect_equal(qp_encode("= =="), "=3D =3D=3D")
  expect_equal(qp_decode("=3D =3D=3D"), "= ==")
})
