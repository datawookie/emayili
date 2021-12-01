spelling::spell_check_package()

devtools::check(cran = TRUE)

# Incoming feasibility checks for CRAN.
#
devtools::check(
  manual = TRUE,
  remote = TRUE,
  incoming = TRUE
)

# Check it on Windows.
#
devtools::check_win_devel()

rhub::check_for_cran()
