library(dplyr)

# spelling::spell_check_package()

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

# Check on macOS.
#
rhub::check_for_cran(
  platforms = rhub::platforms() %>%
    filter(categories == "macOS") %>%
    pull(name),
  env_vars = c(
    `_R_CHECK_FORCE_SUGGESTS_` = "true",
    `_R_CHECK_CRAN_INCOMING_USE_ASPELL_` = "false"
  )
)

# Try this one instead?
#
rhub::check(platform = c("macos-highsierra-release"))

rhub::check_for_cran(
  platforms = "macos-highsierra-release-cran",
  env_vars = c(
    `_R_CHECK_FORCE_SUGGESTS_` = "true",
    `_R_CHECK_CRAN_INCOMING_USE_ASPELL_` = "false"
  )
)
