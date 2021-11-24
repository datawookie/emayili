library(covr)

# Tests that are skipped on CRAN should still be included in coverage report.
#
Sys.setenv(NOT_CRAN = "true")

report()
