skip_on_os(os = c("mac", "windows", "solaris"))

# To ensure that these locales are supported you might need to run this in the
# shell:
#
# sudo locale-gen es_ES es_ES.utf8
# sudo update-locale

# Get current locale.
#
CURRENT_LOCALE = Sys.getlocale("LC_CTYPE")

test_that("es_ES / Spanish", {
  msg <- envelope() %>%
    render("Desde el comité organizador")

  Sys.setlocale("LC_ALL", "es_ES.utf8")

  msg %>%
    as.character() %>%
    cat()

  Sys.setlocale("LC_ALL", "Spanish")

  msg %>%
    as.character() %>%
    cat()
})

# Restore locale.
#
Sys.setlocale("LC_ALL", CURRENT_LOCALE)
