skip_on_ci()
skip_on_cran()
skip_on_os(os = c("mac", "windows", "solaris"))

# To ensure that these locales are supported you might need to run this in the
# shell:
#
# sudo locale-gen es_ES es_ES.utf8
# sudo update-locale

check_render <- function(locale, unicode, txt) {
  test_that(locale, {
    # Write text to file in a UTF8 locale. This ensures that the accented
    # characters are rendered correctly to the file on disk.
    Sys.setlocale("LC_ALL", "en_US.UTF-8")
    md <- tempfile()
    cat(txt, file = md)

    # Change to other locale.
    Sys.setlocale("LC_ALL", locale)
    local_reproducible_output(unicode = unicode)

    # Render from string.
    expect_match(envelope() %>% render(txt) %>% as.character(), txt)

    # Render from file.
    expect_match(envelope() %>% render(md) %>% as.character(), txt)
  })
}

# Get current locale.
#
CURRENT_LOCALE = Sys.getlocale("LC_CTYPE")

# Spanish
#
# Might need to install locale:
#
# $ sudo locale-gen es_ES.utf8
# $ sudo update-locale
#
ES_TXT <- "Desde el comité organizador"
#
check_render("es_ES.utf8", TRUE, ES_TXT)
check_render("Spanish", FALSE, ES_TXT)

# Restore locale.
#
Sys.setlocale("LC_ALL", CURRENT_LOCALE)
