#' Extract sender and recipient(s)
#'
#' @param msg A message object.
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' msg <- envelope() %>%
#'   from("Gerald <gerald@gmail.com>") %>%
#'   to(c("bob@gmail.com", "alice@yahoo.com")) %>%
#'   cc("Craig     < craig@gmail.com>") %>%
#'   bcc("  Erin   <erin@yahoo.co.uk    >")
#'
#' parties(msg)
parties <- function(msg) {
  # Avoid "no visible binding for global variable" note.
  address <- NULL # nocov
  values <- NULL # nocov

  PARTIES <- c("From", "To", "Cc", "Bcc")
  #
  # Retain only populated headers.
  #
  PARTIES <- intersect(PARTIES, names(msg$headers))

  map(msg$headers[PARTIES], ~ .$values) %>%
    tibble(type = names(.), address = .) %>%
    mutate(
      address = map(address, cleave)
    ) %>%
    unnest(address) %>%
    mutate(
      display = map_chr(address, display),
      raw = map_chr(address, raw),
      local = map_chr(address, local),
      domain = map_chr(address, domain),
      address = map_chr(address, as.character)
    )
}
