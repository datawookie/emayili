#' Extract sender and recipient(s)
#'
#' @param msg A message object.
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' email <- envelope() %>%
#'   from("Gerald <gerald@gmail.com>") %>%
#'   to(c("bob@gmail.com", "alice@yahoo.com")) %>%
#'   cc("Craig     < craig@gmail.com>") %>%
#'   bcc("  Erin   <erin@yahoo.co.uk    >")
#'
#' parties(email)
parties <- function(msg) {
  # Avoid "no visible binding for global variable" note.
  address <- NULL # nocov
  values <- NULL # nocov

  map_dfr(c("From", "To", "Cc", "Bcc"), function(type) {
    tibble(
      type,
      address = msg$headers[type]
    )
  }) %>%
    hoist(address, "values") %>%
    unnest(values) %>%
    select(-address) %>%
    rename(address = values) %>%
    mutate(
      display = display(address),
      raw = raw(address),
      local = local(address),
      domain = domain(address)
    )
}
