#' Extract sender and recipient(s)
#'
#' @param msg A message object.
#'
#' @return A tibble.
#' @export
#'
#' @examples
parties <- function(msg) {
  map_dfr(c("From", "To", "Cc", "Bcc"), function(type) {
    tibble(
      type,
      address = as.list(msg$header[type])
    )
  }) %>%
    unnest(cols = c(address)) %>%
    mutate(
      address = as.address(address),
      display = display(address),
      raw = raw(address)
    )
}
