#' Encrypt message
#'
#' @inheritParams parties
#'
#' @return A message object.
#' @export
#'
#' @examples#'
#' \dontrun{
#' # A WWII D-Day message.
#' #
#' # See https://twitter.com/bletchleypark/status/1136490396626800640.
#' #
#' msg <- envelope(
#'   to = "schunk@u-boat.com",
#'   subject = "Top Secret Message",
#'   text = "Immediate readiness. There are indications that the invasion has begun."
#' )
#' msg %>% encrypt()
encrypt <- function(msg) {
  msg$encrypt <- TRUE
  if (get_option_invisible()) invisible(msg) else msg # nocov
}
