url_is_reachable <- function(url) {
  req <- httr2::request(url) |>
    httr2::req_timeout(seconds = 0.5) |>
    httr2::req_error(is_error = function(resp) FALSE)
  resp <- try(httr2::req_perform(req), silent = TRUE)
  !inherits(resp, "try-error")
}

skip_if_hockey_apis_unavailable <- function(
  urls = c(
    "https://api.nhle.com",
    "https://api-web.nhle.com",
    "https://data.naturalstattrick.com"
  )
) {
  if (!all(vapply(urls, url_is_reachable, logical(1)))) {
    testthat::skip(
      "NHL/Natural Stat Trick APIs are unavailable in this environment"
    )
  }
}
