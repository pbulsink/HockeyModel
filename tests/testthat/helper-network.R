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

# #87: clear the session-level NHL seasons cache so tests of the fallback and
# caching behaviour start from a clean state regardless of test order. The
# cache is an environment in the package namespace (the enclosing env of
# .fetch_nhl_seasons), stable across load_all / installed builds.
clear_nhl_seasons_cache <- function() {
  ns_env <- environment(HockeyModel:::.fetch_nhl_seasons)
  rlang::env_unbind(get("nhl_seasons_cache", envir = ns_env), "data")
}
