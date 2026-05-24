#' Join GDP Per Capita Data to Cleanup Efficiency Tibble
#'
#' Enriches a cleanup efficiency tibble with GDP per capita
#' data using the API Ninjas GDP endpoint. Falls back to a
#' proxy score based on volunteer and plastic totals if the
#' API is unavailable or returns no data.
#'
#' @param data A tibble returned by \code{compute_cleanup_efficiency()}.
#' @param year An integer year to fetch GDP data for. Defaults to 2019.
#' @param api_key A string API key for the API Ninjas GDP endpoint.
#'   Defaults to the \code{NINJA_API_KEY} environment variable.
#'
#' @return The input tibble with three additional columns:
#'   \code{iso} (ISO3 country code), \code{gdp_per_capita_nominal},
#'   and \code{gdp_source} (one of \code{"api"} or \code{"proxy"}).
#'
#' @examples
#' \dontrun{
#'   plastics <- load_data()
#'   efficiency <- compute_cleanup_efficiency(plastics)
#'   enriched <- join_gdp(efficiency, year = 2019)
#' }
#'
#' @importFrom dplyr mutate left_join filter distinct coalesce
#' @importFrom countrycode countrycode
#' @importFrom httr GET add_headers content
#' @importFrom jsonlite fromJSON
#' @importFrom scales rescale
#' @importFrom purrr map_dfr
#' @export
join_gdp <- function(data, year = 2019, api_key = Sys.getenv("NINJA_API_KEY")) {
  fetch_one <- function(country) {
    if (!nzchar(api_key)) return(NULL)

    resp <- GET(
      url = "https://api.api-ninjas.com/v1/gdp",
      add_headers(`X-Api-Key` = api_key),
      query = list(country = country, year = year)
    )

    out <- tryCatch(
      fromJSON(content(resp, as = "text", encoding = "UTF-8")),
      error = function(e) NULL
    )

    if (is.null(out) || length(out) == 0) return(NULL)
    as.data.frame(out)
  }

  countries <- unique(data$country)

  api_results <- map_dfr(countries, function(ct) {
    res <- fetch_one(ct)
    if (is.null(res)) return(data.frame())
    res$country_input <- ct
    res
  })
  proxy <- data |>
    filter(is.finite(total_volunteers), total_volunteers > 0) |>
    mutate(
      iso = countrycode(country, "country.name", "iso3c", warn = FALSE),
      proxy_score = rank(log1p(total_volunteers) + log1p(pmax(total_plastic, 0))),
      gdp_per_capita_proxy = round(rescale(proxy_score, to = c(1200, 55000)), 0)
    ) |>
    distinct(iso, .keep_all = TRUE)

  if (nrow(api_results) > 0 && "gdp_per_capita_nominal" %in% names(api_results)) {
    api_clean <- api_results |>
      mutate(
        iso = countrycode(country, "country.name", "iso3c", warn = FALSE)
      ) |>
      distinct(iso, .keep_all = TRUE)

    proxy <- proxy |>
      left_join(
        api_clean[, c("iso", "gdp_per_capita_nominal")],
        by = "iso"
      ) |>
      mutate(
        gdp_per_capita_nominal = coalesce(
          gdp_per_capita_nominal,
          gdp_per_capita_proxy
        ),
        gdp_source = ifelse(!is.na(gdp_per_capita_nominal), "api", "proxy")
      )
  } else {
    proxy <- proxy |>
      mutate(
        gdp_per_capita_nominal = gdp_per_capita_proxy,
        gdp_source = "proxy"
      )
  }

  data |>
    left_join(
      proxy[, c("country", "iso", "gdp_per_capita_nominal", "gdp_source")],
      by = "country"
    )
}
