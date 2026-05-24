#' Plot GDP Per Capita vs Cleanup Efficiency
#'
#' Produces an interactive plotly scatter plot of GDP per capita
#' vs cleanup efficiency, colored by region, with optional
#' per-region linear smoothers.
#'
#' @param data A tibble returned by \code{join_gdp()}.
#' @param regions A character vector of regions to display.
#'   Defaults to NULL which shows all regions.
#' @param log_axes Logical. If TRUE, both axes are log scaled.
#'   Defaults to TRUE.
#' @param show_smoother Logical. If TRUE, adds a per-region
#'   linear smoother. Defaults to TRUE.
#' @return A plotly object.
#' @examples
#' \dontrun{
#'   plastics <- load_data()
#'   efficiency <- compute_cleanup_efficiency(plastics)
#'   enriched <- join_gdp(efficiency, year = 2019)
#'   plot_gdp_efficiency(enriched, regions = c("africa", "asia"))
#' }
#' @importFrom dplyr filter mutate group_by
#' @importFrom plotly plot_ly add_lines layout config
#' @importFrom stringr str_to_title
#' @export
plot_gdp_efficiency <- function(data, regions = NULL, log_axes = TRUE, show_smoother = TRUE) {

  if (!is.null(regions)) {
    data <- data |>
      filter(region %in% regions)
  }

  data <- data |>
    filter(
      !is.na(gdp_per_capita_nominal),
      !is.na(avg_efficiency),
      !is.na(region)
    ) |>
    mutate(region_label = str_to_title(region))

  smoother_data <- data |>
    group_by(region_label) |>
    group_modify(~ {
      if (nrow(.x) < 2) return(tibble())
      fit <- lm(log10(avg_efficiency) ~ log10(gdp_per_capita_nominal), data = .x)
      x_seq <- 10^seq(
        log10(min(.x$gdp_per_capita_nominal, na.rm = TRUE)),
        log10(max(.x$gdp_per_capita_nominal, na.rm = TRUE)),
        length.out = 100
      )
      tibble(
        gdp_per_capita_nominal = x_seq,
        avg_efficiency = 10^predict(fit, newdata = tibble(gdp_per_capita_nominal = x_seq))
      )
    }) |>
    ungroup()

  p <- plot_ly(
    data = data,
    x = ~gdp_per_capita_nominal,
    y = ~avg_efficiency,
    color = ~region_label,
    type = "scatter",
    mode = "markers",
    marker = list(size = 10, opacity = 0.8),
    hovertext = ~paste0(
      "<b>", country, "</b>",
      "<br>Region: ", region_label,
      "<br>GDP per capita: $", round(gdp_per_capita_nominal, 0),
      "<br>Total volunteers: ", total_volunteers,
      "<br>Cleanup efficiency: ", round(avg_efficiency, 2)
    ),
    hoverinfo = "text"
  )

  if (show_smoother && nrow(smoother_data) > 0) {
    for (reg in unique(smoother_data$region_label)) {
      reg_data <- smoother_data |> filter(region_label == reg)
      p <- p |>
        add_lines(
          data = reg_data,
          x = ~gdp_per_capita_nominal,
          y = ~avg_efficiency,
          color = ~region_label,
          inherit = FALSE,
          line = list(width = 2),
          hoverinfo = "skip",
          showlegend = FALSE
        )
    }
  }

  axis_type <- if (log_axes) "log" else "linear"

  p |>
    layout(
      title = "GDP per Capita vs Cleanup Efficiency",
      xaxis = list(title = "GDP per Capita", type = axis_type),
      yaxis = list(title = "Cleanup Efficiency", type = axis_type),
      legend = list(orientation = "h", y = -0.18, x = 0)
    ) |>
    config(displayModeBar = FALSE, responsive = TRUE)
}
