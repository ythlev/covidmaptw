#' Make COVID map
#'
#' @param file File to save to.
#'
#' @export
make_covid_map <- function(file) {
  data_cases <- readr::read_csv(
    "https://od.cdc.gov.tw/eic/Day_Confirmation_Age_County_Gender_19CoV.csv",
    col_names = c(NA, "date", "county", "town", NA, NA, NA, "cases")
  ) |>
  dplyr::slice(-1) |>
  dplyr::mutate(
    date = as.Date(date, format = "%Y%m%d"),
    site_id = paste0(county, town),
    cases = as.integer(cases)
  )

  town <- data_cases |>
  dplyr::count(site_id, wt = cases) |>
  dplyr::right_join(town) |>
  dplyr::mutate(
    n = tidyr::replace_na(n, 0),
    rate = 100000 * n / people_total
  )

  rate_mean <- sum(town$n) / length(town$n)
  breaks <- c(
    0,
    1 / 9 * rate_mean,
    4 / 9 * rate_mean,
    9 / 9 * rate_mean,
    16 / 9 * rate_mean
  )

  town <- town |>
  dplyr::mutate(
    level = findInterval(rate, breaks),
    fill = RColorBrewer::brewer.pal(5, "RdPu")[level]
  )
  colour_town <- town$fill
  names(colour_town) <- town$site_id

  main <- xml2::read_xml(system.file("extdata", "template.svg", package = "covidmaptw", mustWork = TRUE))

  main |>
  xml2::xml_child(1) |>
  xml2::xml_child(1) |>
  xml2::xml_child(1) |>
  xml2::xml_set_text(as.character(max(data_cases$date)))

  breaks <- format(breaks, digits = 2)
  breaks[1] <- paste("<", breaks[2])
  breaks[2:5] <- paste("\u2265", breaks[2:5])

  for (i in 2:6) {
    main |>
    xml2::xml_child(1) |>
    xml2::xml_child(1) |>
    xml2::xml_child(i) |>
    xml2::xml_set_text(breaks[i - 1])
  }

  main |>
  xml2::xml_child(3) |>
  xml2::xml_children() |>
  purrr::walk(~ xml2::xml_set_attr(.x, "fill", colour_town[xml2::xml_attr(.x, "id")]))

  xml2::write_xml(main, file)
}
