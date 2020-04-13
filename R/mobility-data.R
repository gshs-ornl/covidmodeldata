#' Get Latest Mobility Data from Descartes Labs
#'
#' @param admin character. county, state, or all
#'
#' @return df
#' @export
get_mobility <- function(admin = c("county", "state", "all")) {

  admin <- match.arg(admin)

  url <- "https://github.com/descarteslabs/DL-COVID-19/raw/master/DL-us-mobility-daterow.csv"

  df <- readr::read_csv(url) %>%
    dplyr::transmute(
      geoid = fips,
      date,
      country_name = country_code,
      admin_level,
      state_fips = substr(geoid, start = 1, stop = 2),
      state_name = admin1,
      county_name = admin2,
      samples,
      m50,
      m50_index
    )


  if (admin == "county") {
    df <-
    df %>%
      dplyr::filter(
        admin_level == 2
      )
  }

  if (admin == "state") {
    df <-
      df %>%
      dplyr::filter(
        admin_level == 2
      )
  }


  df
}
