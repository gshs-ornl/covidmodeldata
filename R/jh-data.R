#' Get Latest COVID-19 Data from John Hopkins University
#'
#' @importFrom magrittr %>%
#'
#' @return df
#' @export
get_jh <- function() {

  date_grep <- "^[0-9]{1,2}/[0-9]{1,2}/[0-9]{1,2}$"

  # make sure fips, etc gets read in as character
  jh_col_types <- readr::cols(
    UID = readr::col_character(),
    iso2 = readr::col_character(),
    iso3 = readr::col_character(),
    code3 = readr::col_character(),
    FIPS = readr::col_character(),
    Admin2 = readr::col_character(),
    Province_State = readr::col_character(),
    Country_Region = readr::col_character(),
    Combined_Key = readr::col_character()
    )


  cases_url <- "https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"

  deaths_url <- "https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"

  df_cases <- readr::read_csv(cases_url, col_types = jh_col_types) %>%
    tidyr::pivot_longer(cols = tidyselect::matches(date_grep), names_to = "date", values_to = "total_cases") %>%
    dplyr::mutate(date = as.Date(date, "%m/%d/%y")) %>%
    janitor::clean_names()

  df_deaths <- readr::read_csv(deaths_url, col_types = jh_col_types) %>%
    tidyr::pivot_longer(cols = tidyselect::matches(date_grep), names_to = "date", values_to = "total_deaths") %>%
    dplyr::mutate(date = as.Date(date, "%m/%d/%y")) %>%
    janitor::clean_names()%>%
    dplyr::select(
      uid, date, total_deaths
    )

  df <- dplyr::full_join(df_cases, df_deaths, by = c("uid", "date"))

  df <- df %>%
    dplyr::filter(
      code3 == "840" #counties only
    ) %>%
    dplyr::mutate(
      county_fips = substr(uid, start = 4, stop = 8),
      state_fips  = substr(county_fips, 1, 2)
    ) %>%
    dplyr::left_join(covidmodeldata::acs_names, by = c("county_fips", "state_fips")) %>%
    dplyr::select(
      county_fips:county_name_long,
      tidyselect::everything(),
      date,
      tidyselect::starts_with("total_")
    ) %>%
    dplyr::group_by(uid) %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(
      new_cases = total_cases - dplyr::lag(total_cases, default = 0),
      new_deaths = total_deaths - dplyr::lag(total_deaths, default = 0)
    ) %>%
    dplyr::ungroup()


  df
}
