#' Get Latest Mobility Data from Google
#'
#' This dataset is intended to help remediate the impact of COVID-19. It shouldn’t be used for medical diagnostic,
#' prognostic, or treatment purposes. It also isn’t intended to be used for guidance on personal travel plans.
#' Each Community Mobility Report dataset is presented by location and highlights the percent change in visits to
#' places like grocery stores and parks within a geographic area. Location accuracy and the understanding of categorized
#' places varies from region to region, so we don’t recommend using this data to compare changes between countries, or
#' between regions with different characteristics (e.g. rural versus urban areas). We’ll leave a region or category out
#' of the dataset if we don’t have sufficient statistically significant levels of data.
#'
#' @importFrom magrittr %>%
#' @param admin character. Geographic level of the observations. One of county, state, country, or all.
#' @param us_only logical. If `TRUE` return results only within the United States. Default is `TRUE`
#'
#' @return df data frame of percent change in visits to places like grocery stores and parks within a geographic area.
#'
#' @details
#'
#' These datasets show how visits and length of stay at different places change compared to a baseline.
#' We calculate these changes using the same kind of aggregated and anonymized data used to show popular
#' times for places in Google Maps.
#'
#' Changes for each day are compared to a baseline value for that day of the week:
#'
#'  - The baseline is the median value, for the corresponding day of the week, during the 5-week period Jan 3–Feb 6, 2020.
#'
#'  - The datasets show trends over several months with the most recent data representing approximately 2-3 days ago
#'    this is how long it takes to produce the datasets.
#'
#'    What data is included in the calculation depends on user settings, connectivity, and whether it meets our privacy threshold.
#'    If the privacy threshold isn’t met (when somewhere isn’t busy enough to ensure anonymity) we don’t show a change for the day.
#'    As a result, you may encounter empty fields for certain places and dates. We include categories that are useful to social distancing
#'    efforts as well as access to essential services. We calculate these insights based on data from users who have opted-in to Location History
#'    for their Google Account, so the data represents a sample of our users. As with all samples, this may or may not represent the exact behavior
#'    of a wider population.
#'
#' @references Google LLC "Google COVID-19 Community Mobility Reports." https://www.google.com/covid19/mobility/ Accessed: <Date>.
#'
#' @export
#' @md
get_google_mobility <- function(admin = c("county", "state", "country", "all"), us_only = TRUE) {

  admin <- match.arg(admin)

  url <- "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv"

  df <- readr::read_csv(url,
      col_types = readr::cols(
        country_region_code = readr::col_character(),
        country_region = readr::col_character(),
        sub_region_1 = readr::col_character(),
        sub_region_2 = readr::col_character(),
        date = readr::col_date(format = ""),
        retail_and_recreation_percent_change_from_baseline = readr::col_double(),
        grocery_and_pharmacy_percent_change_from_baseline = readr::col_double(),
        parks_percent_change_from_baseline = readr::col_double(),
        transit_stations_percent_change_from_baseline = readr::col_double(),
        workplaces_percent_change_from_baseline = readr::col_double(),
        residential_percent_change_from_baseline = readr::col_double()
        )
      ) %>%
    dplyr::rename(
      country_code = country_region_code,
      country_name = country_region,
      state_name   = sub_region_1,
      county_name  = sub_region_2,
      ggl_parks       = parks_percent_change_from_baseline,
      ggl_workplaces  = workplaces_percent_change_from_baseline,
      ggl_residential = residential_percent_change_from_baseline,
      ggl_retail_and_rec    = retail_and_recreation_percent_change_from_baseline,
      ggl_transit_stations  = transit_stations_percent_change_from_baseline,
      ggl_grocery_and_pharm = grocery_and_pharmacy_percent_change_from_baseline,
    )

  if (us_only) df <- dplyr::filter(df, country_code == "US")

  if (admin == "county")  df <- dplyr::filter(df, !is.na(county_name))
  if (admin == "state")   df <- dplyr::filter(df, is.na(county_name), !is.na(state_name))
  if (admin == "country") df <- dplyr::filter(df, is.na(county_name), is.na(state_name))

  df
}






#' Get Latest Mobility Data from Google
#'
#' [Descartes Labs](https://descarteslabs.com/) is releasing mobility statistics (representing the distance a
#' typical member of a given population moves in a day) at the US admin1 (state)
#' and admin2 (county) level. A technical report describing the motivation behind
#' this work with methodology and definitions is available [here (pdf)](descarteslabs.com/mobility-v097).
#'
#' @importFrom magrittr %>%
#' @param admin character. One of county, state, or all.
#'
#' @return df
#'
#' @details
#'
#' Note: Data for `2020-04-20` did not meet quality control standards, and was not released.
#'
#' From the methodology paper....
#'
#' These statistics include the mean, median and quartiles. For the results reported here,
#' we focus on the median of the max-distance mobility Mmax, which we designate `m50`.
#' We further define a normalized mobility index `m50_index = 100 * (m50 / m50_norm)` where
#' `m50_norm` is a "normal" value of `m50` in a region, defined as the median `m50` in that
#' region during a designated earlier time period. We use the median weekday value of `m50`
#' between the dates of `2020-02-17` and `2020-03-07` as `m50_norm` to investigate
#' COVID-19 related changes in the US. Note that `m50` has dimensions of kilometers,
#' while `m50_index` is dimensionless. `m50_index` is easily transformed to a percentage
#' change from baseline behavior via `m50_pct_change = m50_index - 100`
#'
#' @references Warren & Skillman 2020. Mobility Changes in Response to COVID-19. [descarteslabs.com/mobility-v097](descarteslabs.com/mobility-v097)
#'
#' @export
#' @md
get_descartes_mobility <- function(admin = c("county", "state", "all")) {

  admin <- match.arg(admin)

  url <- "https://github.com/descarteslabs/DL-COVID-19/raw/master/DL-us-mobility-daterow.csv"

  df <- readr::read_csv(url,
        col_types = readr::cols(
          date = readr::col_date(format = ""),
          country_code = readr::col_character(),
          admin_level = readr::col_double(),
          admin1 = readr::col_character(),
          admin2 = readr::col_character(),
          fips = readr::col_character(),
          samples = readr::col_double(),
          m50 = readr::col_double(),
          m50_index = readr::col_double()
        )
  ) %>%
    dplyr::transmute(
      geoid = fips,
      date,
      country_code,
      admin_level,
      state_fips = substr(geoid, start = 1, stop = 2),
      state_name = admin1,
      county_name = admin2,
      des_samples = samples,
      des_m50     = m50,
      des_m50_index = m50_index,
      des_m50_pct_change = des_m50_index - 100
    )


  if (admin == "county") df <- dplyr::filter(df,  admin_level == 2)
  if (admin == "state")  df <- dplyr::filter(df,  admin_level == 1)


  df
}




