#' Harmonize names with other covidmodeldata data sources
#'
#' @noRd
google_translate <- function(df, admin, us_only, summarise_nyc) {


  if (us_only) df <- dplyr::filter(df, country_code == "US")

  if (admin == "county")  df <- dplyr::filter(df, !is.na(county_name) | state_name == "District of Columbia")
  if (admin == "state")   df <- dplyr::filter(df, is.na(county_name), !is.na(state_name))
  if (admin == "country") {
    df <- dplyr::filter(df, is.na(county_name), is.na(state_name))
    usethis::ui_warn("name translation and formatting have not been implemented for yet for `admin = 'country'`")
    return(df)
    }
  if (admin == "all") {
    usethis::ui_warn("name translation and formatting have not been implemented for yet for `admin = 'all'`")
    return(df)
  }

# translate county names --------------------------------------------------
  if (admin == "county") {

    google_names <-
      df %>%
      dplyr::mutate(
        county_name_new = county_name,
        county_name_new = gsub(" (County|Parish|Borough)", "", county_name_new),
        county_name_new = dplyr::if_else(state_name == "District of Columbia", "District of Columbia", county_name_new),
        county_name_new = dplyr::if_else(state_name == "South Dakota" & county_name_new == "Shannon", "Oglala Lakota", county_name_new),
        county_name_new = dplyr::if_else(state_name == "Louisiana" & county_name_new == "La Salle", "LaSalle", county_name_new)
      ) %>%
      dplyr::distinct(
        state_name,
        county_name,
        county_name_new
        ) %>%
      dplyr::group_by(
        state_name,
        county_name_new
        ) %>%
      dplyr::mutate(
        n = dplyr::n()
        ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        county_name_new = dplyr::if_else(n > 1, county_name, county_name_new),
        county_name_new = dplyr::if_else(n > 1 & !grepl(" County", county_name_new), paste(county_name_new, "city"), county_name_new)
      ) %>%
      dplyr::select(-n)

    google_translate <-
      dplyr::left_join(
        google_names,
        acs_names,
        by = c("state_name" = "state_name",
               "county_name_new" = "county_name"
               )
      )

    df <- dplyr::left_join(
      df, google_translate,
      by = c("state_name", "county_name")
      ) %>%
      dplyr::mutate(
        county_name = dplyr::if_else(is.na(county_name_new), county_name, county_name_new)
      ) %>%
      dplyr::select(-county_name_new) %>%
      dplyr::select(
        geoid,
        country_code,
        country_name,
        state_fips,
        state_name,
        county_fips,
        county_name,
        date,
        tidyselect::starts_with("ggl_")
      )


    if (summarise_nyc) {
      df_not_nyc <- dplyr::filter(df, geoid != "36NYC")

      df_nyc <- df %>%
        dplyr::filter(
          geoid == "36NYC"
        ) %>%
        dplyr::mutate(
          county_fips = geoid,
          county_name = "New York City",
          county_name_long = "New York City, New York"
        ) %>%
        dplyr::group_by(
          geoid,
          country_code,
          country_name,
          state_fips,
          state_name,
          county_fips,
          county_name,
          date
        ) %>%
        dplyr::summarise_at(
          dplyr::vars(tidyselect::starts_with("ggl_")), median, na.rm = TRUE
        )

      df <- dplyr::bind_rows(df_not_nyc, df_nyc) %>%
        dplyr::arrange(geoid, date) %>%
        dplyr::select(-county_fips)

    }

    df <- dplyr::rename(df, county_name_ggl = county_name)

  } # end if (county)


# translate state names only ----------------------------------------------
  if (admin == "state") {
    acs_state_names <- dplyr::distinct(acs_names, state_fips, state_name)

    df <- df %>%
      dplyr::left_join(acs_state_names, by = "state_name") %>%
      dplyr::select(
        country_code,
        country_name,
        state_fips,
        state_name,
        date,
        tidyselect::starts_with("ggl_")
      )

  }


  df
}
