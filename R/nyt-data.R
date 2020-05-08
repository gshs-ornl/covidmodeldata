#' Get Latest COVID-19 Data from New York Times Database
#'
#' @param admin character. county level data or state level
#'
#' @return df
#' @export
get_nyt <- function(admin = c("county", "state")) {

  admin <- match.arg(admin)

  if (admin == "county") {
    url <- "https://github.com/nytimes/covid-19-data/raw/master/us-counties.csv"
  }

  if (admin == "state") {
    url <- "https://github.com/nytimes/covid-19-data/raw/master/us-states.csv"
  }

  df <- readr::read_csv(
    url,
    col_types = readr::cols(
      date   = readr::col_date(format = ""),
      county = readr::col_character(),
      state  = readr::col_character(),
      fips   = readr::col_character(),
      cases  = readr::col_double(),
      deaths = readr::col_double()
      )
    )

  df <- janitor::clean_names(df)

  df
}


#' Format New York Times COVID-19 County Level Data
#'
#' This function is for formatting specific issues with the data being reported
#' by the NYT county data in this [repo](https://github.com/nytimes/covid-19-data)
#'
#' @param df the result of calling [get_nyt()]
#' @param na_fill logical. Default is `FALSE`
#' @param distribute_unknowns logical. Distribute cases in "Unknown" counties to
#'        actual counties. Default is `TRUE`
#' @param skip_assignment character vector. A vector of state fips codes to skip in the distribution
#'        of unknown cases.
#' @param seed integer. If set, used in [set.seed()].Default is `1998`
#'
#' @md
#'
#' @return a data frame with additionally formatted data
#' @export
#'
#' @examples
#' library(covidmap)
#'
#' df_raw <- get_nyt()
#'
#' df <- format_nyt(df_raw)
format_nyt <- function(df, distribute_unknowns = TRUE, skip_assignment = NULL, na_fill = FALSE, seed = 1998) {

  probdf <- landscan_usa
  df_formatted <- distribute_unassigned_nyt(
    df = df,
    probdf = probdf,
    distribute_unknowns = distribute_unknowns,
    skip_assignment = skip_assignment,
    na_fill = na_fill,
    seed = seed
  )

  df_formatted
}

#' Distribute unassigned values to counties based on multinomial probabilities
#'
#' This function is intented to be used under the hood of wrapper functions that
#' distributes unassigned values for a specific dataset. For example,
#' [distribute_unassigned_nyt()].
#'
#' @importFrom magrittr %>%
#' @param df data.frame of a single state
#' @param location_to_distribute unique id of location whose values will be distributed
#' @param assign_values_to unqiued ids of locations that will be recieving the unassigned values.
#'        vector of ids or if `NULL` all non `NA` fips values are used.
#'        Default is `NULL`
#' @param na_fill For days in which there are no reported data, how to fill in the `NA`s
#' @param seed integer. If set, used in [set.seed()].Default is `1998`
#' @md
#'
#' @return data frame
distribute_unassigned_state <- function(df, location_to_distribute, assign_values_to = NULL,
                                        na_fill = FALSE, seed = 1998) {

  # prep values to distribute -----------------------------------------------
  # Impute NAs ----------
  df <- df %>%
    dplyr::group_by(state, county, fips) %>%
    dplyr::arrange(date, .by_group = TRUE) %>%
    dplyr::mutate(
      new_cases = cases - dplyr::lag(cases, default = 0),
      new_deaths = deaths - dplyr::lag(deaths, default = 0),
      cases_to_add = 0,
      deaths_to_add = 0
    )

  if (!all(c("cases_mdl", "deaths_mdl") %in% names(df))) {
    df<- df %>%
      dplyr::mutate(
        cases_mdl = cases,
        deaths_mdl = deaths
      )
  }

  if (na_fill) {

    df <- df %>%
      dplyr::mutate(
        cases_mdl = round(as.numeric(zoo::na.approx(cases_mdl, na.rm = FALSE))),
        deaths_mdl = round(as.numeric(zoo::na.approx(deaths_mdl, na.rm = FALSE)))
      )
  }

  df <- df %>%
    dplyr::mutate(
      cases_mdl = dplyr::if_else(is.na(cases_mdl), 0, cases_mdl),
      deaths_mdl = dplyr::if_else(is.na(deaths_mdl), 0, deaths_mdl),
      new_cases_mdl = cases_mdl - dplyr::lag(cases_mdl, default = 0),
      new_deaths_mdl = deaths_mdl - dplyr::lag(deaths_mdl, default = 0)
    ) %>%
    dplyr::ungroup()

  df[is.na(df$new_cases), "new_cases"] <- 0
  df[is.na(df$new_deaths), "new_deaths"] <- 0

  days <- unique(df$date)

  set.seed(seed)

  updatedf_list <- lapply(days, function(day) {

    dfday <- df %>%
      dplyr::filter(
        date == day
      ) %>%
      dplyr::ungroup()

    if (location_to_distribute %in% dfday$county) {

      unknown_index <- dfday$county == location_to_distribute
      unknowndf <- dfday[unknown_index, ]
      updatedf <- dfday[!unknown_index, ]

      cases_to_distribute <- sum(unknowndf[ , "new_cases_mdl"], na.rm = TRUE)
      deaths_to_distribute <- sum(unknowndf[ , "new_deaths_mdl"], na.rm = TRUE)

      if (cases_to_distribute == 0 & deaths_to_distribute == 0) return(updatedf)

      if(is.null(assign_values_to)) target_index <- !is.na(updatedf$fips)
      else target_index <- updatedf$fips %in% assign_values_to

      total_target_cases <- sum(updatedf[target_index, "cases_mdl"], na.rm = TRUE)

      if (total_target_cases >= abs(cases_to_distribute)) probs_col <- "cases_mdl"
      else probs_col <- "night_pop"

      case_probs <- updatedf[ , probs_col , drop = TRUE]
      names(case_probs) <- updatedf$fips

      # to prevent Unknowns getting assigned value
      case_probs[is.na(updatedf$fips)] <- 0
      case_probs[is.na(case_probs)] <- 0

      if(!is.null(assign_values_to)) {
        # basically here just for KC right now
        case_probs[!(names(case_probs) %in% assign_values_to)] <- 0
      }

      orig_cases <- updatedf$cases_mdl
      orig_cases[is.na(orig_cases)] <- 0

      orig_new_cases <- updatedf$new_cases_mdl
      orig_new_cases[is.na(orig_new_cases)] <- 0

      if (cases_to_distribute < 0) {
        # admittedly arbitray cutoffs
        # or as the ML people say hand-tuned hyper parameters
        case_probs[orig_cases <= 5] <- 0

        abs_cases <- abs(cases_to_distribute)
        if (abs_cases <= 5) case_probs[orig_new_cases <= abs_cases] <- 0
        else case_probs[orig_new_cases <= 5] <- 0

      }

      cases_to_add <- as.numeric(rmultinom(1, abs(cases_to_distribute), prob = case_probs))
      if (cases_to_distribute < 0) cases_to_add <- cases_to_add * -1
      updatedf$cases_to_add <- cases_to_add

      if (deaths_to_distribute != 0) {

        county_cases <- orig_cases + updatedf$cases_to_add

        less_cases_than_deaths <-
          county_cases < abs(deaths_to_distribute) | is.na(county_cases)

        death_probs <- case_probs
        death_probs[less_cases_than_deaths] <- 0

        deaths_to_add <- as.numeric(rmultinom(1, abs(deaths_to_distribute), prob = death_probs))
        if (deaths_to_distribute < 0) deaths_to_add * -1
        updatedf$deaths_to_add <- deaths_to_add

      }

    } else return(dfday)

    updatedf
  })

  updatedf <- dplyr::bind_rows(updatedf_list)

  out_df <-
    updatedf %>%
    dplyr::group_by(state, county, fips) %>%
    dplyr::arrange(date, .by_group = TRUE) %>%
    dplyr::mutate(
      new_cases_mdl = new_cases_mdl + cases_to_add,
      new_deaths_mdl = new_deaths_mdl + deaths_to_add,
      cases_mdl = cumsum(new_cases_mdl),
      deaths_mdl = cumsum(new_deaths_mdl)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      -cases_to_add,
      -deaths_to_add
    )

  out_df
}


#' Distribute cases and deaths from "Unknown" counties to mappable counties
#'
#' This function is for handling the specific issues with the data being reported
#' by the NYT in this [repo](https://github.com/nytimes/covid-19-data),
#' so there are some hard coded things, if the issues change, then this might no longer work.
#'
#' @importFrom magrittr %>%
#' @param df data.frame of county level covid-19 data from NYT
#' @param probdf data.frame. Data to be used as probabilities for assigning observations.
#'               required to have county fips codes.
#' @param distribute_unknowns logical. If `TRUE` distribute all "Unknown" values for each state
#' @param skip_assignment character vector. A vector of state fips codes to skip in the distribution
#'        of unknown cases.
#' @param na_fill logical. default is `FALSE`
#' @param seed integer. If set, used in [set.seed()].Default is `NULL`
#' @md
#'
#' @return data frame
distribute_unassigned_nyt <- function(df, probdf, distribute_unknowns = FALSE,
                                      skip_assignment = NULL, na_fill = FALSE,
                                      seed = NULL) {

  # create fips for NYC -----------------------------------------------------
  nyc_fips <- "36NYC"
  df[df$county == "New York City", "fips"] <- nyc_fips

  df_expand <- df %>%
    dplyr::group_by(state) %>%
    tidyr::expand(
      date,
      tidyr::nesting(county, fips)
    ) %>%
    dplyr::left_join(df, by = c("state", "date", "county", "fips"))

  # join probs to df --------------------------------------------------------
  df <- dplyr::left_join(df_expand, probdf, by = c("fips" = "geoid"))

  # distribute Kansas City data ---------------------------------------------
  kc_county_fips <- c("29037", "29047", "29095", "29165")

  out_df <- distribute_unassigned_state(
    df,
    location_to_distribute = "Kansas City",
    assign_values_to = kc_county_fips,
    na_fill = na_fill,
    seed = seed
  )

  # distribute "Unknown" county data ----------------------------------------

  if (distribute_unknowns) {

    state_fips <- out_df %>%
      dplyr::select(fips, state) %>%
      dplyr::mutate(
        state_fips = substr(fips, start = 1, stop = 2)
        ) %>%
      distinct(state_fips, state) %>%
      filter(
        !is.na(state_fips)
      )

    states <- unique(out_df$state)
    state_list <- lapply(states, function(state_name) {

      state_fips_code <- state_fips$state_fips[state_fips$state == state_name]

      state_df <- out_df[out_df$state == state_name, ]
      if (!("Unknown" %in% state_df$county)) return(state_df)
      if (all(state_df$county == "Unknown")) return(state_df)
      if (state_fips_code %in% skip_assignment) return(state_df)

      updated_state_df <-
        distribute_unassigned_state(
          state_df,
          location_to_distribute = "Unknown",
          na_fill = na_fill,
          seed = seed
        )

      updated_state_df
    })

    out_df <- dplyr::bind_rows(state_list) %>%
      dplyr::filter(
        county != "Unknown"
      )
  }

  # save this for when name standardization is done here
  # out_df %>% dplyr::filter(
  #   cases_mdl > 0,
  #   county != "Unknown"
  # ) %>%
  #   dplyr::select(-tidyselect::matches("night_pop|day_pop|state_|county_")) %>%
  #   dplyr::left_join(acs_names_summarised_nyc, by = c("fips" = "geoid")) %>%
  #   dplyr::transmute(
  #     geoid = fips,
  #     state_fips,
  #     state_name = state,
  #     county_name = county,
  #     date,
  #     total_cases = cases,
  #     total_deaths = deaths,
  #     new_cases,
  #     new_deaths,
  #     total_cases_mdl = cases_mdl,
  #     total_deaths_mdl = deaths_mdl,
  #     new_cases_mdl,
  #     new_deaths_mdl
  #   )



  out_df %>% dplyr::filter(
    cases_mdl > 0
  ) %>%
    dplyr::select(-tidyselect::matches("night_pop|day_pop")) %>%
    dplyr::transmute(
      geoid = fips,
      state_fips = substr(geoid, start = 1, stop = 2),
      state_name = state,
      county_name = county,
      date,
      total_cases = cases,
      total_deaths = deaths,
      new_cases,
      new_deaths,
      total_cases_mdl = cases_mdl,
      total_deaths_mdl = deaths_mdl,
      new_cases_mdl,
      new_deaths_mdl
    )

}
