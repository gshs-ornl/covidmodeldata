library(tidyverse)

landscan_usa <- readr::read_csv("data-raw/static-sources/landscan-usa-counties-2018.csv")
prison_gdb <- "landscan-usa-raw/Prison_Boundaries.gdb/568b79a7e7194fcab0e3285549505e3f.gdb"

# create fips for NYC -----------------------------------------------------
nyc_fips <- "36NYC"
nyc_county_fips <- c("36061", "36047", "36081", "36005", "36085")

landscan_usa <-
  landscan_usa %>%
  mutate(
    geoid = if_else(st_cnty %in% nyc_county_fips, nyc_fips, st_cnty)
  ) %>%
  group_by(geoid) %>%
  summarise(
    night_pop = sum(night_pop),
    night_pop_1m = round(night_pop/1e6, 5),
    night_pop_100k = round(night_pop/1e5, 4),
    day_pop = sum(day_pop),
    day_pop_1m = round(day_pop/1e6, 5),
    day_pop_100k = round(day_pop/1e5, 4)
  )



landscan_state <- readr::read_csv("data-raw/static-sources/landscan-usa-counties-2018.csv") %>%
  mutate(
    fips = stringr::str_sub(st_cnty, end = 2)
  ) %>%
  group_by(fips) %>%
  summarise(
    night_pop = sum(night_pop),
    night_pop_1m = round(night_pop/1e6, 5),
    night_pop_100k = round(night_pop/1e5, 4),
    day_pop = sum(day_pop),
    day_pop_1m = round(day_pop/1e6, 5),
    day_pop_100k = round(day_pop/1e5, 4)
  )


# prison population -------------------------------------------------------
prison_df <- st_read(prison_gdb, stringsAsFactors = FALSE) %>%
  janitor::clean_names() %>%
  st_drop_geometry() %>%
  filter(
    status == "OPEN"
  ) %>%
  mutate(
    geoid = if_else(countyfips %in% nyc_county_fips, nyc_fips, countyfips),
    pop_adjusted = if_else(population < 0, as.numeric(0), as.numeric(population))
  ) %>%
  group_by(geoid) %>%
  summarise(
    prison_pop = sum(pop_adjusted, na.rm = TRUE),
    prison_count = n()
  ) %>%
  ungroup()


landscan_usa <- left_join(landscan_usa, prison_df, by = "geoid") %>%
  mutate(
    prison_count = if_else(is.na(prison_count), 0, as.numeric(prison_count)),
    prison_pop = if_else(is.na(prison_pop), 0, prison_pop)
  )


usethis::use_data(landscan_usa, overwrite = TRUE)
usethis::use_data(landscan_state, overwrite = TRUE)
