library(tidyverse)
library(tidycensus)
library(sf)

# create fips for NYC -----------------------------------------------------
nyc_fips <- "36NYC"
nyc_county_fips <- c("36061", "36047", "36081", "36005", "36085")


msa_file <- "data-raw/static-sources/msa-list.xls"
options(tigris_use_cache = TRUE)
api_key <- Sys.getenv("CENSUS_API_KEY")
census_api_key(api_key)

# download acs for geometry -----------------------------------------------
acs_vars <- c(
  acs_total_pop = "B25026_001",
  acs_median_income = "B07011_001",
  acs_median_age = "B01002_001"
)

# download data first and then join with geometries last ------------------
acs_data_nyc <- get_acs(
  geography = "place",
  variables = acs_vars,
  state = "New York",
  geometry = FALSE,
  keep_geo_vars = TRUE,
  output = "wide"
) %>%
  janitor::clean_names() %>%
  filter(
    name == "New York city, New York"
  ) %>%
  mutate(
    geoid = nyc_fips,
    state_fips = "36",
    state_name = "New York",
    county_name = "New York City",
    county_name_long = name
  ) %>%
  select(
    geoid = geoid,
    state_fips,
    state_name,
    county_name,
    county_name_long,
    starts_with("acs_")
  )

# non-shifted geometry just to get extra attributes -----------------------
acs_data_county <- get_acs(
  geography = "county",
  variables = acs_vars,
  state = NULL, # get everystate
  geometry = TRUE,
  keep_geo_vars = TRUE,
  output = "wide"
) %>%
  janitor::clean_names() %>%
  st_drop_geometry() %>%
  mutate(
    state_name = gsub(".*, ", "", name_y)
  ) %>%
  select(
    geoid = geoid,
    state_fips = statefp,
    state_name,
    county_name = name_x,
    county_name_long = name_y,
    starts_with("acs_")
  ) %>%
  group_by(state_name, county_name) %>%
  mutate(
    n = n(),
    county_name_clean = gsub(", .*", "", county_name_long),
  ) %>%
  ungroup() %>%
  mutate(
    county_name = if_else(n > 1, county_name_clean, county_name)
  ) %>%
  select(
    -n,
    -county_name_clean
  )


# generate name look up table ---------------------------------------------
acs_names <-
acs_data_county %>%
  select(-starts_with("acs_")) %>%
  mutate(
    county_fips = geoid,
    geoid = if_else(county_fips %in% nyc_county_fips, nyc_fips, county_fips)
  ) %>%
  select(
    geoid,
    state_fips,
    state_name,
    county_fips,
    county_name,
    county_name_long
  )


# with summarised nyc -----------------------------------------------------
acs_names_summarised_nyc <-
  acs_names %>%
  mutate(
    county_name = if_else(geoid == "36NYC", "New York City", county_name)
  ) %>%
  distinct(
    geoid,
    state_fips,
    state_name,
    county_name
  )



# now back to joining the acs data ----------------------------------------
acs_data_only <-
  acs_data_county %>%
  filter(
    !(geoid %in% nyc_county_fips)
  ) %>%
  bind_rows(acs_data_nyc) %>%
  arrange(geoid)


# join with msa data ------------------------------------------------------
msa_df <- readxl::read_xls(msa_file, skip = 2) %>%
  janitor::clean_names() %>%
  mutate(
    geoid = paste0(fips_state_code, fips_county_code),
    geoid = if_else(geoid %in% nyc_county_fips, nyc_fips, geoid)
  ) %>%
  select(
    -county_county_equivalent,
    -fips_county_code,
    -state_name
  ) %>%
  distinct(.keep_all = TRUE)

acs_data_only <- left_join(acs_data_only, msa_df, by = "geoid")


# shifted geometry for better mapping -------------------------------------
acs_geom <- get_acs(
  geography = "county",
  variables = acs_vars,
  state = NULL, # get everystate
  geometry = TRUE,
  keep_geo_vars = TRUE,
  output = "wide",
  shift_geo = TRUE
) %>%
  janitor::clean_names() %>%
  mutate(
    geoid = if_else(geoid %in% nyc_county_fips, nyc_fips, geoid),
    name = if_else(geoid %in% nyc_fips, "New York City, New York", name)
  ) %>%
  group_by(geoid) %>%
  summarise() %>% # this dissolves the nyc counties into one
  ungroup()


# join cleaned data with cleaned geometries -------------------------------
acs_data <- left_join(acs_geom, acs_data_only, by = "geoid")

# save to data ------------------------------------------------------------
usethis::use_data(acs_data,  overwrite = TRUE)
usethis::use_data(acs_names, overwrite = TRUE)
usethis::use_data(acs_names_summarised_nyc, overwrite = TRUE)

