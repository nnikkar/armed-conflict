library(tidyverse)
library(janitor)
library(purrr)

matmor0 <- read.csv("Data/Raw/maternal_mortality.csv", header = TRUE)
infmor0 <- read.csv("Data/Raw/infant_mortality.csv", header = TRUE)
neomor0 <- read.csv("Data/Raw/neonatal_mortality.csv", header = TRUE)
un5mor0 <- read.csv("Data/Raw/under5_mortality.csv", header = TRUE)

wbfun <- function(dataname, varname) {
  dataname |>
    dplyr::select(iso, X2000:X2019) |>
    pivot_longer(
      cols = starts_with("X"),
      names_to = "year",
      names_prefix = "X",
      values_to = varname
    ) |>
    mutate(year = as.numeric(year)) |>
    arrange(iso, year)
}

matmor <- wbfun(matmor0, "matmor")
infmor <- wbfun(infmor0, "infmor")
neomor <- wbfun(neomor0, "neomor")
un5mor <- wbfun(un5mor0, "un5mor")

wblist <- list(matmor, infmor, neomor, un5mor)

wbdata <- wblist |>
  reduce(full_join, by = c("iso", "year"))

disaster <- read.csv("Data/Raw/disaster.csv")

disaster <- disaster |>
  clean_names() |>
  filter(
    year >= 2000,
    year <= 2019,
    disaster_type %in% c("Earthquake", "Drought")
  ) |>
  select(year, iso, disaster_type) |>
  mutate(
    earthquake = if_else(disaster_type == "Earthquake", 1, 0),
    drought = if_else(disaster_type == "Drought", 1, 0)
  ) |>
  group_by(iso, year) |>
  summarise(
    earthquake = max(earthquake),
    drought = max(drought),
    .groups = "drop"
  )

conflict <- read.csv("Data/Raw/conflict.csv")

covariates <- read.csv("Data/Raw/covariates.csv")

countries <- read.table(
  "Data/Raw/countries.txt",
  header = TRUE
)

regions <- read.table(
  "Data/Raw/regions.txt",
  header = TRUE
)

names(conflict)
head(conflict)
summary(conflict$best)



conflict_year <- conflict |>
  group_by(iso, year) |>
  summarise(
    armed_conflict = if_else(sum(best, na.rm = TRUE) >= 25, 1, 0),
    .groups = "drop"
  ) |>
  mutate(year = year + 1)

names(covariates)
names(countries)
names(regions)

head(countries)
head(regions)

countries <- countries |>
  rename(iso = ISO)

regions <- regions |>
  rename(iso = country_code)

final_data <- wbdata |>
  left_join(covariates, by = c("iso", "year")) |>
  left_join(disaster, by = c("iso", "year")) |>
  left_join(conflict_year, by = c("iso", "year")) |>
  left_join(countries, by = "iso") |>
  left_join(
    regions |>
      select(iso, region, sub_region),
    by = "iso"
  ) |>
  mutate(
    earthquake = replace_na(earthquake, 0),
    drought = replace_na(drought, 0),
    armed_conflict = replace_na(armed_conflict, 0)
  )

write.csv(
  final_data,
  "Data/Processed/final_data.csv",
  row.names = FALSE
)

dim(final_data)

head(final_data)

