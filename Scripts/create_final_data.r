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
  mutate(earthquake = replace_na(earthquake, 0), drought = replace_na(drought, 0),armed_conflict = replace_na(armed_conflict, 0))

write.csv(final_data,"Data/Processed/final_data.csv",row.names = FALSE)

dim(final_data)

head(final_data)

library(dplyr)
conflict |>
  count(conflict_id, iso, year, best, sort = TRUE) |>
  head(10)

colSums(is.na(conflict))
table(is.na(conflict$conflict_id),is.na(conflict$best))

conflict |>
  filter(is.na(best)) |>
  count(iso, year, sort = TRUE) |>
  head(20)

conflict |>
  filter(is.na(best)) |>
  select(iso, year) |>
  distinct() |>
  count(iso, sort = TRUE) |>
  head(20)

conflict |>
  filter(is.na(best)) |>
  select(iso, year) |>
  inner_join(
    conflict |> filter(!is.na(best)),
    by = c("iso", "year")
  ) |>
  nrow()

conflict |>
  filter(is.na(best)) |>
  count(iso, year) |>
  count(n)

range(conflict$year)
table(conflict$year)

library(ggplot2)

conflict |>
  filter(!is.na(best)) |>
  count(year) |>
  ggplot(aes(x = year, y = n)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_point(color = "red") +
  labs(title = "Armed Conflict Records Over Time",
    x = "Year", y = "Number of Records") +
  theme_minimal()

conflict |> group_by(year) |> summarise(deaths = sum(best, na.rm = TRUE)) |>
  ggplot(aes(year, deaths)) +
  geom_area(fill = "#D6A77A", alpha = 0.5) +
  geom_line(color = "#8B2635", linewidth = 1.2) +
  scale_y_continuous(labels = scales::label_comma()) +
  scale_x_continuous(breaks = seq(1999, 2018, 2)) +
  labs(title = "Conflict-Related Deaths Over Time", subtitle = "Annual total of best-estimate deaths, 1999–2018",
       x = "Year", y = "Estimated deaths") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"), panel.grid.minor = element_blank())

conflict |> group_by(iso) |> summarise(deaths = sum(best, na.rm = TRUE)) |>
  slice_max(deaths, n = 10) |>
  ggplot(aes(x = reorder(iso, deaths), y = deaths)) +
  geom_col(fill = "#376B80", width = 0.7) +
  geom_text(aes(label = scales::comma(deaths)), hjust = -0.1, size = 3.5) +
  coord_flip() +
  scale_y_continuous(labels = scales::label_comma(), expand = expansion(mult = c(0, 0.2))) +
  labs(title = "Top 10 Countries by Conflict-Related Deaths",
       subtitle = "Cumulative estimated deaths, 1999–2018",
       x = "Country (ISO code)", y = "Estimated deaths") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"), panel.grid.major.y = element_blank())
#

top5 <- conflict |> group_by(iso) |> summarise(total = sum(best, na.rm = TRUE)) |> slice_max(total, n = 5) |> pull(iso)

conflict |> filter(iso %in% top5) |> group_by(iso, year) |>
  summarise(deaths = sum(best, na.rm = TRUE), .groups = "drop") |>
  ggplot(aes(year, deaths, color = iso)) +
  geom_line(linewidth = 1.2) +
  scale_color_brewer(palette = "Dark2") +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(title = "Conflict-Related Deaths in the Top 5 Countries",
       subtitle = "Annual estimated deaths, 1999–2018",
       x = "Year", y = "Estimated deaths", color = "Country") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom")
#
conflict |> summarise(negative = sum(best < 0, na.rm = TRUE), zero = sum(best == 0, na.rm = TRUE), unique_conflicts = n_distinct(conflict_id, na.rm = TRUE))
#
conflict |> filter(!is.na(conflict_id)) |> group_by(year) |> summarise(n_conflicts = n_distinct(conflict_id)) |>
  ggplot(aes(year, n_conflicts)) +
  geom_col(fill = "#52796F", width = 0.75) +
  scale_x_continuous(breaks = seq(1999, 2018, 2)) +
  labs(title = "Number of Distinct Armed Conflicts by Year",
       subtitle = "Based on unique conflict IDs, 1999–2018",
       x = "Year", y = "Number of conflicts") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"), panel.grid.minor = element_blank())
#

conflict |> summarise(
  records = n(),
  countries = n_distinct(iso),
  conflicts = n_distinct(conflict_id, na.rm = TRUE),
  total_deaths = sum(best, na.rm = TRUE),
  mean_deaths = mean(best, na.rm = TRUE),
  median_deaths = median(best, na.rm = TRUE),
  missing = sum(is.na(best)))
#
regions <- read.table("Data/Raw/regions.txt", header = TRUE)
head(regions)
#
names(regions)
#
conflict |> left_join(regions, by = c("iso" = "country_code")) |>
  filter(!is.na(region)) |> group_by(region) |>
  summarise(deaths = sum(best, na.rm = TRUE)) |>
  ggplot(aes(reorder(region, deaths), deaths)) +
  geom_col(fill = "#B56576", width = 0.7) +
  geom_text(aes(label = scales::comma(deaths)), hjust = -0.1, size = 3.5) +
  coord_flip() +
  scale_y_continuous(labels = scales::label_comma(), expand = expansion(mult = c(0, 0.2))) +
  labs(title = "Conflict-Related Deaths by World Region",
       subtitle = "Cumulative estimated deaths, 1999–2018",
       x = NULL, y = "Estimated deaths") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"), panel.grid.major.y = element_blank())
#