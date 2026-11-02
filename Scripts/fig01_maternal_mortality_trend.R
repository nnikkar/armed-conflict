library(dplyr)
library(ggplot2)

final <- read.csv("Data/Processed/final_data.csv")

matmor_change <- final |>
  dplyr::select(iso, year, matmor) |>
  dplyr::filter(year < 2018) |>
  arrange(iso, year) |>
  group_by(iso) |>
  mutate(diff_matmor = matmor - matmor[1L])

increase_countries <- matmor_change |>
  filter(year == 2017, diff_matmor > 0) |>
  pull(iso)

plot_data <- matmor_change |>
  filter(iso %in% increase_countries) |>
  left_join(
    final |> select(iso, country_name) |> distinct(),
    by = "iso"
  )

p <- ggplot(
  plot_data,
  aes(x = year, y = matmor, color = country_name, group = country_name)
) +
  geom_line() +
  labs(
    title = "Maternal Mortality Trends, 2000–2017",
    x = "Year",
    y = "Maternal mortality",
    color = "Country"
  )

p

# The graph shows that maternal mortality increased in 13 countries from 2000 to 2017.
# The size and pattern of the increase are quite different across countries.

ggsave(
  "Reports/fig01_maternal_mortality_trend.png",
  plot = p,
  width = 9,
  height = 6,
  dpi = 300
)

ggsave(
  "Reports/fig01_maternal_mortality_trend.pdf",
  plot = p, width = 9, height = 6)
