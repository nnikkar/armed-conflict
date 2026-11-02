library(dplyr)
library(table1)

final <- read.csv("Data/Processed/final_data.csv")

dim(final)
names(final)

head(final)

final <- final |>
  mutate(
    armed_conflict = factor(
      armed_conflict,
      levels = c(0, 1),
      labels = c("No armed conflict", "Armed conflict")
    ),
    oecd = factor(oecd, levels = c(0, 1), labels = c("No", "Yes")),
    earthquake = factor(earthquake, levels = c(0, 1), labels = c("No", "Yes")),
    drought = factor(drought, levels = c(0, 1), labels = c("No", "Yes"))
  )

table(final$armed_conflict)

colSums(is.na(final))

# The main issue is with matmor, which has 426 missing values.
# Most other variables have only 20 missing values, gdp_1000 has 62,
# and several variables have no missing data at all.

t1 <- table1(
  ~ matmor + infmor + neomor + un5mor +
    gdp_1000 + oecd + pop_dens + urban +
    age_dep + male_edu + temp + rainfall_1000 +
    earthquake + drought |
    armed_conflict, data = final, overall = "Overall")

t1

# Table 1 compares countries with and without armed conflict.
# The armed-conflict group has higher age dependency and lower male education.

library(flextable)

t1_word <- t1flex(t1)

save_as_docx(
 "Table 1" = t1_word, path = "Reports/table01_armed_conflict.docx")

# This figure shows maternal mortality trends from 2000 to 2017
# for the countries where maternal mortality increased over time.

