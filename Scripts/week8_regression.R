# Fit four mortality models and save a Table 2-style HTML report.
library(plm)
library(texreg)

data <- read.csv("Data/Processed/final_data.csv")
stopifnot(all(data$gdp_1000 > 0, na.rm = TRUE))
data$loggdp <- log(data$gdp_1000)

outcomes <- c("Maternal mortality" = "matmor", "Under-5 mortality" = "un5mor",
              "Infant mortality" = "infmor", "Neonatal mortality" = "neomor")

predictors <- c(
  armed_conflict = "Armed conflict", loggdp = "log(GDP)", oecd = "OECD",
  pop_dens = "Population density", urban = "Urban", age_dep = "Age dependency",
  male_edu = "Male education", temp = "Average temperature",
  rainfall_1000 = "Average rainfall", earthquake = "Earthquake", drought = "Drought"
)

models <- lapply(outcomes, function(outcome) {
  plm(reformulate(names(predictors), response = outcome), data = data,
      index = c("iso", "year"), model = "within", effect = "twoways")
})

htmlreg(models, file = "Reports/week8_table2.html", ci.force = TRUE,
        custom.model.names = names(outcomes), custom.coef.map = as.list(predictors),
        caption = "Armed conflict and mortality with country and year fixed effects")