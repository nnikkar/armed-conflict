# Reproduce the Week 8 model comparisons and GDP plot.
source("Scripts/week8_regression.R")

# Compare the country-only models for maternal mortality.
country_lm <- lm(matmor ~ armed_conflict + factor(iso) - 1, data = data)
country_plm <- plm(matmor ~ armed_conflict, data = data,
                   index = c("iso", "year"), model = "within")
print(c(lm = coef(country_lm)[["armed_conflict"]],
        plm = coef(country_plm)[["armed_conflict"]]))

# Compare all 11 predictors in the country-and-year models.
maternal_plm <- models[["Maternal mortality"]]
maternal_lm <- lm(update(formula(maternal_plm), . ~ . + factor(iso) + factor(year)),
                  data = data)

terms <- names(coef(maternal_plm))
comparison <- round(cbind(lm = coef(maternal_lm)[terms],
                          plm = coef(maternal_plm)[terms]), 4)
print(comparison)
write.csv(comparison, "Reports/week8_model_comparison.csv")

# Save the unadjusted GDP scatterplot with a black fitted line.
png("Reports/gdp_maternal_mortality.png", width = 1200, height = 800, res = 140)
plot(matmor ~ loggdp, data = data, pch = 16, cex = 0.45,
     col = adjustcolor("steelblue", alpha.f = 0.25),
     xlab = "Log GDP per capita", ylab = "Maternal deaths per 100,000 live births")
abline(lm(matmor ~ loggdp, data = data), col = "black", lwd = 2)
dev.off()