# CHL5233 - Week 9: Multiple imputation
# 1. Load packages and prepare the analysis data

library(tidyverse)
library(mice)
library(naniar)
library(plm)

dir.create("Reports/week9", recursive = TRUE, showWarnings = FALSE)

finaldata <- read.csv("Data/Processed/final_data.csv")

# GDP must be positive before taking its logarithm
stopifnot(all(
  is.na(finaldata$gdp_1000) | finaldata$gdp_1000 > 0
))

# Keep the variables needed for imputation and analysis
# Country names and regional labels are excluded
midata <- finaldata |>
  mutate(
    loggdp = log(gdp_1000),
    iso_num = as.integer(factor(iso))
  ) |>
  dplyr::select(
    iso_num, year,
    matmor, infmor, neomor, un5mor,
    armed_conflict, loggdp, oecd, pop_dens,
    urban, age_dep, male_edu, temp,
    rainfall_1000, earthquake, drought
  )

# Check dimensions, countries, and missing values
dim(midata)
length(unique(midata$iso_num))
colSums(is.na(midata))

# 2. Configure multilevel imputation

mice0 <- mice(
  midata,
  seed = 100,
  m = 5,
  maxit = 0,
  printFlag = FALSE
)

meth <- mice0$method
pred <- mice0$predictorMatrix

impute_vars <- c(
  "urban", "male_edu", "temp", "rainfall_1000",
  "loggdp", "pop_dens",
  "matmor", "infmor", "neomor", "un5mor"
)

meth[impute_vars] <- "2l.lmer"

# Country ID identifies clusters; it is not imputed
meth["iso_num"] <- ""
pred["iso_num", ] <- 0
pred[, "iso_num"] <- 0
pred[impute_vars, "iso_num"] <- -2

# Check the setup
meth
pred[impute_vars, "iso_num"]


# 3. Run the required 10 imputations and 20 iterations

mice.multi.out <- mice(
  midata,
  seed = 100,
  m = 10,
  maxit = 20,
  method = meth,
  predictorMatrix = pred,
  printFlag = TRUE
)

# Save immediately after completion
saveRDS(
  mice.multi.out,
  "Reports/week9/mice_out_m10_iter20.rds"
)

# Inspect diagnostics
plot(mice.multi.out)
mice.multi.out$loggedEvents

# Check for remaining missing values in every completed dataset
completed_data <- complete(mice.multi.out, "all")

sapply(completed_data, function(dat) sum(is.na(dat)))

# 4. Create publication-quality convergence plots

chain_to_long <- function(x, statistic) {
  as.data.frame.table(
    x,
    responseName = "value",
    stringsAsFactors = FALSE
  ) |>
    setNames(c("variable", "iteration", "imputation", "value")) |>
    mutate(
      iteration = as.integer(as.character(iteration)),
      imputation = factor(imputation),
      statistic = statistic
    )
}

trace_data <- bind_rows(
  chain_to_long(mice.multi.out$chainMean, "Mean"),
  chain_to_long(mice.multi.out$chainVar, "Standard deviation") |>
    mutate(value = sqrt(pmax(value, 0)))
) |>
  dplyr::filter(variable %in% impute_vars)

variable_labels <- c(
  matmor = "Maternal mortality",
  infmor = "Infant mortality",
  neomor = "Neonatal mortality",
  un5mor = "Under-five mortality",
  loggdp = "Log GDP per capita",
  pop_dens = "Population density",
  urban = "Urban population",
  male_edu = "Male education",
  temp = "Temperature",
  rainfall_1000 = "Rainfall"
)

chain_colours <- c(
  "#0072B2", "#D55E00", "#009E73", "#CC79A7", "#000000",
  "#8C510A", "#6A3D9A", "#E69F00", "#008B8B", "#E7298A"
)

make_trace_plot <- function(variable_name) {
  ggplot(
    dplyr::filter(trace_data, variable == variable_name),
    aes(
      x = iteration,
      y = value,
      colour = imputation,
      group = imputation
    )
  ) +
    geom_line(linewidth = 0.7, alpha = 0.9) +
    facet_wrap(~statistic, scales = "free_y", nrow = 1) +
    scale_colour_manual(values = chain_colours) +
    scale_x_continuous(breaks = c(1, 5, 10, 15, 20)) +
    labs(
      title = paste(
        "Imputation diagnostics:",
        variable_labels[[variable_name]]
      ),
      subtitle = "10 imputations | 20 iterations | Country-level clustering",
      x = "Iteration",
      y = NULL,
      colour = "Imputation",
      caption = paste(
        "Traces summarize imputed values only.",
        "Assess stability and mixing across iterations."
      )
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.background = element_rect(fill = "white", colour = NA),
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold", size = 12),
      plot.title = element_text(face = "bold"),
      legend.position = "bottom",
      plot.caption = element_text(hjust = 0, colour = "#444444")
    ) +
    guides(colour = guide_legend(nrow = 1))
}

# Export each variable as a sharp PNG
for (v in impute_vars) {
  ggsave(
    filename = paste0("Reports/week9/convergence_", v, ".png"),
    plot = make_trace_plot(v),
    width = 12,
    height = 5,
    dpi = 300,
    bg = "white")}

# Export all ten plots into one vector PDF
pdf( "Reports/week9/convergence_all_variables.pdf", width = 12, height = 5)

for (v in impute_vars) {print(make_trace_plot(v))}

dev.off()

# Show maternal mortality diagnostics in Positron
print(make_trace_plot("matmor"))




for (v in c("matmor", "infmor", "neomor", "un5mor")) {
  x <- as.matrix(mice.multi.out$imp[[v]])
  positions <- which(x < 0, arr.ind = TRUE)

  cat("\nVariable:", v, "\n")

  print(data.frame(data_row = rownames(x)[positions[, 1]],
    imputation = colnames(x)[positions[, 2]],
    negative_value = x[positions]))}

sapply(
  finaldata[c("matmor", "infmor", "neomor", "un5mor")],
  function(x) c(min = min(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE),
    negative_count = sum(x < 0, na.rm = TRUE)))

# 5. Fit CC and MI models; pool and save results
outcomes <- c("matmor", "un5mor", "infmor", "neomor")
predictors <- c("armed_conflict", "loggdp", "oecd", "pop_dens", "urban",
               "age_dep", "male_edu", "temp", "rainfall_1000", "earthquake", "drought")
formulas <- setNames(lapply(outcomes, function(y)
  reformulate(c(predictors, "factor(iso_num)", "factor(year)"),
              response = y, intercept = FALSE)), outcomes)

cc_models <- lapply(formulas, function(f) lm(f, data = midata, na.action = na.omit))
mi_models <- lapply(formulas, function(f)
  mice::as.mira(lapply(completed_data, function(d) lm(f, data = d))))
mi_pooled <- lapply(mi_models, mice::pool)
saveRDS(list(cc = cc_models, mi = mi_models, pooled = mi_pooled),
        "Reports/week9/regression_models.rds")

# Check CC sample sizes and pooled armed-conflict estimates
sapply(cc_models, nobs)
for (y in outcomes) {
  cat("\nOutcome:", y, "\n")
  r <- summary(mi_pooled[[y]], conf.int = TRUE)
  print(r[r$term == "armed_conflict", ], row.names = FALSE)}



# 6. Build comparison table: coefficient (95% CI)
if (!requireNamespace("flextable", quietly = TRUE)) install.packages("flextable")
library(flextable)

labels <- c("Armed conflict", "Log GDP per capita", "OECD membership",
  "Population density", "Urban population", "Age dependency ratio",
  "Male education", "Temperature", "Rainfall (scaled)", "Earthquake", "Drought")
fmt <- function(b, lo, hi) sprintf("%.3f (%.3f, %.3f)", b, lo, hi)
tab <- data.frame(Predictor = labels)

for (y in outcomes) {
  b <- coef(cc_models[[y]])[predictors]
  ci <- confint(cc_models[[y]], parm = predictors)
  r <- summary(mi_pooled[[y]], conf.int = TRUE)
  r <- r[match(predictors, r$term), ]
  tab[[paste0(y, "_CC")]] <- fmt(b, ci[, 1], ci[, 2])
  tab[[paste0(y, "_MI")]] <- fmt(r$estimate, r$conf.low, r$conf.high)
}

stopifnot(nrow(tab) == 11, ncol(tab) == 9, !anyNA(tab))
write.csv(tab, "Reports/week9/CC_MI_comparison.csv", row.names = FALSE)

ft <- flextable(tab)
ft <- set_header_labels(ft, values = setNames(
  c("Predictor", rep(c("CC", "MI"), 4)), names(tab)))
ft <- add_header_row(ft, values = c("",
  "Maternal mortality", "Under-five mortality", "Infant mortality",
  "Neonatal mortality"), colwidths = c(1, 2, 2, 2, 2))
ft <- theme_booktabs(ft)
ft <- bg(ft, part = "header", bg = "#17365D")
ft <- color(ft, part = "header", color = "white")
ft <- bold(ft, part = "header")
ft <- bg(ft, i = seq(2, nrow(tab), 2), bg = "#EEF3F8")
ft <- align(ft, j = 2:9, align = "center", part = "all")
ft <- fontsize(ft, size = 9, part = "all")
ft <- padding(ft, padding = 6, part = "all")
ft <- add_header_lines(ft,
  "Mortality and armed conflict: complete-case versus multiple-imputation analyses")
ft <- add_footer_lines(ft, values = c(
  "Cells show coefficients (95% confidence intervals). CC = complete case; MI = multiple imputation.",
  paste("CC observations:", paste(outcomes, sapply(cc_models, nobs),
                                 sep = " = ", collapse = "; ")),
  "MI: 10 imputations, 20 iterations; 3,720 observations per completed dataset; Rubin's rules.",
  "All models adjust for the listed predictors and country/year fixed effects; fixed-effect coefficients are omitted.",
  "Diagnostics: normal-model imputation produced negative mortality values; child-mortality traces showed continued drift."))
ft <- fontsize(ft, part = "footer", size = 8)
ft <- autofit(ft)
save_as_html(ft, path = "Reports/week9/CC_MI_comparison.html")
browseURL(normalizePath("Reports/week9/CC_MI_comparison.html"))