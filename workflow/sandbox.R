library(tidyverse)
library(lme4)
library(brms)

set.seed(1058)

# number of plots
n_plots <- 15

# plot labels
plots <- factor(sample(x = LETTERS, size = n_plots))

# plot-level measure of soil moisture
plot_moisture <- runif(n = length(plots), min = 15, max = 45)
plot_moisture_s <- as.numeric(scale(plot_moisture))

# intercept
b_0 <- 0.5

# beta coefficient for soil moisture == effect size == slope
b_moisture <- 3
# b_moisture <- 6

# residual standard error
err_sd <- 6

# determine random offsets for each plot
plot_pooled_sd <- 4
plot_offsets_uncorr <- rnorm(n = length(plots), 
                             mean = 0, 
                             sd = plot_pooled_sd)

# number of individual plants measured
n_plants <- 2000

plot_data <-
  tidyr::tibble(plot = plots,
                plot_moisture = plot_moisture,
                plot_moisture_s = plot_moisture_s,
                plot_offset_uncorr = plot_offsets_uncorr,
                plot_offset_corr = plot_moisture_s*b_moisture - b_0) %>% 
  dplyr::mutate(plot_offset_semicorr = plot_offset_corr + rnorm(n = nrow(.), mean = 0, sd = 2))

cor(plot_data$plot_moisture_s, plot_data$plot_offset_uncorr)
cor(plot_data$plot_moisture_s, plot_data$plot_offset_corr)
cor(plot_data$plot_moisture_s, plot_data$plot_offset_semicorr)


ggplot2::ggplot(data = data, mapping = ggplot2::aes(x = plot_moisture_s, y = y_uncorr)) +
ggplot2::geom_point()

ggplot2::ggplot(data = data, mapping = ggplot2::aes(x = plot_moisture_s, y = y_corr)) +
ggplot2::geom_point()

ggplot2::ggplot(data = data, mapping = ggplot2::aes(x = plot_moisture_s, y = y_semicorr)) +
  ggplot2::geom_point()

# measured data for each plant
data <-
  tidyr::tibble(plantID = 1:n_plants,
                plot = sample(plots, size = n_plants, replace = TRUE)) %>%
  dplyr::left_join(y = plot_data, by = "plot") %>% 
  dplyr::mutate(residuals = rnorm(n = nrow(.), mean = 0, sd = err_sd)) %>% 
  dplyr::mutate(y_uncorr = b_0 + b_moisture*plot_moisture_s + plot_offset_uncorr + residuals,
                y_corr = b_0 + b_moisture*plot_moisture_s + plot_offset_corr + residuals,
                y_semicorr = b_0 + b_moisture*plot_moisture_s + plot_offset_semicorr + residuals)

# random effect is uncorrelated with plot-level soil moisture
fm1_lme4 <- lme4::lmer(formula = y_uncorr ~ plot_moisture_s + (1 | plot), data = data)
summary(fm1_lme4)

# random effect is perfectly correlated with plot-level soil moisture
fm2_lme4 <- lme4::lmer(formula = y_corr ~ plot_moisture_s + (1 | plot), data = data)
summary(fm2_lme4)

# random effect is semicorrelated with plot-level soil moisture
fm3_lme4 <- lme4::lmer(formula = y_semicorr ~ plot_moisture_s + (1 | plot), data = data)
summary(fm3_lme4)

estimated_plot_offsets <-
  ranef(fm1_lme4)$plot %>%
  dplyr::mutate(plot = factor(rownames(.))) %>%
  dplyr::rename(estimated_plot_offset = `(Intercept)`)

offset_diffs <-
  plot_data %>%
  dplyr::left_join(estimated_plot_offsets) %>%
  dplyr::mutate(diff = plot_offset_uncorr - estimated_plot_offset)

offset_diffs

estimated_plot_offsets <-
  ranef(fm3_lme4)$plot %>%
  dplyr::mutate(plot = factor(rownames(.))) %>%
  dplyr::rename(estimated_plot_offset = `(Intercept)`)

offset_diffs <-
  plot_data %>%
  dplyr::left_join(estimated_plot_offsets) %>%
  dplyr::mutate(diff = plot_offset_uncorr - estimated_plot_offset)

cor(offset_diffs$plot_moisture_s, offset_diffs$estimated_plot_offset)


fm1_brms <- brms::brm(formula = y_uncorr ~ plot_moisture_s + (1 | plot), data = data, cores = 4, chains = 4)
summary(fm1_brms)

pp_check(fm1_brms, nsamples = 50)
