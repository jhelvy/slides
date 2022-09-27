library(tidyverse)
library(logitr)
library(cowplot)
library(viridis)
library(patchwork)

set.seed(5678)

# Compare WTP from Pref and WTP space models ---------------------------------

# Extract pars
mxl_pref1 <- readRDS(here::here('models', 'mxl_pref1.Rds'))
mxl_pref2 <- readRDS(here::here('models', 'mxl_pref2.Rds'))
mxl_pref3 <- readRDS(here::here('models', 'mxl_pref3.Rds'))
mxl_wtp <- readRDS(here::here('models', 'mxl_wtp.Rds'))
pars_pref1 <- coef(mxl_pref1)
pars_pref2 <- coef(mxl_pref2)
pars_pref3 <- coef(mxl_pref3)
pars_wtp <- coef(mxl_wtp)

# Generate simulated data from the estimated parameters
N <- 10^4
draws <- randtoolbox::halton(N, normal = TRUE)
pars <- tibble(
  alpha1 = rep(pars_pref1['price'], N),
  alpha2 = pars_pref2['price_mu'] + draws*pars_pref2['price_sigma'],
  alpha3 = exp(pars_pref3['price_mu'] + draws*pars_pref3['price_sigma']),
  beta1 = pars_pref1['brandyoplait_mu'] + draws*pars_pref1['brandyoplait_sigma'],
  beta2 = pars_pref2['brandyoplait_mu'] + draws*pars_pref2['brandyoplait_sigma'],
  beta3 = pars_pref3['brandyoplait_mu'] + draws*pars_pref3['brandyoplait_sigma'],
  omega = pars_wtp['brandyoplait_mu'] + draws*pars_wtp['brandyoplait_sigma'])

df <- pars %>%
  mutate(
    pref1 = beta1 / (-1*alpha1),
    pref2 = beta2 / (-1*alpha2),
    pref3 = beta3 / alpha3
  ) %>%
  select(pref1, pref2, pref3, omega) %>%
  gather(key = "model", value = "wtp_pref", pref1:pref3) %>%
  mutate(model = fct_recode(model,
    "price_f" = "pref1",
    "price_n" = "pref2",
    "price_ln" = "pref3"
  )) %>%
  gather(key = "space", value = "wtp", c(wtp_pref, omega)) %>%
  mutate(space = fct_recode(space,
    "Preference" = "wtp_pref",
    "WTP" = "omega",
  ))

# Remove extreme values by dropping 0.1% and 99.9% quantiles
df <- df %>%
  group_by(space, model) %>%
  mutate(
    lower = quantile(wtp, 0.01),
    upper = quantile(wtp, 0.99)) %>%
  filter(wtp > lower, wtp < upper)

# Preview
df %>%
  filter(model == "price_ln") %>%
  ggplot() +
  geom_density(
    aes(x = wtp, y = ..density.., fill = space),
    color = "black", size = 0.1, alpha = 0.42) +
  scale_x_continuous(limits = c(1, 4))

# Make the plot
base_plot <- function(data, xlimits, xbreaks) {
  font <- "Fira Sans Condensed"
  plotColors <- c("grey42", "red")
  # Compute means and standard deviations
  stats <- data %>%
    group_by(space) %>%
    summarise(
      mean = mean(wtp),
      sd = sd(wtp)) %>%
    mutate(
      label_mean = paste0("Mean: ", round(mean, 2)),
      label_sd = paste0("SD:     ", round(sd, 3)))
  plot <- ggplot(data) +
    geom_density(
      aes(x = wtp, y = ..density.., fill = space),
      color = "black", size = 0.1, alpha = 0.42) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    scale_x_continuous(limits = xlimits) +
    scale_fill_manual(values = plotColors) +
    scale_color_manual(values = plotColors, guide = FALSE) +
    theme_minimal_hgrid(font_family = font) +
    panel_border() +
    theme(legend.position = "none") +
    labs(x = NULL, y = NULL)
  return(plot)
}

plot_f <- df %>%
  filter(model == "price_f") %>%
  base_plot(xlimits = c(3.6, 3.82), xbreaks = seq(3.6, 3.8, 0.5))
plot_n <- df %>%
  filter(model == "price_n") %>%
  base_plot(xlimits = c(1.5, 6.5), xbreaks = seq(2, 6, 1))
plot_ln <- df %>%
  filter(model == "price_ln") %>%
  base_plot(xlimits = c(1, 4), xbreaks = seq(1, 4, 1))

wtpCompare <- plot_f + plot_n + plot_ln

ggsave(here::here("images", "wtpCompare.pdf"),
       wtpCompare, width = 9, height = 3, device = cairo_pdf)
