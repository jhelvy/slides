library(tidyverse)
library(cowplot)

df <- read_csv(file.path('data', 'world.csv'))

plot <- df %>% 
    ggplot() + 
    geom_point(aes(x = year, y = costPerKw)) + 
    scale_x_continuous(breaks = seq(2006, 2020, 2)) +
    scale_y_continuous(
        limits = c(0, 5000), 
        labels = scales::dollar,
        expand = expansion(mult = c(0, 0.05))
    ) +
    theme_minimal_grid(font_family = "Fira Sans Condensed") +
    panel_border() + 
    labs(
        title = "Global average price of solar PV modules",
        x = "Year",
        y = "Price per kW (2020 $USD)", 
        caption = "Data source: SPV consulting"
    )

ggsave(
    file.path('images', 'cost_historical.png'),
    plot, height = 5, width = 8
)
