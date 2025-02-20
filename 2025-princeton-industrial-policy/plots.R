library(tidyverse)
library(cowplot)
options(dplyr.width = Inf)

# Plot settings

font_main <- "Fira Sans Condensed"
font_label <- 'Roboto Condensed'
plotColors <- c(
    red    = "#980000FF",
    grey   = "grey60",
    green  = "#8FC977FF",
    yellow = "#E8BF4DFF",
    blue   = "#80C5DCFF"
)

pev18 <- read_csv(here::here('data', 'worldEvSales.csv')) %>%
    pivot_longer(
        names_to = 'year',
        values_to = 'sales',
        cols = '2010':'2021'
    ) %>%
    mutate(
        market = Year, type = 'pev', 
        year = as.numeric(year)
    ) %>%
    filter(market == "China") %>%
    select(year, sales) %>% 
    filter(year <= 2018)
pev24 <- read_csv(here::here('data', 'china-sales-exports.csv')) %>% 
    filter(type == 'nev') %>% 
    select(year, sales) %>% 
    mutate(sales = sales*10^6)
pev <- bind_rows(pev18, pev24) %>% 
    arrange(year) %>% 
    mutate(type = 'pev')
cv <- oica::sales_country %>%
    filter(country == 'China', type == 'pc') %>%
    group_by(year) %>%
    summarise(sales = sum(n)) %>%
    mutate(type = 'all') %>%
    filter(year >= min(pev$year)) %>% 
    # Add latest data
    rbind(data.frame(
        year = c(2022, 2023, 2024), 
        sales = c(23563287, 26062824, 27563000), 
        type = c('all', 'all', 'all')
    ))
annualSales <- rbind(pev, cv) %>%
    pivot_wider(names_from = type, values_from = sales) %>%
    mutate(
        icev = all - pev,
        percent_pev = pev / all
    ) %>%
    pivot_longer(
        names_to = 'type',
        values_to = 'sales',
        cols = c('pev', 'icev')
    ) %>%
    mutate(
        sales = sales / 10^6,
        all = all / 10^6
    )

annualSales %>%
    mutate(
        type = str_to_upper(type),
        type = ifelse(type == 'PEV', 'EV', 'ICEV'),
        type = fct_relevel(type, c('EV', 'ICEV')),
        percent_pev = round(percent_pev, 3),
        label = scales::percent(percent_pev)
    ) %>%
    ggplot() +
    geom_col(
        mapping = aes(x = year, y = sales, fill = type),
        alpha = 0.8, width = 0.8
    ) +
    geom_text(
        data = . %>% filter(type == 'EV', percent_pev > 0),
        mapping = aes(x = year, y = all, label = label),
        nudge_y = 0.8, hjust = 0.5,
        family = font_label
    ) +
    scale_fill_manual(values = as.vector(plotColors[c('red', 'grey')])) +
    scale_x_continuous(breaks = seq(2010, 2024, 1)) +
    scale_y_continuous(
        breaks = c(10, 20, 30),
        limits = c(0, 30),
        expand = expansion(mult = c(0, 0.05)),
    ) +
    labs(
        fill = '',
        x = NULL,
        y = 'Annual Vehicle Sales (Millions)',
        title = "In China, PEV sales grow while ICEV sales slow",
        subtitle = "After peaking in 2017, internal combustion engine vehicle (ICEV) sales have declined for 7 straight years",
        caption = 'Data sources: OICA, marklines.com'
    ) +
    theme_minimal_hgrid(font_family = font_main) +
    theme(
        plot.caption = element_text(hjust = 0, face = "italic"),
        plot.title.position = "plot",
        plot.caption.position =  "plot", 
        legend.position = c(0.05, 0.9), 
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA) 
    ) +
    geom_segment(
        data = data.frame(x = 2013.3, xend = 2013.7, y = 22.7, yend = 21.3),
        mapping = aes(x = x, y = y, xend = xend, yend = yend),
        arrow = arrow(20, unit(0.1, "inches"), "last", "closed"),
        alpha = 1, inherit.aes = FALSE
    ) +
    annotate( 
        x = 2013.4, y = 23.5, geom = 'text',
        label = 'PEV % of total sales',
        family = 'Roboto Condensed'
    )

ggsave(
    here::here('images', 'annual-sales.png'),
    width = 8, height = 6
)
