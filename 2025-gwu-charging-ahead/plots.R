library(tidyverse)
library(cowplot)
library(oica)

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
        year = c(2024), 
        sales = c(27563000), 
        type = c('all')
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

# China sales ----

# Create data frame of EV sales
# Source: 
# https://cleantechnica.com/2024/07/02/47-plugin-vehicle-market-share-in-china-ev-sales-report/
ev_data <- data.frame(
    Rank = c(1:20, NA, NA),
    Model = c(
        "BYD Song (BEV+PHEV)", "BYD Qin Plus (BEV+PHEV)", "Tesla Model Y", "BYD Seagull",
        "BYD Destroyer 05 PHEV", "BYD Yuan Plus", "BYD Han (BEV+PHEV)", "Aion Y",
        "AITO M9 (EV+PHEV)", "Wuling HongGuang Mini EV", "Tesla Model 3", "Wuling Bingo",
        "Zeekr 001", "Li Xiang L6", "Changan Lumin", "BYD Tang (BEV+PHEV)",
        "BYD Dolphin", "AITO M7", "Denza D9 (BEV+PHEV)", "Aion S",
        "Others", "TOTAL"
    ),
    Segment = c(
        "D", "D", "D", "A", "D", "C", "E", "C", "F", "A", "D", "B", 
        "E", "D", "A", "E", "B", "E", "E", "D", NA, NA
    ),
    Sales = c(
        55552, 48668, 39985, 33544, 24690, 24646, 17240, 17190, 16462, 16441,
        15230, 13870, 13480, 12965, 12438, 12123, 10832, 10181, 10158, 9847,
        401333, 816875
    ),
    Percentage = c(
        6.8, 6.0, 4.9, 4.1, 3.0, 3.0, 2.1, 2.1, 2.0, 2.0, 1.9, 1.7, 1.7, 1.6,
        1.5, 1.5, 1.3, 1.2, 1.2, 1.2, 49.1, 100
    )
)

# Remove the "Others" and "TOTAL" rows for the chart
chart_data <- ev_data[1:20, ]

# Add country information
chart_data$Country <- ifelse(chart_data$Model %in% c("Tesla Model Y", "Tesla Model 3"), 
                             "US", "China")

# Sort by sales for the chart
chart_data <- chart_data[order(chart_data$Sales), ]

# Convert Model to factor to preserve order in plot
chart_data$Model <- factor(chart_data$Model, levels = chart_data$Model)

# Alternative visualization including sales numbers on bars
chart_data %>% 
    mutate(
        Country = as.character(Country),
        Country = ifelse(Country == 'China', 'Chinese', 'US')
    ) %>% 
    ggplot(aes(x = Sales, y = Model, fill = Country)) +
    geom_col(width = 0.8) +
    scale_fill_manual(values = c("Chinese" = "firebrick", "US" = "steelblue")) +
    labs(
        title = "Top 20 EV Models by Sales (May 2024)",
        subtitle = "Percentage is total % of all vehicle sales",
        x = "Sales Volume",
        y = NULL, 
        fill = 'Brand'
    ) +
    theme_minimal(base_family = font_main) +
    theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "right",
        plot.title = element_text(face = "bold"),
        axis.text.y = element_text(size = 9), 
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA)
    ) +
    scale_x_continuous(
        breaks = seq(0, 60000, 15000),
        limits = c(0, 60000),
        labels = scales::comma
    ) +
    # Add sales labels inside the bars
    geom_text(
        aes(label = format(Sales, big.mark = ",")),
        hjust = 1.1,
        color = "white",
        size = 3, 
        family = font_main
    ) +
    # Add percentage labels at the end of each bar
    geom_text(
        aes(label = paste0(Percentage, "%")),
        hjust = -0.2,
        size = 3, 
        family = font_main
    )

ggsave(
    here::here('images', 'china-sales-may-24.png'),
    width = 8, height = 6
)


# production barplot ----

# Get vehicle production data
data <- production

# Prepare data for stacked bar chart
# First, create a category for Europe and group other countries
production_data <- data %>%
    mutate(region = case_when(
        country == "China" ~ "China",
        country == "USA" ~ "USA",
        country %in% c("Germany", "France", "Italy", "Spain", "UK", "Czech Republic", 
                       "Poland", "Slovakia", "Romania", "Hungary", "Portugal", 
                       "Slovenia", "Austria", "Sweden", "Netherlands", "Belgium", "Finland") ~ "Europe",
        TRUE ~ "Other"
    )) %>%
    group_by(year, region) %>%
    summarize(production = sum(n, na.rm = TRUE), .groups = "drop") %>%
    group_by(year) %>%
    mutate(total = sum(production, na.rm = TRUE),
           percentage = (production / total) * 100) %>%
    ungroup()

# Ensure regions are in desired order for stacking
production_data$region <- factor(production_data$region, 
                                 levels = c("Other", "Europe", "USA", "China"))

# Create the stacked bar chart
ggplot(production_data, aes(x = year, y = percentage, fill = region)) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_manual(values = c("China" = "#E41A1C", 
                                 "USA" = "#377EB8", 
                                 "Europe" = "#4DAF4A", 
                                 "Other" = "gray")) +
    theme_minimal() +
    labs(
        title = "Share of Global Vehicle Production by Region",
        subtitle = "Percentage breakdown showing China's growing dominance",
        x = "Year",
        y = "Percentage of Global Production (%)",
        fill = "Region",
        caption = "Data source: OICA (International Organization of Motor Vehicle Manufacturers)"
    ) +
    theme(
        plot.title = element_text(face = "bold", size = 16),
        axis.title = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(face = "bold")
    ) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    guides(fill = guide_legend(reverse = TRUE))

# production line plot ----

# Get vehicle production data
data <- production %>% 
    filter(year <= 2022)

# Prepare data for line plot showing China, USA, and Europe
production_data <- data %>%
    mutate(region = case_when(
        country == "China" ~ "China",
        country == "USA" ~ "USA",
        country %in% c("Germany", "France", "Italy", "Spain", "UK", "Czech Republic", 
                       "Poland", "Slovakia", "Romania", "Hungary", "Portugal", 
                       "Slovenia", "Austria", "Sweden", "Netherlands", "Belgium", "Finland") ~ "Europe",
        TRUE ~ "Other"
    )) %>%
    group_by(year, region) %>%
    summarize(production = sum(n, na.rm = TRUE), .groups = "drop") %>%
    group_by(year) %>%
    mutate(total = sum(production, na.rm = TRUE),
           percentage = (production / total) * 100) %>%
    ungroup() %>%
    # Filter to only include China, USA, and Europe
    filter(region %in% c("China", "USA", "Europe"))

# Create data frame for labels at the end of each line
label_data <- production_data %>%
    group_by(region) %>%
    filter(year == max(year)) %>%
    ungroup()

# Define colors for consistency
region_colors <- c("China" = "#E41A1C", "USA" = "#377EB8", "Europe" = "#4DAF4A")

# Create the line plot with direct labels and no legend
ggplot(production_data, aes(x = year, y = percentage, color = region, group = region)) +
    geom_line(size = 1.2) +
    geom_point(size = 3) +
    # Add labels at the end of each line
    geom_text(data = label_data, 
              aes(label = region, x = year + 0.2, color = region),
              hjust = 0, 
              fontface = "bold",
              size = 4) +
    scale_color_manual(values = region_colors) +
    theme_minimal(base_family = font_main) +
    labs(
        title = "Share of Global Vehicle Production by Region",
        subtitle = "Comparing China, USA, and Europe",
        x = "Year",
        y = "Percentage of Global Production (%)",
        caption = "Data source: OICA (International Organization of Motor Vehicle Manufacturers)"
    ) +
    theme(
        plot.title = element_text(face = "bold", size = 16),
        axis.title = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        # Remove the legend
        legend.position = "none",
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA)
    ) +
    scale_y_continuous(
        labels = function(x) paste0(x, "%"), 
        limits = c(0, 35)
    ) +
    # Expand x-axis slightly to make room for labels
    scale_x_continuous(expand = expansion(mult = c(0.05, 0.16)))

ggsave(
    here::here('images', 'production-share.png'),
    width = 8, height = 6
)
