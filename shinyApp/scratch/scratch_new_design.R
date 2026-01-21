library(emojifont)
library(dplyr)
library(tidyr)
library(forcats)
library(stringr)
library(ggplot2)
library(patchwork)
library(ggtext)
library(RMySQL)

load.fontawesome()
# Write to MYSQL
# Connection details
host <- Sys.getenv("DB_HOST")
dbname <- Sys.getenv("DB_NAME")
user <- Sys.getenv("DB_USER")
password <- Sys.getenv("DB_CRED")
port <- 3306 # Default MySQL port (change if needed)

# Create the connection
conn <- dbConnect(dbDriver("MySQL"),
                  dbname = dbname,
                  host = host,
                  port = 3306,
                  user = user,
                  password=password)

on.exit(dbDisconnect(conn))

impacts_output_raw  <- DBI::dbGetQuery(conn, "select * from excess_impacts_dev") 




hospital_levels <- c(
  "System Total",
  "Southmead Hospital",
  "Bristol Royal Infirmary",
  "Weston General Hospital"
)

excess_data <- impacts_output_raw %>%
  filter(str_detect(impact, "^om[4,6]_impact(_full_cf)?$")) %>% 
  filter(str_to_title(grp) != "System Total") %>% # Filter out raw total
  select(week, grp, impact, value) %>%
  separate(impact, into = c("metric", "type"), sep = "_", extra = "merge") %>%
  pivot_wider(id_cols = c(week, grp, metric), names_from = type, values_from = value) %>%
  mutate(excess = round(impact - impact_full_cf, 0)) %>%
  # Re-calculate System Total from the rounded site values
  bind_rows(
    group_by(., week, metric) %>%
      summarise(excess = sum(excess, na.rm = TRUE), .groups = "drop") %>%
      mutate(grp = "System Total") 
  ) %>%
  mutate(grp = factor(str_to_title(grp), levels = hospital_levels)) %>%
  mutate(week = lubridate::ymd(week)) %>%
  arrange(week, grp)





plot <- local({
max_y <- max(excess_data$excess[excess_data$metric == "om6"], na.rm = TRUE) * 1.1 
axis_shade <- "grey40"

# Base theme to avoid repetition
shared_theme <- theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title = element_blank(),
    axis.line.x = element_line(color = axis_shade, linewidth = 1.5),
    strip.background = element_blank(),
    strip.text = element_text(size = rel(1.1), face = "bold", hjust = 0.5)
  )

# Plot A: System Total
p_total <- excess_data %>%
  filter(metric == "om6", grp == "System Total") %>%
  ggplot(aes(x = week, y = excess)) +
  geom_col(width = 0.8, fill = "#88a687") + # Slightly darker green for total
  geom_text(aes(label = round(excess)), vjust = -1) +
  scale_y_continuous(limits = c(0, max_y), expand = c(0, 0)) +
  labs(x = NULL, y = NULL) +
  facet_wrap(~grp) +
  shared_theme

# Plot B: Individual Sites
p_sites <- excess_data %>%
  filter(metric == "om6", grp != "System Total") %>%
  ggplot(aes(x = week, y = excess)) +
  geom_col(width = 0.8, fill = "#B4CEB3") +
  geom_text(aes(label = round(excess)), vjust = -1) +
  scale_y_continuous(limits = c(0, max_y), expand = c(0, 0)) +
  labs(x = NULL, y = NULL) +
  facet_wrap(~grp, ncol = 1, axes = "all_x") +
  shared_theme


# Prepare the data for the bed plot
bed_data_site <- excess_data %>%
  filter(metric == "om4", grp != "System Total") %>%
  summarise(excess = round(sum(excess)), .by = grp)

bed_data_total <- bed_data_site %>%
  summarise(excess = sum(excess)) %>%
  mutate(grp = "System Total")

# Combine and set levels for alignment
bed_data <- bind_rows(bed_data_total, bed_data_site) %>%
  mutate(grp = factor(grp, levels = hospital_levels))

# Find a common X limit for the bed plot
max_x_bed <- max(bed_data$excess) * 1.3

# Base Bed Plot Logic (Icons and Bars)
render_bed_plot <- function(data, x_lim) {
  ggplot(data, aes(x = excess, y = fct_rev(grp))) +
    geom_col(width = 0.3, fill = "#88a0a8") +
    geom_text(aes(label = excess), vjust = 1, hjust = -0.75, size = 6, fontface = "bold") +
    geom_text(aes(label = fontawesome("fa-bed")), family = "fontawesome-webfont", 
              vjust = -0.2, hjust = -0.75, size = 6, color = "#88a0a8") +
    scale_x_continuous(limits = c(0, x_lim), expand = c(0, 0)) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.line.y = element_line(color = axis_shade, linewidth = 1.5)
    )
}

bed_p_total <- render_bed_plot(filter(bed_data, grp == "System Total"), max_x_bed)
bed_p_sites <- render_bed_plot(filter(bed_data, grp != "System Total"), max_x_bed)


# Assemble the Left Column (Deaths)
left_col <- p_total / p_sites + plot_layout(heights = c(1, 3)) 

# Assemble the Right Column (Beds)
right_col <- bed_p_total / bed_p_sites + plot_layout(heights = c(1, 3))

# Combine everything
final_report <- (left_col  | plot_spacer() | right_col) + 
  plot_layout(widths = c(2, 0.1, 1))

final_report

});plot




