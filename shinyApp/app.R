library(shiny)
library(bslib)
library(ggplot2)
library(scales)
library(patchwork)
library(emojifont)
library(dplyr)
library(tidyr)
library(forcats)
library(stringr)
library(ggtext)
library(RMySQL)

load.fontawesome()

# Data prep --------------------------------------------------------------

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
n_dta <- impacts_output_raw %>% filter(grp == 'System total', impact == 'n_dta') %>% pull(value) %>% head(1)
dta_time <- impacts_output_raw %>% filter(grp == 'System total', impact == 'dta_time') %>% pull(value) %>% head(1)
max_date <- impacts_output_raw %>% filter(week == max(week, na.rm = TRUE)) %>% pull(week) %>% head(1) %>% lubridate::ymd() %>% format(format ="%d %B")



hospital_levels <- c(
  "System Total",
  "Southmead Hospital",
  "Bristol Royal Infirmary",
  "Weston General Hospital"
)

axis_shade <- "grey40"
col_width <- 6
b_s <- 11
label_pos <- -0.4


format_date_interval <- function(end_date) {
  # Ensure the input is a Date object
  end_date <- as.Date(end_date)
  start_date <- end_date - 69
  
  if (format(start_date, "%m") == format(end_date, "%m")) {
    return(paste0(format(start_date, "%d"), "-", format(end_date, "%d"), " ", format(start_date, "%B")))
  } else {
    return(paste0(format(start_date, "%d"), " ", format(start_date, "%B"), " - ", format(end_date, "%d"), " ", format(end_date, "%B")))
  }
}




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
  arrange(week, grp) %>%
  mutate(week = lubridate::ymd(week))

# UI SECTION -------------------------------------------------------------

ui <- page_fluid(
  # Link to external CSS file
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  fluidRow(
    column(
      width = 10,
      offset = 1,

      h3("Avoidable Impacts Estimator"),
      h6(
        "A weekly-refreshed report on avoidable harms and bed usage associated with delays to ED admission",
        style = "font-style: italic;"
      ),
      p(),
      card(
        full_screen = FALSE,
        class = "no-border-no-pad header-card",
        style = "overflow: hidden !important; margin-bottom: 3vh; flex-shrink: 0;",
        gap = "0vh",
        uiOutput("header")
      ),
      p(),

      layout_columns(
        col_widths = c(7, 5),

        card(
          class = "border-0 shadow-none",
          card_header(
            # Centering the entire header content
            class = "text-center",
            div(
              style = "display: flex; flex-direction: column; gap: 4px;",
              span(
                "Estimated number of avoidable deaths",
                style = "font-weight: bold; font-size: 1.1vw;"
              ),
              span(
                "(number of deaths that wouldn't have occurred if zero ED admission delays)",
                style = "font-weight: normal; font-size: 0.7vw; color: #666;"
              )
            )
          ),
          card_body(
            plotOutput("patchwork_plot1", height = "75vh")
          )
        ),

        card(
          class = "border-0 shadow-none",
          card_header(
            # Centering the entire header content
            class = "text-center",
            div(
              style = "display: flex; flex-direction: column; gap: 4px;",
              span(
                "Estimated avoidable acute bed utilisation",
                style = "font-weight: bold; font-size: 1.1vw;"
              ),
              span(
                "(average number of acute beds in use at any time attributable solely to ED admission delays)",
                style = "font-weight: normal; font-size: 0.7vw; color: #666;white-space: nowrap; overflow: visible;"
              )
            )
          ),
          card_body(
            plotOutput("patchwork_plot2", height = "75vh")
          )
        )
      )
    )
  )
)


# SERVER SECTION ---------------------------------------------------------
server <- function(input, output, session) {
  
  # First patchwork plot
  output$patchwork_plot1 <- renderPlot({
max_y <- max(excess_data$excess[excess_data$metric == "om6"], na.rm = TRUE) * 1.35


# Base theme to avoid repetition
shared_theme <- theme_minimal(base_size = b_s) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = rel(1.4)),
    axis.ticks.y = element_blank(),
    axis.title = element_blank(),
    axis.line.x = element_line(color = axis_shade, linewidth = 1.5),
    strip.background = element_blank(),
    strip.text = element_text(size = rel(1.7), face = "bold", hjust = 0.5),
    strip.placement = "outside"
  )

# Plot A: System Total
p_total <- excess_data %>%
  filter(metric == "om6", grp == "System Total") %>%
  ggplot(aes(x = week, y = excess)) +
  geom_col(width = col_width, fill = "#88a687") + # Slightly darker green for total
  geom_text(aes(label = round(excess)), size = 0.7*b_s, vjust = label_pos) +
  scale_y_continuous(limits = c(0, max_y), expand = c(0, 0)) +
  #   scale_x_date(
  #   breaks = unique(excess_data$week),
  #   labels = label_date_short(sep = "/"),
  # ) +
  scale_x_date(
  breaks = unique(excess_data$week),
  labels = ~ paste0(format(.x - 6, "%b %d"), "\n—\n ", format(.x, "%b %d"))
) +
  labs(x = NULL, y = NULL) +
  facet_wrap(~grp, strip.position = "bottom") +
  shared_theme

# Plot B: Individual Sites
p_sites <- excess_data %>%
  filter(metric == "om6", grp != "System Total") %>%
  ggplot(aes(x = week, y = excess)) +
  geom_col(width = col_width, fill = "#B4CEB3") +
  geom_text(aes(label = round(excess)), size = 0.7*b_s, vjust = label_pos) +
  scale_y_continuous(limits = c(0, max_y), expand = c(0, 0)) +
  scale_x_date(
  breaks = unique(excess_data$week),
  labels = ~ paste0(format(.x - 6, "%b %d"), "\n—\n", format(.x, "%b %d"))
) +
  labs(x = NULL, y = NULL) +
  facet_wrap(~grp, ncol = 1, axes = "all_x", strip.position = "bottom") +
  shared_theme

    p_total / p_sites + plot_layout(heights = c(1, 3))
  })
  
  # Second patchwork plot
  output$patchwork_plot2 <- renderPlot({
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

pal <- c(
    "System Total" =  "#6b838a",
      "Southmead Hospital" = "#88a0a8",
      "Bristol Royal Infirmary" = "#88a0a8",
      "Weston General Hospital" = "#88a0a8"
    )
    
# Base Bed Plot Logic (Icons and Bars)
render_bed_plot <- function(data, x_lim) {
  ggplot(data, aes(x = excess, y = fct_rev(grp), fill = grp, color = grp)) +
    geom_col(width = 0.3) +
    geom_text(aes(label = excess), vjust = 1, hjust = -0.75, size = b_s, fontface = "bold") +
    geom_text(aes(label = fontawesome("fa-bed")), family = "fontawesome-webfont", 
              vjust = -0.2, hjust = -0.75, size = b_s, ) +
    scale_x_continuous(limits = c(0, x_lim), expand = c(0, 0)) +
    scale_fill_manual(values = pal, guide = "none") +
    scale_colour_manual(values = pal, guide = "none") +
    theme_minimal(base_size = b_s) +
    theme(
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.line.y = element_line(color = axis_shade, linewidth = 1.5)
    )
}

bed_p_total <- render_bed_plot(filter(bed_data, grp == "System Total"), max_x_bed)
bed_p_sites <- render_bed_plot(filter(bed_data, grp != "System Total"), max_x_bed)
    
bed_p_total / bed_p_sites + plot_layout(heights = c(1, 3))
  })


  output$header <- renderUI({

    div(
      style = "line-height: 1.2; margin: 0;", # Increased line-height from 1.4 to 1.5
      HTML(glue::glue(
        "<p class='app-standard-text' style='margin: 2.5px 0; max-height: none;'><span>Delays in admitting patients from the ED are associated with increased mortality and longer acute lengths of stay. <strong>In the 10 weeks to {max_date}</strong>, the <b>{scales::number(n_dta, big.mark = ',')} medical admissions</b> from BNSSG EDs had a combined admission delay of <b>{scales::number(dta_time/60, big.mark = ',')} hours</b> ({scales::number(dta_time/(24*60), big.mark = ',')} days). Estimates of the avoidable impacts associated with these delays are detailed below. These estimates are based on multivariable regression modelling which has been externally peer-review validated.</p>"
      ))
    )
  })


}

shinyApp(ui, server)
