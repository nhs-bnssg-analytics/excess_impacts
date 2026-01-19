ends  <- seq.Date(from = today() - dweeks(11), to = report_end, by = "week")

foo <- map(ends, \(d) get_impact_data(report_start = d-ddays(7),
                       report_end = d))


library(emojifont)
load.fontawesome()

library(ggtext)

excess_data <- foo %>%
  bind_rows() %>%
  filter(str_detect(impact, "^om[4,6]_impact(_full_cf)?$")) %>% 
  mutate(grp = factor(grp, levels = c("SOUTHMEAD HOSPITAL",
   "BRISTOL ROYAL INFIRMARY",
    "WESTON GENERAL HOSPITAL", 
    "System total"))) %>%
  filter(grp != "System total") %>%
  select(report_date, grp, impact, value) %>%
  separate(impact, into = c("metric", "type"), sep = "_", extra = "merge") %>%
  pivot_wider(id_cols = c(report_date, grp, metric), names_from = type, values_from = value) %>%
  mutate(excess = impact - impact_full_cf) 



death_plots <- 
  excess_data %>%
  filter(metric == "om6") %>%
  ggplot(aes(x = report_date, y = round(excess))) +
  geom_col(width = 5, fill = "#B4CEB3") +
  geom_text(aes(label = round(excess)), vjust = -1) +
  theme_minimal() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(x = "", y = "") +
  facet_wrap(vars(grp), ncol = 1, axes = "all_x") +
theme( 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_blank(),  # Removes the numbers (0, 5, 10, etc.)
    axis.ticks.y = element_blank(),
    axis.title = element_blank(),
    axis.line.x = element_line(color = "black", linewidth = 1.5),
    strip.background = element_blank()
  )
    

bed_plot <- excess_data %>%
  filter(metric == "om4") %>%
  summarise(excess = sum(excess), .by = c(grp)) %>%
  mutate(excess  = round(excess/10)) %>%
  ggplot(aes(x = excess, y = fct_rev(grp))) +
  geom_col(width = 0.3, fill = "#88a0a8") +
# Layer 1: The Number (Standard Font)
  geom_text(
    aes(label = excess),
    vjust = 1,
    hjust = -0.75,
    # nudge_y = 5,
    # nudge_x = -0.2,      # Shift left
    color = "black",      # Color for the number
    size = 6,
    fontface = "bold"
  ) +
  geom_text(
    aes(label = fontawesome("fa-bed")),
    family = "fontawesome-webfont",
    vjust = -0.2,
    hjust = -0.75,
    # nudge_y = 5,
    # nudge_x = 0.2,       # Shift right
    color = "#88a0a8",    # Color for the icon (Red)
    size = 6
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.3))) +
  scale_y_discrete(expand = expansion(mult = c(0.2, 0.2))) +
  theme_minimal() +
theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),  
    axis.text.y = element_blank(),  
    axis.ticks.x = element_blank(),
    axis.title = element_blank(),
    axis.line.y = element_line(color = "black", linewidth = 1.5),
    strip.background = element_blank()
  )
  

design <- "
  ABC
  ABC
  ABC
"




death_plots + plot_spacer() + bed_plot + plot_layout(design = design, widths = c(2, 0.1, 1))


