require(tidyverse)
library(patchwork)
library(marquee)

impacts_output <- readRDS("data/avoidable_impacts_output.RDS")

wi <- c(5/8, 3/8)
hi <- c(4/5, 1/5)


impacts_output_pres <- impacts_output %>%
  mutate(grp = ifelse(impact %in% c("om4", "om5", "om6"), "System", grp)) %>%
  summarise(value = sum(value), label = label[1], colour = colour[1], .by = c(grp, impact)) %>%
  mutate(value = glue::glue(.open = '-{',
                            .close = '}-',
                            '### {.-{colour}- **-{round(value)}-**}'))  %>%
  mutate(label = str_replace(label,
                             pattern = "(ambulance handover|seen for treatment|ambulance response|deaths|readmissions|bed days)",
                             replacement = glue::glue(.open = "-{",
                                                      .close = "}-",
                                                      "{.-{colour}- **\\1**}"))) %>%
select(grp, impact, label, value) 


direct_impact_plots <- impacts_output_pres %>%
  filter(grp == "System") %>%
pivot_longer(cols = -c(grp, impact)) %>%
mutate(size = case_when(name == "value" ~ 20,
                        name == "label" ~ 14)) %>%
  nest(.by = c(grp, impact, name)) %>%
  mutate(plot = map(data, \(d) {
    d %>% mutate(x = 1, y = 1)  %>%
      ggplot(mapping = aes(x = x, y = y)) +
      # geom_text(aes(label = label))
      geom_marquee(
        aes(size = size, label = value),
        size = 14,
        width = 0.85,
        style = classic_style(align = "center"),
      ) +
      theme_void()
  })) %>%
  pivot_wider(id_cols = c(grp, impact), names_from = name, values_from = plot) %>%
  mutate(ptc = map2(value, label, \(v,l) wrap_plots(list(v, l), ncol = 1, heights = c(1/5,4/5)))) %>%
  pull(ptc)

indirect_ed_impact_plots <- 
  impacts_output_pres %>%
  filter(impact %in% c("om3", "om2")) %>%
  mutate(grp = recode(grp, 
                "WESTON GENERAL HOSPITAL" = "at WGH",
                "BRISTOL ROYAL INFIRMARY" = "at BRI",
                "SOUTHMEAD HOSPITAL" = "at NBT"
                )) %>%
  nest(.by = c(grp, impact, label)) %>%
  mutate(plot = map(data, \(d) {
    d %>% mutate(x = 1, y = 1)  %>%
      ggplot(mapping = aes(x = x, y = y)) +
      # geom_text(aes(label = label))
      geom_marquee(
        aes(size = size, label = value),
        size = 14,
        width = 0.85,
        style = classic_style(align = "center"),
      ) +
      theme_void()
  })) %>%
  select(-data, -impact) %>%
  nest(.by = c(label)) %>%
  mutate(value_margin_plot = map(label, \(l) {
      ggplot() +
      # geom_text(aes(label = label))
      geom_marquee(
        aes(x = 1, y = 1),
        label = l,
        size = 14,
        width = 0.85,
        style = classic_style(align = "center"),
      ) +
      theme_void()
  })
  ) %>%
  mutate(value_ptc = map(data, \(d) wrap_plots(d$plot))) %>%
  mutate(ptc = map2(value_ptc, value_margin_plot, \(v, m) wrap_plots(list(v, m), nrow = 1, widths = wi))) 
  
 
site_margin_plot <- 
  map(indirect_ed_impact_plots$data[[1]]$grp, \(l){
  ggplot() +
    # geom_text(aes(label = label))
    geom_marquee(
      aes(x = 1, y = 1),
      label = l,
      size = 14,
      width = 0.85,
      style = classic_style(align = "center"),
    ) +
    theme_void()
  }) %>%
  wrap_plots(nrow = 1) 

  
site_margin_plot <- wrap_plots(list(site_margin_plot, plot_spacer()), widths = wi)  



indirect_plot_ed <-  wrap_plots(list(indirect_ed_impact_plots$ptc[[1]],
                 indirect_ed_impact_plots$ptc[[2]],
                 site_margin_plot),
            ncol = 1)





indirect_amb_impact_plots <- 
  impacts_output_pres %>%
  filter(str_detect(impact, "om1")) %>%
  mutate(grp = recode(grp, 
                      "cat1" = "(Category 1)",
                      "cat2" = "(Category 2)",
                      "cat3" = "(Category 3)"
  )) %>%
  nest(.by = c(grp, impact, label)) %>%
  mutate(plot = map(data, \(d) {
    d %>% mutate(x = 1, y = 1)  %>%
      ggplot(mapping = aes(x = x, y = y)) +
      # geom_text(aes(label = label))
      geom_marquee(
        aes(size = size, label = value),
        size = 14,
        width = 0.85,
        style = classic_style(align = "center"),
      ) +
      theme_void()
  })) %>%
  select(-data, -impact) %>%
  nest(.by = c(label)) %>%
  mutate(value_margin_plot = map(label, \(l) {
    ggplot() +
      # geom_text(aes(label = label))
      geom_marquee(
        aes(x = 1, y = 1),
        label = l,
        size = 14,
        width = 0.85,
        style = classic_style(align = "center"),
      ) +
      theme_void()
  })
  ) %>%
  mutate(value_ptc = map(data, \(d) wrap_plots(d$plot))) %>%
  mutate(ptc = map2(value_ptc, value_margin_plot, \(v, m) wrap_plots(list(v, m), nrow = 1, widths = wi))) 

  
  
cat_margin_plot <- 
  map(indirect_amb_impact_plots$data[[1]]$grp, \(l){
    ggplot() +
      # geom_text(aes(label = label))
      geom_marquee(
        aes(x = 1, y = 1),
        label = l,
        size = 14,
        width = 1,
        style = classic_style(align = "center"),
      ) +
      theme_void()
  }) %>%
  wrap_plots(nrow = 1) 


cat_margin_plot <- wrap_plots(list(cat_margin_plot, plot_spacer()), widths = wi)  



indirect_plot_amb <-  wrap_plots(list(indirect_amb_impact_plots$ptc[[1]],
                                     cat_margin_plot),
                                ncol = 1)






handover_plots <- 
  impacts_output %>%
  filter(impact == "om2") %>%
  mutate(value = glue::glue(.open = '-{',
                            .close = '}-',
                            '### {.#9532a8 **-{round(value)}-**}')) %>%
  nest(.by = grp) %>%
  mutate(handover_plot = map(data, \(d) {
    d %>% mutate(x = 1, y = 1)  %>%
      ggplot(mapping = aes(x, y)) +
      # geom_text(aes(label = label))
      geom_marquee(
        aes(label = value),
        size = 14,
        width = 0.85,
        style = classic_style(align = "center"),
        #family = 'Comic Sans MS'
      ) +
      theme_void()
  })) %>%
  select(-data) 


ed_plots <- impacts_output %>%
  filter(impact == "om3") %>%
  mutate(value = glue::glue(.open = '-{',
                            .close = '}-',
                            '### {.#A89532 **-{round(value)}-**}')) %>%
  nest(.by = grp) %>%
  mutate(ed_plot = map(data, \(d) {
    d %>% mutate(x = 1, y = 1)  %>%
      ggplot(mapping = aes(x, y)) +
      # geom_text(aes(label = label))
      geom_marquee(
        aes(label = value),
        size = 14,
        width = 0.85,
        style = classic_style(align = "center"),
        #family = 'Comic Sans MS'
      ) +
      theme_void()
  })) %>%
  select(-data)


om_margin_plot <-   impacts_output %>%
    filter(impact %in% c("om2", "om3")) %>%
    select(impact, label, colour) %>%
    mutate(label = str_replace(label,
                               pattern = "(ambulance handover|seen for treatment)",
                               replacement = glue::glue(.open = "-{",
                                                        .close = "}-",
                                                        "{.-{colour}- **\\1**}"))) %>%
    select(-colour) %>%
  nest(.by = impact) %>%
  mutate(om_margin_plot = map(data,
                              \(d) d %>%
                                mutate(x = 1, y = 1)  %>%
                                ggplot(mapping = aes(x, y)) +
                                # geom_text(aes(label = label))
                                geom_marquee(
                                  aes(label = label),
                                  size = 8,
                                  width = 0.85,
                                  style = classic_style(align = "center"),
                                  #family = 'Comic Sans MS'
                                ) +
                                theme_void())) %>%
  select(-data)


site_margin_plot <- tibble(
  grp = c(
    "SOUTHMEAD HOSPITAL",
    "WESTON GENERAL HOSPITAL",
    "BRISTOL ROYAL INFIRMARY"
  ),
  label = c(
    "at Southmead hospital",
    "at Weston general hospital",
    "at Bristol royal infirmary"
  )
) %>%
  nest(.by = grp) %>%
  mutate(site_margin_plot = map(data,
                                \(d) d %>%
                                  mutate(x = 1, y = 1)  %>%
                                  ggplot(mapping = aes(x, y)) +
                                  # geom_text(aes(label = label))
                                  geom_marquee(
                                    aes(label = label),
                                    size = 8,
                                    width = 0.85,
                                    style = classic_style(align = "center"),
                                    #family = 'Comic Sans MS'
                                  ) +
                                  theme_void())) %>%
  select(-data)


wi <- c(3/8, 3/8, 2/8)
hi <- c(4/5, 1/5)

p1 <- left_join(handover_plots, ed_plots) %>%
  left_join(site_margin_plot) %>%
  pivot_longer(-grp, names_to = "om") %>%
  pull(value) %>%
  wrap_plots(ncol = 3, byrow = TRUE, widths = wi)

p2 <- wrap_plots(list(om_margin_plot$om_margin_plot[[1]],
                      om_margin_plot$om_margin_plot[[2]],
                      plot_spacer()), widths = wi)

wrap_plots(list(p1, p2), ncol = 1, heights = hi) +
  plot_annotation("Patients arriving at the emergency department waited an additional..") &
  theme(plot.title = element_marquee(size = 34, width = 1))
