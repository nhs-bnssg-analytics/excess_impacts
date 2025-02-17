library(shiny)
library(tidyverse)
library(ggplot2)
library(patchwork)
library(marquee)
library(bslib)


ui <- page_fluid(
  
  tags$head(
    tags$style("
    /* Override tooltip styles */
      .tooltip-inner {
        background-color: white !important;
        color: black !important;
        max-width: none !important;
        padding: 10px !important;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      
    .no-border {
      border: none !important;
      box-shadow: none !important;
    }
    .column-divider {
        border-right: 3px solid #dee2e6;
        height: 100%;
        padding-right: 1rem;
    }
    .custom-table {
        border-collapse: collapse;
        width: 100%;
        margin: 20px 0;
        font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
      }
      
      .custom-table th {
        background-color: #2c3e50;
        color: white;
        padding: 12px;
        text-align: left;
        border-bottom: 2px solid #1a252f;
      }
      
      .custom-table td {
        padding: 10px;
        border-bottom: 1px solid #e0e0e0;
      }
      
      .custom-table tr:nth-child(even) {
        background-color: #f8f9fa;
      }
      
      .custom-table tr:hover {
        background-color: #e9ecef;
        transition: background-color 0.2s ease;
      }
      
      .custom-table .number-cell {
        text-align: right;
        color: #2980b9;
        font-weight: 500;
      }
      
      .custom-table .highlight {
        color: #e74c3c;
        font-weight: bold;
      }
  ")
  ),
  style = "height: 100vh; display: flex; flex-direction: column; gap: 1rem;",
  
  card(
    class = "no-border",
    style = "height: 145px; overflow: visible !important; overflow-y: hidden !important;",
    h4(HTML("<p>Over the last <b>7 days</b>, our emergency departments saw <b>674 medical admissions</b>. These patients spent a combined <b>350 days</b> waiting for beds - a critical delay impacting both their care and the care for others in need of emergency help. <br> Our analysis estimates that..</p>"))
  ),
  
  div(
    style = "height: 80vh;",  # Fixed height that with the 10vh above and gaps equals ~100vh
    layout_columns(
      col_widths = c(3, 9),
      style = "height: 100%;",
      
      # First column
      div(
        class = "column-divider",
        style = "height: 100%; display: flex; flex-direction: column; gap: 1rem;",
        card(
          class = "no-border",
          style = "height: 140px;",
          h4(HTML("<p>Due to the <b>delay in boarding</b> over the last 7 days there will be an excess of..</p>"))
        ),
        tooltip(
        card(
          class = "no-border",
          style = "flex-grow: 1;",
          plotOutput("mort", height = "33.33%")
        ),
        HTML('
      <table class="custom-table">
        <thead>
          <tr>
            <th>Category</th>
            <th>Value</th>
            <th>Status</th>
          </tr>
        </thead>
        <tbody>
          <tr>
            <td>Revenue</td>
            <td class="number-cell">$50,000</td>
            <td>Active</td>
          </tr>
          <tr>
            <td>Expenses</td>
            <td class="number-cell highlight">$45,000</td>
            <td>Warning</td>
          </tr>
          <tr>
            <td>Profit</td>
            <td class="number-cell">$5,000</td>
            <td>Normal</td>
          </tr>
          <tr>
            <td>Growth</td>
            <td class="number-cell">12%</td>
            <td>Active</td>
          </tr>
        </tbody>
      </table>
    ')),
        card(
          class = "no-border",
          style = "flex-grow: 1;",
          plotOutput("readm", height = "33.33%")
        ),
        card(
          class = "no-border",
          style = "flex-grow: 1;",
          plotOutput("los", height = "33.33%")
        ),
      ),
      
      # Second column
      div(
        class = "column-divider",
        style = "height: 100%; display: flex; flex-direction: column; gap: 1rem;",
        card(
          class = "no-border",
          style = "height: 140px;",
          h4(HTML("<p>Due to pressure on the emergency department caused by the <b>queue for medical boarding</b>, new arrivals requiring the emergency services waited..</p>"))
        ),
        card(
          class = "no-border",
          style = "flex-grow: 1;",
          plotOutput("indirect_amb", height = "33.33%")
        ),
        card(
          style = "padding: 0; margin: 1rem 0; border-top: 3px solid #dee2e6;box-shadow: none !important"
        ),
        card(
          class = "no-border",
          style = "flex-grow: 1;",
          plotOutput("indirect_ed", height = "67.67%")
        )
      )
    )
  )
)

server <- function(input, output) {
  # Placeholder plot 1
  output$plot1 <- renderPlot({
    plot(1:10, main = "Plot 1", type = "b", col = "blue")
  })
  
  # Placeholder plot 2
  output$plot2 <- renderPlot({
    plot(10:1, main = "Plot 2", type = "b", col = "red")
  })
  
  # Placeholder plot 3
  output$plot3 <- renderPlot({
    plot(1:5, main = "Plot 3", type = "b", col = "green")
  })
  
  impacts_output <- readRDS("../data/avoidable_impacts_output.RDS")
  
  wi <- c(4/9, 5/9)
  hi <- c(4/5, 1/5)
  
  
  impacts_output_pres <- impacts_output %>%
    mutate(grp = ifelse(impact %in% c("om4", "om5", "om6"), "System", grp)) %>%
    summarise(value = sum(value), label = label[1], colour = colour[1], .by = c(grp, impact)) %>%
    mutate(value = glue::glue(.open = '-{',
                              .close = '}-',
                              '### {.-{colour}- **-{round(value)}-**}'))  %>%
    mutate(label = str_replace(label,
                               pattern = "(ambulance handover|seen for treatment|ambulance response|deaths|readmissions|LOS)",
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
    mutate(ptc = map2(value, label, \(v,l) wrap_plots(list(v, l), ncol = 1))) %>%
    pull(ptc)
  
  indirect_ed_impact_plots <- 
    impacts_output_pres %>%
    filter(impact %in% c("om3", "om2")) %>%
    arrange(impact) %>%
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
          size = 11,
          width = 0.85,
          style = classic_style(align = "left"),
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
          size = 11,
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
          size = 11,
          width = 0.85,
          style = classic_style(align = "left"),
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
          size = 11,
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
  
  
  output$mort <- renderPlot(direct_impact_plots[[1]])
  output$readm <- renderPlot(direct_impact_plots[[2]])
  output$los <- renderPlot(direct_impact_plots[[3]])
  
  output$indirect_ed = renderPlot(indirect_plot_ed) 
  output$indirect_amb = renderPlot(indirect_plot_amb) 
  
}

shinyApp(ui, server)