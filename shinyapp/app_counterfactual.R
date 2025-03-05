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
    uiOutput("header")
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
            uiOutput("mort")
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
          uiOutput("readm")
        ),
        card(
          class = "no-border",
          style = "flex-grow: 1;",
          uiOutput("los")
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
        layout_column_wrap( width = 1/4,
                            card(
                              class = "no-border",
                              style = "flex-grow: 1;",
                              uiOutput("indirect_resp_1")
                            ),
                            card(
                              class = "no-border",
                              style = "flex-grow: 1;",
                              uiOutput("indirect_resp_2")
                            ),
                            card(
                              class = "no-border",
                              style = "flex-grow: 1;",
                              uiOutput("indirect_resp_3")
                            ),
                            card(
                              class = "no-border",
                              style = "flex-grow: 1;",
                              uiOutput("indirect_resp_margin")
                            )
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
  
  browser()
  
  impacts_output <- readRDS("../data/avoidable_impacts_output.RDS") %>%
    mutate(grp = recode(grp,
                        "WESTON GENERAL HOSPITAL" = "WGH", 
                        "SOUTHMEAD HOSPITAL" = "NBT", 
                        "BRISTOL ROYAL INFIRMARY" = "BRI", 
                        "cat1" = "(Category 1)",
                        "cat2" = "(Category 2)",
                        "cat3" = "(Category 3)"
    ))
  
  
  wi <- c(4/9, 5/9)
  hi <- c(4/5, 1/5)
  
  output$header <- renderUI(h4(
    HTML(glue::glue(
      "<p>Over the last <b>7 days</b>, our emergency departments saw <b>{with(impacts_output, expr = {value[impact == 'n_dta' & grp == 'System total' & report_date == max(report_date)]})} medical admissions</b>. These patients spent a combined <b>round(with(impacts_output, expr = {value[impact == 'dta_time' & grp == 'System total' & report_date == max(report_date)]})/(60*24)) days</b> waiting for beds - a critical delay impacting both their care and the care for others in need of emergency help. <br> Our analysis estimates that..</p>"
    ))
  ))
  
  gen_html_txt <- function(value, colour, metric, text) {
    glue::glue(
      '<div style="text-align: center;">
    <div style="font-size: 4em; color: {colour}; font-weight: bold;">{value}</div>
    <div>
      {text} <span style="color: {colour}; font-weight: bold;">{metric}</span>
    </div>
  </div>'
    )
  }
  

  
  foo <- impacts_output %>%
    filter(report_date == max(report_date)) %>%
    filter(grp %in% c("System total", "(Category 2)")) %>%
    filter(str_detect(impact, "om"),
           !str_detect(impact, "(n_exposure|om5)")) %>%
    select(impact, value, label, target, value_label) %>%
    mutate(impact_grp = str_remove_all(impact, "_impact(_cf)?")) %>%
    # mutate(value_label = gsub(r"((\*\*.*\*\* )(.*))", "<span style='font-weight:bold;font-size:40pt;'>\\1</span><br><span style='font-size:20pt'>\\2</span>", value_label)) %>%
    # mutate(value_label = str_remove_all(value_label, "\\*\\*")) %>%
    nest(.by = impact_grp) %>%
    mutate(impact_plot = map(data, \(d) {d %>%
        ggplot(aes(x = impact, y = value, fill = impact, style = classic_style(align = "center"))) +
        geom_col() +
        geom_hline(aes(yintercept = target), linetype = 2, size = 2) +
        geom_textbox(aes(label = value_label), fill = NA, box.color = NA, halign = 0.5, vjust = c(-0), width = unit(0.4, "npc")) +
        # geom_marquee(aes(y = 1*value, label = value_label), width = unit(10, "cm"), fill = NA, size = 10, vjust = -0.3) +
        theme_minimal() +
        scale_y_continuous(expand = expansion(mult = c(0, 0.5))) +
        scale_fill_manual(values = c("#E4604A", "#A3A78F")) +
        theme(legend.position = "off",
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()
        ) +
        labs(y = "", x = "")
    })
    ) 
  
  
  impacts_output_pres <- impacts_output %>%
    filter(report_date == max(report_date), level == "aggregate") %>%
    filter(impact %in% c("om1a","om1b","om1c", "om2", "om3", "om4", "om5", "om6")) %>%
    mutate(grp = ifelse(impact %in% c("om4", "om5", "om6"), "System", grp)) %>%
    summarise(value = sum(value), label = label[1], colour = colour[1], .by = c(grp, impact)) %>%
    mutate(metric = case_when(
      impact %in% c("om1a", "om1b", "om1c", "om2", "om3") ~ "",
      impact == "om6" ~ label,
      impact == "om5" ~ label,
      impact == "om4" ~ "LOS")) %>%
    mutate(label = case_when(
      impact == "om1a" ~ "(Category 1)",
      impact == "om1b" ~ "(Category 2)",
      impact == "om1c" ~ "(Category 3)",
      impact %in% c("om2", "om3") ~ "",
      impact %in% c("om4", "om5", "om6") ~ str_trim(str_c("excess ", str_remove(label, metric)))
    )
    ) %>%
    mutate(html = gen_html_txt(value = round(value), colour = colour, text = label, metric = metric))
  
  
  
  
  
  output$mort <- renderUI(HTML(impacts_output_pres$html[[1]]))
  output$readm <- renderUI(HTML(impacts_output_pres$html[[2]]))
  output$los <- renderUI(HTML(impacts_output_pres$html[[3]]))
  
  
  output$indirect_resp_1 <- renderUI(HTML(impacts_output_pres$html[[10]]))
  output$indirect_resp_2 <- renderUI(HTML(impacts_output_pres$html[[11]]))
  output$indirect_resp_3 <- renderUI(HTML(impacts_output_pres$html[[12]]))
  output$indirect_resp_margin <- renderUI(HTML(glue::glue('<div>hours longer for <span style="color: {impacts_output_pres$colour[10]}; font-weight: bold;">ambulance response</span></div>')))
  
  
  output$indirect_amb_1 <- renderUI(HTML(impacts_output_pres$html[[7]]))
  output$indirect_amb_2 <- renderUI(HTML(impacts_output_pres$html[[8]]))
  output$indirect_amb_3 <- renderUI(HTML(impacts_output_pres$html[[9]]))
  
  output$indirect_ed_1 <- renderUI(HTML(impacts_output_pres$html[[4]]))
  output$indirect_ed_2 <- renderUI(HTML(impacts_output_pres$html[[5]]))
  output$indirect_ed_3 <- renderUI(HTML(impacts_output_pres$html[[6]]))
  
  # output$mort <- renderPlot(direct_impact_plots[[1]])
  # output$readm <- renderPlot(direct_impact_plots[[2]])
  # output$los <- renderPlot(direct_impact_plots[[3]])
  
  output$indirect_ed = renderPlot(indirect_plot_ed) 
  output$indirect_amb = renderPlot(indirect_plot_amb) 
  
}

shinyApp(ui, server)
