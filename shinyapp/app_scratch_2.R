library(shiny)
library(bslib)
library(ggplot2)
library(ggtext)

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
    .no-border-no-pad {
      padding: 0px !important;
      border: none !important;
      box-shadow: none !important;
      --bs-card-spacer-y: 0 !important;
      --bs-card-spacer-x: 0 !important;
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
      .vertical-line {
        position: absolute;
        left: 49.5%;
        top: 0;
        bottom: 0;
        width: 10x;
        border-left: 4px solid darkgray;
        z-index: 1000;
      }
            .primary-text {
        font-size: 30pt;
        margin-bottom: 1rem;
      }
      
      .secondary-text {
        font-size: 15pt;
        margin-bottom: 1rem;
      }
      /* Fix column wrap layouts */
      .bslib-column-wrap {
        overflow: hidden !important;
      }
  ")
  ),

  style = "height: 100vh; gap: 0rem; overflow: hidden !important; display: flex; flex-direction: column;",
  
  # Top heading paragraph
  card(
    full_screen = FALSE,
    height = "8%",
    class = "no-border-no-pad",
    style = "overflow: hidden !important;",
    uiOutput("header")
  ),

  # Two horizontally split cards
  layout_columns(
    height = "12%",
    col_widths = c(3, 3, 6),
    gap = "0px",
    style = "overflow: hidden !important;",
    card(
      full_screen = FALSE,
      height = "100%",
      class = "no-border-no-pad",
      style = "overflow: hidden !important;",
      div(
        style = "display: flex; justify-content: flex-end; align-items: top; height: 100%; padding-right: 10px; overflow: hidden;",
        HTML("<span style='text-align:center'><h4>Estimated impacts based on..</h4>")
      )
    ), 
    # Left card with right-justified text
    card(
      full_screen = FALSE,
      height = "100%",
      class = "no-border-no-pad",
      style = "overflow: hidden !important;",
      div(
        style = "display: flex; justify-content: flex-end; align-items: top; height: 100%; padding-right: 10px; overflow: hidden;",
        HTML("<h5 style=';color:#E4604A;'>..what actually happened: </h5>")
      )
    ),
    
    # Right card with left-justified text
    card(
      full_screen = FALSE,
      height = "100%",
      class = "no-border-no-pad",
      style = "overflow: hidden !important;",
      div(
        style = "display: flex; justify-content: flex-start; align-items: top; height: 100%; width: 35%; padding-left: 10px; overflow: hidden;",
        HTML("<h5 style='color: #A3A78F;' class='scale-text'>..scenario where medical D2A waits & the medical D2A queue where kept under 1 hour and 25% of the bedded capacity of the ED respectively:</h5>")
      )
    )
  ),
  # Main layout - 3 columns
  layout_columns(
    height = "75%",
    col_widths = c(4, 4, 4),
    gap = "10px",
    
    # Left column - 2 cards with HTML text
    layout_column_wrap(
      width = 1,
      height = "100%",
      style = "padding: 0;",
      
      # Top left card
      card(
        full_screen = FALSE,
        class = "no-border",
        height = "48%",
        card_body(
          HTML("<h4>Direct Impacts</h4>
               <p>Delays for admissions have direct impacts on those patients waiting.
               We have found associations with higher 30-day mortality rates and in-hospital length of stay.</p>
               <ul>"
               )
        )
      ),
      
      # Bottom left card
      card(
        full_screen = FALSE,
        class = "no-border",
        height = "48%",
        card_body(
          HTML("<h4>Indirect Impacts</h4>
               <p>
               The queue for admission has indirect impacts on other patients arriving at the ED.
               We have found associations with higher ambulance handover and response times.
               </p>")
        )
      )
    ),
    
    # Middle column - 4 stacked bar charts
    layout_column_wrap(
      width = 1,
      height = "100%",
      style = "padding: 0;",
      
      # Charts stacked vertically
      card(
        # tags$div(class = "vertical-line"),
        full_screen = FALSE,
        class = "no-border-no-pad",
        height = "100%",
        card_body(
          layout_column_wrap(
            width = 1,
            heights_equal = "row",
            
            # Chart 1
            card(
              style = "padding: 0px !important;border-top: none !important;border-left: none !important;border-right: none !important; border-bottom: 3px solid #dee2e6 !important;box-shadow: none !important;--bs-card-spacer-y: 0 !important;",
              height = "23%",
              card_body(plotOutput("chart1", height = "100%"))
            ),
            
            # Chart 2
            card(
              style = "padding: 0px !important;border-top: none !important;border-left: none !important;border-right: none !important; border-bottom: 3px solid #dee2e6 !important;box-shadow: none !important;--bs-card-spacer-y: 0 !important;",
              height = "23%",
              card_body(plotOutput("chart2", height = "100%"))
            ),
            
            # Chart 3
            card(
              style = "padding: 0px !important;border-top: none !important;border-left: none !important;border-right: none !important; border-bottom: 3px solid #dee2e6 !important;box-shadow: none !important;--bs-card-spacer-y: 0 !important;",
              height = "23%",
              card_body(plotOutput("chart3", height = "100%"))
            ),
            
            # Chart 4
            card(
              style = "padding: 0px !important;border-top: none !important;border-left: none !important;border-right: none !important; border-bottom: 3px solid #dee2e6 !important;box-shadow: none !important;--bs-card-spacer-y: 0 !important;",
              height = "23%",
              card_body(plotOutput("chart4", height = "100%"))
            )
          )
        )
      )
    ),
    
    # Right column - 4 cards with HTML text
    layout_column_wrap(
      width = 1,
      class = "no-border",
      height = "100%",
      style = "padding: 0;",
      
      # Cards stacked vertically
      card(
        class = "no-border",
        full_screen = FALSE,
        height = "100%",
        card_body(
          layout_column_wrap(
            width = 1,
            heights_equal = "row",
            
            # Right card 1
            card(
              class = "no-border-no-pad",
              height = "23%",
              card_body(
                HTML("<h4>Analysis</h4>
                     <p>This metric shows a comparison between actual and target values for Category A.</p>
                     <p>Current trend: <span style='color:green;'>Positive</span></p>")
              )
            ),
            
            # Right card 2
            card(
              class = "no-border-no-pad",
              height = "23%",
              card_body(
                HTML("<h4>Analysis</h4>
                     <p>Category B metrics compare current performance against historical benchmarks.</p>
                     <p>Current trend: <span style='color:orange;'>Steady</span></p>")
              )
            ),
            
            # Right card 3
            card(
              class = "no-border-no-pad",
              height = "23%",
              card_body(
                HTML("<h4>Analysis</h4>
                     <p>Operational metrics in Category C show our efficiency measures.</p>
                     <p>Current trend: <span style='color:green;'>Improving</span></p>")
              )
            ),
            
            # Right card 4
            card(
              class = "no-border-no-pad",
              height = "23%",
              card_body(
                HTML("<h4>Analysis</h4>
                     <p>Outcome measures in Category D reflect overall business impact.</p>
                     <p>Current trend: <span style='color:blue;'>Mixed</span></p>")
              )
            )
          )
        )
      )
    )
  )
)

server <- function(input, output) {

  gen_html_text <- function(emph_text){
    str_c("<span style='font-weight:bold;font-size:30pt;'>",
                        emph_text,
                        # "</span><br><span style='font-size:15pt'>",
                        # sub_text,
                        "</span>")
    }
  
  impacts_output <- readRDS("../data/avoidable_impacts_output.RDS") %>%
    mutate(grp = recode(grp,
                        "WESTON GENERAL HOSPITAL" = "WGH", 
                        "SOUTHMEAD HOSPITAL" = "NBT", 
                        "BRISTOL ROYAL INFIRMARY" = "BRI", 
                        "cat1" = "(Category 1)",
                        "cat2" = "(Category 2)",
                        "cat3" = "(Category 3)"
    )) %>%
    mutate(value_label = gen_html_text(emph_text))
  
  
  wi <- c(4/9, 5/9)
  hi <- c(4/5, 1/5)
  
  output$header <- renderUI(h5(
    HTML(glue::glue(
      "<p>Over the last <b>7 days</b>, our emergency departments saw <b>{with(impacts_output, expr = {value[impact == 'n_dta' & grp == 'System total' & report_date == max(report_date)]})} medical admissions</b>. These patients spent a combined <b>{round(with(impacts_output, expr = {value[impact == 'dta_time' & grp == 'System total' & report_date == max(report_date)]})/(60*24))} days</b> waiting for beds - a critical delay impacting both their care and the care for others in need of emergency help. Using regression analysis on data from the last 3 years, we have isolated the impact DTA delays and queue size has on patients and ambulance performance."
    ))
  ))
  
  
  
  impact_plots <- impacts_output %>%
    filter(report_date == max(report_date)) %>%
    filter(grp %in% c("System total", "(Category 2)")) %>%
    filter(str_detect(impact, "om"),
           !str_detect(impact, "(n_exposure|om5|raw)")) %>%
    select(impact, value, label, target, sub_text, value_label) %>%
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
        theme_void() +
        scale_y_continuous(expand = expansion(mult = c(0, 0.75))) +
        scale_fill_manual(values = c("#E4604A", "#A3A78F")) +
        theme(legend.position = "off",
              plot.margin = margin(0, 0, 0, 0, "pt"),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              plot.title = element_textbox(size = 15, hjust = 0.5)
        ) +
        labs(title =d$sub_text[1],y = "", x = "")
    })
    ) %>%
    pull(impact_plot)
  
  
  
  
  
  
  
  # Generate consistent 2-bar charts for each category
  output$chart1 <- renderPlot({
    impact_plots[[1]]
  })
  
  output$chart2 <- renderPlot({
    impact_plots[[2]]
  })
  
  output$chart3 <- renderPlot({
    impact_plots[[5]]
  })
  
  output$chart4 <- renderPlot({
    impact_plots[[4]]
  })
  
}

shinyApp(ui = ui, server = server)