library(tidyverse)
library(shiny)
library(bslib)
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
      margin: 0 !important;
      border: none !important;
      box-shadow: none !important;
      --bs-card-spacer-y: 0 !important;
      --bs-card-spacer-x: 0 !important;
    }
    .no-border-no-pad-text-center {
      padding: 0px !important;
      margin: 0 !important;
      border: none !important;
      box-shadow: none !important;
      display: grid;
      place-items: center start;
      height: 100%;
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
      .card {
       border-radius: 0 !important;
      }
      .layout-column-no-gap {
      --bs-layout-columns-gap: 0 !important;
      --bs-layout-rows-gap: 0 !important;
      }
            /* Remove vertical padding from select inputs */
      .form-group {
        margin-bottom: 0 !important;
        margin-top: 0 !important;
        padding-bottom: 0 !important;
        padding-top: 0 !important;
      }
      
      /* Optional: adjust the select box itself if needed */
      .selectize-control {
        margin-bottom: 0 !important;
        margin-top: 0 !important;
      }
  ")
  ),

  style = "height: 100vh; gap: 0rem; overflow: hidden !important; display: flex; flex-direction: column;",
  
  layout_columns(gap = "0px", col_widths = c(7, 2, 3),style = "width: 27%;", uiOutput("report_date"), HTML("<div>Run report for:</div>"), selectInput("grp_select", label = NULL, choices = c("System total", "BRI", "NBT", "WGH"))),
  
  # Top heading paragraph
  card(
    full_screen = FALSE,
    height = "10%",
    class = "no-border-no-pad",
    style = "overflow: hidden !important;",
    gap = "0px",
    uiOutput("header")
  ),

  # Two horizontally split cards
  layout_columns(
    height = "10%",
    col_widths = c(4, 2, 2, 4),
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
        style = "display: flex; justify-content: flex-start; align-items: top; height: 100%; padding-left: 10px; overflow: hidden;",
        HTML("<h5 style='color: #A3A78F;' class='scale-text'>..scenario where medical D2A waits & the medical D2A queue where kept under 1 hour and 25% of the bedded capacity of the ED respectively:</h5>")
      )
    )
  ),
  # Main layout - 3 columns
  layout_columns(
    height = "72%",
    col_widths = c(4, 4, 4),
    gap = "0px",
    # style = "overflow: hidden !important;",
    class = "layout-column-no-gap",
    
    # Left column - 2 cards with HTML text
    layout_column_wrap(
      width = 1,
      height = "100%",
      class = "no-border-no-pad-text-center",
      # style = "padding: 0;",
      
      # Top left card
      card(
        full_screen = FALSE,
        class = "no-border-no-pad-text-center",
        height = "48%",
        card_body(
          HTML("<h4>Direct Impacts</h4>
               <p>Delays to admission have direct impacts on those patients waiting.
               We have found associations with higher 30-day mortality rates and in-hospital length of stay.</p>
               <ul>"
               )
        )
      ),
      
      # Bottom left card
      card(
        full_screen = FALSE,
        class = "no-border-no-pad-text-center",
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
              class = "no-border-no-pad",
              # style = "padding: 0px !important;border-top: none !important;border-left: none !important;border-right: none !important; border-bottom: 3px solid #dee2e6 !important;box-shadow: none !important;--bs-card-spacer-y: 0 !important;",
              height = "23%",
              card_body(plotOutput("chart1", height = "100%"))
            ),
            
            # Chart 2
            card(
              class = "no-border-no-pad",
              # style = "padding: 0px !important;border-top: none !important;border-left: none !important;border-right: none !important; border-bottom: 3px solid #dee2e6 !important;box-shadow: none !important;--bs-card-spacer-y: 0 !important;",
              height = "23%",
              card_body(plotOutput("chart2", height = "100%"))
            ),
            
            # Chart 3
            card(
              class = "no-border-no-pad",
              # style = "padding: 0px !important;border-top: none !important;border-left: none !important;border-right: none !important; border-bottom: 3px solid #dee2e6 !important;box-shadow: none !important;--bs-card-spacer-y: 0 !important;",
              height = "23%",
              card_body(plotOutput("chart3", height = "100%"))
            ),
            
            # Chart 4
            card(
              class = "no-border-no-pad",
              # style = "padding: 0px !important;border-top: none !important;border-left: none !important;border-right: none !important; border-bottom: 3px solid #dee2e6 !important;box-shadow: none !important;--bs-card-spacer-y: 0 !important;",
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
      height = "100%",
      class = "no-border-no-pad-text-center",
      # style = "padding: 0;",
      
      # Right card 1
      card(
        class = "no-border-no-pad-text-center",
        height = "23%",
        style = "margin-bottom: 8px;",
        # Small gap between inner cards
        card_body(
          style = "padding: 10px;",
          # Reduce internal padding but keep some for readability
          uiOutput("mort_subtext")
        )
      ),
      
      # Right card 2
      card(
        class = "no-border-no-pad-text-center",
        height = "23%",
        style = "margin-bottom: 8px;",
        # Small gap between inner cards
        card_body(
          style = "padding: 10px;",
          # Reduce internal padding but keep some for readability
          uiOutput("los_subtext")
        )
      ),
      
      # Right card 3
      card(
        class = "no-border-no-pad-text-center",
        height = "23%",
        style = "margin-bottom: 8px;",
        # Small gap between inner cards
        card_body(
          style = "padding: 10px;",
          # Reduce internal padding but keep some for readability
          uiOutput("resp_subtext")
        )
      ),
      
      # Right card 4
      card(
        class = "no-border-no-pad-text-center",
        height = "23%",
        card_body(
          style = "padding: 10px;",
          # Reduce internal padding but keep some for readability
          uiOutput("handover_subtext")
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
  
  impacts_output_raw <- readRDS("../data/avoidable_impacts_output.RDS") %>%
    mutate(grp = recode(grp,
                        "WESTON GENERAL HOSPITAL" = "WGH", 
                        "SOUTHMEAD HOSPITAL" = "NBT", 
                        "BRISTOL ROYAL INFIRMARY" = "BRI", 
                        "cat1" = "(Category 1)",
                        "cat2" = "(Category 2)",
                        "cat3" = "(Category 3)"
    )) %>%
    mutate(value_label = gen_html_text(emph_text))
  
  # Filtered data based on input selection
  impacts_output <- reactive({
    impacts_output_raw %>%
      filter(grp == input$grp_select)
  })
  
  
  # Store max report date as a static value
  max_report_date <- max(impacts_output_raw$report_date)
  
  # Create a reactive expression for the filtered data
  filtered_data <- reactive({
    impacts_output_raw %>%
      filter(grp == input$grp_select)
  })
  
  # Extract key metrics into separate reactive expressions
  n_admits <- reactive({
    filtered_data() %>%
      filter(impact == 'n_dta', report_date == max_report_date) %>%
      pull(value)
  })
  
  dta_time <- reactive({
    filtered_data() %>%
      filter(impact == 'dta_time', report_date == max_report_date) %>%
      pull(value)
  })
  
  excess_mort <- reactive({
    filtered_data() %>%
      filter(report_date == max_report_date, impact %in% c("om6_impact", "om6_impact_cf")) %>%
      select(impact, value) %>%
      pivot_wider(names_from = impact) %>%
      mutate(diff = round(om6_impact) - round(om6_impact_cf)) %>%
      pull(diff)
  })
  
  excess_los <- reactive({
    filtered_data() %>%
      filter(report_date == max_report_date, impact %in% c("om4_impact", "om4_impact_cf")) %>%
      select(impact, value) %>%
      pivot_wider(names_from = impact) %>%
      mutate(diff = round(om4_impact) - round(om4_impact_cf)) %>%
      pull(diff) %>%
      `*`(7)
  })
  
  excess_resp <- reactive({
    filtered_data() %>%
      filter(report_date == max_report_date, impact %in% c("om1b_raw")) %>%
      pull(emph_text)
  })
  
  n_resp <- reactive({
    filtered_data() %>%
      filter(report_date == max_report_date, impact %in% c("om1b_n_exposure")) %>%
      pull(emph_text)
  })
  
  resp_target <- reactive({
    filtered_data() %>%
      filter(report_date == max_report_date, impact %in% c("om1b_n_exposure")) %>%
      pull(target)
  })
  
  excess_handover <- reactive({
    filtered_data() %>%
      filter(report_date == max_report_date, impact %in% c("om2_raw")) %>%
      pull(emph_text)
  })
  
  n_handover <- reactive({
    filtered_data() %>%
      filter(report_date == max_report_date, impact %in% c("om2_n_exposure")) %>%
      pull(emph_text)
  })
  
  handover_target <- reactive({
    filtered_data() %>%
      filter(report_date == max_report_date, impact %in% c("om2_n_exposure")) %>%
      pull(target)
  })
  
  n_amb <- reactive({
    val <- excess_handover()
    if (is.null(val) || length(val) == 0) return(0)
    
    val %>%
      str_remove_all(",") %>%
      as.numeric() %>%
      `/`(168) %>%
      round()
  })
  
  # Function to get chart data by metric
  get_chart_data <- function(metric) {
    # For om1b and om2 metrics, always use Category 2
    if (metric %in% c("om1b")) {
      return(    impacts_output_raw %>%
                   filter(report_date == max_report_date,
                          str_detect(impact, paste0("^", metric))) %>%
                   select(impact, value, label, target, sub_text, value_label))
    } else {
      # For other metrics, use the user-selected group
      return(    impacts_output_raw %>%
                   filter(report_date == max_report_date,
                          grp == input$grp_select,  
                          str_detect(impact, paste0("^", metric))) %>%
                   select(impact, value, label, target, sub_text, value_label))
    }
    

  }
  
  # Get individual chart data directly
  mortality_chart_data <- reactive({
    get_chart_data("om6") %>% 
      filter(!str_detect(impact, "n_exposure|raw"))
  })
  
  los_chart_data <- reactive({
    get_chart_data("om4")%>% 
      filter(!str_detect(impact, "n_exposure|raw"))
  })
  
  resp_chart_data <- reactive({
    get_chart_data("om1b") %>%
      filter(!str_detect(impact, "n_exposure|raw"))
  })
  
  handover_chart_data <- reactive({
    get_chart_data("om2") %>% 
      filter(!str_detect(impact, "n_exposure|raw"))
  })
  
  # Create a function to generate plots with consistent formatting
  create_plot <- function(data) {
    if (is.null(data) || nrow(data) == 0) {
      # Return an empty plot for debugging
      return(ggplot() + 
               theme_void() + 
               labs(title = "No data available"))
    }

    data %>%
      ggplot(aes(x = impact, y = value, fill = impact)) +
      geom_col() +
      geom_hline(aes(yintercept = target), linetype = 2, size = 2, colour = "grey30") +
      geom_textbox(aes(label = value_label), fill = NA, box.color = NA, halign = 0.5, vjust = -0.2, width = unit(0.4, "npc")) +
      theme_void() +
      scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
      scale_fill_manual(values = c("#E4604A", "#A3A78F")) +
      theme(legend.position = "off",
            plot.margin = margin(0, 0, 0, 0, "pt"),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.line.x = element_line(linewidth = 3, colour = "grey30"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_textbox(size = 20, hjust = 0.5)
      ) +
      labs(title = data$sub_text[1], y = "", x = "")
  }
  
  # Render UI outputs with proper isolation 
  output$report_date <- renderUI({
    HTML(glue::glue(
      "Report based on data from {format(max_report_date, '%d %B %Y')}"
    ))
  })
  
  output$header <- renderUI({
    # Use req() to ensure reactives are ready
    req(n_admits(), dta_time())
    
    h5(
      HTML(glue::glue(
        "<p>Over the last <b>7 days</b>, our emergency departments saw <b>{n_admits()} medical admissions</b>. These patients spent a combined <b>{round(dta_time()/(60*24))} days</b> waiting for beds - a critical delay impacting both their care and the care for others in need of emergency help. Using regression analysis on data from the last 3 years, we have isolated the impact DTA delays and queue size has on patients and ambulance performance."
      ))
    )
  })
  
  output$mort_subtext <- renderUI({
    req(excess_mort(), n_admits())
    
    HTML(
      glue::glue("<p><strong>Due to delays for their admission</strong>, we estimate there will be <strong>{excess_mort()} avoidable deaths</strong> in the cohort of emergency Medical admissions from the <strong>last 7 days</strong>.</p>"
      )
    )
  })
  
  output$los_subtext <- renderUI({
    req(n_admits(), excess_los())
    
    HTML(
      glue::glue("<p>The <strong>{n_admits()}</strong> Medical admits from the <strong>last 7 days</strong> will have an estimated <strong>excess {excess_los()} days of length of stay</strong> combinded, <strong>due to their delays for admission</strong>. This is equivalent to <strong>{excess_los()/7} excess Medical beds occupied per day.</p>"
      )
    )
  })
  
  output$resp_subtext <- renderUI({
    req(n_resp(), excess_resp(), resp_target())
    
    HTML(
      glue::glue("<p>The <strong>{n_resp()}</strong> Catergory 2 ambulance responses over the <strong>last 7 days</strong> had an <strong>excess {excess_resp()} hours response time</strong> combinded, <strong>due to the medical DTA queue</strong>. The impact of this excess on meeting the <strong>national target (dashed-line)</strong> that <strong>90% of responses are within {resp_target()} minutes</strong> can be seen in the chart."
      )
    )
  })
  
  output$handover_subtext <- renderUI({
    req(n_handover(), excess_handover(), handover_target(), n_amb())
    
    HTML(
      glue::glue("<p>The <strong>{n_handover()}</strong> ambulance handovers over the <strong>last 7 days</strong> had an <strong>excess {excess_handover()} hours handover time</strong> combinded, <strong>due to the medical DTA queue</strong>. This excess delay is <strong>equivalent to {n_amb()} ambulances being out of service througout the last 7 days</strong>. The impact of this excess on meeting the <strong>national target (dashed-line)</strong> that <strong>90% of handovers are within {handover_target()} minutes</strong> can be seen in the chart."
      )
    )
  })
  
  # Plot outputs - render directly from the chart data
  output$chart1 <- renderPlot({
    create_plot(mortality_chart_data())
  })
  
  output$chart2 <- renderPlot({
    create_plot(los_chart_data())
  })
  
  output$chart3 <- renderPlot({
    create_plot(resp_chart_data())
  })
  
  output$chart4 <- renderPlot({
    create_plot(handover_chart_data())
  })
  
}

shinyApp(ui = ui, server = server)