library(tidyverse)
library(shiny)
library(bslib)
library(ggtext)

# # utility functions

# Prety html text
gen_html_text <- function(emph_text){
  str_c("<span style='font-weight:bold;font-size:30pt;'>",
        emph_text,
        # "</span><br><span style='font-size:15pt'>",
        # sub_text,
        "</span>")
}

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
    ggfx::with_outer_glow(geom_textbox(aes(label = value_label), fill = NA, box.color = NA, halign = 0.5, vjust = -0.2, width = unit(0.4, "npc")),
                          colour = "white", x_offset = 0, y_offset = 0, expand = 10, sigma = 0) +
    theme_void() +
    scale_y_continuous(expand = expansion(mult = c(0, 0.4))) +
    scale_fill_manual(values = c("#beaed4", "#A3A78F")) +
    theme(legend.position = "off",
          plot.margin = margin(0, 0, 0, 0, "pt"),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.line.x = element_line(linewidth = 2.5, colour = "grey30"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_textbox(size = 20, hjust = 0.5),
          plot.caption = element_textbox(size = 20, hjust = 0.5, margin = margin(t = 10), face = "bold")
    ) +
    labs(caption = data$sub_text[1], y = "", x = "")
}


# load raw data
impacts_output_raw <- readRDS("../data/avoidable_impacts_output.RDS") %>%
  mutate(grp = recode(grp,
                      "System total" = "BNSSG total (Bristol Royal Infirmary, Southmead Hospital, & Weston General Hospital)",
                      "WESTON GENERAL HOSPITAL" = "WGH", 
                      "SOUTHMEAD HOSPITAL" = "NBT", 
                      "BRISTOL ROYAL INFIRMARY" = "BRI", 
                      "cat1" = "(Category 1)",
                      "cat2" = "(Category 2)",
                      "cat3" = "(Category 3)"
  )) %>%
  mutate(
    scenario = case_when(
      str_detect(impact, r"((?<!full_)cf)") | str_detect(impact, r"((?<!full_)raw)")~ "cf",
      str_detect(impact, "full_cf") | str_detect(impact, "full_raw") ~ "full_cf",
      str_detect(impact, r"(impact(?!_cf))") ~ "actual",
      .default = "n/a"
    ),
    sub_text = case_when(
      str_detect(impact, "om6_impact") ~ str_c(sub_text, "*"),
      str_detect(impact, "om4_impact") ~ str_c(sub_text, "*"),
      str_detect(impact, "om2_impact") ~ str_c(sub_text, "**"),
      str_detect(impact, "om1[abc]_impact") ~ str_c(sub_text, "**")
    )) %>%
  mutate(impact = str_remove(impact, "_full")) %>%
  mutate(value_label = gen_html_text(emph_text)) 

# Store max report date as a static value
max_report_date <- max(impacts_output_raw$report_date)

# Create a cache environment for pre-rendered plots
plotCache <- new.env()

# Function to pre-render all charts
preRenderAllCharts <- function(impacts_output_raw) {
  # Get max report date once
  max_report_date <- max(impacts_output_raw$report_date)
  
  message("Pre-rendering all chart combinations...")
  
  # Define all possible input combinations
  hospitals <- c(
    "BNSSG total (Bristol Royal Infirmary, Southmead Hospital, & Weston General Hospital)", 
    "BRI", 
    "NBT", 
    "WGH"
  )
  
  scenarios <- c("cf", "full_cf")
  
  # Pre-render all combinations
  for (hospital in hospitals) {
    for (scen in scenarios) {
      message("Pre-rendering charts for: ", hospital, " | Scenario: ", scen)
      
      # Get mortality chart data with exact same filtering logic as in reactive expressions
      mortality_data <- impacts_output_raw %>%
        filter(report_date == max_report_date,
               grp == hospital,
               scenario %in% c("n/a", "actual", scen),
               str_detect(impact, "^om6")) %>%
        filter(!str_detect(impact, "n_exposure|raw")) %>%
        select(impact, value, label, target, sub_text, value_label)
      
      # Get LOS chart data
      los_data <- impacts_output_raw %>%
        filter(report_date == max_report_date,
               grp == hospital,
               scenario %in% c("n/a", "actual", scen),
               str_detect(impact, "^om4")) %>%
        filter(!str_detect(impact, "n_exposure|raw")) %>%
        select(impact, value, label, target, sub_text, value_label)
      
      # Get resp chart data
      resp_data <- impacts_output_raw %>%
        filter(report_date == max_report_date,
               scenario %in% c("n/a", "actual", scen),
               str_detect(impact, "^om1b")) %>%
        filter(!str_detect(impact, "n_exposure|raw")) %>%
        select(impact, value, label, target, sub_text, value_label)
      
      # Get handover chart data
      handover_data <- impacts_output_raw %>%
        filter(report_date == max_report_date,
               grp == hospital,
               scenario %in% c("n/a", "actual", scen),
               str_detect(impact, "^om2")) %>%
        filter(!str_detect(impact, "n_exposure|raw")) %>%
        select(impact, value, label, target, sub_text, value_label)
      
      # Create all charts
      chart1 <- create_plot(mortality_data)
      chart2 <- create_plot(los_data)
      chart3 <- create_plot(resp_data)
      chart4 <- create_plot(handover_data)
      
      # Store in cache with keys
      plotCache[[paste("chart1", hospital, scen, sep = "_")]] <- chart1
      plotCache[[paste("chart2", hospital, scen, sep = "_")]] <- chart2
      plotCache[[paste("chart3", hospital, scen, sep = "_")]] <- chart3
      plotCache[[paste("chart4", hospital, scen, sep = "_")]] <- chart4
    }
  }
  
  message("All charts pre-rendered successfully!")
}

preRenderAllCharts(impacts_output_raw)

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
        font-size: 25pt;
        margin-bottom: 1rem;
      }
      
      .secondary-text {
        font-size: 12pt;
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
      
       /* Scenario labels styles */
      .scenario-header {
        display: flex;
        align-items: center;
        padding: 10px 0;
      }
      
      .scenario-label {
        display: flex;
        align-items: center;
        font-size: 14px;
        font-weight: 500;
      }
      
      .scenario-indicator {
        width: 16px;
        height: 16px;
        margin-right: 8px;
        border-radius: 3px;
      }
      
      .scenario-arrow {
        font-size: 24px;
        margin: 0 10px;
        color: #555;
      }
      
      .scenario-bracket {
        font-size: 40px;
        font-weight: 300;
        margin: 0 5px;
        color: #555;
      }
      
  ")
  ),

  style = "height: 100vh; gap: 0rem; overflow: hidden !important; display: flex; flex-direction: column;",
  
  layout_columns(
    height = "auto",
    gap = "0px", 
    col_widths = c(2, 1, 4, 1, 4),  # Adjusted for better balance
    style = "width: 100%; background-color: #f8f9fa; border-bottom: 1px solid #e9ecef; padding: 8px 15px; display: flex; align-items: center;",
    
    div(
      style = "display: flex; align-items: center; height: 100%;",
      uiOutput("report_date")
    ),
    
    div(
      style = "display: flex; align-items: center; justify-content: flex-end; height: 100%;",
      HTML("<div style='margin-right: 10px; font-weight: 500;'><strong>Run report for:</strong></div>")
    ),
    
    div(
      style = "display: flex; align-items: center; height: 100%;",
      div(
        style = "width: 80%;",
        selectInput(
          "grp_select", 
          label = NULL, 
          choices = c("BNSSG total (Bristol Royal Infirmary, Southmead Hospital, & Weston General Hospital)", "BRI", "NBT", "WGH"),
          width = "100%",
          selectize = TRUE
        )
      )
    ),
    
    div(
      style = "display: flex; align-items: center; justify-content: flex-end; height: 100%;",
      HTML("<div style='margin-right: 10px; font-weight: 500;'><strong>Under scenario:</strong></div>")
    ),
    
    div(
      style = "display: flex; align-items: center; height: 100%;",
      div(
        style = "width: 80%;",
        selectInput(
          "scenario_select", 
          label = NULL, 
          choices = c("Medical waits for admission: < 1 hour. Medical DTA Queue < 25% of ED bed capacity" = "cf", "Medical waits for admission: 0 hour. Medical DTA Queue 0% of ED bed capacity" = "full_cf"),
          width = "100%",
          selectize = TRUE
        )
      )
    )
  ),
  
  # Top heading paragraph
  card(
    full_screen = FALSE,
    height = "6%",
    class = "no-border-no-pad",
    style = "overflow: hidden !important;",
    gap = "0px",
    uiOutput("header")
  ),

  # Scenario descriptions - centered VS version
  layout_columns(
    height = "6%",
    col_widths = c(12),
    gap = "0px",
    style = "overflow: hidden !important;",
    card(
      full_screen = FALSE,
      height = "100%",
      class = "no-border-no-pad",
      style = "overflow: hidden !important;",
      div(
        class = "scenario-header",
        uiOutput("scenario_text")
      )
    )
  ),
  # Main layout - 3 columns
  layout_columns(
    height = "80%",
    col_widths = c(4, 4, 4),
    gap = "0px",
    # style = "overflow: hidden !important;",
    class = "layout-column-no-gap",
    
    # Left column 
    layout_column_wrap(
      width = 1,
      height = "100%",
      # class = "no-border-no-pad-text-center",
      # style = "padding: 0;",
      
      # Left card 1
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
      
      # Left card 2
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
      
      # Left card 3
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
      
      # Left card 4
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
    ,
    
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
      
      # Top left card
      card(
        full_screen = FALSE,
        class = "no-border-no-pad-text-center",
        height = "48%",
        card_body(
          HTML("<h4>*Direct Impacts</h4>
               <p>When patients experience delays in hospital admission, there are direct negative consequences on their health outcomes. Our analysis has revealed a clear association between these admission delays and increased 30-day mortality rates, as well as prolonged in-hospital lengths of stay.</p>
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
          HTML("<h4>**Indirect Impacts</h4>
               <p>
               The medical Decision to Admit (DTA) queue has wider impact, affecting not only patients waiting for admission, but also creating knock-on effects for other patients arriving at the Emergency Department. Specifically, our analysis demonstrates that a larger DTA queue is associated with a measurable increase in both ambulance handover times and emergency response times, indicating a bottleneck in the patient flow.
               </p>")
        )
      )
    )
  )
)

server <- function(input, output) {
  
  shinyOptions(renderPrint.trunc = FALSE)
  
  # Create a reactive expression for the filtered data
  filtered_data <- reactive({
    impacts_output_raw %>%
      filter(grp == input$grp_select, scenario %in% c("n/a", "actual", input$scenario_select))
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
  
  # Make sure these reactive expressions are handling edge cases properly
  dta_time_avg <- reactive({
    result <- filtered_data() %>%
      filter(impact == 'mean_dta_time', report_date == max_report_date)
    
    # Check if we got any rows back
    if (nrow(result) == 0) {
      cat("dta_time_avg: No data found\n")
      return(NA)
    }
    
    return(result %>% pull(emph_text))
  })
  
  n_dta_norm <- reactive({
    result <- filtered_data() %>%
      filter(impact == 'n_dta_norm', report_date == max_report_date)
    
    # Check if we got any rows back
    if (nrow(result) == 0) {
      cat("n_dta_norm: No data found\n")
      return(NA)
    }
    
    return(result %>% pull(emph_text))
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
    filtered_data()
    impacts_output_raw %>%
      filter(report_date == max_report_date, 
             impact %in% c("om1b_raw"),
             scenario %in% c("n/a", "actual", input$scenario_select)) %>%
      pull(emph_text)
  })
  
  n_resp <- reactive({
    impacts_output_raw %>%
      filter(report_date == max_report_date, 
             impact %in% c("om1b_n_exposure")) %>%
      pull(emph_text)
  })
  
  resp_target <- reactive({
    impacts_output_raw %>%
      filter(report_date == max_report_date, 
             impact %in% c("om1b_n_exposure")) %>%
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
                          scenario %in% c("n/a", "actual", input$scenario_select),
                          str_detect(impact, paste0("^", metric))) %>%
                   select(impact, value, label, target, sub_text, value_label))
    } else {
      # For other metrics, use the user-selected group
      return(    impacts_output_raw %>%
                   filter(report_date == max_report_date,
                          grp == input$grp_select,
                          scenario %in% c("n/a", "actual", input$scenario_select),
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
  
  # Render UI outputs with proper isolation 
  output$report_date <- renderUI({
    HTML(glue::glue(
      "<span><h4><strong>Excess Impacts</strong>: A what-if analysis</h4> Based on data from <strong> {format(max_report_date, '%d %B %Y')}</strong></span>"
    ))
  })
  
  output$header <- renderUI({
    # Use req() to ensure reactives are ready
    req(n_admits(), dta_time())
    
    site_text <- switch(input$grp_select,
                        "BNSSG total (Bristol Royal Infirmary, Southmead Hospital, & Weston General Hospital)" = "our emergency department",
                        "BRI" = "the emergency department at BRI",
                        "NBT" = "the emergency department at NBT",
                        "WGH" = "the emergency department at WGH")
    
    h5(
      HTML(glue::glue(
        "<p><span class='secondary-text'>Over the last <b>7 days</b>, {site_text} saw <b>{n_admits()} medical admissions</b>. These patients spent a combined <b>{round(dta_time()/(60*24))} days</b> waiting for beds - a critical delay impacting both their care and the care for others in need of emergency help. Using regression analysis on data from the last 3 years, we have isolated the impact DTA delays and queue size has on patients and ambulance performance.<strong> From this we estimate the impacts based on 2 scenarios..</strong></span></p>"
      ))
    )
  })
  
  output$scenario_text <- renderUI({
    # Use req() to ensure reactive values are ready before rendering
    req(dta_time_avg(), n_dta_norm())
    
    dta_time <- dta_time_avg()
    dta_queue <- n_dta_norm()
    
    HTML(glue::glue('
    <div style="display: grid; grid-template-columns: 1fr auto 1fr; width: 100%; padding: 0 20px; align-items: center;">
      <div style="display: flex; align-items: center; justify-content: flex-end; padding-right: 20px;">
        <span class="scenario-indicator" style="background-color: #beaed4;"></span>
        <span style="font-weight: 500; color: #333;"><strong>Actual</strong>: where admission waits were <strong>{dta_time} hours</strong> & the DTA queue was <strong>{dta_queue}</strong> of the bed capacity, on average</span>
      </div>
      <div style="margin: 0 15px; font-size: 24px; color: #666; font-weight: bold;">VS</div>
      <div style="display: flex; align-items: center; padding-left: 20px;">
        <span class="scenario-indicator" style="background-color: #A3A78F;"></span>
        <span style="font-weight: 500; color: #333;"><strong>Hypothetical</strong>: If medical waits for admission & Decision to Admit (DTA) queue were kept <strong>under 1 hour</strong> and <strong>25%</strong> of ED bed capacity</span>
      </div>
    </div>
  '))
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
  
  output$chart1 <- renderPlot({
    # Create cache key from inputs
    cache_key <- paste("chart1", input$grp_select, input$scenario_select, sep = "_")
    
    # Simply return the pre-rendered plot - no conditional logic
    plotCache[[cache_key]]
  }, execOnResize = FALSE)  # Add this parameter
  
  output$chart2 <- renderPlot({
    cache_key <- paste("chart2", input$grp_select, input$scenario_select, sep = "_")
    plotCache[[cache_key]]
  }, execOnResize = FALSE)  # Add this parameter
  
  output$chart3 <- renderPlot({
    cache_key <- paste("chart3", input$grp_select, input$scenario_select, sep = "_")
    plotCache[[cache_key]]
  }, execOnResize = FALSE)  # Add this parameter
  
  output$chart4 <- renderPlot({
    cache_key <- paste("chart4", input$grp_select, input$scenario_select, sep = "_")
    plotCache[[cache_key]]
  }, execOnResize = FALSE)  # Add this parameter
  
  
}

shinyApp(ui = ui, server = server)