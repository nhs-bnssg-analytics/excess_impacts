library(dplyr)
library(stringr)
library(ggplot2)
library(shiny)
library(tidyr)
library(lubridate)
library(bslib)
library(ggtext)
library(RMySQL)

# Helper function to replace %||% operator
`%||%` <- function(x, y) if (is.null(x)) y else x

format_week_interval <- function(end_date) {
  # Ensure the input is a Date object
  end_date <- as.Date(end_date)
  start_date <- end_date - 6
  
  if (format(start_date, "%m") == format(end_date, "%m")) {
    return(paste0(format(start_date, "%d"), "-", format(end_date, "%d"), " ", format(start_date, "%B")))
  } else {
    return(paste0(format(start_date, "%d"), " ", format(start_date, "%B"), " - ", format(end_date, "%d"), " ", format(end_date, "%B")))
  }
}

app_colours <- c(
  actual = "#88a0a8",     # actual
  hypothetical = "#B4CEB3" # hypothetical
)

gen_html_text <- function(emph_text){
  str_c(
        # "<span style='font-weight:bold;font-size:30pt;'>",
        "<span class='primary-text'>",
        emph_text,
        # "</span><br><span style='font-size:15pt'>",
        # sub_text,
        "</span>")
}


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


impacts_output_raw  <- DBI::dbGetQuery(conn, "select * from excess_impacts") %>%
  mutate(report_date = ymd(report_date)) %>%
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


# Store max/max report date as a static value
max_report_date <- max(impacts_output_raw$report_date)
min_report_date <- min(impacts_output_raw$report_date)

static_report_date_line <- HTML(glue::glue(
  # "<span><h5><strong>Excess Impacts:</strong> a what-if analysis based on the week <strong>{format(max_report_date-ddays(7), '%d')}-{format(max_report_date, '%d %B %Y')}</strong></span></h5>"
  "<span style='line-height: 1'>
  <h5><strong>Excess Impacts:</strong> a what-if analysis</h5>
  <p style='font-size: 0.8em; font-weight: lighter;'>open sidebar for more information & options</p>
</span>"
))


ui <- page_sidebar(
  
  tags$head(
tags$script(HTML("
      $(document).ready(function() {
        // Add text to the sidebar toggle button after a short delay to ensure DOM is ready
        setTimeout(function() {
          var toggleBtn = $('.bslib-sidebar-layout-toggle');
          
          // Only add the label if it doesn't exist yet
          if (toggleBtn.find('.sidebar-toggle-label').length === 0) {
            toggleBtn.append('<span class=\"sidebar-toggle-label\">More information & options</span>');
          }
        }, 100);
      });
    ")),
# Script to update CSS variables based on viewport width
tags$script("
      // Wait for Shiny to be fully initialized
      $(document).on('shiny:connected', function(event) {
        function updateFontSizes() {
          // Get viewport width
          var width = window.innerWidth;
          var height = window.innerHeight;
          
          // Read slider values directly from DOM
          var primaryMultiplier = parseFloat($('#primaryTextSize').val() || 1);
          var secondaryMultiplier = parseFloat($('#secondaryTextSize').val() || 0.7);
          
          // Calculate sizes
          var baseSize = width / 50;
          var primarySize = baseSize * primaryMultiplier;
          var secondarySize = baseSize * secondaryMultiplier;
          
          // Apply limits
          primarySize = Math.min(Math.max(primarySize, 12), 48);
          secondarySize = Math.min(Math.max(secondarySize, 10), 36);
          
          // Update CSS variables
          document.documentElement.style.setProperty('--primary-font-size', primarySize + 'px');
          document.documentElement.style.setProperty('--secondary-font-size', secondarySize + 'px');
          
          // Send values to Shiny using the correct method
          Shiny.setInputValue('viewport_width', width);
          Shiny.setInputValue('viewport_height', height);
          Shiny.setInputValue('calculated_primary_size', primarySize);
          Shiny.setInputValue('calculated_secondary_size', secondarySize);
          
          console.log('Updated font sizes. Viewport: ' + width + 'x' + height + 
                      ' Primary: ' + primarySize + 'px, Secondary: ' + secondarySize + 'px');
        }
        
        // Initial update
        updateFontSizes();
        
        // Update on window resize with debounce
        var resizeTimer;
        $(window).on('resize', function() {
          clearTimeout(resizeTimer);
          resizeTimer = setTimeout(updateFontSizes, 100);
        });
        
        // Update when sliders change
        $(document).on('input change', '#primaryTextSize, #secondaryTextSize', function() {
          updateFontSizes();
        });
        
        // Execute again after a short delay to ensure everything is loaded
        setTimeout(updateFontSizes, 500);
      });
    "),
  # Additional JavaScript to ensure inputs are sent
  tags$script("
    // Additional method to ensure viewport size is sent
    Shiny.addCustomMessageHandler('requestViewportSize', function(message) {
      var width = window.innerWidth;
      var height = window.innerHeight;
      Shiny.setInputValue('viewport_width_requested', width);
      Shiny.setInputValue('viewport_height_requested', height);
      console.log('Viewport size requested and sent: ' + width + 'x' + height);
    });
  "),

tags$style("
      :root {
        --primary-font-size: 40px;
        --secondary-font-size: 18px;
      }
      
      .primary-text {
        font-size: var(--primary-font-size);
        margin-bottom: 1rem;
      }
      
      .secondary-text {
        font-size: var(--secondary-font-size);
        margin-bottom: 1rem;
      }
    ")

  ),

style = "height: 100vh; max-height: 100vh; overflow: hidden;",

sidebar = sidebar(
  id = "sidebar",
  bg = "#f8f9fa",
  width = "500px",
  open = FALSE,
  padding = "15px",
  style = "border-right: 1px solid #e9ecef;",
  
  div(
    id = "report_date",
    HTML(glue::glue(
      # "<span><h4><strong>Excess Impacts</strong> </br> A what-if analysis</h4> Based on the week </br> <strong> {format(max_report_date-ddays(7), '%d')}-{format(max_report_date, '%d %B %Y')}</strong></span>"
      "<span><h4><strong>Excess Impacts</strong> </br> A what-if analysis</h4></span>"
    ))
  ),
  HTML("<span><p>For comments or questions, email Nick Howlett (Modelling and Analytics, ICB): <a href='mailto:nick.howlett5@nhs.net'>nick.howlett5@nhs.net</a></h5></p><span>"),
  HTML("<strong>Run report for:</strong>"),
  div(
    style = "margin-top: 20px;",
    selectInput(
      "grp_select", 
      label = NULL, 
      choices = c("BNSSG total (Bristol Royal Infirmary, Southmead Hospital, & Weston General Hospital)", "BRI", "NBT", "WGH"),
      width = "100%",
      selectize = TRUE
    )
  ),
  HTML("<strong>Select scenario:</strong>"),
  
    
    # Static table structure with no renderUI
    div(
      class = "scenario-selection",
      
      # Hidden radio input that stores our selection
      radioButtons(
        "scenario_select",
        label = NULL,
        choices = c("cf", "full_cf"),
        selected = "cf"  # Default selection
      ),
      
      # Table that shows our options
      tags$table(
        id = "scenario-table",
        class = "scenario-table",
        tags$thead(
          tags$tr(
            tags$th("Select"),
            tags$th("Medical waits for admission"),
            tags$th("Medical DTA Queue")
          )
        ),
        tags$tbody(
          # First row - Option 1
          tags$tr(
            id = "row-cf",
            class = "scenario-row selected", # Default selected
            onclick = "document.getElementById('row-cf').querySelector('input[type=\"radio\"]').checked = true; Shiny.setInputValue('scenario_select', 'cf');",
            tags$td(
              class = "radio-cell",
              div(
                class = "radio-container",
                tags$input(
                  type = "radio",
                  name = "scenario_visual", 
                  checked = "checked" # Default checked
                )
              )
            ),
            tags$td("< 1 hour"),
            tags$td("< 25% of ED bed capacity")
          ),
          
          # Second row - Option 2
          tags$tr(
            id = "row-full_cf",
            class = "scenario-row",
            onclick = "document.getElementById('row-full_cf').querySelector('input[type=\"radio\"]').checked = true; Shiny.setInputValue('scenario_select', 'full_cf');",
            tags$td(
              class = "radio-cell",
              div(
                class = "radio-container",
                tags$input(
                  type = "radio",
                  name = "scenario_visual"
                )
              )
            ),
            tags$td("0 hour"),
            tags$td("0% of ED bed capacity")
          )
        )
      ),
      
      # JavaScript to handle highlighting
      tags$script(HTML("
        $(document).ready(function() {
          // Function to update rows based on selection
          function updateSelectedRow(value) {
            // Remove selected class from all rows
            $('.scenario-row').removeClass('selected');
            // Add selected class to the chosen row
            $('#row-' + value).addClass('selected');
            
            // Update radio buttons
            $('input[name=\"scenario_visual\"]').prop('checked', false);
            $('#row-' + value + ' input[name=\"scenario_visual\"]').prop('checked', true);
          }
          
          // Listen for changes to the hidden radio input
          $('#scenario_select').on('change', function() {
            updateSelectedRow($(this).val());
          });
        });
      ")),
    ),
  

HTML("
<p><strong>More information</strong></p>
     <p>In this analysis we examine the impact of <strong>delays in medical admissions</strong>, where there are two types of impact:</p>

<ul>
    <li><strong>Direct Impacts</strong>
        <p>When patients experience <strong>delays in hospital admission</strong>, there are direct negative consequences on their health outcomes. Our analysis has revealed a <strong>clear association</strong> between these admission delays and <strong>increased 30-day mortality rates</strong>, as well as <strong>prolonged in-hospital lengths of stay</strong>.</p>
    </li>
    <li><strong>Indirect Impacts</strong>
        <p>The <strong>medical Decision to Admit (DTA) queue</strong> has wider impact, affecting not only patients waiting for admission, but also creating knock-on effects for other patients arriving at the Emergency Department. Specifically, our analysis demonstrates that a <strong>larger DTA queue is associated with a measurable increase in both ambulance handover times and emergency response times</strong>.</p>
    </li>
</ul>

<p><strong>Regression analysis</strong> is a powerful statistical tool that allows us to examine the relationship between variables. A key strength of regression lies in its ability to <strong>control for confounding variables</strong>. Here, we have controlled for patient demographics (<strong>age, sex, deprivation</strong>), and <strong>comorbidities</strong>, as well as time-based and ED factors (<strong>time of arrival, number of previous arrivals in 60 days</strong>).</p>

<p>While regression analysis is excellent at identifying and quantifying <strong>associations</strong>, it's crucial to remember that <strong>association does not automatically imply causation</strong>. Even after controlling for numerous potential confounders, a statistically significant association between variables <strong>does not definitively prove that one variable causes the other</strong>.
There could still be:</p>
<ul>
    <li><strong>Unmeasured confounders</strong>: Variables we didn't know about or couldn't measure might still be influencing the relationship.</li>
    <li><strong>Reverse causality</strong>: The dependent variable might actually be influencing the independent variable.</li>
    <li><strong>Spurious relationships</strong>: The observed association might be due to chance or some other unmodeled factor.</li>
</ul>"),

# HTML("<p><h4><strong>More information</strong></h4></p>
# <p><strong>Regression analysis</strong> is a powerful statistical tool that allows us to examine the relationship between variables. A key strength of regression lies in its ability to <strong>control for confounding variables</strong>. Here, we have controlled for patient demographics (<strong>age, sex, deprivation</strong>), and <strong>comorbidities</strong>, as well as time-based and ED factors (<strong>time of arrival, number of previous arrivals in 60 days</strong>).</p>
# 
# <p>While regression analysis is excellent at identifying and quantifying <strong>associations</strong>, it's crucial to remember that <strong>association does not automatically imply causation</strong>. Even after controlling for numerous potential confounders, a statistically significant association between variables <strong>does not definitively prove that one variable causes the other</strong>.
# There could still be:</p>
# <ul>
#     <li><strong>Unmeasured confounders</strong>: Variables we didn't know about or couldn't measure might still be influencing the relationship.</li>
#     <li><strong>Reverse causality</strong>: The dependent variable might actually be influencing the independent variable.</li>
#     <li><strong>Spurious relationships</strong>: The observed association might be due to chance or some other unmodeled factor.</li>
# </ul>"),
#         




# HTML("<p>In this analysis we examine the impact of <strong>medical boarding</strong>, where there are two types of impact:</p>
# 
# <ul>
#     <li><strong>Direct Impacts</strong>
#         <p>When patients experience <strong>delays in hospital admission</strong>, there are direct negative consequences on their health outcomes. Our analysis has revealed a <strong>clear association</strong> between these admission delays and <strong>increased 30-day mortality rates</strong>, as well as <strong>prolonged in-hospital lengths of stay</strong>.</p>
#     </li>
#     <li><strong>Indirect Impacts</strong>
#         <p>The <strong>medical Decision to Admit (DTA) queue</strong> has wider impact, affecting not only patients waiting for admission, but also creating knock-on effects for other patients arriving at the Emergency Department. Specifically, our analysis demonstrates that a <strong>larger DTA queue is associated with a measurable increase in both ambulance handover times and emergency response times</strong>.</p>
#     </li>
# </ul>"
#         )

),
div(
  class = "main-content-container",
  verbatimTextOutput("debugText"),
  # Only show main heading when sidebar is closed
  conditionalPanel(
    condition = "input.sidebar !== true",
    div(
      id = "main_heading",
      class = "main-heading",
      uiOutput("report_date_line")
    )
  ),
  
  # Top heading paragraph
  card(
    full_screen = FALSE,
    height = "100px",
    class = "no-border-no-pad header-card",
    style = "overflow: visible !important; margin-bottom: 20px; flex-shrink: 0;",
    gap = "0px",
    uiOutput("header")
  ),
  
  # Scenario descriptions - centered VS version
  card(
    full_screen = FALSE,
    height = "100px",
    class = "no-border-no-pad",
    style = "overflow: visible !important; margin-bottom: 20px; flex-shrink: 0;",
    div(
      class = "scenario-header",
      uiOutput("scenario_text")
    )
  ),
  # Main layout - 3 columns
  layout_columns(
    height = "calc(100vh - 280px)",
    col_widths = c(5, 4, 3),
    gap = "0px",
    class = "layout-column-no-gap flex-fill", 
    style = "overflow: hidden !important;",
    
    # Left column 
    layout_column_wrap(
      width = 1,
      height = "100%",
      
      # Left card 1
      card(
        class = "no-border-no-pad",
        height = "20%",
        # style = "display: flex; align-items: center; justify-content: flex-start; padding: 0;",
        card_body(
          style = "justify-content: center;",
          uiOutput("mort_subtext")
        )
      ),
      
      # Left card 2
      card(
        class = "no-border-no-pad",
        height = "20%",
        # style = "display: flex; align-items: center; justify-content: flex-start; padding: 0;",
        card_body(
          style = "justify-content: center;",
          uiOutput("los_subtext")
        )
      ),
      
      # Left card 3
      card(
        class = "no-border-no-pad",
        height = "20%",
        # style = "display: flex; align-items: center; justify-content: flex-start; padding: 0;",
        card_body(
          style = "justify-content: center;",
          uiOutput("resp_subtext")
        )
      ),
      
      # Left card 4
      card(
        class = "no-border-no-pad",
        height = "20%",
        # style = "display: flex; align-items: center; justify-content: flex-start; padding: 0;",
        card_body(
          style = "justify-content: center;",
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
              height = "20%",
              card_body(plotOutput("chart1", height = "100%"))
            ),
            
            # Chart 2
            card(
              class = "no-border-no-pad",
              # style = "padding: 0px !important;border-top: none !important;border-left: none !important;border-right: none !important; border-bottom: 3px solid #dee2e6 !important;box-shadow: none !important;--bs-card-spacer-y: 0 !important;",
              height = "20%",
              card_body(plotOutput("chart2", height = "100%"))
            ),
            
            # Chart 3
            card(
              class = "no-border-no-pad",
              # style = "padding: 0px !important;border-top: none !important;border-left: none !important;border-right: none !important; border-bottom: 3px solid #dee2e6 !important;box-shadow: none !important;--bs-card-spacer-y: 0 !important;",
              height = "20%",
              card_body(plotOutput("chart3", height = "100%"))
            ),
            
            # Chart 4
            card(
              class = "no-border-no-pad",
              # style = "padding: 0px !important;border-top: none !important;border-left: none !important;border-right: none !important; border-bottom: 3px solid #dee2e6 !important;box-shadow: none !important;--bs-card-spacer-y: 0 !important;",
              height = "20%",
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
      class = "no-border-no-pad-text-center"
      # style = "padding: 0;",
      
    )
  )
)
)

server <- function(input, output, session) {
  
  viewportDims <- reactiveValues(
    width = 1200,
    height = 800
  )
  
  # Request viewport size when session starts
  observe({
    session$sendCustomMessage("requestViewportSize", list())
  })
  
  # Update from JavaScript inputs when available
  observe({
    # Try first method
    if (!is.null(input$viewport_width)) {
      viewportDims$width <- input$viewport_width
      viewportDims$height <- input$viewport_height
    }
    
    # Try backup method
    if (!is.null(input$viewport_width_requested)) {
      viewportDims$width <- input$viewport_width_requested
      viewportDims$height <- input$viewport_height_requested
    }
  })
  
  # Calculate font sizes on the server side as a backup
  fontSizes <- reactive({
    # Base size calculation using viewport width
    baseSize <- viewportDims$width / 50
    
    # Apply user's multipliers
    primarySize <- baseSize * input$primaryTextSize
    secondarySize <- baseSize * input$secondaryTextSize
    
    # Ensure reasonable bounds
    primarySize <- min(max(primarySize, 12), 48)
    secondarySize <- min(max(secondarySize, 10), 36)
    
    list(
      primary = primarySize,
      secondary = secondarySize
    )
  })
  
  # Optional debug information
  output$debugText <- renderPrint({
    # Primary values from JavaScript
    cat("Viewport dimensions (JS): ", input$viewport_width %||% "N/A", " x ", 
        input$viewport_height %||% "N/A", "\n")
    
    # Backup values from secondary method
    cat("Viewport dimensions (requested): ", input$viewport_width_requested %||% "N/A", " x ", 
        input$viewport_height_requested %||% "N/A", "\n")
    
    # Values used in calculations
    cat("Viewport dimensions (used): ", viewportDims$width, " x ", viewportDims$height, "\n\n")
    
    # Font sizes
    cat("Calculated primary size (JS): ", input$calculated_primary_size %||% "N/A", "px\n")
    cat("Calculated secondary size (JS): ", input$calculated_secondary_size %||% "N/A", "px\n")
    
    # Backup font sizes calculated on server
    cat("Calculated primary size (server): ", round(fontSizes()$primary, 1), "px\n")
    cat("Calculated secondary size (server): ", round(fontSizes()$secondary, 1), "px\n\n")

  })
  
  # Create a reactive value to store the selected scenario
  # This ensures we have a valid value from the start
  selected <- reactiveVal("cf")
  
  # Update reactive value when input changes
  observe({
    if (!is.null(input$scenario_select)) {
      selected(input$scenario_select)
    }
  })
  
  # Create the selection table
  output$scenario_table <- renderUI({
    # Get current selection with fallback to default
    current_selection <- selected()
    
    tags$div(
      class = "scenario-selection",
      
      # Hidden radio input that stores our selection
      radioButtons(
        "scenario_select",
        label = NULL,
        choices = c("cf", "full_cf"),
        selected = current_selection
      ),
      
      # Table that shows our options
      tags$table(
        class = "scenario-table",
        tags$thead(
          tags$tr(
            tags$th("Select"),
            tags$th("Medical waits for admission"),
            tags$th("Medical DTA Queue")
          )
        ),
        tags$tbody(
          # First row - Option 1
          tags$tr(
            id = "row-cf",
            class = paste("scenario-row", if(current_selection == "cf") "selected" else ""),
            onclick = "Shiny.setInputValue('scenario_select', 'cf');",
            tags$td(
              class = "radio-cell",
              div(
                class = "radio-container",
                tags$input(
                  type = "radio",
                  name = "scenario_visual", 
                  checked = if(current_selection == "cf") "checked" else NULL
                )
              )
            ),
            tags$td("< 1 hour"),
            tags$td("< 25% of ED bed capacity")
          ),
          
          # Second row - Option 2
          tags$tr(
            id = "row-full_cf",
            class = paste("scenario-row", if(current_selection == "full_cf") "selected" else ""),
            onclick = "Shiny.setInputValue('scenario_select', 'full_cf');",
            tags$td(
              class = "radio-cell",
              div(
                class = "radio-container",
                tags$input(
                  type = "radio",
                  name = "scenario_visual",
                  checked = if(current_selection == "full_cf") "checked" else NULL
                )
              )
            ),
            tags$td("0 hour"),
            tags$td("0% of ED bed capacity")
          )
        )
      )
    )
  })
  
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

  excess_mort_pre <- reactive({
    filtered_data() %>%
      filter(report_date == min_report_date, impact %in% c("om6_impact", "om6_impact_cf")) %>%
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

  excess_los_pre <- reactive({
    filtered_data() %>%
      filter(report_date == min_report_date, impact %in% c("om4_impact", "om4_impact_cf")) %>%
      select(impact, value) %>%
      pivot_wider(names_from = impact) %>%
      mutate(diff = round(om4_impact) - round(om4_impact_cf)) %>%
      pull(diff) %>%
      `*`(7)
  })
  
  excess_resp <- reactive({
    filtered_data() # hack to make this reactive expression appear to change when location input changes
    impacts_output_raw %>%
      filter(report_date == max_report_date, 
             impact %in% c("om1b_raw"),
             scenario %in% c("n/a", "actual", input$scenario_select)) %>%
      pull(emph_text)
  })

  excess_resp_pre <- reactive({
    filtered_data() # hack to make this reactive expression appear to change when location input changes
    impacts_output_raw %>%
      filter(report_date == min_report_date, 
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
  
  excess_handover_pre <- reactive({
    filtered_data() %>%
      filter(report_date == min_report_date, impact %in% c("om2_raw")) %>%
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
  
  
  # Create a function to generate plots with consistent formatting
  create_plot <- function(data, font_size= 20) {
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
      ggfx::with_outer_glow(geom_textbox(aes(label = value_label), size = font_size/3, fill = NA, box.color = NA, halign = 0.5, vjust = -0.2, width = unit(0.4, "npc")),
                            colour = "white", x_offset = 0, y_offset = 0, expand = 12.5, sigma = 0) +
      theme_void() +
      scale_y_continuous(expand = expansion(mult = c(0, 0.5))) +
      scale_fill_manual(values = unname(app_colours)) +
      theme(legend.position = "off",
            plot.margin = margin(0, 0, 0, 0, "pt"),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.line.x = element_line(linewidth = 2.5, colour = "grey30"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_textbox(size = font_size/2, hjust = 0.5),
            plot.caption = element_textbox(size = font_size/2, hjust = 0.5, margin = margin(t = 10), face = "bold")
      ) +
      labs(caption = data$sub_text[1], y = "", x = "")
  }
  
  
  # Use the pre-rendered UI in your output
  output$report_date <- renderUI({
    static_report_date
  })
  
  output$report_date_line <- renderUI({
    static_report_date_line
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
      style = "line-height: 1.5; margin: 0;",  # Increased line-height from 1.4 to 1.5
      HTML(glue::glue(
        "<p class='app-standard-text' style='margin: 5px 0; max-height: none;'><span><strong>Between {format_week_interval(max_report_date)}</strong>, {site_text} saw <b>{n_admits()} medical admissions</b>. These patients spent a combined <b>{round(dta_time()/(60*24))} days</b> waiting for beds - a critical delay impacting both their care and the care for others in need of emergency help. Using regression analysis on data from the last 3 years, we have isolated the impact DTA delays and queue size has on patients and ambulance performance.<strong> From this we estimate the impacts based on 2 scenarios..</strong></span></p>"
      ))
    )
  })
  
  output$scenario_text <- renderUI({
    # Use req() to ensure reactive values are ready before rendering
    req(dta_time_avg(), n_dta_norm())
    
    dta_time <- dta_time_avg()
    dta_queue <- n_dta_norm()
    
    HTML(glue::glue('
  <div style="display: flex; align-items: flex-start; width: 100%;">
    <div style="width: 15%; flex: none;"></div>
    <div style="width: 41.67%; display: flex; align-items: flex-start; justify-content: flex-end; padding-right: 10px;">
      <span class="scenario-indicator" style="background-color: {app_colours["actual"]}; margin-right: 5px; margin-top: 4px;"></span>
      <span style="font-weight: 500; color: #333;"><strong>Actual</strong>: where admission waits were <strong>{dta_time} hours</strong> & the Decision to Admit (DTA) queue was <strong>{dta_queue}</strong> of the bed capacity, on average</span>
    </div>
    <div style="width: 3.33%; text-align: center; font-size: 24px; color: #666; font-weight: bold;">VS</div>
    <div style="width: 40%; display: flex; align-items: flex-start; padding-left: 10px;">
      <span class="scenario-indicator" style="background-color: {app_colours["hypothetical"]}; margin-right: 5px; margin-top: 4px;"></span>
      <span style="font-weight: 500; color: #333;"><strong>Hypothetical</strong>: If medical waits for admission & DTA queue were kept <strong>under 1 hour</strong> and <strong>25%</strong> of ED bed capacity</span>
    </div>
  </div>
  '))
  })
  
  
  output$mort_subtext <- renderUI({
    req(excess_mort(), excess_mort_pre(), n_admits())
    
    pre_text <- case_when(
      excess_mort() > excess_mort_pre() ~ glue::glue("(<strong>up</strong> from <strong>{excess_mort_pre()}</strong> last week)"),
      excess_mort() < excess_mort_pre() ~ glue::glue("(<strong>down</strong> from <strong>{excess_mort_pre()}</strong> last week)"),
      excess_mort() == excess_mort_pre() ~ glue::glue("(<strong>unchanged</strong> from last week)"),
      .default = ""
      )
    
    HTML(
      glue::glue("<p class='app-standard-text style='margin: 0;'><strong>Due to delays for their admission</strong>, we estimate there will be <strong>{excess_mort()} avoidable deaths</strong> in the cohort of emergency Medical admissions from the <strong>last 7 days</strong> {pre_text}.</p>"
      )
    )
  })
  
  output$los_subtext <- renderUI({
    req(n_admits(), excess_los(), excess_los_pre())
    
    pre_text <- case_when(
      excess_los() > excess_los_pre() ~ glue::glue("(<strong>up</strong> from <strong>{excess_los_pre()/7}</strong> last week)"),
      excess_los() < excess_los_pre() ~ glue::glue("(<strong>down</strong> from <strong>{excess_los_pre()/7}</strong> last week)"),
      excess_los() == excess_los_pre() ~ glue::glue("(<strong>unchanged</strong> from last week)"),
      .default = ""
    )
    
    HTML(
      glue::glue("<p class='app-standard-text style='margin: 0;'>The <strong>{n_admits()}</strong> Medical admits from the <strong>last 7 days</strong> will have an estimated <strong>excess {excess_los()} days of length of stay</strong> combined, <strong>due to their delays for admission</strong>. This is equivalent to <strong>{excess_los()/7} excess Medical beds occupied</strong> per day {pre_text}.</p>"
      )
    )
  })
  
  output$resp_subtext <- renderUI({
    req(n_resp(), excess_resp(), excess_resp_pre(), resp_target())
    
    pre_text <- case_when(
      excess_resp() > excess_resp_pre() ~ glue::glue("(<strong>up</strong> from <strong>{excess_resp_pre()}</strong> last week)"),
      excess_resp() < excess_resp_pre() ~ glue::glue("(<strong>down</strong> from <strong>{excess_resp_pre()}</strong> last week)"),
      excess_resp() == excess_resp_pre() ~ glue::glue("(<strong>unchanged</strong> from last week)"),
      .default = ""
    )
    
    HTML(
      glue::glue("<p class='app-standard-text style='margin: 0;'>The <strong>{n_resp()}</strong> Catergory 2 ambulance responses over the <strong>last 7 days</strong> had an <strong>excess {excess_resp()} hours response time</strong> combined, <strong>due to the medical DTA queue</strong> {pre_text}. The impact of this excess on meeting the <strong>national target (dashed-line)</strong> that <strong>90% of responses are within {resp_target()} minutes</strong> can be seen in the chart."
      )
    )
  })
  
  output$handover_subtext <- renderUI({
    req(n_handover(), excess_handover(), excess_handover_pre(), handover_target(), n_amb())
    
    pre_text <- case_when(
      excess_handover() > excess_handover_pre() ~ glue::glue("(<strong>up</strong> from <strong>{excess_handover_pre()}</strong> last week)"),
      excess_handover() < excess_handover_pre() ~ glue::glue("(<strong>down</strong> from <strong>{excess_handover_pre()}</strong> last week)"),
      excess_handover() == excess_handover_pre() ~ glue::glue("(<strong>unchanged</strong> from last week)"),
      .default = ""
    )
    
    HTML(
      glue::glue("<p class='app-standard-text style='margin: 0;'>The <strong>{n_handover()}</strong> ambulance handovers over the <strong>last 7 days</strong> had an <strong>excess {excess_handover()} hours handover time</strong> combined {pre_text}, <strong>due to the medical DTA queue</strong>. This excess delay is <strong>equivalent to {n_amb()} ambulances being out of service througout the last 7 days</strong>. The impact of this excess on meeting the <strong>national target (dashed-line)</strong> that <strong>90% of handovers are within {handover_target()} minutes</strong> can be seen in the chart."
      )
    )
  })
  
  # Plot outputs - render directly from the chart data
  output$chart1 <- renderPlot({
    req(input$calculated_primary_size)
    create_plot(mortality_chart_data(), font_size = input$calculated_primary_size)
  }) %>% bindCache(input$scenario_select, input$grp_select)

  output$chart2 <- renderPlot({
    create_plot(los_chart_data())
  }) %>% bindCache(input$scenario_select, input$grp_select)
  
  output$chart3 <- renderPlot({
    create_plot(resp_chart_data())
  }) %>% bindCache(input$scenario_select, input$grp_select)
  
  output$chart4 <- renderPlot({
    create_plot(handover_chart_data())
  }) %>% bindCache(input$scenario_select, input$grp_select)
  
  outputOptions(output, "scenario_table", suspendWhenHidden = FALSE)
  
}

shinyApp(ui = ui, server = server)