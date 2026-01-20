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
    # "<span style='font-weight:bold;font-size: 0.8em;'>",
    "<span class='app-standard-text'>",
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

#print(paste("Working directory at server start:", getwd()))

# setwd("/srv/shiny-server/")

impacts_output_raw  <-DBI::dbGetQuery(conn, "select * from excess_impacts") %>%
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
  <p><h5><strong>Excess Impacts:</strong> a what-if analysis</h5></p>
  <p style='font-size: 0.8em; font-weight: lighter;'>open sidebar for more information & options</p>
</span>"
))


ui <- page_sidebar(
  
  tags$head(
    tags$style("
  /* Main page and container settings */
html, body, .bslib-page-fill {
  height: 100vh !important;
  max-height: 100vh !important;
  /*overflow: hidden !important;/
  margin: 0 !important;
  padding: 0 !important;
}

.bslib-page-fill {
  background-color: #ffffff;
}

.main-content-container {
  padding: 5px 20px !important;
  /* height: 100vh !important;  */
  display: flex !important;
  flex-direction: column !important;
  overflow: hidden !important;
}

/* Flex utilities */
.flex-fill {
  flex: 1 1 auto !important;
  min-height: 0 !important;
  overflow: hidden !important;
}

/* Card styling */
.card {
  border-radius: 0 !important;
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
  display: flex !important;
  flex-direction: column !important;
  justify-content: center !important; /* Vertical centering */
  height: 100%;
  --bs-card-spacer-y: 0 !important;
  --bs-card-spacer-x: 0 !important;
}

.no-border-no-pad-text-center .card-body {
  display: flex !important;
  align-items: center !important; /* Vertical centering */
  justify-content: flex-start !important;
  height: 100% !important;
  padding: 10px !important;
}

.no-border-no-pad-text-center p {
  margin: 0 !important;
}

.header-card {
  margin-bottom: 5px !important;
  min-height: 50px !important;
}

/* Layout utilities */
.layout-column-no-gap {
  --bs-layout-columns-gap: 0 !important;
  --bs-layout-rows-gap: 0 !important;
}

.bslib-column-wrap {
  overflow: hidden !important;
}

.bslib-layout-column {
  overflow: hidden !important;
}

/* Sidebar styling */
.bslib-sidebar-layout {
  height: 100vh !important;
  max-height: 100vh !important;
  overflow: hidden !important;
}

.bslib-sidebar-layout > .bslib-sidebar-layout-panel {
  height: 100vh !important;
  max-height: 100vh !important;
  overflow: auto !important;
}

/* Main layout container */
#main_layout {
  flex: 1 !important;
  display: flex !important;
  flex-direction: column !important;
  overflow: hidden !important;
  min-height: 0 !important;
}

/* Chart container */
.chart-container {
  flex: 1 !important;
  overflow: hidden !important;
  min-height: 0 !important;
  display: flex !important;
  flex-direction: column !important;
}

/* Text styling */
.primary-text {
  font-size: 25pt;
  margin-bottom: 1rem;
}

.secondary-text {
  font-size: 14px !important;
  margin-bottom: 0.5rem !important;
  line-height: 1.4 !important;
}

/* Form styling */
.form-group {
  margin-bottom: 0 !important;
  margin-top: 0 !important;
  padding-bottom: 0 !important;
  padding-top: 0 !important;
}

.selectize-control {
  margin-bottom: 0 !important;
  margin-top: 0 !important;
}

/* Tooltip customization */
.tooltip-inner {
  background-color: white !important;
  color: black !important;
  max-width: none !important;
  padding: 10px !important;
  box-shadow: 0 2px 4px rgba(0,0,0,0.1);
}

/* Divider styling */
.column-divider {
  border-right: 3px solid #dee2e6;
  height: 100%;
  padding-right: 1rem;
}

.vertical-line {
  position: absolute;
  left: 49.5%;
  top: 0;
  bottom: 0;
  width: 10px;
  border-left: 4px solid darkgray;
  z-index: 1000;
}

/* Updated scenario grid styling */
  .scenario-grid {
    display: grid;
    grid-template-columns: 5fr 4fr 3fr;
    width: 100%;
    min-height: 60px;
    padding: 0 10px;
    align-items: center;
    white-space: normal;
    overflow: visible !important;
  }
  
  .scenario-vs {
    text-align: center;
    font-size: 24px;
    color: #666;
    font-weight: bold;
    margin: 0;
  }

.scenario-label {
  display: flex;
  align-items: center;
  font-size: 14px;
  font-weight: 500;
}

.scenario-indicator {
  width: 20px;
  height: 20px;
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

/* Table styling */
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

/* Custom styles for sidebar toggle */
      .sidebar-toggle-label {
        margin-left: 8px;
        font-size: 14px;
        font-weight: 500;
        display: inline-block;
        vertical-align: middle;
      }
      
      /* Hide label when sidebar is open */
      .bslib-sidebar-layout.sidebar-open .sidebar-toggle-label {
        display: none;
      }

/* Radio-table wrapper */
.radio-table-wrapper {
  position: relative;
  margin-bottom: 20px;
}

.radio-buttons-column {
  position: absolute;
  top: 45px;
  left: 5px;
  z-index: 10;
  width: 60px;
}

.radio-buttons-column .radio {
  margin: 0 !important;
  display: flex;
  align-items: center;
  justify-content: center;
  height: 46px !important;
  margin-top: 0 !important;
  margin-bottom: 0 !important;
}

.radio-buttons-column .radio label {
  margin: 0;
  padding: 0;
  height: 100%;
  width: 100%;
  display: flex;
  align-items: center;
  justify-content: center;
}

.radio-buttons-column .radio span {
  display: none;
}

.table-container {
  margin-left: 0;
}

.content-table {
  width: 100%;
  border-collapse: collapse;
  border: 1px solid #ddd;
}

.content-table th, .content-table td {
  padding: 12px 15px !important;
  text-align: left;
  border-bottom: 1px solid #ddd;
  height: 22px !important;
}

.content-table th {
  background-color: #f5f5f5;
  font-weight: bold;
}

.col-select {
  width: 60px;
}

.content-row:hover {
  background-color: #f9f9f9;
}

.content-row.active {
  background-color: #e8f0fe;
}

.shiny-options-group {
  position: relative;
  top: 54px !important;
  margin-top: 0 !important;
}

.radio-buttons-column .radio:first-child {
  margin-bottom: 15px !important;
}

.radio-buttons-column .radio:last-child {
  margin-top: 10px !important;
}
/* Add these if not already covered */
.bslib-gap-spacing {
  padding-top: 0 !important;
  margin-top: 0 !important;
}

.html-fill-container {
  padding-top: 0 !important;
  margin-top: 0 !important;
}
.app-standard-text {
  font-size: 0.95vw !important; /* Choose your preferred size */
  line-height: 1.5 !important;
  margin: 5px 0 !important;
  font-weight: 400 !important;
}
  "),
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
          var baseSize = width/10;
          var primarySize = baseSize * primaryMultiplier;
          var secondarySize = baseSize * secondaryMultiplier;
          
         // Apply limits - tighter range for less dramatic scaling
          primarySize = Math.min(Math.max(primarySize, 16), 28);
          secondarySize = Math.min(Math.max(secondarySize, 12), 22);
          
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
    ")

  ),

style = "height: 100vh; max-height: 100vh;",

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
      "<span><h5><strong>Excess Impacts</strong> </br> A what-if analysis</h5></span>"
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
      
      # CSS styles for our table
      tags$style(HTML("
        /* Hide the actual radio buttons from Shiny */
        #scenario_select { 
          display: none; 
        }
        
        /* Table styling */
        .scenario-table {
          width: 100%;
          border-collapse: collapse;
          margin: 15px 0;
          box-shadow: 0 1px 3px rgba(0,0,0,0.1);
        }
        
        .scenario-table th, .scenario-table td {
          padding: 12px 15px;
          text-align: left;
          border: 1px solid #ddd;
        }
        
        .scenario-table th {
          background-color: #f5f5f5;
          font-weight: bold;
        }
        
        /* Row styling */
        .scenario-row {
          cursor: pointer;
          transition: background-color 0.2s;
        }
        
        .scenario-row:hover {
          background-color: #f0f0f0;
        }
        
        .scenario-row.selected {
          background-color: #e0e0e0;
        }
        
        /* Radio cell styling */
        .radio-cell {
          width: 60px;
          text-align: center;
          vertical-align: middle;
        }
        
        /* Radio container for better centering */
        .radio-container {
          display: flex;
          justify-content: center;
          align-items: center;
          width: 100%;
          height: 100%;
        }
        
        /* Make the radio buttons slightly larger */
        .radio-container input[type='radio'] {
          width: 18px;
          height: 18px;
          margin: 0;
          cursor: pointer;
          /* Radio color is now default blue */
        }
      "))
    ),
  
  # uiOutput("scenario_table"),

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
    min_height = "9vh",
    class = "no-border-no-pad header-card",
    style = "overflow: hidden !important; margin-bottom: 3vh; flex-shrink: 0;",
    gap = "0vh",
    uiOutput("header")
  ),
  
  # Scenario descriptions - centered VS version
  card(
    full_screen = FALSE,
    height = "8vh",
    class = "no-border-no-pad",
    style = "overflow: visible !important; margin-bottom: 2vh; flex-shrink: 0;",
    div(
      class = "scenario-header",
      uiOutput("scenario_text")
    )
  ),
  # Main layout - 3 columns
  layout_columns(
    height = "75vh",
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
),
verbatimTextOutput("debugText")
)

server <- function(input, output, session) {
  
  
  # 1. Create a reactive timer that fires every 15 minutes (900,000 ms)
  data_refresh_timer <- reactiveTimer(900000)
  
  # 2. Define the function to connect and query the database
  get_impacts_data <- function() {
    # Establish connection (using your existing environment variables)
    conn <- dbConnect(dbDriver("MySQL"),
                      dbname = Sys.getenv("DB_NAME"),
                      host = Sys.getenv("DB_HOST"),
                      port = 3306,
                      user = Sys.getenv("DB_USER"),
                      password = Sys.getenv("DB_CRED"))
    
    on.exit(dbDisconnect(conn))
    
    # Perform the query and initial data processing
    DBI::dbGetQuery(conn, "select * from excess_impacts") %>%
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
  }
  
  # 3. Create the reactive data frame using the timer
  impacts_output_reactive <- reactive({
    # Invalidate the reactive expression every 15 minutes
    data_refresh_timer()
    
    # Isolate the function call from the timer to only rerun when the timer fires
    isolate({
      message("Refreshing data from MySQL at: ", Sys.time())
      get_impacts_data()
    })
  })
  
  max_report_date_reactive <- reactive({
    max(impacts_output_reactive()$report_date)
  })
  
  min_report_date_reactive <- reactive({
    min(impacts_output_reactive()$report_date)
  })
  
  
  # Default viewport dimensions
  viewportDims <- reactiveValues(
    width = 1200,
    height = 800
  )
  
  # Request viewport size when session starts
  observe({
    session$sendCustomMessage("requestViewportSize", list())
  })
  
  # fontSizeReactive <- reactive({
  #   # Use the JavaScript value if available, otherwise use a sensible default
  #   input$calculated_primary_size %||% 24
  # })
  # 
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
      ),
      
      # CSS styles for our table
      tags$style(HTML("
        /* Hide the actual radio buttons from Shiny */
        #scenario_select { 
          display: none; 
        }
        
        /* Table styling */
        .scenario-table {
          width: 100%;
          border-collapse: collapse;
          margin: 15px 0;
          box-shadow: 0 1px 3px rgba(0,0,0,0.1);
        }
        
        .scenario-table th, .scenario-table td {
          padding: 12px 15px;
          text-align: left;
          border: 1px solid #ddd;
        }
        
        .scenario-table th {
          background-color: #f5f5f5;
          font-weight: bold;
        }
        
        /* Row styling */
        .scenario-row {
          cursor: pointer;
          transition: background-color 0.2s;
        }
        
        .scenario-row:hover {
          background-color: #f0f0f0;
        }
        
        .scenario-row.selected {
          background-color: #e0e0e0;
        }
        
        /* Radio cell styling */
        .radio-cell {
          width: 60px;
          text-align: center;
          vertical-align: middle;
        }
        
        /* Radio container for better centering */
        .radio-container {
          display: flex;
          justify-content: center;
          align-items: center;
          width: 100%;
          height: 100%;
        }
        
        /* Make the radio buttons slightly larger */
        .radio-container input[type='radio'] {
          width: 18px;
          height: 18px;
          margin: 0;
          cursor: pointer;
        }
      "))
    )
  })
  
  # Create a reactive expression for the filtered data
  filtered_data <- reactive({
    req(input$grp_select, input$scenario_select)
    impacts_output_reactive() %>%
      filter(grp == input$grp_select, scenario %in% c("n/a", "actual", input$scenario_select))
  })
  
  # Extract key metrics into separate reactive expressions
  n_admits <- reactive({
    req(input$grp_select, input$scenario_select)
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
    req(input$grp_select)
    get_chart_data("om1b") %>%
      filter(!str_detect(impact, "n_exposure|raw"))
  })
  
  handover_chart_data <- reactive({
    get_chart_data("om2") %>% 
      filter(!str_detect(impact, "n_exposure|raw"))
  })
  
  
create_plot <- function(data) {
  if (is.null(data) || nrow(data) == 0) {
    return(ggplot() + 
             theme_void() + 
             labs(title = "No data available"))
  }

  col_label_size <- 10
  
  data %>%
    ggplot(aes(x = impact, y = value, fill = impact)) +
    geom_col() +
    geom_hline(aes(yintercept = target), 
               linetype = "dashed", 
               linewidth = 0.8, 
               colour = "grey30") +
    # Lightweight glow: white shadow layer
    ggtext::geom_richtext(
      aes(label = value_label), 
      size = col_label_size,  # Fixed size in mm - ggplot handles scaling
      vjust = -0.1,
      fill = "white",
      label.color = "white",
      label.padding = unit(c(0.3, 0.3, 0.3, 0.3), "lines"),
      fontface = "bold",
      colour = "white",
      alpha = 0.9
    ) +
    # Main text layer
    ggtext::geom_richtext(
      aes(label = value_label), 
      size = col_label_size,  # Fixed size in mm
      vjust = -0.1,
      fill = NA,
      label.color = NA,
      fontface = "bold",
      colour = "grey20"
    ) +
    theme_void() +
    scale_y_continuous(expand = expansion(mult = c(0, 0.5))) +
    scale_fill_manual(values = unname(app_colours)) +
    theme(
      legend.position = "none",
      plot.margin = margin(5, 5, 5, 5, "pt"),
      axis.line.x = element_line(linewidth = 1, colour = "grey30"),
      panel.grid = element_blank(),
      plot.title = element_markdown(size = rel(1.2), hjust = 0.5),
      plot.caption = element_markdown(size = rel(1.7), hjust = 0.5, 
                                      margin = margin(t = 5), 
                                      face = "bold")
    ) +
    labs(caption = data$sub_text[1], y = NULL, x = NULL)
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
    req(n_admits(), dta_time(), max_report_date_reactive())
    
    site_text <- switch(input$grp_select,
                        "BNSSG total (Bristol Royal Infirmary, Southmead Hospital, & Weston General Hospital)" = "our emergency department",
                        "BRI" = "the emergency department at BRI",
                        "NBT" = "the emergency department at NBT",
                        "WGH" = "the emergency department at WGH")
    
    h5(
      style = "line-height: 1.2; margin: 0;",  # Increased line-height from 1.4 to 1.5
      HTML(glue::glue(
        "<p class='app-standard-text' style='margin: 2.5px 0; max-height: none;'><span><strong>Between {format_week_interval(max_report_date_reactive())}</strong>, {site_text} saw <b>{n_admits()} medical admissions</b>. These patients spent a combined <b>{round(dta_time()/(60*24))} days</b> waiting for beds - a critical delay impacting both their care and the care for others in need of emergency help. Using regression analysis on data from the last 3 years, we have isolated the impact DTA delays and queue size has on patients and ambulance performance.<strong> From this we estimate the impacts based on 2 scenarios..</strong></span></p>"
      ))
    )
  })
  
  output$scenario_text <- renderUI({
    # Use req() to ensure reactive values are ready before rendering
    req(dta_time_avg(), n_dta_norm())
    
    dta_time <- dta_time_avg()
    dta_queue <- n_dta_norm()
    
    HTML(glue::glue('
<div style="display: flex; align-items: flex-start; width: 100%; font-size: 0.9vw;">
  <div style="width: 15%; flex: none;"></div>
  <div style="width: 41.67%; display: flex; align-items: flex-start; justify-content: flex-end; padding-right: 10px;">
    <span class="scenario-indicator" style="background-color: {app_colours["actual"]}; margin-right: 0.3vw; margin-top: 0.2vw;"></span>
    <span style="font-weight: 500; color: #333;"><strong>Actual</strong>: where admission waits were <strong>{dta_time} hours</strong> & the Decision to Admit (DTA) queue was <strong>{dta_queue}</strong> of the bed capacity, on average</span>
  </div>
  <div style="width: 3.33%; text-align: center; font-size: 1vw; color: #666; font-weight: bold;">VS</div>
  <div style="width: 40%; display: flex; align-items: flex-start; padding-left: 10px;">
    <span class="scenario-indicator" style="background-color: {app_colours["hypothetical"]}; margin-right: 0.3vw; margin-top: 0.2vw;"></span>
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
  create_plot(mortality_chart_data())
}, res = 96) %>% bindCache(input$scenario_select, input$grp_select)

output$chart2 <- renderPlot({
  create_plot(los_chart_data())
}, res = 96) %>% bindCache(input$scenario_select, input$grp_select)

output$chart3 <- renderPlot({
  create_plot(resp_chart_data())
}, res = 96) %>% bindCache(input$scenario_select, input$grp_select)

output$chart4 <- renderPlot({
  create_plot(handover_chart_data())
}, res = 96) %>% bindCache(input$scenario_select, input$grp_select)

  
  outputOptions(output, "scenario_table", suspendWhenHidden = FALSE)
  
}

shinyApp(ui = ui, server = server)