library(dplyr)
library(stringr)
library(ggplot2)
library(shiny)
library(tidyr)
library(lubridate)
library(bslib)
library(ggtext)
library(RMySQL)

app_colours <- c(
  actual = "#88a0a8",     # actual
  hypothetical = "#B4CEB3" # hypothetical
)

gen_html_text <- function(emph_text){
  str_c("<span style='font-weight:bold;font-size:30pt;'>",
        emph_text,
        # "</span><br><span style='font-size:15pt'>",
        # sub_text,
        "</span>")
}

# 1. Module for Database Connection
# Encapsulates database connection logic
db_connection_module <- function() {
  # Server-side logic (no UI in this module)
  moduleServer(
    "db_connection",
    function(input, output, session) {
      # Connection details (consider storing these in a config file)
      host <- Sys.getenv("DB_HOST")
      dbname <- Sys.getenv("DB_NAME")
      user <- Sys.getenv("DB_USER")
      password <- Sys.getenv("DB_CRED")
      port <- 3306
      
      # Create and return the connection
      conn <- dbConnect(RMySQL::MySQL(), # Specify the package explicitly
                        dbname = dbname,
                        host = host,
                        port = port,
                        user = user,
                        password = password)
      
      # Ensure disconnection on session end
      session$onSessionEnded(function() {
        if (DBI::dbIsValid(conn)) { # Check if the connection is still valid
          DBI::dbDisconnect(conn)
        }
      })
      
      return(conn) # Return the connection object
    }
  )
}


# 2. Module for Data Retrieval and Processing
# Encapsulates data fetching and transformation
data_module <- function() {
  moduleServer(
    "data_processing",
    function(input, output, session) {
      # Use the database connection from the module
      
      conn <- db_connection_module()#get the connection
      # conn <- callModule(db_connection_module, "db_connection") #get the connection
      
      # Reactive expression for raw data (dependent on grp_select)
      impacts_output_raw <- reactive({
        req(conn) # Ensure the database connection is valid.
        
        DBI::dbGetQuery(conn, "SELECT * FROM excess_impacts") %>%
          mutate(report_date = ymd(report_date)) %>%
          mutate(grp = recode(
            grp,
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
              str_detect(impact, r"((?<!full_)cf)") |
                str_detect(impact, r"((?<!full_)raw)") ~ "cf",
              str_detect(impact, "full_cf") |
                str_detect(impact, "full_raw") ~ "full_cf",
              str_detect(impact, r"(impact(?!_cf))") ~ "actual",
              .default = "n/a"
            ),
            sub_text = case_when(
              str_detect(impact, "om6_impact") ~ str_c(sub_text, "*"),
              str_detect(impact, "om4_impact") ~ str_c(sub_text, "*"),
              str_detect(impact, "om2_impact") ~ str_c(sub_text, "**"),
              str_detect(impact, "om1[abc]_impact") ~ str_c(sub_text, "**")
            )
          ) %>%
          mutate(impact = str_remove(impact, "_full")) %>%
          mutate(value_label = gen_html_text(emph_text))
      })
      
      # Store max report date as a static value
      max_report_date <- reactive({
        max(impacts_output_raw()$report_date)
      })
      
      # Return the processed data and max_report_date
      return(list(
        impacts_output_raw = impacts_output_raw,
        max_report_date = max_report_date
      ))
    }
  )
}


# 3. UI Module for Scenario Selection Table
# Modularizes the radio button table
radio_table_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "radio-table-wrapper",
    div(
      class = "radio-buttons-column",
      radioButtons(
        ns("scenario_select"),
        label = NULL,
        choices = list(
          "Option 1" = "cf",
          "Option 2" = "full_cf"
        ),
        selected = "cf"
      )
    ),
    div(
      class = "table-container",
      tags$table(
        class = "content-table",
        tags$thead(tags$tr(
          tags$th(class = "col-select", "Select"),
          tags$th("Medical waits for admission"),
          tags$th("Medical DTA Queue")
        )),
        tags$tbody(
          tags$tr(
            id = ns("row-cf"),
            class = "content-row",
            tags$td(class = "col-select", ""),
            tags$td("< 1 hour"),
            tags$td("< 25% of ED bed capacity")
          ),
          tags$tr(
            id = ns("row-full_cf"),
            class = "content-row",
            tags$td(class = "col-select", ""),
            tags$td("0 hour"),
            tags$td("0% of ED bed capacity")
          )
        )
      )
    ),
    tags$script(
      HTML(
        "
        $(document).ready(function() {
          function updateRowHighlighting() {
            var selectedValue = $('#scenario_select').val();
            $('.content-row').removeClass('active');
            $('#row-' + selectedValue).addClass('active');
          }
          updateRowHighlighting();
          $('#scenario_select input').on('change', function() {
            updateRowHighlighting();
          });
        });
        "
      )
    ),
    tags$style(
      HTML(
        "
        /* CSS styles (same as before) */
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
        
        .content-table th,
        .content-table td {
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
        "
      )
    )
  )
}

# 4.  Module server for Scenario Selection
radio_table_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # No server logic needed for the table in this example, but you
    # could add logic here to react to radio button changes if needed.
    # For example, you could update a reactive value based on the
    # selected scenario:
    
    selected_scenario <- reactive({
      input$scenario_select
    })
    
    return(selected_scenario) # Make selected scenario available to main server
  })
}


# 5. UI for the Main Content Area
#Organized the main UI elements
main_content_ui <- function(id) {
  ns <- NS(id)
  div(
    # Only show main heading when sidebar is closed
    conditionalPanel(
      condition = "input.sidebar !== true",
      div(
        id = "main_heading",
        class = "main-heading",
        uiOutput(ns("report_date_line"))
      )
    ),
    # Top heading paragraph
    card(
      full_screen = FALSE,
      height = "100px",
      class = "no-border-no-pad header-card",
      style = "overflow: visible !important; margin-bottom: 20px; flex-shrink: 0;",
      gap = "0px",
      uiOutput(ns("header"))
    ),
    # Scenario descriptions - centered VS version
    card(
      full_screen = FALSE,
      height = "100px",
      class = "no-border-no-pad",
      style = "overflow: visible !important; margin-bottom: 20px; flex-shrink: 0;",
      div(class = "scenario-header", uiOutput(ns("scenario_text")))
    ),
    # Main layout - 3 columns
    layout_columns(
      height = "calc(100vh - 250px)",
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
          style = "display: flex; align-items: center; justify-content: flex-start; padding: 0;",
          card_body(
            style = "width: 100%; height: 100%; display: flex; align-items: center; padding: 10px;",
            uiOutput(ns("mort_subtext"))
          )
        ),
        # Left card 2
        card(
          class = "no-border-no-pad",
          height = "20%",
          style = "display: flex; align-items: center; justify-content: flex-start; padding: 0;",
          card_body(
            style = "width: 100%; height: 100%; display: flex; align-items: center; padding: 10px;",
            uiOutput(ns("los_subtext"))
          )
        ),
        # Left card 3
        card(
          class = "no-border-no-pad",
          height = "20%",
          style = "display: flex; align-items: center; justify-content: flex-start; padding: 0;",
          card_body(
            style = "width: 100%; height: 100%; display: flex; align-items: center; padding: 10px;",
            uiOutput(ns("resp_subtext"))
          )
        ),
        # Left card 4
        card(
          class = "no-border-no-pad",
          height = "20%",
          style = "display: flex; align-items: center; justify-content: flex-start; padding: 0;",
          card_body(
            style = "width: 100%; height: 100%; display: flex; align-items: center; padding: 10px;",
            uiOutput(ns("handover_subtext"))
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
                height = "20%",
                card_body(plotOutput(ns("chart1"), height = "100%"))
              ),
              # Chart 2
              card(
                class = "no-border-no-pad",
                # style = "padding: 0px !important;border-top: none !important;border-left: none !important;border-right: none !important; border-bottom: 3px solid #dee2e6 !important;box-shadow: none !important;--bs-card-spacer-y: 0 !important;",
                height = "20%",
                card_body(plotOutput(ns("chart2"), height = "100%"))
              ),
              # Chart 3
              card(
                class = "no-border-no-pad",
                # style = "padding: 0px !important;border-top: none !important;border-left: none !important;border-right: none !important; border-bottom: 3px solid #dee2e6 !important;box-shadow: none !important;--bs-card-spacer-y: 0 !important;",
                height = "20%",
                card_body(plotOutput(ns("chart3"), height = "100%"))
              ),
              # Chart 4
              card(
                class = "no-border-no-pad",
                # style = "padding: 0px !important;border-top: none !important;border-left: none !important;border-right: none !important; box-shadow: none !important;--bs-card-spacer-y: 0 !important;",
                height = "20%",
                card_body(plotOutput(ns("chart4"), height = "100%"))
              )
            )
          )
        )
      ),
      # Right column - text descriptions
      layout_column_wrap(
        width = 1,
        height = "100%",
        # style = "padding-left: 1rem;",
        card(
          class = "no-border-no-pad-text-center",
          height = "25%",
          card_body(HTML(
            "<p class='app-standard-text'>
            The <strong>mortality rate</strong> is shown as the number of <strong>additional deaths</strong>
            within 30 days of admission, per 1000 admissions, compared to the expected rate
            if there were no ED delays.
            </p>"
          ))
        ),
        card(
          class = "no-border-no-pad-text-center",
          height = "25%",
          card_body(HTML(
            "<p class='app-standard-text'>
            <strong>Length of stay</strong> is shown as the <strong>additional days</strong>
            patients stay in hospital, on average, compared to if there were no ED delays.
            </p>"
          ))
        ),
        card(
          class = "no-border-no-pad-text-center",
          height = "25%",
          card_body(HTML(
            "<p class='app-standard-text'>
            <strong>Emergency response</strong> is shown as the <strong>extra minutes</strong>
            taken, on average, for ambulances to reach patients.
            </p>"
          ))
        ),
        card(
          class = "no-border-no-pad-text-center",
          height = "25%",
          card_body(HTML(
            "<p class='app-standard-text'>
            <strong>Ambulance handover delay</strong> is shown as the <strong>extra hours</strong>
            patients wait in ambulances before being admitted to the ED.
            </p>"
          ))
        )
      )
    )
  )
}


# 6. Server Logic for the Main Content Area
main_content_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Get data from data_module
    data_objects <- data_module()
    # data_objects <- callModule(data_module, "data_processing")
    impacts_output_raw <- data_objects$impacts_output_raw
    max_report_date <- data_objects$max_report_date
    
    # Get selected scenario from radio table
    selected_scenario <- radio_table_server("scenario_table")
    # selected_scenario <- callModule(radio_table_server, "scenario_table")
    
    #  Filter data based on user selections
    filtered_data <- reactive({
      req(impacts_output_raw(), input$grp_select, selected_scenario()) # Ensure data and inputs are available
      
      impacts_output_raw() %>%
        filter(report_date == max_report_date()) %>% #use reactive
        filter(grp == input$grp_select) %>%
        filter(scenario %in% c(selected_scenario(), "n/a", "actual"))
    })
    
    # ---  UI Output ---
    output$report_date_line <- renderUI({
      HTML(
        glue::glue(
          "<span style='line-height: 1'><h5><strong>Excess Impacts:</strong> a what-if analysis</h5></span>"
        )
      )
    })
    
    output$header <- renderUI({
      req(input$grp_select)
      
      grp_name <- input$grp_select
      
      HTML(
        glue::glue(
          "<h1 class='primary-text'> {grp_name}</h1>
          <p class='secondary-text'>
          The following charts show the estimated impact of delays to hospital admission
          on patient outcomes for <strong>{grp_name}</strong>.
          </p>"
        )
      )
    })
    
    output$scenario_text <- renderUI({
      HTML(
        "<div class='scenario-grid'>
          <span class='scenario-label'>
            <span class='scenario-indicator' style='background-color: #B4CEB3;'></span>
            Option 1
          </span>
          <span class='scenario-vs'> vs </span>
          <span class='scenario-label'>
            <span class='scenario-indicator' style='background-color: #88a0a8;'></span>
            Option 2
          </span>
        </div>"
      )
    })
    
    # --- Data for the value boxes ---
    # Mortality
    mortality_data <- reactive({
      req(filtered_data())
      filtered_data() %>%
        filter(impact == "mortality_impact")
    })
    
    n_mort <- reactive({
      req(mortality_data())
      mortality_data() %>%
        filter(scenario == "cf") %>%
        pull(value_label)
    })
    
    excess_mortality <- reactive({
      req(mortality_data())
      mortality_data() %>%
        filter(scenario == "cf") %>%
        pull(sub_text)
    })
    
    # Length of Stay
    los_data <- reactive({
      req(filtered_data())
      filtered_data() %>%
        filter(impact == "los_impact")
    })
    
    n_los <- reactive({
      req(los_data())
      los_data() %>%
        filter(scenario == "cf") %>%
        pull(value_label)
    })
    
    excess_los <- reactive({
      req(los_data())
      los_data() %>%
        filter(scenario == "cf") %>%
        pull(sub_text)
    })
    
    # Response Time
    resp_data <- reactive({
      req(filtered_data())
      filtered_data() %>%
        filter(impact == "ambulance_impact")
    })
    
    n_resp <- reactive({
      req(resp_data())
      resp_data() %>%
        filter(scenario == "cf") %>%
        pull(value_label)
    })
    
    excess_resp <- reactive({
      req(resp_data())
      resp_data() %>%
        filter(scenario == "cf") %>%
        pull(sub_text)
    })
    
    resp_target <- reactive({
      req(resp_data())
      resp_data() %>%
        filter(scenario == "cf") %>%
        pull(target)
    })
    
    # Handover Time
    handover_data <- reactive({
      req(filtered_data())
      filtered_data() %>%
        filter(impact == "handover_impact")
    })
    
    n_handover <- reactive({
      req(handover_data())
      handover_data() %>%
        filter(scenario == "cf") %>%
        pull(value_label)
    })
    
    excess_handover <- reactive({
      req(handover_data())
      handover_data() %>%
        filter(scenario == "cf") %>%
        pull(sub_text)
    })
    
    handover_target <- reactive({
      req(handover_data())
      handover_data() %>%
        filter(scenario == "cf") %>%
        pull(target)
    })
    
    n_amb <- reactive({
      req(handover_data())
      handover_data() %>%
        filter(scenario == "cf") %>%
        pull(context)
    })
    
    # ---  Value box  outputs ---
    output$mort_subtext <- renderUI({
      req(n_mort(), excess_mortality())
      HTML(
        glue::glue(
          "<p class='app-standard-text style='margin: 0;'>
          The <strong> excess mortality </strong> is <strong> {n_mort()} deaths per 1000 admissions </strong>
          {excess_mortality()}
          </p>"
        )
      )
    })
    
    output$los_subtext <- renderUI({
      req(n_los(), excess_los())
      HTML(
        glue::glue(
          "<p class='app-standard-text style='margin: 0;'>
          The <strong> extra length of stay </strong> is <strong> {n_los()} days </strong>
          {excess_los()}
          </p>"
        )
      )
    })
    
    output$resp_subtext <- renderUI({
      req(n_resp(), excess_resp(), resp_target())
      HTML(
        glue::glue(
          "<p class='app-standard-text style='margin: 0;'>
          The <strong> extra time </strong> for ambulances to reach patients is <strong> {n_resp()} minutes </strong>
          {excess_resp()}.
          The impact of this excess on meeting the <strong> national target </strong> that <strong> 95% </strong> of responses are within <strong> {resp_target()} minutes </strong> can be seen in the chart.
          </p>"
        )
      )
    })
    
    output$handover_subtext <- renderUI({
      req(n_handover(), excess_handover(), handover_target(), n_amb())
      HTML(
        glue::glue(
          "<p class='app-standard-text style='margin: 0;'>
          The <strong> {n_handover()} </strong> ambulance handovers over the <strong> last 7 days </strong>
          had an <strong> excess {excess_handover()} hours </strong> handover time combined,
          <strong> due to the medical DTA queue </strong>. This excess delay is
          <strong> equivalent to {n_amb()} ambulances being out of service throughout the last 7 days </strong>.
          The impact of this excess on meeting the <strong> national target (dashed-line) </strong>
          that <strong> 90% </strong> of handovers are within <strong> {handover_target()} minutes </strong>
          can be seen in the chart.
          </p>"
        )
      )
    })
    
    # --- Chart data ---
    mortality_chart_data <- reactive({
      req(filtered_data())
      filtered_data() %>%
        filter(impact == "mortality_impact")
    })
    
    los_chart_data <- reactive({
      req(filtered_data())
      filtered_data() %>%
        filter(impact == "los_impact")
    })
    
    resp_chart_data <- reactive({
      req(filtered_data())
      filtered_data() %>%
        filter(impact == "ambulance_impact")
    })
    
    handover_chart_data <- reactive({
      req(filtered_data())
      filtered_data() %>%
        filter(impact == "handover_impact")
    })
    
    # --- Plot outputs ---
    output$chart1 <- renderPlot({
      create_plot(mortality_chart_data())
    }) %>% bindCache(selected_scenario(), input$grp_select)
    
    output$chart2 <- renderPlot({
      create_plot(los_chart_data())
    }) %>% bindCache(selected_scenario(), input$grp_select)
    
    output$chart3 <- renderPlot({
      create_plot(resp_chart_data())
    }) %>% bindCache(selected_scenario(), input$grp_select)
    
    output$chart4 <- renderPlot({
      create_plot(handover_chart_data())
    })  %>% bindCache(selected_scenario(), input$grp_select)
    
  })
}


# 7.  Plotting function
#   -  Moved the plotting function outside the server
create_plot <- function(data) {

  req(data)
  #use the app colors
  actual_colour <- app_colours["actual"]
  hypothetical_colour <- app_colours["hypothetical"]
  
  ggplot(data, aes(x = category, y = value, fill = scenario)) +
    geom_bar(
      stat = "identity",
      position = "dodge",
      width = 0.7,
      aes(alpha = scenario)
    ) +
    geom_errorbar(
      aes(
        ymin = lower_ci,
        ymax = upper_ci,
        group = scenario
      ),
      position = position_dodge(width = 0.7),
      width = 0.2,
      color = "black"
    ) +
    scale_fill_manual(
      values = c(actual = actual_colour, cf = hypothetical_colour),
      labels = c("Option 2", "Option 1")
    ) +
    scale_alpha_manual(values = c(actual = 1, cf = 1)) + #both are fully opaque
    
    # Add target line if it exists in the data
    {
      if ("target" %in% names(data))
        geom_hline(
          aes(yintercept = target),
          color = "black",
          linetype = "dashed"
        )
    } +
    
    #  Add labels
    labs(
      x = NULL,
      y = NULL,
      fill = NULL
    ) +
    
    # Theme
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(
        size = 14,
        face = "bold",
        hjust = 0.5
      ),
      axis.text.x = element_text(
        size = 12,
        angle = 0,
        hjust = 0.5,
        vjust = 0.5
      ),
      axis.text.y = element_text(size = 12),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_line(
        color = "#E0E0E0",
        size = 0.5
      ),
      plot.margin = margin(5, 5, 5, 5, "mm")
    )
}


# 8.  Shiny App
#  -  Call the modules
ui <- page_sidebar(
  tags$head(
    tags$style(
      "
      /* Main page and container settings */
html,
body,
.bslib-page-fill {
  height: 100vh !important;
  max-height: 100vh !important;
  overflow: hidden !important;
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
  justify-content: center !important;
  /* Vertical centering */
  height: 100%;
  --bs-card-spacer-y: 0 !important;
  --bs-card-spacer-x: 0 !important;
}

.no-border-no-pad-text-center .card-body {
  display: flex !important;
  align-items: center !important;
  /* Vertical centering */
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

.bslib-sidebar-layout>.bslib-sidebar-layout-panel {
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
  box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
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

.content-table th,
.content-table td {
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
  font-size: 16px !important;
  /* Choose your preferred size */
  line-height: 1.5 !important;
  margin: 5px 0 !important;
  font-weight: 400 !important;
}
"
    ),
tags$script(
  HTML(
    "
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
      "
  )
)
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
    HTML(
      glue::glue(
        # "<span><h4><strong>Excess Impacts</strong> </br> A what-if analysis</h4> Based on the week </br> <strong> {format(max_report_date-ddays(7), '%d')}-{format(max_report_date, '%d %B %Y')}</strong></span>"
        "<span><h4><strong>Excess Impacts</strong> </br> A what-if analysis</h4></span>"
      )
    )
  ),
  div(
    style = "margin-top: 20px;",
    tags$strong("Run report for:", style = "font-weight: 500;"),
    selectInput(
      "grp_select",
      label = NULL,
      choices = c(
        "BNSSG total (Bristol Royal Infirmary, Southmead Hospital, & Weston General Hospital)",
        "BRI",
        "NBT",
        "WGH"
      ),
      width = "100%",
      selectize = TRUE
    )
  ),
  # Use the UI from the radio table module
  radio_table_ui("scenario_table"),
  HTML(
    "<h4>*Direct Impacts</h4>
      <p>
        When patients experience delays in hospital admission, there are direct negative
        consequences on their health outcomes. Our analysis has revealed a clear
        association between these admission delays and increased 30-day mortality
        rates, as well as prolonged in-hospital lengths of stay.
      </p>
      </p>
      <h4>**Indirect Impacts</h4>
      <p>
        The medical Decision to Admit (DTA) queue has wider impact, affecting not only
        patients waiting for admission, but also creating knock-on effects for other
        patients arriving at the Emergency Department. Specifically, our analysis
        demonstrates that a larger DTA queue is associated with a measurable
        increase in both ambulance handover times and emergency response times,
        indicating a bottleneck in the patient flow.
      </p>"
  )
),
div(
  class = "main-content-container",
  # Use the main content UI
  main_content_ui("main_content")
)
)

server <- function(input, output, session) {
  

  # # Initialize database connection
  # #  -  Call the db connection module
  # # db_connection_module("db_connection")
  # callModule(db_connection_module, "db_connection")
  # 
  # # Initialize data and max report date
  # #  -  Call the data module
  # # data_objects <- data_module("data_processing")
  # data_objects <- data_module()
  # data_objects <- callModule(data_module, "data_processing")
  # 
  # #Call the main content server
  # # main_content_server("main_content")
  # callModule(main_content_server, "main_content")
  
  db_connection_module()
  data_objects <- data_module()
  main_content_server("main_content")
  
}

shinyApp(ui, server)
