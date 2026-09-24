library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(ggplot2)
library(RMySQL)
library(lubridate)
library(stringr)


gen_html_text <- function(emph_text){
  str_c(
    # "<span style='font-weight:bold;font-size: 0.8em;'>",
    "<span class='app-standard-text'>",
        emph_text,
        # "</span><br><span style='font-size:15pt'>",
        # sub_text,
        "</span>")
}


# --- UI Constants & Helpers ---
metric_ids <- c("mort", "los", "resp", "handover")

# --- Custom CSS ---
app_css <- "
  /* Remove scrollbars and fix layout */
  html, body { height: 100%; overflow: hidden; }
  .main-content-container { height: 100vh; padding: 10px 20px; display: flex; flex-direction: column; }
  
  /* Clean Card Styling */
  .no-border-no-pad { border: none !important; box-shadow: none !important; padding: 0 !important; }
  .card-body { padding: 5px !important; }

  /* Scenario Table Styling */
  .scenario-table { width: 100%; border-collapse: collapse; margin-top: 10px; cursor: pointer; }
  .scenario-table th { background: #f8f9fa; padding: 8px; border: 1px solid #dee2e6; font-size: 0.9em; }
  .scenario-table td { padding: 8px; border: 1px solid #dee2e6; font-size: 0.85em; }
  .scenario-row.selected { background-color: #e8f0fe; font-weight: bold; }
  
  /* Typography */
  .app-standard-text { font-size: 0.95vw; line-height: 1.4; color: #444; }
  .sidebar-info { font-size: 0.85em; line-height: 1.3; color: #666; }
"


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
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  title = "Excess Impacts Analysis",
  
  tags$head(tags$style(app_css)),
  
  # --- Sidebar ---
  sidebar = sidebar(
    id = "sidebar",
    width = 400,
    open = FALSE,
    
    h5("Analysis Settings", style = "font-weight: bold;"),
    p("For queries: nick.howlett5@nhs.net", class = "sidebar-info"),
    
    selectInput("grp_select", "Run report for:", 
                choices = c("BNSSG total", "BRI", "NBT", "WGH"), width = "100%"),
    
    hr(),
    strong("Select Scenario:"),
    # Simplified Table Selector
    tags$table(class = "scenario-table",
      tags$thead(tags$tr(tags$th("Admission Wait"), tags$th("DTA Queue"))),
      tags$tbody(
        tags$tr(id = "row-cf", class = "scenario-row selected",
                onclick = "Shiny.setInputValue('scenario_select', 'cf'); $('.scenario-row').removeClass('selected'); $(this).addClass('selected');",
                tags$td("< 1 hour"), tags$td("< 25% capacity")),
        tags$tr(id = "row-full_cf", class = "scenario-row",
                onclick = "Shiny.setInputValue('scenario_select', 'full_cf'); $('.scenario-row').removeClass('selected'); $(this).addClass('selected');",
                tags$td("0 hours"), tags$td("0% capacity"))
      )
    ),
    
    hr(),
    div(class = "sidebar-info",
        strong("Direct Impacts:"), p("Delays correlate with 30-day mortality and length of stay."),
        strong("Indirect Impacts:"), p("Large DTA queues impact ambulance handovers."),
        tags$i("Note: Association does not imply causation.")
    )
  ),

  # --- Main Content ---
  div(class = "main-content-container",
    
    # Header Section
    card(
      height = "15vh", class = "no-border-no-pad",
      card_body(
        uiOutput("report_date_line"),
        uiOutput("scenario_text")
      )
    ),

    # Dynamic Grid for Charts and Text
    # This replaces the massive blocks of manual layout_columns
    layout_columns(
      col_widths = c(5, 7),
      gap = "10px",
      
      # Column 1: Labels/Subtext
      layout_column_wrap(
        width = 1, heights_equal = "all",
        lapply(metric_ids, function(id) {
          card(class = "no-border-no-pad", card_body(uiOutput(paste0(id, "_subtext"))))
        })
      ),
      
      # Column 2: Charts
      layout_column_wrap(
        width = 1, heights_equal = "all",
        lapply(1:4, function(i) {
          card(class = "no-border-no-pad", card_body(plotOutput(paste0("chart", i), height = "100%")))
        })
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Data refresh timer (15 minutes)
  data_refresh_timer <- reactiveTimer(900000)
  
  get_impacts_data <- function() {
    conn <- dbConnect(dbDriver("MySQL"),
                      dbname = Sys.getenv("DB_NAME"),
                      host = Sys.getenv("DB_HOST"),
                      port = 3306,
                      user = Sys.getenv("DB_USER"),
                      password = Sys.getenv("DB_CRED"))
    
    on.exit(dbDisconnect(conn))
    
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
  
  impacts_output_reactive <- reactive({
    data_refresh_timer()
    isolate({
      message("Refreshing data from MySQL at: ", Sys.time())
      get_impacts_data()
    })
  })
  
  max_report_date_reactive <- reactive({ max(impacts_output_reactive()$report_date) })
  min_report_date_reactive <- reactive({ min(impacts_output_reactive()$report_date) })
  
  filtered_data <- reactive({
    req(input$grp_select, input$scenario_select)
    impacts_output_reactive() %>%
      filter(grp == input$grp_select, scenario %in% c("n/a", "actual", input$scenario_select))
  })
  
  # Extract metrics
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
  
  dta_time_avg <- reactive({ 
    filtered_data() %>% 
      filter(impact == 'mean_dta_time', report_date == max_report_date) %>% 
      pull(emph_text) 
  })
  
  n_dta_norm <- reactive({ 
    filtered_data() %>% 
      filter(impact == 'n_dta_norm', report_date == max_report_date) %>% 
      pull(emph_text) 
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
    impacts_output_raw %>%
      filter(report_date == max_report_date, 
             impact == "om1b_raw", 
             scenario %in% c("n/a", "actual", input$scenario_select)) %>%
      pull(emph_text)
  })
  
  excess_resp_pre <- reactive({
    impacts_output_raw %>%
      filter(report_date == min_report_date, 
             impact == "om1b_raw", 
             scenario %in% c("n/a", "actual", input$scenario_select)) %>%
      pull(emph_text)
  })
  
  n_resp <- reactive({ 
    impacts_output_raw %>% 
      filter(report_date == max_report_date, impact == "om1b_n_exposure") %>% 
      pull(emph_text) 
  })
  
  resp_target <- reactive({ 
    impacts_output_raw %>% 
      filter(report_date == max_report_date, impact == "om1b_n_exposure") %>% 
      pull(target) 
  })
  
  excess_handover <- reactive({ 
    filtered_data() %>% 
      filter(report_date == max_report_date, impact == "om2_raw") %>% 
      pull(emph_text) 
  })
  
  excess_handover_pre <- reactive({ 
    filtered_data() %>% 
      filter(report_date == min_report_date, impact == "om2_raw") %>% 
      pull(emph_text) 
  })
  
  n_handover <- reactive({ 
    filtered_data() %>% 
      filter(report_date == max_report_date, impact == "om2_n_exposure") %>% 
      pull(emph_text) 
  })
  
  handover_target <- reactive({ 
    filtered_data() %>% 
      filter(report_date == max_report_date, impact == "om2_n_exposure") %>% 
      pull(target) 
  })
  
  n_amb <- reactive({
    val <- excess_handover()
    if (is.null(val) || length(val) == 0) return(0)
    val %>% str_remove_all(",") %>% as.numeric() %>% `/`(168) %>% round()
  })
  
  # Get chart data
  get_chart_data <- function(metric) {
    if (metric %in% c("om1b")) {
      return(impacts_output_raw %>%
               filter(report_date == max_report_date,
                      scenario %in% c("n/a", "actual", input$scenario_select),
                      str_detect(impact, paste0("^", metric))) %>%
               select(impact, value, label, target, sub_text, value_label))
    } else {
      return(impacts_output_raw %>%
               filter(report_date == max_report_date,
                      grp == input$grp_select,
                      scenario %in% c("n/a", "actual", input$scenario_select),
                      str_detect(impact, paste0("^", metric))) %>%
               select(impact, value, label, target, sub_text, value_label))
    }
  }
  
  mortality_chart_data <- reactive({ 
    get_chart_data("om6") %>% 
      filter(!str_detect(impact, "n_exposure|raw")) 
  })
  
  los_chart_data <- reactive({ 
    get_chart_data("om4") %>% 
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
  
  # Function to create plots - now with fixed dimensions for PNG
  create_plot <- function(data, width = 800, height = 400) {
    if (is.null(data) || nrow(data) == 0) {
      return(ggplot() + theme_void() + labs(title = "No data available"))
    }
    
    # Calculate font size based on plot dimensions
    font_size <- width / 50
    
    data %>%
      ggplot(aes(x = impact, y = value, fill = impact)) +
      geom_col() +
      geom_hline(aes(yintercept = target), linetype = 2, size = 2, colour = "grey30") +
      ggfx::with_outer_glow(
        geom_textbox(aes(label = value_label), 
                     size = font_size, 
                     fill = NA, 
                     box.color = NA, 
                     halign = 0.5, 
                     vjust = -0.2, 
                     width = unit(0.4, "npc")),
        colour = "white", 
        x_offset = 0, 
        y_offset = 0, 
        expand = 12.5, 
        sigma = 0
      ) +
      theme_void() +
      scale_y_continuous(expand = expansion(mult = c(0, 0.75))) +
      scale_fill_manual(values = unname(app_colours)) +
      theme(
        legend.position = "off",
        plot.margin = margin(0, 0, 0, 0, "pt"),
        axis.line.x = element_line(linewidth = 2.5, colour = "grey30"),
        plot.caption = element_textbox(size = font_size * 1.2, hjust = 0.5, margin = margin(t = 10), face = "bold")
      ) +
      labs(caption = data$sub_text[1], y = "", x = "")
  }
  
  # Render plots as PNG images
  output$chart1 <- renderImage({
    outfile <- tempfile(fileext = ".png")
    png(outfile, width = 800, height = 400, res = 96)
    print(create_plot(mortality_chart_data(), 800, 400))
    dev.off()
    list(src = outfile, contentType = "image/png", alt = "Mortality chart")
  }, deleteFile = TRUE)
  
  output$chart2 <- renderImage({
    outfile <- tempfile(fileext = ".png")
    png(outfile, width = 800, height = 400, res = 96)
    print(create_plot(los_chart_data(), 800, 400))
    dev.off()
    list(src = outfile, contentType = "image/png", alt = "Length of stay chart")
  }, deleteFile = TRUE)
  
  output$chart3 <- renderImage({
    outfile <- tempfile(fileext = ".png")
    png(outfile, width = 800, height = 400, res = 96)
    print(create_plot(resp_chart_data(), 800, 400))
    dev.off()
    list(src = outfile, contentType = "image/png", alt = "Response time chart")
  }, deleteFile = TRUE)
  
  output$chart4 <- renderImage({
    outfile <- tempfile(fileext = ".png")
    png(outfile, width = 800, height = 400, res = 96)
    print(create_plot(handover_chart_data(), 800, 400))
    dev.off()
    list(src = outfile, contentType = "image/png", alt = "Handover time chart")
  }, deleteFile = TRUE)
  
  # UI outputs
  output$report_date_line <- renderUI({ static_report_date_line })
  
  output$header <- renderUI({
    req(n_admits(), dta_time(), max_report_date_reactive())
    
    site_text <- switch(input$grp_select,
                        "BNSSG total (Bristol Royal Infirmary, Southmead Hospital, & Weston General Hospital)" = "our emergency department",
                        "BRI" = "the emergency department at BRI",
                        "NBT" = "the emergency department at NBT",
                        "WGH" = "the emergency department at WGH")
    
    h5(
      style = "line-height: 1.2; margin: 0;",
      HTML(glue::glue(
        "<p class='app-standard-text' style='margin: 2.5px 0; max-height: none;'><span><strong>Between {format_week_interval(max_report_date_reactive())}</strong>, {site_text} saw <b>{n_admits()} medical admissions</b>. These patients spent a combined <b>{round(dta_time()/(60*24))} days</b> waiting for beds - a critical delay impacting both their care and the care for others in need of emergency help. Using regression analysis on data from the last 3 years, we have isolated the impact DTA delays and queue size has on patients and ambulance performance.<strong> From this we estimate the impacts based on 2 scenarios..</strong></span></p>"
      ))
    )
  })
  
  output$scenario_text <- renderUI({
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
}

shinyApp(ui = ui, server = server)