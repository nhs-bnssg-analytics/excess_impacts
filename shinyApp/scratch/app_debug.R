library(tidyverse)
library(shiny)
library(bslib)
library(ggtext)

ui <- page_fluid(
 selectInput("grp_select", label = NULL, choices = c("System total", "BRI", "NBT", "WGH")),
 verbatimTextOutput("debug_n_resp"),
 verbatimTextOutput("debug_excess_resp"),
 verbatimTextOutput("debug_resp_target")
 # uiOutput("los_subtext"),
 # uiOutput("mort_subtext"),
 # uiOutput("handover_subtext"),
 # uiOutput("resp_subtext")
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
    impacts_output_raw %>%
      filter(report_date == max_report_date, 
             impact %in% c("om1b_raw")) %>%
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
    
    print("Debug info:")
    print(n_admits())
    print(excess_mort())
    
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
  
  output$debug_n_resp <- renderPrint({
    tryCatch({
      result <- n_resp()
      print(paste("n_resp result:", result))
      print(paste("n_resp class:", class(result)))
      print(paste("n_resp is NULL:", is.null(result)))
      print(paste("n_resp length:", length(result)))
      result
    }, error = function(e) {
      print(paste("Error in n_resp():", e$message))
      NULL
    })
  })
  
  output$debug_excess_resp <- renderPrint({
    tryCatch({
      result <- excess_resp()
      print(paste("excess_resp result:", result))
      print(paste("excess_resp class:", class(result)))
      print(paste("excess_resp is NULL:", is.null(result)))
      print(paste("excess_resp length:", length(result)))
      result
    }, error = function(e) {
      print(paste("Error in excess_resp():", e$message))
      NULL
    })
  })
  
  output$debug_resp_target <- renderPrint({
    tryCatch({
      result <- resp_target()
      print(paste("resp_target result:", result))
      print(paste("resp_target class:", class(result)))
      print(paste("resp_target is NULL:", is.null(result)))
      print(paste("resp_target length:", length(result)))
      result
    }, error = function(e) {
      print(paste("Error in resp_target():", e$message))
      NULL
    })
  })
  
}

shinyApp(ui = ui, server = server)