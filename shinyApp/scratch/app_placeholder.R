library(shiny) 
library(bslib)
library(ggplot2)

ui <- page_fluid(
  theme = bs_theme(version = 5),
  
  # Add CSS for tooltip positioning and tables
  tags$head(
    tags$style("
      .tooltip-inner {
        max-width: 300px;
        text-align: left;
      }
      .tooltip-table {
        margin: 5px 0;
        border-collapse: separate;
        border-spacing: 2px;
      }
      .tooltip-table td {
        padding: 2px 8px;
        font-size: 0.9em;
      }
      .tooltip-table td:first-child {
        color: #666;
      }
    ")
  ),
  
  h1("Business Metrics Dashboard", class = "mt-3 mb-4"),
  
  layout_columns(
    col_widths = c(6, 6),
    
    # Left column
    card(
      card(
        class = "mb-3",
        card_header("Revenue"),
        h5("Monthly Revenue"),
        tooltip(
          plotOutput("revenue_plot", 
                     height = "120px",
                     hover = hoverOpts(id = "revenue_hover", delay = 100)),
          HTML("
            <table class='tooltip-table'>
              <tr><td>YTD Revenue:</td><td>$2.8M</td></tr>
              <tr><td>Q3 Growth:</td><td>15%</td></tr>
              <tr><td>Top Product:</td><td>Widget Pro</td></tr>
              <tr><td>Profit Margin:</td><td>32%</td></tr>
            </table>
          ")
        )
      ),
      p(
        "Monthly revenue is",
        span("up by 12%", style = "color: #198754; font-weight: bold;"),
        "compared to last month, showing strong growth in our core business segments."
      ),
      card(
        card_header("Customer Base"),
        h5("Active Users"),
        tooltip(
          plotOutput("users_plot", 
                     height = "120px",
                     hover = hoverOpts(id = "users_hover", delay = 100)),
          HTML("
            <table class='tooltip-table'>
              <tr><td>Daily Active:</td><td>8,234</td></tr>
              <tr><td>Avg Session:</td><td>45 mins</td></tr>
              <tr><td>Growth Rate:</td><td>25% YoY</td></tr>
              <tr><td>Mobile Users:</td><td>68%</td></tr>
            </table>
          ")
        )
      ),
      p(
        "Our active user base has shown",
        span("steady growth", style = "color: #0dcaf0; font-weight: bold;"),
        "with a retention rate of 85% over the past quarter."
      )
    ),
    
    # Right column
    card(
      card(
        class = "mb-3",
        card_header("Expenses"),
        h5("Operating Costs"),
        tooltip(
          plotOutput("expenses_plot", 
                     height = "120px",
                     hover = hoverOpts(id = "expenses_hover", delay = 100)),
          HTML("
            <table class='tooltip-table'>
              <tr><td>Personnel:</td><td>$44,205</td></tr>
              <tr><td>Infrastructure:</td><td>$29,470</td></tr>
              <tr><td>Marketing:</td><td>$14,735</td></tr>
              <tr><td>Other:</td><td>$9,824</td></tr>
            </table>
          ")
        )
      ),
      p(
        "Operating expenses are",
        span("slightly elevated", style = "color: #dc3545; font-weight: bold;"),
        "due to recent infrastructure investments and expansion efforts."
      ),
      card(
        card_header("Customer Satisfaction"),
        h5("CSAT Score"),
        tooltip(
          plotOutput("satisfaction_plot", 
                     height = "120px",
                     hover = hoverOpts(id = "satisfaction_hover", delay = 100)),
          HTML("
            <table class='tooltip-table'>
              <tr><td>Response Rate:</td><td>68%</td></tr>
              <tr><td>NPS Score:</td><td>72</td></tr>
              <tr><td>Support Rating:</td><td>4.9/5.0</td></tr>
              <tr><td>Feature Rating:</td><td>4.7/5.0</td></tr>
            </table>
          ")
        )
      ),
      p(
        "Customer satisfaction remains",
        span("consistently high", style = "color: #ffc107; font-weight: bold;"),
        "with particularly positive feedback on our new features."
      )
    )
  )
)

server <- function(input, output) {
  # Helper function to create number plots
  create_number_plot <- function(value, color, subtext) {
    ggplot() +
      geom_text(aes(x = 0, y = 0.2, label = value),
                size = 16,  # Larger text size
                color = color,
                fontface = "bold") +
      geom_text(aes(x = 0, y = -0.2, label = subtext),
                size = 4,  # Smaller text size
                color = "black") +
      theme_void() +
      theme(
        plot.margin = margin(0, 0, 0, 0),
        plot.background = element_rect(fill = "transparent", color = NA)
      ) +
      coord_cartesian(clip = "off", ylim = c(-0.5, 0.5))
  }
  
  output$revenue_plot <- renderPlot({
    create_number_plot("$245,678", "#198754", "12% increase from last month")
  })
  
  output$users_plot <- renderPlot({
    create_number_plot("12,459", "#0dcaf0", "85% retention rate")
  })
  
  output$expenses_plot <- renderPlot({
    create_number_plot("$98,234", "#dc3545", "8% over budget")
  })
  
  output$satisfaction_plot <- renderPlot({
    create_number_plot("4.8/5.0", "#ffc107", "Top 95th percentile")
  })
}

shinyApp(ui, server)
