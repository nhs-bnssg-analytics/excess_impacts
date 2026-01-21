library(shiny)
library(bslib)

ui <- page_sidebar(
  title = "Scenario Selection Demo",
  sidebar = sidebar(
    title = "Controls",
    "Select a scenario to display"
  ),
  
  mainPanel(
    card(
      card_header("Scenario Selection"),
      
      # Table-style radio buttons with column headers
      div(
        class = "table-radio-container",
        # Table header
        div(class = "table-radio-header",
            div("Select"),
            div("Medical waits for admission"),
            div("Medical DTA Queue")
        ),
        
        # Custom radio buttons with table formatting
        div(
          class = "table-radio-body",
          # First option
          div(class = "table-radio-row",
              div(class = "radio-cell", 
                  tags$input(type = "radio", name = "scenario_select", value = "cf", id = "scenario_select_cf", checked = "checked")),
              div(class = "data-cell", "< 1 hour"),
              div(class = "data-cell", "< 25% of ED bed capacity")
          ),
          
          # Second option
          div(class = "table-radio-row",
              div(class = "radio-cell", 
                  tags$input(type = "radio", name = "scenario_select", value = "full_cf", id = "scenario_select_full_cf")),
              div(class = "data-cell", "0 hour"),
              div(class = "data-cell", "0% of ED bed capacity")
          )
        )
      ),
      
      # Custom CSS for the table-style radio buttons
      tags$style(HTML("
        .table-radio-container {
          display: table;
          width: 100%;
          border-collapse: collapse;
          border: 1px solid #ddd;
        }
        
        /* Header row styling */
        .table-radio-header {
          display: table-row;
          font-weight: bold;
          background-color: #f2f2f2;
        }
        .table-radio-header div {
          display: table-cell;
          padding: 10px 15px;
          border-bottom: 2px solid #ddd;
        }
        
        /* Set column widths */
        .table-radio-header div:nth-child(1),
        .radio-cell {
          width: 80px;
          text-align: center;
        }
        
        /* Body rows styling */
        .table-radio-body {
          display: table-row-group;
        }
        .table-radio-row {
          display: table-row;
          cursor: pointer;
        }
        .table-radio-row:hover {
          background-color: #f5f5f5;
        }
        .table-radio-row.active {
          background-color: #e8f0fe;
        }
        .radio-cell, .data-cell {
          display: table-cell;
          padding: 10px 15px;
          vertical-align: middle;
          border-bottom: 1px solid #ddd;
        }
        .radio-cell input {
          cursor: pointer;
        }
      ")),
      
      # JavaScript to handle row selection and update hidden input
      tags$script(HTML("
        $(document).ready(function() {
          // Initial state - mark active row
          updateActiveRow();
          
          // Click on row to select radio
          $('.table-radio-row').click(function() {
            $(this).find('input[type=\"radio\"]').prop('checked', true).trigger('change');
            updateActiveRow();
            Shiny.setInputValue('scenario_select', $(this).find('input[type=\"radio\"]').val());
          });
          
          // Click on radio also updates active class
          $('input[name=\"scenario_select\"]').change(function() {
            updateActiveRow();
            Shiny.setInputValue('scenario_select', $(this).val());
          });
          
          function updateActiveRow() {
            $('.table-radio-row').removeClass('active');
            $('input[name=\"scenario_select\"]:checked').closest('.table-radio-row').addClass('active');
          }
        });
      ")),
    )
  )
)

server <- function(input, output, session) {
  output$selected_scenario <- renderPrint({
    input$scenario_select
  })
}

shinyApp(ui, server)