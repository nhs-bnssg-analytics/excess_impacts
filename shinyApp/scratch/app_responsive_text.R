library(shiny) 
library(ggplot2)
library(ggtext)
library(bslib)

ui <- page_fluid(
  title = "Responsive Text Size with Multiple Font Sizes",
  
  # Add CSS for responsive text sizing using CSS variables
  tags$head(
    tags$style("
      :root {
        --primary-font-size: 24px;
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
    "),
    
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
    ")
  ),
  
  # Invisible div to store viewport info as a backup method
  tags$div(
    id = "viewport-size-info",
    style = "display: none;",
    textOutput("viewport_size_text")
  ),
  
  # Text display container
  div(
    class = "position-relative border rounded p-4 bg-light",
    style = "min-height: 300px;",
    div(
      # Primary text
      div(
        class = "primary-text",
        HTML("This is primary text that scales <strong>based on the viewport size</strong>.")
      ),
      
      # Secondary text
      div(
        class = "secondary-text",
        HTML("This is secondary text in a smaller size. You can control both sizes independently with the sliders.")
      ),
      
      # Primary text again
      div(
        class = "primary-text",
        HTML("Back to primary text size. Try resizing your browser window to see how both text sizes adapt.")
      )
    )
  ),
  
  # Add controls
  card(
    card_header("Controls"),
    card_body(
      sliderInput("primaryTextSize", "Primary Text Size Multiplier:",
                  min = 0.5, max = 3, value = 1, step = 0.1),
      sliderInput("secondaryTextSize", "Secondary Text Size Multiplier:",
                  min = 0.5, max = 3, value = 0.7, step = 0.1),
      checkboxInput("debugInfo", "Show debug info", FALSE)
    )
  ),
  
  # Conditionally display debug information
  conditionalPanel(
    condition = "input.debugInfo",
    verbatimTextOutput("debugText")
  ),
  
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
  ")
)

server <- function(input, output, session) {
  # Default viewport dimensions
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
  
  # Provide viewport info for JavaScript as a backup method
  output$viewport_size_text <- renderText({
    paste0(viewportDims$width, "x", viewportDims$height)
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
    
    # Slider values
    cat("Slider values:\n")
    cat("Primary multiplier: ", input$primaryTextSize, "\n")
    cat("Secondary multiplier: ", input$secondaryTextSize, "\n")
  })
}

# Helper for NULL handling
`%||%` <- function(a, b) if (is.null(a)) b else a

shinyApp(ui, server)
