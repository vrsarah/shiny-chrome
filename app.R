# Install required packages if needed
# install.packages(c("shiny", "webshot2"))

library(shiny)
library(webshot2)

ui <- fluidPage(
  titlePanel("Website Screenshot Tool"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("url", "Enter URL:", value = "https://posit.co"),
      numericInput("width", "Viewport Width:", 1200, min = 300, max = 2000),
      numericInput("height", "Viewport Height:", 800, min = 300, max = 2000),
      selectInput("device", "Device Emulation:", 
                  choices = c("None", "iPhone", "iPad", "Pixel", "Galaxy")),
      actionButton("capture", "Take Screenshot", class = "btn-primary"),
      hr(),
      downloadButton("download", "Download Screenshot")
    ),
    
    mainPanel(
      imageOutput("screenshot", height = "600px")
    )
  )
)

server <- function(input, output, session) {
  
  # Create a reactive value to store screenshot path
  screenshot_path <- reactiveVal(NULL)
  
  observeEvent(input$capture, {
    req(input$url)
    
    # Create a temporary file for the screenshot
    temp_file <- tempfile(fileext = ".png")
    
    # Set device-specific options if selected
    if(input$device != "None") {
      device_opts <- switch(input$device,
                           "iPhone" = "iPhone X", 
                           "iPad" = "iPad", 
                           "Pixel" = "Pixel 2", 
                           "Galaxy" = "Galaxy S5")
      
      webshot(
        url = input$url,
        file = temp_file,
        delay = 2,
        vwidth = input$width,
        vheight = input$height,
        cliprect = "viewport",
        zoom = 1,
        quiet = TRUE,
        device = device_opts
      )
    } else {
      # Regular screenshot
      webshot(
        url = input$url,
        file = temp_file,
        delay = 2,
        vwidth = input$width,
        vheight = input$height,
        cliprect = "viewport",
        zoom = 1,
        quiet = TRUE
      )
    }
    
    # Save the file path
    screenshot_path(temp_file)
  })
  
  # Display the screenshot
  output$screenshot <- renderImage({
    req(screenshot_path())
    
    list(
      src = screenshot_path(),
      width = "100%",
      alt = paste("Screenshot of", input$url)
    )
  }, deleteFile = FALSE)
  
  # Enable download of the screenshot
  output$download <- downloadHandler(
    filename = function() {
      # Create a filename based on the URL
      paste0("screenshot-", gsub("[^a-zA-Z0-9]", "-", input$url), ".png")
    },
    content = function(file) {
      req(screenshot_path())
      file.copy(screenshot_path(), file)
    }
  )
}

shinyApp(ui, server)
