# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Hello Shiny!"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "RenderTime",
                  label = "Longest Render Time:",
                  min = as.double(min(df.longestGPUGrid$hostLongestRenderTime)),
                  max = as.double(max(df.longestGPUGrid$hostLongestRenderTime)),
                  value = 52.118999, step = 1),
      
      selectInput(inputId = "hostname", label="Host Name", choices = df$hostname),
      
      selectInput(inputId = "gpuuid", label="GPU ID", choices = df$gpuUUID)
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "hostPlot"),
      plotOutput(outputId = "gpuPlot")
      
    )
  )
)