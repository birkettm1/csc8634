# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Multiple GPU node map rendering system, execution stats."),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Level to be investigated
      selectInput(inputId = "level", label="Level", 
                  choices = unique(df.longestGPUGrid$level), 
                  selected=12),
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "RenderTime",
                  label = "Longest Render Time:",
                  min = as.double(min(df.longestGPUGrid$hostLongestRenderTime)),
                  max = as.double(max(df.longestGPUGrid$hostLongestRenderTime)),
                  value = c(65), step = 1)
      
      #selectInput(inputId = "hostname", label="Host Name", choices = df$hostname),
      
      #selectInput(inputId = "gpuuid", label="GPU ID", choices = df$gpuUUID)
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(

      span(textOutput("taskCount"), style="color:red"),
      span(textOutput("renderTime"), style="color:red"),

      # Output: Histogram ----
      plotOutput(outputId = "hostPlot"),
      dataTableOutput("gpuList")
      
    )
  )
)