# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Multiple GPU node map rendering system, execution stats."),
  tags$p("This dashboard shows the performance of a cloud basid mapping solution. The solution was rendering using an Infrastructure as a Service (IaaS) cloud environment and up to 1024 graphical process unit (GPU) nodes which was used to compute a realistic visualisation of Newcastle Upon Tyne and its environmental data as captured by the Newcastle Urban Observatory."),
  tags$p("The data has been managed to show the host execution time, related to the grid being rendered and the ID of the GPU Card and to slim the size of the dataset to make it manageable, the top upper quartile of render time has been taken."),

  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      
      tags$h3("Filter:"),
      # Input: Level to be investigated
      selectInput(inputId = "level", label="Level", 
                  choices = unique(df.longestGPUGrid$level), 
                  selected=12),
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "RenderTime",
                  label = "Longest Render Time (secs):",
                  min = as.integer(min(df.longestGPUGrid$hostLongestRenderTime)),
                  max = as.integer(max(df.longestGPUGrid$hostLongestRenderTime)),
                  value = c(65), step = 1),
      
      #selectInput(inputId = "hostname", label="Host Name", choices = df$hostname),
      
      #selectInput(inputId = "gpuuid", label="GPU ID", choices = df$gpuUUID)
      
      tags$h4("Selected:"),
      span(textOutput("taskCount"), style="color:red"),
      span(textOutput("renderTime"), style="color:red")
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Histogram ----
      plotOutput(outputId = "hostPlot"),
      
      tags$h5("GPU UIDs used"),
      dataTableOutput("gpuList")
      
    )
  )
)