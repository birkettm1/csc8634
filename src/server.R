server <- function(input, output, session) {
  # assign values to `output` here
  output$hostPlot <- renderPlot({
    
    filterJobId <- unique(select(filter(df.longestGPUGrid, level==input$level), jobId.x))
    df <- filter(df.longestGPUGrid, jobId.x == filterJobId) #filter by level
    
    #get the count
    taskCount <- df %>% count()
    output$taskCount <- renderText({ paste("Number of tasks at level ", input$level, ": ", taskCount) })
    output$renderTime <- renderText({})
    
    if (taskCount > 0){
      #filter on render time
      df <- filter(df.longestGPUGrid, hostLongestRenderTime >= input$RenderTime) #filter by render time
      output$renderTime <- renderText({ paste(paste("Number of tasks above a render time of ", input$RenderTime, " seconds: "), df %>% count()) })
      
      # Can also set the label and select items
      #updateSelectInput(session, "hostname", choices = df$hostname)
      #updateSelectInput(session, "gpuuid", choices = df$gpuUUID)
      
      #output grid to render time
      ggplot(df, aes(XY, hostLongestRenderTime)) + 
        geom_point() +
        labs(y="Render Time", x = "Grid") +
        theme_bw() + 
        scale_fill_brewer(palette="PuBu") +
        theme(axis.text.x = element_text(angle = 90))
      

    }
  })
  
  #output list of gpuuid
  output$gpuList <- renderDataTable({
    filterJobId <- unique(select(filter(df.longestGPUGrid, level==input$level), jobId.x))
    df <- filter(df.longestGPUGrid, jobId.x == filterJobId) #filter by level
    df <- filter(df.longestGPUGrid, hostLongestRenderTime >= input$RenderTime) #filter by render time
    
    arrange(unique(select(df, hostname, gpuUUID, XY, hostLongestRenderTime)), desc(hostLongestRenderTime))
  })
  
  
}