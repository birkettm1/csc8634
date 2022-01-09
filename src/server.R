server <- function(input, output, session) {
  # assign values to `output` here
  
  output$hostPlot <- renderPlot({
    
    df <- filter(df.longestGPUGrid, hostLongestRenderTime >= input$RenderTime)
    
    # Can also set the label and select items
    updateSelectInput(session, "hostname", choices = df$hostname)
    updateSelectInput(session, "gpuuid", choices = df)
    
    ggplot(df, aes(hostname, hostLongestRenderTime)) + 
      geom_point() +
      labs(title="Host by Render Time", y="Render Time", x = "Hostname") +
      scale_x_discrete(label=function(x) substring(x,nchar(x)-5,nchar(x))) +
      theme_bw() + 
      scale_fill_brewer(palette="PuBu") +
      theme(axis.text.x = element_text(angle = 90))
  })
  
  output$gpuPlot <- renderPlot({
    
    df <- filter(df.longestGPUGrid, hostLongestRenderTime >= input$RenderTime)
    
    ggplot(df, aes(gpuUUID, hostLongestRenderTime)) + 
      geom_col() +
      labs(title="GPU by Render Time", y="Render Time", x = "GPU UID") +
      scale_x_discrete(label=function(x) substring(x,nchar(x)-5,nchar(x))) +
      theme_bw() + 
      scale_fill_brewer(palette="PuBu") +
      theme(axis.text.x = element_text(angle = 90))
  })
  
  
}