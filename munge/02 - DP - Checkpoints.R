#munge checkpoint execution times

#just get the total render
df.total_render = filter(application.checkpoints , eventName == "TotalRender")
#order by taskId
df.total_render = arrange(df.total_render, taskId, timestamp)

#create empty statistics df
df.execution <- data.frame(timestamp=character(),
                           hostname = character(), 
                           jobId = character(), 
                           taskId = character(),
                           totalRenderTime = integer()) 


startTime <- NULL

#start to calculate execution and output row
for (row in 1:10) {
  timestamp <- df.total_render[row, "timestamp"]
  eventtype  <- df.total_render[row, "eventType"]
  #print(timestamp)
  
  if (eventtype == 'START'){
    startTime <- timestamp
    
  } else {
    endTime <- timestamp
    totalTime = parse_date_time(endTime, orders="YmdHMS") - parse_date_time(startTime, orders="YmdHMS")
    #print(totalTime)
    
    #create a data frame
    df <- data.frame(df.total_render[row, "timestamp"],df.total_render[row, "hostname"], 
                     df.total_render[row, "jobId"],df.total_render[row, "taskId"], totalTime)
    
    #set the names
    names(df) <- c('timestamp','hostname','jobId','taskId','totalRenderTime')
    
    #add to existing df
    df.execution <- rbind(df.execution, df)
  }
}

#start to calculate execution and output row
for (row in 1:nrow(df.total_render)) {
  timestamp <- df.total_render[row, "timestamp"]
  eventtype  <- df.total_render[row, "eventType"]
  #print(row)
  
  if (eventtype == 'START'){
    startTime <- timestamp
    
  } else {
    endTime <- timestamp
    
    if (!is.null(startTime)){
      totalTime = as.Date(endTime) - as.Date(startTime)
      print(totalTime)
      
      #create a data frame
      #df <- data.frame(df.total_render$timestamp,df.total_render$hostname, df.total_render$jobId,df.total_render$taskId,totalTime)
      
      #set the names
      #names(df) <- c('timestamp','hostname','jobId','taskId','totalRenderTime')
      
      #add to existing df
      #df.execution <- rbind(df.execution, df)
    }
  }
}

