#munge checkpoint execution times

#just get the total render
df.total_render = filter(application.checkpoints , eventName == "TotalRender")

#order by taskId
df.total_render = arrange(df.total_render, taskId, timestamp)

#create empty statistics df
df.execution <- data.frame(start=character(),
                           end=character(),
                           hostname = character(), 
                           jobId = character(), 
                           taskId = character(),
                           totalRenderTime = integer()) 

startTime <- NULL

#start to calculate execution and output row
for (row in 1:nrow(df.total_render)) {
#for (row in 1:10) {
  timestamp <- df.total_render[row, "timestamp"]
  eventtype  <- df.total_render[row, "eventType"]
  #print(timestamp)
  
  if (eventtype == 'START'){
    startTime <- parse_date_time(timestamp, orders="YmdHMS")
    
  } else {
    endTime <- parse_date_time(timestamp, orders="YmdHMS")
    totalTime = endTime - startTime
    #print(totalTime)
    
    #create a data frame
    df <- data.frame(startTime, endTime ,df.total_render[row, "hostname"], 
                     df.total_render[row, "jobId"],df.total_render[row, "taskId"], totalTime)
    
    #set the names
    names(df) <- c('start','end','hostname','jobId','taskId','totalRenderTime')
    
    #add to existing df
    df.execution <- rbind(df.execution, df)
  }
}

cache('df.execution')