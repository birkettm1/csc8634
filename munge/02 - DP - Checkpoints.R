#munge checkpoint execution times
#this creates df.total_render, 
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

#dome some feature engineering
renderTimeSd <- sd(df.execution$totalRenderTime)
upperQuartileValue <- mean(df.execution$totalRenderTime) + renderTimeSd
lowerQuartileValue <- mean(df.execution$totalRenderTime) - renderTimeSd

#get the upper quartile long renders
df.longrender <- select(filter(df.execution, totalRenderTime > upperQuartileValue),start,end,hostname,jobId,taskId,totalRenderTime)

#count by hostname
df.longrendercount <- df.longrender %>% count(hostname)

#longest hosts
longrendercountmean <- mean(df.longrendercount$n)  
longrendercountsd <- sd(df.longrendercount$n)
longrenderupper <-longrendercountmean + longrendercountsd
df.longesthosts <- select(filter(df.longrendercount, n>=longrenderupper),hostname)

#complete the dataset for longest executions
df.longestexecutions <- merge(df.longesthosts, df.execution, by="hostname")
df.longestexecutions <- filter(df.longestexecutions, totalRenderTime > mean(df.longestexecutions$totalRenderTime) + sd(df.longestexecutions$totalRenderTime))

