library('ProjectTemplate')
load.project()

for (dataset in project.info$data)
{
  message(paste('Showing top 5 rows of', dataset))
  print(head(get(dataset)))
}


#This file contains application checkpoint events throughout the execution of the render job
examine.df(application.checkpoints) 
#This file contains the x,y co-ordinates of which part the image was being rendered for each task.
examine.df(task.x.y) 
#This file contains metrics that were output regarding the status of the GPU on the virtual machine.
examine.df(gpu)


#checkpoints data investigation
df.examine(application.checkpoints)
df.headtail(application.checkpoints)
df.view(application.checkpoints)

application.checkpoints %>% count(taskId)
#all tasks guids = 10
application.checkpoints %>% count(jobId)
#lots of jobId = 1024-lvl12-7e026be3-5fd0-48ee-b7d1-abd61f747705
application.checkpoints %>% count(hostname)
#somewhere in the region of each individual hostname

#just get the total render
df.total_render = filter(application.checkpoints , eventName == "TotalRender")
#order by taskId
df.total_render = arrange(df.total_render, taskId, timestamp)
#create empty statistics vector
df.execution <- data.frame(timestamp=character(),
                 hostname = character(), 
                 jobId = character(), 
                 taskId = character(),
                 totalRenderTime = character()) 

startTime <- NULL

#start to calculate execution and output row
for (row in 1:nrow(df.total_render)) {
  timestamp <- df.total_render[row, "timestamp"]
  #eventtype  <- df.total_render[row, "eventType"]
  #print(row)
  
  if (eventtype == 'START'){
    startTime <-  as.POSIXlt(timestamp)

  } else {
    endTime <-  as.POSIXlt(timestamp)
    
    if (!is.null(startTime)){
      totalTime = endTime - startTime
      
      df.execution %>% add_row(timestamp = df.total_render$timestamp,
                               hostname = df.total_render$hostname,
                               jobId = df.total_render$jobId,
                               taskId = df.total_render$taskId,
                               totalRenderTime = totalTime)
    }
  }
}