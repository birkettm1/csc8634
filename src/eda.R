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


## Data Understanding 
(collect initial data, describe, explore, data quality)


#checkpoints data investigation
df.examine(application.checkpoints)
df.headtail(application.checkpoints)
df.view(application.checkpoints)

#counts000b158b-0ba3-4dca-bf5b-1b3bd5c28207
df.taskcount <- application.checkpoints %>% count(taskId) #id of the azure batch task
summary(df.taskcount)
filter(df.taskcount, n==10)
filter(df.taskcount, n>11)
select(filter(application.checkpoints, taskId == '000b158b-0ba3-4dca-bf5b-1b3bd5c28207'), jobId, taskId)
select(filter(application.checkpoints, taskId == '0052c4f2-9b51-4063-86da-bc09db2f2029'), jobId, taskId)

#all tasks guids = 10
df.jobcount <- application.checkpoints %>% count(jobId) #id of azure batch job
summary(df.jobcount)

#lots of jobId = 1024-lvl12-7e026be3-5fd0-48ee-b7d1-abd61f747705
df.hostcount <- application.checkpoints %>% count(hostname) #hostname of the virtual machine auto-assigned by the Azure batch system.
summary(df.hostcount)
filter(df.hostcount, n>700)

#host use
ggplot(data=df.hostcount, aes(x=hostname, y=n, group=1)) +
  geom_line() +
  labs(title="Use of Hosts", y="Count", x = "Host") + 
  theme_bw() + 
  scale_fill_brewer(palette="PuBu")

#somewhere in the region of each individual hostname
application.checkpoints %>% count(eventName)

#all records have a complete set of events - 132080
application.checkpoints %>% count(eventType)
# all start events have a stop event - 330200 of each

#investigate GPU data
df.examine(gpu)
summary(gpu)
summary(gpu$gpuSerial)
df.headtail(gpu)
nrow(gpu)

#gpu cards
df.serials <- gpu %>% count(gpuSerial)

#card use - all cards used 1500 times ish apart from 5 outliers
filter(df.serials, n>2500)
select(
  filter(gpu, gpuSerial %in% c(320118119713,323617020221,325017019048,325217085205,325217086299))
  , hostname, gpuSerial, gpuUUID)
plot.continuous(df.serials, "gpuSerial")

#continuous variables
gpu <- na.omit(gpu) 
df.powerdraw <- gpu %>% count(powerDrawWatt)
plot.continuous(df.powerdraw, "powerDrawWatt")

df.tempc <- gpu %>% count(gpuTempC)
plot.continuous(df.tempc, "gpuTempC")

df.utilisedpc = gpu %>% count(gpuUtilPerc)
plot.continuous(df.utilisedpc, "gpuUtilPerc")

df.memutilisedpc = gpu %>% count(gpuMemUtilPerc)
plot.continuous(df.memutilisedpc, "gpuMemUtilPerc")

#qq
ggplot(gpu, aes(sample=gpuTempC)) +
  stat_qq()




## Data Preparation 
\textcolor{red}{(select, clean, construct, integrate, format)}
\textcolor{red}{What, concisely, did you do?}

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