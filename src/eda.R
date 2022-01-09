library('ProjectTemplate')
load.project()
#clear.cache()

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
#qq, coefficient of variation, confidence interval, standard deviation
summary(gpu)
gpu <- na.omit(gpu) 

#create stats - moved to munge - DU - GPU

#stats
df.gpustats
ci.mean(gpu)

#stats plot
ggplot(df.gpustats, aes(x=variable, y=mean)) + 
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2) +
  geom_line() +
  geom_point() +
  labs(title="Mean and Standard Deviation of GPU Stats", y="N", x="Variable Name") + 
  theme_bw() + 
  scale_fill_brewer(palette="PuBu")


#continuous plots
df.powerdraw <- gpu %>% count(powerDrawWatt)
plot.continuous(df.powerdraw, "powerDrawWatt")
plot.qq(gpu, "powerDrawWatt") #qq - normalised?

df.tempc <- gpu %>% count(gpuTempC)
plot.continuous(df.tempc, "gpuTempC")
plot.qq(gpu, "gpuTempC") #qq

df.utilisedpc = gpu %>% count(gpuUtilPerc)
plot.continuous(df.utilisedpc, "gpuUtilPerc")
plot.qq(gpu, "gpuUtilPerc") #qq

df.memutilisedpc = gpu %>% count(gpuMemUtilPerc)
plot.continuous(df.memutilisedpc, "gpuMemUtilPerc")
plot.qq(gpu, "gpuMemUtilPerc") #qq




## Data Preparation 
\textcolor{red}{(select, clean, construct, integrate, format)}
\textcolor{red}{What, concisely, did you do?}

#df.execution <- cache('df.execution')
#clear.cache('df.execution')
library('ProjectTemplate')
load.project()

summary(df.execution)
head(df.execution) #got the execution time
df.execution %>% count(hostname)

#summary
length(df.execution$totalRenderTime)
min(df.execution$totalRenderTime) 
max(df.execution$totalRenderTime)
mean(df.execution$totalRenderTime)
renderTimeSd <- sd(df.execution$totalRenderTime)
upperQuartileValue <- mean(df.execution$totalRenderTime) + renderTimeSd
lowerQuartileValue <- mean(df.execution$totalRenderTime) - renderTimeSd

#plot the distribution
ggplot(df.execution, aes(x=totalRenderTime)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=1,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +  # Overlay with transparent density plot
  geom_vline(aes(xintercept=mean(totalRenderTime, na.rm=T)),   # Ignore NA values for mean
           color="red", linetype="solid", size=1) + 
  geom_vline(aes(xintercept=mean(totalRenderTime + renderTimeSd, na.rm=T)),   # Ignore NA values for upper sd
             color="red", linetype="dashed", size=1) + 
  geom_vline(aes(xintercept=mean(totalRenderTime - renderTimeSd, na.rm=T)),   # Ignore NA values for lowersd
             color="red", linetype="dashed", size=1) + 
  labs(title="Distribution of Total Render Time in 1 Second Bins", y="Density", x="Render Time in Seconds") + 
  theme_bw() + 
  scale_fill_brewer(palette="PuBu")

#get the upper quartile
filter(df.execution, totalRenderTime > upperQuartileValue)

#get the 95th quartile
ninequantile <- quantile(df.execution$totalRenderTime, .95) 

#get the long render records
df.longrender <- select(filter(df.execution, totalRenderTime > upperQuartileValue),start,end,hostname,jobId,taskId,totalRenderTime)
summary(df.longrender)

#count by hostname
df.longrendercount <- df.longrender %>% count(hostname)
max(df.longrendercount$n)  
min(df.longrendercount$n)  
longrendercountmean <- mean(df.longrendercount$n)  
longrendercountsd <- sd(df.longrendercount$n)
longrenderupper <-longrendercountmean + longrendercountsd
df.longesthosts <- select(filter(df.longrendercount, n>=longrenderupper),hostname)

#get the execution for the longest hosts
df.longestexecutions <- merge(df.longesthosts, df.execution, by="hostname")
summary(df.longestexecutions)
head(df.longestexecutions)

#get the top longest tasks
df.longestexecutions <- filter(df.longestexecutions, totalRenderTime > mean(df.longestexecutions$totalRenderTime) + sd(df.longestexecutions$totalRenderTime))
summary(df.longestexecutions)
arrange(df.longestexecutions, desc(totalRenderTime))
df.longestexecutions %>% count(jobId)
df.longestexecutions %>% count(taskId)
df.longestexecutions %>% count(hostname)

ggplot(data=df.longestexecutions, aes(x=taskId, y=totalRenderTime, group=1)) +
  geom_line() +
  geom_point() #+ 
  #ylim(c(0,100))

#jobId
ggplot(df.longestexecutions, aes(x=jobId, y=totalRenderTime)) + 
  geom_boxplot()  + 
  theme(axis.text.x = element_text(angle = 90))

#taskId
ggplot(df.longestexecutions, aes(x=taskId, y=totalRenderTime)) + 
  geom_boxplot()  + 
  theme(axis.text.x = element_text(angle = 90))

#need to integrate gpu on hostname and time
library('ProjectTemplate')
load.project()

#takes ages so get from cache
gpu <- cache('gpu')
task.x.y <- cache('task.x.y')
df.gpustats <- cache('df.gpustats')
df.execution <- cache('df.execution')
df.longestexecutions <- cache('df.longestexecutions')
df.longestGPU <- cache('df.longestGPU')
df.longestGPUSlim <- cache('df.longestGPUSlim')
df.jobsbyrendertime <- cache('df.jobsbyrendertime')

summary(df.longestGPU)
head(df.longestGPU)

#get slimmer dataset
df.longestGPUSlim <- select(df.longestGPU, hostname, gpuSerial, gpuUUID, hostLongestRenderTime)
arrange(df.longestGPUSlim, desc(hostLongestRenderTime))
unique(df.longestGPUSlim$gpuSerial)
unique(df.longestGPUSlim$hostname)
summary(df.longestGPUSlim)

#do some counts
arrange(df.longestGPUSlim %>% count(hostname), desc(n))
plot.continuous(df.longestGPUSlim %>% count(hostname), "hostname")

#plot the occurrence of hosts
ggplot(data=df.longestGPUSlim %>% count(hostname), aes(as.character(hostname), y=n, group=1)) +
  geom_line() +
  labs(title="Number of occurances of hostname in the top percentile longest running tasks", 
       y="Count", x = "Hostname") 
  theme_bw() + 
  scale_fill_brewer(palette="PuBu") +
  theme(axis.text.x = element_text(angle = 90))

arrange(df.longestGPUSlim %>% count(gpuSerial), desc(n))
plot.continuous(df.longestGPUSlim %>% count(gpuSerial), "gpuSerial")

#plot the occurrence of gpus
ggplot(data=df.longestGPUSlim %>% count(gpuSerial), aes(as.character(gpuSerial), y=n, group=1)) +
  geom_line() +
  labs(title="Number of occurances of GPU in the top percentile longest running tasks", 
       y="Count", x = "GPU Serial") +
  theme_bw() + 
  scale_fill_brewer(palette="PuBu") +
  theme(axis.text.x = element_text(angle = 90))

#plot the occurrence of gpus
ggplot(data=df.longestGPUSlim %>% count(gpuUUID), aes(as.character(gpuUUID), y=n, group=1)) +
  geom_line() +
  labs(title="Number of occurances of GPU in the top percentile longest running tasks", 
       y="Count", x = "GPU Serial") +
  theme_bw() + 
  scale_fill_brewer(palette="PuBu") +
  theme(axis.text.x = element_text(angle = 90))

#longest running host
#longest running job time = 78.751 seconds
df.jobsbyrendertime = distinct(df.longestGPUSlim %>%
                                 group_by(hostname) %>%
                                 arrange(desc(hostLongestRenderTime)), hostname, gpuSerial, hostLongestRenderTime)

longestHostname = df.jobsbyrendertime[1,1]
longestGPUSerial = df.jobsbyrendertime[1,2]
longestHostTime = df.jobsbyrendertime[1,3]

#hostname 95b4ae6d890e4c46986d91d7ac4bf08200000G 
#gpuserial 320118119842 
#running time 78.751 sec

