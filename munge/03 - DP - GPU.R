summary(gpu)

gpu$starttime = sapply(gpu$timestamp, function(i){
  x = parse_date_time(i, orders="YmdHMS")
})

#cache('gpu')

matcheddatalist = list()

for (row in 1:nrow(df.longestexecutions)) {
#for (row in 1:3) {
  
  #get the hostname, start and end
  longhost <- df.longestexecutions[row, "hostname"]
  longstart <- df.longestexecutions[row, "start"]
  longend <- df.longestexecutions[row, "end"]
  longJobId <- df.longestexecutions[row, "jobId"]
  longTaskId <- df.longestexecutions[row, "taskId"]
  
  #get everything executed on that host in that timespan
  match <- filter(gpu, hostname == longhost & starttime >= longstart & starttime <= longend)
  
  #get the first executed job and assume thats the first task
  match <- arrange(match, timestamp) %>% filter(row_number()==1)
  match$hostLongestRenderTime <- df.longestexecutions[row, "totalRenderTime"]
  
  match$jobId = longJobId
  match$taskId = longTaskId
  
  #add to the list
  matcheddatalist[[row]] <- match
}

#merge the data frames
df.longestGPU <- do.call(rbind, matcheddatalist)

#cache('df.longestGPU')
#cache('task.x.y')

#create slim dataset
df.longestGPUSlim <- select(df.longestGPU, hostname, gpuSerial, hostLongestRenderTime)
#cache('df.longestGPUSlim')

#jobs by render time
#longest running job time = 78.751 seconds
df.jobsbyrendertime = distinct(df.longestGPUSlim %>%
                                 group_by(hostname) %>%
                                 arrange(desc(hostLongestRenderTime)), hostname, gpuSerial, hostLongestRenderTime)
cache('df.jobsbyrendertime')

