gpu$starttime = sapply(gpu$timestamp, function(i){
  x = parse_date_time(i, orders="YmdHMS")
})
summary(gpu)

matcheddatalist = list()

for (row in 1:nrow(df.longestexecutions)) {
#for (row in 1:1) {
  
  #get the hostname, start and end
  longhost <- df.longestexecutions[row, "hostname"]
  longstart <- df.longestexecutions[row, "start"]
  longend <- df.longestexecutions[row, "end"]
  longJobId <- df.longestexecutions[row, "jobId"]
  longTaskId <- df.longestexecutions[row, "taskId"]
  
  ##approx 1 second
  #print(longstart)
  #print(longend)
  
  #get everything executed on that host in that timespan
  match <- filter(gpu, hostname == longhost & starttime >= longstart & starttime <= longend)
  
  #proved there is only one card for this longest task
  #print(select(match, hostname, gpuSerial, gpuUUID, starttime))
  #print(unique(match$gpuSerial))
  #print(match)

  #get the first executed job and assume that's the first task 
  match <- arrange(match, desc(powerDrawWatt),desc(gpuTempC),desc(gpuUtilPerc),desc(gpuMemUtilPerc)) %>% filter(row_number()==1) #get the most "expensive" second in the running task
  match$hostLongestRenderTime <- df.longestexecutions[row, "totalRenderTime"]
  
  match$jobId = longJobId
  match$taskId = longTaskId
  
  #add to the list
  matcheddatalist[[row]] <- match
}

print(matcheddatalist)
#merge the data frames
df.longestGPU <- do.call(rbind, matcheddatalist)
select(filter(df.longestGPU, gpuSerial == '323617042956'), hostname, gpuSerial, gpuUUID)

#create slim dataset
df.longestGPUSlim <- select(df.longestGPU, hostname, gpuSerial, gpuUUID, hostLongestRenderTime)

#jobs by render time
#longest running job time = 78.751 seconds
df.jobsbyrendertime = distinct(df.longestGPUSlim %>%
                                 group_by(hostname) %>%
                                 arrange(desc(hostLongestRenderTime)), hostname, gpuSerial, hostLongestRenderTime)

