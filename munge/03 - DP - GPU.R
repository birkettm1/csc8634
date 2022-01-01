summary(gpu)

gpu$starttime = sapply(gpu$timestamp, function(i){
  x = parse_date_time(i, orders="YmdHMS")
})

matcheddatalist = list()

for (row in 1:nrow(df.longestexecutions)) {
#for (row in 1:3) {
  
  #get the hostname, start and end
  longhost <- df.longestexecutions[row, "hostname"]
  longstart <- df.longestexecutions[row, "start"]
  longend <- df.longestexecutions[row, "end"]
  
  #get everything executed on that host in that timespan
  match <- filter(gpu, hostname == longhost & starttime >= longstart & starttime <= longend)
  
  #get the first executed job and assume thats the first task
  match <- arrange(match, timestamp) %>% filter(row_number()==1)
  match$totalRenderTime = longhost <- df.longestexecutions[row, "totalRenderTime"]
  
  #add to the list
  matcheddatalist[[row]] <- match
}

#merge the data frames
df.longestGPU <- do.call(rbind, matcheddatalist)
