#df.longestGPU to task.x.y

#add x and y to df.longestGPU using taskId
df.longestGPUGrid <- left_join(df.longestGPU, task.x.y, by = c("taskId" = "taskId"))
df.longestGPUGrid <- df.longestGPUGrid %>% unite("XY", x:y, sep= ",", remove = FALSE)