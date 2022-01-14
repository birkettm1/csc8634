df.gpustats <- data.frame(
  variable = character(),
  n = integer(),
  min = integer(), 
  mean = integer(),
  max = integer(),
  sd = integer()
)
rownames <- c("variable", "n", "min", "mean", "max", "sd")

powerdraw <- data.frame("Power Draw", length(gpu$powerDrawWatt), min(gpu$powerDrawWatt), 
                        mean(gpu$powerDrawWatt), max(gpu$powerDrawWatt), sd(gpu$powerDrawWatt))
temp <- data.frame("Temperature", length(gpu$gpuTempC), min(gpu$gpuTempC), 
                   mean(gpu$gpuTempC), max(gpu$gpuTempC), sd(gpu$gpuTempC))
util <- data.frame("Utilised Percent", length(gpu$gpuUtilPerc), min(gpu$gpuUtilPerc), 
                   mean(gpu$gpuUtilPerc), max(gpu$gpuUtilPerc), sd(gpu$gpuUtilPerc))
mem <- data.frame("Memory Utilised Percent", length(gpu$gpuMemUtilPerc), min(gpu$gpuMemUtilPerc), 
                  mean(gpu$gpuMemUtilPerc), max(gpu$gpuMemUtilPerc), sd(gpu$gpuMemUtilPerc))

names(powerdraw) <- rownames
names(temp) <- rownames
names(util) <- rownames
names(mem) <- rownames

df.gpustats <- rbind(df.gpustats, powerdraw)
df.gpustats <- rbind(df.gpustats, temp)
df.gpustats <- rbind(df.gpustats, util)
df.gpustats <- rbind(df.gpustats, mem)
