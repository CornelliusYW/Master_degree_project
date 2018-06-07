countcogs <- function(cogcolumn){
    cogs <- unlist(lapply(cogcolumn, function(x) unique(unlist(lapply(x, function(y) strsplit(as.character(y), ""))))))
    data.frame(table(cogs))
}

# Example data
df <- data.frame(id = c("a1", "a2", "a3", "a4"), cog = c("J", "NU", "K", "RTKL"))

data_master <- read.csv("LFQ_master_regulation_second_batch_all_updated.csv")
data_master.stat <- subset(data_master, day2.day4.ratio < 1)
data_master.log <- subset(data_master, day2.day4.ratio > 1)
data_master.log$Regulation <- "log"
data_master.stat$Regulation <- "stat"

data_1 <- rbind(
  data_master.log,
  data_master.stat
)
data <- subset(data_1, p_value <0.05)

data_log <- subset(data, Regulation =='log')
data_stat <- subset(data, Regulation =='stat')


# Count COGs
data_count_log <- countcogs(data_log$func)
data_count_stat <- countcogs(data_stat$func)
data_count_log$Regulation <- 'log'
data_count_stat$Regulation <- 'stat'
data_log_stat <- rbind(data_count_log, data_count_stat)

write.csv(data_log_stat, file="LFQ_master_regulation_second_batch_count.csv")
