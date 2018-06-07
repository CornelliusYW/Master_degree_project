
library(growthcurver)
file <- read.csv("measure2.csv")
par(mfcol = c(5,1))
gc_out <- SummarizeGrowthByPlate(file, plot_fit = TRUE, plot_file = "gc_plots.pdf")
file_OD <- subset(file, select = c('time','OD1', 'OD2','OD3'))
file$mean <- rowMeans(file[,2:4])

gc_fit1 <- SummarizeGrowth(file$time, file$mean)
gc_fit2 <- SummarizeGrowth(file$time, file$OD2)
gc_fit3 <- SummarizeGrowth(file$time, file$OD3)
gc_fit4 <- SummarizeGrowth(file$ï..time, file$OD4)
gc_fit5 <- SummarizeGrowth(file$ï..time, file$OD5)

attach(mtcars)
par(mfrow=c(3,1))
plot(gc_fit1, main = 'T. immobilis Manual Measurement', xlab ='Minute (m)', ylab ='OD(??=600 nm)')
plot(gc_fit2, main = 'T. immobilis R2', xlab ='Minute (m)', ylab ='OD(??=600 nm)')
plot(gc_fit3, main = 'T. immobilis R3', xlab ='Minute (m)', ylab ='OD(??=600 nm)')
plot(gc_fit4, main = 'T. immobilis R4', xlab ='Hour (h)', ylab ='OD(??=600 nm)')
plot(gc_fit5, main = 'T. immobilis R5', xlab ='Hour (h)', ylab ='OD(??=600 nm)')
par(old.par)

