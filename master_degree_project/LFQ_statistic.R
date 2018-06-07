LFQdata <- read.csv('Max_Quant_MS-18-012b.csv')

#Subset the necesseary data
LFQfilter <- subset(LFQdata, select = c(Majority.protein.IDs,Mol.Weight,LFQ_intensity_1_1, LFQ_intensity_2:LFQ_intensity_6)) 

#Subset for the day 2 exclusive protein with LFQ intensity only found in log phase (all 3 replicates
LFQday2specific <- subset(LFQfilter,LFQ_intensity_1_1>0 & LFQ_intensity_2 > 0 & LFQ_intensity_3 > 0 &
                  LFQ_intensity_4 == 0 & LFQ_intensity_5 == 0 & LFQ_intensity_6 == 0)

#Subset for the day 4 exclusive protein with LFQ intensity only found in stationary phase (all 3 replicates)
LFQday4specific <- subset(LFQfilter,LFQ_intensity_1_1 == 0 & LFQ_intensity_2 == 0 & LFQ_intensity_3 == 0 &
                  LFQ_intensity_4 > 0 & LFQ_intensity_5 > 0 & LFQ_intensity_6 > 0)

#Subset for data with LFQ intensity in both growth phase and all replicates
LFQboth <- subset(LFQfilter,LFQ_intensity_1_1 > 0 & LFQ_intensity_2 > 0 & LFQ_intensity_3 > 0 &
                  LFQ_intensity_4 > 0 & LFQ_intensity_5> 0 & LFQ_intensity_6 > 0)

#Fold change of the protein 
LFQboth$day2.mean <- rowMeans(LFQboth[,3:5])
LFQboth$day4.mean <- rowMeans(LFQboth[,6:8])
LFQboth$day2.day4.ratio <- LFQboth$day2.mean/LFQboth$day4.mean

#Statitistical analysis for the fold change (LFQ-intensity found in both phases)
t.result <- apply(LFQboth[,3:8], 1, function (x) t.test(x[1:3],x[4:6], var.equal = TRUE))
LFQboth$p_value <- unlist(lapply(t.result, function(x) x$p.value))

write.csv(LFQday4specific, file = 'LFQ_day4specific_first_replicate.csv')
