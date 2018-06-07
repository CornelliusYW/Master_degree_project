###Import the nice plot
library(ggplot2)
library(cowplot)

###reading the data
data_master <- read.csv("LFQ_master_regulation_second_batch_count.csv")

###COG category general function annotation
data_master$General.Function <- with(data_master,
                            ifelse(data_master$func == "C" | data_master$func  == "E" | data_master$func  == "F" | data_master$func  == "G" | data_master$func  == "E"|data_master$func  == "I"|data_master$func  == "P"|data_master$func  == "Q"|  data_master$func  == "H" , 
                                   "Metabolism", 
                                      ifelse(data_master$func  == "A"| data_master$func  == "B"|data_master$func  == "J"|data_master$func  == "K"|data_master$func  == "L", "Information Storage & Processing",
                                      ifelse(data_master$func  == "D"|data_master$func  == "M"|data_master$func  == "N"|data_master$func  == "O"|data_master$func  == "T"|data_master$func  == "U"|data_master$func  == "V"|data_master$func  == "W" |data_master$func  == "Y"|data_master$func  == "Z", "Cellular Processing & Signalling",
                                    "Poor"))))

###COG category individual function annotation
data_master$Individual.Function <- with(data_master, ifelse(data_master$func =="D", "Cell cycle control, cell division, chromosome partitioning", ifelse(data_master$func =="M", "Cell wall/membrane/envelope biogenesis", ifelse(data_master$func == "N", "Cell motility", ifelse(data_master$func =="O", "Post-translational modification, protein turnover, and chaperones", ifelse(data_master$func =="T", "Signal transduction mechanism", ifelse(data_master$func =="U", "Intracellular trafficking, secretion, and vesicular transport", ifelse(data_master$func =="V", "Defense mechanism", ifelse(data_master$func =="W", "Extracellular structures", ifelse(data_master$func =="Y", "Nuclear structure", ifelse(data_master$func =='Z', "Cytoskeleton",
                                                         ifelse(data_master$func =="A", "RNA processing and modification", ifelse(data_master$func =="B", "Chromatin structure and dynamics", ifelse(data_master$func =="J", "Translation, ribosomal structure and biogenesis", ifelse(data_master$func =="K", "Transcription", ifelse(data_master$func =="L", "Replication, recombination and repair",
                                                         ifelse(data_master$func =="C", "Energy production and conversion", ifelse(data_master$func =="E", "Amino acid transport and metabolism", ifelse(data_master$func =="F","Nucleotide transport and metabolism", ifelse(data_master$func =="G", "Carbohydate transport and metabolism", ifelse(data_master$func =="H", "Coenzyme transport and metabolism", ifelse(data_master$func =="I", "Lipid transport and metabolism", ifelse(data_master$func =="P", "Inoragnic ion transport and metabolism", ifelse(data_master$func =="Q", "Secondary metabolites biosynthesis, transport, and catabolism", ifelse(data_master$func =="R", "General function prediction only", "function unknown"
                                                         )))))))))))))))))))))))))

###Data aggregation for COG category general function
data_master <- data_master[order(data_master$General.Function),]
data_log_stat_count <- aggregate(Freq ~  Regulation + General.Function, data_master, sum)


###Create plot
plot.individual <- ggplot(data_log_stat, aes(x= individual.function, y=Freq,  fill = Regulation), show.legend = TRUE)+
  geom_col(stat='identity', position = position_dodge())+ylab("Count")+
  xlab("Individual Function")+theme(
    legend.position = 'top',
    axis.text.y = element_text(size = 7),
    axis.text.x = element_text(size = 7),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    legend.text = element_text(size = 8),
    legend.title =  element_text(size = 8, face = 'bold'))

plot.individual +facet_grid (General.Function~., scales = 'free', space = 'free',
  labeller = label_wrap_gen(width = .1))+
   theme(strip.text.y = element_text(size = 6), 
        strip.background = element_blank()) + coord_flip()

###Store the plot into pdf
ggsave("Individual_Function_plot_Timmobilis.pdf")
dev.off()
