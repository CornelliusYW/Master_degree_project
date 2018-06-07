##Clear workspace
rm(list=ls())

#Load packages
library(readr)
library(ggplot2)
library(gridExtra)
require(cowplot)
require(scales)
require(reshape2)

###Load data files
MBLW1 <- read.csv("LFQ_day2_day4_first_replicate.csv")

###select for differentially expressed proteins (log and stat)
MBLW1.stat <- subset(MBLW1, day2.day4.ratio < 1)
MBLW1.log <- subset(MBLW1, day2.day4.ratio > 1)

##add new column to distinguish between "log" and "stat" proteins
MBLW1.log$regulation <- "log"
MBLW1.stat$regulation <- "stat"

###merge datafiles column by row
LAB.rbind <- rbind(
  MBLW1.log,
  MBLW1.stat
)

###select proteins with at least 2-fold up- and downregulation
LABquant <- subset(LAB.rbind, day2.day4.ratio < 0.5 | day2.day4.ratio > 2.0)

###select protein with p-value <0.05 
LABquant <- subset(LAB.rbind, p_value <0.05)

#Create custom theme
mytheme <-
  theme(legend.position = "top",
        legend.title=element_blank(),
        legend.text=element_text(size = 7),
        legend.key.size = unit(0.3, "cm"),
        plot.title = element_text(size=9),
        axis.title = element_text(size=9),
        axis.text = element_text(size=8)
  )

###define custom facet theme
facet_theme <- theme(strip.text.x = element_text(size=10), ## change text appearance for facet labels x-axis
                     strip.text.y = element_text(size=10), ## change text appearance for facet labels y-axis
                     strip.background = element_blank())
###create output file as pdf
pdf("Volcano_plot.pdf", height = 10/2.54, width = 10/2.54)

###plot volcano plot
p1 <- ggplot(data=data.read, 
             aes(x=log2(day2.day4.ratio), y =-log10(p_value), colour = Regulation
             ))  +
      geom_point(alpha=1, size=1.5) +
      xlab("log2 fold change (log vs. stat)") +
      ylab("-log10 p-value") +
      scale_fill_brewer() +
      mytheme +
      facet_theme +  geom_vline(aes(xintercept=0), colour="gray", linetype="dashed")


dev.off()

