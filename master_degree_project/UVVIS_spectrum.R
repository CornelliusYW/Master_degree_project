
###Load packages (cowplot makes ggplots look nicer, reshape is for transforming data frames, ggplot2 is for plotting)
library("cowplot")
library("tidyr")
library("ggplot2")
library('reshape2')

###import file
UVVISmain <- read.csv("080218_UVVIS.csv")
row.names(UVVISmain) <-UVVISmain$Wavelength.nm.


UVVISmain <- subset(UVVISmain, UVVISmain$Wavelength.nm.> 349 )
###reshape data frame, so that the 4 columns containing the spectral data are merged into one column + addition of another column that contains the Day-IDs
test1 <- melt(UVVISmain, id.vars = "Wavelength.nm.")

###plot as line plot, colour by variable (here: Day)
ggplot(test1, aes(Wavelength.nm., value, colour = variable))+xlab('Wavelength [nm]') +ylab('Normalized absorbance') + geom_line() 

