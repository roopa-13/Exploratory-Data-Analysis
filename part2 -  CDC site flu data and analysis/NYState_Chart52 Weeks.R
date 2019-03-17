library(ggplot2)
library(reshape2) 

data1 <-read.csv("WHO_NREVSS_Clinical_Labs_NY.csv", header=T)
names(data1)<- c("Region Type","Region","Year","Week","TotalTested", "A","B","PercentPositive","PercentPositiveA","PercentPositiveB")

data1$wy <- paste(data1$Year, data1$Week, sep = '-')
#names(data1)
#head(data1$wy)

bar_data <- data1[,c(6,7,11)]
melt_bar <- melt(bar_data, id = c('wy'))
melt_bar <- melt_bar[! is.na(melt_bar$value) ,]
#names(melt_bar)

line_data <- data1[,c(8,9,10,11)]
melt_line <- melt(line_data, id = c('wy'))
#names(melt_line)
melt_line$variable <-  factor(melt_line$variable,levels = c("PercentPositive", "PercentPositiveA", "PercentPositiveB"))

chart8 <- ggplot() +
  geom_bar(data = melt_bar, aes(x = factor(wy), y = value, fill = variable), color = 'black', stat = 'identity') +
  scale_fill_manual(values = c('yellow', 'darkgreen')) +
  xlab('Week') +
  ylab('Number of positive specimens') +
  geom_line(data = melt_line, aes(x = factor(wy), y = value*400, color = variable, group = variable, linetype = variable)) +
  scale_y_continuous(sec.axis = sec_axis(~.*(1/400), name = "Percent Positive", breaks = seq(0,35,5)), breaks = seq(0,14000,2000), limits = c(0,14000)) +
  theme(legend.title = element_blank(),
        axis.ticks.x = element_blank(), 
        axis.line.x.top = element_line(size = 1),
        axis.line.y = element_line(size=1),
        axis.text.x = element_text(angle=90),
        panel.background = element_rect(fill = 'white')) +
  scale_color_manual(values = c("black", "orange", "green")) +
  #scale_linetype_manual(values = c("solid", "longdashed", "dashed")) +
  ggtitle("Influenza Positive Tests Reported to CDC by U.S. Clinical Laboratories, \nNew York, 2018-2019 Season")

chart8
