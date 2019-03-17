library(ggplot2)
library(reshape2) 
library(plotly)

data7 <-read.csv(file ='WHO_NREVSS_Public_Health_Labs.csv', header=T)
names(data7) <- c('Region Type','Region','Year','Week','Total tested','A (H1N1)pdm09','A (H3N2)','A(Subtyping not performed)','B (lineaege not performed)', 'B (Victoria lineage)', 'B (Yamagata lineage)' ,'H3N2v')
data7$yw <- paste(data7$Year,data7$Week,sep='-')
data7 <- data7[, -c(1,2,3,4,5)]
#names(data7)
data_melt <- melt(data7, id = c('yw'))
data_melt$variable <- factor(data_melt$variable, levels = c('A(Subtyping not performed)','A (H1N1)pdm09', 'A (H3N2)','H3N2v','B (lineaege not performed)', 'B (Victoria lineage)', 'B (Yamagata lineage)'))
chart7 <- ggplot(data = data_melt) +
  xlab('Week') +
  ylab('Number of positive specimens') +
  ggtitle("Influenza Positive Tests Reported to CDC by U.S. Public Health Laboratories, \nNational Summary, 2018-2019 Season") +
  geom_bar(mapping = aes(x = factor(yw),  y = value, fill = variable), color = 'black', stat = 'identity') +
  theme(legend.title = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x.top = element_line(size = 1),
        axis.text = element_text(angle = 90),
        axis.line.y = element_line(size =1),
        panel.background = element_rect(fill = 'white')) +
  
  scale_fill_manual(values = c('#F5F236', '#F29E06', '#FA0C05', '#992BFF', '#005533', '#99FF00', '#66D533'))
#ggplotly(chart7)
chart7
