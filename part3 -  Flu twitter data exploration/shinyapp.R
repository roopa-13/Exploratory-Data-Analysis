#install.packages("shiny")
#install.packages("shinythemes") 
#install.packages("readr")
#install.packages("ggplot2")
#install.packages("shinyBS")
#install.packages("plotly")
library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(ggplot2)
library(shinyBS)
library(usmap)
library(gridExtra)

#install.packages("devtools")
#devtools::install_github("wmurphyrd/fiftystater")

library(plotly)
library(fiftystater)

df<-read.csv('cdcshiny.csv')
twitter_data<-read.csv('totaltweets_processed.csv')
hn_data<-read.csv('hn_processed.csv')
flu_data<-read.csv('flu_processed.csv')


ui <- bootstrapPage(
  titlePanel("Influenza Data Analysis"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(inputId = "filter", label = strong("Filters"),
                  choices = c('CDC Heatmap','Total Tweets','Flu','H1N1','CDC Heatmap VS Twitter Heatmap','CDC Heatmap VS Limited Tweet Chart'),
                  selected = "CDC Heatmap")  , width=3    
    ),
    
    mainPanel(
      plotOutput(outputId = "tweetdata"),textOutput(outputId = 'text'),plotlyOutput(outputId = "heatmap")
      
    )
  )
)

server <- function(input, output) {
  
   output$heatmap <- renderPlotly({
      
  })
  
   output$text <- renderText({
     if(input$filter=='CDC Heatmap'){
       print("The activity levels in the Heat Map are based on the percent of outpatient visits from each state due to ILI and are compared to the average percent of ILI visits that occur during weeks with little or no influenza virus circulation. 
             Activity levels range from minimal, to average and to high, which corresponds to ILI activity from outpatient clinics being much higher than average.
             Data were insufficient to calculate an ILI activity level from the U.S. Virgin Islands. There is no replicated representation of New York City, Alaska, Hawaii, Puerto Rico and district of Columbia.")
     }
     else if(input$filter=='Flu'){
       print("This is a heatmap (intensity) chart plotted with respect to the tweet count collected for the keyword 'flu'; plotted corresponding to each US state.")
     }
     else if(input$filter=='H1N1'){
       print("This is a intensity chart plotted with respect to the tweet count collected for the keyword 'H1N1'; plotted corresponding to each US state.")
     }
     else if(input$filter=='Total Tweets'){
       print("This is a intensity chart plotted with respect to the tweet count, from the total number of tweets collected, plotted corresponding to each US state.")
     }
     else if(input$filter=='CDC Heatmap VS Twitter Heatmap'){
       print("We display the heatmap generated for the CDC data and the total collected tweets against each other. The graphs illustrate the intensity of the tweet count from each of the corresponding states. With respect to the Twitter Data HeatChart, the intensity goes lighter with increase in count whereas for the CDC chart, darker the color greater the count. Twitter Chart shows light blue for the state of California depicting that it contains the largest number of tweets i.e about 215; a slightly darker shade of blue for New York with about 150 tweets. With respect to the CDC graph, the red shades potray the greater count.")
     }
     else if(input$filter=='CDC Heatmap VS Limited Tweet Chart'){
       print("We display the heatmap generated for the CDC data and the tweets collected for a specific keyword against each other. Similar to the previous depiction, for keyword DataChart, the intensity goes lighter with increase in count whereas for the CDC chart, darker the color greater the count. Keyword-Specific Chart shows light blue for the state of California depicting that it contains the largest number of tweets; a slightly darker shade of blue for the state of New York, Texas. With respect to the CDC graph, the red shades potray the greater count.") 
     }
   })
   
  output$tweetdata<-renderPlot({
    
    total_plt <- plot_usmap(data = twitter_data, values = "Freq", lines = "black") + 
      scale_fill_continuous(name = "Freq", label = scales::comma) + 
      theme(legend.position = "right",
            plot.title = element_text(hjust = 0.5, face="bold", size=16)) + 
      ggtitle("Twitter Data HeatChart") 
  
    flu_plt <- plot_usmap(data = flu_data, values = "Freq", lines = "black") + 
      scale_fill_continuous(name = "Freq", label = scales::comma) + 
      theme(legend.position = "right",
            plot.title = element_text(hjust = 0.5, face="bold", size=16)) + 
      ggtitle("HeatMap for Keyword FLU") 
    
    l_plt <- plot_usmap(data = flu_data, values = "Freq", lines = "black") + 
      scale_fill_continuous(name = "Freq", label = scales::comma) + 
      theme(legend.position = "right",
            plot.title = element_text(hjust = 0.5, face="bold", size=16)) + 
      ggtitle("HeatMap for Limited Tweets") 
    
    hn_plt <- plot_usmap(data = hn_data, values = "Freq", lines = "black") + 
      scale_fill_continuous(name = "Freq", label = scales::comma) + 
      theme(legend.position = "right",
            plot.title = element_text(hjust = 0.5, face="bold", size=16)) + 
      ggtitle("HeatMap for Keyword H1N1") 
    
    plt <- plot_usmap(data = df, values = "ACTIVITY.LEVEL", lines = "black") +
      scale_fill_manual(values = c("#FF0000", "#FF3800" ,"#FF7100" ,"#FFAA00" ,"#FFE200" ,"#E2FF00" ,"#AAFF00", "#71FF00", "#38FF00" ,"#00FF00")) +
      theme(legend.position = "right",
            legend.title = element_text("ILI Activity Level", face = "bold"),
            plot.title = element_text(hjust = 0.5, face="bold", size=16)) + 
      ggtitle("2018-19 Influenza Season Week 8 ending Feb 23, 2019")
    
    if(input$filter=='CDC Heatmap'){
      plt
    }
    else if(input$filter=='Flu'){
      flu_plt
    }
    else if(input$filter=='H1N1'){
      hn_plt
    }
    else if(input$filter=='Total Tweets'){
      total_plt
    }
    else if(input$filter=='CDC Heatmap VS Twitter Heatmap'){
      grid.arrange(plt,total_plt,nrow=1)
    }
    else if(input$filter=='CDC Heatmap VS Limited Tweet Chart'){
      grid.arrange(plt,l_plt,nrow=1)
    }
  })

}

shinyApp(ui = ui, server = server)

























