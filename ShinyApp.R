setwd("~/adv ba wid r sourav")

if(!require(shiny)){
  install.packages("shiny")
  library(shiny)
}
if(!require(tidyverse)){
  install.packages("tidyverse")
  library(tidyverse)
}
if(!require(lubridate)){
  install.packages("lubridate")
  library(lubridate)
}
if(!require(tidytext)){
  install.packages("tidytext")
  library(tidytext)
}
if(!require(reshape2)){
  install.packages("reshape2")
  library(reshape2)
}
if(!require(stringr)){
  install.packages("stringr")
  library(stringr)
}
library(data.table)
library(ggplot2)



BillBoard<-read_csv("billboard_lyrics_1964-2015.csv")
dim(BillBoard)
str(BillBoard)
df<-data.frame(table(BillBoard$Rank,BillBoard$Artist))

#removing zero hits

colnames(df)<-c('Rank','Artist','Hits')
df<-subset(df,Hits!=0)
view(df)



ui <- fluidPage(
  
  titlePanel("BillBoard Lyrics 1965-2015"),
  
  
  sidebarLayout(
    
    # Input(s)
    sidebarPanel(
      
      
      
      
      # Select variable for grouping
      selectInput(inputId = "Rank", 
                  label = "Rank:",
                  choices = c(df$Rank), 
                  selected = 1),
      
      
      selectInput(inputId = "Year", 
                  label = "Year:",
                  choices = c(BillBoard$Year), 
                  selected = 1965)
      
      
      
      
      
      
    ),
    
    
    # Outputs
    mainPanel(
      tabsetPanel(type="tabs",
                  tabPanel("Hits(select Rank)",plotOutput("barplot")),
                  tabPanel("Lyrics(select Year)",plotOutput("barplot1")),
                  tabPanel("Title(select Year)",plotOutput("barplot2")),
                  tabPanel("Sentiment Analysis",plotOutput("barplot3"))
                  
      )
    )
  )
  
)




server <- function(input, output) {
  
  #Create barplot object the plotOutput function is expecting
  output$barplot <- renderPlot({
    
    data<-data.frame(df%>%group_by(Rank==input$Rank))
    colnames(data)<-c('Rank','Artist','Hits','Input')
    data<-subset(data,Input=='TRUE')
 
    
    
    ggplot(data =data, aes(x =Artist,y =Hits)) +
      geom_bar(stat='identity') + theme_minimal()+ggtitle('Hits for an Artist',subtitle=NULL)+
      theme(axis.text.x=element_text(angle=45,hjust=1))
  })
  
  output$barplot1 <- renderPlot({
    tidy_songs<-BillBoard%>%unnest_tokens(word,Lyrics)
    tidy_songs<-subset(tidy_songs,word!="im")
    #Removing stopwords
    songs<-tidy_songs%>%anti_join(stop_words)%>%
      group_by(Year==input$Year)%>%count(word,sort=TRUE)
    colnames(songs)<-c("Bool","word","n")
    songs<-subset(songs,Bool=="TRUE")
    
   
   
      ##some more words are getting added to the freq words every year
    ggplot(subset(songs,n>50), aes(x =word,y =n)) +
      geom_bar(stat='identity') + theme_minimal()+ggtitle('Freq words in song Lyrics for selected Year',subtitle='words are changing over time')+
      theme(axis.text.x=element_text(angle=45,hjust=1))
  })
  
  output$barplot2 <- renderPlot({
    tidy_songs<-BillBoard%>%unnest_tokens(word,Song)
    #tidy_songs<-subset(tidy_songs,word!="im")
    #Removing stopwords
    songs1<-tidy_songs%>%anti_join(stop_words)%>%
      group_by(Year==input$Year)%>%count(word,sort=TRUE)
    colnames(songs1)<-c("Bool","word","n")
    songs1<-subset(songs1,Bool=="TRUE")
    
    
    ##some more words are geeting added to the freq words every year
    ggplot(subset(songs1,n>1), aes(x =word,y =n)) +
      geom_bar(stat='identity') + theme_minimal()+ggtitle('Freq words in song title for selected Year',subtitle='Words are changing over time')+
      theme(axis.text.x=element_text(angle=45,hjust=1))
  })
  
  output$barplot3 <- renderPlot({
    tidy_songs<-BillBoard%>%unnest_tokens(word,Lyrics)
    song_sentiment<-tidy_songs%>%
      inner_join(get_sentiments("bing"))
                 
    

    # Which words are driving sentiment scores?
    song_sentiment %>%
      count(word, sentiment) %>%
      group_by(sentiment) %>%
      # Take the top 10 words for each sentiment
      top_n(10) %>%
      ungroup() %>%
      mutate(word = reorder(word, n)) %>%
      # Set up the plot with aes()
      ggplot(aes(x= word, y = n, fill = sentiment)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~ sentiment, scales = "free") +
      coord_flip()+ggtitle('Overall Sentiment Analysis')
    
  })
}

shinyApp(ui = ui, server = server)