#
# This is a Shiny web application for Sky Liu's MA615 Final Project
# You can run the application by clicking
# the 'Run App' button above.
#
# The presentation slides and writen reports can be found here:
#
#    https://github.com/ayeskychen/MA615_FinalProj.git
#

library(shiny)
library(data.table)
library(ggplot2)
library(plotly)
library(foreign)
library(shinydashboard)
library(tidyr)
library(dplyr)
library(magrittr)
library(readr)
library(scales)
library(knitr)
library(benford.analysis)
library(tidytext)
library(wordcloud)
library(stringr)
library(leaflet)
library(kableExtra)

options("scipen"=100, "digits"=2)

# Define UI for application that draws a histogram
ui <- dashboardPage( 
  dashboardHeader(title = "Analysis on TripAdvisor Restaurant Review in Major European Cities",
                  titleWidth = 750),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "Overview",icon = icon("book-open")),
      menuItem("Map", tabName = "Map",icon = icon("globe-europe")),
      menuItem("Text Analysis on Reviews", tabName = "Text", icon = icon("comment-alt")),
      menuItem("Benford's Analysis on Reviews", tabName = "Benford", icon = icon("sort-numeric-up"))

    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "Overview",
              fluidRow(
                box(
                  title = "Backgroud",
                  width = 12,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  h5("This project took European restaurant data from TripAdvisor as the main data sourse and conducted analysis on restaurant ratings and reviews.")
                ),
                box(
                  title = "Data",
                  width = 12,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  h5("The major dataset was collected from Kaggle[1]. The dataset was obtained by scrapping the ratings and reviews for restaurants across 31 European cities from TripAdvisor."),
                  tableOutput('sample1'),
                  h5("Another dataset containing geolocation information of the cities was collected from Simplemaps World Cities Database[2]."),
                  tableOutput('sample2'),
                  h6("[1] TripAdvisor Restaurants Info for 31 Euro-Cities. https://www.kaggle.com/damienbeneschi/krakow-ta-restaurans-data-raw"),
                  h6("[2] Simplemaps World Cities Database. https://simplemaps.com/data/world-cities")
                )
                
              )#fluidrow1
      ),#dashitem1
      
      tabItem(tabName = "Map",
              fluidRow(
                box(
                  title = "Map",
                  width = 12,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  h5("This page provides an overview of the geographically information of the cities from the dataset and allows interaction to view the total number of restaurant, average rating and average number of reviews."),
                  h5("Please click on marker to see detailed information."),
                  leafletOutput("map")
                )
                
              )#fluidrow2
      ),#dashitem2
      
      tabItem(tabName = "Benford",
              fluidRow(
                box(
                  title = "Motives behind Benford's Law Analysis: detect fake reviews",
                  width = 12,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  h5("Higher ratings are associated with higher number of reviews."),
                  h5("Businesses might generate fake reviews to increase overall number of reviews."),
                  h5("Theoratically, number of reviews for each restaurant should be random and follow the Benfords Law distribution."),
                  plotlyOutput("benford1")
                ),
                tabBox(
                  title = " ",
                  width = 12,
                  tabPanel("1-Digit Benford's Law Analysis",
                           h5("In general, the number of reviews follows One-Digit Benfords Law distribution, and no obvious susbicious reviews are detected."),
                           plotOutput("benford2")),
                  tabPanel("2-Digit Benford's Law Analysis",
                           h5("Two-Digit Benfords Law Analysis shows some strikings on digit 20, 30, etc. This result could not provide any insights, becasue in two-digit analysis, any single digit number will be treated as two-digit number. For example, a restaurant with 8 reviews would be counted as 80 reviews."),
                           plotOutput("benford3"))
                )
                
              )#fluidrow3
      ),#dashiem3
      
      tabItem(tabName = "Text",
              fluidRow(
                box(
                  title = "Text Analysis on the Most Frequently Used Words in Reviews",
                  width = 12,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  h5("Text analysis focused on the most frequently used word in reviews of each star level."),
                  h5("Non-characters, common stopwords and customized stopwords were all deleted before the analysis.")
                ),
                tabBox(
                  title = "Most Frequently Used Word",
                  width = 12,
                  tabPanel("1-Star Restaurant",
                           h5("The reviews for 1-star restaurants are mostly strongly negative, like 'terrible', 'horrible', 'worst', 'disgusting' and etc."),
                           plotOutput("text1")
                           ),
                  tabPanel("2-Star Restaurant",
                           h5("The reviews for 2-star restaurants are slightly less negative than the ones for 1-star restaurants. Still, negative words like 'bad', 'poor' and etc. are commonly used."),
                           plotOutput("text2")),
                  tabPanel("3-Star Restaurant",
                           h5("The reviews for 3-star restaurants look much better. More than half of most frequently used words are positive, a few negative words are used though."),
                           plotOutput("text3")),
                  tabPanel("4-Star Restaurant",
                           h5("The reviews for 4-star restaurants are mostly positive"),
                           plotOutput("text4")),
                  tabPanel("5-Star Restaurant",
                           h5("The reviews for 5-star restaurants are positive overall, very similar to reviews for 4-star restaurants."),
                           plotOutput("text5"))
              )
                
              )#fluidrow3
      )
      
      )#dashitems
    
  )#dashbody
  
  
  
)#ui

# Define server logic required to draw a histogram
server <- function(input, output) {
  #read data
  data<-fread("TA_restaurants_curated.csv")
  #select variables
  data1<-data%>%dplyr::select(Name,City,Rating,`Number of Reviews`,Reviews)
  #clear out NAs
  data1<-na.omit(data1)
  data1<-data1%>%separate(Reviews, c("Reviews", "Review_Date"),"]")%>%dplyr::select(-Review_Date)
  data3<-data1%>%filter(Rating>0)
  #leading digits for bfd
  leading<-extract.digits(data$`Number of Reviews`, number.of.digits = 2,
                          sign="positive", second.order = FALSE, discrete=TRUE, round=3)
  data2<-cbind(data1,leading$data.digits)
  colnames(data2)[colnames(data2) == "V2"] <- "leading_digit"

  #text analysis data
  one_star<-data1%>%filter(Rating<2)%>%dplyr::select(Reviews)
  two_star<-data1%>%filter(Rating<3&Rating>1.5)%>%dplyr::select(Reviews)
  three_star<-data1%>%filter(Rating<4&Rating>2.5)%>%dplyr::select(Reviews)
  four_star<-data1%>%filter(Rating<5&Rating>3.5)%>%dplyr::select(Reviews)
  five_star<-data1%>%filter(Rating>4.5)%>%dplyr::select(Reviews)
  #map
  location<-fread("worldcities.csv")
  location<-location%>%dplyr::select(2:4)
  colnames(location)<-c("City","lat","lng")
  City_Info <- data1 %>% group_by(City) %>% summarise(
    n = n(),
    Ave_Star = mean(Rating),
    Ave_NReview = mean(`Number of Reviews`))
  
  
  City_Info<-left_join(City_Info,location,by = "City")
  #copenhagen Oporto
  City_Info[17,5]<-55.676098
  City_Info[17,6]<-12.568337
  City_Info[46,5]<-41.14961
  City_Info[46,6]<--8.61099
  CityMap<-rbind(City_Info[1,],City_Info[3,],City_Info[10:11,],City_Info[14:18,],City_Info[22:23,],City_Info[27,],City_Info[29:31,],City_Info[33,],City_Info[35,],City_Info[38:41,],City_Info[45:48,],City_Info[53:54,],City_Info[57:58,],City_Info[61,],City_Info[63,])
  
  CityMap$Ave_Star<-round(CityMap$Ave_Star, digits = 2)
  CityMap$Ave_NReview<-round(CityMap$Ave_NReview, digits = 0)
  
  CityMap<-CityMap%>%mutate(color= ifelse(Ave_Star<3.9,"blue",
                                          ifelse(Ave_Star<4.0 & Ave_Star>=3.9,"green",
                                                 ifelse(Ave_Star<4.1 & Ave_Star>=4.0,"yellow",
                                                        ifelse(Ave_Star<4.2 & Ave_Star>=4.1,"orange","red")))))

  CityMap$popup <- with(CityMap, paste("<b>", City, "</b>","<br />","Restaurant #:" ,n,"<br />","Average # Reviews:" ,Ave_NReview,"<br />","Average Rating:" ,Ave_Star,"<br />"))
  
  markers <- awesomeIcons(
    icon='map-marker',
    iconColor = "Black",
    markerColor = CityMap$color,
    library='fa')
  
  leg<-data.frame(Ave_Star=c("3.8-3.9","3.9-4.0","4.0-4.1","4.1-4.2",">4.2"),col=c("blue","green","yellow","orange","red"))
 


 

  s<-stop_words
  s<-rbind(stop_words,c("food","SMART"),c("service","SMART"),c("avoid","SMART"),c("restaurant","SMART"),c("bar","SMART"),c("lunch","SMART"),c("experience","SMART"),c("pizza","SMART"),c("staff","SMART"),c("dinner","SMART"),c("meal","SMART"),c("italian","SMART"),c("atmosphere","SMART"),c("cheap","SMART"),c("location","SMART"),c("breakfast","SMART"),c("price","SMART"),c("local","SMART"))
  pal2 <- brewer.pal(8,"Dark2")
  
  
  #overview output

  output$sample1 <- function() {
    kableExtra::kable(data1[2:3,])%>%kable_styling()
  }
  output$sample2 <- function() {
    kableExtra::kable(CityMap[1:2,])%>%kable_styling()
  }
  #map output
  output$map <- renderLeaflet({
    leaflet(data = CityMap, width = "100%" ) %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      addAwesomeMarkers(
        lng = ~lng, 
        lat = ~lat, 
        popup = ~popup,
        icon = markers
      ) %>%
      addLegend(
        position='topright',
        labels= leg$Ave_Star,
        colors = leg$col,
        opacity = 0.3,
        title="Ave_Star"
      )
  })
  #benford output
  output$benford1 <- renderPlotly({
    rating_Info <- data3 %>% group_by(Rating) %>% summarise(
      n = n(),
      Ave_NReview = mean(`Number of Reviews`))
    rating_Info$Ave_NReview<-round(rating_Info$Ave_NReview, digits = 0)
    plot_ly(rating_Info, x = ~Rating, y = ~Ave_NReview, type = 'bar', marker = list(color = '#89CFF0')) %>%
      layout(title = "Average Rating and Reviews for 31 EU City Restaurants",
             xaxis = list(title = "\nRating"),
             yaxis = list(title = "Average Number of Reviews\n"))
  })
  
  output$benford2 <- renderPlot({
    bfd_numRev1 <- benford(data$`Number of Reviews`,number.of.digits = 1)
    plot(bfd_numRev1)
  })
  
  output$benford3 <- renderPlot({
    bfd_numRev2 <- benford(data$`Number of Reviews`,number.of.digits = 2)
    plot(bfd_numRev2)
  })
  
  #text output
  output$ta11 <- renderPlot({
    colnames(one_star) <- "text"
    line <- c(1:length(one_star))
    one_star <- cbind(line,one_star)
    one_star$text<-as.character(one_star$text)
    #a token per row
    one_star <-one_star %>%unnest_tokens(word,text)
    #get rid of any non-characters
    one_star <- one_star %>%mutate(word = str_extract(word,"[a-z']+"))
    #get rid of stop-words
    one_star<- one_star %>% anti_join(s)
    
    one_star <-na.omit(one_star)

    onestar<-one_star %>%
      count(word, sort = TRUE)%>%
      filter(n > 12) %>%
      mutate(word = reorder(word, n))
    ggplot(data=onestar,aes(word, n)) +
      geom_bar(stat="identity", fill = "#89CFF0")+ 
      xlab(NULL) +coord_flip() +ggtitle("Word Count for 1-star Restaurant")
  })
  
  output$ta12 <- renderPlot({
    colnames(one_star) <- "text"
    line <- c(1:length(one_star))
    one_star <- cbind(line,one_star)
    one_star$text<-as.character(one_star$text)
    #a token per row
    one_star <-one_star %>%unnest_tokens(word,text)
    #get rid of any non-characters
    one_star <- one_star %>%mutate(word = str_extract(word,"[a-z']+"))
    #get rid of stop-words
    one_star<- one_star %>% anti_join(s)
    
    one_star <-na.omit(one_star)
    onestar<-one_star %>%
      count(word, sort = TRUE)%>%
      filter(n > 12) %>%
      mutate(word = reorder(word, n))
    
    one_star %>%
      anti_join(s) %>%
      count(word) %>%
      with(wordcloud(word, n, max.words = 101,colors=pal2))
  })
  
  output$ta21 <- renderPlot({
    
    colnames(two_star) <- "text"
    line <- c(1:length(two_star))
    two_star <- cbind(line,two_star)
    two_star$text<-as.character(two_star$text)
    #a token per row
    two_star <-two_star %>%unnest_tokens(word,text)
    #get rid of any non-characters
    two_star <- two_star %>%mutate(word = str_extract(word,"[a-z']+"))
    
    #get rid of stop-words
    two_star<- two_star %>% anti_join(s)
    
    two_star <-na.omit(two_star)
    
    two_star %>%
      count(word, sort = TRUE)%>%
      filter(n > 74) %>%
      mutate(word = reorder(word, n))%>%
      ggplot(aes(word, n)) +geom_col() +xlab(NULL) +
      geom_bar(stat="identity", fill = "#89CFF0")+ coord_flip() +ggtitle("Figure 7: Word Count for 2-star Restaurant")
  })
  
  output$ta22 <- renderPlot({
    colnames(two_star) <- "text"
    line <- c(1:length(two_star))
    two_star <- cbind(line,two_star)
    two_star$text<-as.character(two_star$text)
    #a token per row
    two_star <-two_star %>%unnest_tokens(word,text)
    #get rid of any non-characters
    two_star <- two_star %>%mutate(word = str_extract(word,"[a-z']+"))
   
    #get rid of stop-words
    two_star<- two_star %>% anti_join(s)
    
    two_star <-na.omit(two_star)
    two_star %>%
      anti_join(s) %>%
      count(word) %>%
      with(wordcloud(word, n, max.words = 100,colors=pal2))
  })

  output$ta31 <- renderPlot({
    
    colnames(three_star) <- "text"
    line <- c(1:length(three_star))
    three_star <- cbind(line,three_star)
    three_star$text<-as.character(three_star$text)
    #a token per row
    three_star <-three_star %>%unnest_tokens(word,text)
    #get rid of any non-characters
    three_star <- three_star %>%mutate(word = str_extract(word,"[a-z']+"))
    
    #get rid of stop-words
    three_star<- three_star %>% anti_join(s)
    
    three_star <-na.omit(three_star)
    
    #word count
    three_star %>%
      count(word, sort = TRUE)%>%
      filter(n > 530) %>%
      mutate(word = reorder(word, n))%>%
      ggplot(aes(word, n)) +
      geom_bar(stat="identity", fill = "#89CFF0")+xlab(NULL) +coord_flip() +ggtitle("Word Count for 3-star Restaurant")
    })
  
  output$ta32 <- renderPlot({
    colnames(three_star) <- "text"
    line <- c(1:length(three_star))
    three_star <- cbind(line,three_star)
    three_star$text<-as.character(three_star$text)
    #a token per row
    three_star <-three_star %>%unnest_tokens(word,text)
    #get rid of any non-characters
    three_star <- three_star %>%mutate(word = str_extract(word,"[a-z']+"))
    
    #get rid of stop-words
    three_star<- three_star %>% anti_join(s)
    
    three_star <-na.omit(three_star)
    three_star %>%
      anti_join(s) %>%
      count(word) %>%
      with(wordcloud(word, n, max.words = 100,colors=pal2))
  })
  
  
  output$ta41 <- renderPlot({
    
    colnames(four_star) <- "text"
    line <- c(1:length(four_star))
    four_star <- cbind(line,four_star)
    four_star$text<-as.character(four_star$text)
    #a token per row
    four_star <-four_star %>%unnest_tokens(word,text)
    #get rid of any non-characters
    four_star <- four_star %>%mutate(word = str_extract(word,"[a-z']+"))
    
    #get rid of stop-words
    four_star<- four_star %>% anti_join(s)
    
    four_star <-na.omit(four_star)
    
    #word count
    four_star %>%
      count(word, sort = TRUE)%>%
      filter(n > 1400) %>%
      mutate(word = reorder(word, n))%>%
      ggplot(aes(word, n)) +
      geom_bar(stat="identity", fill = "#89CFF0")+ 
      xlab(NULL) +coord_flip() +ggtitle("Word Count for 4-star Restaurant")
    })
  
  output$ta42 <- renderPlot({
    colnames(four_star) <- "text"
    line <- c(1:length(four_star))
    four_star <- cbind(line,four_star)
    four_star$text<-as.character(four_star$text)
    #a token per row
    four_star <-four_star %>%unnest_tokens(word,text)
    #get rid of any non-characters
    four_star <- four_star %>%mutate(word = str_extract(word,"[a-z']+"))
    
    #get rid of stop-words
    four_star<- four_star %>% anti_join(s)
    
    four_star <-na.omit(four_star)
    
    four_star %>%
      anti_join(s) %>%
      count(word) %>%
      with(wordcloud(word, n, max.words = 100,colors=pal2))
  })
  
  output$ta51 <- renderPlot({
    
    colnames(five_star) <- "text"
    line <- c(1:length(five_star))
    five_star <- cbind(line,five_star)
    five_star$text<-as.character(five_star$text)
    #a token per row
    five_star <-five_star %>%unnest_tokens(word,text)
    #get rid of any non-characters
    five_star <- five_star %>%mutate(word = str_extract(word,"[a-z']+"))
    
    #get rid of stop-words
    five_star<- five_star %>% anti_join(s)
    
    five_star <-na.omit(five_star)
    
    #word count
    five_star %>%
      count(word, sort = TRUE)%>%
      filter(n > 230) %>%
      mutate(word = reorder(word, n))%>%
      ggplot(aes(word, n)) +
      geom_bar(stat="identity", fill = "#89CFF0")+ xlab(NULL) +coord_flip() +ggtitle(" Word Count for 5-star Restaurant")
  })
  
  output$ta52 <- renderPlot({
    colnames(five_star) <- "text"
    line <- c(1:length(five_star))
    five_star <- cbind(line,five_star)
    five_star$text<-as.character(five_star$text)
    #a token per row
    five_star <-five_star %>%unnest_tokens(word,text)
    #get rid of any non-characters
    five_star <- five_star %>%mutate(word = str_extract(word,"[a-z']+"))
    
    #get rid of stop-words
    five_star<- five_star %>% anti_join(s)
    
    five_star <-na.omit(five_star)
    
    five_star %>%
      anti_join(s) %>%
      count(word) %>%
      with(wordcloud(word, n, max.words = 100,colors=pal2))
  })
  
  output$text1 <- renderImage({

    filename <- normalizePath(file.path('one1.png'))
    

    list(src = filename,
         width = 600,
         height = 350,
         alt = paste("Image number"))
    
  }, deleteFile = FALSE)
  
  output$text2 <- renderImage({

    filename <- normalizePath(file.path('two1.png'))
    

    list(src = filename,
         width = 600,
         height = 350,
         alt = paste("Image number"))
    
  }, deleteFile = FALSE)
  
  output$text3 <- renderImage({
    
    filename <- normalizePath(file.path('three1.png'))
    
    
    list(src = filename,
         width = 600,
         height = 350,
         alt = paste("Image number"))
    
  }, deleteFile = FALSE)
  
  output$text4 <- renderImage({
    
    filename <- normalizePath(file.path('four1.png'))
    
    
    list(src = filename,
         width = 600,
         height = 350,
         alt = paste("Image number"))
    
  }, deleteFile = FALSE)
  
  output$text5 <- renderImage({
    
    filename <- normalizePath(file.path('five1.png'))
    
    
    list(src = filename,
         width = 600,
         height = 350,
         alt = paste("Image number"))
    
  }, deleteFile = FALSE)


  
}#server

# Run the application 
shinyApp(ui = ui, server = server)

