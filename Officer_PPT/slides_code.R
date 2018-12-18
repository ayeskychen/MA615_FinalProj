library(officer)
library(magrittr)
library(tidyverse)
library(readxl)

##  pic a template you like
pres1 <- read_pptx("Default.pptx") 


##  get the layout
layout_summary(pres1)

master <- "Facet"



#SLIDE1 TITLE
layout_properties(x = pres1, layout = "Title Slide", master = master )

pres1 %<>%  add_slide(layout = "Title Slide", master = master) %>% 
  ph_with_text(type = "ctrTitle", str = "TripAdvisor Restaurants Review in Major Euro-Cities") %>%
  ph_with_text(type="subTitle",str="Sky Liu
               ")



#SLIDE 2 OVERVIEW

pres1 %<>% add_slide(layout="Title and Content",master=master) %>%
  ph_with_text(type="title", str="Overview")  %>%
  ph_with_ul(type = "body", index = 1, str_list = c("This project took European restaurant data from TripAdvisor as the main data sourse and conducted analysis on restaurant ratings and reviews.",
                                                    "Major techniques applied in the analysis:",
                                                    "data cleaning using dplyr",
                                                    "Benford's Law analysis using benford.analysis",
                                                    "text mining using tidytext and wordcloud",
                                                    "mapping using leaflet",
                                                    "data visualization using plotly"), 
                                                         level_list = c(1,1,2,2,2,2,2))

#SLIDE 3 DATA

pres1 %<>% add_slide(layout="Title and Content",master=master) %>%
     ph_with_text(type="title", str="Data Cleaning")  %>%
     ph_with_ul(type = "body", index = 1, str_list = c("Datasets",
                                                       "Kaggle: Ratings and reviews for restaurants across 31 European cities from TripAdvisor[1]",
                                                       "Simplemaps: World Cities Database[2] .",
                                                       "Original data: 125527 records of restaurants from 31 European cities.",
                                                       "After cleaning: 108178 records"), 
                               level_list = c(1,2,2,1,1))
#SLIDE 4 MAP
pres1 %<>% add_slide(layout="Title and Content",master=master) %>%
  ph_with_text(type="title", str="Data Cleaning")  %>%
  ph_with_ul(type = "body", index = 1, str_list = c("Geographically information of the cities from the dataset",
                                                    "Interaction to view:",
                                                    "the total number of restaurant",
                                                    "average rating",
                                                    "average number of reviews."), 
             level_list = c(1,1,2,2,2))%>%
  ph_with_img_at(src = "map.png", width = 5,height = 3,left = 5,top = 4,rot = 0)




#SLIDE 5 BENFORD1
pres1 %<>% add_slide(layout="Title and Content",master=master) %>%
  ph_with_text(type="title", str="Benford's Law Analysis") %>%
  ph_with_ul(type = "body", index = 1, str_list = c("Motives behind Benford's Law Analysis: detect fake reviews",
                                                    "Higher ratings are associated with higher number of reviews.",
                                                    "Businesses might generate fake reviews to increase overall number of reviews." ), 
             level_list = c(1,2,2))%>%
  ph_with_img_at(src = "figure2.png", width = 5,height = 3,left = 3,top = 4,rot = 0)

#SLIDE 6 BENFORD2
pres1 %<>% add_slide(layout="Title and Content",master=master) %>%
  ph_with_text(type="title", str="Benford's Law Analysis") %>%
  ph_with_ul(type = "body", index = 1, str_list = c("The number of reviews follows One-Digit Benfords Law distribution",
                                                    "No obvious susbicious reviews are detected"), 
             level_list = c(1,1))%>%
  ph_with_img_at(src = "Bfd1.png", width = 5,height = 3,left = 3,top = 4,rot = 0)

#SLIDE 7 TEXT1
pres1 %<>% add_slide(layout="Title and Content",master=master) %>%
  ph_with_text(type="title", str="Text Analysis on Reviews")%>%
  ph_with_ul(type = "body", index = 1, str_list = c("The most frequently used word in reviews of each star level.",
                                                    "Non-characters, common stopwords and customized stopwords deleted.",
                                                    "Customized stopwords: food, service, restaurant, dinner, lunch, etc.",
                                                    "Summary",
                                                    "1-star restaurants: mostly strongly negative words like,'terrible', 'horrible','worst'",
                                                    "2-star restaurants: slightly less negative though, 'bad', 'poor' and etc commonly used",
                                                    "3-star restaurants: more than half of most frequently used words are positive",
                                                    "4-star restaurants: mostly positive",
                                                    "5-star restaurants: very similar to 4-star restaurants reviews."), 
             level_list = c(1,1,2,1,2,2,2,2,2))

#SLIDE 7 TEXT2
pres1 %<>% add_slide(layout="Two Content",master=master) %>%
  ph_with_text(type="title", str="Reviews for 1-star Restaurants") %>%
  ph_with_img_at(src = "one1.png", width = 7,height = 4,left = 1,top = 2,rot = 0)%>%
  ph_with_img_at(src = "one2.png", width = 4,height = 4,left = 7,top = 2,rot = 0)
  
#SLIDE 7 TEXT3
pres1 %<>% add_slide(layout="Two Content",master=master) %>%
  ph_with_text(type="title", str="Reviews for 2-star Restaurants") %>%
  ph_with_img_at(src = "two1.png", width = 7,height = 4,left = 1,top = 2,rot = 0)%>%
  ph_with_img_at(src = "two2.png", width = 4,height = 4,left = 7,top = 2,rot = 0)


#SLIDE 7 TEXT4
pres1 %<>% add_slide(layout="Two Content",master=master) %>%
  ph_with_text(type="title", str="Reviews for 3-star Restaurants") %>%
  ph_with_img_at(src = "three1.png", width = 7,height = 4,left = 1,top = 2,rot = 0)%>%
  ph_with_img_at(src = "three2.png", width = 4,height = 4,left = 7,top = 2,rot = 0)


#SLIDE 7 TEXT5
pres1 %<>% add_slide(layout="Two Content",master=master) %>%
  ph_with_text(type="title", str="Reviews for 4-star restaurants") %>%
  ph_with_img_at(src = "four1.png", width = 7,height = 4,left = 1,top = 2,rot = 0)%>%
  ph_with_img_at(src = "four2.png", width = 4,height = 4,left = 7,top = 2,rot = 0)


#SLIDE 7 TEXT6
pres1 %<>% add_slide(layout="Two Content",master=master) %>%
  ph_with_text(type="title", str="Reviews for 5-star Restaurants") %>%
  ph_with_img_at(src = "five1.png", width = 7,height = 4,left = 1,top = 2,rot = 0)%>%
  ph_with_img_at(src = "five2.png", width = 4,height = 4,left = 7,top = 2,rot = 0)

#SLIDE 8 CONCLUSION

pres1 %<>% add_slide(layout="Title and Content",master=master) %>%
  ph_with_text(type="title", str="Conclusion")  %>%
  ph_with_ul(type = "body", index = 1, str_list = c("The ratings and reviews of the restaurants from the major 31 European cities look fair.",
                                                    "No susbicious reviews detected by using Benford's Law analysis",
                                                    "The review contents are consistant with ratings",
                                                    "Higher rating restaurants are mostly in South East Europe."), 
             level_list = c(1,2,2,2))

#SLIDE 9 THANKYOU

pres1 %<>%  add_slide(layout = "Title Slide", master = master) %>% 
  ph_with_text(type = "ctrTitle", str = "Thank You")

#SLIDE 9 Citation

pres1 %<>% add_slide(layout="Title and Content",master=master) %>%
  ph_with_text(type="title", str="Citation")  %>%
  ph_with_ul(type = "body", index = 1, str_list = c("[1] TripAdvisor Restaurants Info for 31 Euro-Cities. ",
                                                    "https://www.kaggle.com/damienbeneschi/krakow-ta-restaurans-data-raw",
                                                    "[2] Simplemaps World Cities Database.",
                                                    "https://simplemaps.com/data/world-cities"), 
             level_list = c(1,2,1,2))

pres1 <- remove_slide(pres1, index = 1)
#########
print(pres1, target = "MA616_FinalProj.pptx") 
 