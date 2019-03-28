## Data Management with R
## Final Data Analysis Assignment
## 15/12/2017
## Lorenzo Andreoli & Riccardo Colnaghi 
################################################################################
## Load necessarry packages 
library(tidyverse)
library(stringr)
library(ggplot2)
library(openintro)
library(readxl)
library(readr)
library(DBI)
library(httr)
library(jsonlite)
library(haven)
library(gdata)
library(lubridate)
library(countrycode)
library(gridExtra)
library(cowplot)
library(leaflet)
library(leaflet.extras)
library(magrittr)
library(igraph)
library(ggraph)
library(htmlwidgets) 
library(dygraphs) 
library(flexdashboard) 
library(shiny) 
library(plotly) 
library(geojsonio)
library(geojsonlint)
library(htmlwidgets)
library(htmltools)
library(maps)
library(rworldmap)
library(devtools)

setwd("~/Desktop/1.Data_Management_R/GitHub/Final_Project_Data_Management/")

# Import Dataset 
df <- data.frame(read_xlsx("ACLED-Version-7-All-Africa-1997-2016_dyadic-file.xlsx"))
class(df)

# Explore the raw data ###
# Are there any NAs? 
any(is.na(df))
# How many NAs? 
sum(is.na(df))
# Are there missing values? 
any(!complete.cases(df))
# view dimension 
dim(df)
## name of columns 
names(df)
# structure of dataframe 
str(df)
# structure with dplyr
glimpse(df)
# summary data 
summary(df)
# lower Capital letters of variables 
names(df)<- tolower(names(df))
# Event Type description
table(df$event_type)
# frequency for each year 
hist(df$year) 
 

################################################################################
# Belligerent vs. Paceful countries
################################################################################

# Top 15 countries with the most conflicts 

n_conf_count <- df %>% group_by(country) %>%
        summarise(number_conflicts = n()) %>%
        arrange(desc(number_conflicts))

num_conf_country_top <- head(n_conf_count,15)
num_conf_country_top$country <- as.factor(num_conf_country_top$country)

plot1 <- ggplot(num_conf_country_top, aes(x = reorder(country, number_conflicts), 
                                  y = number_conflicts)) +
        geom_bar(stat='identity',colour="black", fill = "dark red") +
        geom_text(aes(x = country, y = 1, 
                      label = paste0("(",number_conflicts,")",sep="")),
                  hjust=-0.1, vjust=.5, size = 2.5, colour = 'white') +
        labs(x = 'Country',
             y = 'Number of Conflicts', 
             title = 'Countries with the most conflicts') +
        theme_bw()+
        theme(axis.text.x = element_text(angle = 90),
              axis.text.y = element_text(size = 10),
              plot.title = element_text(size=10, face = "bold"),
              axis.title.x = element_text(size = 10),
              axis.title.y = element_text(size = 10),
              legend.text = element_text(size = 10)) +
        coord_flip() 

## Bottom 15 countries with the least conflcits 

num_conf_country_bot <- tail(n_conf_count,15)
num_conf_country_bot$country <- as.factor(num_conf_country_bot$country)

plot2 <- ggplot(num_conf_country_bot, aes(x = reorder(country, number_conflicts), 
                             y = number_conflicts)) +
        geom_bar(stat='identity',colour="black", fill = "light blue") +
        geom_text(aes(x = country, y= 1, 
                      label = paste0("(",number_conflicts,")",sep="")),
                  hjust=-0.2, vjust=.5, size = 2.5, colour = 'black') +
        labs(x = 'Country',
             y = 'Number of Conflicts', 
             title = 'Countries with the least conflicts') +
        theme_bw()+
        theme(axis.text.x = element_text(angle = 90),
              axis.text.y = element_text(size = 10),
              plot.title = element_text(size=10, face = "bold"),
              axis.title.x = element_text(size = 10),
              axis.title.y = element_text(size = 10),
              legend.text = element_text(size = 10)) +
        coord_flip() 
      
## merge the two graphs
plot1_2 <- plot_grid(plot1, plot2)
plot1_2

################################################################################
## How conflicts have evolved? 
################################################################################
# conflicts by event type between 1997 and 2009 
conf_num_97 = df %>% group_by(country,event_type, year) %>%
        summarise(number_conflicts = n()) %>%
        arrange(desc(number_conflicts))

conf_num_97$country = as.factor(conf_num_97$country)
conf_num_97$event_type = as.factor(conf_num_97$event_type)
conf_num_97$year = as.factor(conf_num_97$year)

plot3 <- conf_num_97 %>%  filter(year %in% 
        c("1997", "1998", "1999", "2000", "2001","2002","2003","2004","2005","2006","2007","2008","2009")) %>% 
ggplot(aes(x= reorder(country, number_conflicts), y=number_conflicts, fill=event_type)) +
        geom_bar(stat = 'identity', position = position_stack(reverse = TRUE)) +
        scale_y_continuous(limits = c(0,5000), expand = c(0, 0)) +
        coord_flip() +
        labs(x = "Country",
             y = "Number of Conflicts",
             fill = "Legend",
             title = "Conflicts between 1997-2009") +
        theme_bw()+
        theme(axis.text.y = element_text(size = 5, face = "bold"),
              axis.text.x = element_text(angle = 90, vjust = (-.1)),
              plot.title = element_text(size=10, face = "bold"),
              axis.title.x = element_text(size = 10),
              axis.title.y = element_text(size = 10),
              legend.text = element_text(size = 10))


# conflicts by event type between 2010 and 2016
conf_num_97 = df %>% group_by(country,event_type, year) %>%
        summarise(number_conflicts = n()) %>%
        arrange(desc(number_conflicts))

conf_num_97$country = as.factor(conf_num_97$country)
conf_num_97$event_type = as.factor(conf_num_97$event_type)
conf_num_97$year = as.factor(conf_num_97$year)

plot4 <- conf_num_97 %>%  filter(year %in% 
        c("2010","2011","2012","2013", "2014", "2015", "2016")) %>% 
        ggplot(aes(x= reorder(country, number_conflicts), y=number_conflicts, fill=event_type)) +
        geom_bar(stat = 'identity', position = position_stack(reverse = TRUE)) +
        scale_y_continuous(limits = c(0,17000), expand = c(0, 0)) +
        coord_flip() +
        labs(x = "Country",
             y = "Number of Conflicts",
             fill = "Legend",
             title = "Conflicts between 2010-2016") +
        theme_bw()+
        theme(axis.text.y = element_text(size = 5, face = "bold"),
              axis.text.x = element_text(angle = 90, vjust = (-.1)),
              plot.title = element_text(size=10, face = "bold"),
              axis.title.x = element_text(size = 10),
              axis.title.y = element_text(size = 10),
              legend.text = element_text(size = 10))

# Evolution of the type of conflicts over years 
conf_year <- df %>% group_by(event_type, year) %>%
        summarise(number_conflicts = n()) %>%
        arrange(desc(number_conflicts))

conf_year$event_type <- as.factor(conf_year$event_type)
conf_year$year <- as.numeric(as.character(conf_year$year))
conf_year$number_conflicts <- as.numeric(as.character(conf_year$number_conflicts))
                            
plot5 <- conf_year %>% 
        ggplot(aes(x=year, y=number_conflicts,group=event_type, color= event_type)) +
        geom_line() +
        scale_x_continuous(limits = c(1997,2016), expand = c(0, 0), breaks= 1997:2016) +
        scale_y_continuous(limits = c(0,7000), expand = c(0, 0)) +
        labs(x = "Years",
             y = "Number of Conflicts",
             color = "Legend",
             title = "How conflicts changed over years") +
        theme_bw()+
        theme(axis.text.x = element_text(angle = 90, vjust = (-.1)),
              legend.position="bottom",
              plot.title = element_text(size=10, face = "bold"),
              axis.title.x = element_text(size = 10),
              axis.title.y = element_text(size = 10),
              legend.text = element_text(size = 10))

# Merge the three graphs 
bottom_plots <- plot_grid(plot3 + theme(legend.position="none"), 
                          plot4 + theme(legend.position="none"), 
                          align = 'h', rel_widths = c(1, 1.5))

prow2 <- plot_grid( plot5 + theme(legend.position="none"),
                   bottom_plots + theme(legend.position="none"),
                   ncol = 1, rel_heights = c(1, 1.2))

legend2 <- get_legend(plot4+ theme(legend.position="bottom"))
plot3_4_5 <-plot_grid(prow2, legend2, ncol = 1, rel_heights = c(1, .2))
plot3_4_5


################################################################################
## Arab Spring 
################################################################################
## Where riots and protests took place from 2010 and 2011: A visual representation of the Arab Spring 

RP = "Riots/Protests"

RP_map = df %>% 
        filter(!is.na(latitude)) %>%
        filter(!is.na(longitude)) %>%
        filter(str_detect(event_type, RP) ) %>%
        filter(year == 2010:2011)

factpal <- colorFactor(c("#ca0020","#404040"), 
                       RP_map$year)

center_lon = median(RP_map$longitude)
center_lat = median(RP_map$latitude)

plot6 <- leaflet(RP_map) %>% addTiles() %>%
        addCircles(lng = ~longitude, lat = ~latitude,radius = ~(fatalities),
                   color = ~factpal(year),
                   popup = ~notes)  %>%
        addLegend("bottomleft", pal = factpal, values = ~year,
                  title = "Protests and Riots between 2010-2011")
plot6

################################################################################
## A comparison between Arab Spring Countries and Non-Arab Spring countries after 2010

conf_num_arab = df %>% 
        group_by(country,event_type, year) %>%
        summarise(number_conflicts = n()) %>%
        arrange(desc(number_conflicts))

conf_num_arab$cou = as.factor(conf_num_arab$country)
conf_num_arab$event_type = as.factor(conf_num_arab$event_type)
conf_num_arab$year = as.factor(conf_num_arab$year)

plot7 <- conf_num_arab %>%  
        filter(year %in% c("2010","2011","2012","2013", "2014", "2015", "2016")) %>% 
        filter(country %in% c("Egypt", "Libya","Tunisia", "Morocco", "Algeria", "Sudan", "Djibouti")) %>% 
        ggplot(aes(x= reorder(country, number_conflicts), y=number_conflicts, fill=event_type), width=3, height=4) +
        geom_bar(stat = 'identity', position = position_stack(reverse = TRUE)) +
        scale_fill_manual(name = 'Legend',
                           breaks = c("Battle-Government regains territory",
                                      "Battle-No change of territory", 
                                      "Battle-Non-state actor overtakes territory",
                                      "Headquarters or base established",
                                      "Non-violent transfer of territory", 
                                      "Remote violence", 
                                      "Riots/Protests", 
                                      "Strategic development", 
                                      "Violence against civilians"),
                           values = c("Battle-Government regains territory"  = "#F8766D",
                                      "Battle-No change of territory" = "#D39200", 
                                      "Battle-Non-state actor overtakes territory" = "#93AA00",
                                      "Headquarters or base established" = "#00BA38", 
                                      "Non-violent transfer of territory" = "#00C19F" , 
                                      "Remote violence" = "#00B9E3", 
                                      "Riots/Protests" = "#619CFF",
                                      "Strategic development" = "#DB72FB", 
                                      "Violence against civilians" = "#FF61C3")) +
        scale_y_continuous(limits = c(0,8000), expand = c(0, 0)) +
        labs(x = "Country",
             y = "Number of Conflicts",
             fill = "Legend",
             title = "Conflicts in Countries of the Arab Spring") +
        theme_bw()+
        theme(axis.text.y = element_text(size = 10),
              axis.text.x = element_text(angle = 90, vjust = (-.1)),
              plot.title = element_text(size=10, face = "bold"),
              axis.title.x = element_text(size = 10),
              axis.title.y = element_text(size = 10),
              legend.text = element_text(size = 10))

conf_num_no_arab = df %>% 
        group_by(country,event_type, year) %>%
        filter(year %in% c("2010","2011","2012","2013", "2014", "2015", "2016")) %>% 
        filter(!country %in% c("Egypt", "Libya","Tunisia", "Morocco", "Algeria", "Sudan", "Djibouti")) %>% 
        summarise(number_conflicts = n()) %>%
        arrange(desc(number_conflicts))

conf_num_no_arab$cou = as.factor(conf_num_no_arab$country)
conf_num_no_arab$event_type = as.factor(conf_num_no_arab$event_type)
conf_num_no_arab$year = as.factor(conf_num_no_arab$year)

plot8 <-ggplot(conf_num_no_arab[0:40,], 
               aes(x= reorder(country, number_conflicts), y=number_conflicts, fill=event_type), width=3, height=4) +
        geom_bar(stat = 'identity', position = position_stack(reverse = TRUE)) +
        scale_fill_manual(name = 'Legend',
                           breaks = c("Battle-No change of territory", 
                                      "Remote violence", 
                                      "Riots/Protests", 
                                      "Strategic development", 
                                      "Violence against civilians"),
                           values = c("Battle-No change of territory" = "#D39200",  
                                      "Remote violence" = "#00B9E3", 
                                      "Riots/Protests" = "#619CFF",
                                      "Strategic development" = "#DB72FB", 
                                      "Violence against civilians" = "#FF61C3")) +
        scale_y_continuous(limits = c(0,16000), expand = c(0, 0)) +
        labs(x = "Country",
             y = "Number of Conflicts",
             title = "Conflicts in Countries NOT in the Arab Sping") +
        theme_bw()+
        theme(axis.text.y = element_text(size = 10),
              axis.text.x = element_text(angle = 90, vjust = (-.1)),
              plot.title = element_text(size=10, face = "bold"),
              axis.title.x = element_text(size = 10),
              axis.title.y = element_text(size = 10),
              legend.text = element_text(size = 10))

plot7_8 <- plot_grid(plot8 + theme(legend.position="none"),
                   plot7 + theme(legend.position="bottom"),
                   ncol = 1) 
plot7_8 
       
################################################################################
##  25 most frequent actors involved in conflicts
################################################################################

actor_involved_1 = df %>% 
        group_by(actor1) %>% 
        summarise(act_inv = n()) %>%
        arrange(desc(act_inv)) 

actor_involved_2 = df %>% 
        group_by(actor2) %>% 
        summarise(act_inv = n()) %>%
        arrange(desc(act_inv))

colnames(actor_involved_1) = c("Actor","Frequency")
colnames(actor_involved_2) = c("Actor","Frequency")

actor_involved_tot = rbind(actor_involved_1,actor_involved_2)

actor_involved_tot = actor_involved_tot %>%
        group_by(Actor) %>%
        summarise(act_inv = sum(Frequency)) %>%
        arrange(desc(act_inv))

Civilians = "Civilians"
Protesters  = "Protesters"
Rioters  = "Rioters"
Military ="Military Forces of"
Unidentified = "Unidentified"

actor_involved_tot = actor_involved_tot   %>%
        filter(!str_detect(Actor,Unidentified) ) %>%
        filter(!str_detect(Actor,Protesters) ) %>%
        filter(!str_detect(Actor,Rioters) ) %>%
        filter(!str_detect(Actor,Military) ) %>% 
        filter(!str_detect(Actor,Civilians) ) 

plot9 <- ggplot(actor_involved_tot[0:25,], aes(x = reorder(Actor, act_inv),y = act_inv)) +
        geom_bar(stat='identity',colour="black", fill = c("dark blue")) +
        geom_text(aes(x = Actor, y=1, label = paste0("(",act_inv,")",sep="")),
                  hjust=-.2, vjust=.5, size = 3, colour = 'white') +
        labs(x = 'Actor', y = 'Number of Conflicts', title = '25 most Active Actors in Conflicts') +
        scale_y_continuous(limits = c(0,8000), expand = c(0, 0)) +
        coord_flip() + 
        theme_bw()+
theme(axis.text.x = element_text(angle = 90, vjust = (-.1)), 
      legend.position="bottom",
      plot.title = element_text(size=10, face = "bold"),
      axis.title.x = element_text(size = 10),
      axis.title.y = element_text(size = 10),
      legend.text = element_text(size = 10))
plot9


################################################################################
## LRA: Lords Resistance Army
################################################################################

## Timeline of LRA operations 
LRA_time <- df %>% group_by(event_type, year, actor1, fatalities) %>%
        filter(actor1 == "LRA: Lords Resistance Army") %>% 
        summarise(number_conflicts = n()) %>%
        arrange(desc(number_conflicts)) %>% 
        ggplot(aes(x=year, y=number_conflicts,fill=event_type)) +
        geom_bar(stat = 'identity')+
        labs(x = "Years",
             y = "Conflicts",
             color = "Legend",
             title = "Conflicts involving LRA over years") +
        scale_x_continuous(limits = c(1997,2016), expand = c(0, 0), breaks= 1997:2016) +
        theme_bw()+
        theme(axis.text.x = element_text(angle = 90, vjust = (-.005)), 
              legend.position="bottom",
              plot.title = element_text(size=10, face = "bold"),
              axis.title.x = element_text(size = 10),
              axis.title.y = element_text(size = 10),
              legend.text = element_text(size = 10))
plot10 <- LRA_time
plot10

# Operation of the LRA army
LRA <- df %>%
        filter(actor1 == "LRA: Lords Resistance Army") %>% 
        filter(!is.na(latitude)) %>%
        filter(!is.na(longitude)) %>% 
        filter(!is.na(year))
     

factpal <- colorFactor(c("#ca0020","#f4a582","#f7f7f7","#92c5de","#404040"), 
                       LRA$year)

lon <- median(LRA$longitude)
lat <- median(LRA$latitude)

plot11 <- leaflet(LRA) %>% addTiles() %>%
        addCircles(lng = ~longitude, lat = ~latitude,radius = ~(fatalities),
                   popup = ~notes, 
                   color = ~factpal(year))  %>%
        setView(lng=lon, lat=lat,zoom = 5) %>%
        addLegend("bottomright", pal = factpal, values = ~year,
                  title = "Operating Years of LRA")
plot11

################################################################################
## Comparison of Fatalaties vs. Conflicts
################################################################################

fat_conf <- df %>% 
        group_by(event_type, year, actor1, fatalities) %>%
        summarise(number_conflicts = n()) %>%
        arrange(desc(number_conflicts)) 

fat_conf$event_type <- as.factor(fat_conf$event_type)
fat_conf$year <- as.numeric(as.character(fat_conf$year))
fat_conf$number_conflicts <- as.numeric(as.character(fat_conf$number_conflicts))
fat_conf$fatalities <- as.numeric(as.character(fat_conf$fatalities))

plot_con <-fat_conf %>%         
        ggplot(aes(x= year, y= number_conflicts, fill = "red")) +
        geom_bar(stat = 'identity', position = position_stack(reverse = TRUE))+
        labs(x = "Years",
         y = "Conflicts",
        title = "Conflicts  over years") +
        guides(fill=FALSE)

plot_fat <-fat_conf %>%         
        ggplot(aes(x= year, y= fatalities)) +
        geom_bar(stat = 'identity', position = position_stack(reverse = TRUE))+
        labs(x = "Years",
             y = "Fatalities",
             title = "Fatalities  over years") +
        guides(fill=FALSE)

plot12 <- plot_grid(plot_con,plot_fat, align = "v", labels = "AUTO", ncol = 1)
plot12

################################################################################
## EXTRA
################################################################################
## Interactive Maps with the Fatalities 

plot13 <- leaflet() %>% addTiles() %>%
        addMarkers(data=df,
                   clusterOptions = markerClusterOptions(),
                   clusterId = "fatalities") %>%
        addEasyButton(easyButton(
                states = list(
                        easyButtonState(
                                stateName="unfrozen-markers",
                                icon="close",
                                title="Freeze Clusters",
                                onClick = JS("
                                             function(btn, map) {
                                             var clusterManager =
                                             map.layerManager.getLayer('cluster', 'warCluster');
                                             clusterManager.freezeAtZoom();
                                             popup=~as.character(fatalities);
                                             btn.state('frozen-markers');
                                             }")
      ),
      easyButtonState(
              stateName="frozen-markers",
              icon="close",
              title="UnFreeze Clusters",
              onClick = JS("
                           function(btn, map) {
                           var clusterManager =
                           map.layerManager.getLayer('cluster', 'warCluster');
                           clusterManager.unfreeze();
                           popup=~as.character(fatalities);
                           btn.state('unfrozen-markers');
                           }")
      )
              )
              ))
plot13



