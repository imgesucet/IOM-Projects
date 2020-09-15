#functions needed to obtain top search hits regarding migration around the world
# Imge Su Cetin


###############################################################
#Functions
#libraries needed 
library(shiny)
library(shinydashboard)
library(gtrendsR)
library(ggplot2)
library(stringr)
library(ggpubr)
library("tidyverse")
library(ggpubr)
library(shinythemes)

#iso data
iso <- read_csv("iso_country_lang_data.csv")
#data for world map
world<-map_data("world")

#top hits function
top.hits<- function(world,search_keyword,time,category=0){
  a<-gtrends(keyword = search_keyword,time = time,category = category)
  country.hits<-a$interest_by_country%>%
    filter(location %in% world$region, hits>0)%>%
    mutate(region = location, hits = as.numeric(hits))%>%
    select(region,hits)%>%
    ungroup()
  return(country.hits)
}

top.hits.map <- function(country.hits){
  ggplot() +
    geom_map(data = world,
             map = world,
             aes(x = long, y = lat, map_id = region),
             fill="#ffffff", color="#ffffff", size=0.15) +
    geom_map(data = country.hits,
             map = world,
             aes(fill = hits, map_id = region),
             color="#ffffff", size=0.15) +
    scale_fill_continuous(low = 'grey', high = 'red') +
    theme(axis.ticks = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank())
  
  
}

##data<-top.hits(world=world,search_keyword = "migration",time = "now 1-d",category = 555)
##top.hits.map(data)

##data
##is.data.frame(data)