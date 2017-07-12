library(shiny)
library(shinydashboard)
library(plyr)
library(reshape2)
library(reshape)
library(wordcloud)
require('rCharts')
library('dplyr')
library(shinyjs)
library(jsonlite)
library(DT)
library(httr)
library(shinyBS)



############ Needed to add View Buttons to table###########################
viewCache <- function(df){
  df$redis_cache <- paste0("<button id='button_",df$message_id,"' type='button' class='btn btn-default action-button' onclick='Shiny.onInputChange(&quot;select_button&quot;,  this.id)'>View Cache</button>")
  return(df)
}
###########################################################################


date_convertion_to_IST <- function(date_string){
  date_string <- strftime(strptime(date_string, format="%Y-%m-%dT%H:%MZ"),"%Y-%m-%d %H:%M:%S")
  utc_date_dtring <- as.POSIXct(date_string, tz="UTC")
  ist_date_string <- format(utc_date_dtring, tz="Asia/Kolkata",usetz=TRUE)
  ist_date_string<- strftime(ist_date_string, "%Y-%m-%d %H:%M:%S")
  return(ist_date_string)
}

fetch_elastic_stats <- function(date,channel){
  data = list(
    channel=channel,
    date_text=date 
  )
  res <- POST("http://ip-172-31-44-20.ap-south-1.compute.internal/mogambo_api/analytics/fetch_stats", body = data, encode = "form", verbose())
  data <- fromJSON(content(res, "text", encoding='UTF-8'))
  return(data)
}

fetch_elastic_message <- function(date,channel){
  data = list(
    channel=channel,
    date_text=date 
  )
  res <- POST("http://ip-172-31-44-20.ap-south-1.compute.internal/mogambo_api/analytics/fetch_messages", body = data, encode = "form", verbose())
  data <- fromJSON(content(res, "text", encoding='UTF-8'))
  return(data)
}

######## wordcloud#################
wordcloudentity<-function(freq.df)
{
  df <- freq.df 
  pal2 <- brewer.pal(8,"Dark2")
  wordcloud(df$word,df$freq,max.words=200,min.freq = 1,random.order = F, rot.per=.25, colors=pal2)}
#################################

break_messages_type<-c("True_no_nodes","True_trash_detected","True_nothing_changed","True_negation","True_botbreak")
##########


#### Default columns to select##
default_columns <- c("chat_link", "body", "story", "last_nodes", "predicted_domain", "stop_logic_data","message_id")


## Date filter
date_filters <- c( "Last 1 Hour", "Last 2 Hour", "Last 4 Hour", "Last 12 Hour", "Yesterday", "Last Week")



channels_list <- c("flightschannel","reminderschannel","aroundmechannel","trainschannel","cabschannel","rechargechannel")
