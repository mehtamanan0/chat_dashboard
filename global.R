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
library(elastic)
library(DT)


source("elastic_data.R")
source("config.R")


############ Needed to add View Buttons to table###########################
viewCache <- function(df){
  df$redis_cache <- paste0("<button id='button_",df$msg_id,"' type='button' class='btn btn-default action-button' onclick='Shiny.onInputChange(&quot;select_button&quot;,  this.id)'>View Cache</button>")
  return(df)
}
###########################################################################

# Elastic Connection
connect(es_host = ELASTIC_AWS_HOST, es_path = "", es_port = ELASTIC_PORT, es_transport_schema  = "https")

date_convertion_to_IST <- function(date_string){
  date_string <- strftime(strptime(date_string, format="%Y-%m-%dT%H:%MZ"),"%Y-%m-%d %H:%M:%S")
  utc_date_dtring <- as.POSIXct(date_string, tz="UTC")
  ist_date_string <- format(utc_date_dtring, tz="Asia/Kolkata",usetz=TRUE)
  ist_date_string<- strftime(ist_date_string, "%Y-%m-%d %H:%M:%S")
  return(ist_date_string)
}


# read channel df
channel_data_df <-function(date,channel){
  duration <- start_end_time(date)
  query <- message_query_generator(channel, duration[1], duration[2])
  channel_data_df <- elastic_get_data(MESSAGE_KINESIS_INDEX, MESSAGE_KINESIS_TYPE, query, 1000)
  channel_data_df$created_at <- date_convertion_to_IST(channel_data_df$created_at)
  channel_data_df <- plyr::rename(channel_data_df, c("business_via_name"="channel"))
  return(channel_data_df)
}

channel_daily_stats_df <-function(channel, date){
  duration <- start_end_time(date)
  query <- stats_query_generator(channel, duration[1], duration[2])
  channel_daily_stats_df <- elastic_get_data(STATS_KINESIS_INDEX, STATS_KINESIS_TYPE, query, 1000)
  channel_daily_stats_df$created_at <- date_convertion_to_IST(channel_daily_stats_df$created_at)
  channel_daily_stats_df <- plyr::rename(channel_daily_stats_df, c("business_via_name"="channel"))
  channel_daily_stats_df <- plyr::rename(channel_daily_stats_df, c("conversation_no"="conv_no"))
  return(channel_daily_stats_df)
}

channel_daily_stats_df_for_plot <-function(channel, date){
  duration <- stats_start_end_time(date)
  query <- stats_query_generator(channel, duration[1], duration[2])
  channel_daily_stats_df_for_plot <- elastic_get_data(STATS_KINESIS_INDEX, STATS_KINESIS_TYPE, query, 1000)
  channel_daily_stats_df_for_plot$created_at <- date_convertion_to_IST(channel_daily_stats_df_for_plot$created_at)
  channel_daily_stats_df_for_plot <- plyr::rename(channel_daily_stats_df_for_plot, c("business_via_name"="channel"))
  channel_daily_stats_df_for_plot <- plyr::rename(channel_daily_stats_df_for_plot, c("conversation_no"="conv_no"))
  return(channel_daily_stats_df_for_plot)
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
date_filters <- c("Last 12 Hour", "Last 2 Hour", "Last 4 Hour", "Last 1 Hour", "Yesterday", "Last Week")

start_end_time<-function(date){
  hour = 3600
  
  if(date=="Last 1 Hour"){
    end_time = strftime((as.POSIXlt(Sys.time(), "UTC", "%Y-%m-%dT%H:%M:%S")), "%Y-%m-%dT%H:%MZ")
    start_time = strftime((as.POSIXlt(Sys.time()-hour*1, "UTC", "%Y-%m-%dT%H:%M:%S")), "%Y-%m-%dT%H:%MZ")
  }
  else if(date=="Last 2 Hour"){
    end_time = strftime((as.POSIXlt(Sys.time(), "UTC", "%Y-%m-%dT%H:%M:%S")), "%Y-%m-%dT%H:%MZ")
    start_time = strftime((as.POSIXlt(Sys.time()-hour*2, "UTC", "%Y-%m-%dT%H:%M:%S")), "%Y-%m-%dT%H:%MZ")
  }
  else if(date=="Last 4 Hour"){
    end_time = strftime((as.POSIXlt(Sys.time(), "UTC", "%Y-%m-%dT%H:%M:%S")), "%Y-%m-%dT%H:%MZ")
    start_time = strftime((as.POSIXlt(Sys.time()-hour*4, "UTC", "%Y-%m-%dT%H:%M:%S")), "%Y-%m-%dT%H:%MZ")
  }
  else if(date=="Last 12 Hour"){
    end_time = strftime((as.POSIXlt(Sys.time(), "UTC", "%Y-%m-%dT%H:%M:%S")), "%Y-%m-%dT%H:%MZ")
    start_time = strftime((as.POSIXlt(Sys.time()-hour*12, "UTC", "%Y-%m-%dT%H:%M:%S")), "%Y-%m-%dT%H:%MZ")
  }
  else if(date=="Yesterday"){
    curr_day =  as.Date(curr_time)
    start_time = curr_day -1
    end_time <- curr_day
  }
  else if(date=="Last Week"){
    curr_day =  as.Date(curr_time)
    start_time = curr_day -8
    end_time <- curr_day
  }
  return(c(start_time, end_time))
}

stats_start_end_time <- function(date){
  hour = 3600
  curr_time <- as.POSIXlt(Sys.time())
  curr_time$min <- 0
  curr_time$sec <- 0 
  if(date %in% c('Last 1 Hour','Last 2 Hour','Last 4 Hour','Last 12 Hour')){
    end_time = strftime((as.POSIXlt(Sys.time(), "UTC", "%Y-%m-%dT%H:%M:%S")), "%Y-%m-%dT%H:%MZ")
    start_time = strftime((as.POSIXlt(Sys.time()-hour*12, "UTC", "%Y-%m-%dT%H:%M:%S")), "%Y-%m-%dT%H:%MZ")
  }
  else {
    curr_day =  as.Date(curr_time)
    start_time = curr_day - 8
    end_time <- curr_day
  }
  return(c(start_time, end_time))
}




