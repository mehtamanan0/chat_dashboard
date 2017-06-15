library(shiny)
library(shinydashboard)
library(plyr)
library(reshape2)
library(reshape)
library(wordcloud)
require('rCharts')
library('dplyr')
library(shinyjs)



#DATA_DIRECTORY = '/home/ubuntu/dashboard_data/processed_data/'
DATA_DIRECTORY = 'processed_data/'

############ Needed to add View Buttons to table###########################
viewCache <- function(df){
  df$redis_cache <- paste0("<button id='button_",df$msg_id,"' type='button' class='btn btn-default action-button' onclick='Shiny.onInputChange(&quot;select_button&quot;,  this.id)'>View Cache</button>")
  return(df)
}
###########################################################################

########## SQL CONNECTION ################################
library(RSQLite)
con <- dbConnect(SQLite(), "/home/haptik/Downloads/mydb")
##########################################################


################## Redis Connection ###########################
#library(rredis)
#redisConnect(host = "redis.haptikdev.com", port=6379)
#redisSelect(1)
##############################################################

#get reddis cache
get_redis_cache <- function(msg_id){
  redis_keys <- paste(":1:production|v1|ml_message|",msg_id,sep="")
  data <- redisGet(redis_keys)
  return(data)
}

# read channel df
channel_data_df <-function(date,channel){
  duration <- start_end_time(date)
  query <- paste("SELECT * from data where channel='", channel,"' and created_at >= '",duration[1],"' and created_at <= '",duration[2],"'",sep="")
  res <- dbSendQuery(con, query)
  channel_data_df <- fetch(res)
  channel_data_df$body = as.character(channel_data_df$body)
  return(channel_data_df)
}

channel_daily_stats_df <-function(channel, date){
  duration <- start_end_time(date)
  query <- paste("SELECT * from daily_analysis where channel='", channel,"' and created_at >= '",duration[1],"' and created_at <= '",duration[2],"'",sep="")
  res <- dbSendQuery(con, query)
  channel_daily_stats_df <- fetch(res)
  return(channel_daily_stats_df)
}

channel_daily_stats_df_for_plot <-function(channel, date){
  duration <- stats_start_end_time(date)
  query <- paste("SELECT * from daily_analysis where channel='", channel,"' and created_at >= '",duration[1],"' and created_at <= '",duration[2],"'",sep="")
  res <- dbSendQuery(con, query)
  channel_daily_stats_df_for_plot <- fetch(res)
  return(channel_daily_stats_df_for_plot)
}


######## wordcloud#################
wordcloudentity<-function(freq.df)
{
  df <- freq.df 
  pal2 <- brewer.pal(8,"Dark2")
  wordcloud(df$word,df$freq,max.words=200,min.freq = 1,random.order = F, rot.per=.25, colors=pal2)}
#################################



#####break message type
break_messages_type<-c("True_no_nodes","True_trash_detected","True_nothing_changed","True_negation","True_botbreak")
##########


#### Default columns to select##
default_columns <- c("chat_links", "body", "story", "last_node", "domain_data", "stop_logic_data","msg_id")

## Date filter
date_filters <- c("Last 1 Hour", "Last 2 Hour", "Last 4 Hour", "Last 12 Hour", "Yesterday", "Last Week")

start_end_time<-function(date){
  hour = 3600
  curr_time <- as.POSIXlt(Sys.time())
  curr_time$min <- 0
  curr_time$sec <- 0 
  if(date=="Last 1 Hour"){
    start_time <- curr_time- hour*1
    end_time <- curr_time - hour*0
  }
  else if(date=="Last 2 Hour"){
    start_time <- curr_time- hour*2
    end_time <- curr_time - hour*0
  }
  else if(date=="Last 4 Hour"){
    start_time <- curr_time- hour*4
    end_time <- curr_time - hour*0
  }
  else if(date=="Last 12 Hour"){
    start_time <- curr_time- hour*12
    end_time <- curr_time - hour*0
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
    start_time <- curr_time- hour*12
    end_time <- curr_time - hour*0
  }
  else {
    curr_day =  as.Date(curr_time)
    start_time = curr_day - 8
    end_time <- curr_day
  }
  return(c(start_time, end_time))
  
}

