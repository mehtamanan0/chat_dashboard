library(shiny)
library(shinydashboard)
library(plyr)
library(reshape2)
library(reshape)
library(wordcloud)
require('rCharts')
library('dplyr')
library(shinyjs)

Sys.setenv(TZ="Asia/Kolkata")

percent <- reactive({ 
 end_end_conv()
})
#DATA_DIRECTORY = '/home/ubuntu/dashboard_data/processed_data/'
DATA_DIRECTORY = 'processed_data/'

############ Needed to add View Buttons to table###########################
viewCache <- function(df){
  df$redis_cache <- paste0("<button id='button_",df$msg_id,"' type='button' class='btn btn-default action-button' onclick='Shiny.onInputChange(&quot;select_button&quot;,  this.id)'>View Cache</button>")
  return(df)
}
###########################################################################

########## SQL CONNECTION ################################
library(RMySQL)
con = dbConnect(MySQL(), user='haptik', password='Batman1305', dbname='mogambo_reporting', host='haptik-staging-3-read-replica.ckfxzl3qckrk.ap-south-1.rds.amazonaws.com')
#con <- dbConnect(SQLite(), "/home/haptik/Downloads/mydb")
##########################################################


################## Redis Connection ###########################
library(rredis)
#redisConnect(host = "", port=6379)
#redisSelect(1)
##############################################################



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
default_columns <- c("chat_links", "body", "story", "last_node", "domain_data", "stop_logic_data","msg_id")

## Date filter
date_filters <- c("Last 1 Hour", "Last 2 Hour", "Last 4 Hour", "Last 12 Hour", "Yesterday", "Last Week")

start_end_time<-function(date){
  hour = 3600
  curr_time <- as.POSIXlt(Sys.time())
  curr_time$min <- 0
  curr_time$sec <- 0 
  if(date=="Last 1 Hour"){
    start_time <- curr_time- hour*2
    end_time <- curr_time - hour*1
  }
  else if(date=="Last 2 Hour"){
    start_time <- curr_time- hour*3
    end_time <- curr_time - hour*1
  }
  else if(date=="Last 4 Hour"){
    start_time <- curr_time- hour*5
    end_time <- curr_time - hour*1
  }
  else if(date=="Last 12 Hour"){
    start_time <- curr_time- hour*13
    end_time <- curr_time - hour*1
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

get_all_channel<-function(){
  query <- "select distinct(channel) from daily_analysis"
  res<-dbSendQuery(con,query)
  data<-fetch(res,-1)
  return(data$channel)
}
