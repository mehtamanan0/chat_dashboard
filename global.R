library(shiny)
library(shinydashboard)
library(plyr)
library(reshape2)
library(reshape)
library(wordcloud)
require('rCharts')
library('dplyr')


DATA_DIRECTORY = '/home/ubuntu/dashboard/processed_data/'

# channel
channel<-reactive({
  channel <-input$channel})

# date
date<- reactive({
  date<-input$date})

# get date string for today
get_today_date_string <- function(){
  format(Sys.time(), "%Y-%m-%d")
}

# get data filename for given channel
get_data_filename_for_channel<-function(date,channel){
  filename <- paste0(DATA_DIRECTORY, date,'-',channel,'-data.csv')
  return(filename)
}

# get daily stats filename for channel
get_data_analysis_filename<-function(channel){
  filename <- paste0(DATA_DIRECTORY, channel,'-daily_analysis.csv')
  return(filename)
}

# read channel df
channel_data_df <-function(date,channel){
  channel_data_filename <- get_data_filename_for_channel(date, channel)
  channel_data_df <- read.csv(channel_data_filename)
  channel_data_df$body = as.character(channel_data_df$body)
  channel_data_df$count <- 1
  return(channel_data_df)
}

channel_daily_stats_df <-function(channel){
  channel_daily_stats_filename <- get_data_analysis_filename(channel)
  channel_daily_stats_df <- read.csv(channel_daily_stats_filename)
  return(channel_daily_stats_df)
}

####################### dataframe ########################
#channels data
data_df <- channel_data_df("2017-05-16","cabschannel")


# daily stats for given channel
stats_df <-channel_daily_stats_df("cabschannel")

# daily stats df for given date 
stats_df_day <- stats_df[stats_df$date=="2017-05-16",]
############################################################


############################### Statistics ##################
# total conversation
total_conv <- sum(stats_df_day['total_chats'])

#total users
total_users <-length(unique(data_df$coll_id))

#total gogo automation
end_end_conv <- round((sum(stats_df_day['end_to_end_chats'])/sum(stats_df_day['total_chats']))*100,2)
##########################################################################################################



#################### Show data table ########################## 
columns <- c("chat_links","coll_id","conv_no","body","message_by","message_type_text","new_conv","node_data","detection_method","stop_logic_data",
             "story")

data_df$chat_links <- paste0("<a href='",  data_df$chat_links, "' target='_blank'>See Chats</a>")
data_show_df <- data_df[,columns]
################################################################

min_date_range <- as.Date(Sys.time()) - 10
stats_df$date = strftime(stats_df$date,"%d/%m/%Y")
daily_stats = stats_df[stats_df$date >= min_date_range,]
daily_stats <- group_by(stats_df, date)
daily_stats <- summarize(daily_stats, users_count = mean(users_count, na.rm = T), total_chats = sum (total_chats, 
                        na.rm = T),end_to_end_chats = round((sum(end_to_end_chats)/sum(total_chats))*100,2))
plot <- data.frame(date=daily_stats$date,conversation=daily_stats$total_chats,users=daily_stats$users_count,gogo_automation=daily_stats$end_to_end_chats)


######################## story_wise_analysis ###################
conv_story <- dcast(data_df,coll_id + conv_no + story + breaked_conv ~ "count1",value.var = "count", fun.aggregate = sum)
conv_story$count1 <- 1
story_count <- dcast(conv_story, story + breaked_conv ~"value", value.var = "count1", fun.aggregate = sum)
story_count <- story_count[story_count$story!="",]
story_count <- cast(story_count, story~breaked_conv,sum)

story_count$False = as.numeric(story_count$False) 
story_count$True = as.numeric(story_count$True)
story_count$total_conv <- story_count$False + story_count$True
story_count$gogo_automation <- (story_count$False/story_count$total_conv)*100
story_count <- plyr::rename(story_count, c("False"="#Gogo Chat", "True"="#Chats Breaked","total_conv" = "Total Conversations","gogo_automation"="%Gogo Automate"))
columns <- c("story","Total Conversations","#Gogo Chat","#Chats Breaked","%Gogo Automate")
story_count <- story_count[,columns] 
story_count <- story_count[order(-story_count$`Total Conversations`),]
##########################################################################



############################### Sub story wise analysis ####################
conv_story2 <- dcast(data_df,coll_id + conv_no + story + node_data + breaked_conv ~ "count1",value.var = "count", fun.aggregate = sum)
conv_story2$count1 <- 1
story_count2 <- dcast(conv_story2, story + node_data + breaked_conv ~"value", value.var = "count1", fun.aggregate = sum)
story_count2 <- cast(story_count2, story+node_data~breaked_conv,sum)
story_count2 <- story_count2[story_count2$node_data!="",]

story_count2$False = as.numeric(story_count2$False) 
story_count2$True = as.numeric(story_count2$True)
story_count2$total_conv <- story_count2$False + story_count2$True
story_count2$gogo_automation <- (story_count2$False/story_count2$total_conv)*100
columns <- c("story","Node","Total Conversations","#Gogo Chat","#Chats Breaked","%Gogo Automate")
story_count2 <- plyr::rename(story_count2, c("node_data"="Node","False"="#Gogo Chat", "True"="#Chats Breaked","total_conv" = "Total Conversations","gogo_automation"="%Gogo Automate"))
story_count2 <- story_count2[,columns] 
story_count2 <- story_count2[order(-story_count2$`Total Conversations`),]
##############################################################################




######## wordcloud#################
wordcloudentity<-function(freq.df)
{
  df <- freq.df 
  pal2 <- brewer.pal(8,"Dark2")
  wordcloud(df$word,df$freq,max.words=200,min.freq = 1,random.order = F, rot.per=.25, colors=pal2)}
#################################



#####break message type
break_messages_type<-c("True_no_nodes","True_trash_detected","True_nothing_changed","True_negation")
##########