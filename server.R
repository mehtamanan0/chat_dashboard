
shinyServer(function(input, output, session){
  
  data_df_r <- reactive({
    data_df <- channel_data_df(as.character(input$date),input$channel)
    updateSelectInput(session, "stories_input", label = NULL, choices =c("All",as.character(unique(data_df$story))), selected = "All")  # input$date and others are Date objects. When outputting
    updateSelectInput(session, "node", label = NULL, choices =as.character(unique(data_df$node_data)), selected = NULL)  # input$date and others are Date objects. When outputting
    updateSelectInput(session, "stop_logic", label = NULL, choices =c("All",as.character(unique(data_df$stop_logic_data))), selected = NULL)  # input$date and others are Date objects. When outputting
    updateSelectInput(session, "message_by", label = NULL, choices =as.character(unique(data_df$message_by)), selected = NULL)  # input$date and others are Date objects. When outputting
    return(data_df)
  })
  stats_df_r <-reactive({
    stats_df_r <- channel_daily_stats_df(input$channel)
    })
  
  ############################### Statistics ##################
  # total conversation
  total_conv <- reactive({
    stats_df <- stats_df_r()
    stats_df_day <- stats_df[stats_df$date==as.character(input$date),]
    total_conv <- sum(stats_df_day['total_chats'])
    return(total_conv)})
  
  #total users
  total_users <-reactive({
    data_df <- data_df_r()
    total_users <- length(unique(data_df$coll_id))
    return(total_users)
    })
  
  #total gogo automation
  end_end_conv <-reactive({ 
    stats_df <-stats_df_r()
    updateSelectInput(session, "stories", label = NULL, choices =as.character(unique(stats_df$story)), selected = stats_df$story[1])  # input$date and others are Date objects. When outputting
    stats_df_day <- stats_df[stats_df$date==as.character(input$date),]
    end_end_conv <- round((sum(stats_df_day['end_to_end_chats'])/sum(stats_df_day['total_chats']))*100,2)
    return(end_end_conv)
  })
  ##########################################################################################################
  
  output$total_conv <- renderValueBox({
    valueBox(
      value = format(total_conv(),big.mark=",",scientific=FALSE),
      subtitle = "Total Conversations",
      icon = icon("list")
    )
  })
  output$users <- renderValueBox({
    valueBox(
      value = format(total_users(),big.mark=",",scientific=FALSE),
      subtitle = "Total Unique Users",
      icon = icon("users")
    )
  })
  output$automation <- renderValueBox({
    valueBox(
      value = paste(format(end_end_conv(),big.mark=",",scientific=FALSE),"%"),
      subtitle = "End to End Gogo Conversations",
      icon = icon("download")
    )
  })
  

updateSelectInput(session, "stories", label = NULL, choices =as.character(unique(story_count$story)), selected = story_count$story[1])  # input$date and others are Date objects. When outputting
updateSelectInput(session, "stories_input", label = NULL, choices =c("All",as.character(unique(data_df$story))), selected = "All")  # input$date and others are Date objects. When outputting
updateSelectInput(session, "node", label = NULL, choices =as.character(unique(data_df$node_data)), selected = NULL)  # input$date and others are Date objects. When outputting
updateSelectInput(session, "stop_logic", label = NULL, choices =c("All",as.character(unique(data_df$stop_logic_data))), selected = NULL)  # input$date and others are Date objects. When outputting
updateSelectInput(session, "message_by", label = NULL, choices =as.character(unique(data_df$message_by)), selected = NULL)  # input$date and others are Date objects. When outputting

updateSelectInput(session, "break_message_word_cloud", label = NULL, choices =c("All",as.character(unique(data_df$stop_logic_data))), selected = "All")  # input$date and others are Date objects. When outputting
updateSelectInput(session, "node_word_cloud", label = NULL, choices =as.character(unique(data_df$node_data)), selected = as.character(data_df$node_data[1]))  # input$date and others are Date objects. When outputting



datos<- function(){
  story_ <- input$stories
  date_  <- as.character(input$date)
  stats_df <-stats_df_r()
  stats_df_day <- stats_df[stats_df$date==date_,]
  story_count2 <- stats_df_day
  story_count2 <- group_by(story_count2, story, node)
  story_count2 <- summarize(story_count2,total_chats = sum (total_chats, na.rm = T),end_to_end_chats = sum(end_to_end_chats))
  story_count2$end_to_end_chats <- round((story_count2$end_to_end_chats/story_count2$total_chats)*100,2)
  story_count2 <- data.frame(story=story_count2$story,node=story_count2$node,conversation=story_count2$total_chats,gogo_automation=story_count2$end_to_end_chats)
  columns <- c("story","Node","Total Conversations","%Gogo Automate")
  story_count2 <- plyr::rename(story_count2, c("node"="Node","conversation"="Total Conversations","gogo_automation"="%Gogo Automate"))
  story_count2 <- story_count2[,columns] 
  story_count2 <- story_count2[order(-story_count2$`Total Conversations`),]
  if(story_ =="All"){
    df<-subset(story_count2, select = -c(story) )
  }
  else{
    df <-story_count2[story_count2$story==story_,]
    df <-subset(df, select = -c(story) )
  }
  return(df)
}

output$table3 =  renderTable(
  datos(),digits = 0,include.rownames=FALSE
)
story_input <- reactive({
  story_input <- input$stories_input
  
})

node<- reactive({
  data_df <- data_df_r()
  if(input$stories_input=="All"){
  updateSelectInput(session, "node", label = NULL, choices =as.character(unique(data_df$node_data)), selected = "All")  # input$date and others are Date objects. When outputting
}
else{
  updateSelectInput(session, "node", label = NULL, choices =as.character(unique(data_df[data_df$story==input$stories_input,]$node_data)), selected =NULL)  # input$date and others are Date objects. When outputting
}})

dataoutput<-function(){
  node()
  data_df <- data_df_r()
  columns <- c("chat_links","coll_id","conv_no","body","message_by","message_type_text","new_conv","node_data","detection_method","stop_logic_data",
               "story")
  data_df$chat_links <- paste0("<a href='",  data_df$chat_links, "' target='_blank'>See Chats</a>")
  data_show_df <- data_df[,columns]
 
  if(input$stories_input=="All"){
    df1 = data_show_df
  }
  else{
    df1 <- data_show_df[data_show_df$story==input$stories_input,]
  }
  if(!is.null(input$node)){
    df2 <- df1[df1$node_data==input$node,]
  }
  else{
    df2 <- df1
  }
  if(!is.null(input$message_by)){
    df3 <- df2[df2$message_by==input$message_by,]
  }
  else{
    df3 <- df2
  }
  if(input$new_conversation){
    df4 <- df3[df3$new_conv=="True",]
  }
  else{
    df4 <- df3
  }
  if(input$break_message){
    df5 <- df4[df4$stop_logic_data %in% break_messages_type,]
    updateSelectInput(session, "stop_logic", label = NULL, choices =c("All",as.character(unique(df5$stop_logic_data))), selected = "All")
  }
  else{
    updateSelectInput(session, "stop_logic", label = NULL, choices =c("All",as.character(unique(df4$stop_logic_data))), selected = "All")
    if(input$stop_logic!="All"){
      df5 <- df4[df4$stop_logic_data == input$stop_logic,]  
    }
    else{
      df5 <- df4
    }
  }
  return(df5)
}


output$table2 =  DT::renderDataTable(
  dataoutput(),class = 'cell-border stripe',rownames = FALSE,options = list(
    autoWidth = TRUE,
    columnDefs = list(list(width = '200px', targets = 3)),scrollX = TRUE
  ), escape = FALSE)




  output$downloadData <- downloadHandler(
  filename = function() { paste('dataset', '.csv', sep='') },
  content = function(file) {
    write.csv(dataoutput(), file)
  })
  

output$chart <- renderChart({
   stats_df <-stats_df_r()
   min_date_range <- as.Date(Sys.time()) - 10
   stats_df$date = strftime(stats_df$date,"%d/%m/%Y")
   daily_stats = stats_df[stats_df$date >= min_date_range,]
   daily_stats <- group_by(stats_df, date)
   daily_stats <- summarize(daily_stats, users_count = mean(users_count, na.rm = T), total_chats = sum (total_chats, 
                                                                                                       na.rm = T),end_to_end_chats = round((sum(end_to_end_chats)/sum(total_chats))*100,2))
   plot <- data.frame(date=daily_stats$date,conversation=daily_stats$total_chats,users=daily_stats$users_count,gogo_automation=daily_stats$end_to_end_chats)
  
  
  
  
    h <- Highcharts$new()
    h$chart(zoomType="xy")
    h$title(text="Channel Performance - Last 7 Days")
    h$subtitle(text="Total Conversations, Total Users Present, % Gogo Automation on daily basis")
    h$xAxis(categories = as.character(plot$date),crosshair = TRUE)
    h$yAxis(list(list(title = list(text = 'Conversation',style = list(color = "#4572A7")),labels=list(style = list(color = "#4572A7")))
                 , list(labels=list(style = list(color = "#89A54E")),title = list(text = 'Users',style = list(color = "#89A54E")), opposite = TRUE)
                 , list(labels=list(style = list(color = "#AA4643")),title = list(text = '% Gogo Automation',style = list(color = "#AA4643")), opposite = TRUE))
    )
    h$series(name = 'Conversations', type = 'column', color = '#4572A7',
             data = plot$conversation)
    h$series(name = 'Users', type = 'spline', color = '#89A54E',
             data = plot$users,
             yAxis = 1)
    
    h$series(name = '% Gogo Automation', type = 'spline', color = '#AA4643',
             data = plot$gogo_automation,
             yAxis = 2)  
    h$addParams(dom = "chart")
    return(h)
})

story_count <- reactive({
  stats_df <-stats_df_r()
  stats_df_day <- stats_df[stats_df$date==as.character(input$date),]
  story_count <- stats_df_day
  story_count <- group_by(story_count, story)
  story_count <- summarize(story_count,total_chats = sum (total_chats, na.rm = T),end_to_end_chats = sum(end_to_end_chats))
  story_count$end_to_end_chats <- round((story_count$end_to_end_chats/story_count$total_chats)*100,2)
  story_count <- data.frame(story=story_count$story,conversation=story_count$total_chats,gogo_automation=story_count$end_to_end_chats)
  columns <- c("story","Total Conversations","%Gogo Automate")
  story_count <- plyr::rename(story_count, c("conversation"="Total Conversations","gogo_automation"="%Gogo Automate"))
  story_count <- story_count[,columns] 
  story_count <- story_count[order(-story_count$`Total Conversations`),]
  return(story_count)
})

output$table1 =  renderTable(
  story_count(),digits = 0,include.rownames=FALSE
)

get_word_cloud_table <- function(ngram,node,breakmessage){
  data_df <- data_df_r()
  df1 <- data_df[data_df$message_by=="User",]
  if(node != "All") {
    df2 <- df1[df1$node_data %in% node,]
  }
  else{
    df2<-df1
  }
  if(breakmessage != "All") {
    df3 <- df2[df2$stop_logic_data == breakmessage,]
  }
  else{
    df3<-df2
  }
  if(ngram=="Unigram"){
    df4<-df3[df3$unigram!="",]$unigram
  }
  else if(ngram=="Bigram"){
    df4<-df3[df3$bigram!="",]$bigram
  }
  else{
    df4<-df3[df3$trigram!="",]$trigram
  }
  a<-df4
  k <- paste(as.character(a), sep=",", collapse=",")
  r <- strsplit(k,",")
  df = data.frame(word = r)
  df$freq <- rep(1,nrow(df))
  names(df) <- c("word","freq")
  freq.df = dcast(df, word~"freq", value.var = "freq", fun.aggregate = sum)
  freq.df<-freq.df[((freq.df$word!="")&(freq.df$freq>1)),]
  print(head(freq.df))
  freq.df <- freq.df[order(-freq.df$freq),]
  return(freq.df)
}

ngram <- reactive({
  ngram <-input$ngram
})

output$wordcloud_plot <-renderPlot({
  ngram <- ngram()
  node <- input$node_word_cloud
  break_message <- input$break_message_word_cloud
  freq.df = get_word_cloud_table(ngram,node,break_message)
  wordcloudentity(freq.df)
})

output$wordTable <-renderTable(
  get_word_cloud_table(ngram(),input$node_word_cloud,input$break_message_word_cloud),include.rownames=FALSE,digits=0)


})

