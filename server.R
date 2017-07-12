shinyServer(function(input, output, session){
  
  get_redis_cache <- function(msg_id){
    data_df <- data_df_r()
    cache_data <- data_df[data_df$message_id==msg_id,]$message_cache[1]
    cache_data <- gsub("\n",'<br>',cache_data)
    cache_data <- paste0("<pre>",cache_data,"</pre>")
    print(typeof(cache_data))
    if(is.null(cache_data)){
      cache_data = 'No Cache Found'
    }
    return(cache_data)
  }
  
  all_stats_r <- reactive({
    all_stats <- fetch_elastic_stats(input$date, input$channel)
    return(all_stats)
  })
  
  all_message_r <- reactive({
    all_message <- fetch_elastic_message(input$date, input$channel)
    return(all_message)
  })
  
  data_df_r <- reactive({
    all_message <- all_message_r()
    data_df <- all_message$message_data
    updateSelectInput(session, "stories_input", label = NULL, choices =c("All",as.character(unique(data_df$story))), selected = "All")  # input$date and others are Date objects. When outputting
    updateSelectInput(session, "node", label = NULL, choices =as.character(unique(data_df$last_nodes)), selected = NULL)  # input$date and others are Date objects. When outputting
    updateSelectInput(session, "stop_logic", label = NULL, choices =as.character(unique(data_df$stop_logic_data)), selected = NULL)  # input$date and others are Date objects. When outputting
    updateSelectInput(session, "message_type", label = NULL, choices =as.character(unique(data_df$message_type_text)), selected = NULL)  # input$date and others are Date objects. When outputting
    updateSelectInput(session, "message_by", label = NULL, choices =as.character(unique(data_df$message_by)), selected = "User")  # input$date and others are Date objects. When outputting
    updateSelectInput(session, "include", label = NULL, choices = names(data_df), selected = default_columns)  # input$date and others are Date objects. When outputting
    updateSelectInput(session, "break_message_word_cloud", label = NULL, choices =as.character(unique(data_df$stop_logic_data)), selected =break_messages_type )  # input$date and others are Date objects. When outputting
    updateSelectInput(session, "node_word_cloud", label = NULL, choices =as.character(unique(data_df$last_nodes)), selected = NULL)  # input$date and others are Date objects. When outputting
    updateSelectInput(session, "stop_logic_story", label = NULL, choices =c("All",(as.character(unique(data_df$story)))), selected = "All")  # input$date and others are Date objects. When outputtin
    data_df[is.na(data_df)] <- "None"
    return(data_df)
  })
  
  ############################### Statistics ##################
  # total conversation
  total_conv <- reactive({
    all_stats <- all_stats_r()
    stats_df <- all_stats$stats_data
    unique_chats <- dcast(stats_df, coll_id + conversation_no ~ "sum")
    unique_chats$sum <-1
    total_conv <- sum(unique_chats['sum'])
    return(total_conv)})
  
  #total users
  total_users <-reactive({
    all_stats <- all_stats_r()
    stats_df <- all_stats$stats_data
    total_users <- length(unique(stats_df$coll_id))
    return(total_users)
  })
  
  #total gogo automation
  end_end_conv <-reactive({ 
    all_stats <- all_stats_r()
    stats_df <- all_stats$stats_data
    updateSelectInput(session, "stories", label = NULL, choices =as.character(unique(stats_df$story)), selected = stats_df$story[1])  # input$date and others are Date objects. When outputting
    stats_df_day <- group_by(stats_df,coll_id, conversation_no)
    stats_df_day <- summarize(stats_df_day,end_to_end_gogo_chat = min(end_to_end_gogo_chat), total_chats=1)
    end_end_conv <- round((sum(stats_df_day$end_to_end_gogo_chat)/sum(stats_df_day$total_chats))*100,2)
    return(end_end_conv)
  })
  
  #total gogo automation
  atleast_one_conv <-reactive({ 
    all_stats <- all_stats_r()
    stats_df <- all_stats$stats_data
    stats_df_day <- group_by(stats_df,coll_id, conversation_no)
    stats_df_day <- summarize(stats_df_day,atleast_one_gogo_response = max(atleast_one_gogo_response), total_chats=1)
    atleast_one_conv <- round((sum(stats_df_day$atleast_one_gogo_response)/sum(stats_df_day$total_chats))*100,2)
    return(atleast_one_conv)
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
      subtitle = paste("End-to-End ( ","At-least one - ",format(atleast_one_conv(),big.mark=",",scientific=FALSE),"% )",sep=""),
      icon = icon("download")
    )
  })
  
  break_conversations <- function(data_df){
    all_stats <- all_stats_r()
    stats_df <- all_stats$stats_data
    stats_df_day <- group_by(stats_df,coll_id, conversation_no)
    stats_df_day <- summarize(stats_df_day,end_to_end_gogo_chat = min(end_to_end_gogo_chat),total_chats = 1)
    break_df <- stats_df_day[stats_df_day$end_to_end_gogo_chat==0,]
    break_df$coll_conv <-paste(break_df$coll_id,"_",break_df$conversation_no) 
    data_df$coll_conv <-paste(data_df$coll_id,"_",data_df$conversation_no) 
    data_df <- data_df[data_df$coll_conv %in% break_df$coll_conv, ]
    data_df$bm <- 0
    included <- c()
    data_df_break<-data_df[data_df$message_by %in% c('User','Assistant'),]
    for(i in 2:nrow(data_df_break)){
      if((data_df_break$message_by[i]=='Assistant') & (data_df_break$coll_conv[i]==data_df_break$coll_conv[i-1]) & !(data_df_break$coll_conv[i] %in% included)){
        included <- c(included, data_df_break$coll_conv[i])
        data_df_break$bm[i-1] <-1 
      } 
    }
    data_df_break <- data_df_break[data_df_break$bm==1,]
    #data_df <- data_df[data_df$msg_id %in% data_df_break$msg_id,]
    return(data_df_break)
  }
  
  datos<- function(){
    story_ <- input$stories
    all_stats <- all_stats_r()
    stats_df_sub_story <- all_stats$stats_data
    story_count2 <- stats_df_sub_story
    story_count2 <- group_by(story_count2, coll_id, conversation_no, story, node)
    story_count2 <- summarize(story_count2,total_chats = 1,end_to_end_gogo_chat = min(end_to_end_gogo_chat))
    
    story_count2 <- group_by(story_count2, story, node)
    story_count2 <- summarize(story_count2,total_chats = sum(total_chats),end_to_end_gogo_chat = sum(end_to_end_gogo_chat))
    
    story_count2$end_to_end_gogo_chat <- round((story_count2$end_to_end_gogo_chat/story_count2$total_chats)*100,2)
    story_count2 <- data.frame(story=story_count2$story,node=story_count2$node,conversation=story_count2$total_chats,gogo_automation=story_count2$end_to_end_gogo_chat)
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
  output$table2 =  DT::renderDataTable(
    dataoutput(),class = 'cell-border stripe',rownames = FALSE,
    filter = 'top',
    options = list(
      autoWidth = TRUE,
      lengthChange = FALSE,
      columnDefs = list(list(width = '200px', targets = 1)),scrollX = TRUE
    ), escape = FALSE)
  
  output$table3 =   renderDataTable(
    datos(),options = list(
      autoWidth = TRUE,
      lengthChange = FALSE,
      iDisplayLength=10,
      sDom  = '<"top">lrt<"bottom">ip',
      columnDefs = list(list(width = '200px', targets = 1))
    )
  )
  story_input <- reactive({
    story_input <- input$stories_input
    
  })
  node<- reactive({
    data_df <- data_df_r()
    if(input$stories_input=="All"){
      updateSelectInput(session, "node", label = NULL, choices =as.character(unique(data_df$last_nodes)), selected = "All")  # input$date and others are Date objects. When outputting
    }
    else{
      updateSelectInput(session, "node", label = NULL, choices =as.character(unique(data_df[data_df$story==input$stories_input,]$last_node)), selected =NULL)  # input$date and others are Date objects. When outputting
    }})
  
  stop_logic_node <- reactive({
    data_df <- data_df_r()
    if(input$stop_logic_story=="All"){
      updateSelectInput(session, "node_word_cloud", label = NULL, choices =as.character(unique(data_df$last_nodes)), selected =NULL )  # input$date and others are Date objects. When outputting
    }
    else{
      updateSelectInput(session, "node_word_cloud", label = NULL, choices =as.character(unique(data_df[data_df$story==input$stop_logic_story,]$last_node)), selected =as.character(unique(data_df[data_df$story==input$stop_logic_story,]$last_node)))  # input$date and others are Date objects. When outputting
    }})
  
  dataoutput<-function(){
    node()
    data_df <- data_df_r()
    data_df$chat_link <- paste0("<a href='",  data_df$chat_link, "' target='_blank'>See Chats</a>")
    data_show_df <- data_df
    if(input$stories_input=="All"){
      df1 = data_show_df
    }
    else{
      df1 <- data_show_df[data_show_df$story==input$stories_input,]
    }
    if(!is.null(input$node)){
      df2 <- df1[df1$last_nodes %in% input$node,]
    }
    else{
      df2 <- df1
    }
    
    if(input$new_conversation){
      df3 <- df2[df2$new_conversation==TRUE,]
    }
    else{
      df3 <- df2
    }
    
    if(input$break_message){
      df4 <- break_conversations(df3)
    }
    else{
      df4 <- df3
    }
    if(!is.null(input$message_by)){
      df5 <- df4[df4$message_by==input$message_by,]
    }
    else{
      df5 <- df4
    }
    
    if(!is.null(input$stop_logic)){
      df6 <- df5[df5$stop_logic_data %in% input$stop_logic,]  
    }
    else{
      df6 <- df5
    }
    if(!is.null(input$message_type)){
      df7 <- df6[df6$message_type_text %in% input$message_type,]  
    }
    else{
      df7 <- df6
    }
    df7$last_nodes <- as.character(df7$last_nodes)
    df7$message_id <- as.character(df7$message_id)
    columns <- input$include
    df7 <- df7[,columns]
    df7 <- viewCache(df7)
    return(df7)
  }
  
  output$table2 =  renderDataTable(
    dataoutput(),class = 'cell-border stripe',rownames = FALSE,
    selection = list(mode = 'single'),
    options = list(
      autoWidth = TRUE,
      lengthChange = FALSE,
      columnDefs = list(list(width = '200px',targets = 1)),scrollX = TRUE
    ), escape = FALSE)
  
  output$table3 =  renderTable(
    datos(),digits = 0,include.rownames=FALSE
  )
  
  msg_id <- reactive({
    index <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
    return(index)})
  
  SelectedRow <- eventReactive(input$select_button,{
    get_redis_cache(msg_id())
  })
  
  
  observeEvent(input$select_button, {
    toggleModal(session, "modalExample", "open")
  })
  
  output$popup <- renderUI({
    bsModal("modalExample", "Message Cache", "BUTnew", size = "large",
            HTML(SelectedRow())
    )
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { paste('dataset.csv', sep='') },
    content = function(file) {
      p1 <- dataoutput()
      p1$save(file, standalone = TRUE)
    })
  
  
  output$chart <- renderChart({
    all_stats <- all_stats_r()
    stats_df_plot <- all_stats$plot_stats_data
    if(input$date %in% c('Last 1 Hour','Last 2 Hour','Last 4 Hour','Last 12 Hour')){
      stats_df_plot$created_at<- date_convertion_to_IST(stats_df_plot$created_at)
      stats_df_plot$created_at <- as.POSIXlt(stats_df_plot$created_at, format="%Y-%m-%d %H:%M:%S")
      stats_df_plot$hour <- stats_df_plot$created_at$hour
      stats_df_plot$date <- stats_df_plot$hour
    }
    else{
      stats_df_plot$created_at<- date_convertion_to_IST(stats_df_plot$created_at)
      stats_df_plot$created_at <- as.POSIXlt(stats_df_plot$created_at, format="%Y-%m-%d %H:%M:%S")
      stats_df_plot$date <- as.Date(stats_df_plot$created_at)
    }
    daily_stats <- stats_df_plot[c("date","coll_id","conversation_no","end_to_end_gogo_chat","atleast_one_gogo_response")]
    
    daily_stats <- group_by(daily_stats,date , coll_id, conversation_no)
    daily_stats <- summarize(daily_stats,users_count = n_distinct(coll_id), end_to_end_gogo_chat = min(end_to_end_gogo_chat),
                             atleast_one_gogo_response = max(atleast_one_gogo_response),
                             total_chats=1)
    
    daily_stats <- group_by(daily_stats, date)
    daily_stats <- summarize(daily_stats, users_count = sum(users_count, na.rm = T), total_chats = sum (total_chats, na.rm = T),
                             atleast_one_gogo_response = sum(atleast_one_gogo_response, na.rm=T),
                             end_to_end_gogo_chat = round((sum(end_to_end_gogo_chat)/sum(total_chats))*100,2))
    
    plot <- data.frame(date=daily_stats$date,conversation=daily_stats$total_chats,users=daily_stats$users_count,gogo_automation=daily_stats$end_to_end_gogo_chat, atlest_one_response=daily_stats$atleast_one_gogo_response )
    plot$users <- as.integer(plot$users)
    
    
    h <- Highcharts$new()
    h$chart(zoomType="xy")
    h$title(text="Performance - Past Days/Hours")
    h$subtitle(text="Total Conversations, Total Users Present, % Gogo Automation on daily/hourly basis")
    
    h$xAxis(categories = as.character(plot$date))
    h$yAxis(list(list(title = list(text = 'Conversation',style = list(color = "#4572A7")),labels=list(style = list(color = "#4572A7")))
                 , list(labels=list(style = list(color = "#89A54E")),title = list(text = 'Users',style = list(color = "#89A54E")), opposite = TRUE)
                 , list(labels=list(style = list(color = "#AA4643")),title = list(text = '% Gogo Automation',style = list(color = "#AA4643")), opposite = TRUE))
    )
    h$series(name = 'Total Chats', type = 'column', color = '#4572A7',
             data = plot$conversation)
    h$series(name = 'Chats with atleast 1 gogo response', type = 'column', color = '#BCC6CC',
             data = plot$atlest_one_response)
    h$series(name = 'Total users', type = 'spline', color = '#89A54E',
             data = plot$users,
             yAxis = 1)
    
    h$series(name = '% Gogo Automation', type = 'spline', color = '#AA4643',
             data = plot$gogo_automation,
             yAxis = 2)  
    h$addParams(dom = "chart")
    return(h)
  })
  
  story_count <- reactive({
    all_stats <- all_stats_r()
    stats_df <- all_stats$stats_data
    #stats_df_day <- stats_df[stats_df$date==as.character(input$date),]
    story_count <- stats_df
    story_count <- group_by(story_count,coll_id,conversation_no, story)
    story_count <- summarize(story_count,total_chats = 1,end_to_end_gogo_chat = min(end_to_end_gogo_chat))
    
    story_count <- group_by(story_count, story)
    story_count <- summarize(story_count,total_chats = sum(total_chats),end_to_end_gogo_chat = sum(end_to_end_gogo_chat))
    
    
    story_count$end_to_end_gogo_chat <- round((story_count$end_to_end_gogo_chat/story_count$total_chats)*100,2)
    story_count <- data.frame(story=story_count$story,conversation=story_count$total_chats,gogo_automation=story_count$end_to_end_gogo_chat)
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
    stop_logic_node()
    data_df <- data_df_r()
    df1 <- data_df[data_df$message_by=="User",]
    if(!is.null(node)) {
      df2 <- df1[df1$last_nodes %in% node,]
    }
    else{
      df2<-df1
    }
    if(!is.null(breakmessage)) {
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
    freq.df<-freq.df[((freq.df$word!="")&(freq.df$freq>=1)&(freq.df$word!="NA")),]
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
  
  #total story break
  mdata <-reactive({
    data_df <- data_df_r()
    if(input$stop_logic_story=="All"){
      b<-dcast(data_df,story+stop_logic_data~"count")
      b<-b[b$count>15,]
      mdata<-b[b$stop_logic_data %in% input$break_message_word_cloud,]
      mdata <-dcast(mdata, story~stop_logic_data)
    }
    else{
      b<-dcast(data_df,last_nodes+stop_logic_data~"")
      b<-b[b$last_nodes %in% input$node_word_cloud,]
      mdata<-b[b$stop_logic_data %in% input$break_message_word_cloud,]
      mdata <-dcast(mdata, last_nodes~stop_logic_data)
      mdata<- plyr::rename(mdata, c("last_nodes"="story"))
    }
    return(mdata)
  })
  
  output$pie_plot <- renderChart({ 
    stop_logic_node()
    bar_data <- mdata()
    a <- Highcharts$new()  
    a$title(text='Break Messages')
    a$chart(type = "column")
    a$xAxis(categories=bar_data$story)
    a$data(bar_data)  
    a$addParams(dom = "pie_plot")
    a$plotOptions(series=list(stacking="normal"))
    return(a)
  })
})

