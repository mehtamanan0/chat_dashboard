shinyServer(function(input, output, session){
  
  get_redis_cache <- function(msg_id){
    data_df <- data_df_r()
    cache_data <- data_df[data_df$message_id==msg_id,]$message_cache[1]
    if(is.null(cache_data)){
      cache_data = 'No Cache Found'
    }
    return(cache_data)
  }
  
  all_stats_r <- reactive({
    all_stats <- fetch_elastic_stats(input$date, input$channel)
    updateSelectInput(session, "stories", label = NULL, choices =c("All",as.character(unique(all_stats$stories_stats$story))), selected = "All") 
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
    data_df$last_nodes <- as.character(data_df$last_nodes)
    data_df$message_id <- as.character(data_df$message_id)
    data_df[is.na(data_df)] <- "None"
    return(data_df)
  })
  
  ############################### Statistics ##################
  # total conversation
  total_conv <- reactive({
    all_stats <- all_stats_r()
    return(all_stats$total_conversation)
    })
  
  #total users
  total_users <-reactive({
    all_stats <- all_stats_r()
    return(all_stats$total_users)
  })
  
  #total gogo automation
  end_end_conv <-reactive({ 
    all_stats <- all_stats_r()
    return(all_stats$end_to_end_gogo_percent)
  })
  
  #total gogo automation
  atleast_one_conv <-reactive({ 
    all_stats <- all_stats_r()
    return(all_stats$atleast_one_gogo_percent)
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
    breaked_coll_conv <- all_stats$breaked_conv
    data_df$coll_conv <-paste(data_df$coll_id,"_",data_df$conversation_no,sep="") 
    data_df <- data_df[data_df$coll_conv %in% breaked_coll_conv, ]
    data_df <- data_df[with(data_df, order(message_id)), ]
    coll_conv <- unique(data_df$coll_conv)
    break_message_ids = c()
    for(i in 1:length(coll_conv)){
      break_conv_df <- data_df[data_df$coll_conv==coll_conv[i],]
      for(j in 2:nrow(break_conv_df)){
        if(!(is.na(break_conv_df$message_by[j]))&(break_conv_df$message_by[j]=='Assistant')){
          break_message_ids <- c(break_message_ids,break_conv_df$message_id[j-1])
          break
        } 
      }
    }
    data_df <- data_df[data_df$message_id %in% break_message_ids,]
    return(data_df)
  }
  
  datos<- function(){
    story_ <- input$stories
    all_stats <- all_stats_r()
    story_count2 <-all_stats$nodes_stats
    story_count2 <- data.frame(story=story_count2$story,node=story_count2$node,conversation=story_count2$total_chats,gogo_automation=story_count2$end_to_end_gogo_chat, users = story_count2$users)
    story_count2 <- plyr::rename(story_count2, c("node"="Node","conversation"="Total Conversations","gogo_automation"="%Gogo Automate","users"="Total Users"))
    columns <- c("story","Node","Total Conversations","%Gogo Automate", "Total Users")
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
            HTML(paste('<div id="test"></div> <link rel="stylesheet" href="styles.css">
               <script type="text/javascript" src="renderjson.js"></script>
               <script>
               var json = ',SelectedRow(),';
               renderjson.set_show_to_level(1);
               document.getElementById("test").appendChild(renderjson(json));
               </script>'))
    )
    
  })
  
  
  output$downloadData <- downloadHandler(
    filename = function() { paste('dataset', '.csv', sep='') },
    content = function(file) {
      write.csv(dataoutput(), file)
    })
  
  output$chart <- renderChart({
    all_stats <- all_stats_r()
    daily_stats <- all_stats$history_chart_data
    print(daily_stats)
    plot <- data.frame(date=daily_stats$date,conversation=daily_stats$total_chats,users=daily_stats$users,gogo_automation=daily_stats$gogo_automation, atlest_one_response=daily_stats$atlest_one_response )
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
    story_count <- all_stats$stories_stats
    story_count <- data.frame(story=story_count$story,conversation=story_count$total_chats,gogo_automation=story_count$end_to_end_gogo_chat, users = story_count$users)
    columns <- c("story","Total Conversations","%Gogo Automate","Total Users")
    story_count <- plyr::rename(story_count, c("conversation"="Total Conversations","gogo_automation"="%Gogo Automate", "users"="Total Users"))
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
  observeEvent(input$refresh, {
    session$reload() 
  })
})

