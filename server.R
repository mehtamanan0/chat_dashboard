
shinyServer(function(input, output, session){

  output$total_conv <- renderValueBox({
    valueBox(
      value = format(total_conv,big.mark=",",scientific=FALSE),
      subtitle = "Total Conversations",
      icon = icon("list")
    )
  })
  output$users <- renderValueBox({
    valueBox(
      value = format(total_users,big.mark=",",scientific=FALSE),
      subtitle = "Total Unique Users",
      icon = icon("users")
    )
  })
  output$automation <- renderValueBox({
    valueBox(
      value = paste(format(end_end_conv,big.mark=",",scientific=FALSE),"%"),
      subtitle = "End to End Gogo Conversations",
      icon = icon("download")
    )
  })
  

updateSelectInput(session, "stories", label = NULL, choices =as.character(unique(story_count$story)), selected = story_count$story[1])  # input$date and others are Date objects. When outputting
updateSelectInput(session, "stories_input", label = NULL, choices =c("All",as.character(unique(data_df$story))), selected = "All")  # input$date and others are Date objects. When outputting
updateSelectInput(session, "node", label = NULL, choices =as.character(unique(data_df$node_data)), selected = NULL)  # input$date and others are Date objects. When outputting
updateSelectInput(session, "stop_logic", label = NULL, choices =as.character(unique(data_df$stop_logic_data)), selected = NULL)  # input$date and others are Date objects. When outputting
updateSelectInput(session, "message_by", label = NULL, choices =as.character(unique(data_df$message_by)), selected = NULL)  # input$date and others are Date objects. When outputting

updateSelectInput(session, "break_message_word_cloud", label = NULL, choices =c("All",as.character(unique(data_df$stop_logic_data))), selected = "All")  # input$date and others are Date objects. When outputting
updateSelectInput(session, "node_word_cloud", label = NULL, choices =as.character(unique(data_df$node_data)), selected = as.character(data_df$node_data[1]))  # input$date and others are Date objects. When outputting


node<-reactive({
  if(stories_input()=="All"){
    updateSelectInput(session, "node", label = NULL, choices =as.character(unique(data_df$node_data)), selected = "All")  # input$date and others are Date objects. When outputting
  }
  else{
  updateSelectInput(session, "node", label = NULL, choices =as.character(unique(data_df[data_df$story==input$stories_input,]$node_data)), selected =NULL)  # input$date and others are Date objects. When outputting
  }})


story<-reactive({
  story<-input$stories
})

stories_input<-reactive({
  stories_input<-input$stories_input
})

new_conversation<-reactive({
  new_conversation<-input$new_conversation
})

break_message<-reactive({
  break_message<-input$break_message
})



datos<- function(){
  node()
  story_ = story()
  if(story_ =="All"){
    df<-subset(story_count2, select = -c(story) )
  }
  else{
    df <-story_count2[story_count2$story==story_,]
    df <-subset(df, select = -c(story) )
  }
  return(df)
}

dataoutput<-reactive({
  if(stories_input()=="All"){
    df1 = data_show_df
  }
  else{
    df1 <- data_show_df[data_show_df$story==stories_input(),]
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
  if(new_conversation()){
    df4 <- df3[df3$new_conv=="True",]
  }
  else{
    df4 <- df3
  }
  if(break_message()){
    updateSelectInput(session, "stop_logic", label = NULL, choices =as.character(unique(data_df$stop_logic_data)), selected = NULL)
    df5 <- df4[df4$stop_logic_data %in% break_messages_type,]
  }
  else{
    if(!is.null(input$stop_logic)){
      df5 <- df4[df4$stop_logic_data == input$stop_logic,]  
    }
    else{
      df5 <- df4
    }
  }
  return(df5)
})

dataquestion<-function(){
  df <- dataoutput()
  df <- df[df$question!="",]
  df <- df[,c("question","story","node_data")]
  return(df)
}

output$table1 =  renderTable(
  story_count,digits = 0
  )


output$question_table =  DT::renderDataTable(
  dataquestion(),class = 'cell-border stripe',rownames = FALSE,options = list(
    autoWidth = TRUE,
    columnDefs = list(list(width = '200px', targets = "_all")),scrollX = TRUE
))


output$table2 =  DT::renderDataTable(
  dataoutput(),class = 'cell-border stripe',rownames = FALSE,options = list(
    autoWidth = TRUE,
    columnDefs = list(list(width = '200px', targets = 3)),scrollX = TRUE
  ), escape = FALSE)

output$table3 =  renderTable(
  datos(),digits = 0,include.rownames=FALSE
  )


  output$downloadData <- downloadHandler(
  filename = function() { paste('dataset', '.csv', sep='') },
  content = function(file) {
    write.csv(dataoutput(), file)
  })
  

output$chart <- renderChart({
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

output$table1 =  renderTable(
  story_count,digits = 0,include.rownames=FALSE
)

get_word_cloud_table <- function(ngram,node,breakmessage){
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

