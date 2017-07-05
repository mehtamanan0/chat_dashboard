dbHeader <- dashboardHeader(title= "Teja: Gogo Ki Report", tags$li(class = "dropdown",
                                                                   tags$a(href="http://haptik.ai", target="_blank", 
                                                                          tags$img(height = "20px", alt="", src="./logo.png"))))
dashboardPage(skin = "black",
              dbHeader,
              dashboardSidebar(
                sidebarMenu(
                  menuItem("Channel Bot-Stats", tabName = "haptik", icon = icon("cloud")),
                  menuItem("Break Messages", tabName = "break", icon = icon("cloud")),
                  selectInput('date', 'What Happens in', multiple=FALSE, selectize=TRUE,choices = date_filters),
                  selectInput('channel', 'Select Channel', multiple=FALSE, selectize=TRUE,choices = c("flightschannel","reminderschannel"))
                ),
                img(src='./tejalogo.png',height=220,width=220,style="margin-top:180px;margin-left:5px;opacity:0.2;")
              ),
              dashboardBody(
                tags$style(type="text/css",
                           ".shiny-output-error { visibility: hidden; }",
                           ".shiny-output-error:before { visibility: hidden; }"
                ),
                
                
                tags$head(  
                  tags$head(tags$style(HTML("input[type='search']:disabled {visibility:hidden}"))),
                  tags$style(HTML('
                                  
                                  /* main sidebar */
                                  
                                  .skin-black .sidebar-menu>li.active>a{
                                  color:#333;
                                  background-color:#e9e9e9;
                                  }
                                  .skin-black .sidebar-menu>li>a:hover{
                                  background-color:#e9e9e9;
                                  }
                                  .skin-black .main-sidebar {
                                  background-color:#fafafa;
                                  }
                                  .sidebar-menu .treeview-menu>li{
                                  background-color:#DCDCDC;
                                  }
                                  .box.box-solid.box-warning>.box-header{
                                  background-color:#DCDCDC;
                                  }
                                  .skin-black .treeview-menu>li.active>a{
                                  color:#000;
                                  }
                                  .box.box-solid.box-warning{
                                  border: 1px solid 	#C0C0C0;
                                  }
                                  .skin-black .sidebar a{
                                  color:#696969;
                                  }
                                  .skin-black .sidebar .shiny-input-container .control-label{
                                  color:#333;
                                  }
                                  .box.box-solid.box-warning>.box-header{
                                  color:#333;
                                  }
                                  .box.box-solid.box-primary
                                  {
                                  border: 1px solid #C0C0C0;
                                  }
                                  div.box-body div#table1{
                                  height: 51.75vh;
                                  overflow-y: auto;
                                  }
                                  div.box-body div#table3{
                                  height: 40vh;
                                  overflow-y: auto;
                                  }
                                  span.info-box-text{
                                  color:#7e7e7e;
                                  font-weight:bold;
                                  font-size: 14px;
                                  }
                                  span.info-box-number{
                                  font-weight:normal;
                                  font-style:italic;
                                  font-size:14px;
                                  color:#7e7e7e;
                                  }
                                  div.info-box-content>p{
                                  font-weight:bold;
                                  font-size:22px;
                                  color:#7e7e7e;
                                  }
                                  div.info-box-content{
                                  text-align:center;
                                  }
                                  .info-box-icon{
                                  height:100%;
                                  width:90px;
                                  }
                                  div#progressBox2.shiny-html-output.col-sm-4.shiny-bound-output{
                                  margin-left:1%;
                                  margin-right:1%;
                                  }
                                  td{
                                  align:center;
                                  }
                                  #date_id{color: #635B59;
                                  }
                                  '
                  ))),
                
                
                tabItems(
                  tabItem(tabName = "haptik",
                          fluidRow(
                            valueBoxOutput("total_conv"),valueBoxOutput("users"),valueBoxOutput("automation"),
                            box(chartOutput("chart","highcharts"),width="12")
                          ),
                          
                          fluidRow(
                            box(
                              title = "Stories Wise Chat Analysis", status = "primary", solidHeader = TRUE,
                              tableOutput("table1")
                            ),
                            
                            box(
                              title = "Sub Story Wise Chat Analysis", status = "primary", solidHeader = TRUE,
                              selectInput('stories', 'Select Stories', multiple=FALSE, selectize=TRUE,choices = NULL,selected = NULL),
                              tableOutput("table3")
                            )
                            
                          )),
                  tabItem(tabName = "break",
                          fluidRow(
                            box(
                              div(align = "left",downloadButton('downloadData', 'Download CSV',style = "background-color:#a9cce3")),
                              width="9",dataTableOutput("table2"),status = "primary", solidHeader = TRUE),
                            #dataTableOutput("table2"),
                            
                            box(title="Filters",width="3", solidHeader = TRUE,status = "warning",height = "816",
                                selectInput('stories_input', 'Select Story', multiple=FALSE, selectize=TRUE,choices = NULL),
                                selectInput('node', 'Select Node', multiple=TRUE, selectize=TRUE,choices = NULL),
                                selectInput('stop_logic', 'Select Stop Logic', multiple=TRUE, selectize=TRUE,choices = NULL),
                                selectInput('message_type', 'Select Message Type', multiple=TRUE, selectize=TRUE,choices = NULL),
                                selectInput('message_by', 'Message By', multiple=FALSE, selectize=TRUE,choices = NULL),
                                selectInput('include', 'Select Columns', multiple=TRUE,selectize=TRUE, choices=NULL),
                                checkboxInput('new_conversation', 'New Convesation only', value = FALSE, width = NULL),
                                checkboxInput('break_message', 'Select Break Message Only', value = FALSE, width = NULL)
                            )
                            
                            
                            
                          ),
                          fluidRow(
                            tabBox(
                              title = "Word Level Analyis",
                              width="9",
                              # The id lets us use input$tabset1 on the server to find the current tab
                              id = "tabset1",
                              tabPanel("Message break analysis",chartOutput("pie_plot","highcharts")),
                              tabPanel("Wordcloud",selectInput('ngram', 'Choose Ngram', multiple=FALSE, selectize=TRUE,choices = c("Bigram","Trigram","Unigram")), plotOutput("wordcloud_plot")),
                              tabPanel("WordTable",tableOutput("wordTable")),
                              tabPanel("New Vocabulory")
                              
                            ),
                            box(title="Filters",width="3", solidHeader = TRUE,status = "warning",
                                selectInput('stop_logic_story', 'Select Story', multiple=FALSE, selectize=TRUE,choices = NULL),
                                selectInput('node_word_cloud', 'Select Node', multiple=TRUE, selectize=TRUE,choices = NULL),
                                selectInput('break_message_word_cloud', 'Select Break Logic', selectize=TRUE,choices = NULL,multiple=TRUE))
                            
                          )
                          
                  )
                  
                  # Boxes need to be put in a row (or column)
                  
                )
                  ))
