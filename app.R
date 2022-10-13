  library(shiny)
  library(toastui)
  library(shinymanager)
  ## load colours
  # cols <- toupper(c(
  #   "#bebada","#fb8072","#80b1d3","#fdb462","#b3de69","#fccde5","#FDBF6F","#A6CEE3",
  #   "#56B4E9","#B2DF8A","#FB9A99","#CAB2D6","#A9C4E2","#79C360","#FDB762","#9471B4",
  #   "#A4A4A4","#fbb4ae","#b3cde3","#ccebc5","#decbe4","#fed9a6","#ffffcc","#e5d8bd",
  #   "#fddaec","#f2f2f2","#8dd3c7","#d9d9d9"))
  
  inactivity <- "function idleTimer() {
  var t = setTimeout(logout, 120000);
  window.onmousemove = resetTimer; // catches mouse movements
  window.onmousedown = resetTimer; // catches mouse movements
  window.onclick = resetTimer;     // catches mouse clicks
  window.onscroll = resetTimer;    // catches scrolling
  window.onkeypress = resetTimer;  //catches keyboard actions
  
  function logout() {
  window.close();  //close the window
  }
  
  function resetTimer() {
  clearTimeout(t);
  t = setTimeout(logout, 120000);  // time is in milliseconds (1000 is 1 second)
  }
  }
  idleTimer();"
  

  
  user=readRDS("user.rds")
  if(file.exists("date.rds")){
    date <- readRDS("date.rds")}else {date <- data.frame(start=NULL,end=NULL,color=NULL,title=NULL,body=NULL)}
  
  # data.frame with credentials info
  credentials <- data.frame(
    user ,
    password = c("*******",rep("c2022",length(user)-1)),
    is_hashed_password = FALSE,
    admin = c(TRUE,  rep("FALSE",length(user)-1) ),
    # comment = c("alsace", "auvergne", "bretagne"), %>% 
    stringsAsFactors = FALSE
  )
  
  ui <- secure_app(head_auth = tags$script(inactivity),
                   fluidPage(
                     
                     pageWithSidebar(
                       
                       headerPanel(title="Calendar Planner",windowTitle="Calendar Planner"),
                       
                       sidebarPanel(
                        
  
  
                         h3("Duration"),
                        
                         div(class="row",
                             div(class="col-md-6",
                                 dateInput("start","From",value=format(as.Date(Sys.time(),"%Y-%m-%d",tz="Europe/Stockholm"),"%Y-%m-%d"))
                             ),
                             div(class="col-md-6",
                                 dateInput("end","To",value=format(as.Date(Sys.time(),"%Y-%m-%d",tz="Europe/Stockholm"),"%Y-%m-%d"))
                             ), 
                         ),radioButtons(
                           "Title","Type :",
                           
                           choices = c("Meeting" = "Meeting",
                                       "Conference" = "Conference",
                                       "Project " = "Project ",
                                       "Sport"="Sport"
                           ),
                           selected = "Meeting",inline=TRUE
                         ),
                         
          
                         textInput("description", "Description"),
                         actionButton("click", "Save the meeting"),
                         br(),
                         br(),
                         textInput("NewRecord", "New Record"),
                         helpText("Only the admin could add new record!!"),
                         actionButton("click1", "Save a New Record"),
                         verbatimTextOutput("res_auth"),
                        
                       ),
                       
                       
                       mainPanel(
                         
                         calendarOutput("my_calendar")
                         
                         
                       ) 
                       
                     )
                     
                   ) )
  
  server <- function(input, output, session) {
    
    result_auth <- secure_server(check_credentials = check_credentials(credentials))
    
    output$res_auth <- renderPrint({
      #reactiveValuesToList(result_auth)
      
      if (result_auth$admin==TRUE){
        user1=addUser()
      paste("The user :",tail(user1, n=1), "is added" )
      }
      else if (result_auth$admin==FALSE){print(paste("The user :",result_auth$user, "is online!"))}
      
    })
    
    addUser=eventReactive (input$click1, {
      if (result_auth$admin==TRUE){
        user=append(user,input$NewRecord)
        saveRDS(user,"user.rds")
      }else{(print("Only the admin could add new record!!"))}
      user<-user
      
    })
    
    dateData=eventReactive (input$click, {

     
      
      addDate <- isolate(data.frame(start = as.character(input$start), 
                                    end = as.character(input$end), 
                                    color = "green",title = input$Title,
                                    body = input$description))
      isolate(date <- rbind(date, addDate))
      saveRDS(date,"date.rds")
      date=date
     
    })
    
   

   
  
    ## OUT: out_plot ------------------------------------------------------------
    ## plots figure
    output$my_calendar <- renderCalendar({
     if (input$click==TRUE){
       date=dateData()
     }
      
      calendar(date, navigation = TRUE) %>%
        cal_props(
          list(
            id = 1,
            name = "PERSO",
            color = "white",
            bgColor = "firebrick",
            borderColor = "firebrick"
          ),
          list(
            id = 2,
            name = "WORK",
            color = "white",
            bgColor = "forestgreen",
            borderColor = "forestgreen"
          )
        )
    })  
    
    
    
    
    
  }
  
  
  shinyApp(ui = ui, server = server)
