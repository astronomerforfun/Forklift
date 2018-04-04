library(forecast)
library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(scales)
library(DT)
library(stringr)


source("NEW.R", local = TRUE)

branch1 <- unique(distinctlineitems$BRANCH)


ui <- navbarPage(
  theme = shinytheme("cosmo"),
  title = "EXPENDITURES",
  tabPanel("DATA",
           sidebarLayout(
             sidebarPanel(
               h5("Instructions: 
                  1) Choose Branch, 2) filter by Keyword(s), 3) click the line item(s) 
                  of interst, 4) Go to Analysis Tab for Chart and Aggregated Monthly Totals."),
               checkboxGroupInput("branches",label = NULL,choices = branch1 ,selected = NULL),
               # actionButton('selectallB','Select All'), #select all button
               textInput("words", "Search"),
               h5("Separate keywords with commas.")
             ),
             mainPanel(
               
               DT::dataTableOutput("table")
                                    
               )
             )
           ),
  tabPanel("ANALYSIS",
           # sidebarLayout(
           #   # sidebarPanel(
           #   #   # sliderInput("months", "Periods", min = 1, max = 36, value = c(1,36), step = 1)
           #   # ),
           # ),
            
             mainPanel(
               # fluidRow(csvDownloadUI("dwnld", "DOWNLOAD"), style = "padding:10px"),
                 plotOutput("plot"),
                 fluidRow(csvDownloadUI("dwnld", "DOWNLOAD"), style = "padding:10px"),
                 dataTableOutput("table2")
             )
           )
  )
  

# Define server logic required to draw a histogram
server <- function(input, output, session) {
 
  
  ##############Reactives###########
  ####Basic Structure########
  #Filter df distinctlineitems by branches, then filter distinclineitems by search bar (wordreact())
  #the id filter (dataclick) then filters data1 (whole df- uncompressed/aggregated).  Then that data
  #is used by the agg() reactive to aggregate by date and sum.  The agg() reactive is then
  #used in ggplot to graph the data that was chosen in the click in distinctlineitems.
   branchfilter <- reactive({
     filt <- distinctlineitems[distinctlineitems$BRANCH %in% input$branches,]
     return(filt)
   })


  wordreact <- reactive({
    if(is.null(branchfilter())) return(NULL)
    inputwords <- toupper(input$words)
    inputwords2 <- gsub(" ", "", inputwords)
    commas <- strsplit(inputwords2, ",")
    var <- paste(commas[[1]], collapse = "|")
    test <- branchfilter()[grep(var,branchfilter()$REMARKS),]
    return(test)
  })

   ids <- reactive({
     # if(length(input$table_rows_selected) < 1) return(NULL)
     id <- input$table_rows_selected
     id2 <- wordreact()$REMARKS[id]
     return(id2)
     
   })

   graphids <- reactive({
     filtuid <- data1[data1$REMARKS %in% ids(),] #first get the df
   })

   
   agg <- reactive({
     g <- graphids()
    uuu <- g%>%
       group_by(DATE)%>%
       summarise(AGG_TOTAL = sum(TOTAL, na.rm = T))
     return(uuu)
   })

   
   # slide <- reactive({
   # graphslide <- seq(input$months[1], input$months[2])
   # data2 <- agg()[nrow(graphslide),]
   # print(graphslide)
   # })
   ###########TABLES##################
   output$table2 <- DT::renderDataTable({
     DT::datatable(agg(),
                   filter = "top",
                   rownames = FALSE,
                   colnames = c("Date", "Total Monthly Aggregated Expenditures"))%>%
       formatCurrency(2, digits = 0, interval = 3, mark = ",", currency = "$")
                  
   })
   
   output$table <- DT::renderDataTable({
     DT::datatable(wordreact(),
                   filter = "top",
                   rownames = FALSE)
   })
   
   ###############PLOTS#################
   
   output$plot <- renderPlot({
     ggplot(agg(), aes(DATE, AGG_TOTAL)) + geom_point() + geom_line() +
       geom_smooth(method = loess) + theme_bw() + 
       scale_y_continuous(label = scales::dollar) + 
       ylab("Aggregate Monthly Expenditures") + xlab("Date") +
       ggtitle("Aggregated Monthly Expenditures")
   })

   # Select all button statement
   # observe({
   #   if(input$selectallB == 0) return(NULL)
   #   else if(input$selectallB%%2 == 1){
   #     updateCheckboxGroupInput(session,'branches',choices = branch1, selected = branch1 )
   #     updateActionButton(session,'selectallB',label = 'Deselect All')
   #   } else {
   #     updateCheckboxGroupInput(session,'branches',choices = branch1, selected = NULL )
   #     updateActionButton(session,'selectallB',label = 'Select All')
   #   }
   # })

dwnld <- reactive({
agg()
})

  
  observe({
    callModule(csvDownload,"dwnld", dwnld)
    # print(marginfordwnld())
  })
}
# Run the application 
shinyApp(ui = ui, server = server)

