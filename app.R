library(shiny)
library(ggthemes)
library(shinythemes)
library(scales)
library(shinydashboard)
library(ggrepel)

library(googleVis)

library(dplyr)
library(tidyr)
library(ggplot2)
library(NLP)
library(tm)
#library(readxl)
#library(readr)
library(lubridate)
library(wordcloud)

load("data.Rda")


payType <- c(unique(salDat$payUnit),"Annual Equivalent")

# Define UI for application that draws a histogram

header <- dashboardHeader(title = "IT Job Salary Statistic",
                          titleWidth = 300)

sidebar <- dashboardSidebar(
  width = 300,
  textInput("jobTitle","Job Title:",value = "software developer"),
  actionButton("go","Submit"),
  radioButtons("payType","Select Pay Unit:",choices = payType, selected = payType[5]),
  br(),
  plotOutput("wordCloud", width = 300, height = 300)
)

body <- dashboardBody(

  box(
    title = "Payment Rate Over Time", status = "primary", solidHeader = TRUE,
    collapsible = TRUE,
    plotOutput("salPlot", height = 400)
    #htmlOutput("salPlot")
  ),
  
  box(
    title = "Average Payment Rate by State", status = "primary", solidHeader = TRUE,
    collapsible = TRUE,
    #plotOutput("avgSalStatePlot", height = 400)
    htmlOutput("avgSalStatePlot")
  ),
  
  #fluidRow(
    # # A static valueBox
    # valueBox(10 * 2, "New Orders", icon = icon("credit-card")),
    
    # Dynamic valueBoxes
    # valueBoxOutput("maxPayment"),
    # valueBoxOutput("medianPayment"),
    # valueBoxOutput("minPayment")
    # 
    box(
       status = "primary", solidHeader = TRUE, title = "Payment Summary", 
      # collapsible = TRUE,
      #title = "Payment Summary", width = NULL, background = "light-blue",
      #width = 200,
      htmlOutput("maxPayment"),
      htmlOutput("medianPayment"),
      htmlOutput("minPayment")
    ),
  
  fluidRow(
    column(width = 6,
      box(
        status = "primary", #solidHeader = TRUE,#title = "Payment Summary Over Year", 
        # collapsible = TRUE,
        #title = "Payment Summary", width = NULL, background = "light-blue",
        #width = 200,
       # h5(tableOutput("paySumYear"))
       htmlOutput("paySumYear")
        
      ),
      box(
        title = "Payment Update Trend:", status = "warning", #solidHeader = TRUE,
        background = "light-blue",
        htmlOutput("payChangeRate")
     )
    )
  )
  # box(
  #   title = "Average Payment vs. Demand", status = "primary", solidHeader = TRUE,
  #   collapsible = TRUE,
  #   #plotOutput("avgSalStatePlot", height = 400)
  #   htmlOutput("avgPayVsDemand")
  # )
  # 
#  )
)

ui <- dashboardPage(header, sidebar, body)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  action <- eventReactive(input$go, {
    input$jobTitle
  })
  
  job <- reactive({
    
    jobT <- action()#input$jobTitle
  
    aDF <- salDat[grep(jobT,salDat$jobTitle),]
    
    payTypeSelected <- input$payType
    if(payTypeSelected != payType[5]){
      aDF <- aDF %>%
                filter(payUnit == payTypeSelected) %>%
                mutate(payment = payRate)
    } else {
      aDF <- aDF %>%
                mutate(payment = payAnnualRate)
    }
    
    validate(
      need(nrow(aDF) > 5, "Sorry! Not enough data points. Please check for different Job Title!")
    )
    aDF
  })
  
  output$salPlot <- renderPlot({
  # output$salPlot <- renderGvis({
    ## ggplot2
    gSal <- ggplot(job(), aes(x=dateAddedF, y = payment, fill = employmentType))
      gSal <- gSal + geom_bar(stat = "identity")
      gSal <- gSal + geom_smooth(method = lm)
      gSal <- gSal + scale_y_continuous(labels=comma)#function(n){format(n, scientific = FALSE)})
      #gSal <- gSal + ggtitle(paste(action(),"Payment Rate Over Time", sep = " : "))
      gSal <- gSal + ylab("Payment Rate") + xlab("Hiring Date")
      gSal <- gSal + theme_economist()
      print(gSal)
    ## End: ggplot2
    
    # ## googleVis
    # dat <- job() %>%
    #             select(dateAddedF, payment, employmentType) %>%
    #             group_by(dateAddedF) %>%
    #             summarise(max(payment))
    # ## end googleVis
    
  })
  
  #output$avgSalStatePlot <- renderPlot({
  output$avgSalStatePlot <- renderGvis({
    
    dat <- job()
    
    dJob_avg <- dat %>%
    #dJob <- job() %>%
      group_by(worksiteState) %>%
      summarise(avgSal = mean(payment)) %>% #, numJ = count(id)) %>%
      arrange(desc(avgSal))
    
    dJob_cnt <- dat %>%
      group_by(worksiteState) %>%
      summarise(n_distinct(id))

    dJob <- inner_join(dJob_avg,dJob_cnt, by="worksiteState")
    
    
    dJob$worksiteState=factor(dJob$worksiteState,levels=dJob$worksiteState)
    dJob$worksiteState = factor(dJob$worksiteState)
    # dJob$payment = as.double(dJob$payment)
    # dJob$numJ = as.double(dJob$numJ)
    
    ## ggplot2
    # gJob <- ggplot(data = dJob, aes(x = worksiteState, y = avgSal))
    # gJob <- gJob + geom_bar(stat = "identity",colour = "blue", fill="blue")
    # gJob <- gJob + coord_flip()
    # gJob <- gJob + scale_y_continuous(labels=comma)
    # #gJob <- gJob + scale_x_continuous() 
    # gJob <- gJob + ylab("Payment Rate") + xlab("Hiring State")
    # #gJob <- gJob + ggtitle(paste(action(),"Average Payment Rate by State", sep = " : "))
    # gJob <- gJob + theme_economist()
    # print(gJob)
    
    ## googleVis
    names(dJob) <- c("State", "Avg.Payment", "Post.Count")
    
    gvisComboChart(dJob, xvar = "State", yvar = c("Avg.Payment", "Post.Count"),
                   options=list(#title="Hello World",
                                #titleTextStyle="{color:'red',
                                #fontName:'Courier',
                                #fontSize:16}",
                                curveType="function", 
                                pointSize=9,
                                seriesType="bars",
                                series="[{type:'bars', 
                                targetAxisIndex:0,
                                color:'blue'}, 
                                {type:'line', 
                                targetAxisIndex:1,
                                color:'red'}]",
                                # vAxes="[{title:'USD',
                                # format:'#,###%',
                                # titleTextStyle: {color: 'black'},
                                # textStyle:{color: 'black'},
                                # textPosition: 'out'}, 
                                # {title:'Millions',
                                # format:'#,###',
                                # titleTextStyle: {color: 'grey'},
                                # textStyle:{color: 'grey'},
                                # textPosition: 'out',
                                # minValue:0}]",
                                # hAxes="[{title:'Date',
                                # textPosition: 'out'}]",
                                width=500, height=400
                   ), 
                   chartid="twoaxiscombochart"
    )
                 # options = list(height = "400px",
                 #                legend = "top"))
    ## End: googleVis
  })
  
  # output$avgPayVsDemand <- renderGvis({
  #   
  #   dat <- job()
  #   
  #   dJob_avg <- dat %>%
  #     group_by(worksiteState) %>%
  #     summarise(avgSal = mean(payment)) #%>% #, numJ = count(id)) %>%
  #     
  #   
  #   dJob_cnt <- dat %>%
  #     group_by(worksiteState) %>%
  #     count(id)
  # 
  #   dJob <- inner_join(dJob_avg,dJob_cnt, by="worksiteState")
  #   
  #   dJob <- dJob %>% arrange(desc(avgSal))
  #   
  #   dJob$worksiteState=factor(dJob$worksiteState,levels=dJob$worksiteState)
  #   dJob$worksiteState = factor(dJob$worksiteState)
  #   
  #   
  #   ## googleVis
  #   names(dJob) <- c("State", "Average Payment", "id","Num of Placement")
  #   
  #   gvisComboChart(dJob, xvar = "State", yvar = c("Average Payment","Num of Placement"),
  #                options = list(height = "400px",
  #                               legend = "top"))
  #   ## End: googleVis
  # })
  
  output$wordCloud <- renderPlot({
    
    #set.seed(1234)
    dat <- job()
    docs <- Corpus(VectorSource(dat$jobTitle))
    dtm <- TermDocumentMatrix(docs)
    m <- as.matrix(dtm)
    v <- sort(rowSums(m),decreasing=TRUE)
    d <- data.frame(word = names(v),freq=v)
    #d <- cbind(d, percentage = )
    
    wordcloud(words = d$word, freq = d$freq, min.freq = 1,
              max.words=200, random.order=FALSE, rot.per=0.35,
              colors=brewer.pal(8, "Dark2"))
    
    

  })
  
  output$maxPayment <- renderUI({# renderValueBox({
    dat <- job()
    payTypeSelected <- input$payType
    if((payTypeSelected == payType[5])|(payTypeSelected == payType[1])){
      payTypeSelected = "per Year"
    }
    
    HTML(paste0("<h4>","Maximum:&nbsp;","<b>", "$",
                format(max(dat$payment),big.mark = ","),"</b>"," USD ", payTypeSelected,"</h4>"))
  })
  
  output$medianPayment <- renderUI({#renderValueBox({
    dat <- job()
    payTypeSelected <- input$payType
    if((payTypeSelected == payType[5])|(payTypeSelected == payType[1])){
      payTypeSelected = "per Year"
    }
    
    HTML(paste0("<h4>","Median:&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;","<tab>", "<b>","$",
                format(median(dat$payment),big.mark = ","),"</b>"," USD ", payTypeSelected,"</h4>"))
  })
  
  output$minPayment <- renderUI({#renderValueBox({
    dat <- job()
    payTypeSelected <- input$payType
    if((payTypeSelected == payType[5])|(payTypeSelected == payType[1])){
      payTypeSelected = "per Year"
    }
    
    HTML(paste0("<h4>","Minimum:&nbsp;","<b>", "$",
                format(min(dat$payment),big.mark = ","),"</b>"," USD ", payTypeSelected,"</h4>"))
    
  })
  
  output$payChangeRate <- renderUI({#renderValueBox({
    dat <- job()
    d <- dat %>% group_by(year = year(dateAddedF)) %>%
      summarise(avg = mean(payment)) %>%
      arrange(year)
    
    l <- lm(d$avg ~ d$year)
    
    changeRate <- l$coefficients[2]
    
    HTML(paste0("<h3>","<b>",round(changeRate/d$avg[1]*100,digits = 2)," %","</b>",
                " over Year","</h3>"))

  })
  
  #output$paySumYear <- renderTable({
  output$paySumYear <- renderGvis({
    
    dat <- job()
    d_avg <- dat %>% group_by(year = year(dateAddedF)) %>%
                  summarise(avg = mean(payment))
    
    d_cnt <- dat %>% group_by(year = year(dateAddedF)) %>%
                  summarise(n_distinct(id))
    
    d <- inner_join(d_avg,d_cnt,by="year")
    
    d$avg <- round(d$avg, digits = 0)
    d$avg <- format(d$avg,big.mark = ",")
    
    d$year <- as.character(d$year)
    names(d) <- c("Year","Avg.Payment","Post.Count")
    # d
    gvisTable(d, options = list(height = "180px", width = "220px"))
    
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

