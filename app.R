  #
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(plotly)
library(shiny)
library(DT)
library(ggplot2)
library(htmltools)
library(dplyr)
library(shinydashboard)
library(googleVis)
library(highcharter)
emp <-read.csv("C:/Users/User/Dropbox/R_Studio/CaseStudy2/CaseSudy2/CaseStudy2-data.csv")


n_rows <- as.numeric(nrow(emp))
all_Jobroles <- sort(unique(emp$JobRole))

colnames(emp)[1] = "Age"
emp$Attrition = as.integer(as.factor(emp$Attrition)) - 1
emp$BusinessTravel = as.integer(as.factor(emp$BusinessTravel))
emp$Gender = as.integer(as.factor(emp$Gender))
emp$JobRole = as.factor(emp$JobRole)
emp$MaritalStatus = as.integer(as.factor(emp$MaritalStatus))
emp$OverTime = as.integer(as.factor(emp$OverTime))
emp$EducationField = as.integer(as.factor(emp$EducationField))
emp$StandardHours <- NULL
emp$PerformanceRating <- NULL
emp$Over18 <- NULL
emp$EmployeeCount <- NULL
emp$JobLevel <- NULL
emp$DailyRate <- NULL
emp$HourlyRate <- NULL
emp$DailyRate <- NULL
emp$MonthlyRate <- NULL
emp$PercentSalaryHike <- NULL



temp <- emp

# Define UI for application that plots features of movies
ui <- dashboardPage(skin = c("red"), 
  dashboardHeader(title = "Attrition Analysis"),
  
  dashboardSidebar(
    HTML(paste("Enter a value between 1 and", n_rows)),
    
    selectInput(
      inputId = "JobRole",
      label = "Select Job role:",
      choices = all_Jobroles,
      selected = "Research Scientist",
      selectize = TRUE,
      multiple = FALSE
    ),
    
    
    
    # Select variable for y-axis
    selectInput(
      inputId = "y",
      label = "Y-axis:",
      choices = c(
        "Age" = "Age",
        "Attrition" = "Attrition",
        "Gende" = "Gender",
        "Job Satisfaction" = "JobSatisfaction",
        "Monthly Income" = "MonthlyIncome",
        "Distance From Home" = "DistanceFromHome",
        "Job Roles" = "JobRole"
      ),
      selected = "Age"
    ),
    # Select variable for x-axis
    selectInput(
      inputId = "x",
      label = "X-axis:",
      choices = c(
        "Age" = "Age",
        "Attrition" = "Attrition",
        "Gender" = "Gender",
        "Job Satisfaction" = "JobSatisfaction",
        "Monthly Income" = "MonthlyIncome",
        "Distance From Home" = "DistanceFromHome",
        "Job Roles" = "JobRole"
      ),
      selected = "MonthlyIncome"
    ),
    selectInput(
      inputId = "z",
      label = "Color by:",
      choices = c(
        "Age" = "Age",
        "Attrition" = "Attrition",
        "Gender" = "Gender",
        "Job Satisfaction" = "JobSatisfaction",
        "Monthly Income" = "MonthlyIncome",
        "Distance From Home" = "DistanceFromHome"
      ),
      selected = "Attrition"
    ),
    # Set alpha level for transparency plots
    sliderInput(
      inputId = "alpha",
      label = "Alpha:",
      min = 0,
      max = 1,
      value = 0.5
    )
  ),
 
  dashboardBody(
    fluidRow(
      # A static infoBox
      infoBox("Total Rows", 1470 , icon = icon("credit-card")),
      infoBox("Attrition Type", "Yes Or NO" , icon = icon("credit-card"))
      
    ),
    
    tabBox(
      title = "Analysing Employee Attrition Tendency",
      # The id lets us use input$tabset1 on the server to find the current tab
      id = "tabset1",
      height = 550,
      
      tabPanel(
        "Plot 1",
        "",
        box(title ="Scatter Plot",solidHeader = TRUE, collapsible = TRUE, plotOutput(outputId = "scatterplot")),
        box(title ="Density Plot" , solidHeader = TRUE, collapsible = TRUE,  plotOutput(outputId = "densityplot"))
      ),
      
      tabPanel(
        "Plot2",
        fluidRow(
          box(title = "bubble chart",htmlOutput("bubble", height = 250))
              ),
        fluidRow(
          
          box(title = "Dynamic Chart",plotlyOutput("plot1", height = 250))
        )
        
    #  box(htmlOutput("bubble", height = 250)),
    #  box(plotlyOutput("plot1", height = 250))
        
       
      ),
      tabPanel(
        "Plot3",
        "",
        box(highchartOutput("hc_1"), height = 450),
        box(highchartOutput("hc_4"), height = 450)
        
        
      ),
      
      tabPanel(
        "Plot 4",
        box(highchartOutput("hc_2")),
        box(highchartOutput("hc_3"))
      ),
      
      tabPanel(
        "Dataset Representation",
        "First tab content",
       box(DT::dataTableOutput(outputId = "Attritiontable"), width = 12) 
      ),
      width = 12
    )
  )
  
  
  
)




# Define server function required to create the scatterplot
server <- function(input, output) {
  dataInput <- reactive({
    filter(emp, emp$JobRole == input$JobRole)
  })
  
  ####Infobox
  
  output$progressBox <- renderInfoBox({
    infoBox(
      "Total Job Roles", paste0(25 + input$count, "%"), icon = icon("list"),
      color = "purple"
    )
  })
  
  output$approvalBox <- renderInfoBox({
    testing <- emp%>%
    infoBox(
      "Highest Income",  max(testing$MonthlyIncome) , icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow"
    )
  })
  
  # Create scatterplot object the plotOutput function is expecting
  output$scatterplot <- renderPlot({
    ggplot(data = emp, aes_string(
      x = input$x,
      y = input$y,
      color = input$z
    )) +
      geom_point(alpha = input$alpha)
  })
  
  output$densityplot <- renderPlot({
    ggplot(data = emp, aes_string(x = input$x)) +
      geom_density()
  })
  
  # Create data table
  output$Attritiontable <- DT::renderDataTable({
    dataInput()
  })
  
  
  
  output$bubble <- renderGvis({
    top <- sample_n(emp,10)
    gvisBubbleChart(top, idvar="Department",
                    xvar="Age", yvar="MonthlyIncome",
                    colorvar="JobRole", sizevar="DistanceFromHome",
                    options=list(title = "Bubble Chart according to the department", height = 300, width = 1300
                    )
    ) })
  
  output$plot1 <- renderPlotly(
    {abcd <- emp$input$x
    abcd1 <- emp$input$y
    plot_ly(emp, x = ~get(input$x), y = ~get(input$y), xaxis = list(title = input$x),
            yaxis = list(title = input$y), height = 300, width = 1300 )
   
  })
  
  output$event <- renderPrint({
    d <- event_data("plotly_hover")
    if (is.null(d)) "Hover on a point!" else d
  })
  #######highchart
  output$hc_1 <- renderHighchart({
    df$attrition <- as.factor(df$attrition)
    df <- temp
    
    
    names(temp) <- tolower(names(temp))
    
    temp <- temp %>%
      mutate(population = monthlyincome*ifelse(attrition == "1", -1, 1))
    
    series1 <- temp %>% 
      group_by(attrition, age)  %>% 
      do(data = list(sequence = .$population)) %>% 
      ungroup() %>% 
      group_by(attrition) %>% 
      do(data = .$data) %>%
      mutate(name = attrition) %>% 
      list_parse()
    
    maxpop1 <- max(abs(temp$population))
    
    xaxis1 <- list(categories = sort(unique(temp$age)),
                   reversed = FALSE, tickInterval = 5,
                   labels = list(step = 2))
    
    highchart() %>%
      hc_chart(type = "bar")  %>% 
      hc_motion(enabled = TRUE, labels = temp$distancefromhome, series = c(0,1), autoplay = TRUE, updateInterval = 1) %>% 
      hc_add_series_list(series1) %>% 
      hc_plotOptions(
        series = list(stacking = "normal"),
        bar = list(groupPadding = 0, pointPadding =  0, borderWidth = 0)
      ) %>% 
      hc_tooltip(shared = TRUE) %>% 
      hc_yAxis(
        labels = list(
          formatter = JS("function(){ return Math.abs(this.value); }") 
        ),
        tickInterval = 0.1,
        min = -maxpop1,
        max = maxpop1) %>% 
      hc_xAxis(
        xaxis1,
        rlist::list.merge(xaxis1, list(opposite = TRUE, linkedTo = 0))
      ) %>% 
      hc_tooltip(shared = FALSE,
                 formatter = JS("function () { return '<b>' + this.series.name + ', Age ' + this.point.category + '</b><br/>' + 'Population: ' + Highcharts.numberFormat(Math.abs(this.point.y), 0);}")
      ) 
  })
  
 ############Heatmap
  output$hc_2 <- renderHighchart({
    heat_emp <- emp %>% group_by(JobRole)%>% summarise(Total_income = sum(MonthlyIncome))%>% arrange(desc(Total_income))
    
    hchart(heat_emp,type = "treemap", 
           hcaes(x = JobRole, value = Total_income, color = Total_income))  %>% 
      hc_add_theme(hc_theme_google()) %>%hc_credits(enabled = TRUE, text = "", 
      style = list(fontSize = "10px")) %>%hc_title(text = "Total Income based on Job Role")
    
  })
  
  output$hc_3 <- renderHighchart({
    
    heat2_emp <- emp %>% group_by(Department)%>% summarise(Total_income = sum(MonthlyIncome))%>% arrange(desc(Total_income))
    hchart(heat2_emp, type = "treemap", 
           hcaes(x = Department, value = Total_income, color = Total_income))  %>% 
      hc_add_theme(hc_theme_google()) %>% hc_credits(enabled = TRUE, text = "", 
      style = list(fontSize = "10px")) %>% hc_title(text = "Total Income based on Department")
    
    
  })
  
  
  
  output$hc_4 <- renderHighchart({
    box_emp <- emp
    
    hcboxplot(x = box_emp$MonthlyIncome, var = box_emp$JobSatisfaction, var2 = box_emp$MaritalStatus,
              outliers = FALSE) %>% 
      hc_chart(type = "column")
  })
  
  
  
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)
