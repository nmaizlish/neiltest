# app.R - ITHIM Website Development - Neil Maizlish
# Provides example of a website with modern navigation features and
# R Shiny capability of visualizing data.
# Simulates data outputs from an R version of ITHIM, but does
# use Shiny ithim developed by University of Wisconsin

library(shiny)
library(slickR)
library(shinythemes)
library(ggplot2)
library(shinyBS)
library(readxl)
setwd ("C:/Users/Noneof/Documents/R/CalITHIM/")

################## READ Data, Web Banners, CSS Style Sheet ############

# Read web text for headers and blurb
home_text <- read_excel("webITHIMtext2018-02-10.xlsx", sheet="Home")
scen_text <- read_excel("webITHIMtext2018-02-10.xlsx", sheet="Scenarios")
meth_text <- read_excel("webITHIMtext2018-02-10.xlsx", sheet="Methods")

## Read active travel times and distances for Pre-Established Scenarios #   
ATtimes <- read.csv(file= "ATtimes2.csv", head=TRUE, sep=",")
Distances <- read.csv(file= "ScenarioDistances.csv", head=TRUE, sep=",")

setwd ("C:/Users/Noneof/Documents/R/CalITHIM/www")

# Banner header file names for Carousel
teamImg=c("Pedestrians_city_centre_banner.png", "CopenhagenRushHour_banner.png", "StrandedAmbulanceSuperstormSandy2012_banner.png", "BikeBus_banner.png", "TrafficJamAmericanHighway_banner.png")

##################  START of Shiny User Interface (UI) ###############

ui <- fluidPage (theme = shinytheme("cerulean"), 

# CCCS Style sheet
includeCSS("styles.css"),

# Title Panel
  titlePanel("California Integrated Transport and Health Impacts Model - DRAFT - 2 APR 2018"), tags$div(id="top"),
# Navigation Bar and footer
 navbarPage("", id = 'tabs', footer=column(12, fluidRow(div(style = "height:50px;background-color: rgb(0,173,231);",tags$br(),
    tags$ul(
  tags$a("Home", href = "#panel1", style="display:inline-block;width:150px;color:white;"), 
  tags$a("Scenarios", href = "#panel2", style="display:inline-block;width:150px;color:white;"), 
  tags$a("Outcomes", href = "#panel3", style="display:inline-block;width:150px;color:white;"), 
  tags$a("Methods", href = "#panel4", style="display:inline-block;width:150px;color:white;"),   
  tags$a("Defaults", href = "#panel5", style="display:inline-block;width:150px;color:white;"), 
  tags$a("Decision Support", href = "#panel6", style="display:inline-block;width:150px;color:white;"), 
  tags$a("References", href = "#panel7", style="display:inline-block;width:150px;color:white;"), 
  tags$a("Tutorials", href = "#panel8", style="display:inline-block;width:150px;color:white;"), style="text-align:center;"))), br(),

"2018 (C) Copyright Neil Maizlish", align="center"), position= c("static-top"), 

# Tabs for Pages
# Panel for Home Page
tabPanel(h6("Home", style="color:white;"), value = "#panel1", slickR(obj = teamImg, slickOpts=list(dots=T, autoplay=T,arrows=F, pauseOnHover=F, fade=T,speed=750), slideId = 'ex1', width = "100%", height = "400px"),h6("See References for photo credits", align="right"), 

   fluidRow( column(12),

# Column 1 - Transportation and Health: A Two-Way Street
#            Active Travel and Climate Change 
fluidRow( column(3,offset=1,h4(home_text[1,1]),p(home_text[1,2],align="left"), h4(home_text[2,1]),p(home_text[2,2],align="left")),

# Column 2 - Integrated Transport and Health Impacts Model 
#History and Use of ITHIM (part 1)

column(3,h4(home_text[3,1]),p(home_text[3,2],align="left"), h4(home_text[4,1]), p(home_text[4,2],align="left")),

# Column 3 - History and Use ( part 2) + Getting started
column(3, HTML(paste0(home_text[4,3])), p(home_text[4,4],align="left"), h4(home_text[5,1]),HTML(paste0(home_text[5,2])) )),div(tags$a(href="#top", "Top of page"), align="center"),tags$br())

),

# Tab Panel for Scenarios Page
# Scenarios
tabPanel(h6("Scenarios", style="color:white;"), value="#panel2",
         
 fluidRow(
           column(4, p(tags$img(src="Strategy_Concept.png", height="80px", align="left", style="padding-right: 5px;", style="padding-bottom: 5px;"), h3(scen_text[1,1])),
                  
                  p(scen_text[1,2]), p(scen_text[1,3]), HTML(paste(scen_text[1,4]))),
           column(4, HTML(paste(scen_text[2,2])), p(scen_text[2,3]),HTML(paste(scen_text[2,4])),HTML(paste(scen_text[2,5]))),
           
          column(4, p(tags$img(src="if_organizer_1118206.png", height="100px", align="left", style="padding-right: 10px;", style="padding-bottom: 10px;"), h3(scen_text[3,1]),tags$br(),p(scen_text[3,2]),br(),
           p(tags$img(src="500px-Flag-map_of_California.png", height="100px", align="left", style="padding-right: 10px;", style="padding-bottom: 10px;"), h3(scen_text[4,1]),tags$br(),p(scen_text[4,2])) 
           ))), 
        
        h3(scen_text[5,2], align="center"),hr(), h2(scen_text[5,3], align="center"),br(),

# Col 1 - user selections
fluidRow(column(2, offset=1, div(style = "background-color: rgb(242, 242, 242);padding: 5px;border:2px solid #0088dd;", h4("Options Panel"),
          radioButtons("Scenario", "1. Scenarios:", choices=list("SCS2040", "CARB2030","CSMP2020","USSG","ShortTrips", "Percent of Baseline", "Absolute Time", "Upload Data"), width="175px"),
          radioButtons("Horizon", "2. Evaluation Year:", choices=list("2010","2020", "2030","2040","2050"), width="175px"),
          radioButtons("Region", "3. Geography:", choices=list("California","SF Bay Area","San Joaquin Valley","Sacramento Area","Southern California","San Diego County"), width="175px"))),

# Col 2 - Tables of Active Travel Times and Distances

column(4, htmlOutput("t1ttl"),tableOutput("table1"),

# Times and central tendency choices
fluidRow(column(2, offset=1,div(style = "font-size: 10px;",radioButtons("Time", "Time Basis:",choices=list("weekly","daily"), width="175px",inline=TRUE))),
         column(2, offset=2,div(style = "font-size: 10px;",radioButtons("Centraltend", "Mean or Median:", choices=list("mean","median"), width="175px", inline=TRUE)))),tags$hr(), 

fluidRow(column(5, div(radioButtons("detail", "Detailed Active Travel Tables", choices = list("Less", "More"), width="300px",inline=TRUE))),
         column(4, h4(HTML('<a href="#panel3">Go to Outcomes Page</a>')))),
fluidRow(column(5, conditionalPanel("input.detail == 'More'", 
 radioButtons("tables", "Additional tables", choices = list("Age by sex", "Age by sex by mode", "Distribution"),selected ="age by sex")))),tags$hr(),

   

# Tool tip example
bsTooltip("Time", "Switch Baseline and Scenario times from minutes/day to minutes/week", placement="bottom"),
bsTooltip("Centraltend", "Switch Baseline and Scenario times from means to medians", placement="bottom"), 

       htmlOutput("t2ttl"),tableOutput("table2"), 

fluidRow(column(2, offset=1,div(style = "font-size: 10px;",radioButtons("time", "Time basis:", choices=list("daily","weekly","annual"),selected="annual",width="225px",inline=TRUE))),
         column(2, offset=3,div(style = "font-size: 10px;",radioButtons("metric", "Miles/Kilometers:", choices=list("miles","kilometers"),selected="miles",width="175px",inline=TRUE)))),tags$hr()

),

bsTooltip("time", "Switch Baseline and Scenario times from miles per day, week, or year", placement="bottom"),
bsTooltip("metric", "Switch Baseline and Scenario distances from miles to kilometers", placement="bottom"),

# Col 3 - histogram of active travel times
column(4, htmlOutput("f1ttl"),plotOutput("histo1"), htmlOutput("f2ttl"),plotOutput("histo2")) ),div(tags$a(href="#top", "Top of page"), align="center"),tags$br()), 
  
# Tab Panel for Outcomes Page 
tabPanel(h6("Outcomes"), value="#panel3", h2("Outcomess - Under construction",align="center"),tableOutput("table3")),

# Tab Panel for Methods Page
tabPanel(h6("Methods", style="color:white;"), value="#panel4", 
fluidRow( column(12),h2("Methods",align="center"),

fluidRow( column(3,offset=1,h4(meth_text[1,1]),p(meth_text[1,2],align="left"), h4(meth_text[2,1]), p(meth_text[2,2],align="left"), h4(meth_text[3,1]), p(meth_text[3,2],align="left")), 

column(3,h4(meth_text[4,1]),p(meth_text[4,2],align="left"), h4(meth_text[6,1]), tags$a(href=meth_text[6,2], meth_text[6,3])), 

column(3,h4(meth_text[5,1]),p(meth_text[5,2],align="left"),h4(meth_text[7,1]),p(meth_text[7,2],align="left") )))), 

# Tab Panel for Defaults Page
tabPanel(h6("Defaults"), value="#panel5", h2("Defaults - Under construction",align="center")),

# Tab Panel for Decicion Support Page
tabPanel(h6("Decision Support"), value="#panel6", h2("Decision Support - Under construction",align="center")),

# Tab Panel for References Page
tabPanel(h6("References"), value="#panel7",h2("References - Under construction",align="center")),

# Tab Panel for Tutorial Page
tabPanel(h6("Tutorials"), value="#panel8", h2("Tutorials - Under construction",align="center"))


#navbarPage end:
)
# fluid page end:
)

server <- function(input,output, session) {
  output$teamImg <- renderSlickR(teamImg)
  
  
# Title for Table with interactive inputs
  output$t1ttl <- renderUI({HTML(paste0("<b>", "Table 1. Per capita ", 
  input$Centraltend," ", input$Time," active travel times (minutes), ", input$Scenario, ", ", input$Region, "</b>"))})
  
 output$f1ttl <- renderUI({HTML(paste0("<b>", "Figure 1. Per capita ", 
  input$Centraltend," ", input$Time," active travel times (minutes), ", input$Scenario, ", ", input$Region, "</b>"))})

 output$t2ttl <- renderUI({HTML(paste0("<b>", "Table 2. Per capita mean", 
  " ", input$time," active travel distance (", input$metric,") ", input$Scenario, ", ", input$Region, "</b>"))})

 output$f2ttl <- renderUI({HTML(paste0("<b>", "Figure 2. Per capita mean", 
  " ", input$time," active travel distance (", input$metric,") ", input$Scenario, ", ", input$Region, "</b>"))})
 
# Apply selection criteria to Sub Set Rows
 
  NewATtimes <- reactive({ATtimes <- subset(ATtimes, Region==input$Region & Centraltend ==input$Centraltend & Time ==input$Time & ScenarioName ==input$Scenario) })
  
NewDistances <- reactive({Distances <- subset(Distances, Region==input$Region & ScenarioName ==input$Scenario) }) 
 
  y <-reactive({input$metric}) 
  x <-reactive({input$time})
 
    
  # Select and Output just the necessary columns
  output$table1 <- renderTable(NewATtimes()[,c("Mode","Baseline","Scenario","PercentChange")])
  
  
 # Format tables to account for user display options mi(km) or d/w/y time 
 output$table2 <- renderTable({
  tab2 <- NewDistances()
  if (y() == "miles")  tab2[,6:7] <- tab2[,6:7]*1.6
  if (x() == "annual") {tab2[5:8]} else 
  if (x() == "daily")  {cbind(tab2[5],tab2[,6:7]/365,tab2[8])} else 
  if (x() == "weekly") {cbind(tab2[5],tab2[,6:7]/52, tab2[8])}

  })
    

# Reshape file to be in long format
  output$histo1 <- renderPlot({

NewATtimes_long <- reshape(NewATtimes(),
 varying= c("Baseline","Scenario"),
 v.name="Time",
 timevar="ScenarioName",
 times = c("Baseline","Scenario"),
 new.row.names = 1:6,
  direction="long")

# Create factors so modes can be put in any order    
    NewATtimes_long <- data.frame(Mode = factor(c("Walk","Bike","Total","Walk","Bike","Total"), levels = c("Bike","Walk","Total")),

ScenarioName = factor(c("Baseline","Baseline","Baseline","Scenario","Scenario","Scenario"), levels=c("Baseline","Scenario")),
  Time = NewATtimes_long$Time)

# Use ggplot for histogram chart
ggplot(NewATtimes_long, aes(x=Mode, y=Time, fill=ScenarioName)) + geom_bar(stat="identity", position=position_dodge()) + xlab("Mode") + ylab(paste0(input$Centraltend," min/person/",substr(input$Time,1,1))) + theme_bw() + theme(axis.title.x = element_text(size=14, face="bold"),axis.title.y = element_text(size=14, face="bold"), axis.text = element_text(size=14, face="bold"))
 
  })
  
# Travel distances plot: distances in input file are in miles per year
#  Reshape file to be in long format
  output$histo2 <- renderPlot({

    fig2 <- NewDistances()
    if (y() == "miles")  fig2[,6:7] <- fig2[,6:7]*1.6
    if (x() == "annual") {fig2} else 
    if (x() == "daily")  fig2[,6:7] <- fig2[,6:7]/365 else 
    if (x() == "weekly") fig2[,6:7] <- fig2[,6:7]/52
    
        if (x() == "annual") timebasis <- "year" else  
        if (x() == "daily")  timebasis <- "day" else
        if (x() == "weekly") timebasis <- "week"

 NewDistances_long <- reshape(fig2,
 varying= c("Baseline","Scenario"),
 v.name="Time",
 timevar="ScenarioName",
 times = c("Baseline","Scenario"),
 new.row.names = 1:18,
  direction="long")


# Create factors so modes can be put in any order    
    NewDistances_long <- data.frame(Mode = factor(c("Walk","Bike","Car-Driver", "Car-Passenger", "Bus", "Rail", "Motorcycle", "Truck", "Total", "Walk","Bike","Car-Driver", "Car-Passenger", "Bus", "Rail", "Motorcycle", "Truck","Total"), levels = c("Bike","Walk","Rail", "Bus", "Motorcycle", "Truck","Car-Passenger", "Car-Driver", "Total")),

ScenarioName = factor(c("Baseline", "Baseline","Baseline","Baseline","Baseline","Baseline","Baseline","Baseline", "Baseline","Scenario","Scenario","Scenario","Scenario","Scenario","Scenario","Scenario","Scenario","Scenario"), levels=c("Baseline","Scenario")),
  Time = NewDistances_long$Time)

# Use ggplot for histogram chart
ggplot(NewDistances_long,  aes(x=Mode, y=Time, fill=ScenarioName)) + geom_bar(data=subset(NewDistances_long, Mode != c("Motorcycle","Truck","Total")),  stat="identity", position=position_dodge()) + xlab("Mode") + ylab(paste0(input$metric,"/person/",timebasis)) + theme_bw() + theme(axis.title.x = element_text(size=14, face="bold"),axis.title.y = element_text(size=14, face="bold"), axis.text = element_text(size=10, face="bold"))

 })

# Navigation for footer and between pages
  observeEvent(input$tabs, {
    
    if (getUrlHash() == input$tabs) return()
    updateQueryString(
      paste0(getQueryString(), input$tabs),
      "push"
    )
  }, ignoreInit = TRUE)
  observeEvent(getUrlHash(), {
    hash <- getUrlHash()
    if (hash == input$tabs) return()
    valid <- c("#panel1", "#panel2", "#panel3","#panel4", "#panel5", "#panel6", "#panel7", "#panel8")
    if (hash %in% valid) {
      updateTabsetPanel(session, "tabs", hash)
    }
  })   
  }

shinyApp(ui = ui, server = server) 



