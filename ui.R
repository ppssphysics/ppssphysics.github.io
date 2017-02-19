library(shiny)
library(shinydashboard)

RegionNames = c("All","Africa", "Americas", "Asia","Europe","Oceania")
SubRegionNames = c("All","East Asia","Eastern Europe","Greater China","Middle East & North Africa","North America","South America","South Asia","Sub-Saharan Africa","Western Europe","Australia and New Zealand","Caribbean","Central America","Central Asia","Eastern Africa","Eastern Asia","Eastern Europe","Melanesia","Micronesia","Middle Africa","Northern Africa","Northern America","Northern Europe","South America","South-Eastern Asia","Southern Africa","Southern Asia","Southern Europe","Western Africa","Western Asia","Western Europe")


shinyUI(
   
   navbarPage("Developing Data Products",

   ###############################################
   # Home page
   ###############################################

   tabPanel("Introduction",
            includeHTML("www/intro.html")
   ),

   ###############################################
   # Participation Statistics
   ###############################################

   tabPanel("Participation",
      dashboardPage(#skin="black",
         dashboardHeader(title="Participation Dashboard"),

         dashboardSidebar(
            selectInput("RegionSel", "Select a Region:",choices = RegionNames)
            #selectInput("SubRegionSel", "Choose a Sub-Region:",choices = SubRegionNames)
        ),

         dashboardBody(
           tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "www/mystyle_1.css")),
            
           # Info boxes
           ##############
           
            #fluidRow(
            #   infoBoxOutput("progressBox1"),
            #   infoBoxOutput("progressBox2")
            #),
         
           # Completion and Gender Plots
           #############################
           
           fluidRow(
            box(
                title = "Number Completed Surveys", status = "primary", solidHeader = TRUE,collapsible = TRUE,
                selectInput("Normalisation1", "Choose Display Type:",choices = c("Absolute","Percentage")),
                plotOutput("surveys",height=250)
            ),
            box(
                title = "Gender Distribution", status = "primary", solidHeader = TRUE,collapsible = TRUE,
                selectInput("Normalisation2", "Choose Display Type:",choices = c("Absolute","Percentage")),
                plotOutput("genderplot",height=250)
            )
           ),
           
           # Regional Distributions
           #############################
           
           fluidRow(
            box(
              title = "Regional Distribution", status = "primary", solidHeader = TRUE,collapsible = TRUE,
              selectInput("Normalisation3", "Choose Display Type:",choices = c("Absolute","Percentage")),
              plotOutput("regionalplot",height=250)
            )
            #box(
            #    title = "Sub-regional Distribution", status = "primary", solidHeader = TRUE,collapsible = TRUE,
            #  selectInput("Normalisation4", "Choose Display Type:",choices = c("Absolute","Percentage")),
            #  plotOutput("subregionalplot",height=800)
            #)
           )
         )
      )
   ),

   ###############################################
   # Insights page
   ###############################################

   tabPanel("Insights",
            dashboardPage(#skin="black",
                dashboardHeader(title="Insights"),
                
                dashboardSidebar(
                    selectInput("QuestionSel", "Choose a Question:",choices = c("The world is...","What defines my identity...")),
                    selectInput("RegionSel2", "Select a Region:",choices = RegionNames)
                ),
                
                dashboardBody(
                    tags$head(tags$style(HTML('.info-box {min-height: 65px;} .info-box-icon {height: 65px; line-height: 45px;} .info-box-content {padding-top: 0px; padding-bottom: 0px;}'))),
                    
                    fluidRow(
                        box(
                            title = "Overall Results", status = "warning",
                            plotOutput("Qplot1",height=300,width=600)
                        )
                    ),
                    fluidRow(
                        box(
                            title = "Slit by Gender", status = "primary",
                            plotOutput("Qplot2",height=300,width=600)
                        )
                    ),
                    fluidRow(
                        box(
                            title = "Split by Region", status = "primary",
                            plotOutput("Qplot3", height=300,width=600)
                        )
                    )
                )
        )
    )
))
