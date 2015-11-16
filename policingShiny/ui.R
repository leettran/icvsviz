source("helper.R")

shinyUI(dashboardPage(
  skin = "black",
  
  dashboardHeader(title = "ICVS Viz"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("World Map", tabName = "world", icon = icon("map-marker", lib = "glyphicon")),
      menuItem("Country Compare", tabName = "country", icon = icon("align-center", lib = "glyphicon")),
      menuItem("Correlations (gViz)", tabName = "corr", icon = icon("google")),
      menuItem("About", tabName = "about", icon = icon("info"))
    )
    
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "world",
        fluidRow(
          column(width = 9, 
                 box(width = NULL, plotOutput("worldmap"), collapsible = FALSE,
                     title = "World Results", status = "primary", solidHeader = TRUE)
          ),
          column(width = 3,
            box(width = NULL, status = "primary", solidHeader = TRUE, title = "Parameters",
                  uiOutput("yearSlider"),
                  selectInput("wquestion", label = "Question:", 
                              choices = list(
                                "Sentiment" = c(unique(as.character(QuestionMap$Title[grep("(^P)|(^S)", QuestionMap$Id)]))),
                                "Report to Police" = c(unique(as.character(QuestionMap$Title[grep("^C\\d{2}B400", QuestionMap$Id)])))
                              )
                  ),
                  box(
                    uiOutput("Question.World"),
                    width = NULL,
                      status = "primary", solidHeader = FALSE),
                  box(width = NULL, status = "warning", 
                    h5("Note: unfilled countries do not have available data for the 
                       given question and year."))
              ))
        )),
    tabItem(tabName = "country",
        fluidRow(
          column(width = 9, 
              uiOutput("countrybox")
          ),
          column(width = 3,
                 box(width = NULL, collapsible = FALSE, title = "Parameters", status = "primary", solidHeader = TRUE,
                        radioButtons("ncountries", label = "Number of countries", 
                                    choices = list("1" = 1, "2" = 2, "3" = 3), selected = 1, inline = TRUE),
                        conditionalPanel(condition="input.ncountries <= 3", 
                                        uiOutput("Country1")
                        ),
                        conditionalPanel(condition="input.ncountries >= 2", 
                                        uiOutput("Country2")
                        ),
                        conditionalPanel(condition="input.ncountries == 3", 
                                        uiOutput("Country3")
                        ),
                        tags$hr(),
                        selectInput("cquestion", label = "Question:", 
                                   choices = list(
                                     "Sentiment" = c(unique(as.character(QuestionMap$Title[grep("(^P)|(^S)", QuestionMap$Id)]))),
                                     "Report to Police" = c(unique(as.character(QuestionMap$Title[grep("^C\\d{2}B400", QuestionMap$Id)])))
                                   )
                        ),
                        box(width = NULL, status = "primary", solidHeader = FALSE,
                         uiOutput("Question.Country")
                        )
                 ))
        )),
    tabItem(tabName = "corr",
      fluidRow(
        column(width = 9,
            box(title = "Correlations", width = NULL, status = "primary", solidHeader = TRUE,
                htmlOutput("motionviz", align = "center")
            )      
        ),
        column(width = 3,
          box(title = "Parameters", width = NULL, status = "primary", solidHeader = TRUE,
            selectInput("mcquestion1", label = "Question 1 (x-axis):",
                        choices = list(
                          "Sentiment" = c(unique(as.character(QuestionMap$Title[grep("(^P)|(^S)", QuestionMap$Id)]))),
                          "Report to Police" = c(unique(as.character(QuestionMap$Title[grep("^C\\d{2}B400", QuestionMap$Id)])))
                        ), 
                        selected = unique(as.character(QuestionMap$Title))[1]),
            box(width = NULL, status = "primary", solidHeader = TRUE,
                uiOutput("Question.Corr1")
            ),
            selectInput("mcquestion2", label = "Question 2 (y-axis):", 
                        choices = list(
                          "Sentiment" = c(unique(as.character(QuestionMap$Title[grep("(^P)|(^S)", QuestionMap$Id)]))),
                          "Report to Police" = c(unique(as.character(QuestionMap$Title[grep("^C\\d{2}B400", QuestionMap$Id)])))
                        ), 
                        selected = unique(as.character(QuestionMap$Title))[2]),
            box(width = NULL, status = "primary", solidHeader = TRUE,
                uiOutput("Question.Corr2")
            )
      ))
    )),
    tabItem(tabName = "about", 
            fluidRow(
              box(title = "ICVS Background", status = "info", solidHeader = TRUE, collapsible = TRUE, width = 8,
                h4("The International Crime Victimization Survey (ICVS) is a (mostly) quadrennial
                   survey of international perceptions concerning safety, crime, and policing."),
                h4("The first survey was performed in 1989, and the latest is from 2010, for a total of six surveys to date."),
                h4("For more information, please see the links below:"),
                tags$hr(),
                tags$a(href = "http://www.unicri.it/services/library_documentation/publications/icvs/", "Official Site", style = "font-size: 18px;"),
                tags$br(),
                tags$a(href = "https://en.wikipedia.org/wiki/International_Crime_Victims_Survey", "Wiki", style = "font-size: 18px;")
              ),
              box(title = "About", status = "info", solidHeader = TRUE, collapsible = TRUE, width = 8,
                  h4("John Montroy"),
                  h5("Programmer"),
                  tags$span(
                    tags$a(href = "https://www.linkedin.com/in/john-montroy-9516a917", icon("linkedin", "fa-2x")),
                    tags$a(href = "https://github.com/jmontroy90", icon("github", "fa-2x"), style = "margin-left: 20px;"),
                    tags$a(href = "https://www.facebook.com/jmontroy", icon("facebook", "fa-2x"), style = "margin-left: 20px;")
                  ),
                  tags$hr(),
                  # h4("My analysis / thoughts on this project located here: ", tags$a(href = ))
                  h4("This project was completed for the NYC Data Science Academy. More info on them at: "),
                  tags$a(href = "http://nycdatascience.com/", "NYC Data Science", style = "font-size: 18px;"),
                  h4("All code available at the GitHub location above.")
              )
    ))
  ))
))









