source("helper.R")

shinyServer(

  function(input, output) {
    
    ### DYNAMIC UI RENDERS ###
    
    output$yearSlider <- renderUI({
      years <- getYears.World()
      sliderInput("wyear", label = "Year:", 
                    min=min(years), max=max(years), value = min(years), step = 4, sep = "")  
    })
    
    output$countries <- renderUI({
      countrylist <- getCountries()
      selectizeInput("countries", "Select countries to compare: ", 
                     sort(countrylist), selected = countrylist[1], multiple = TRUE)  
    })
    
    output$Country1<- renderUI({
      countrylist <- getCountries()
      selectInput("country1", "Country 1: ", sort(countrylist), selected = countrylist[1])
    })
    
    output$Country2 <- renderUI({
      countrylist <- getCountries()
      selectInput("country2", "Country 2: ", sort(countrylist), selected = countrylist[2])
    })
    
    output$Country3 <- renderUI({
      countrylist <- getCountries()
      selectInput("country3", "Country 3: ", sort(countrylist), selected = countrylist[3])
    })
    
    output$Question.World <- renderUI({
      strong(em(paste0("\"", as.character(qinfo.world()$FullText), "\"")))
    })
       
    output$Question.Country <- renderUI({
      strong(em(paste0("\"", as.character(qinfo.country()$FullText), "\"")))
    }) 
    
    output$Question.Corr1 <- renderUI({
      strong(em(paste0("\"", as.character(qinfo.mcq1()$FullText), "\"")))
    }) 
    
    output$Question.Corr2 <- renderUI({
      strong(em(paste0("\"", as.character(qinfo.mcq2()$FullText), "\"")))
    }) 
    
    output$countrybox <- renderUI({
      box(width = NULL, plotOutput("countrymap"), collapsible = FALSE,
          title = "Country Compare", status = "primary", solidHeader = TRUE, height = paste0(heightSize() * 1.3, "px"))
    })
  
    ### REACTIVES ###
    
    getYears.World <- reactive({
        ICVS.results %>%
          filter(Question == input$wquestion) %>%
          select(Year) %>%
          distinct() %>%
          .$Year %>%
          as.numeric(.)
      })
    
    getCountries <- reactive({
      ICVS.results %>%
        filter(Question == input$cquestion) %>%
        select(Country) %>%
        distinct() %>%
        .$Country %>%
        as.character(.)    
    })
  
    # data frames
    ICVS.world.df <- reactive({
      subset(ICVS.results, Question == input$wquestion & Year == input$wyear)
    })
    
    ICVS.country.df1 <- reactive({
      subset(ICVS.results, Question == input$cquestion & Country == input$country1) %>%
        data.frame(.)
    })
    
    ICVS.country.df2 <- reactive({
      subset(ICVS.results, Question == input$cquestion & Country == input$country2) %>%
        data.frame(.)
    })
    
    ICVS.country.df3 <- reactive({
      subset(ICVS.results, Question == input$cquestion & Country == input$country3) %>%
        data.frame(.)
    })
    
    ICVS.gvis.df <- reactive({
      Df1 <- subset(ICVS.results, Question == input$mcquestion1)
      Df2 <- subset(ICVS.results, Question == input$mcquestion2)
      Df.net <- inner_join(Df1, Df2, by = c("Year","Country")) %>%
        mutate(Year = as.numeric(Year))
      Df.net <- Df.net %>% 
        rename_(.dots = setNames("Score.x", input$mcquestion1)) %>%
        rename_(.dots = setNames("Score.y", input$mcquestion2))
      Df.net
    })
    
    # maps
    ICVS.country.map1 <- reactive({
      subset(worldmap, region == input$country1)
    })
    
    ICVS.country.map2 <- reactive({
      subset(worldmap, region == input$country2)
    })
    
    ICVS.country.map3 <- reactive({
      subset(worldmap, region == input$country3)
    })
    
    # question info
    qinfo.world <- reactive({
      QuestionMap[QuestionMap$Title == input$wquestion,]
    })
    
    qinfo.country <- reactive({
      QuestionMap[QuestionMap$Title == input$cquestion,]
    })
    
    qinfo.mcq1 <- reactive({
      QuestionMap[QuestionMap$Title == input$mcquestion1,]
    })
    
    qinfo.mcq2 <- reactive({
      QuestionMap[QuestionMap$Title == input$mcquestion2,]
    })
    
    heightSize <- function() {
      as.numeric(input$ncountries) * 200
    }
  
    ### PLOTS ###
    
    output$worldmapP <- renderPlotly({
      WorldMapVizP(ICVS.world.df(), qinfo.world())
    })
    
    output$worldmap <- renderPlot({
        WorldMapViz(ICVS.world.df(), qinfo.world())
    })
    
    output$countrymap <- renderPlot({
      # loops through the number of countries selected to compare, creating a list
      # of plot objects. This is passed to a function that strips out the legend from each
      # and grid arranges them with one legend at the top.
      
      # future enhancement will be using selectize to select as many countries as you'd like
      validate(
        need(dim(ICVS.country.df1())[1] != 0, "Loading data set...")
      )
      if (input$ncountries == 2) {
        validate(need(dim(ICVS.country.df2())[1] != 0, "Loading data set..."))
      }
      if (input$ncountries == 3) {
        validate(need(dim(ICVS.country.df3())[1] != 0, "Loading data set..."))
      }
      
      glist <- list()
      for(x in 1:input$ncountries) {
          
        ICVS.country.looper <- paste0("ICVS.country.df",x,"()")
        ICVS.map.looper <- paste0("ICVS.country.map",x,"()")
        
        glist[[length(glist)+1]] <- CountryMapViz(
          eval(parse(text=ICVS.country.looper)), 
          eval(parse(text=ICVS.map.looper)), 
          qinfo.country()
        )  
      } 
      
      grid_arrange_shared_legend(glist)
      
    }, height = heightSize)
    
    
    
    output$motionviz <- renderGvis({
      gvisMotionChart(
        data = ICVS.gvis.df(), 
        idvar = "Country", 
        timevar = "Year",
        xvar = input$mcquestion1, 
        yvar = input$mcquestion2
      )
    })
  }
)

