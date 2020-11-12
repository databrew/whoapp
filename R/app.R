##################################################
# UI
##################################################
#' @import shiny
#' @import shinydashboard
#' @import leaflet
#' @import shiny
#' @import plotly
#' @import ggplot2
#' @import RColorBrewer
#' @import gt
#' @import sp
#' @import DT
#' @import lubridate
#' @import RPostgres
#' @import yaml
#' @import leafgl
#' @import sf
#' @import shinybusy
#' @import ggrepel
app_ui <- function(request) {
  options(scipen = '999')
  
  tagList(
    mobile_golem_add_external_resources(),
    
    dashboardPage(
      dashboardHeader(title = 'WHO app'),
      dashboardSidebar(
        sidebarMenu(
          id = 'sidebar',
          menuItem(
            text="Main",
            tabName="main",
            icon=icon("archway")),
          
          menuItem(
            text = 'About',
            tabName = 'about',
            icon = icon("cog", lib = "glyphicon"))
        )),
      dashboardBody(
        
        # add_busy_spinner(
        #   spin = "folding-cube",#  "self-building-square",
        #   position = 'bottom-right',
        #   onstart = TRUE,
        #   height = '100px',
        #   width = '100px',
        #   margins = c(10, 10)#,
        #   # color = 'de0000'
        # ),
        
        # tags$head(
        #   tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        # ),
        
        tabItems(
          tabItem(
            tabName="main",
            navbarPage(
              title = 'Analysis',
              tabPanel('Country',
                       fluidPage(
                         fluidRow(
                           column(4,
                                  selectInput('country_1', 'Select country',choices = c(unique(dat$country)),selected = c(unique(dat$country)), multiple = TRUE),
                                  uiOutput('ui_inputs_1')),
                           column(8, 
                                  plotlyOutput('bar_plot'))
                         )
                       )
              ),
              tabPanel('Indicator',
                       fluidPage(
                         fluidRow(
                           column(4,
                                  selectInput('ind_2', 'Select indicators',choices = c(unique(dat$indicator_code)),selected = c('cataincidence_q1', 'cataincidence_q2','cataincidence_q3', 'cataincidence_q4', 'cataincidence_q5'), multiple = TRUE),
                                  uiOutput('ui_inputs_2')),
                           column(8, 
                                  plotlyOutput('point_plot'))
                         )
                       )
                
              ),
              tabPanel('Map',
                       fluidPage(
                         fluidRow(
                           column(3,
                                  selectInput('year_3', 'Choose a year', choices=unique(dat$year), selected = sort(unique(dat$year))[1])),
                           column(3,
                                  uiOutput('ui_inputs_3')
                                  ),
                         ),
                         fluidRow(
                           column(12, leafletOutput('map_1'))
                         )
                         
                       )
                    )
           
            )
           ),
          tabItem(
            tabName = 'about',
            make_about()
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
mobile_golem_add_external_resources <- function(){
  addResourcePath(
    'www', system.file('app/www', package = 'bohemia')
  )
  
  
  share <- list(
    title = "WHO app",
    url = "https://datacat.cc/who/",
    image = "https://www.databrew.cc/images/logo_clear.png",
    description = "WHO app",
    twitter_user = "data_brew"
  )
  
  tags$head(
    
    # Facebook OpenGraph tags
    tags$meta(property = "og:title", content = share$title),
    tags$meta(property = "og:type", content = "website"),
    tags$meta(property = "og:url", content = share$url),
    tags$meta(property = "og:image", content = share$image),
    tags$meta(property = "og:description", content = share$description),
    
    # Twitter summary cards
    tags$meta(name = "twitter:card", content = "summary"),
    tags$meta(name = "twitter:site", content = paste0("@", share$twitter_user)),
    tags$meta(name = "twitter:creator", content = paste0("@", share$twitter_user)),
    tags$meta(name = "twitter:title", content = share$title),
    tags$meta(name = "twitter:description", content = share$description),
    tags$meta(name = "twitter:image", content = share$image),
    # 
    golem::activate_js(),
    golem::favicon(),
    # # Add here all the external resources
    # # Google analytics script
    # includeHTML(system.file('app/www/google-analytics-mini.html', package = 'covid19')),
    # includeScript(system.file('app/www/script.js', package = 'covid19')),
    # includeScript(system.file('app/www/dtselect.js', package = 'saint')),
    # includeScript('inst/app/www/script.js'),
    
    # includeScript('www/google-analytics.js'),
    # If you have a custom.css in the inst/app/www
    tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
    # tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
  )
}

##################################################
# SERVER
##################################################
#' @import shiny
#' @import leaflet
#' @import yaml
#' @import lubridate
#' @import gt
#' @import DT
app_server <- function(input, output, session) {
  
  message('Working directory is :', getwd())
  
  #  ANALYSIS BY COUNTRY SECTION ###########################
  # render ui for other inputs based on country
  output$ui_inputs_1 <- renderUI({
    # country_names <- 'Bulgaria'
    country_names <- input$country_1
    if(is.null(country_names)){
      NULL
    } else {
      pd <- whoapp::dat %>% dplyr::filter(country %in% country_names)
      year_names <- sort(unique(pd$year))
      ind_names <- sort(unique(pd$indicator_code))
      # remove quintile ind (q1, q2, etc)
      # ind_names <- ind_names[!grepl('_q', ind_names)]
      fluidPage(
        fluidRow(
          selectInput('ind_1', 'Choose indicator', choices = ind_names, selected = ind_names[1]),
          selectInput('year_1', 'Choose years', choices = year_names, selected = year_names, multiple = TRUE)
        )
      )
    
    }
  })
  
  # bar plot
  output$bar_plot <- renderPlotly({
    country_names <- input$country_1
    year_names <- input$year_1
    ind_names <- input$ind_1
    if(is.null(year_names)){
      NULL
    } else {
      pd <- dat %>% filter(country %in% country_names, 
                           year %in% year_names, 
                           indicator_code %in% ind_names)
      # save(pd, file = 'bar_pd.rda')
      ggplot(pd, aes(year, value, fill=country)) +
        geom_bar(stat='identity', position = 'dodge')
      
    }
  })
  
  #  ANALYSIS BY INDICATOR SECTION ###########################
  output$ui_inputs_2 <- renderUI({
    # country_names <- 'Bulgaria'
    ind_names <- input$ind_2
    if(is.null(ind_names)){
      NULL
    } else {
      pd <- whoapp::dat %>% dplyr::filter(indicator_code %in% ind_names)
      year_names <- sort(unique(pd$year))
      country_names <- sort(unique(pd$country))
      # remove quintile ind (q1, q2, etc)
      # ind_names <- ind_names[!grepl('_q', ind_names)]
      fluidPage(
        fluidRow(
          selectInput('country_2', 'Choose a country', choices = country_names, selected = country_names[1]),
          selectInput('year_2', 'Choose years', choices = year_names, selected = year_names, multiple = TRUE)
        )
      )
      
    }
  })
  
  # point plot
  output$point_plot <- renderPlotly({
    country_names <- input$country_2
    year_names <- input$year_2
    ind_names <- input$ind_2
    if(is.null(year_names)){
      NULL
    } else {
      pd <- dat %>% filter(country %in% country_names, 
                           year %in% year_names, 
                           indicator_code %in% ind_names)
      # save(pd, file = 'point_pd.rda')
      ggplot(pd, aes(year, value, color=indicator_code)) +
        geom_point() +
        geom_line(aes(group=indicator_code))

    }
  })
  
  #  MAP SECTION ###########################
  output$ui_inputs_3 <- renderUI({
    # country_names <- 'Bulgaria'
    year_name <- input$year_3
    if(is.null(year_name)){
      NULL
    } else {
      pd <- whoapp::dat %>% dplyr::filter(year == year_name)
      ind_names <- sort(unique(pd$ind))
      # remove quintile ind (q1, q2, etc)
      # ind_names <- ind_names[!grepl('_q', ind_names)]
      fluidPage(
        fluidRow(
          selectInput('ind_3', 'Choose an indicator', choices = ind_names, selected = ind_names[1]),
        )
      )
      
    }
  })
  
  # map
  output$map_1 <- renderLeaflet({
    year_name <- input$year_3
    ind_name <- input$ind_3
    if(is.null(ind_name)){
      NULL
    } else {
      pd <- whoapp::dat %>% filter(year==year_name,
                           indicator_code==ind_name)
      shp <- whoapp::world
      shp@data <- shp@data %>% dplyr::left_join(pd, by=c('NAME'='country'))
      na_rows <- which(!is.na(shp@data$value))
      shp <- shp[na_rows,]
      map_palette <- colorNumeric(palette = brewer.pal(9, "Greens"), domain=shp@data$value, na.color="white")
     
      # Make tooltip
      map_text <- paste(
        "Indicator: ",  shp@data$indicator_code,"<br>",
        "Country: ", as.character(shp@data$NAME),"<br/>", 
        "Value: ", paste0(round(shp@data$value, digits = 2)), "<br/>",
        "Year: ", as.character(shp@data$year),"<br/>",
        sep="") %>%
        lapply(htmltools::HTML)
      
    leaflet(shp, options = leafletOptions(zoomControl = FALSE, minZoom = 1, maxZoom = 10)) %>% 
        addProviderTiles('Esri.WorldShadedRelief') %>%
        # addTiles(carto) %>%
        addPolygons( 
          color = 'black',
          fillColor = ~map_palette(value), 
          stroke=TRUE, 
          fillOpacity = 0.9, 
          weight=1,
          label = map_text,
          highlightOptions = highlightOptions(
            weight = 1,
            fillColor = 'white',
            fillOpacity = 1,
            color = "white",
            opacity = 1.0,
            bringToFront = TRUE,
            sendToBack = TRUE
          ),
          labelOptions = labelOptions( 
            noHide = FALSE,
            style = list("font-weight" = "normal", padding = "3px 8px"), 
            textsize = "13px", 
            direction = "auto"
          )
        ) %>% 
        # setView(lat=0, lng=0 , zoom=1.7) %>%
        # setView(lat=mp$lat, lng=mp$lon , zoom=mp$the_zoom) %>%
        addLegend(pal=map_palette,  values=~value, opacity=0.9, position = "bottomleft", na.label = "NA" )
    }
    
  })
  
}

app <- function(){
  # Detect the system. If on AWS, don't launch browswer
  is_aws <- grepl('aws', tolower(Sys.info()['release']))
  
  shinyApp(ui = app_ui,
           server = app_server,
           options = list('launch.browswer' = !is_aws))
}
