##################################################
# UI
##################################################
#' @import shiny
#' @import shinydashboard
#' @import leaflet
#' @import shiny
#' @import ggplot2
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
        add_busy_spinner(
          spin = "folding-cube",#  "self-building-square",
          position = 'bottom-right',
          onstart = TRUE,
          height = '100px',
          width = '100px',
          margins = c(10, 10)#,
          # color = 'de0000'
        ),
        
        # tags$head(
        #   tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        # ),
        
        tabItems(
          tabItem(
            tabName="main",
            fluidPage(
              h1('Main page')
            )),
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
  

}

app <- function(){
  # Detect the system. If on AWS, don't launch browswer
  is_aws <- grepl('aws', tolower(Sys.info()['release']))
  
  shinyApp(ui = app_ui,
           server = app_server,
           options = list('launch.browswer' = !is_aws))
}
