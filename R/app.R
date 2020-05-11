
##################################################
# UI
##################################################
#' @import shiny
#' @import shinydashboard
#' @import leaflet
#' @import shiny
#' @import ggplot2
#' @import shinyMobile
app_ui <- function(request) {
  options(scipen = '999')
  
  tagList(
    mobile_golem_add_external_resources(),
    
    dashboardPage(
      dashboardHeader (title = "Databrew Dashboard"),
      dashboardSidebar(
        sidebarMenu(
          menuItem(
            text="Main",
            tabName="main"),
          menuItem(
            text="Charts",
            tabName="charts"),
          menuItem(
            text = 'Widgets',
            tabName = 'widgets'),
          menuItem(
            text = 'About',
            tabName = 'about')
        )),
      dashboardBody(
        # tags$head(
        #   tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        # ),
        tabItems(
          tabItem(
            tabName="main",
            navbarPage(title = '',
                       collapsible = TRUE,
                       tabPanel(title = "Xing's test page",
                                fluidRow(
                                  shinydashboard::box(title = 'This is another box',
                                                      width = 6,
                                                      status = 'warning',
                                                      collapsible = TRUE,
                                                      footer = 'This is a footer',
                                                      plotOutput('plot1')),
                                  column(6,
                                         h1('Big heading (h1)'),
                                         h2('Less big heading (h2)'),
                                         h3('Sort of big heading (h3)'),
                                         h4('Not so big heading (h4)'),
                                         h5('Small heading (h5)'),
                                         h6('Heading w/ background (h6)'))
                                ),
                                fluidRow(
                                  column(4,
                                         h4('A bunch of inputs'),
                                         p(selectInput('abc', 'Pick a place', choices = c('Home', 'Away', 'In-between')),
                                           radioButtons('xyz', 'What do you like?', choices = c('Ice cream', 'Pizza', 'Both', 'Neither', 'Ice pizza')),
                                           dateRangeInput('aslk', 'Date range', start = Sys.Date() - 20, end = Sys.Date() - 5),
                                           actionButton('action', 'This is a button', icon = icon('download')),
                                           sliderInput('lakjaasa', 'This is a slider', min = 0, max = 100, value = 25),
                                           textInput('qwer', 'This is some text input'))),
                                  column(4,
                                         h4('Here is some regular text'),
                                         p('This is normal (ie, p) text, This is normal (ie, p) text, This is normal (ie, p) text,
                                     This is normal (ie, p) text, This is normal (ie, p) text, This is normal (ie, p) text'),
                                         helpText('This is "help text"')),
                                  shinydashboard::box(title = 'This is another box',
                                                      width = 4,
                                                      status = 'info',
                                                      collapsible = TRUE,
                                                      footer = 'This is a footer',
                                                      leaflet::leafletOutput('l1')
                                  )
                                )
                       ),
                       navbarMenu("Tab A",
                                  tabPanel("Dropdown A"),
                                  tabPanel("Dropdown B")),
                       tabPanel('Tab B'),
                       tabPanel('Tab C'),
                       tabPanel('Tab D'),
                       navbarMenu("Tab E",
                                  tabPanel("Dropdown 1"),
                                  tabPanel("Dropbdown 2"))
                       
            )
          ),
          tabItem(
            tabName = 'about',
            fluidPage(
              fluidRow(
                div(img(src='logo_clear.png', align = "center"), style="text-align: center;"),
                h4('Built in partnership with ',
                   a(href = 'http://databrew.cc',
                     target='_blank', 'Databrew'),
                   align = 'center'),
                p('Empowering research and analysis through collaborative data science.', align = 'center'),
                div(a(actionButton(inputId = "email", label = "info@databrew.cc",
                                   icon = icon("envelope", lib = "font-awesome")),
                      href="mailto:info@databrew.cc",
                      align = 'center')),
                style = 'text-align:center;'
              )
            )
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
    'www', system.file('app/www', package = 'saint')
  )
  
  
  # share <- list(
  #   title = "Databrew's COVID-19 Data Explorer",
  #   url = "https://datacat.cc/covid19/",
  #   image = "http://www.databrew.cc/images/blog/covid2.png",
  #   description = "Comparing epidemic curves across countries",
  #   twitter_user = "data_brew"
  # )
  
  tags$head(
    
    # # Facebook OpenGraph tags
    # tags$meta(property = "og:title", content = share$title),
    # tags$meta(property = "og:type", content = "website"),
    # tags$meta(property = "og:url", content = share$url),
    # tags$meta(property = "og:image", content = share$image),
    # tags$meta(property = "og:description", content = share$description),
    # 
    # # Twitter summary cards
    # tags$meta(name = "twitter:card", content = "summary"),
    # tags$meta(name = "twitter:site", content = paste0("@", share$twitter_user)),
    # tags$meta(name = "twitter:creator", content = paste0("@", share$twitter_user)),
    # tags$meta(name = "twitter:title", content = share$title),
    # tags$meta(name = "twitter:description", content = share$description),
    # tags$meta(name = "twitter:image", content = share$image),
    # 
    # # golem::activate_js(),
    # # golem::favicon(),
    # # Add here all the external resources
    # # Google analytics script
    # includeHTML(system.file('app/www/google-analytics-mini.html', package = 'covid19')),
    # includeScript(system.file('app/www/script.js', package = 'covid19')),
    # includeScript(system.file('app/www/mobile.js', package = 'covid19')),
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
app_server <- function(input, output, session) {
  
}

app <- function(){
  # Detect the system. If on AWS, don't launch browswer
  is_aws <- grepl('aws', tolower(Sys.info()['release']))
  shinyApp(ui = app_ui,
           server = app_server,
           options = list('launch.browswer' = !is_aws))
}