
##################################################
# UI
##################################################
#' @import shiny
#' @import shinydashboard
#' @import leaflet
#' @import shiny
#' @import ggplot2
app_ui <- function(request) {
  options(scipen = '999')
  
  tagList(
    mobile_golem_add_external_resources(),
    
    dashboardPage(
      dashboardHeader (title = "SAINT"),
      dashboardSidebar(
        sidebarMenu(
          menuItem(
            text="Main",
            tabName="main"),
          menuItem(
            text="Raw data",
            tabName="raw_data"),
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
            # navbarPage(title = '',
            #            collapsible = TRUE,
            #            tabPanel(title = "Overview",
            fluidPage(
              fluidRow(
                column(6,
                       actionButton('action', 'Refresh data')),
                column(6,
                       uiOutput('ts_ui'))
              ),
              fluidRow(
                column(6,
                       h3('Participants with missing forms'),
                       DT::dataTableOutput('dt_missing')),
                column(6,
                       h3('Forms per day'),
                       plotOutput('plot_forms'))
              ),
              fluidRow(
                column(6,
                       h3('COVID flags'),
                       DT::dataTableOutput('dt_covid')),
                column(6,
                       h3('IVM flags'),
                       DT::dataTableOutput('dt_ivm'))
              ),
              fluidRow(
                column(12,
                       h3('COVID and IVM symptoms flags'),
                       DT::dataTableOutput('dt_both'))
              )
            )
            # ),
            # navbarMenu("Data",
            #            tabPanel("Raw data"),
            #            tabPanel("Meta-data"))
            
            # )
          ),
          tabItem(
            tabName = 'raw_data',
            fluidPage(
              h1('Raw data'),
              helpText('The below table is the data as it appears on the ODK server'),
              DT::dataTableOutput('dt_raw')
            )
          ),
          tabItem(
            tabName = 'about',
            fluidPage(
              fluidRow(
                div(img(src='www/logo_clear.png', align = "center"), style="text-align: center;"),
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
#' @import yaml
app_server <- function(input, output, session) {

  
  # Get into reactive object
  data_list <- reactiveValues(data = data.frame(),
                              ts = as.character(Sys.time()))
    
  
  # Observe the action button (or app start) to load data
  observeEvent(input$action, {
    # Get data
    df <- get_data(data_file = paste0(getwd(), '/data.csv'),
             user = yaml::read_yaml('credentials/credentials.yaml')$user,
             password = yaml::read_yaml('credentials/credentials.yaml')$password)
    data_list$data <- df
    data_list$ts <- as.character(Sys.time())
  }, ignoreNULL = FALSE)
  
  output$dt_raw <- DT::renderDataTable({
    out <- data_list$data
    out
  },
  options = list(scrollX = TRUE))
  
  output$ts_ui <- renderUI({
    out <- data_list$ts
    helpText(paste0('Data last updated at: ', out))
  })
  
  output$dt_missing <- DT::renderDataTable({
    tibble(x = c('This will be a table',
                 'of people who have not filled',
                 'out a form by 14:00, or at all',
                 'for any given day.'))
  })
  
  output$dt_covid <- DT::renderDataTable({
    tibble(x = c('This will be a table of people who have:',
                 'Fver > 7 days',
                 'Cough > 14 days',
                 'Fatigue > 10 days',
                 '10 or more bowel movements in a day',
                 '5 o more bowele movements over 3 days'))
  })
  
  output$dt_ivm <- DT::renderDataTable({
    tibble(x = c('This will be a table of:',
                 'The day of symptom onset after treatment'))
  })
  
  output$dt_both <- DT::renderDataTable({
    tibble(x = c('This will be a table of people with symptoms',
                 'Common to both COVID-19 and IVM:',
                 'Vomiting, diarrhea, headache',
                 'Fever, fatigue, not feeling well generally'))
  })
  
  output$plot_forms <- renderPlot({
    pd <- expand.grid(id = 1:10,
                      date = seq(Sys.Date()-5, Sys.Date(), by = 1)) %>%
      mutate(form = 1)
    remove <- sample(1:nrow(pd), 3)
    pd <- pd[!1:nrow(pd) %in% remove,]
    cols <- colorRampPalette(RColorBrewer::brewer.pal(n = 9, name = 'Spectral'))(length(unique(pd$id)))
    ggplot(data = pd,
           aes(x = date,
               y = form,
               fill = factor(id))) +
      geom_bar(stat = 'identity',
               color = 'black',
               size = 0.2) +
      scale_fill_manual(name = 'ID',
                        values = cols) +
      labs(x = 'Date',
           y = 'Forms filled out',
           title = 'Placeholder chart of forms filled out')
  })
  
}

app <- function(){
  # Detect the system. If on AWS, don't launch browswer
  is_aws <- grepl('aws', tolower(Sys.info()['release']))
  shinyApp(ui = app_ui,
           server = app_server,
           options = list('launch.browswer' = !is_aws))
}