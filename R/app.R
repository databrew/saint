
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
                       h3('Form submission times'),
                       DT::dataTableOutput('dt_missing')),
                column(6,
                       h3('Forms per day'),
                       plotOutput('plot_forms'))
              ),
              fluidRow(
                column(12,
                       h3('Symptoms table'),
                       helpText('The below table shows the number of consecutive days a patient has had a given symptom, as of the most recent observation. Click on the symptom to order by number of days.'),
                       DT::dataTableOutput('dt_symptoms'))
              ),
              fluidRow(

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
    pd <- data_list$data
    attr(pd$end_time, 'tzone') <- 'Europe/Paris'
    out <- pd %>%
      arrange(end_time) %>%
      group_by(pin) %>%
      summarise(x = dplyr::last(end_time)) %>%
      filter(!is.na(pin)) %>%
      ungroup %>%
      mutate(`Hours ago` = round(as.numeric(as.difftime(Sys.time()- x, units = 'hours')), digits = 2)) %>%
      mutate(x = as.character(x)) %>%
      dplyr::rename(`Last form submitted at` = x)
    out
  })
  
  output$dt_symptoms <- DT::renderDataTable({
    pd <- data_list$data
    pd <- pd %>%
      mutate(date = as.Date(end_time)) %>%
      arrange(pin,
              date) %>%
      filter(!is.na(pin)) %>%
      group_by(pin) %>%
      summarise(max_date = max(date),
                congestion_last = dplyr::last(congestion_si_no),
                congestion_days = dplyr::last(sequence(rle(as.character(congestion_si_no))$lengths)),
                congestion_days = ifelse(congestion_last == 'No', 0, congestion_days),
                
                diarrea_last = dplyr::last(diarrea_si_no),
                diarrea_days = dplyr::last(sequence(rle(as.character(diarrea_si_no))$lengths)),
                diarrea_days = ifelse(diarrea_last == 'No', 0, diarrea_days),
                
                fatiga_last = dplyr::last(fatiga_si_no),
                fatiga_days = dplyr::last(sequence(rle(as.character(fatiga_si_no))$lengths)),
                fatiga_days = ifelse(fatiga_last == 'No', 0, fatiga_days),
                
                malestar_last = dplyr::last(malestar_si_no),
                malestar_days = dplyr::last(sequence(rle(as.character(malestar_si_no))$lengths)),
                malestar_days = ifelse(malestar_last == 'No', 0, malestar_days),
                
                medicacion_last = dplyr::last(medicacion_si_no),
                medicacion_days = dplyr::last(sequence(rle(as.character(medicacion_si_no))$lengths)),
                medicacion_days = ifelse(medicacion_last == 'No', 0, medicacion_days),
                
                temp_last = dplyr::last(temp_si_no),
                temp_days = dplyr::last(sequence(rle(as.character(temp_si_no))$lengths)),
                temp_days = ifelse(temp_last == 'No', 0, temp_days),
                
                tos_last = dplyr::last(tos_si_no),
                tos_days = dplyr::last(sequence(rle(as.character(tos_si_no))$lengths)),
                tos_days = ifelse(tos_last == 'No', 0, tos_days),
                
                vomitos_last = dplyr::last(vomitos_si_no),
                vomitos_days = dplyr::last(sequence(rle(as.character(vomitos_si_no))$lengths)),
                vomitos_days = ifelse(vomitos_last == 'No', 0, vomitos_days),
                ) %>%
      # Remove column names with "last"
      dplyr::select(-contains('_last')) %>%
      # Rename column
      dplyr::rename(`Last observation` = max_date)
    # Remove the "days" from column names
    names(pd) <- gsub('_days', '', names(pd))
    pd
  })
  

  output$plot_forms <- renderPlot({
    pd <- data_list$data
    pd <- pd %>%
      group_by(date = as.Date(end_time),
               pin) %>%
      tally %>%
      ungroup %>%
      filter(!is.na(pin),
             !is.na(date))
    cols <- colorRampPalette(RColorBrewer::brewer.pal(n = 9, name = 'Spectral'))(length(unique(pd$pin)))
    
    ggplot(data = pd,
           aes(x = date,
               y = n,
               fill = factor(pin))) +
      geom_bar(stat = 'identity',
               color = 'black',
               size = 0.2) +
      scale_fill_manual(name = 'ID',
                        values = cols) +
      labs(x = 'Date',
           y = 'Forms filled out',
           title = 'Forms filled out by date') #+
      # guides(fill = guide_legend(reverse = TRUE))

  })
  
}

app <- function(){
  # Detect the system. If on AWS, don't launch browswer
  is_aws <- grepl('aws', tolower(Sys.info()['release']))
  shinyApp(ui = app_ui,
           server = app_server,
           options = list('launch.browswer' = !is_aws))
}