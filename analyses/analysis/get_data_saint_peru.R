source('../../R/get_data.R')
library(dplyr)
library(readr)
library(lubridate)
library(yaml)
library(saint)

creds <- yaml::yaml.load_file('../../credentials/credentials.yaml')

# Get original
x = get_data(data_file = 'data.csv',
             user = creds$databrew_odk_user,
             password = creds$databrew_odk_pass,
             form_id = 'saintperu')

# Get new version
y = bohemia::odk_get_data(url = creds$databrew_odk_server,
                          user = creds$databrew_odk_user,
                          password = creds$databrew_odk_pass,
                          id = 'saintperu2',
                          id2 = 'saintperu',
                          pre_auth = TRUE)
y <- y$non_repeats
y$server <- NULL


joined <- bind_rows(
  x %>% mutate(form = 'saintperu'),
  y %>% mutate(form = 'saintperu2') %>% mutate(deposiciones = as.character(deposiciones),
                                               pin = as.character(pin),
                                               pin2 = as.character(pin2),
                                               temp = as.character(temp))
)
write_csv(joined, '~/Desktop/saintperu.csv')
