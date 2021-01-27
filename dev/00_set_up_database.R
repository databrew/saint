library(saint)
library(yaml)
library(aws.s3)
library(dplyr)
library(readr)
creds <- yaml::read_yaml(file = '../credentials/credentials.yaml')
s3_creds <- read_csv('../credentials/s3.csv')

already_done <- NULL

saint1 <- 
  odk_get_data(url = 'https://bohemia.systems',
                           id = 'saintperu',
                           id2 = 'data',
                           unknown_id2 = FALSE,
                           uuids = NULL,
                           exclude_uuids = already_done,
                           user = creds$databrew_odk_user,
                           password = creds$databrew_odk_pass,
                           widen = TRUE,
                           pre_auth = FALSE,
                           use_data_id = FALSE, 
                           sleeper = 0.25,
                           use_progress_bar = TRUE,
                           chunk_size = 1000)

saint2 <- 
  odk_get_data(url = 'https://bohemia.systems',
               id = 'saintperu2',
               unknown_id2 = TRUE,
               uuids = NULL,
               exclude_uuids = already_done,
               user = creds$databrew_odk_user,
               password = creds$databrew_odk_pass,
               widen = TRUE,
               pre_auth = FALSE,
               use_data_id = FALSE, 
               sleeper = 0.25,
               use_progress_bar = TRUE,
               chunk_size = 1000)

df <-
  bind_rows(saint1$non_repeats,
            saint2$non_repeats)

# Save as an R object
tempfile <- tempfile()
save(df, file = tempfile)

Sys.setenv(
  "AWS_ACCESS_KEY_ID" = s3_creds$`Access key ID`,
  "AWS_SECRET_ACCESS_KEY" = s3_creds$`Secret access key`,
  "AWS_DEFAULT_REGION" = "eu-west-3"
)

# bucketlist()
put_object(
  file = tempfile,
  object = 'df.RData',
  bucket = 'saintperu'
)

# # Read
# tempfile <- tempfile()  
# save_object(object = "s3://saintperu/df.RData", file = tempfile)
# load(tempfile)

