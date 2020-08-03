## code to prepare `my_dataset` dataset goes here
library(usethis)
library(readxl)
# usethis::use_data(my_dataset, overwrite = TRUE)
download.file(url = 'https://github.com/databrew/saint/raw/master/forms/saint/saint.xls',
              destfile = 'form.xls')
form <- read_excel('form.xls')
