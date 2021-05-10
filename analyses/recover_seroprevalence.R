library(DBI)
library(RPostgres)

con <- dbConnect(RPostgres::Postgres(), dbname = 'saint')  


hh_covid <- dbGetQuery(conn = con,
                       'select * from aggregate."SER8NCE_OTHER_HH_COVID_REPEAT"')
hh_covid_suspected <- dbGetQuery(conn = con,
                                 'select * from aggregate."SER8NCE_OTHER_HH_COVID_SUSPECTED_REPEAT"')
hh_member <- dbGetQuery(conn = con,
                                 'select * from aggregate."SER8NCE_HH_MEMBER_REPEAT"')

library(readr)
write_csv(hh_covid, '~/Desktop/hh_covid.csv')
write_csv(hh_covid_suspected, '~/Desktop/hh_covid_suspected.csv')
write_csv(hh_member, '~/Desktop/hh_member.csv')
dbDisconnect(con)
