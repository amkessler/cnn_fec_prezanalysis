library(RPostgreSQL)

# Connecting to RPostgreSQL

drv <- dbDriver('PostgreSQL')  
db <- 'cnn_digital_fec'  
host_db <- 'cnn-digital-fecproject.c8pl6djmdw8e.us-east-1.rds.amazonaws.com'  
db_port <- '5432'  
db_user <- Sys.getenv("FEC_DB_USER") #set this value in .Renviron
db_password <- Sys.getenv("FEC_DB_PASSWORD") #set this value in .Renviron

con <- dbConnect(drv, dbname=db, host=host_db, port=db_port, user=db_user, password=db_password)

