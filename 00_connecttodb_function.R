library(RPostgreSQL)
library(dbplyr)

connect_to_fec <- function(user, password) {
  
  drv <- dbDriver('PostgreSQL')  
  db <- 'cnn_digital_fec'  
  host_db <- 'cnn-digital-fecproject.c8pl6djmdw8e.us-east-1.rds.amazonaws.com'  
  db_port <- '5432'  
  db_user <- user
  db_password <- password
  
  con <- dbConnect(drv, dbname=db, host=host_db, port=db_port, user=db_user, password=db_password)
  
  # remove user and pw info from the environment
  rm(db_user)
  rm(db_password)
  rm(db_port)
  
  print("Connected! Here are the tables in the database... ")
 
  #list the tables in the database to confirm connection
  dbplyr::src_dbi(con)


}


