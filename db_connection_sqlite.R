#---- db connection -----

## SQLite
library(RSQLite)


# STEP 1: connect to db


db_name <- list.files(path= "db\\", pattern=NULL, all.files=FALSE,
           full.names=FALSE)

db_path<- paste("db\\", db_name, sep="")


db <- dbConnect(SQLite(), dbname= db_path)
                

# STEP 2: Declare what to retrive from db. SQL query

Query_SQL <- paste("
                  SELECT * FROM advert
                      ", sep="")

# STEP 3: Connect to db and retrive data

RawData_Obs <- dbGetQuery(db, Query_SQL)


# STEP 4: close connection

dbDisconnect(db)


