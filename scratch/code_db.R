# Load the DBI and MySQL driver packages
library(DBI)
library(RMySQL) # Or RMariaDB

# Connection details (replace with your actual details)
host <- "bns-000-as02" # e.g., "192.168.1.100" or "db.example.com"
user <- Sys.getenv("db_user")
password <- Sys.getenv("db_pw")
dbname <- "ics_db"
port <- 3306 # Default MySQL port (change if needed)

# Create the connection
con <- dbConnect(MySQL(),
                 host = host,
                 user = user,
                 password = password,
                 dbname = dbname,
                 port = port)

# Example: Run a query
dbGetQuery(con, "SELECT * FROM discharge_pathway_projections") 
