library(mongolite)

options(mongodb = list(
  "host" = "ds050087.mlab.com:50087",
  "username" = "dlevybooth",
  "password" = "#Three3"
))
databaseName <- "thermo1"
collectionName <- "users"

connection <- mongo(collection = "users", db = "thermo1", url = sprintf(
  "mongodb://%s:%s@%s/%s",
  options()$mongodb$username,
  options()$mongodb$password,
  options()$mongodb$host,
  databaseName))

saveData <- function(data) {
  # Connect to the database
  db <- mongo(collection = collectionName,
              url = sprintf(
                "mongodb://%s:%s@%s/%s",
                options()$mongodb$username,
                options()$mongodb$password,
                options()$mongodb$host,
                databaseName))
  # Insert the data into the mongo collection as a data.frame
  data <- data
  db$insert(data)
}

loadData <- function() {
  # Connect to the database
  db <- mongo(collection = collectionName,
              url = sprintf(
                "mongodb://%s:%s@%s/%s",
                options()$mongodb$username,
                options()$mongodb$password,
                options()$mongodb$host,
                databaseName))
  # Read all the entries
  data <- db$find()
  data
}


loadComments <- function(collection) {
  # Connect to the database
  db <- mongo(collection = collection,
              url = sprintf(
                "mongodb://%s:%s@%s/%s",
                options()$mongodb$username,
                options()$mongodb$password,
                options()$mongodb$host,
                databaseName))
  # Read all the entries
  data <- db$find()
  data
}

saveComments <- function(data, collection) {
  # Connect to the database
  db <- mongo(collection = collection,
              url = sprintf(
                "mongodb://%s:%s@%s/%s",
                options()$mongodb$username,
                options()$mongodb$password,
                options()$mongodb$host,
                databaseName))
  # Insert the data into the mongo collection as a data.frame
  data <- data
  db$insert(data)
}


