LoadDbData<-function(dbPath, dataTable){
  connection<-dbConnect(RSQLite::SQLite(), dbPath)
  result = dbReadTable(connection, dataTable)
  dbDisconnect(connection)
  return(result)
}

SpecTypesTable = LoadDbData(appData, "SpecTypes")
DishesTable = LoadDbData(appData, "Dishes")
DishPerTypeTable = LoadDbData(appData, "DishPerType")

LoadUsersData<-function(dbPath){
  connection<-dbConnect(RSQLite::SQLite(), dbPath)
  credentials = dbReadTable(connection, "credentials")
  dbDisconnect(connection)
}

LoadAllBacterilogy<-function(dbPath, dateToSearch = Sys.Date()){
  connection<-dbConnect(RSQLite::SQLite(), dbPath)
  query = "SELECT * FROM BactAccepted WHERE AccDate >= :d;"
  pars = list(d = as.numeric(as.POSIXct(dateToSearch)))
  tmp <- dbGetQuery(connection, query, pars)
  dbDisconnect(connection)
  return(tmp)
}

LoadBacterilogyByStatus<-function(dbPath, status, dateToSearch = Sys.Date()){
  connection<-dbConnect(RSQLite::SQLite(), dbPath)
  query = "SELECT * FROM BactAccepted WHERE AccDate >= :d AND IsCompleted = :s;"
  pars = list(d = as.numeric(as.POSIXct(dateToSearch)),
              s = as.logical(status))
  tmp <- dbGetQuery(connection, query, pars)
  dbDisconnect(connection)
  return(tmp)
}

LoadSerologyByStatus<-function(dbPath, status, dateToSearch = Sys.Date()){
  connection<-dbConnect(RSQLite::SQLite(), dbPath)
  query = "SELECT AxaptaCode FROM SerolAccepted WHERE AccDate >= :d AND IsCompleted = :s;"
  pars = list(d = as.numeric(as.POSIXct(dateToSearch)),
              s = as.logical(status))
  tmp <- dbGetQuery(connection, query, pars)
  dbDisconnect(connection)
  return(tmp)
}

LoadUsedAxNums<-function(dbPath, tableName){
  connection<-dbConnect(RSQLite::SQLite(), dbPath)
  query = paste0("SELECT DISTINCT AxaptaCode FROM ", tableName, ";")
  tmp <- dbGetQuery(connection, query)
  dbDisconnect(connection)
  return(tmp)
}

LoadUsedBarcodes<-function(dbPath, tableName){
  connection<-dbConnect(RSQLite::SQLite(), dbPath)
  query = paste0("SELECT DISTINCT SampleCode FROM ", tableName, ";")
  tmp <- dbGetQuery(connection, query)
  dbDisconnect(connection)
  return(tmp)
}

LoadTask<-function(dbPath, currTask, tableName = "SerolAccepted"){
  connection<-dbConnect(RSQLite::SQLite(), dbPath)
  query = paste0("SELECT * FROM ", tableName, " WHERE TaskName >= :t;")
  pars<-list(t = currTask)
  tmp <- dbGetQuery(connection, query, pars)
  dbDisconnect(connection)
  return(tmp)
}

LoadSerology<-function(dbPath, dateToSearch = Sys.Date(), taskName = NULL){
  connection<-dbConnect(RSQLite::SQLite(), dbPath)
  query = "SELECT * FROM SerolAccepted WHERE AccDate >= :d;"
  pars = list(d = as.numeric(as.POSIXct(dateToSearch)))
  if(!is.null(taskName)){
    query = "SELECT * FROM SerolAccepted WHERE AccDate >= :d AND TaskName = :n;"
    pars = list(d = as.numeric(as.POSIXct(dateToSearch), n = taskName))
  }
  tmp <- dbGetQuery(connection, query, pars)
  dbDisconnect(connection)
  return(tmp)
}

LoadTasksNums<-function(dbPath, dateToSearch = Sys.Date()){
  connection<-dbConnect(RSQLite::SQLite(), dbPath)
  query = "SELECT DISTINCT TaskName FROM SerolAccepted WHERE AccDate >= :d;"
  pars = list(d = as.numeric(as.POSIXct(dateToSearch)))
  tmp <- dbGetQuery(connection, query, pars)
  dbDisconnect(connection)
  return(tmp)
}

BactAcceptedTable = LoadAllBacterilogy(appData)

SerolAcceptedTable = LoadSerology(appData)

LoadUsersData(usersDb)