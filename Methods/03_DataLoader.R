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

LoadSerology<-function(dbPath){
  connection<-dbConnect(RSQLite::SQLite(), dbPath)
  query = "SELECT * FROM SerolAccepted WHERE AccDate = :d;"
  pars = list(d = Sys.Date()-1)
  tmp <- dbGetQuery(connection, query, pars)
  dbDisconnect(connection)
  return(tmp)
}

BactAcceptedTable = LoadAllBacterilogy(appData)

SerolAcceptedTable = LoadSerology(appData)

LoadUsersData(usersDb)