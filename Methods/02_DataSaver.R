CreateInitialTables<-function(dbPath){
  connection<-dbConnect(RSQLite::SQLite(), dbPath)
  tables<-dbListTables(connection)
  if(!("SpecTypes" %in% tables)){ dbWriteTable(connection, "SpecTypes", SpecTypesTable) }
  if(!("Dishes" %in% tables)){ dbWriteTable(connection, "Dishes", DishesTable) }
  if(!("DishPerType" %in% tables)){ dbWriteTable(connection, "DishPerType", DishPerTypeTable) }
  if(!("BactAccepted" %in% tables)){ dbWriteTable(connection, "BactAccepted", BactAcceptedTable) }
  if(!S("SerolAccepted" %in% tables)){ dbWriteTable(connection, "SerolAccepted", SerolAcceptedTable) }
  dbDisconnect(connection)
}

UpdateSpecimenMaxNum<-function(groupName, spTypesTab, dbPath){
  connection<-dbConnect(RSQLite::SQLite(), dbPath)
  val <- spTypesTab[spTypesTab$Name == groupName,]
  query <- "UPDATE SpecTypes SET MaxNum = :num WHERE Name = :nam"
  pars<-list(num = val[1,]$MaxNum, nam = val[1,]$Name)
  dbExecute(connection, query, pars)
  dbDisconnect(connection)
}

AddSpecimenType<-function(groupName, spTypesTab, dbPath){
  connection<-dbConnect(RSQLite::SQLite(), dbPath)
  val <- spTypesTab %>% find(Name == groupName)
  query <- "INSERT INTO SpecTypes (MaxNum, Name) VALUES (:num, :nam)"
  pars<-list(num = val[1,]$MaxNum, nam = val[1,]$Name)
  dbExecute(connection, query, pars)
  dbDisconnect(connection)
}

AddDish<-function(dishName, dishTable, dbPath){
  connection<-dbConnect(RSQLite::SQLite(), dbPath)
  query <- "INSERT INTO Dishes (Name) VALUES (:nam)"
  pars<-list(nam = dishName)
  dbExecute(connection, query, pars)
  dbDisconnect(connection)
}

UpdateDishAndType<-function(dish, type, dTypesTable, dbPath){
  connection<-dbConnect(RSQLite::SQLite(), dbPath)
  val <- dTypesTable %>% find(Type == type, Dish = dish)
  query <- "UPDATE SpecTypes SET DishPerSample = :num WHERE Type = :nam AND Dish = :d"
  pars<-list(num = val[1,]$DishPerSample, nam = val[1,]$Type, d = val$Dish)
  dbExecute(connection, query, pars)
  dbDisconnect(connection)
}

AddBactAcceptedRecord<-function(acceptedTable, dbPath){
  connection<-dbConnect(RSQLite::SQLite(), dbPath)
  query <- "INSERT INTO BactAccepted (
    AxaptaCode,
    SampleCode,
    AccDate,
    Type,
    CurrentNum,
    IsCompleted
)
VALUES (
    :ax,
    :samCode,
    :accDate,
    :type,
    :curNum,
    :isCompleted
);"
  dbBegin(connection)
  tryCatch({
    for(rowNum in 1:nrow(acceptedTable)){
      pars<-list(ax = acceptedTable[rowNum,]$AxaptaCode,
                 samCode = acceptedTable[rowNum,]$SampleCode,
                 accDate = as.numeric(acceptedTable[rowNum,]$AccDate),
                 type = acceptedTable[rowNum,]$Type,
                 curNum = as.numeric(acceptedTable[rowNum,]$CurrentNum),
                 isCompleted = as.logical(acceptedTable[rowNum,]$IsCompleted))
      dbExecute(connection, query, pars)
    }
    dbCommit(connection)
    SuccessAlert("Успешно", "сохранено в БД")
  },
  warning = function(w){
    dbRollback(connection)
    WarningAlert("Предупреждение", conditionMessage(w))
  },
  error = function(e){
    dbRollback(connection)
    WarningAlert("Ошибка", conditionMessage(e))
  },
  finally = {
    dbDisconnect(connection)
  })
}