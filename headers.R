#credentials <- data.frame(
  #user = pwds$Id,
  #password = pwds$pwd, # password will automatically be hashed
  #stringsAsFactors = FALSE
#)

if(Sys.info()["sysname"] == "Linux"){
  usersDb<-"DataBases/Users.sqlite"
  appData<-"DataBases/AccSpecimens.sqlite"
  methodsData<-"Methods"
  ModulesData<-"Modules"
  CustomElements<-"DesignElements"
}

if(Sys.info()["sysname"] == "Windows"){
  usersDb<-"E:/LabApps/ProtectedApp/DataBases/Users.sqlite"
  appData<-"E:/LabApps/ProtectedApp/DataBases/AccSpecimens.sqlite"
  methodsData<-"E:/LabApps/ProtectedApp/Methods"
  ModulesData<-"E:/LabApps/ProtectedApp/Modules"
  CustomElements<-"E:/LabApps/ProtectedApp/DesignElements"
}


if(!file.exists(usersDb)){
  mydb <- dbConnect(RSQLite::SQLite(), usersDb)
  create_db(
    credentials_data = credentials,
    sqlite_path = usersDb, # will be created
  )
  dbDisconnect(mydb)
}

set_labels(
  language = "en",
  "Please authenticate" = "Вход в систему",
  "Username:" = "Имя пользователя:",
  "Password:" = "Пароль:",
  "Login" = "Вход",
  "Username or password are incorrect" = "Неправильный пароль или логин"
)

#LoadFilesRegion
loadSection<-function(path, recursion = FALSE){
  files<-list.files(path, full.names = TRUE, recursive = recursion)
  for(f in files){
    source(f)
  }
}
loadSection(methodsData)
loadSection(ModulesData, TRUE)
loadSection(CustomElements, TRUE)
#LoadFilesRegion